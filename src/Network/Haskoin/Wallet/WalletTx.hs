{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.WalletTx where

import           Control.Applicative             ((<|>))
import           Control.Arrow                   (second, (&&&))
import           Control.Monad                   (fail)
import           Data.Aeson                      (object, (.=), (.:))
import qualified Data.Aeson                      as Json
import           Data.Aeson.TH
import           Data.Aeson.Types                (Parser)
import qualified Data.ByteString                 as BS
import           Data.Decimal                    as Decimal
import           Data.Either                     (partitionEithers)
import           Data.List                       (sortOn, sum)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid
import qualified Data.Serialize                  as Serialize
import           Data.String.Conversions         (cs)
import           Data.String.ToString
import           Data.Text                       (Text)
import           Haskoin.Address
import           Haskoin.Block
import           Haskoin.Constants
import           Haskoin.Keys
import           Haskoin.Script
import qualified Haskoin.Store.Data              as Store
import           Haskoin.Transaction
import           Haskoin.Util
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.Doc
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Options.Applicative.Help.Pretty

data TxType = TxDebit | TxInternal | TxCredit
    deriving (Show, Eq)

instance ToString TxType where
    toString =
        \case
            TxDebit -> "debit"
            TxInternal -> "internal"
            TxCredit -> "credit"

instance Json.ToJSON TxType where
    toJSON = Json.String . cs . toString

instance Json.FromJSON TxType where
    parseJSON =
        Json.withText "txtype" $ \case
            "debit" -> return TxDebit
            "internal" -> return TxInternal
            "credit" -> return TxCredit

data WalletTx = WalletTx
    { walletTxId            :: TxHash
    , walletTxType          :: TxType
    , walletTxAmount        :: Integer
    , walletTxMyOutputs     :: Map Address (Natural, SoftPath)
    , walletTxOtherOutputs  :: Map Address Natural
    , walletTxNonStdOutputs :: Natural
    , walletTxMyInputs      :: Map Address (Natural, SoftPath)
    , walletTxOtherInputs   :: Map Address Natural
    , walletTxNonStdInputs  :: Natural
    , walletTxSize          :: Natural
    , walletTxFee           :: Natural
    , walletTxFeeByte       :: Decimal
    , walletTxBlockRef      :: Store.BlockRef
    } deriving (Eq, Show)

walletTxToJSON :: Network -> WalletTx -> Either String Json.Value
walletTxToJSON net tx = do
    myOutputsT <- mapAddrText net $ walletTxMyOutputs tx
    othOutputsT <- mapAddrText net $ walletTxOtherOutputs tx
    myInputsT <- mapAddrText net $ walletTxMyInputs tx
    othInputsT <- mapAddrText net $ walletTxOtherInputs tx
    return $
        object
            [ "txid" .= walletTxId tx
            , "type" .= walletTxType tx
            , "amount" .= walletTxAmount tx
            , "myoutputs" .= myOutputsT
            , "otheroutputs" .= othOutputsT
            , "nonstdoutputs" .= walletTxNonStdOutputs tx
            , "myinputs" .= myInputsT
            , "otherinputs" .= othInputsT
            , "nonstdinputs" .= walletTxNonStdInputs tx
            , "size" .= walletTxSize tx
            , "fee" .= walletTxFee tx
            , "feebyte" .= show (walletTxFeeByte tx)
            , "block" .= walletTxBlockRef tx
            ]

walletTxParseJSON :: Network -> Json.Value -> Parser WalletTx
walletTxParseJSON net =
    Json.withObject "wallettx" $ \o -> do
        myOutputsT <- o .: "myoutputs"
        othOutputsT <- undefined
        myInputsT <- undefined
        othInputsT <- undefined
        WalletTx
            <$> o .: "txid"
            <*> o .: "type"
            <*> o .: "amount"
            <*> (either fail return $ mapTextAddr net myOutputsT)
            <*> (either fail return $ mapTextAddr net othOutputsT)
            <*> o .: "nonstdoutputs"
            <*> (either fail return $ mapTextAddr net myInputsT)
            <*> (either fail return $ mapTextAddr net othInputsT)
            <*> o .: "nonstdinputs"
            <*> o .: "size"
            <*> o .: "fee"
            <*> (read <$> o .: "feebyte")
            <*> o .: "block"

mapAddrText :: Network -> Map Address v -> Either String (Map Text v)
mapAddrText net m = do
    let f (a, v) = (, v) <$> addrToStringE net a
    Map.fromList <$> mapM f (Map.assocs m)

mapTextAddr :: Network -> Map Text v -> Either String (Map Address v)
mapTextAddr net m = do
    let f (a, v) = (, v) <$> stringToAddrE net a
    Map.fromList <$> mapM f (Map.assocs m)

fromStoreTransaction :: Map Address SoftPath -> Store.Transaction -> WalletTx
fromStoreTransaction walletAddrs sTx =
    WalletTx
        { walletTxId = Store.transactionId sTx
        , walletTxType = txType
        , walletTxAmount = amount
        , walletTxMyOutputs = myOutputsMap
        , walletTxOtherOutputs = othOutputsMap
        , walletTxNonStdOutputs = nonStdOut
        , walletTxMyInputs = myInputsMap
        , walletTxOtherInputs = othInputsMap
        , walletTxNonStdInputs = nonStdIn
        , walletTxSize = size
        , walletTxFee = fee
        , walletTxFeeByte = feeByte
        , walletTxBlockRef = Store.transactionBlock sTx
        }
  where
    size = fromIntegral $ Store.transactionSize sTx :: Natural
    fee = fromIntegral $ Store.transactionFees sTx :: Natural
    feeByte = Decimal.roundTo 2 $ (fromIntegral fee) / (fromIntegral size)
    (outputMap, nonStdOut) = outputAddressMap $ Store.transactionOutputs sTx
    myOutputsMap = Map.intersectionWith (,) outputMap walletAddrs
    othOutputsMap = Map.difference outputMap walletAddrs
    (inputMap, nonStdIn) = inputAddressMap $ Store.transactionInputs sTx
    myInputsMap = Map.intersectionWith (,) inputMap walletAddrs
    othInputsMap = Map.difference inputMap walletAddrs
    myOutputsSum = fromIntegral $ sum $ fst <$> Map.elems myOutputsMap :: Integer
    myInputsSum = fromIntegral $ sum $ fst <$> Map.elems myInputsMap :: Integer
    amount = myOutputsSum - myInputsSum
    txType | amount > 0 = TxCredit
           | abs amount == fromIntegral fee = TxInternal
           | otherwise = TxDebit

outputAddressMap :: [Store.StoreOutput] -> (Map Address Natural, Natural)
outputAddressMap outs =
    (Map.fromListWith (+) rs, sum ls)
  where
    (ls, rs) = partitionEithers $ f <$> outs
    f (Store.StoreOutput v _ _ Nothing)  = Left $ fromIntegral v
    f (Store.StoreOutput v _ _ (Just a)) = Right (a, fromIntegral v)

inputAddressMap :: [Store.StoreInput] -> (Map Address Natural, Natural)
inputAddressMap ins =
    (Map.fromListWith (+) rs, sum ls)
  where
    (ls, rs) = partitionEithers $ f <$> ins
    f (Store.StoreCoinbase _ _ _ _)           = Left 0
    f (Store.StoreInput _ _ _ _ v _ Nothing)  = Left $ fromIntegral v
    f (Store.StoreInput _ _ _ _ v _ (Just a)) = Right (a, fromIntegral v)

{-

data DetailedTx = DetailedTx
    { detailedTxHash          :: Maybe TxHash
    , detailedTxSize          :: Maybe Natural
    , detailedTxAmount        :: Maybe Integer
    , detailedTxType          :: Maybe TxType
    , detailedTxOutbound      :: Map Text Natural
    , detailedTxNonStdOutputs :: Natural
    , detailedTxInbound       :: Map Text (Natural, Maybe SoftPath)
    , detailedTxMyInputs      :: Map Text (Natural, Maybe SoftPath)
    , detailedTxOtherInputs   :: Map Text Natural
    , detailedTxNonStdInputs  :: Natural
    , detailedTxFee           :: Maybe Natural
    , detailedTxFeeByte       :: Maybe Text
    , detailedTxHeight        :: Maybe Natural
    , detailedTxBlockHash     :: Maybe BlockHash
    } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 8) ''DetailedTx)

emptyDetailedTx :: DetailedTx
emptyDetailedTx =
    DetailedTx
    { detailedTxHash = Nothing
    , detailedTxSize = Nothing
    , detailedTxAmount = Nothing
    , detailedTxType = Nothing
    , detailedTxOutbound = Map.empty
    , detailedTxNonStdOutputs = 0
    , detailedTxInbound = Map.empty
    , detailedTxMyInputs = Map.empty
    , detailedTxOtherInputs = Map.empty
    , detailedTxNonStdInputs = 0
    , detailedTxFee = Nothing
    , detailedTxFeeByte = Nothing
    , detailedTxHeight = Nothing
    , detailedTxBlockHash = Nothing
    }

buildDetailedTx :: Endo DetailedTx -> DetailedTx
buildDetailedTx e = appEndo (e <> fillAmountFee) emptyDetailedTx

fillAmountFee :: Endo DetailedTx
fillAmountFee =
    Endo $ \dtx ->
        dtx
            { detailedTxAmount = Just $ amnt dtx
            , detailedTxFeeByte = cs . show <$> feeByteM dtx
            , detailedTxType = Just $ txType dtx
            }
  where
    inboundSum dtx = sum $ fst <$> Map.elems (detailedTxInbound dtx)
    myCoinsSum dtx = sum $ fst <$> Map.elems (detailedTxMyInputs dtx)
    amnt dtx = toInteger (inboundSum dtx) - toInteger (myCoinsSum dtx)
    feeByteM dtx = do
        sat <- fromIntegral <$> detailedTxFee dtx :: Maybe Decimal
        bytes <- fromIntegral <$> detailedTxSize dtx :: Maybe Decimal
        return $ roundTo 2 $ sat / bytes
    txType dtx
        | amnt dtx > 0 = TxInbound
        | Just (fromIntegral $ abs (amnt dtx)) == detailedTxFee dtx = TxInternal
        | otherwise = TxOutbound

fillSoftPath :: Map Text SoftPath -> Endo DetailedTx
fillSoftPath addrMap =
    Endo $ \dtx ->
        dtx
            { detailedTxInbound = mergeSoftPath (detailedTxInbound dtx)
            , detailedTxMyInputs = mergeSoftPath (detailedTxMyInputs dtx)
            }
  where
    mergeSoftPath = Map.intersectionWith f addrMap
    f path (amnt, _) = (amnt, Just path)

fillUnsignedTx :: Network -> Tx -> Endo DetailedTx
fillUnsignedTx net tx = fillTx net tx <> go
  where
    go =
        Endo $ \dtx ->
            dtx
                { detailedTxHash = Nothing
                , detailedTxSize =
                      Just $
                      fromIntegral $
                      guessTxSize (length $ txIn tx) [] (length $ txOut tx) 0
                }

fillTx :: Network -> Tx -> Endo DetailedTx
fillTx net tx =
    Endo $ \dtx ->
        dtx
            { detailedTxOutbound = outbound dtx
            , detailedTxNonStdOutputs = nonStd
            , detailedTxHash = Just $ txHash tx
            , detailedTxSize =
                  Just $ fromIntegral $ BS.length $ Serialize.encode tx
            , detailedTxFee = detailedTxFee dtx <|> feeM dtx
            }
  where
    (outAddrMap, nonStd) = txOutAddressMap net $ txOut tx
    outbound dtx = Map.difference outAddrMap (detailedTxInbound dtx)
    outSum = fromIntegral $ sum $ outValue <$> txOut tx :: Natural
    myInSum dtx = sum $ fst <$> Map.elems (detailedTxMyInputs dtx) :: Natural
    othInSum dtx = sum $ Map.elems (detailedTxOtherInputs dtx) :: Natural
    feeM dtx = (myInSum dtx + othInSum dtx) `safeSubtract` outSum

fillInputs ::
       Network
    -> Map Text SoftPath
    -> [TxOut]
    -> Endo DetailedTx
fillInputs net walletAddrs txOs =
    Endo $ \dtx ->
        dtx
            { detailedTxMyInputs = Map.map (second Just) myValMap
            , detailedTxOtherInputs = othValMap
            }
  where
    valMap = fst $ txOutAddressMap net txOs
    myValMap = Map.intersectionWith (,) valMap walletAddrs
    othValMap = Map.difference valMap myValMap

fillInbound ::
       Network
    -> Map Text SoftPath
    -> [TxOut]
    -> Endo DetailedTx
fillInbound net walletAddrs txOs =
    Endo $ \dtx -> dtx {detailedTxInbound = Map.map (second Just) inboundMap}
  where
    (outValMap, _) = txOutAddressMap net txOs
    inboundMap = Map.intersectionWith (,) outValMap walletAddrs

txOutAddressMap :: Network -> [TxOut] -> (Map Text Natural, Natural)
txOutAddressMap net txout =
    (Map.fromListWith (+) rs, sum ls)
  where
    xs = (decodeTxOutAddr net &&& outValue) <$> txout
    (ls, rs) = partitionEithers $ partE <$> xs
    partE (Right a, v) = Right (a, fromIntegral v)
    partE (Left _, v)  = Left (fromIntegral v)

decodeTxOutAddr :: Network -> TxOut -> Either String Text
decodeTxOutAddr net to = do
    so <- decodeTxOutSO to
    addr <- maybeToEither err $ outputAddress so
    maybeToEither err $ addrToString net addr
  where
    err = "Could not decode Address in decodeTxOutAddr"

decodeTxOutSO :: TxOut -> Either String ScriptOutput
decodeTxOutSO = decodeOutputBS . scriptOutput

isExternal :: SoftPath -> Bool
isExternal (Deriv :/ 0 :/ _) = True
isExternal _                 = False

-}
