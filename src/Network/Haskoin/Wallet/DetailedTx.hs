{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Haskoin.Wallet.DetailedTx where

import           Control.Applicative             ((<|>))
import           Control.Arrow                   (second, (&&&))
import qualified Data.Aeson                      as Json
import           Data.Aeson.TH
import qualified Data.ByteString                 as BS
import           Data.Decimal
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
import           Network.Haskoin.Address
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.Doc
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Options.Applicative.Help.Pretty

data TxType = TxInbound | TxInternal | TxOutbound
    deriving (Show, Eq)

instance ToString TxType where
    toString =
        \case
            TxInbound -> "inbound"
            TxInternal -> "internal"
            TxOutbound -> "outbound"

instance Json.ToJSON TxType where
    toJSON = Json.String . cs . toString

instance Json.FromJSON TxType where
    parseJSON =
        Json.withText "txtype" $ \case
            "inbound" -> return TxInbound
            "internal" -> return TxInternal
            "outbound" -> return TxOutbound

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
