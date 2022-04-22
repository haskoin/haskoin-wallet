{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.TxInfo where

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (unless)
import           Data.Aeson                    (object, (.:), (.=))
import qualified Data.Aeson                    as Json
import           Data.Aeson.Types              (Parser)
import qualified Data.ByteString               as BS
import           Data.Decimal                  as Decimal
import           Data.Either                   (partitionEithers)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe, isJust)
import qualified Data.Serialize                as S
import           Data.String.Conversions       (cs)
import           Data.String.ToString
import           Data.Text                     (Text)
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Keys
import           Haskoin.Script
import qualified Haskoin.Store.Data            as Store
import           Haskoin.Transaction
import           Haskoin.Util
import           Network.Haskoin.Wallet.FileIO
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural

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
            _ -> fail "Invalid TxType"

data TxInfo = TxInfo
    { txInfoId            :: !TxHash
    , txInfoType          :: !TxType
    , txInfoAmount        :: !Integer
    , txInfoMyOutputs     :: !(Map Address (Natural, SoftPath))
    , txInfoOtherOutputs  :: !(Map Address Natural)
    , txInfoNonStdOutputs :: ![Store.StoreOutput]
    , txInfoMyInputs      :: !(Map Address (Natural, SoftPath))
    , txInfoOtherInputs   :: !(Map Address Natural)
    , txInfoNonStdInputs  :: ![Store.StoreInput]
    , txInfoSize          :: !Natural
    , txInfoFee           :: !Natural
    , txInfoFeeByte       :: !Decimal
    , txInfoBlockRef      :: !Store.BlockRef
    , txInfoConfirmations :: !Natural
    } deriving (Eq, Show)

txInfoToJSON :: Network -> TxInfo -> Either String Json.Value
txInfoToJSON net tx = do
    myOutputsT <- mapAddrText net $ txInfoMyOutputs tx
    othOutputsT <- mapAddrText net $ txInfoOtherOutputs tx
    myInputsT <- mapAddrText net $ txInfoMyInputs tx
    othInputsT <- mapAddrText net $ txInfoOtherInputs tx
    return $
        object
            [ "txid" .= txInfoId tx
            , "type" .= txInfoType tx
            , "amount" .= txInfoAmount tx
            , "myoutputs" .= myOutputsT
            , "otheroutputs" .= othOutputsT
            , "nonstdoutputs" .=
              (Store.storeOutputToJSON net <$> txInfoNonStdOutputs tx)
            , "myinputs" .= myInputsT
            , "otherinputs" .= othInputsT
            , "nonstdinputs" .=
              (Store.storeInputToJSON net <$> txInfoNonStdInputs tx)
            , "size" .= txInfoSize tx
            , "fee" .= txInfoFee tx
            , "feebyte" .= show (txInfoFeeByte tx)
            , "block" .= txInfoBlockRef tx
            , "confirmations" .= txInfoConfirmations tx
            ]

txInfoParseJSON :: Network -> Json.Value -> Parser TxInfo
txInfoParseJSON net =
    Json.withObject "TxInfo" $ \o ->
        TxInfo
            <$> o .: "txid"
            <*> o .: "type"
            <*> o .: "amount"
            <*> (f =<< o .: "myoutputs")
            <*> (f =<< o .: "otheroutputs")
            <*> (mapM (Store.storeOutputParseJSON net) =<< o .: "nonstdoutputs")
            <*> (f =<< o .: "myinputs")
            <*> (f =<< o .: "otherinputs")
            <*> (mapM (Store.storeInputParseJSON net) =<< o .: "nonstdinputs")
            <*> o .: "size"
            <*> o .: "fee"
            <*> (read <$> o .: "feebyte")
            <*> o .: "block"
            <*> o .: "confirmations"
      where
        f = either fail return . mapTextAddr net

mapAddrText :: Network -> Map Address v -> Either String (Map Text v)
mapAddrText net m = do
    let f (a, v) = (, v) <$> addrToTextE net a
    Map.fromList <$> mapM f (Map.assocs m)

mapTextAddr :: Network -> Map Text v -> Either String (Map Address v)
mapTextAddr net m = do
    let f (a, v) = (, v) <$> textToAddrE net a
    Map.fromList <$> mapM f (Map.assocs m)

toTxInfo ::
       Map Address SoftPath -> Natural -> Store.Transaction -> TxInfo
toTxInfo walletAddrs currHeight sTx =
    TxInfo
        { txInfoId = Store.transactionId sTx
        , txInfoType = txType amount fee
        , txInfoAmount = amount
        , txInfoMyOutputs = myOutputsMap
        , txInfoOtherOutputs = othOutputsMap
        , txInfoNonStdOutputs = nonStdOut
        , txInfoMyInputs = myInputsMap
        , txInfoOtherInputs = othInputsMap
        , txInfoNonStdInputs = nonStdIn
        , txInfoSize = size
        , txInfoFee = fee
        , txInfoFeeByte = feeByte
        , txInfoBlockRef = Store.transactionBlock sTx
        , txInfoConfirmations = fromMaybe 0 confM
        }
  where
    size = fromIntegral $ Store.transactionSize sTx :: Natural
    fee = fromIntegral $ Store.transactionFees sTx :: Natural
    feeByte = Decimal.roundTo 2 $ fromIntegral fee / fromIntegral size
    (outputMap, nonStdOut) = outputAddressMap $ Store.transactionOutputs sTx
    myOutputsMap = Map.intersectionWith (,) outputMap walletAddrs
    othOutputsMap = Map.difference outputMap walletAddrs
    (inputMap, nonStdIn) = inputAddressMap $ Store.transactionInputs sTx
    myInputsMap = Map.intersectionWith (,) inputMap walletAddrs
    othInputsMap = Map.difference inputMap walletAddrs
    myOutputsSum =
        fromIntegral $ sum $ fst <$> Map.elems myOutputsMap :: Integer
    myInputsSum = fromIntegral $ sum $ fst <$> Map.elems myInputsMap :: Integer
    amount = myOutputsSum - myInputsSum
    confM =
        case Store.transactionBlock sTx of
            Store.MemRef _ -> Nothing
            Store.BlockRef height _ ->
                (+ 1) <$> currHeight `safeSubtract` fromIntegral height

{- Helpers for building address maps -}

outputAddressMap ::
       [Store.StoreOutput] -> (Map Address Natural, [Store.StoreOutput])
outputAddressMap outs =
    (Map.fromListWith (+) rs, ls)
  where
    (ls, rs) = partitionEithers $ f <$> outs
    f (Store.StoreOutput v _ _ (Just a)) = Right (a, fromIntegral v)
    f s                                  = Left s

inputAddressMap ::
       [Store.StoreInput] -> (Map Address Natural, [Store.StoreInput])
inputAddressMap ins =
    (Map.fromListWith (+) rs, ls)
  where
    (ls, rs) = partitionEithers $ f <$> ins
    f (Store.StoreInput _ _ _ _ v _ (Just a)) = Right (a, fromIntegral v)
    f s                                       = Left s

txOutAddressMap :: [TxOut] -> (Map Address Natural, [TxOut])
txOutAddressMap outs =
    (Map.fromListWith (+) rs, ls)
  where
    (ls, rs) = partitionEithers $ f <$> outs
    f to@(TxOut v s) =
        case scriptToAddressBS s of
            Left _  -> Left to
            Right a -> Right (a, fromIntegral v)

txInAddressMap ::
       Network
    -> [(OutPoint, TxOut)]
    -> (Map Address (Natural, [SigInput]), [(OutPoint, TxOut)])
txInAddressMap net ins =
    (Map.fromListWith (\(a, b) (c, d) -> (a + c, b <> d)) rs, mconcat ls)
  where
    sh = maybeSetForkId net sigHashAll
    (ls, rs) = partitionEithers $ f <$> ins
    f (op, to@(TxOut v s)) =
        case scriptToAddressBS s of
            Left _ -> Left [(op, to)]
            Right a ->
                Right
                    ( a
                    , ( fromIntegral v
                      , [SigInput (addressToOutput a) v op sh Nothing]))

maybeSetForkId :: Network -> SigHash -> SigHash
maybeSetForkId net
    | isJust (getSigHashForkId net) = setForkIdFlag
    | otherwise = id

{- Unsigned Transactions -}

data UnsignedTxInfo = UnsignedTxInfo
    { unsignedTxInfoType         :: !TxType
    , unsignedTxInfoAmount       :: !Integer
    , unsignedTxInfoMyOutputs    :: !(Map Address (Natural, SoftPath))
    , unsignedTxInfoOtherOutputs :: !(Map Address Natural)
    , unsignedTxInfoMyInputs     :: !(Map Address (Natural, SoftPath, [SigInput]))
    , unsignedTxInfoOtherInputs  :: !(Map Address (Natural, [SigInput]))
    , unsignedTxInfoSize         :: !Natural
    , unsignedTxInfoFee          :: !Natural
    , unsignedTxInfoFeeByte      :: !Decimal
    } deriving (Eq, Show)

unsignedToTxInfo :: Tx -> UnsignedTxInfo -> TxInfo
unsignedToTxInfo tx uTx =
    TxInfo
        { txInfoId = txHash tx
        , txInfoType = unsignedTxInfoType uTx
        , txInfoAmount = unsignedTxInfoAmount uTx
        , txInfoMyOutputs = unsignedTxInfoMyOutputs uTx
        , txInfoOtherOutputs = unsignedTxInfoOtherOutputs uTx
        , txInfoNonStdOutputs = []
        , txInfoMyInputs = myInputs
        , txInfoOtherInputs = Map.map fst $ unsignedTxInfoOtherInputs uTx
        , txInfoNonStdInputs = []
        , txInfoSize = fromIntegral size
        , txInfoFee = unsignedTxInfoFee uTx
        , txInfoFeeByte = feeByte
        , txInfoBlockRef = Store.MemRef 0
        , txInfoConfirmations = 0
        }
  where
    size = BS.length $ S.encode tx
    feeByte =
        Decimal.roundTo 2 $
        fromIntegral (unsignedTxInfoFee uTx) / fromIntegral size
    myInputs = Map.map f $ unsignedTxInfoMyInputs uTx
    f (n,p,_) = (n,p)

unsignedTxInfoToJSON ::
       Network -> UnsignedTxInfo -> Either String Json.Value
unsignedTxInfoToJSON net tx = do
    myOutputsT <- mapAddrText net $ unsignedTxInfoMyOutputs tx
    othOutputsT <- mapAddrText net $ unsignedTxInfoOtherOutputs tx
    myInputsT <- mapAddrText net $ unsignedTxInfoMyInputs tx
    othInputsT <- mapAddrText net $ unsignedTxInfoOtherInputs tx
    return $
        object
            [ "type" .= unsignedTxInfoType tx
            , "amount" .= unsignedTxInfoAmount tx
            , "myoutputs" .= myOutputsT
            , "otheroutputs" .= othOutputsT
            , "myinputs" .= myInputsT
            , "otherinputs" .= othInputsT
            , "size" .= unsignedTxInfoSize tx
            , "fee" .= unsignedTxInfoFee tx
            , "feebyte" .= show (unsignedTxInfoFeeByte tx)
            ]

unsignedTxInfoParseJSON :: Network -> Json.Value -> Parser UnsignedTxInfo
unsignedTxInfoParseJSON net =
    Json.withObject "UnsignedTxInfo" $ \o ->
        UnsignedTxInfo
            <$> o .: "type"
            <*> o .: "amount"
            <*> (f =<< o .: "myoutputs")
            <*> (f =<< o .: "otheroutputs")
            <*> (f =<< o .: "myinputs")
            <*> (f =<< o .: "otherInputs")
            <*> o .: "size"
            <*> o .: "fee"
            <*> (read <$> o .: "feebyte")
      where
        f = either fail return . mapTextAddr net

parseTxSignData ::
       Network
    -> XPubKey
    -> TxSignData
    -> Either String UnsignedTxInfo
parseTxSignData net pubkey tsd@(TxSignData tx _ inPaths outPaths _ _ _) = do
    coins <- txSignDataCoins tsd
        -- Fees
    let outSum = fromIntegral $ sum $ outValue <$> txOut tx :: Natural
        inSum = fromIntegral $ sum $ outValue . snd <$> coins :: Natural
    fee <- maybeToEither "Fee is negative" $ inSum `safeSubtract` outSum
    let feeByte = Decimal.roundTo 2 $ fromIntegral fee / fromIntegral size
        -- Outputs
        (outputMap, nonStdOut) = txOutAddressMap $ txOut tx
        myOutputsMap = Map.intersectionWith (,) outputMap outPathAddrs
        othOutputsMap = Map.difference outputMap outPathAddrs
        -- Inputs
        (inputMap, nonStdIn) = txInAddressMap net coins
        myInputsMap =
            Map.intersectionWith (\(n, xs) p -> (n, p, xs)) inputMap inPathAddrs
        othInputsMap = Map.difference inputMap inPathAddrs
        -- Amounts
        myOutputsSum =
            fromIntegral $ sum $ fst <$> Map.elems myOutputsMap :: Integer
        myInputsSum =
            fromIntegral $ sum $ fst3 <$> Map.elems myInputsMap :: Integer
        amount = myOutputsSum - myInputsSum
        -- Sanity checks
    unless (null nonStdOut) $
        Left "There are non-standard outputs in the transaction"
    unless (null nonStdIn) $
        Left "There are non-standard inputs in the transaction"
    unless (length coins == length (txIn tx)) $
        Left "Referenced input transactions are missing"
    unless (length inPaths == Map.size myInputsMap) $
        Left "Private key derivations don't match the transaction inputs"
    unless (length outPaths == Map.size myOutputsMap) $
        Left "Output derivations don't match your transaction outputs"
    return $
        UnsignedTxInfo
            { unsignedTxInfoType = txType amount fee
            , unsignedTxInfoAmount = amount
            , unsignedTxInfoMyOutputs = myOutputsMap
            , unsignedTxInfoOtherOutputs = othOutputsMap
            , unsignedTxInfoMyInputs = myInputsMap
            , unsignedTxInfoOtherInputs = othInputsMap
            , unsignedTxInfoSize = fromIntegral size -- estimate
            , unsignedTxInfoFee = fee
            , unsignedTxInfoFeeByte = feeByte --estimate
            }
  where
    inPathAddrs = Map.fromList $ (pathToAddr pubkey &&& id) <$> inPaths
    outPathAddrs = Map.fromList $ (pathToAddr pubkey &&& id) <$> outPaths
    size = guessTxSize (length $ txIn tx) [] (length $ txOut tx) 0

txSignDataCoins :: TxSignData -> Either String [(OutPoint, TxOut)]
txSignDataCoins (TxSignData tx depTxs _ _ _ _ _) =
    maybeToEither "Referenced input transactions are missing" $ mapM f ops
  where
    ops = prevOutput <$> txIn tx :: [OutPoint]
    txMap = Map.fromList $ (txHash &&& txOut) <$> depTxs :: Map TxHash [TxOut]
    f :: OutPoint -> Maybe (OutPoint, TxOut)
    f op@(OutPoint h i) =
        (op, ) <$> ((!!? fromIntegral i) =<< Map.lookup h txMap)

txType :: Integer -> Natural -> TxType
txType amount fee
    | amount > 0 = TxCredit
    | abs amount == fromIntegral fee = TxInternal
    | otherwise = TxDebit

pathToAddr :: XPubKey -> SoftPath -> Address
pathToAddr pubKey = xPubAddr . (`derivePubPath` pubKey)

