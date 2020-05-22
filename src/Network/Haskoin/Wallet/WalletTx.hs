{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.WalletTx where

import           Control.Arrow                   ((&&&))
import           Control.Monad                   (unless)
import           Data.Aeson                      (object, (.:), (.=))
import qualified Data.Aeson                      as Json
import           Data.Aeson.Types                (Parser)
import qualified Data.ByteString                 as BS
import           Data.Decimal                    as Decimal
import           Data.Either                     (partitionEithers)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe, isJust)
import qualified Data.Serialize                  as S
import           Data.String.Conversions         (cs)
import           Data.String.ToString
import           Data.Text                       (Text)
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Keys
import           Haskoin.Script
import qualified Haskoin.Store.Data              as Store
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

data WalletTx = WalletTx
    { walletTxId            :: TxHash
    , walletTxType          :: TxType
    , walletTxAmount        :: Integer
    , walletTxMyOutputs     :: Map Address (Natural, SoftPath)
    , walletTxOtherOutputs  :: Map Address Natural
    , walletTxNonStdOutputs :: [Store.StoreOutput]
    , walletTxMyInputs      :: Map Address (Natural, SoftPath)
    , walletTxOtherInputs   :: Map Address Natural
    , walletTxNonStdInputs  :: [Store.StoreInput]
    , walletTxSize          :: Natural
    , walletTxFee           :: Natural
    , walletTxFeeByte       :: Decimal
    , walletTxBlockRef      :: Store.BlockRef
    , walletTxConfirmations :: Natural
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
            , "nonstdoutputs" .=
              (Store.storeOutputToJSON net <$> walletTxNonStdOutputs tx)
            , "myinputs" .= myInputsT
            , "otherinputs" .= othInputsT
            , "nonstdinputs" .=
              (Store.storeInputToJSON net <$> walletTxNonStdInputs tx)
            , "size" .= walletTxSize tx
            , "fee" .= walletTxFee tx
            , "feebyte" .= show (walletTxFeeByte tx)
            , "block" .= walletTxBlockRef tx
            , "confirmations" .= walletTxConfirmations tx
            ]

walletTxParseJSON :: Network -> Json.Value -> Parser WalletTx
walletTxParseJSON net =
    Json.withObject "wallettx" $ \o ->
        WalletTx
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

toWalletTx ::
       Map Address SoftPath -> Natural -> Store.Transaction -> WalletTx
toWalletTx walletAddrs currHeight sTx =
    WalletTx
        { walletTxId = Store.transactionId sTx
        , walletTxType = txType amount fee
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
        , walletTxConfirmations = fromMaybe 0 confM
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
    f s = Left s

inputAddressMap ::
       [Store.StoreInput] -> (Map Address Natural, [Store.StoreInput])
inputAddressMap ins =
    (Map.fromListWith (+) rs, ls)
  where
    (ls, rs) = partitionEithers $ f <$> ins
    f (Store.StoreInput _ _ _ _ v _ (Just a)) = Right (a, fromIntegral v)
    f s = Left s

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

data WalletUnsignedTx = WalletUnsignedTx
    { walletUnsignedTxType         :: TxType
    , walletUnsignedTxAmount       :: Integer
    , walletUnsignedTxMyOutputs    :: Map Address (Natural, SoftPath)
    , walletUnsignedTxOtherOutputs :: Map Address Natural
    , walletUnsignedTxMyInputs     :: Map Address ( Natural
                                                  , SoftPath
                                                  , [SigInput])
    , walletUnsignedTxSize         :: Natural
    , walletUnsignedTxFee          :: Natural
    , walletUnsignedTxFeeByte      :: Decimal
    } deriving (Eq, Show)

unsignedToWalletTx :: Tx -> WalletUnsignedTx -> WalletTx
unsignedToWalletTx tx uTx =
    WalletTx
        { walletTxId = txHash tx
        , walletTxType = walletUnsignedTxType uTx
        , walletTxAmount = walletUnsignedTxAmount uTx
        , walletTxMyOutputs = walletUnsignedTxMyOutputs uTx
        , walletTxOtherOutputs = walletUnsignedTxOtherOutputs uTx
        , walletTxNonStdOutputs = []
        , walletTxMyInputs = myInputs
        , walletTxOtherInputs = Map.empty
        , walletTxNonStdInputs = []
        , walletTxSize = fromIntegral size
        , walletTxFee = walletUnsignedTxFee uTx
        , walletTxFeeByte = feeByte
        , walletTxBlockRef = Store.MemRef 0
        , walletTxConfirmations = 0
        }
  where
    size = BS.length $ S.encode tx
    feeByte =
        Decimal.roundTo 2 $
        fromIntegral (walletUnsignedTxFee uTx) / fromIntegral size
    myInputs = Map.map f $ walletUnsignedTxMyInputs uTx
    f (n,p,_) = (n,p)

walletUnsignedTxToJSON ::
       Network -> WalletUnsignedTx -> Either String Json.Value
walletUnsignedTxToJSON net tx = do
    myOutputsT <- mapAddrText net $ walletUnsignedTxMyOutputs tx
    othOutputsT <- mapAddrText net $ walletUnsignedTxOtherOutputs tx
    myInputsT <- mapAddrText net $ walletUnsignedTxMyInputs tx
    return $
        object
            [ "type" .= walletUnsignedTxType tx
            , "amount" .= walletUnsignedTxAmount tx
            , "myoutputs" .= myOutputsT
            , "otheroutputs" .= othOutputsT
            , "myinputs" .= myInputsT
            , "size" .= walletUnsignedTxSize tx
            , "fee" .= walletUnsignedTxFee tx
            , "feebyte" .= show (walletUnsignedTxFeeByte tx)
            ]

walletUnsignedTxParseJSON :: Network -> Json.Value -> Parser WalletUnsignedTx
walletUnsignedTxParseJSON net =
    Json.withObject "walletunsignedtx" $ \o ->
        WalletUnsignedTx
            <$> o .: "type"
            <*> o .: "amount"
            <*> (f =<< o .: "myoutputs")
            <*> (f =<< o .: "otheroutputs")
            <*> (f =<< o .: "myinputs")
            <*> o .: "size"
            <*> o .: "fee"
            <*> (read <$> o .: "feebyte")
      where
        f = either fail return . mapTextAddr net

parseTxSignData ::
       Network -> XPubKey -> TxSignData -> Either String WalletUnsignedTx
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
    unless (Map.null othInputsMap) $
        Left
            "There are inputs in the transaction that don't belong to the wallet"
    unless (length coins == length (txIn tx)) $
        Left "Referenced input transactions are missing"
    unless (length inPaths == Map.size myInputsMap) $
        Left "Private key derivations don't match the transaction inputs"
    unless (length outPaths == Map.size myOutputsMap) $
        Left "Output derivations don't match your transaction outputs"
    return $
        WalletUnsignedTx
            { walletUnsignedTxType = txType amount fee
            , walletUnsignedTxAmount = amount
            , walletUnsignedTxMyOutputs = myOutputsMap
            , walletUnsignedTxOtherOutputs = othOutputsMap
            , walletUnsignedTxMyInputs = myInputsMap
            , walletUnsignedTxSize = fromIntegral size -- estimate
            , walletUnsignedTxFee = fee
            , walletUnsignedTxFeeByte = feeByte --estimate
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

