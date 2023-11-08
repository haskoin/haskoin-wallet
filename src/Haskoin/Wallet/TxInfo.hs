{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Haskoin.Wallet.TxInfo where

import Control.Arrow ((&&&))
import Control.Monad (unless)
import Data.Aeson (object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Json
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.String.ToString (ToString (..))
import Data.Text (Text)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

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
  { txInfoHash :: !(Maybe TxHash),
    txInfoType :: !TxType,
    txInfoAmount :: !Integer,
    txInfoMyOutputs :: !(Map Address MyOutputs),
    txInfoOtherOutputs :: !(Map Address Natural),
    txInfoNonStdOutputs :: ![Store.StoreOutput],
    txInfoMyInputs :: !(Map Address MyInputs),
    txInfoOtherInputs :: !(Map Address OtherInputs),
    txInfoNonStdInputs :: ![Store.StoreInput],
    txInfoSize :: !Natural,
    txInfoFee :: !Natural,
    txInfoFeeByte :: !Natural,
    txInfoBlockRef :: !Store.BlockRef,
    txInfoConfirmations :: !Natural,
    txInfoPending :: !(Maybe TxInfoPending)
  }
  deriving (Eq, Show)

data TxInfoPending = TxInfoPending
  { pendingNosigHash :: !TxHash,
    pendingSigned :: !Bool,
    pendingOnline :: !Bool
  }
  deriving (Eq, Show)

instance Json.ToJSON TxInfoPending where
  toJSON (TxInfoPending h s o) =
    object
      [ "nosighash" .= h,
        "signed" .= s,
        "online" .= o
      ]

instance Json.FromJSON TxInfoPending where
  parseJSON =
    withObject "TxInfoPending" $ \o -> do
      h <- o .: "nosighash"
      s <- o .: "signed"
      onl <- o .: "online"
      return $ TxInfoPending h s onl

data MyOutputs = MyOutputs
  { myOutputsValue :: !Natural,
    myOutputsPath :: !SoftPath,
    myOutputsLabel :: !Text
  }
  deriving (Eq, Show)

instance Json.ToJSON MyOutputs where
  toJSON (MyOutputs i p l) =
    object
      [ "value" .= i,
        "path" .= p,
        "label" .= l
      ]

instance Json.FromJSON MyOutputs where
  parseJSON =
    withObject "MyOutputs" $ \o -> do
      i <- o .: "value"
      p <- o .: "path"
      l <- o .: "label"
      return $ MyOutputs i p l

data MyInputs = MyInputs
  { myInputsValue :: !Natural,
    myInputsPath :: !SoftPath,
    myInputsLabel :: !Text,
    myInputsSigInput :: [SigInput]
  }
  deriving (Eq, Show)

instance MarshalJSON Ctx MyInputs where
  marshalValue ctx (MyInputs i p l s) =
    object $
      [ "value" .= i,
        "path" .= p,
        "label" .= l
      ]
        ++ ["siginput" .= (marshalValue ctx <$> s) | not (null s)]

  unmarshalValue ctx =
    withObject "MyInputs" $ \o -> do
      i <- o .: "value"
      p <- o .: "path"
      l <- o .: "label"
      sM <- o .:? "siginput"
      s <- unmarshalValue ctx `mapM` fromMaybe [] sM
      return $ MyInputs i p l s

data OtherInputs = OtherInputs
  { otherInputsValue :: !Natural,
    otherInputsSigInput :: [SigInput]
  }
  deriving (Eq, Show)

instance MarshalJSON Ctx OtherInputs where
  marshalValue ctx (OtherInputs i s) =
    object $
      ("value" .= i)
        : ["siginput" .= (marshalValue ctx <$> s) | not (null s)]

  unmarshalValue ctx =
    withObject "OtherInputs" $ \o -> do
      i <- o .: "value"
      sM <- o .:? "siginput"
      s <- unmarshalValue ctx `mapM` fromMaybe [] sM
      return $ OtherInputs i s

instance MarshalJSON (Network, Ctx) TxInfo where
  marshalValue (net, ctx) tx =
    object
      [ "txid" .= txInfoHash tx,
        "type" .= txInfoType tx,
        "amount" .= txInfoAmount tx,
        "myoutputs" .= mapAddrText net (txInfoMyOutputs tx),
        "otheroutputs" .= mapAddrText net (txInfoOtherOutputs tx),
        "nonstdoutputs" .= (marshalValue net <$> txInfoNonStdOutputs tx),
        "myinputs" .= marshalMap net ctx (txInfoMyInputs tx),
        "otherinputs" .= marshalMap net ctx (txInfoOtherInputs tx),
        "nonstdinputs" .= (marshalValue net <$> txInfoNonStdInputs tx),
        "size" .= txInfoSize tx,
        "fee" .= txInfoFee tx,
        "feebyte" .= txInfoFeeByte tx,
        "block" .= txInfoBlockRef tx,
        "confirmations" .= txInfoConfirmations tx,
        "pending" .= txInfoPending tx
      ]
  unmarshalValue (net, ctx) =
    Json.withObject "TxInfo" $ \o ->
      TxInfo
        <$> o .: "txid"
        <*> o .: "type"
        <*> o .: "amount"
        <*> (mapTextAddr net <$> o .: "myoutputs")
        <*> (mapTextAddr net <$> o .: "otheroutputs")
        <*> (mapM (unmarshalValue net) =<< o .: "nonstdoutputs")
        <*> (unmarshalMap net ctx =<< o .: "myinputs")
        <*> (unmarshalMap net ctx =<< o .: "otherinputs")
        <*> (mapM (unmarshalValue net) =<< o .: "nonstdinputs")
        <*> o .: "size"
        <*> o .: "fee"
        <*> o .: "feebyte"
        <*> o .: "block"
        <*> o .: "confirmations"
        <*> o .: "pending"

marshalMap ::
  (MarshalJSON Ctx v) =>
  Network ->
  Ctx ->
  Map Address v ->
  Map Text Json.Value
marshalMap net ctx m = mapAddrText net $ Map.map (marshalValue ctx) m

unmarshalMap ::
  (MarshalJSON Ctx v) =>
  Network ->
  Ctx ->
  Map Text Json.Value ->
  Parser (Map Address v)
unmarshalMap net ctx m = mapTextAddr net <$> (unmarshalValue ctx `mapM` m)

mapAddrText :: Network -> Map Address v -> Map Text v
mapAddrText net m =
  either error id $ do
    let f (a, v) = (,v) <$> addrToTextE net a
    Map.fromList <$> mapM f (Map.assocs m)

mapTextAddr :: Network -> Map Text v -> Map Address v
mapTextAddr net m =
  either error id $ do
    let f (a, v) = (,v) <$> textToAddrE net a
    Map.fromList <$> mapM f (Map.assocs m)

storeToTxInfo ::
  Map Address SoftPath -> Natural -> Store.Transaction -> TxInfo
storeToTxInfo walletAddrs currHeight sTx =
  TxInfo
    { txInfoHash = Just sTx.txid,
      txInfoType = txType amount fee,
      txInfoAmount = amount,
      txInfoMyOutputs = Map.map (\(v, p) -> MyOutputs v p "") myOutputsMap,
      txInfoOtherOutputs = othOutputsMap,
      txInfoNonStdOutputs = nonStdOut,
      txInfoMyInputs = Map.map (\(v, p) -> MyInputs v p "" []) myInputsMap,
      txInfoOtherInputs = Map.map (`OtherInputs` []) othInputsMap,
      txInfoNonStdInputs = nonStdIn,
      txInfoSize = size,
      txInfoFee = fee,
      txInfoFeeByte = feeByte,
      txInfoBlockRef = sTx.block,
      txInfoConfirmations = fromMaybe 0 confM,
      txInfoPending = Nothing
    }
  where
    size = fromIntegral sTx.size :: Natural
    fee = fromIntegral sTx.fee :: Natural
    feeByte = fee `div` size
    (outputMap, nonStdOut) = outputAddressMap sTx.outputs
    myOutputsMap = Map.intersectionWith (,) outputMap walletAddrs
    othOutputsMap = Map.difference outputMap walletAddrs
    (inputMap, nonStdIn) = inputAddressMap sTx.inputs
    myInputsMap = Map.intersectionWith (,) inputMap walletAddrs
    othInputsMap = Map.difference inputMap walletAddrs
    myOutputsSum =
      fromIntegral $ sum $ fst <$> Map.elems myOutputsMap :: Integer
    myInputsSum = fromIntegral $ sum $ fst <$> Map.elems myInputsMap :: Integer
    amount = myOutputsSum - myInputsSum
    confM =
      case sTx.block of
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

txOutAddressMap :: Ctx -> [TxOut] -> (Map Address Natural, [TxOut])
txOutAddressMap ctx outs =
  (Map.fromListWith (+) rs, ls)
  where
    (ls, rs) = partitionEithers $ f <$> outs
    f to@(TxOut v s) =
      case scriptToAddressBS ctx s of
        Left _ -> Left to
        Right a -> Right (a, fromIntegral v)

txInAddressMap ::
  Network ->
  Ctx ->
  [(OutPoint, TxOut)] ->
  (Map Address (Natural, [SigInput]), [(OutPoint, TxOut)])
txInAddressMap net ctx ins =
  (Map.fromListWith (\(a, b) (c, d) -> (a + c, b <> d)) rs, mconcat ls)
  where
    sh = maybeSetForkId net sigHashAll
    (ls, rs) = partitionEithers $ f <$> ins
    f (op, to@(TxOut v s)) =
      case scriptToAddressBS ctx s of
        Left _ -> Left [(op, to)]
        Right a ->
          Right
            ( a,
              ( fromIntegral v,
                [SigInput (addressToOutput a) v op sh Nothing]
              )
            )

maybeSetForkId :: Network -> SigHash -> SigHash
maybeSetForkId net
  | isJust net.sigHashForkId = setForkIdFlag
  | otherwise = id

parseTxSignData ::
  Network ->
  Ctx ->
  XPubKey ->
  TxSignData ->
  Either String TxInfo
parseTxSignData net ctx pubkey tsd@(TxSignData tx _ inPaths outPaths signed) = do
  coins <- txSignDataCoins tsd
  -- Fees
  let outSum = fromIntegral $ sum $ (.value) <$> tx.outputs :: Natural
      inSum = fromIntegral $ sum $ (.value) . snd <$> coins :: Natural
  fee <- maybeToEither "Fee is negative" $ inSum `safeSubtract` outSum
  let feeByte = fee `div` fromIntegral size
      -- Outputs
      (outputMap, nonStdOut) = txOutAddressMap ctx tx.outputs
      myOutputsMap = Map.intersectionWith (,) outputMap outPathAddrs
      othOutputsMap = Map.difference outputMap outPathAddrs
      -- Inputs
      (inputMap, nonStdIn) = txInAddressMap net ctx coins
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
  unless (length coins == length tx.inputs) $
    Left "Referenced input transactions are missing"
  unless (length inPaths == Map.size myInputsMap) $
    Left "Input derivations don't match the transaction inputs"
  unless (length outPaths == Map.size myOutputsMap) $
    Left "Output derivations don't match the transaction outputs"
  return $
    TxInfo
      { txInfoHash = if signed then Just $ txHash tx else Nothing,
        txInfoType = txType amount fee,
        txInfoAmount = amount,
        txInfoMyOutputs =
          Map.map (\(v, p) -> MyOutputs v p "") myOutputsMap,
        txInfoOtherOutputs = othOutputsMap,
        txInfoNonStdOutputs = [],
        txInfoMyInputs =
          Map.map (\(i, p, s) -> MyInputs i p "" s) myInputsMap,
        txInfoOtherInputs = Map.map (uncurry OtherInputs) othInputsMap,
        txInfoNonStdInputs = [],
        txInfoSize = fromIntegral size,
        txInfoFee = fee,
        txInfoFeeByte = feeByte,
        txInfoBlockRef = Store.MemRef 0,
        txInfoConfirmations = 0,
        txInfoPending = Just $ TxInfoPending (nosigTxHash tx) signed False
      }
  where
    inPathAddrs = Map.fromList $ (pathToAddr ctx pubkey &&& id) <$> inPaths
    outPathAddrs = Map.fromList $ (pathToAddr ctx pubkey &&& id) <$> outPaths
    size
      | signed = BS.length $ S.encode tx
      | otherwise = guessTxSize (length tx.inputs) [] (length tx.outputs) 0

txSignDataCoins :: TxSignData -> Either String [(OutPoint, TxOut)]
txSignDataCoins (TxSignData tx depTxs _ _ _) =
  maybeToEither "Referenced input transactions are missing" $ mapM f ops
  where
    ops = (.outpoint) <$> tx.inputs :: [OutPoint]
    txMap =
      Map.fromList $ (txHash &&& (.outputs)) <$> depTxs :: Map TxHash [TxOut]
    f :: OutPoint -> Maybe (OutPoint, TxOut)
    f op@(OutPoint h i) =
      (op,) <$> ((!!? fromIntegral i) =<< Map.lookup h txMap)

txType :: Integer -> Natural -> TxType
txType amount fee
  | amount > 0 = TxCredit
  | abs amount == fromIntegral fee = TxInternal
  | otherwise = TxDebit

pathToAddr :: Ctx -> XPubKey -> SoftPath -> Address
pathToAddr ctx pk path = xPubAddr ctx $ derivePubPath ctx path pk
