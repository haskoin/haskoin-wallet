{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections   #-}
module Network.Haskoin.Wallet.TxInformation where

import           Control.Arrow                           ((&&&))
import           Data.Decimal
import           Data.List                               (sortOn, sum)
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import           Foundation
import           Foundation.Collection
import           Network.Haskoin.Block                   hiding (blockHashToHex)
import           Network.Haskoin.Crypto                  hiding (addrToBase58)
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction             hiding (txHashToHex)
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.ConsolePrinter
import           Network.Haskoin.Wallet.FoundationCompat
import qualified Prelude

data TxInformation = TxInformation
    { txInfoTxHash      :: Maybe TxHash
    , txInfoTxSize      :: Maybe (CountOf (Element (UArray Word8)))
    , txInfoOutbound    :: Map Address Satoshi
    , txInfoNonStd      :: Satoshi
    , txInfoInbound     :: Map Address (Satoshi, Maybe SoftPath)
    , txInfoMyInputs    :: Map Address (Satoshi, Maybe SoftPath)
    , txInfoOtherInputs :: Map Address Satoshi
    , txInfoFee         :: Maybe Satoshi
    , txInfoHeight      :: Maybe Natural
    , txInfoBlockHash   :: Maybe BlockHash
    } deriving (Eq, Show)

data AddressTx = AddressTx
    { addrTxAddress   :: !Address
    , addrTxTxHash    :: !TxHash
    , addrTxAmount    :: !Integer
    , addrTxHeight    :: Maybe Natural
    , addrTxBlockHash :: Maybe BlockHash
    }
    deriving (Eq, Show)

emptyTxInfo :: TxInformation
emptyTxInfo =
    TxInformation
    { txInfoTxHash = Nothing
    , txInfoTxSize = Nothing
    , txInfoOutbound = Map.empty
    , txInfoNonStd = 0
    , txInfoInbound = Map.empty
    , txInfoMyInputs = Map.empty
    , txInfoOtherInputs = Map.empty
    , txInfoFee = Nothing
    , txInfoHeight = Nothing
    , txInfoBlockHash = Nothing
    }

mergeAddressTxs :: [AddressTx] -> [TxInformation]
mergeAddressTxs as =
    Map.elems $ Map.mapMaybeWithKey toMvt aMap
  where
    aMap = Map.fromListWith (<>) $ (addrTxTxHash &&& (:[])) <$> as
    toMvt :: TxHash -> [AddressTx] -> Maybe TxInformation
    toMvt tid atxs =
        case head <$> nonEmpty atxs of
            Just a ->
                let (os, is) = partition ((< 0) . addrTxAmount) atxs
                in Just
                       TxInformation
                       { txInfoTxHash = Just tid
                       , txInfoTxSize = Nothing
                       , txInfoOutbound = Map.empty
                       , txInfoNonStd = 0
                       , txInfoInbound = toAddrMap is
                       , txInfoMyInputs = toAddrMap os
                       , txInfoOtherInputs = Map.empty
                       , txInfoFee = Nothing
                       , txInfoHeight = addrTxHeight a
                       , txInfoBlockHash = addrTxBlockHash a
                       }
            _ -> Nothing
    toAddrMap :: [AddressTx] -> Map Address (Satoshi, Maybe SoftPath)
    toAddrMap = Map.map (, Nothing) . Map.fromListWith (+) . fmap toAddrVal
    toAddrVal :: AddressTx -> (Address, Satoshi)
    toAddrVal = addrTxAddress &&& fromIntegral . abs . addrTxAmount

txInfoAmount :: TxInformation -> Integer
txInfoAmount TxInformation {..} =
    toInteger inboundSum - toInteger myCoinsSum
  where
    inboundSum = sum $ fmap fst $ Map.elems txInfoInbound :: Satoshi
    myCoinsSum = sum $ fmap fst $ Map.elems txInfoMyInputs :: Satoshi

txInfoFeeByte :: TxInformation -> Maybe Decimal
txInfoFeeByte TxInformation {..} = do
    sat <- feeDecimalM
    bytes <- sizeDecimalM
    return $ roundTo 2 $ sat Prelude./ bytes
  where
    feeDecimalM = fromIntegral <$> txInfoFee :: Maybe Decimal
    sizeDecimalM =
        fromIntegral . fromCount <$> txInfoTxSize :: Maybe Decimal

txInfoTxType :: TxInformation -> String
txInfoTxType s
    | amnt > 0 = "Inbound"
    | Just (fromIntegral $ abs amnt) == txInfoFee s = "Self"
    | otherwise = "Outbound"
  where
    amnt = txInfoAmount s

txInfoFillPath :: Map Address SoftPath -> TxInformation -> TxInformation
txInfoFillPath addrMap txInfo =
    txInfo
    { txInfoInbound = mergeSoftPath (txInfoInbound txInfo)
    , txInfoMyInputs = mergeSoftPath (txInfoMyInputs txInfo)
    }
  where
    mergeSoftPath = Map.intersectionWith f addrMap
    f path (amnt, _) = (amnt, Just path)

txInfoFillUnsignedTx :: Tx -> TxInformation -> TxInformation
txInfoFillUnsignedTx tx txInfo =
    (txInfoFillTx tx txInfo)
    { txInfoTxHash = Nothing
    , txInfoTxSize =
          Just $
          toCount $
          guessTxSize
              (fromCount $ length $ txIn tx)
              []
              (fromCount $ length $ txOut tx)
              0
    }

txInfoFillTx :: Tx -> TxInformation -> TxInformation
txInfoFillTx tx txInfo =
    txInfo
    { txInfoOutbound = outbound
    , txInfoNonStd = nonStd
    , txInfoTxHash = Just $ txHash tx
    , txInfoTxSize = Just $ length $ encodeBytes tx
    , txInfoFee = txInfoFee txInfo <|> feeM
    }
  where
    (outAddrMap, nonStd) = txOutAddressMap $ txOut tx
    outbound = Map.difference outAddrMap (txInfoInbound txInfo)
    outSum = sum $ (toNatural . outValue) <$> txOut tx :: Natural
    myInSum =
        sum $ fst <$> Map.elems (txInfoMyInputs txInfo) :: Natural
    othInSum =
        sum $ Map.elems (txInfoOtherInputs txInfo) :: Natural
    feeM = myInSum + othInSum - outSum :: Maybe Natural

txInfoFillInputs ::
       Map Address SoftPath
    -> [TxOut]
    -> TxInformation
    -> TxInformation
txInfoFillInputs walletAddrs txOs txInf =
    txInf
    { txInfoMyInputs = Map.map (second Just) myValMap
    , txInfoOtherInputs = othValMap
    }
  where
    valMap = fst $ txOutAddressMap txOs
    myValMap = Map.intersectionWith (,) valMap walletAddrs
    othValMap = Map.difference valMap myValMap

txInfoFillInbound ::
       Map Address SoftPath -> [TxOut] -> TxInformation -> TxInformation
txInfoFillInbound walletAddrs txOs txInf =
    txInf { txInfoInbound = Map.map (second Just) inboundMap }
  where
    (outValMap, _) = txOutAddressMap txOs
    inboundMap = Map.intersectionWith (,) outValMap walletAddrs

txOutAddressMap :: [TxOut] -> (Map Address Satoshi, Satoshi)
txOutAddressMap txout =
    (Map.fromListWith (+) rs, sum ls)
  where
    xs = fmap (decodeTxOutAddr &&& toNatural . outValue) txout
    (ls, rs) = partitionEithers $ fmap partE xs
    partE (Right a, v) = Right (a, v)
    partE (Left _, v)  = Left v

decodeTxOutAddr :: TxOut -> Either String Address
decodeTxOutAddr = decodeTxOutSO >=> eitherString . outputAddress

decodeTxOutSO :: TxOut -> Either String ScriptOutput
decodeTxOutSO = eitherString . decodeOutputBS . scriptOutput

isExternal :: SoftPath -> Bool
isExternal (Deriv :/ 0 :/ _) = True
isExternal _                 = False

txInfoFormatCompact ::
       HardPath
    -> AmountUnit
    -> Maybe Bool
    -> Maybe Natural
    -> TxInformation
    -> ConsolePrinter
txInfoFormatCompact _ unit _ heightM s@TxInformation {..} =
    vcat [title <+> confs, nest 4 $ vcat [txid, outbound, self, inbound]]
  where
    title =
        case txInfoTxType s of
            "Outbound" -> formatTitle "Outbound Payment"
            "Inbound"  -> formatTitle "Inbound Payment"
            "Self"     -> formatTitle "Payment To Yourself"
            _          -> consoleError $ formatError "Invalid tx type"
    confs =
        case heightM of
            Just currHeight ->
                case (currHeight -) =<< txInfoHeight of
                    Just conf ->
                        formatStatic $
                        "(" <> show (conf + 1) <> " confirmations)"
                    _ -> formatStatic "(Pending)"
            _ -> mempty
    txid = maybe mempty (formatTxHash . txHashToHex) txInfoTxHash
    outbound
        | txInfoTxType s /= "Outbound" = mempty
        | txInfoNonStd == 0 && Map.null txInfoOutbound = mempty
        | otherwise =
            vcat $
            [feeKey] <> fmap (addrFormat negate) (Map.assocs txInfoOutbound) <>
            [nonStdRcp]
    nonStdRcp
        | txInfoNonStd == 0 = mempty
        | otherwise =
            formatStatic "Non-standard recipients:" <+>
            formatIntegerAmount unit (fromIntegral txInfoNonStd)
    feeKey =
        case txInfoFee of
            Just fee ->
                formatKey "Fees:" <+>
                formatIntegerAmountWith formatFee unit (fromIntegral fee)
            _ -> mempty
    self
        | txInfoTxType s /= "Self" = mempty
        | otherwise = feeKey
    inbound
        | txInfoTxType s /= "Inbound" = mempty
        | Map.null txInfoInbound = mempty
        | otherwise =
            vcat $
            [ if Map.size txInfoInbound > 1
                  then formatKey "Total amount:" <+>
                       formatIntegerAmount unit (txInfoAmount s)
                  else mempty
            ] <>
            fmap (addrFormat id) (Map.assocs $ Map.map fst txInfoInbound)
    addrFormat f (a, v) =
        formatAddress (addrToBase58 a) <> formatStatic ":" <+>
        formatIntegerAmount unit (f $ fromIntegral v)

txInfoFormat ::
       HardPath
    -> AmountUnit
    -> Maybe Bool
    -> Maybe Natural
    -> TxInformation
    -> ConsolePrinter
txInfoFormat accDeriv unit txSignedM heightM s@TxInformation {..} =
    vcat [information, nest 2 $ vcat [outbound, inbound, myInputs, otherInputs]]
  where
    information =
        vcat
            [ formatTitle "Tx Information"
            , nest 4 $
              vcat
                  [ formatKey (block 15 "Tx Type:") <>
                    formatStatic (txInfoTxType s)
                  , case txInfoTxHash of
                        Just tid ->
                            formatKey (block 15 "Tx hash:") <>
                            formatTxHash (txHashToHex tid)
                        _ -> mempty
                  , formatKey (block 15 "Amount:") <>
                    formatIntegerAmount unit (txInfoAmount s)
                  , case txInfoFee of
                        Just fee ->
                            formatKey (block 15 "Fees:") <>
                            formatIntegerAmountWith
                                formatFee
                                unit
                                (fromIntegral fee)
                        _ -> mempty
                  , case txInfoFeeByte s of
                        Just feeByte ->
                            formatKey (block 15 "Fee/byte:") <>
                            formatFeeBytes feeByte
                        _ -> mempty
                  , case txInfoTxSize of
                        Just bytes ->
                            formatKey (block 15 "Tx size:") <>
                            formatStatic (show (fromCount bytes) <> " bytes")
                        _ -> mempty
                  , case txInfoHeight of
                        Just height ->
                            formatKey (block 15 "Block Height:") <>
                            formatStatic (show height)
                        _ -> mempty
                  , case txInfoBlockHash of
                        Just bh ->
                            formatKey (block 15 "Block Hash:") <>
                            formatBlockHash (blockHashToHex bh)
                        _ -> mempty
                  , case heightM of
                        Just currHeight ->
                            formatKey (block 15 "Confirmations:") <>
                            case (currHeight -) =<< txInfoHeight of
                                Just conf -> formatStatic $ show $ conf + 1
                                _         -> formatStatic "Pending"
                        _ -> mempty
                  , case txSignedM of
                        Just signed ->
                            formatKey (block 15 "Signed:") <>
                            if signed
                                then formatTrue "Yes"
                                else formatFalse "No"
                        _ -> mempty
                  ]
            ]
    outbound
        | txInfoTxType s /= "Outbound" = mempty
        | txInfoNonStd == 0 && Map.null txInfoOutbound = mempty
        | otherwise =
            vcat
                [ formatTitle "Outbound"
                , nest 2 $
                  vcat $
                  fmap addrFormatOutbound (Map.assocs txInfoOutbound) <>
                  [nonStdRcp]
                ]
    nonStdRcp
        | txInfoNonStd == 0 = mempty
        | otherwise =
            formatAddrVal
                unit
                accDeriv
                (formatStatic "Non-standard recipients")
                Nothing
                (negate $ fromIntegral txInfoNonStd)
    inbound
        | Map.null txInfoInbound = mempty
        | otherwise =
            vcat
                [ formatTitle "Inbound"
                , nest 2 $
                  vcat $
                  fmap addrFormatInbound $
                  sortOn (((not . isExternal) <$>) . snd . snd) $
                  Map.assocs txInfoInbound
                ]
    myInputs
        | Map.null txInfoMyInputs = mempty
        | otherwise =
            vcat
                [ formatTitle "Spent Coins"
                , nest 2 $
                  vcat $ fmap addrFormatMyInputs (Map.assocs txInfoMyInputs)
                ]
    otherInputs
        | Map.null txInfoOtherInputs = mempty
        | otherwise =
            vcat
                [ formatTitle "Other Coins"
                , nest 2 $
                  vcat $
                  fmap addrFormatOtherInputs (Map.assocs txInfoOtherInputs)
                ]
    addrFormatInbound (a, (v, pM)) =
        formatAddrVal
            unit
            accDeriv
            ((if maybe False isExternal pM
                  then formatAddress
                  else formatInternalAddress) $
             addrToBase58 a)
            pM
            (fromIntegral v)
    addrFormatMyInputs (a, (v, pM)) =
        formatAddrVal
            unit
            accDeriv
            (formatInternalAddress $ addrToBase58 a)
            pM
            (negate $ fromIntegral v)
    addrFormatOtherInputs (a, v) =
        formatAddrVal
            unit
            accDeriv
            (formatInternalAddress $ addrToBase58 a)
            Nothing
            (negate $ fromIntegral v)
    addrFormatOutbound (a, v) =
        formatAddrVal
            unit
            accDeriv
            (formatAddress $ addrToBase58 a)
            Nothing
            (negate $ fromIntegral v)

formatAddrVal ::
       AmountUnit
    -> HardPath
    -> ConsolePrinter
    -> Maybe SoftPath
    -> Integer
    -> ConsolePrinter
formatAddrVal unit accDeriv title pathM amnt =
    vcat
        [ title
        , nest 4 $
          vcat
              [ formatKey (block 8 "Amount:") <> formatIntegerAmount unit amnt
              , case pathM of
                    Just p ->
                        mconcat
                            [ formatKey $ block 8 "Deriv:"
                            , formatDeriv $
                              show $ ParsedPrv $ toGeneric $ accDeriv ++/ p
                            ]
                    _ -> mempty
              ]
        ]
