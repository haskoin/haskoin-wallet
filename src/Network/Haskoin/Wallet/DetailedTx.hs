{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.Haskoin.Wallet.DetailedTx where

import           Control.Arrow                           ((&&&))
import           Data.Decimal
import           Data.List                               (sortOn, sum)
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import           Foundation
import           Foundation.Collection
import           Foundation.Compat.Text
import           Network.Haskoin.Address
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.FoundationCompat
import           Network.Haskoin.Wallet.Printer
import qualified Prelude

data DetailedTx = DetailedTx
    { detailedTxHash          :: Maybe TxHash
    , detailedTxSize          :: Maybe (CountOf (Element (UArray Word8)))
    , detailedTxOutbound      :: Map Address Satoshi
    , detailedTxNonStdOutputs :: Satoshi
    , detailedTxInbound       :: Map Address (Satoshi, Maybe SoftPath)
    , detailedTxMyInputs      :: Map Address (Satoshi, Maybe SoftPath)
    , detailedTxOtherInputs   :: Map Address Satoshi
    , detailedTxNonStdInputs  :: Satoshi
    , detailedTxFee           :: Maybe Satoshi
    , detailedTxHeight        :: Maybe Natural
    , detailedTxBlockHash     :: Maybe BlockHash
    } deriving (Eq, Show)

data TxType = TxInbound | TxInternal | TxOutbound
    deriving (Show, Eq)

txTypeString :: TxType -> String
txTypeString =
    \case
        TxInbound -> "Inbound"
        TxInternal -> "Internal"
        TxOutbound -> "Outbound"

emptyDetailedTx :: DetailedTx
emptyDetailedTx =
    DetailedTx
    { detailedTxHash = Nothing
    , detailedTxSize = Nothing
    , detailedTxOutbound = Map.empty
    , detailedTxNonStdOutputs = 0
    , detailedTxInbound = Map.empty
    , detailedTxMyInputs = Map.empty
    , detailedTxOtherInputs = Map.empty
    , detailedTxNonStdInputs = 0
    , detailedTxFee = Nothing
    , detailedTxHeight = Nothing
    , detailedTxBlockHash = Nothing
    }

detailedTxAmount :: DetailedTx -> Integer
detailedTxAmount DetailedTx {..} =
    toInteger inboundSum - toInteger myCoinsSum
  where
    inboundSum = sum $ fst <$> Map.elems detailedTxInbound
    myCoinsSum = sum $ fst <$> Map.elems detailedTxMyInputs

detailedTxFeeByte :: DetailedTx -> Maybe Decimal
detailedTxFeeByte DetailedTx {..} = do
    sat <- fromIntegral <$> detailedTxFee
    bytes <- fromIntegral . fromCount <$> detailedTxSize
    return $ roundTo 2 $ sat Prelude./ bytes

detailedTxType :: DetailedTx -> TxType
detailedTxType s
    | amnt > 0 = TxInbound
    | Just (fromIntegral $ abs amnt) == detailedTxFee s = TxInternal
    | otherwise = TxOutbound
  where
    amnt = detailedTxAmount s

detailedTxFillPath :: Map Address SoftPath -> DetailedTx -> DetailedTx
detailedTxFillPath addrMap detailedTx =
    detailedTx
    { detailedTxInbound = mergeSoftPath (detailedTxInbound detailedTx)
    , detailedTxMyInputs = mergeSoftPath (detailedTxMyInputs detailedTx)
    }
  where
    mergeSoftPath = Map.intersectionWith f addrMap
    f path (amnt, _) = (amnt, Just path)

detailedTxFillUnsignedTx :: Network -> Tx -> DetailedTx -> DetailedTx
detailedTxFillUnsignedTx net tx detailedTx =
    (detailedTxFillTx net tx detailedTx)
    { detailedTxHash = Nothing
    , detailedTxSize =
          Just $
          toCount $
          guessTxSize
              (fromCount $ length $ txIn tx)
              []
              (fromCount $ length $ txOut tx)
              0
    }

detailedTxFillTx :: Network -> Tx -> DetailedTx -> DetailedTx
detailedTxFillTx net tx detailedTx =
    detailedTx
    { detailedTxOutbound = outbound
    , detailedTxNonStdOutputs = nonStd
    , detailedTxHash = Just $ txHash tx
    , detailedTxSize = Just $ length $ encodeBytes tx
    , detailedTxFee = detailedTxFee detailedTx <|> feeM
    }
  where
    (outAddrMap, nonStd) = txOutAddressMap net $ txOut tx
    outbound = Map.difference outAddrMap (detailedTxInbound detailedTx)
    outSum = sum $ toNatural . outValue <$> txOut tx :: Natural
    myInSum =
        sum $ fst <$> Map.elems (detailedTxMyInputs detailedTx) :: Natural
    othInSum =
        sum $ Map.elems (detailedTxOtherInputs detailedTx) :: Natural
    feeM = myInSum + othInSum - outSum :: Maybe Natural

detailedTxFillInputs ::
       Network
    -> Map Address SoftPath
    -> [TxOut]
    -> DetailedTx
    -> DetailedTx
detailedTxFillInputs net walletAddrs txOs txInf =
    txInf
    { detailedTxMyInputs = Map.map (second Just) myValMap
    , detailedTxOtherInputs = othValMap
    }
  where
    valMap = fst $ txOutAddressMap net txOs
    myValMap = Map.intersectionWith (,) valMap walletAddrs
    othValMap = Map.difference valMap myValMap

detailedTxFillInbound ::
       Network
    -> Map Address SoftPath
    -> [TxOut]
    -> DetailedTx
    -> DetailedTx
detailedTxFillInbound net walletAddrs txOs txInf =
    txInf { detailedTxInbound = Map.map (second Just) inboundMap }
  where
    (outValMap, _) = txOutAddressMap net txOs
    inboundMap = Map.intersectionWith (,) outValMap walletAddrs

txOutAddressMap :: Network -> [TxOut] -> (Map Address Satoshi, Satoshi)
txOutAddressMap net txout =
    (Map.fromListWith (+) rs, sum ls)
  where
    xs = fmap (decodeTxOutAddr net &&& toNatural . outValue) txout
    (ls, rs) = partitionEithers $ fmap partE xs
    partE (Right a, v) = Right (a, v)
    partE (Left _, v)  = Left v

decodeTxOutAddr :: Network -> TxOut -> Either String Address
decodeTxOutAddr net = decodeTxOutSO >=> eitherString . outputAddress net

decodeTxOutSO :: TxOut -> Either String ScriptOutput
decodeTxOutSO = eitherString . decodeOutputBS . scriptOutput

isExternal :: SoftPath -> Bool
isExternal (Deriv :/ 0 :/ _) = True
isExternal _                 = False

detailedTxFormatCompact ::
       HardPath
    -> AmountUnit
    -> Maybe Bool
    -> Maybe Natural
    -> DetailedTx
    -> Printer
detailedTxFormatCompact _ unit _ heightM s@DetailedTx {..} =
    vcat [title <+> confs, nest 4 $ vcat [txid, outbound, self, inbound]]
  where
    title =
        case detailedTxType s of
            TxOutbound -> formatTitle "Outbound Payment"
            TxInbound  -> formatTitle "Inbound Payment"
            TxInternal -> formatTitle "Payment To Yourself"
    confs =
        case heightM of
            Just currHeight ->
                case (currHeight -) =<< detailedTxHeight of
                    Just conf ->
                        formatStatic $
                        "(" <> show (conf + 1) <> " confirmations)"
                    _ -> formatStatic "(Pending)"
            _ -> mempty
    txid = maybe mempty (formatTxHash . fromText . txHashToHex) detailedTxHash
    outbound
        | detailedTxType s /= TxOutbound = mempty
        | detailedTxNonStdOutputs == 0 && Map.null detailedTxOutbound = mempty
        | otherwise =
            vcat $
            [feeKey] <> fmap (addrFormat negate) (Map.assocs detailedTxOutbound) <>
            [nonStdRcp]
    nonStdRcp
        | detailedTxNonStdOutputs == 0 = mempty
        | otherwise =
            formatStatic "Non-standard recipients:" <+>
            formatIntegerAmount unit (fromIntegral detailedTxNonStdOutputs)
    feeKey =
        case detailedTxFee of
            Just fee ->
                formatKey "Fees:" <+>
                formatIntegerAmountWith formatFee unit (fromIntegral fee)
            _ -> mempty
    self
        | detailedTxType s /= TxInternal = mempty
        | otherwise = feeKey
    inbound
        | detailedTxType s /= TxInbound = mempty
        | Map.null detailedTxInbound = mempty
        | otherwise =
            vcat $
            [ if Map.size detailedTxInbound > 1
                  then formatKey "Total amount:" <+>
                       formatIntegerAmount unit (detailedTxAmount s)
                  else mempty
            ] <>
            fmap (addrFormat id) (Map.assocs $ Map.map fst detailedTxInbound)
    addrFormat f (a, v) =
        formatAddress (fromText $ addrToString a) <> formatStatic ":" <+>
        formatIntegerAmount unit (f $ fromIntegral v)

detailedTxFormat ::
       HardPath
    -> AmountUnit
    -> Maybe Bool
    -> Maybe Natural
    -> DetailedTx
    -> Printer
detailedTxFormat accDeriv unit txSignedM heightM s@DetailedTx {..} =
    vcat [information, nest 2 $ vcat [outbound, inbound, myInputs, otherInputs]]
  where
    information =
        vcat
            [ formatTitle "Tx Information"
            , nest 4 $
              vcat
                  [ formatKey (block 15 "Tx Type:") <>
                    formatStatic (txTypeString $ detailedTxType s)
                  , case detailedTxHash of
                        Just tid ->
                            formatKey (block 15 "Tx hash:") <>
                            formatTxHash (fromText $ txHashToHex tid)
                        _ -> mempty
                  , formatKey (block 15 "Amount:") <>
                    formatIntegerAmount unit (detailedTxAmount s)
                  , case detailedTxFee of
                        Just fee ->
                            formatKey (block 15 "Fees:") <>
                            formatIntegerAmountWith
                                formatFee
                                unit
                                (fromIntegral fee)
                        _ -> mempty
                  , case detailedTxFeeByte s of
                        Just feeByte ->
                            formatKey (block 15 "Fee/byte:") <>
                            formatFeeBytes feeByte
                        _ -> mempty
                  , case detailedTxSize of
                        Just bytes ->
                            formatKey (block 15 "Tx size:") <>
                            formatStatic (show (fromCount bytes) <> " bytes")
                        _ -> mempty
                  , case detailedTxHeight of
                        Just height ->
                            formatKey (block 15 "Block Height:") <>
                            formatStatic (show height)
                        _ -> mempty
                  , case detailedTxBlockHash of
                        Just bh ->
                            formatKey (block 15 "Block Hash:") <>
                            formatBlockHash (fromText $ blockHashToHex bh)
                        _ -> mempty
                  , case heightM of
                        Just currHeight ->
                            formatKey (block 15 "Confirmations:") <>
                            case (currHeight -) =<< detailedTxHeight of
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
        | detailedTxType s /= TxOutbound = mempty
        | detailedTxNonStdOutputs == 0 && Map.null detailedTxOutbound = mempty
        | otherwise =
            vcat
                [ formatTitle "Outbound"
                , nest 2 $
                  vcat $
                  fmap addrFormatOutbound (Map.assocs detailedTxOutbound) <>
                  [nonStdRcp]
                ]
    nonStdRcp
        | detailedTxNonStdOutputs == 0 = mempty
        | otherwise =
            formatAddrVal
                unit
                accDeriv
                (formatStatic "Non-standard recipients")
                Nothing
                (negate $ fromIntegral detailedTxNonStdOutputs)
    inbound
        | Map.null detailedTxInbound = mempty
        | otherwise =
            vcat
                [ formatTitle "Inbound"
                , nest 2 $
                  vcat $
                  fmap addrFormatInbound $
                  sortOn (((not . isExternal) <$>) . snd . snd) $
                  Map.assocs detailedTxInbound
                ]
    myInputs
        | Map.null detailedTxMyInputs = mempty
        | otherwise =
            vcat
                [ formatTitle "Spent Coins"
                , nest 2 $
                  vcat $ fmap addrFormatMyInputs (Map.assocs detailedTxMyInputs)
                ]
    otherInputs
        | Map.null detailedTxOtherInputs = mempty
        | otherwise =
            vcat
                [ formatTitle "Other Coins"
                , nest 2 $
                  vcat $
                  fmap addrFormatOtherInputs (Map.assocs detailedTxOtherInputs)
                ]
    addrFormatInbound (a, (v, pM)) =
        formatAddrVal
            unit
            accDeriv
            ((if maybe False isExternal pM
                  then formatAddress
                  else formatInternalAddress) $
             fromText $ addrToString a)
            pM
            (fromIntegral v)
    addrFormatMyInputs (a, (v, pM)) =
        formatAddrVal
            unit
            accDeriv
            (formatInternalAddress $ fromText $ addrToString a)
            pM
            (negate $ fromIntegral v)
    addrFormatOtherInputs (a, v) =
        formatAddrVal
            unit
            accDeriv
            (formatInternalAddress $ fromText $ addrToString a)
            Nothing
            (negate $ fromIntegral v)
    addrFormatOutbound (a, v) =
        formatAddrVal
            unit
            accDeriv
            (formatAddress $ fromText $ addrToString a)
            Nothing
            (negate $ fromIntegral v)

formatAddrVal ::
       AmountUnit
    -> HardPath
    -> Printer
    -> Maybe SoftPath
    -> Integer
    -> Printer
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
