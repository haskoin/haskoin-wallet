{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import           Network.Haskoin.Address
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.Doc
import           Network.Haskoin.Wallet.FoundationCompat
import           Options.Applicative.Help.Pretty
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

detailedTxCompactDoc ::
       HardPath
    -> AmountUnit
    -> Maybe Bool
    -> Maybe Natural
    -> DetailedTx
    -> Doc
detailedTxCompactDoc _ unit _ heightM s@DetailedTx {..} =
    vcat [title <+> doc confs, nest 4 $ vcat [txid, outbound, self, inbound]]
  where
    title =
        case detailedTxType s of
            TxOutbound -> titleDoc "Outbound Payment"
            TxInbound  -> titleDoc "Inbound Payment"
            TxInternal -> titleDoc "Payment To Yourself"
    confs =
        case heightM of
            Just currHeight ->
                case (currHeight -) =<< detailedTxHeight of
                    Just conf ->
                        "(" <> show (conf + 1) <> " confirmations)"
                    _ -> "(Pending)"
            _ -> mempty
    txid = maybe mempty (txHashDoc . textDoc . txHashToHex) detailedTxHash
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
            "Non-standard recipients:" <+>
            integerAmountDoc unit (fromIntegral detailedTxNonStdOutputs)
    feeKey =
        case detailedTxFee of
            Just fee ->
                keyDoc 0 "Fees:" <+>
                integerAmountWithDoc feeDoc unit (fromIntegral fee)
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
                  then keyDoc 0 "Total amount:" <+>
                       integerAmountDoc unit (detailedTxAmount s)
                  else mempty
            ] <>
            fmap (addrFormat id) (Map.assocs $ Map.map fst detailedTxInbound)
    addrFormat f (a, v) =
        addressDoc (textDoc $ addrToString a) <> colon <+>
        integerAmountDoc unit (f $ fromIntegral v)

detailedTxDoc ::
       HardPath
    -> AmountUnit
    -> Maybe Bool
    -> Maybe Natural
    -> DetailedTx
    -> Doc
detailedTxDoc accDeriv unit txSignedM heightM s@DetailedTx {..} =
    vcat [information, nest 2 $ vcat [outbound, inbound, myInputs, otherInputs]]
  where
    information =
        vcat
            [ titleDoc "Tx Information"
            , nest 4 $
              vcat
                  [ keyDoc 15 "Tx Type:" <>
                    doc (txTypeString $ detailedTxType s)
                  , case detailedTxHash of
                        Just tid ->
                            keyDoc 15 "Tx hash:" <>
                            txHashDoc (textDoc $ txHashToHex tid)
                        _ -> mempty
                  , keyDoc 15 "Amount:" <>
                    integerAmountDoc unit (detailedTxAmount s)
                  , case detailedTxFee of
                        Just fee ->
                            keyDoc 15 "Fees:" <>
                            integerAmountWithDoc feeDoc unit (fromIntegral fee)
                        _ -> mempty
                  , case detailedTxFeeByte s of
                        Just feeByte ->
                            keyDoc 15 "Fee/byte:" <> feeBytesDoc feeByte
                        _ -> mempty
                  , case detailedTxSize of
                        Just bytes ->
                            keyDoc 15 "Tx size:" <> doc (show $ fromCount bytes) <+>
                            "bytes"
                        _ -> mempty
                  , case detailedTxHeight of
                        Just height ->
                            keyDoc 15 "Block Height:" <> doc (show height)
                        _ -> mempty
                  , case detailedTxBlockHash of
                        Just bh ->
                            keyDoc 15 "Block Hash:" <>
                            blockHashDoc (textDoc $ blockHashToHex bh)
                        _ -> mempty
                  , case heightM of
                        Just currHeight ->
                            keyDoc 15 "Confirmations:" <>
                            case (currHeight -) =<< detailedTxHeight of
                                Just conf -> doc $ show $ conf + 1
                                _ -> "Pending"
                        _ -> mempty
                  , case txSignedM of
                        Just signed ->
                            keyDoc 15 "Signed:" <>
                            if signed
                                then trueDoc "Yes"
                                else falseDoc "No"
                        _ -> mempty
                  ]
            ]
    outbound
        | detailedTxType s /= TxOutbound = mempty
        | detailedTxNonStdOutputs == 0 && Map.null detailedTxOutbound = mempty
        | otherwise =
            vcat
                [ titleDoc "Outbound"
                , nest 2 $
                  vcat $
                  fmap addrFormatOutbound (Map.assocs detailedTxOutbound) <>
                  [nonStdRcp]
                ]
    nonStdRcp
        | detailedTxNonStdOutputs == 0 = mempty
        | otherwise =
            addrValDoc
                unit
                accDeriv
                "Non-standard recipients"
                Nothing
                (negate $ fromIntegral detailedTxNonStdOutputs)
    inbound
        | Map.null detailedTxInbound = mempty
        | otherwise =
            vcat
                [ titleDoc "Inbound"
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
                [ titleDoc "Spent Coins"
                , nest 2 $
                  vcat $ fmap addrFormatMyInputs (Map.assocs detailedTxMyInputs)
                ]
    otherInputs
        | Map.null detailedTxOtherInputs = mempty
        | otherwise =
            vcat
                [ titleDoc "Other Coins"
                , nest 2 $
                  vcat $
                  fmap addrFormatOtherInputs (Map.assocs detailedTxOtherInputs)
                ]
    addrFormatInbound (a, (v, pM)) =
        addrValDoc
            unit
            accDeriv
            ((if maybe False isExternal pM
                  then addressDoc
                  else internalAddressDoc) $
             textDoc $ addrToString a)
            pM
            (fromIntegral v)
    addrFormatMyInputs (a, (v, pM)) =
        addrValDoc
            unit
            accDeriv
            (internalAddressDoc $ textDoc $ addrToString a)
            pM
            (negate $ fromIntegral v)
    addrFormatOtherInputs (a, v) =
        addrValDoc
            unit
            accDeriv
            (internalAddressDoc $ textDoc $ addrToString a)
            Nothing
            (negate $ fromIntegral v)
    addrFormatOutbound (a, v) =
        addrValDoc
            unit
            accDeriv
            (addressDoc $ textDoc $ addrToString a)
            Nothing
            (negate $ fromIntegral v)

addrValDoc ::
       AmountUnit
    -> HardPath
    -> Doc
    -> Maybe SoftPath
    -> Integer
    -> Doc
addrValDoc unit accDeriv title pathM amnt =
    vcat
        [ title
        , nest 4 $
          vcat
              [ keyDoc 8 "Amount:" <> integerAmountDoc unit amnt
              , case pathM of
                    Just p ->
                        mconcat
                            [ keyDoc 8 "Deriv:"
                            , derivationDoc $ doc $
                              show $ ParsedPrv $ toGeneric $ accDeriv ++/ p
                            ]
                    _ -> mempty
              ]
        ]
