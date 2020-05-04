{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.Haskoin.Wallet.DetailedTx where

import           Control.Applicative             ((<|>))
import           Control.Arrow                   (second, (&&&))
import qualified Data.ByteString                 as BS
import           Data.Decimal
import           Data.Either                     (partitionEithers)
import           Data.List                       (sortOn, sum)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe)
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

data DetailedTx = DetailedTx
    { detailedTxHash          :: Maybe TxHash
    , detailedTxSize          :: Maybe Int
    , detailedTxOutbound      :: Map Address Natural
    , detailedTxNonStdOutputs :: Natural
    , detailedTxInbound       :: Map Address (Natural, Maybe SoftPath)
    , detailedTxMyInputs      :: Map Address (Natural, Maybe SoftPath)
    , detailedTxOtherInputs   :: Map Address Natural
    , detailedTxNonStdInputs  :: Natural
    , detailedTxFee           :: Maybe Natural
    , detailedTxHeight        :: Maybe Natural
    , detailedTxBlockHash     :: Maybe BlockHash
    } deriving (Eq, Show)

data TxType = TxInbound | TxInternal | TxOutbound
    deriving (Show, Eq)

instance ToString TxType where
    toString =
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
    bytes <- fromIntegral <$> detailedTxSize
    -- This division is lossy but it's for display only
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

detailedTxFillUnsignedTx :: Tx -> DetailedTx -> DetailedTx
detailedTxFillUnsignedTx tx detailedTx =
    (detailedTxFillTx tx detailedTx)
        { detailedTxHash = Nothing
        , detailedTxSize =
              Just $ guessTxSize (length $ txIn tx) [] (length $ txOut tx) 0
        }

detailedTxFillTx :: Tx -> DetailedTx -> DetailedTx
detailedTxFillTx tx detailedTx =
    detailedTx
        { detailedTxOutbound = outbound
        , detailedTxNonStdOutputs = nonStd
        , detailedTxHash = Just $ txHash tx
        , detailedTxSize = Just $ BS.length $ Serialize.encode tx
        , detailedTxFee = detailedTxFee detailedTx <|> feeM
        }
  where
    (outAddrMap, nonStd) = txOutAddressMap $ txOut tx
    outbound = Map.difference outAddrMap (detailedTxInbound detailedTx)
    outSum = fromIntegral $ sum $ outValue <$> txOut tx :: Natural
    myInSum = sum $ fst <$> Map.elems (detailedTxMyInputs detailedTx) :: Natural
    othInSum = sum $ Map.elems (detailedTxOtherInputs detailedTx) :: Natural
    feeM = (myInSum + othInSum) `safeSubtract` outSum

detailedTxFillInputs ::
       Map Address SoftPath
    -> [TxOut]
    -> DetailedTx
    -> DetailedTx
detailedTxFillInputs walletAddrs txOs txInf =
    txInf
    { detailedTxMyInputs = Map.map (second Just) myValMap
    , detailedTxOtherInputs = othValMap
    }
  where
    valMap = fst $ txOutAddressMap txOs
    myValMap = Map.intersectionWith (,) valMap walletAddrs
    othValMap = Map.difference valMap myValMap

detailedTxFillInbound ::
       Map Address SoftPath
    -> [TxOut]
    -> DetailedTx
    -> DetailedTx
detailedTxFillInbound walletAddrs txOs txInf =
    txInf { detailedTxInbound = Map.map (second Just) inboundMap }
  where
    (outValMap, _) = txOutAddressMap txOs
    inboundMap = Map.intersectionWith (,) outValMap walletAddrs

txOutAddressMap :: [TxOut] -> (Map Address Natural, Natural)
txOutAddressMap txout =
    (Map.fromListWith (+) rs, sum ls)
  where
    xs = (decodeTxOutAddr &&& outValue) <$> txout
    (ls, rs) = partitionEithers $ partE <$> xs
    partE (Right a, v) = Right (a, fromIntegral v)
    partE (Left _, v)  = Left (fromIntegral v)

decodeTxOutAddr :: TxOut -> Either String Address
decodeTxOutAddr to = do
    so <- decodeTxOutSO to
    maybeToEither "Error in decodeTxOutAddr" $ outputAddress so

decodeTxOutSO :: TxOut -> Either String ScriptOutput
decodeTxOutSO = decodeOutputBS . scriptOutput

isExternal :: SoftPath -> Bool
isExternal (Deriv :/ 0 :/ _) = True
isExternal _                 = False

detailedTxCompactDoc ::
       Network
    -> HardPath
    -> AmountUnit
    -> Maybe Bool
    -> Maybe Natural
    -> DetailedTx
    -> Doc
detailedTxCompactDoc net _ unit _ heightM s@DetailedTx {..} =
    vcat [title <+> text confs, nest 4 $ vcat [txid, outbound, self, inbound]]
  where
    title =
        case detailedTxType s of
            TxOutbound -> titleDoc "Outbound Payment"
            TxInbound  -> titleDoc "Inbound Payment"
            TxInternal -> titleDoc "Payment To Yourself"
    confs =
        case heightM of
            Just currHeight ->
                case (currHeight `safeSubtract`) =<< detailedTxHeight of
                    Just conf -> "(" <> show (conf + 1) <> " confirmations)"
                    _         -> "(Pending)"
            _ -> mempty
    txid = maybe mempty (txHashDoc . text . cs . txHashToHex) detailedTxHash
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
         addressDoc (text . cs $ addrStr net a) <> colon <+>
            integerAmountDoc unit (f $ fromIntegral v)

detailedTxDoc ::
       Network
    -> HardPath
    -> AmountUnit
    -> Maybe Bool
    -> Maybe Natural
    -> DetailedTx
    -> Doc
detailedTxDoc net accDeriv unit txSignedM heightM s@DetailedTx {..} =
    vcat [information, nest 2 $ vcat [outbound, inbound, myInputs, otherInputs]]
  where
    information =
        vcat
            [ titleDoc "Tx Information"
            , nest 4 $
              vcat
                  [ keyDoc 15 "Tx Type:" <> text (toString $ detailedTxType s)
                  , case detailedTxHash of
                        Just tid ->
                            keyDoc 15 "Tx hash:" <>
                            txHashDoc (text . cs $ txHashToHex tid)
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
                            keyDoc 15 "Tx size:" <> text (show bytes) <+>
                            "bytes"
                        _ -> mempty
                  , case detailedTxHeight of
                        Just height ->
                            keyDoc 15 "Block Height:" <> text (show height)
                        _ -> mempty
                  , case detailedTxBlockHash of
                        Just bh ->
                            keyDoc 15 "Block Hash:" <>
                            blockHashDoc (text . cs $ blockHashToHex bh)
                        _ -> mempty
                  , case heightM of
                        Just currHeight ->
                            keyDoc 15 "Confirmations:" <>
                            case (currHeight `safeSubtract`) =<<
                                 detailedTxHeight of
                                Just conf -> text $ show $ conf + 1
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
             text . cs $ addrStr net a)
            pM
            (fromIntegral v)
    addrFormatMyInputs (a, (v, pM)) =
        addrValDoc
            unit
            accDeriv
            (internalAddressDoc $ text . cs $ addrStr net a)
            pM
            (negate $ fromIntegral v)
    addrFormatOtherInputs (a, v) =
        addrValDoc
            unit
            accDeriv
            (internalAddressDoc $ text . cs $ addrStr net a)
            Nothing
            (negate $ fromIntegral v)
    addrFormatOutbound (a, v) =
        addrValDoc
            unit
            accDeriv
            (addressDoc $ text . cs $ addrStr net a)
            Nothing
            (negate $ fromIntegral v)


addrStr :: Network -> Address -> Text
addrStr net a =
    fromMaybe
        (exitError "Invalid Address in haskoin-wallet")
        (addrToString net a)

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
                            , derivationDoc $
                              text $
                              show $ ParsedPrv $ toGeneric $ accDeriv ++/ p
                            ]
                    _ -> mempty
              ]
        ]
