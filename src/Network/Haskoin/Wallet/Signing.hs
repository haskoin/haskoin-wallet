{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.Signing where

import           Control.Arrow                           ((&&&))
import           Data.Aeson.TH                           (deriveJSON)
import           Data.List                               (nub, sum, sortOn)
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import           Data.Ord
import qualified Data.Set                                as Set
import           Foundation
import           Foundation.Collection
import           Foundation.Compat.Text
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.FoundationCompat
import           Network.Haskoin.Wallet.HTTP
import           Network.Haskoin.Wallet.DetailedTx

{- Building Transactions -}

data WalletCoin = WalletCoin
    { walletCoinOutPoint     :: !OutPoint
    , walletCoinScriptOutput :: !ScriptOutput
    , walletCoinValue        :: !Satoshi
    } deriving (Eq, Show)

instance Coin WalletCoin where
    coinValue = fromIntegral . walletCoinValue

instance Ord WalletCoin where
    compare = compare `on` walletCoinValue

toWalletCoin :: (OutPoint, ScriptOutput, Satoshi) -> WalletCoin
toWalletCoin (op, so, v) = WalletCoin op so v

buildTxSignData ::
       Network
    -> AccountStore
    -> Map Address Satoshi
    -> Satoshi
    -> Satoshi
    -> IO (Either String (TxSignData, AccountStore))
buildTxSignData net store rcpMap feeByte dust
    | Map.null rcpMap = return $ Left "No recipients provided"
    | otherwise = do
        allCoins <-
            fmap toWalletCoin <$> httpUnspent net (Map.keys walletAddrMap)
        case buildWalletTx net walletAddrMap allCoins change rcpMap feeByte dust of
            Right (tx, depTxHash, inDeriv, outDeriv) -> do
                depTxs <- httpRawTxs net depTxHash
                return $
                    Right
                        ( TxSignData
                          { txSignDataTx = tx
                          , txSignDataInputs = depTxs
                          , txSignDataInputPaths = inDeriv
                          , txSignDataOutputPaths = outDeriv
                          }
                        , if null outDeriv
                              then store
                              else store')
            Left err -> return $ Left err
  where
    walletAddrMap =
        Map.fromList $ allExtAddresses store <> allIntAddresses store
    (change, store') = nextIntAddress store

buildWalletTx ::
       Network
    -> Map Address SoftPath -- All account addresses
    -> [WalletCoin]
    -> (Address, SoftPath, Natural) -- change
    -> Map Address Satoshi -- recipients
    -> Satoshi -- Fee per byte
    -> Satoshi -- Dust
    -> Either String (Tx, [TxHash], [SoftPath], [SoftPath])
buildWalletTx net walletAddrMap coins (change, changeDeriv, _) rcpMap feeByte dust = do
    (selectedCoins, changeAmnt) <-
        eitherString $
        second toNatural <$>
        chooseCoins
            (fromIntegral tot)
            (fromIntegral feeByte)
            nRcps
            True
            descCoins
    let (txRcpMap, outDeriv)
            | changeAmnt <= dust = (rcpMap, [])
            | otherwise = (Map.insert change changeAmnt rcpMap, [changeDeriv])
        ops = fmap walletCoinOutPoint selectedCoins
    tx <-
        eitherString $
        buildAddrTx net ops $
        bimap addrToString fromIntegral <$> Map.assocs txRcpMap
    inCoinAddrs <-
        eitherString $
        mapM (outputAddress net . walletCoinScriptOutput) selectedCoins
    let inDerivMap = Map.restrictKeys walletAddrMap $ Set.fromList inCoinAddrs
    return
        ( tx
        , nub $ fmap outPointHash ops
        , nub $ Map.elems inDerivMap
        , nub $ outDeriv <> myPaths)
    -- Add recipients that are in our own wallet
  where
    myPaths = Map.elems $ Map.intersection walletAddrMap rcpMap
    nRcps = Map.size rcpMap + 1
    tot = sum $ Map.elems rcpMap
    descCoins = sortOn Down coins

buildSwipeTx ::
       Network
    -> AccountStore
    -> [Address]
    -> Satoshi -- Fee per byte
    -> IO (Either String (TxSignData, AccountStore))
buildSwipeTx net store addrs feeByte = do
    coins <- fmap toWalletCoin <$> httpUnspent net addrs
    let tot = sum $ walletCoinValue <$> coins
        fee = guessTxFee (fromIntegral feeByte) 1 (fromCount $ length coins)
        ops = walletCoinOutPoint <$> coins
        depTxHash = nub $ outPointHash <$> ops
    if null coins
        then return $ Left "No coins were found in the given addresses"
        else do
            depTxs <- httpRawTxs net depTxHash
            return $
                eitherString $ do
                    amnt <-
                        maybeToEither "The inputs do not cover the required fee" $
                        tot - fromIntegral fee
                    tx <-
                        buildAddrTx
                            net
                            ops
                            [(addrToString (fst rcpt), fromIntegral amnt)]
                    return
                        ( TxSignData
                          { txSignDataTx = tx
                          , txSignDataInputs = depTxs
                          , txSignDataInputPaths = []
                          , txSignDataOutputPaths = [snd rcpt]
                          }
                        , store')
  where
    (rcpt, store') = nextExtAddress store

{- Signing Transactions -}

bip44Deriv :: Network -> Natural -> HardPath
bip44Deriv net a = Deriv :| 44 :| getBip44Coin net :| fromIntegral a

signingKey :: Network -> String -> String -> Natural -> Either String XPrvKey
signingKey net pass mnem acc = do
    seed <- eitherString $ mnemonicToSeed (stringToBS pass) (toText mnem)
    return $ derivePath (bip44Deriv net acc) (makeXPrvKey net seed)

data TxSignData = TxSignData
    { txSignDataTx          :: !Tx
    , txSignDataInputs      :: ![Tx]
    , txSignDataInputPaths  :: ![SoftPath]
    , txSignDataOutputPaths :: ![SoftPath]
    } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 10) ''TxSignData)

pubDetailedTx :: Network -> TxSignData -> XPubKey -> Either String DetailedTx
pubDetailedTx net (TxSignData tx inTxs inPaths outPaths) pubKey
    | fromCount (length coins) /= fromCount (length $ txIn tx) =
        Left "Referenced input transactions are missing"
    | fromCount (length inPaths) /= Map.size (detailedTxMyInputs info) =
        Left "Tx is missing inputs from private keys"
    | fromCount (length outPaths) /= Map.size (detailedTxInbound info) =
        Left "Tx is missing change outputs"
    | otherwise = return info
  where
    info =
        detailedTxFillUnsignedTx net tx $
        detailedTxFillInbound net outAddrMap (txOut tx) $
        detailedTxFillInputs net inAddrMap (snd <$> coins) emptyDetailedTx
    outAddrMap = Map.fromList $ fmap (pathToAddr pubKey &&& id) outPaths
    inAddrMap = Map.fromList $ fmap (pathToAddr pubKey &&& id) inPaths
    coins = mapMaybe (findCoin inTxs . prevOutput) $ txIn tx

signWalletTx ::
       Network
    -> TxSignData
    -> XPrvKey
    -> Either String (DetailedTx, Tx, Bool)
signWalletTx net tsd@(TxSignData tx _ inPaths _) signKey = do
    sigDat <- mapM g myCoins
    signedTx <-
        eitherString $
        signTx net tx (fmap f sigDat) (wrapSecKey True <$> prvKeys)
    let vDat = rights $ fmap g allCoins
        isSigned = noEmptyInputs signedTx && verifyStdTx net signedTx vDat
    info <- pubDetailedTx net tsd pubKey
    return
        ( if isSigned
              then detailedTxFillTx net signedTx info
              else info
        , signedTx
        , isSigned)
  where
    pubKey = deriveXPubKey signKey
    (myCoins, othCoins) = parseTxCoins net tsd pubKey
    allCoins = myCoins <> othCoins
    prvKeys = fmap (xPrvKey . (`derivePath` signKey)) inPaths
    f (so, val, op) = SigInput so val op (maybeSetForkId net sigHashAll) Nothing
    g (op, to) = (, outValue to, op) <$> decodeTxOutSO to

signSwipeTx ::
       Network
    -> TxSignData
    -> [SecKeyI]
    -> Either String (DetailedTx, Tx, Bool)
signSwipeTx net (TxSignData tx inTxs _ _) prvKeys = do
    sigDat <- mapM g coins
    signedTx <- eitherString $ signTx net tx (fmap f sigDat) prvKeys
    let isSigned = noEmptyInputs signedTx && verifyStdTx net signedTx sigDat
    return
        ( if isSigned
              then detailedTxFillTx net signedTx info
              else info
        , signedTx
        , isSigned)
  where
    info = detailedTxFillInputs net Map.empty (snd <$> coins) emptyDetailedTx
    coins = mapMaybe (findCoin inTxs . prevOutput) $ txIn tx
    f (so, val, op) = SigInput so val op (maybeSetForkId net sigHashAll) Nothing
    g (op, to) = (, outValue to, op) <$> decodeTxOutSO to

maybeSetForkId :: Network -> SigHash -> SigHash
maybeSetForkId net
    | isJust (getSigHashForkId net) = setForkIdFlag
    | otherwise = id

noEmptyInputs :: Tx -> Bool
noEmptyInputs = all (not . null) . fmap (asBytes scriptInput) . txIn

pathToAddr :: XPubKey -> SoftPath -> Address
pathToAddr pubKey = xPubAddr . (`derivePubPath` pubKey)

parseTxCoins ::
       Network
    -> TxSignData
    -> XPubKey
    -> ([(OutPoint, TxOut)], [(OutPoint, TxOut)])
parseTxCoins net (TxSignData tx inTxs inPaths _) pubKey =
    partition (isMyCoin . snd) coins
  where
    inAddrs = nub $ fmap (pathToAddr pubKey) inPaths
    coins = mapMaybe (findCoin inTxs . prevOutput) $ txIn tx
    isMyCoin to =
        case decodeTxOutAddr net to of
            Right a -> a `elem` inAddrs
            _       -> False

findCoin :: [Tx] -> OutPoint -> Maybe (OutPoint, TxOut)
findCoin txs op@(OutPoint h i) = do
    matchTx <- find ((== h) . txHash) txs
    to <- txOut matchTx ! fromIntegral i
    return (op, to)

