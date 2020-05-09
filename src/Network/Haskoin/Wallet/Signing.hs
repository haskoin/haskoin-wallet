{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.Signing where

import           Control.Arrow                       (second, (&&&), (***))
import           Data.Aeson.TH                       (deriveJSON)
import qualified Data.ByteString                     as BS
import           Data.Either                         (rights)
import           Data.List                           (find, nub, partition,
                                                      sortOn, sum)
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Keys
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.DetailedTx
import           Network.Haskoin.Wallet.HTTP
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural

{- Building Transactions -}

data WalletCoin = WalletCoin
    { walletCoinOutPoint     :: !OutPoint
    , walletCoinScriptOutput :: !ScriptOutput
    , walletCoinValue        :: !Natural
    } deriving (Eq, Show)

instance Coin WalletCoin where
    coinValue = fromIntegral . walletCoinValue

instance Ord WalletCoin where
    compare = comparing walletCoinValue

toWalletCoin :: (OutPoint, ScriptOutput, Natural) -> WalletCoin
toWalletCoin (op, so, v) = WalletCoin op so v

buildTxSignData ::
       Network
    -> AccountStore
    -> Map Address Natural
    -> Natural
    -> Natural
    -> IO (Either String (TxSignData, Commit AccountStore))
buildTxSignData net store rcpMap feeByte dust
    | Map.null rcpMap = return $ Left "No recipients provided"
    | otherwise = do
        allCoins <-
            fmap toWalletCoin <$> httpUnspent net (Map.keys walletAddrMap)
        case buildWalletTx
                 net
                 walletAddrMap
                 allCoins
                 change
                 rcpMap
                 feeByte
                 dust of
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
                              then NoCommit store
                              else newStore)
            Left err -> return $ Left err
  where
    walletAddrMap =
        Map.fromList $ fmap f $ extAddresses store <> intAddresses store
    f (a,p,_) = (a,p)
    (change, newStore) = genIntAddress store

buildWalletTx ::
       Network
    -> Map Address SoftPath -- All account addresses
    -> [WalletCoin]
    -> (Address, SoftPath, Natural) -- change
    -> Map Address Natural -- recipients
    -> Natural -- Fee per byte
    -> Natural -- Dust
    -> Either String (Tx, [TxHash], [SoftPath], [SoftPath])
buildWalletTx net walletAddrMap coins (change, changeDeriv, _) rcpMap feeByte dust = do
    (selectedCoins, changeAmnt) <-
        second fromIntegral <$>
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
        buildAddrTx net ops $
        (addrStr net *** fromIntegral) <$> Map.assocs txRcpMap
    inCoinAddrs <-
        mapM
            (maybeToEither "buildWalletTx Error" .
             outputAddress . walletCoinScriptOutput)
            selectedCoins
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
    -> Natural -- Fee per byte
    -> IO (Either String (TxSignData, Commit AccountStore))
buildSwipeTx net store addrs feeByte = do
    coins <- fmap toWalletCoin <$> httpUnspent net addrs
    let tot = sum $ walletCoinValue <$> coins
        fee = guessTxFee (fromIntegral feeByte) 1 (length coins)
        ops = walletCoinOutPoint <$> coins
        depTxHash = nub $ outPointHash <$> ops
    if null coins
        then return $ Left "No coins were found in the given addresses"
        else do
            depTxs <- httpRawTxs net depTxHash
            return $ do
                amnt <-
                    maybeToEither "The inputs do not cover the required fee" $
                    tot `safeSubtract` fromIntegral fee
                tx <-
                    buildAddrTx
                        net
                        ops
                        [(addrStr net (fst3 rcpt), fromIntegral amnt)]
                return
                    ( TxSignData
                          { txSignDataTx = tx
                          , txSignDataInputs = depTxs
                          , txSignDataInputPaths = []
                          , txSignDataOutputPaths = [snd3 rcpt]
                          }
                    , commitStore)
  where
    (rcpt, commitStore) = genExtAddress store

{- Signing Transactions -}

signingKey ::
       Network -> BS.ByteString -> Text -> Natural -> Either String XPrvKey
signingKey net pass mnem acc = do
    seed <- mnemonicToSeed pass mnem
    return $ derivePath (bip44Deriv net acc) (makeXPrvKey seed)

data TxSignData = TxSignData
    { txSignDataTx          :: !Tx
    , txSignDataInputs      :: ![Tx]
    , txSignDataInputPaths  :: ![SoftPath]
    , txSignDataOutputPaths :: ![SoftPath]
    } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 10) ''TxSignData)

pubDetailedTx :: TxSignData -> XPubKey -> Either String DetailedTx
pubDetailedTx (TxSignData tx inTxs inPaths outPaths) pubKey
    | length coins /= length (txIn tx) =
        Left "Referenced input transactions are missing"
    | length inPaths /= Map.size (detailedTxMyInputs info) =
        Left "Tx is missing inputs from private keys"
    | length outPaths /= Map.size (detailedTxInbound info) =
        Left "Tx is missing change outputs"
    | otherwise = return info
  where
    info =
        detailedTxFillUnsignedTx tx $
        detailedTxFillInbound outAddrMap (txOut tx) $
        detailedTxFillInputs inAddrMap (snd <$> coins) emptyDetailedTx
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
        signTx net tx (fmap f sigDat) prvKeys
    let vDat = rights $ fmap g allCoins
        isSigned = noEmptyInputs signedTx && verifyStdTx net signedTx vDat
    info <- pubDetailedTx tsd pubKey
    return
        ( if isSigned
              then detailedTxFillTx signedTx info
              else info
        , signedTx
        , isSigned)
  where
    pubKey = deriveXPubKey signKey
    (myCoins, othCoins) = parseTxCoins tsd pubKey
    allCoins = myCoins <> othCoins
    prvKeys = fmap (xPrvKey . (`derivePath` signKey)) inPaths
    f (so, val, op) = SigInput so val op (maybeSetForkId net sigHashAll) Nothing
    g (op, to) = (, outValue to, op) <$> decodeTxOutSO to

signSwipeTx ::
       Network
    -> TxSignData
    -> [SecKey]
    -> Either String (DetailedTx, Tx, Bool)
signSwipeTx net (TxSignData tx inTxs _ _) prvKeys = do
    sigDat <- mapM g coins
    signedTx <- signTx net tx (fmap f sigDat) prvKeys
    let isSigned = noEmptyInputs signedTx && verifyStdTx net signedTx sigDat
    return
        ( if isSigned
              then detailedTxFillTx signedTx info
              else info
        , signedTx
        , isSigned)
  where
    info = detailedTxFillInputs Map.empty (snd <$> coins) emptyDetailedTx
    coins = mapMaybe (findCoin inTxs . prevOutput) $ txIn tx
    f (so, val, op) = SigInput so val op (maybeSetForkId net sigHashAll) Nothing
    g (op, to) = (, outValue to, op) <$> decodeTxOutSO to

maybeSetForkId :: Network -> SigHash -> SigHash
maybeSetForkId net
    | isJust (getSigHashForkId net) = setForkIdFlag
    | otherwise = id

noEmptyInputs :: Tx -> Bool
noEmptyInputs = all (not . BS.null) . fmap scriptInput . txIn

pathToAddr :: XPubKey -> SoftPath -> Address
pathToAddr pubKey = xPubAddr . (`derivePubPath` pubKey)

parseTxCoins ::
       TxSignData -> XPubKey -> ([(OutPoint, TxOut)], [(OutPoint, TxOut)])
parseTxCoins (TxSignData tx inTxs inPaths _) pubKey =
    partition (isMyCoin . snd) coins
  where
    inAddrs = nub $ fmap (pathToAddr pubKey) inPaths
    coins = mapMaybe (findCoin inTxs . prevOutput) $ txIn tx
    isMyCoin to =
        case decodeTxOutAddr to of
            Right a -> a `elem` inAddrs
            _       -> False

findCoin :: [Tx] -> OutPoint -> Maybe (OutPoint, TxOut)
findCoin txs op@(OutPoint h i) = do
    matchTx <- find ((== h) . txHash) txs
    to <- txOut matchTx !!? fromIntegral i
    return (op, to)

