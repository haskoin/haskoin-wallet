{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.Signing where

import           Control.Arrow                           ((&&&))
import           Data.Aeson.TH                           (deriveJSON)
import           Data.List                               (nub, sum)
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import           Data.Ord
import qualified Data.Set                                as Set
import           Foundation
import           Foundation.Collection
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.FoundationCompat hiding (addrToBase58)
import           Network.Haskoin.Wallet.HTTP
import           Network.Haskoin.Wallet.TxInformation

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

buildTxSignData :: AccountStore
                -> Map Address Satoshi
                -> Satoshi
                -> Satoshi
                -> IO (Either String (TxSignData, AccountStore))
buildTxSignData store rcpMap feeByte dust
    | Map.null rcpMap = return $ Left "No recipients provided"
    | otherwise = do
        allCoins <-
            fmap toWalletCoin <$> httpUnspent (Map.keys walletAddrMap)
        case buildWalletTx walletAddrMap allCoins change rcpMap feeByte dust of
            Right (tx, depTxHash, inDeriv, outDeriv) -> do
                depTxs <- (fst <$>) <$> httpTxs depTxHash
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

buildWalletTx :: Map Address SoftPath -- All account addresses
              -> [WalletCoin]
              -> (Address, SoftPath, Natural) -- change
              -> Map Address Satoshi -- recipients
              -> Satoshi -- Fee per byte
              -> Satoshi -- Dust
              -> Either String (Tx, [TxHash], [SoftPath], [SoftPath])
buildWalletTx walletAddrMap coins (change, changeDeriv, _) rcpMap feeByte dust = do
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
        buildAddrTx ops $
        bimap addrToBase58 fromIntegral <$> Map.assocs txRcpMap
    inCoinAddrs <-
        eitherString $
        mapM (outputAddress . walletCoinScriptOutput) selectedCoins
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
    descCoins = sortBy (comparing Down) coins

buildSwipeTx ::
       AccountStore
    -> [Address]
    -> Satoshi -- Fee per byte
    -> IO (Either String (TxSignData, AccountStore))
buildSwipeTx store addrs feeByte = do
    coins <- fmap toWalletCoin <$> httpUnspent addrs
    let tot = sum $ walletCoinValue <$> coins
        fee = guessTxFee (fromIntegral feeByte) 1 (fromCount $ length coins)
        ops = walletCoinOutPoint <$> coins
        depTxHash = nub $ outPointHash <$> ops
    if null coins
        then return $ Left "No coins were found in the given addresses"
        else do
            depTxs <- (fst <$>) <$> httpTxs depTxHash
            return $
                eitherString $ do
                    amnt <-
                        maybeToEither "The inputs do not cover the required fee" $
                        tot - fromIntegral fee
                    tx <-
                        buildAddrTx
                            ops
                            [(addrToBase58 (fst rcpt), fromIntegral amnt)]
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

bip44Deriv :: Natural -> HardPath
bip44Deriv a = Deriv :| 44 :| bip44Coin :| fromIntegral a

signingKey :: String -> String -> Natural -> Either String XPrvKey
signingKey pass mnem acc = do
    seed <- eitherString $ mnemonicToSeed (stringToBS pass) (stringToBS mnem)
    return $ derivePath (bip44Deriv acc) (makeXPrvKey seed)

data TxSignData = TxSignData
    { txSignDataTx          :: !Tx
    , txSignDataInputs      :: ![Tx]
    , txSignDataInputPaths  :: ![SoftPath]
    , txSignDataOutputPaths :: ![SoftPath]
    } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 10) ''TxSignData)

pubTxInfo :: TxSignData -> XPubKey -> Either String TxInformation
pubTxInfo (TxSignData tx inTxs inPaths outPaths) pubKey
    | fromCount (length coins) /= fromCount (length $ txIn tx) =
        Left "Referenced input transactions are missing"
    | fromCount (length inPaths) /= Map.size (txInfoMyInputs info) =
        Left "Tx is missing inputs from private keys"
    | fromCount (length outPaths) /= Map.size (txInfoInbound info) =
        Left "Tx is missing change outputs"
    | otherwise = return info
  where
    info =
        txInfoFillUnsignedTx tx $
        txInfoFillInbound outAddrMap (txOut tx) $
        txInfoFillInputs inAddrMap (snd <$> coins) emptyTxInfo
    outAddrMap = Map.fromList $ fmap (pathToAddr pubKey &&& id) outPaths
    inAddrMap = Map.fromList $ fmap (pathToAddr pubKey &&& id) inPaths
    coins = mapMaybe (findCoin inTxs . prevOutput) $ txIn tx

signWalletTx :: TxSignData -> XPrvKey -> Either String (TxInformation, Tx, Bool)
signWalletTx tsd@(TxSignData tx _ inPaths _) signKey = do
    sigDat <- mapM g myCoins
    signedTx <- eitherString $ signTx tx (fmap f sigDat) prvKeys
    let vDat = rights $ fmap g allCoins
        isSigned = noEmptyInputs signedTx && verifyStdTx signedTx vDat
    info <- pubTxInfo tsd pubKey
    return
        ( if isSigned
              then txInfoFillTx signedTx info
              else info
        , signedTx
        , isSigned)
  where
    pubKey = deriveXPubKey signKey
    (myCoins, othCoins) = parseTxCoins tsd pubKey
    allCoins = myCoins <> othCoins
    prvKeys = fmap (toPrvKeyG . xPrvKey . (`derivePath` signKey)) inPaths
    f (so, val, op) = SigInput so val op (maybeSetForkId sigHashAll) Nothing
    g (op, to) = (, outValue to, op) <$> decodeTxOutSO to

signSwipeTx :: TxSignData -> [PrvKey] -> Either String (TxInformation, Tx, Bool)
signSwipeTx (TxSignData tx inTxs _ _) prvKeys = do
    sigDat <- mapM g coins
    signedTx <- eitherString $ signTx tx (fmap f sigDat) prvKeys
    let isSigned = noEmptyInputs signedTx && verifyStdTx signedTx sigDat
    return
        ( if isSigned
              then txInfoFillTx signedTx info
              else info
        , signedTx
        , isSigned)
  where
    info = txInfoFillInputs Map.empty (snd <$> coins) emptyTxInfo
    coins = mapMaybe (findCoin inTxs . prevOutput) $ txIn tx
    f (so, val, op) = SigInput so val op (maybeSetForkId sigHashAll) Nothing
    g (op, to) = (, outValue to, op) <$> decodeTxOutSO to

maybeSetForkId :: SigHash -> SigHash
maybeSetForkId
    | isJust sigHashForkId = setForkIdFlag
    | otherwise = id

noEmptyInputs :: Tx -> Bool
noEmptyInputs = all (not . null) . fmap (asBytes scriptInput) . txIn

pathToAddr :: XPubKey -> SoftPath -> Address
pathToAddr pubKey = xPubAddr . (`derivePubPath` pubKey)

parseTxCoins :: TxSignData -> XPubKey
             -> ([(OutPoint, TxOut)],[(OutPoint, TxOut)])
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
    to <- txOut matchTx ! fromIntegral i
    return (op, to)

