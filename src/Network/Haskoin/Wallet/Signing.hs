{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.Signing where

import           Control.Arrow                       (second, (&&&), (***))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson.TH                       (deriveJSON)
import qualified Data.ByteString                     as BS
import           Data.Default                        (def)
import           Data.Either                         (rights)
import           Data.List                           (find, nub, partition,
                                                      sortOn, sum)
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import           Data.Word                           (Word64)
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Crypto
import           Haskoin.Keys
import           Haskoin.Script
import qualified Haskoin.Store.Data                  as Store
import           Haskoin.Store.WebClient
import           Haskoin.Transaction
import           Haskoin.Util
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.FileIO
import           Network.Haskoin.Wallet.Util
import           Network.Haskoin.Wallet.WalletTx
import           Numeric.Natural
import           System.Random

-- Building Transactions --

buildTxSignData ::
       (MonadIO m, MonadReader Network m, MonadError String m)
    => AccountStore
    -> [(Address, Natural)]
    -> Natural
    -> Natural
    -> Bool
    -> m (TxSignData, Commit AccountStore)
buildTxSignData store rcpts feeByte dust rcptPay
    | null rcpts = throwError "No recipients provided"
    | otherwise = do
        net <- network
        acc <- liftEither $ accountStoreAccount store
        allCoins <-
            liftExcept $
            apiBatch
                20
                def {configNetwork = net}
                (GetAddrsUnspent (Map.keys walletAddrMap) def)
        gen <- liftIO newStdGen
        (tx, pickedCoins) <-
            liftEither $
            buildWalletTx net gen rcpts change allCoins feeByte dust rcptPay
        (inDerivs, outDerivs') <-
            liftEither $ getDerivs pickedCoins rcpts walletAddrMap
        let noChange = length (txOut tx) == length rcpts
            outDerivs =
                if noChange
                    then outDerivs'
                    else changeDeriv : outDerivs'
        -- Get dependant transactions
        let depTxHash = outPointHash . prevOutput <$> txIn tx
        depTxsRaw <-
            liftExcept $
            apiBatch 20 def {configNetwork = net} (GetTxsRaw depTxHash)
        let depTxs = Store.getRawResultList depTxsRaw
        return
            ( TxSignData tx depTxs inDerivs outDerivs acc False net
            , if noChange
                  then NoCommit store -- No change address was used
                  else newStore)
  where
    walletAddrMap = storeAddressMap store
    ((change, changeDeriv), newStore) = runAccountStore store genIntAddress

buildWalletTx ::
       Network
    -> StdGen
    -> [(Address, Natural)] -- recipients
    -> Address -- change
    -> [Store.Unspent] -- Coins to choose from
    -> Natural -- Fee per byte
    -> Natural -- Dust
    -> Bool -- Recipients Pay for Fee
    -> Either String (Tx, [Store.Unspent])
buildWalletTx net gen rcptsN change coins feeByteN dustN rcptPay = do
    (pickedCoins, changeAmnt) <-
        chooseCoins tot feeCoinSel (length rcptsN + 1) True (sortDesc coins)
    let nOuts =
            if changeAmnt <= dust
                then length rcptsN
                else length rcptsN + 1
        totFee = guessTxFee (fromIntegral feeByteN) nOuts (length pickedCoins)
    rcptsPayN <-
        if rcptPay
            then makeRcptsPay (fromIntegral totFee) rcptsN
            else return rcptsN
    let rcpts = second fromIntegral <$> rcptsPayN
        allRcpts
            | changeAmnt <= dust = rcpts
            | otherwise = (change, changeAmnt) : rcpts
        ops = Store.unspentPoint <$> pickedCoins
    when (any ((<= dust) . snd) allRcpts) $
        Left "Recipient output is smaller than the dust value"
    let rdmRcpts = evalState (randomShuffle allRcpts) gen
    tx <- buildAddrTx net ops =<< mapM (addrToText2 net) rdmRcpts
    return (tx, pickedCoins)
  where
    feeCoinSel =
        if rcptPay
            then 0
            else fromIntegral feeByteN :: Word64
    dust = fromIntegral dustN :: Word64
    tot = fromIntegral $ sum $ snd <$> rcptsN :: Word64

makeRcptsPay ::
       Natural -> [(Address, Natural)] -> Either String [(Address, Natural)]
makeRcptsPay fee rcpts =
    mapM f rcpts
  where
    f (a, v) = (a,) <$> maybeToEither err (v `safeSubtract` toPay)
    err = "Recipients can't pay for the fee"
    (q, r) = fee `quotRem` fromIntegral (length rcpts)
    toPay = if r == 0 then q else q + 1

getDerivs ::
       [Store.Unspent]
    -> [(Address, Natural)]
    -> Map Address SoftPath
    -> Either String ([SoftPath], [SoftPath])
getDerivs pickedCoins rcpts walletAddrMap = do
    selCoinAddrs <- maybeToEither err $ mapM Store.unspentAddress pickedCoins
    let inDerivs =
            Map.elems $
            Map.restrictKeys walletAddrMap $ Set.fromList selCoinAddrs
    return (inDerivs, outDerivs)
  where
    outDerivs = Map.elems $ Map.intersection walletAddrMap $ Map.fromList rcpts
    err = "Could not read unspent address in getDerivs"

-- Signing Transactions --

signingKey ::
       Network -> BS.ByteString -> Text -> Natural -> Either String XPrvKey
signingKey net pass mnem acc = do
    seed <- mnemonicToSeed pass mnem
    return $ derivePath (bip44Deriv net acc) (makeXPrvKey seed)

signWalletTx ::
       TxSignData
    -> XPrvKey
    -> Either String (TxSignData, WalletTx)
signWalletTx tsd@(TxSignData tx _ inPaths _ _ signed net) signKey = do
    when signed $ Left "The transaction is already signed"
    wTx <- parseTxSignData net pubKey tsd
        -- signing
    let myInputs = walletUnsignedTxMyInputs wTx
        sigInputs = mconcat $ lst3 <$> Map.elems myInputs
    signedTx <- signTx net tx sigInputs prvKeys
        -- validation
    let f i = (sigInputScript i, sigInputValue i, sigInputOP i)
        vDat = f <$> sigInputs
        isSigned = noEmptyInputs signedTx && verifyStdTx net signedTx vDat
    unless isSigned $ Left "The transaction could not be signed"
    return
        ( tsd {txSignDataTx = signedTx, txSignDataSigned = True}
        , unsignedToWalletTx signedTx wTx)
  where
    pubKey = deriveXPubKey signKey
    prvKeys = xPrvKey . (`derivePath` signKey) <$> inPaths

noEmptyInputs :: Tx -> Bool
noEmptyInputs = (not . any BS.null) . fmap scriptInput . txIn

-- Transaction Sweeping --

buildSweepSignData ::
       (MonadIO m, MonadReader Network m, MonadError String m)
    => AccountStore
    -> [Address]
    -> Natural
    -> Natural
    -> m ([TxSignData], Commit AccountStore)
buildSweepSignData store addrs feeByte dust
    | null addrs = throwError "No addresses provided to sweep"
    | otherwise = do
        net <- network
        acc <- liftEither $ accountStoreAccount store
        coins <-
            liftExcept $
            apiBatch
                10
                def {configNetwork = net}
                (GetAddrsUnspent addrs def {paramLimit = Just 0})
        when (null coins) $
            throwError "There are no coins to sweep in those addresses"
        gen <- liftIO newStdGen
        (txs, commitStore) <-
            liftEither $ buildSweepTxs net gen store coins feeByte dust
        res <- forM txs $ \(tx, pickedCoins, outDerivs) -> do
            let depTxHash = outPointHash . prevOutput <$> txIn tx
            depTxsRaw <-
                liftExcept $
                apiBatch 20 def {configNetwork = net} (GetTxsRaw depTxHash)
            let depTxs = Store.getRawResultList depTxsRaw
            (inDerivs, _) <- liftEither $ getDerivs pickedCoins [] walletAddrMap
            return $ TxSignData tx depTxs inDerivs outDerivs acc False net
        return (res, commitStore)
  where
    walletAddrMap = storeAddressMap store

buildSweepTxs ::
       Network
    -> StdGen
    -> AccountStore
    -> [Store.Unspent]
    -> Natural
    -> Natural
    -> Either String ([(Tx, [Store.Unspent], [SoftPath])], Commit AccountStore)
buildSweepTxs net gen store allCoins feeByte dust =
    flip evalState gen $
    retryEither 10 $ do
        shuffledCoins <- randomShuffle allCoins
        runExceptT $ runAccountStoreT store $ go shuffledCoins []
  where
    go [] acc = return acc
    go coins acc = do
        nIns <- lift . lift $ randomRange 2 5
        let (pickedCoins, restCoins) = splitAt nIns coins
            coinsTot = sum $ Store.unspentAmount <$> pickedCoins
            fee = guessTxFee (fromIntegral feeByte) 2 (length pickedCoins)
            amntTot = coinsTot - fee
            amntMin = fromIntegral dust
        when (amntTot < 2 * amntMin) $
            throwError "Could not find a sweep solution"
        amnt1 <- lift . lift $ randomRange amntMin (amntTot - amntMin)
        let amnt2 = amntTot - amnt1
        (addr1, deriv1) <- genExtAddress
        (addr2, deriv2) <- genExtAddress
        rcpts <- lift . lift $ randomShuffle [(addr1, amnt1), (addr2, amnt2)]
        rcptsTxt <- liftEither $ mapM (addrToText2 net) rcpts
        tx <-
            liftEither $
            buildAddrTx net (Store.unspentPoint <$> pickedCoins) rcptsTxt
        go restCoins ((tx, pickedCoins, [deriv1, deriv2]) : acc)

-- Utilities --

randomRange :: (Random a, MonadState StdGen m) => a -> a -> m a
randomRange low hi = do
    gen <- get
    let (res, newGen) = randomR (low, hi) gen
    put newGen
    return res

randomShuffle :: MonadState StdGen m => [a] -> m [a]
randomShuffle [] = return []
randomShuffle [x] = return [x]
randomShuffle xs = do
    i <- randomRange 0 (length xs - 1)
    let (as, (e:bs)) = splitAt i xs
    (e :) <$> randomShuffle (as <> bs)

retryEither ::
       MonadState StdGen m
    => Natural
    -> m (Either err a)
    -> m (Either err a)
retryEither 1 m = m
retryEither i m = either (const $ retryEither (i-1) m) (return . Right) =<< m

{-

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

findCoin :: [Tx] -> OutPoint -> Maybe (OutPoint, TxOut)
findCoin txs op@(OutPoint h i) = do
    matchTx <- find ((== h) . txHash) txs
    to <- txOut matchTx !!? fromIntegral i
    return (op, to)

 -}
