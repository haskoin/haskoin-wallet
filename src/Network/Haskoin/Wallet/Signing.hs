{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
module Network.Haskoin.Wallet.Signing where

import           Control.Arrow                       (second, (&&&), (***))
import           Control.Monad                       (when, unless, forM, (<=<))
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Aeson.TH                       (deriveJSON)
import qualified Data.ByteString                     as BS
import           Data.Default                        (def)
import           Data.Either                         (rights)
import           Data.List                           (find, nub, partition,
                                                      sort, sortOn, sum)
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import           Data.Word                           (Word64)
import           Haskoin.Address
import           Haskoin.Crypto
import           Haskoin.Network
import           Haskoin.Script
import qualified Haskoin.Store.Data                  as Store
import           Haskoin.Store.WebClient
import           Haskoin.Transaction
import           Haskoin.Util
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.FileIO
import           Network.Haskoin.Wallet.TxInfo
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           System.Random
import Data.Serialize (encode)

-- Building Transactions --

conf :: Network -> ApiConfig
conf net = ApiConfig net (def :: ApiConfig).host

buildTxSignData ::
       (MonadIO m, MonadError String m, MonadState AccountStore m)
    => Network
    -> Ctx
    -> [(Address, Natural)]
    -> Natural
    -> Natural
    -> Bool
    -> m TxSignData
buildTxSignData net ctx rcpts feeByte dust rcptPay
    | null rcpts = throwError "No recipients provided"
    | otherwise = do
        origStore <- get
        acc <- liftEither =<< gets accountStoreAccount
        walletAddrMap <- gets (storeAddressMap ctx)
        let req = GetAddrsUnspent (Map.keys walletAddrMap) def
        Store.SerialList allCoins <- liftExcept $ apiBatch ctx 20 (conf net) req
        (change, changeDeriv) <- genIntAddress ctx
        gen <- liftIO newStdGen
        (tx, pickedCoins) <-
            liftEither $
            buildWalletTx net ctx gen rcpts change allCoins feeByte dust rcptPay
        (inDerivs, outDerivs') <-
            liftEither $ getDerivs pickedCoins rcpts walletAddrMap
        let noChange = length (tx.outputs) == length rcpts
            outDerivs =
                if noChange
                    then outDerivs'
                    else changeDeriv : outDerivs'
            -- Get dependant transactions
        let depTxHash = (.hash) . (.outpoint) <$> tx.inputs
        depTxsRaw <- liftExcept $ apiBatch ctx 20 (conf net) (GetTxsRaw depTxHash)
        let depTxs = depTxsRaw.get
        when noChange $ put origStore -- Rollback store changes
        return $ TxSignData tx depTxs inDerivs outDerivs acc False net

buildWalletTx ::
       Network
    -> Ctx
    -> StdGen -- Randomness for coin selection and change address order
    -> [(Address, Natural)] -- recipients
    -> Address -- change
    -> [Store.Unspent] -- Coins to choose from
    -> Natural -- Fee per byte
    -> Natural -- Dust
    -> Bool -- Recipients Pay for Fee
    -> Either String (Tx, [Store.Unspent])
buildWalletTx net ctx gen rcptsN change coins feeByteN dustN rcptPay =
    flip evalStateT gen $ do
        rdmCoins <- randomShuffle coins
        (pickedCoins, changeAmnt) <-
            lift $ chooseCoins tot feeCoinSel (length rcptsN + 1) False rdmCoins
        let nOuts =
                if changeAmnt <= dust
                    then length rcptsN
                    else length rcptsN + 1
            totFee =
                guessTxFee (fromIntegral feeByteN) nOuts (length pickedCoins)
        rcptsPayN <-
            if rcptPay
                then lift $ makeRcptsPay (fromIntegral totFee) rcptsN
                else return rcptsN
        let rcpts = second fromIntegral <$> rcptsPayN
            allRcpts
                | changeAmnt <= dust = rcpts
                | otherwise = (change, changeAmnt) : rcpts
            ops = (.outpoint) <$> pickedCoins
        when (any ((<= dust) . snd) allRcpts) $
            lift $ Left "Recipient output is smaller than the dust value"
        rdmRcpts <- randomShuffle allRcpts
        tx <- lift $ buildAddrTx net ctx ops =<< mapM (addrToText2 net) rdmRcpts
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
    selCoinAddrs <- maybeToEither err $ mapM (.address) pickedCoins
    let inDerivs =
            Map.elems $
            Map.restrictKeys walletAddrMap $ Set.fromList selCoinAddrs
    return (inDerivs, outDerivs)
  where
    outDerivs = Map.elems $ Map.intersection walletAddrMap $ Map.fromList rcpts
    err = "Could not read unspent address in getDerivs"

-- Signing Transactions --

signingKey :: Network -> Ctx -> Text -> Text -> Natural -> Either String XPrvKey
signingKey net ctx pass mnem acc = do
    seed <- mnemonicToSeed pass mnem
    return $ derivePath ctx (bip44Deriv net acc) (makeXPrvKey seed)

signWalletTx ::
       Ctx
    -> TxSignData
    -> XPrvKey
    -> Either String (TxSignData, TxInfo)
signWalletTx ctx tsd@TxSignData{ txSignDataInputPaths = inPaths } signKey =
    signTxWithKeys ctx tsd publicKey prvKeys
  where
    publicKey = deriveXPubKey ctx signKey
    prvKeys = (.key) . (\p -> derivePath ctx p signKey) <$> inPaths

signTxWithKeys ::
       Ctx
    -> TxSignData
    -> XPubKey
    -> [SecKey]
    -> Either String (TxSignData, TxInfo)
signTxWithKeys ctx tsd@(TxSignData tx _ _ _ _ signed net) publicKey secKeys = do
    when signed $ Left "The transaction is already signed"
    txInfoU <- parseTxSignData net ctx publicKey tsd
        -- signing
    let myInputs = unsignedTxInfoMyInputs txInfoU
        othInputs = unsignedTxInfoOtherInputs txInfoU
        mySigInputs = mconcat $ myInputsSigInput <$> Map.elems myInputs
        othSigInputs = mconcat $ otherInputsSigInput <$> Map.elems othInputs
        sigInputs = mySigInputs <> othSigInputs
    signedTx <- signTx net ctx tx sigInputs secKeys
        -- validation
    let f i = (i.script, i.value, i.outpoint)
        vDat = f <$> sigInputs
        isSigned = noEmptyInputs signedTx && verifyStdTx net ctx signedTx vDat
    unless isSigned $ Left "The transaction could not be signed"
    return
        ( tsd {txSignDataTx = signedTx, txSignDataSigned = True}
        , unsignedToTxInfo signedTx txInfoU)

noEmptyInputs :: Tx -> Bool
noEmptyInputs = (not . any BS.null) . fmap (.script) . (.inputs)

-- Transaction Sweeping --

buildSweepSignData ::
       (MonadIO m, MonadError String m, MonadState AccountStore m)
    => Network
    -> Ctx
    -> [Address]
    -> Natural
    -> Natural
    -> m [TxSignData]
buildSweepSignData net ctx addrs feeByte dust
    | null addrs = throwError "No addresses provided to sweep"
    | otherwise = do
        acc <- liftEither =<< gets accountStoreAccount
        walletAddrMap <- gets (storeAddressMap ctx)
        let req = GetAddrsUnspent addrs def {limit = Just 0}
        Store.SerialList coins <-
            liftExcept $ apiBatch ctx 20 (conf net) req
        when (null coins) $
            throwError "There are no coins to sweep in those addresses"
        gen <- liftIO newStdGen
        txs <- evalStateT (buildSweepTxs net ctx coins feeByte dust) gen
        forM txs $ \(tx, pickedCoins, outDerivs) -> do
            let depTxHash = (.hash) . (.outpoint) <$> (.inputs) tx
            depTxsRaw <-
                liftExcept $
                apiBatch ctx 20 (conf net) (GetTxsRaw depTxHash)
            let depTxs = depTxsRaw.get
            (inDerivs, _) <- liftEither $ getDerivs pickedCoins [] walletAddrMap
            return $ TxSignData tx depTxs inDerivs outDerivs acc False net

buildSweepTxs ::
       (MonadError String m, MonadState AccountStore m)
    => Network
    -> Ctx
    -> [Store.Unspent]
    -> Natural
    -> Natural
    -> StateT StdGen m [(Tx, [Store.Unspent], [SoftPath])]
buildSweepTxs net ctx allCoins feeByte dust = do
    origStore <- lift get
    liftEither <=< retryEither 10 $ do
        lift $ put origStore -- Retry with the original store
        shuffledCoins <- randomShuffle allCoins
        runExceptT $ go shuffledCoins []
  where
    go [] acc = return acc
    go coins acc = do
        nIns <- randomRange 1 5
        let (pickedCoins, restCoins) = splitAt nIns coins
            coinsTot = sum $ (.value) <$> pickedCoins
            fee = guessTxFee (fromIntegral feeByte) 2 (length pickedCoins)
            amntTot = coinsTot - fee
            amntMin = fromIntegral dust
        when (amntTot < 2 * amntMin) $
            throwError "Could not find a sweep solution"
        amnt1 <- randomRange amntMin (amntTot - amntMin)
        let amnt2 = amntTot - amnt1
        (addr1, deriv1) <- lift $ lift (genExtAddress ctx)
        (addr2, deriv2) <- lift $ lift (genExtAddress ctx)
        rcpts <- randomShuffle [(addr1, amnt1), (addr2, amnt2)]
        rcptsTxt <- liftEither $ mapM (addrToText2 net) rcpts
        tx <-
            liftEither $
            buildAddrTx net ctx ((.outpoint) <$> pickedCoins) rcptsTxt
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
retryEither 0 _ = error "Must retryEither at least 1 time"
retryEither 1 m = m
retryEither i m = either (const $ retryEither (i-1) m) (return . Right) =<< m

