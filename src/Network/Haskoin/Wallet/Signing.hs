{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.Haskoin.Wallet.Signing where

import Conduit (MonadUnliftIO, ResourceT)
import Control.Arrow (second)
import Control.Monad (forM, unless, void, when, (<=<))
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    liftEither,
    runExceptT,
  )
import Control.Monad.State
  ( MonadIO (..),
    MonadState (get, put),
    MonadTrans (lift),
    StateT,
    evalStateT,
    gets,
  )
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Haskoin.Address (Address, textToAddr)
import Haskoin.Crypto
  ( Ctx,
    Fingerprint,
    SecKey,
    SoftPath,
    XPrvKey (key),
    XPubKey,
    derivePath,
    deriveXPubKey,
    makeXPrvKey,
    mnemonicToSeed,
    xPubFP,
  )
import Haskoin.Network (Network)
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
  ( ApiConfig (ApiConfig, host),
    GetAddrsUnspent (GetAddrsUnspent),
    GetTxsRaw (GetTxsRaw),
    LimitsParam (limit),
    apiBatch,
  )
import Haskoin.Transaction
  ( OutPoint (hash),
    SigInput (outpoint, script, value),
    Tx (inputs, outputs),
    TxIn (outpoint, script),
    buildAddrTx,
    chooseCoins,
    guessTxFee,
    signTx,
    verifyStdTx,
  )
import Haskoin.Util (maybeToEither)
import Network.Haskoin.Wallet.Config
import Network.Haskoin.Wallet.Database
import Network.Haskoin.Wallet.FileIO
  ( TxSignData
      ( TxSignData,
        txSignDataInputPaths,
        txSignDataSigned,
        txSignDataTx
      ),
  )
import Network.Haskoin.Wallet.TxInfo
  ( MyInputs (myInputsSigInput),
    OtherInputs (otherInputsSigInput),
    TxInfo,
    UnsignedTxInfo (unsignedTxInfoMyInputs, unsignedTxInfoOtherInputs),
    parseTxSignData,
    unsignedToTxInfo,
  )
import Network.Haskoin.Wallet.Util
  ( addrToText2,
    liftExcept,
    safeSubtract,
  )
import Numeric.Natural (Natural)
import System.Random (Random (randomR), StdGen, newStdGen, initStdGen)
import Data.Either (rights)

{- Building Transactions -}

buildTxSignData ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  DBAccount ->
  [(Address, Natural)] ->
  Natural ->
  Natural ->
  Bool ->
  ExceptT String (DB m) TxSignData
buildTxSignData net ctx accId acc rcpts feeByte dust rcptPay
  | null rcpts = throwError "No recipients provided"
  | otherwise = do
      -- Get all spendable coins in the account
      allCoins <- getSpendableCoins accId
      -- Get a change address
      (change, changeDeriv) <- peekInternalAddress ctx accId 0
      -- Build a transaction and pick the coins
      gen <- liftIO newStdGen
      (tx, pickedCoins) <-
        liftEither $
          buildWalletTx net ctx gen rcpts change allCoins feeByte dust rcptPay
      -- Get the derivations of our recipients and picked coins
      rcptsDerivs <- mapM (f . getAddrDeriv net) $ fst <$> rcpts
      inDerivs <- mapM (f . getCoinDeriv net) pickedCoins
      -- Check if we have a change output
      let noChange = length tx.outputs == length rcpts
          outDerivs =
            if noChange
              then rcptsDerivs
              else changeDeriv : rcptsDerivs
      -- Get the dependent transactions
      let depTxHash = (.hash) . (.outpoint) <$> tx.inputs
      depTxs <- mapM getRawTx depTxHash
      -- Commit the internal address if we used it
      unless noChange $ void $ commitInternalAddress accId Nothing
      -- Return the result
      let idx = fromIntegral $ dBAccountIndex acc
      return $ TxSignData tx depTxs inDerivs outDerivs idx False net
  where
    f = liftEither <=< lift

buildWalletTx ::
  Network ->
  Ctx ->
  StdGen -> -- Randomness for coin selection and change address order
  [(Address, Natural)] -> -- recipients
  Address -> -- change
  [Store.Unspent] -> -- Coins to choose from
  Natural -> -- Fee per byte
  Natural -> -- Dust
  Bool -> -- Recipients Pay for Fee
  Either String (Tx, [Store.Unspent])
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
      lift $
        Left "Recipient output is smaller than the dust value"
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

{- Signing Transactions -}

data MnemonicPass = MnemonicPass
  { mnemonicWords :: !Text,
    mnemonicPass :: !Text
  }
  deriving (Eq, Show)

-- Compute an account signing key
signingKey :: Network -> Ctx -> MnemonicPass -> Natural -> Either String XPrvKey
signingKey net ctx mnem acc = do
  seed <- mnemonicToSeed (mnemonicPass mnem) (mnemonicWords mnem)
  return $ derivePath ctx (bip44Deriv net acc) (makeXPrvKey seed)

-- Compute the unique wallet fingerprint given a mnemonic
walletFingerprint :: Network -> Ctx -> MnemonicPass -> Either String Fingerprint
walletFingerprint net ctx mnem = do
  xPrvKey <- signingKey net ctx mnem 0
  return $ xPubFP ctx $ deriveXPubKey ctx xPrvKey

signWalletTx ::
  Ctx ->
  TxSignData ->
  XPrvKey ->
  Either String (TxSignData, TxInfo)
signWalletTx ctx tsd@TxSignData {txSignDataInputPaths = inPaths} signKey =
  signTxWithKeys ctx tsd publicKey prvKeys
  where
    publicKey = deriveXPubKey ctx signKey
    prvKeys = (.key) . (\p -> derivePath ctx p signKey) <$> inPaths

signTxWithKeys ::
  Ctx ->
  TxSignData ->
  XPubKey ->
  [SecKey] ->
  Either String (TxSignData, TxInfo)
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
    ( tsd {txSignDataTx = signedTx, txSignDataSigned = True},
      unsignedToTxInfo signedTx txInfoU
    )

noEmptyInputs :: Tx -> Bool
noEmptyInputs = (not . any BS.null) . fmap (.script) . (.inputs)

{- Transaction Sweeping -}

buildSweepSignData ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  DBAccount ->
  [Address] ->
  Natural ->
  Natural ->
  ExceptT String (DB m) [TxSignData]
buildSweepSignData net ctx accId acc addrs feeByte dust
  | null addrs = throwError "No addresses provided to sweep"
  | otherwise = do
      -- Get the unspent coins of the addresses
      Store.SerialList coins <-
        liftExcept . apiBatch ctx coinBatch (conf net) $
          GetAddrsUnspent addrs def {limit = Just 0}
      when (null coins) $
        throwError "There are no coins to sweep in those addresses"
      -- Build a set of sweep transactions
      gen <- liftIO initStdGen
      (txs, intCount) <-
        evalStateT (buildSweepTxs net ctx accId coins feeByte dust) gen
      -- For each sweep transaction
      res <- forM txs $ \(tx, pickedCoins, outDerivs) -> do
        -- Get the dependent transactions
        let depTxHash = (.hash) . (.outpoint) <$> (.inputs) tx
        Store.RawResultList depTxs <-
          liftExcept . apiBatch ctx txFullBatch (conf net) $
            GetTxsRaw depTxHash
        -- Check if any of the coins belong to us
        resE <- lift $ mapM (getCoinDeriv net) pickedCoins
        let inDerivs = rights resE
            accIdx = fromIntegral $ dBAccountIndex acc
        return $ TxSignData tx depTxs inDerivs outDerivs accIdx False net
      -- Commit the internal addresses used
      _ <- commitInternalAddress accId (Just intCount)
      return res

buildSweepTxs ::
     (MonadUnliftIO m)
  => Network
  -> Ctx
  -> DBAccountId
  -> [Store.Unspent]
  -> Natural
  -> Natural
  -> StateT
       StdGen
       (ExceptT String (DB m))
       ([(Tx, [Store.Unspent], [SoftPath])], Natural)
buildSweepTxs net ctx accId allCoins feeByte dust = do
  liftEither <=< retryEither 10 $ do
    shuffledCoins <- randomShuffle allCoins
    runExceptT $ go shuffledCoins [] 0
  where
    go [] acc idx = return (acc, fromIntegral idx)
    go coins acc idx = do
      nIns <- randomRange 1 5
      let (pickedCoins, restCoins) = splitAt nIns coins
          coinsTot = sum $ (.value) <$> pickedCoins
          fee = guessTxFee (fromIntegral feeByte) 2 (length pickedCoins)
          amntTot = coinsTot - fee
          amntMin = fromIntegral dust + 1
      when (amntTot < 2 * amntMin) $
        throwError "Could not find a sweep solution"
      amnt1 <- randomRange amntMin (amntTot - amntMin)
      let amnt2 = amntTot - amnt1
      (addr1, deriv1) <- lift . lift $ peekInternalAddress ctx accId idx
      (addr2, deriv2) <- lift . lift $ peekInternalAddress ctx accId (idx+1)
      rcpts <- randomShuffle [(addr1, amnt1), (addr2, amnt2)]
      rcptsTxt <- liftEither $ mapM (addrToText2 net) rcpts
      tx <-
        liftEither $
          buildAddrTx net ctx ((.outpoint) <$> pickedCoins) rcptsTxt
      go restCoins ((tx, pickedCoins, [deriv1, deriv2]) : acc) (idx+2)

-- Utilities --

randomRange :: (Random a, MonadState StdGen m) => a -> a -> m a
randomRange low hi = do
  gen <- get
  let (res, newGen) = randomR (low, hi) gen
  put newGen
  return res

randomShuffle :: (MonadState StdGen m) => [a] -> m [a]
randomShuffle [] = return []
randomShuffle [x] = return [x]
randomShuffle xs = do
  i <- randomRange 0 (length xs - 1)
  case splitAt i xs of
    (as, e : bs) -> (e :) <$> randomShuffle (as <> bs)
    _ -> error "randomShuffle"

retryEither ::
     (Monad m) => Natural -> m (Either String a) -> m (Either String a)
retryEither 0 _ = error "Must retryEither at least 1 time"
retryEither 1 m = m
retryEither i m = do
  resE <- m
  case resE of
    Left _ -> retryEither (i-1) m
    Right _ -> return resE

