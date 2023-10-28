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
import Data.Either (rights)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Haskoin.Address (Address, addrToText, textToAddr)
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
    txHash,
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
import Network.Haskoin.Wallet.Util
  ( addrToText2,
    liftExcept,
    safeSubtract,
  )
import Numeric.Natural (Natural)
import System.Random (Random (randomR), StdGen, initStdGen)

{- Building Transactions -}

buildTxSignData ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  StdGen ->
  DBAccountId ->
  [(Address, Natural)] ->
  Natural ->
  Natural ->
  Bool ->
  ExceptT String (DB m) TxSignData
buildTxSignData net ctx gen accId rcpts feeByte dust rcptPay
  | null rcpts = throwError "No recipients provided"
  | otherwise = do
      -- Get all spendable coins in the account
      allCoins <- getSpendableCoins accId
      -- Get a change address
      dbAddr <- nextFreeIntAddr ctx accId
      (change, changeDeriv) <- liftEither $ fromDBAddr net dbAddr
      -- Build a transaction and pick the coins
      (tx, pickedCoins) <-
        liftEither $
          buildWalletTx net ctx gen rcpts change allCoins feeByte dust rcptPay
      -- Get the derivations of our recipients and picked coins
      rcptsDerivsE <- mapM (lift . getAddrDeriv net accId) $ fst <$> rcpts
      let rcptsDerivs = rights rcptsDerivsE -- It's OK to fail here
      inDerivs <- mapM (liftEither <=< lift . getCoinDeriv net accId) pickedCoins
      -- Check if we have a change output
      let noChange = length tx.outputs == length rcpts
          outDerivs =
            if noChange
              then rcptsDerivs
              else changeDeriv : rcptsDerivs
      -- Get the dependent transactions
      let depTxHash = (.hash) . (.outpoint) <$> tx.inputs
      depTxs <- mapM getRawTx depTxHash
      -- Return the result
      return $ TxSignData tx depTxs (nub inDerivs) (nub outDerivs) False

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
  Network ->
  Ctx ->
  TxSignData ->
  XPrvKey ->
  Either String (TxSignData, TxInfo)
signWalletTx net ctx tsd@TxSignData {txSignDataInputPaths = inPaths} signKey =
  signTxWithKeys net ctx tsd publicKey prvKeys
  where
    publicKey = deriveXPubKey ctx signKey
    prvKeys = (.key) . (\p -> derivePath ctx p signKey) <$> inPaths

signTxWithKeys ::
  Network ->
  Ctx ->
  TxSignData ->
  XPubKey ->
  [SecKey] ->
  Either String (TxSignData, TxInfo)
signTxWithKeys net ctx tsd@(TxSignData tx _ _ _ signed) publicKey secKeys = do
  when signed $ Left "The transaction is already signed"
  txInfoU <- parseTxSignData net ctx publicKey tsd
  -- signing
  let myInputs = unsignedTxInfoMyInputs txInfoU
      othInputs = unsignedTxInfoOtherInputs txInfoU
      mySigInputs = mconcat $ myInputsSigInput <$> Map.elems myInputs
      othSigInputs = mconcat $ otherInputsSigInput <$> Map.elems othInputs
      sigInputs = mySigInputs <> othSigInputs
  signedTx <- signTx net ctx tx sigInputs secKeys
  let txInfo = unsignedToTxInfo signedTx txInfoU
      isSigned = verifyTxInfo net ctx signedTx txInfo
  unless isSigned $ Left "The transaction could not be signed"
  return
    ( tsd {txSignDataTx = signedTx, txSignDataSigned = True},
      txInfo
    )

verifyTxInfo :: Network -> Ctx -> Tx -> TxInfo -> Bool
verifyTxInfo net ctx tx txInfo =
  let myInputs = txInfoMyInputs txInfo
      othInputs = txInfoOtherInputs txInfo
      mySigInputs = mconcat $ myInputsSigInput <$> Map.elems myInputs
      othSigInputs = mconcat $ otherInputsSigInput <$> Map.elems othInputs
      sigInputs = mySigInputs <> othSigInputs
      f i = (i.script, i.value, i.outpoint)
      vDat = f <$> sigInputs
   in txHash tx == txInfoHash txInfo
        && noEmptyInputs tx
        && verifyStdTx net ctx tx vDat

noEmptyInputs :: Tx -> Bool
noEmptyInputs = (not . any BS.null) . fmap (.script) . (.inputs)

{- Transaction Sweeping -}

buildSweepSignData ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  [Address] ->
  [Address] ->
  Natural ->
  Natural ->
  ExceptT String (DB m) TxSignData
buildSweepSignData net ctx accId sweepFrom sweepTo feeByte dust
  | null sweepFrom = throwError "No addresses to sweep from"
  | null sweepTo = throwError "No addresses to sweep to"
  | otherwise = do
      -- Get the unspent coins of the sweepFrom addresses
      Store.SerialList coins <-
        liftExcept . apiBatch ctx coinBatch (conf net) $
          GetAddrsUnspent sweepFrom def {limit = Just 0}
      when (null coins) $
        throwError "There are no coins to sweep in those addresses"
      -- Build a set of sweep transactions
      gen <- liftIO initStdGen
      tx <- liftEither $ buildSweepTx net ctx gen coins sweepTo feeByte dust
      -- Get the dependent transactions
      let depTxHash = (.hash) . (.outpoint) <$> (.inputs) tx
      Store.RawResultList depTxs <-
        liftExcept . apiBatch ctx txFullBatch (conf net) $
          GetTxsRaw depTxHash
      -- Check if any of the coins belong to us
      inDerivs <- rights <$> lift (mapM (getAddrDeriv net accId) sweepFrom)
      -- Check if any of the sweepTo addrs belong to us
      outDerivs <- rights <$> lift (mapM (getAddrDeriv net accId) sweepTo)
      return $ TxSignData tx depTxs (nub inDerivs) (nub outDerivs) False

buildSweepTx ::
  Network ->
  Ctx ->
  StdGen ->
  [Store.Unspent] ->
  [Address] ->
  Natural ->
  Natural ->
  Either String Tx
buildSweepTx net ctx gen coins sweepTo feeByte dust =
  flip evalStateT gen $ do
    rdmOutpoints <- ((.outpoint) <$>) <$> randomShuffle coins
    rdmSweepTo <- randomShuffle sweepTo
    let coinsTot = sum $ (.value) <$> coins
        fee = guessTxFee (fromIntegral feeByte) (length sweepTo) (length coins)
    when (coinsTot < fee) $
      throwError "Could not find a sweep solution: fee is too large"
    rdmAmntsM <-
      randomSplitIn
        (coinsTot - fee) -- will not overflow
        (fromIntegral $ length sweepTo)
        (fromIntegral $ dust + 1)
    rdmAmnts <- lift $ maybeToEither "Could not find a sweep solution" rdmAmntsM
    addrsT <- lift $ mapM (maybeToEither "Addr" . addrToText net) rdmSweepTo
    lift $ buildAddrTx net ctx rdmOutpoints (zip addrsT rdmAmnts)

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

-- Split the number a into n parts of minimum value d
randomSplitIn ::
  (Random a, Num a, Ord a, MonadState StdGen m) => a -> a -> a -> m (Maybe [a])
randomSplitIn a n d
  | a < n * d = return Nothing
  | n == 1 = return $ Just [a]
  | otherwise = do
      randPart <- randomRange d (a - d * (n - 1))
      ((randPart :) <$>) <$> randomSplitIn (a - randPart) (n - 1) d
