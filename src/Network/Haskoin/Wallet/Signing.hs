{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.Signing where

import           Control.Arrow                       (second, (&&&), (***))
import           Control.Monad.Except
import           Control.Monad.Reader
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

{- Building Transactions -}

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
        (tx, selCoins) <-
            liftEither $
            buildWalletTx net rcpts change allCoins feeByte dust rcptPay
        (inDerivs, outDerivs) <-
            liftEither $ getDerivs tx selCoins rcpts walletAddrMap changeDeriv
        -- Get dependant transactions
        let depTxHash = outPointHash . prevOutput <$> txIn tx
        depTxsRaw <-
            liftExcept $
            apiBatch 20 def {configNetwork = net} (GetTxsRaw depTxHash)
        let depTxs = Store.getRawResultList depTxsRaw
        return
            ( TxSignData tx depTxs inDerivs outDerivs acc False net
            , if length (txOut tx) == length rcpts
                  then NoCommit store -- No change address was used
                  else newStore)
  where
    walletAddrMap = storeAddressMap store
    ((change, changeDeriv), newStore) = genIntAddress store

buildWalletTx ::
       Network
    -> [(Address, Natural)] -- recipients
    -> Address -- change
    -> [Store.Unspent] -- Coins to choose from
    -> Natural -- Fee per byte
    -> Natural -- Dust
    -> Bool -- Recipients Pay for Fee
    -> Either String (Tx, [Store.Unspent])
buildWalletTx net rcptsN change coins feeByteN dustN rcptPay = do
    (selCoins, changeAmnt) <-
        chooseCoins tot feeCoinSel (length rcptsN + 1) True (sortDesc coins)
    let nOuts =
            if changeAmnt <= dust
                then length rcptsN
                else length rcptsN + 1
        totFee = guessTxFee (fromIntegral feeByteN) nOuts (length selCoins)
    rcptsPayN <-
        if rcptPay
            then makeRcptsPay (fromIntegral totFee) rcptsN
            else return rcptsN
    let rcpts = second fromIntegral <$> rcptsPayN
        allRcpts
            | changeAmnt <= dust = rcpts
            | otherwise = (change, changeAmnt) : rcpts -- TODO: Randomize this
        ops = Store.unspentPoint <$> selCoins
    when (any ((<= dust) . snd) allRcpts) $
        Left "Recipient output is smaller than the dust value"
    tx <- buildAddrTx net ops =<< mapM (addrToText2 net) allRcpts
    return (tx, selCoins)
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
       Tx
    -> [Store.Unspent]
    -> [(Address, Natural)]
    -> Map Address SoftPath
    -> SoftPath
    -> Either String ([SoftPath], [SoftPath])
getDerivs tx selCoins rcpts walletAddrMap changeDeriv = do
    selCoinAddrs <- maybeToEither err $ mapM Store.unspentAddress selCoins
    let inDerivs =
            Map.elems $
            Map.restrictKeys walletAddrMap $ Set.fromList selCoinAddrs
    return (inDerivs, outDerivs)
  where
    myOutDeriv = Map.elems $ Map.intersection walletAddrMap $ Map.fromList rcpts
    outDerivs =
        if length (txOut tx) == length rcpts
            then myOutDeriv -- No change address was used
            else changeDeriv : myOutDeriv
    err = "Could not read unspent address in buildWalletTx"

{- Signing Transactions -}

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
