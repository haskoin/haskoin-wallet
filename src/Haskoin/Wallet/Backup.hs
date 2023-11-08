{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Haskoin.Wallet.Backup where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Data.Aeson (object, (.:), (.=))
import qualified Data.Aeson as Json
import Data.Default (def)
import Data.List (nub, sort, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Serialize as S
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Esqueleto.Legacy as E
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)

newtype WalletBackup = WalletBackup [AccountBackup]
  deriving (Eq, Show)

instance MarshalJSON Ctx WalletBackup where
  marshalValue ctx (WalletBackup accs) =
    object ["accounts" .= (marshalValue ctx <$> accs)]
  unmarshalValue ctx =
    Json.withObject "WalletBackup" $ \o -> do
      accs <- mapM (unmarshalValue ctx) =<< o .: "accounts"
      return $ WalletBackup accs

data AccountBackup = AccountBackup
  { accountBackupName :: !Text,
    accountBackupWallet :: !Fingerprint,
    accountBackupPubKey :: !XPubKey,
    accountBackupNetwork :: !Network,
    accountBackupExternal :: !Natural,
    accountBackupInternal :: !Natural,
    accountBackupLabels :: Map Address Text,
    accountBackupFree :: [Address],
    accountBackupCreated :: !UTCTime
  }
  deriving (Eq, Show)

instance MarshalJSON Ctx AccountBackup where
  marshalValue ctx AccountBackup {..} =
    let net = accountBackupNetwork
        f = Map.mapKeys (fromJust . addrToText net)
     in object
          [ "name" .= accountBackupName,
            "wallet" .= accountBackupWallet,
            "xpubkey"
              .= marshalValue (accountBackupNetwork, ctx) accountBackupPubKey,
            "network" .= accountBackupNetwork.name,
            "external" .= accountBackupExternal,
            "internal" .= accountBackupInternal,
            "labels" .= f accountBackupLabels,
            "free" .= (fromJust . addrToText net <$> accountBackupFree),
            "created" .= accountBackupCreated
          ]
  unmarshalValue ctx =
    Json.withObject "AccountBackup" $ \o -> do
      net <- maybe mzero pure . netByName =<< o .: "network"
      let f = Map.mapKeys (fromJust . textToAddr net)
      AccountBackup
        <$> o .: "name"
        <*> o .: "wallet"
        <*> (unmarshalValue (net, ctx) =<< o .: "xpubkey")
        <*> pure net
        <*> o .: "external"
        <*> o .: "internal"
        <*> (f <$> o .: "labels")
        <*> ((fromJust . textToAddr net <$>) <$> o .: "free")
        <*> o .: "created"

createBackup :: (MonadUnliftIO m) => Ctx -> ExceptT String (DB m) WalletBackup
createBackup ctx = do
  accs <- lift getAccounts
  backs <-
    forM accs $ \(_, acc@DBAccount {..}) -> do
      let net = accountNetwork acc
          (DBWalletKey fpT) = dBAccountWallet
      fp <- liftEither $ textToFingerprint fpT
      labelVals <-
        lift . select . from $ \a -> do
          where_ $
            a ^. DBAddressAccountWallet ==. val dBAccountWallet
              &&. a ^. DBAddressAccountDerivation ==. val dBAccountDerivation
              &&. a ^. DBAddressInternal ==. val False
              &&. a ^. DBAddressLabel !=. val ""
          return (a ^. DBAddressAddress, a ^. DBAddressLabel)
      labels <- forM labelVals $ \(Value at, Value l) -> do
        a <- liftMaybe "Address" $ textToAddr net at
        return (a, l)
      freeVals <-
        lift . select . from $ \a -> do
          where_ $
            a ^. DBAddressAccountWallet ==. val dBAccountWallet
              &&. a ^. DBAddressAccountDerivation ==. val dBAccountDerivation
              &&. a ^. DBAddressInternal ==. val True
              &&. a ^. DBAddressFree ==. val True
          return $ a ^. DBAddressAddress
      free <- mapM (liftMaybe "Address" . textToAddr net . unValue) freeVals
      return
        AccountBackup
          { accountBackupName = dBAccountName,
            accountBackupWallet = fp,
            accountBackupPubKey = accountXPubKey ctx acc,
            accountBackupNetwork = accountNetwork acc,
            accountBackupExternal = fromIntegral dBAccountExternal,
            accountBackupInternal = fromIntegral dBAccountInternal,
            accountBackupLabels = Map.fromList labels,
            accountBackupFree = free,
            accountBackupCreated = dBAccountCreated
          }
  return $ WalletBackup backs

data SyncRes = SyncRes
  { syncResAccount :: !DBAccount,
    syncResBlockHash :: !BlockHash,
    syncResBlockHeight :: !BlockHeight,
    syncResTxUpdates :: !Natural,
    syncResCoinUpdates :: !Natural
  }
  deriving (Eq, Show)

restoreBackup ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  WalletBackup ->
  ExceptT String (DB m) [SyncRes]
restoreBackup ctx cfg (WalletBackup accs) =
  mapM (restoreAccount ctx cfg) accs

restoreAccount ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  AccountBackup ->
  ExceptT String (DB m) SyncRes
restoreAccount ctx cfg AccountBackup {..} = do
  -- Insert the account
  (accId, acc) <-
    insertAccount
      accountBackupNetwork
      ctx
      accountBackupWallet
      accountBackupName
      accountBackupPubKey
  let net = accountBackupNetwork
  -- Set the external and internal derivation indices
  (e, i) <- discoverAddrs net ctx cfg accountBackupPubKey
  let idxE = max e accountBackupExternal
      idxI = max i accountBackupInternal
  discoverAccGenAddrs ctx cfg accId AddrExternal $ fromIntegral idxE
  discoverAccGenAddrs ctx cfg accId AddrInternal $ fromIntegral idxI
  -- Perform an account sync
  syncRes <- sync ctx cfg net accId True
  -- Set address labels
  forM_ (Map.assocs accountBackupLabels) $ \(addr, l) -> do
    at <- liftMaybe "Address" $ addrToText net addr
    lift . update $ \a -> do
      set a [DBAddressLabel =. val l]
      where_ $ a ^. DBAddressAddress ==. val at
  -- Set Free internal addresses
  addrsT <- mapM (liftMaybe "Address" . addrToText net) accountBackupFree
  _ <- lift $ setAddrsFree AddrFree addrsT
  -- Set the creation time
  lift . update $ \a -> do
    set a [DBAccountCreated =. val accountBackupCreated]
    where_ $
      a ^. DBAccountWallet ==. val (dBAccountWallet acc)
        &&. a ^. DBAccountDerivation ==. val (dBAccountDerivation acc)
  acc' <- getAccountById accId
  return syncRes {syncResAccount = acc'}

discoverAddrs ::
  (MonadIO m) =>
  Network ->
  Ctx ->
  Config ->
  XPubKey ->
  ExceptT String m (Natural, Natural)
discoverAddrs net ctx cfg pub = do
  let recoveryGap = configRecoveryGap cfg
  e <- go extDeriv 0 (Page recoveryGap 0)
  i <- go intDeriv 0 (Page recoveryGap 0)
  return (fromIntegral e, fromIntegral i)
  where
    go path d page@(Page lim off) = do
      let addrs = addrsDerivPage ctx path page pub
          req = GetAddrsBalance $ fst <$> addrs
      let host = apiHost net cfg
      Store.SerialList bals <- liftExcept $ apiCall ctx host req
      let vBals = filter ((/= 0) . (.txs)) bals
      if null vBals
        then return d
        else do
          let dMax = findMax addrs $ (.address) <$> vBals
          go path (dMax + 1) (Page lim (off + lim))
    -- Find the largest ID amongst the addresses that have a positive balance
    findMax :: [(Address, SoftPath)] -> [Address] -> Int
    findMax addrs balAddrs =
      let fAddrs = filter ((`elem` balAddrs) . fst) addrs
       in fromIntegral $ maximum $ last . pathToList . snd <$> fAddrs

sync ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  Network ->
  DBAccountId ->
  Bool ->
  ExceptT String (DB m) SyncRes
sync ctx cfg net accId full = do
  let host = apiHost net cfg
  -- Check API health
  checkHealth ctx net cfg
  -- Get the new best block before starting the sync
  best <- liftExcept $ apiCall ctx host (GetBlockBest def)
  -- Get the addresses from our local database
  (addrPathMap, addrBalMap) <- allAddressesMap net accId
  -- Fetch the address balances online
  Store.SerialList storeBals <-
    liftExcept . apiBatch ctx (configAddrBatch cfg) host $
      GetAddrsBalance (Map.keys addrBalMap)
  -- Filter only those addresses whose balances have changed
  balsToUpdate <-
    if full
      then return storeBals
      else liftEither $ filterAddresses storeBals addrBalMap
  let addrsToUpdate = (.address) <$> balsToUpdate
  -- Update balances
  updateAddressBalances net balsToUpdate
  newAcc <- lift $ updateAccountBalances accId
  -- Get a list of our confirmed txs in the local database
  -- Use an empty list when doing a full sync
  confirmedTxs <- if full then return [] else getConfirmedTxs accId True
  -- Fetch the txids of the addresses to update
  aTids <- searchAddrTxs net ctx cfg confirmedTxs addrsToUpdate
  -- We also want to check if there is any change in unconfirmed txs
  uTids <- getConfirmedTxs accId False
  let tids = nub $ uTids <> aTids
  -- Fetch the full transactions
  Store.SerialList txs <-
    liftExcept $ apiBatch ctx (configTxFullBatch cfg) host (GetTxs tids)
  -- Convert them to TxInfo and store them in the local database
  let txInfos = storeToTxInfo addrPathMap (fromIntegral best.height) <$> txs
  resTxInfo <- forM txInfos $ repsertTxInfo net ctx accId
  -- Fetch and update coins
  Store.SerialList storeCoins <-
    liftExcept . apiBatch ctx (configCoinBatch cfg) host $
      GetAddrsUnspent addrsToUpdate def
  (coinCount, newCoins) <- refreshCoins net accId addrsToUpdate storeCoins
  -- Get the dependent tranactions of the new coins
  depTxsHash <-
    if full
      then return $ (.outpoint.hash) <$> storeCoins
      else mapM (liftEither . coinToTxHash) newCoins
  Store.RawResultList rawTxs <-
    liftExcept
      . apiBatch ctx (configTxFullBatch cfg) host
      $ GetTxsRaw
      $ nub depTxsHash
  lift $ forM_ rawTxs insertRawTx
  -- Remove pending transactions if they are online
  pendingTids <- pendingTxHashes accId
  let toRemove = filter ((`elem` tids) . fst) pendingTids
  forM_ toRemove $ \(_, key) -> lift $ deletePendingTxOnline key
  -- Update the best block for this network
  lift $ updateBest net (headerHash best.header) best.height
  return $
    SyncRes
      newAcc
      (headerHash best.header)
      (fromIntegral best.height)
      (fromIntegral $ length $ filter id $ snd <$> resTxInfo)
      (fromIntegral coinCount)

-- Filter addresses that need to be updated
filterAddresses ::
  [Store.Balance] ->
  Map Address AddressBalance ->
  Either String [Store.Balance]
filterAddresses sBals aMap
  | sort ((.address) <$> sBals) /= sort (Map.keys aMap) =
      Left "Sync: addresses do not match"
  | otherwise =
      Right $ filter f sBals
  where
    f s =
      let b = fromJust $ s.address `Map.lookup` aMap
       in s.txs /= addrBalanceTxs b
            || s.confirmed /= addrBalanceConfirmed b
            || s.unconfirmed /= addrBalanceUnconfirmed b
            || s.utxo /= addrBalanceCoins b

searchAddrTxs ::
  (MonadIO m) =>
  Network ->
  Ctx ->
  Config ->
  [TxHash] ->
  [Address] ->
  ExceptT String m [TxHash]
searchAddrTxs _ _ _ _ [] = return []
searchAddrTxs net ctx cfg confirmedTxs as
  | length as > fromIntegral (configAddrBatch cfg) =
      nub . concat <$> mapM (go Nothing 0) (chunksOf (configAddrBatch cfg) as)
  | otherwise =
      nub <$> go Nothing 0 as
  where
    go hashM offset' xs = do
      Store.SerialList txRefs <-
        liftExcept $
          apiCall
            ctx
            (apiHost net cfg)
            ( GetAddrsTxs
                xs
                def
                  { limit = Just $ fromIntegral (configTxBatch cfg),
                    start = StartParamHash <$> hashM,
                    offset = offset'
                  }
            )
      -- Remove txs that we already have
      let tids = ((.txid) <$> txRefs) \\ confirmedTxs
      -- Either we have reached the end of the stream, or we have hit some
      -- txs in confirmedTxs. In both cases, we can stop the search.
      if length tids < fromIntegral (configTxBatch cfg)
        then return tids
        else do
          let lastId = (last tids).get
          rest <- go (Just lastId) 1 xs
          return $ tids <> rest

coinToTxHash :: DBCoin -> Either String TxHash
coinToTxHash coin =
  maybeToEither "coinToTxHash: Invalid outpoint" $ do
    bs <- decodeHex $ dBCoinOutpoint coin
    op <- eitherToMaybe (S.decode bs) :: Maybe OutPoint
    return op.hash
