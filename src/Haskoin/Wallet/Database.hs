{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskoin.Wallet.Database where

import Conduit (MonadUnliftIO, ResourceT)
import Control.Arrow (Arrow (second))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Aeson as Json
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.List (find, nub, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word64)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Migration.SemVersion
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util (Page (Page), textToAddrE)
import Numeric.Natural (Natural)

{- SQL Table Definitions -}

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

DBVersion
  version Text
  Primary version
  deriving Show

DBWallet
  fingerprint Text
  Primary fingerprint
  deriving Show

DBAccount
  name Text
  wallet DBWalletId
  index Int
  network Text
  derivation Text
  external Int
  internal Int
  xPubKey Text
  balanceConfirmed Word64
  balanceUnconfirmed Word64
  balanceCoins Word64
  created UTCTime default=CURRENT_TIME
  Primary wallet derivation
  UniqueName name
  UniqueXPubKey xPubKey
  UniqueNetworkId wallet network index
  deriving Show
  deriving Eq

DBAddress
  index Int
  accountWallet DBWalletId
  accountDerivation Text
  derivation Text
  address Text
  label Text
  balanceConfirmed Word64
  balanceUnconfirmed Word64
  balanceCoins Word64
  balanceTxs Word64
  balanceReceived Word64
  internal Bool
  free Bool
  created UTCTime default=CURRENT_TIME
  Primary accountWallet accountDerivation derivation
  UniqueAddress address
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show
  deriving Eq

DBTxInfo
  accountWallet DBWalletId
  accountDerivation Text
  txid Text
  blockRef ByteString
  confirmed Bool
  blob ByteString
  created UTCTime default=CURRENT_TIME
  Primary accountWallet accountDerivation txid
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show
  deriving Eq

DBCoin
  accountWallet DBWalletId
  accountDerivation Text
  outpoint Text
  address Text
  value Word64
  blockRef ByteString
  blob ByteString
  confirmed Bool
  locked Bool
  created UTCTime default=CURRENT_TIME
  Primary outpoint
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show
  deriving Eq

DBRawTx
  hash Text
  blob ByteString
  Primary hash
  deriving Show
  deriving Eq

DBPendingTx
  accountWallet DBWalletId
  accountDerivation Text
  nosigHash Text
  blob ByteString
  online Bool
  created UTCTime default=CURRENT_TIME
  Primary nosigHash
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show
  deriving Eq

DBBest
  network Text
  bestBlock Text
  bestHeight Int
  Primary network
  deriving Show
|]

type DB m = ReaderT SqlBackend (NoLoggingT (ResourceT m))

runDB :: (MonadUnliftIO m) => Config -> DB m a -> m a
runDB cfg action = do
  dbFile <- liftIO $ databaseFile cfg
  runSqlite (cs dbFile) action

globalMigration :: (MonadUnliftIO m) => DB m ()
globalMigration = void $ runMigrationQuiet migrateAll

{- Meta -}

-- Database versions use major.minor only. Patch versions are ignored.
setVersion :: (MonadUnliftIO m) => SemVersion -> DB m ()
setVersion semVer = do
  verM <- selectOne $ from return
  let verTxt = cs $ verString $ toMinor semVer
  case verM of
    Nothing -> P.insert_ $ DBVersion verTxt
    Just (Entity k _) -> P.update k [DBVersionVersion P.=. verTxt]

getVersion :: (MonadUnliftIO m) => DB m SemVersion
getVersion = do
  resM <- selectOne . from $ \v -> return $ v ^. DBVersionVersion
  return $ parseSemVersion . cs . unValue $ fromJust resM

updateBest ::
  (MonadUnliftIO m) => Network -> BlockHash -> BlockHeight -> DB m ()
updateBest net hash height = do
  let netT = cs net.name
      key = DBBestKey netT
  P.repsert key $ DBBest netT (blockHashToHex hash) (fromIntegral height)

getBest ::
  (MonadUnliftIO m) => Network -> DB m (Maybe (BlockHash, BlockHeight))
getBest net = do
  let netT = cs net.name
  resM <- P.get $ DBBestKey netT
  return $ do
    DBBest _ a b <- resM
    hash <- hexToBlockHash a
    return (hash, fromIntegral b)

{- Accounts -}

instance ToJSON DBAccount where
  toJSON acc =
    object
      [ "name" .= dBAccountName acc,
        "wallet" .= dBAccountWallet acc,
        "index" .= dBAccountIndex acc,
        "network" .= dBAccountNetwork acc,
        "derivation" .= dBAccountDerivation acc,
        "external" .= dBAccountExternal acc,
        "internal" .= dBAccountInternal acc,
        "xPubKey" .= dBAccountXPubKey acc,
        "balance" .= toJSON (AccountBalance confirm unconfirm coins),
        "created" .= dBAccountCreated acc
      ]
    where
      confirm = dBAccountBalanceConfirmed acc
      unconfirm = dBAccountBalanceUnconfirmed acc
      coins = dBAccountBalanceCoins acc

instance FromJSON DBAccount where
  parseJSON =
    withObject "DBAccount" $ \o -> do
      bal <- o .: "balance"
      DBAccount
        <$> o .: "name"
        <*> o .: "wallet"
        <*> o .: "index"
        <*> o .: "network"
        <*> o .: "derivation"
        <*> o .: "external"
        <*> o .: "internal"
        <*> o .: "xPubKey"
        <*> (bal .: "confirmed")
        <*> (bal .: "unconfirmed")
        <*> (bal .: "coins")
        <*> o .: "created"

data AccountBalance = AccountBalance
  { -- | confirmed balance
    accBalanceConfirmed :: !Word64,
    -- | unconfirmed balance
    accBalanceUnconfirmed :: !Word64,
    -- | number of unspent outputs
    accBalanceCoins :: !Word64
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON AccountBalance where
  toJSON b =
    object
      [ "confirmed" .= accBalanceConfirmed b,
        "unconfirmed" .= accBalanceUnconfirmed b,
        "coins" .= accBalanceCoins b
      ]

instance FromJSON AccountBalance where
  parseJSON =
    withObject "accountbalance" $ \o ->
      AccountBalance
        <$> o .: "confirmed"
        <*> o .: "unconfirmed"
        <*> o .: "coins"

accountIndex :: DBAccount -> Natural
accountIndex = fromIntegral . dBAccountIndex

accountNetwork :: DBAccount -> Network
accountNetwork =
  fromMaybe (error "Invalid Network in database")
    . netByName
    . cs
    . dBAccountNetwork

accountWallet :: DBAccount -> Fingerprint
accountWallet acc =
  let (DBWalletKey walletFP) = dBAccountWallet acc
   in fromRight (error "Invalid WalletId in database") $
        textToFingerprint walletFP

accountXPubKey :: Ctx -> DBAccount -> XPubKey
accountXPubKey ctx acc =
  fromMaybe (error "Invalid XPubKey in database") $
    xPubImport (accountNetwork acc) ctx (dBAccountXPubKey acc)

nextAccountDeriv :: (MonadUnliftIO m) => Fingerprint -> Network -> DB m Natural
nextAccountDeriv walletFP net = do
  let walletId = DBWalletKey $ fingerprintToText walletFP
  idxs <-
    select . from $ \a -> do
      where_ $
        a ^. DBAccountNetwork ==. val (cs net.name)
          &&. a ^. DBAccountWallet ==. val walletId
      return $ a ^. DBAccountIndex
  return $ smallestUnused $ fromIntegral . unValue <$> idxs

smallestUnused :: [Natural] -> Natural
smallestUnused xs = fromJust $ find (not . (`elem` xs)) [0 ..]

existsAccount :: (MonadUnliftIO m) => Text -> DB m Bool
existsAccount name = P.existsBy $ UniqueName name

existsXPubKey :: (MonadUnliftIO m) => Network -> Ctx -> XPubKey -> DB m Bool
existsXPubKey net ctx key = P.existsBy $ UniqueXPubKey $ xPubExport net ctx key

getWalletOrCreate :: (MonadUnliftIO m) => Fingerprint -> DB m DBWalletId
getWalletOrCreate fp = do
  let key = DBWalletKey $ fingerprintToText fp
  walletM <- P.get key
  case walletM of
    Just _ -> return key
    _ -> P.insert $ DBWallet $ fingerprintToText fp

insertAccount ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Fingerprint ->
  Text ->
  XPubKey ->
  ExceptT String (DB m) (DBAccountId, DBAccount)
insertAccount net ctx walletFP name xpub = do
  existsName <- lift $ existsAccount name
  if existsName
    then throwError $ "Account " <> cs name <> " already exists"
    else do
      existsKey <- lift $ existsXPubKey net ctx xpub
      if existsKey
        then throwError "The XPubKey already exists"
        else do
          time <- liftIO getCurrentTime
          walletId <- lift $ getWalletOrCreate walletFP
          let path = bip44Deriv net $ fromIntegral $ xPubChild xpub
              idx = fromIntegral $ xPubIndex xpub
              account =
                DBAccount
                  { dBAccountName = name,
                    dBAccountWallet = walletId,
                    dBAccountIndex = idx,
                    dBAccountNetwork = cs net.name,
                    dBAccountDerivation = cs $ pathToStr path,
                    dBAccountExternal = 0,
                    dBAccountInternal = 0,
                    dBAccountXPubKey = xPubExport net ctx xpub,
                    dBAccountBalanceConfirmed = 0,
                    dBAccountBalanceUnconfirmed = 0,
                    dBAccountBalanceCoins = 0,
                    dBAccountCreated = time
                  }
          key <- lift $ P.insert account
          return (key, account)

deleteAccount :: (MonadUnliftIO m) => DBAccountId -> DB m ()
deleteAccount accId@(DBAccountKey accWallet accDeriv) = do
  delete . from $ \a ->
    where_ $
      a ^. DBAddressAccountWallet ==. val accWallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
  delete . from $ \t ->
    where_ $
      t ^. DBTxInfoAccountWallet ==. val accWallet
        &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
  delete . from $ \c ->
    where_ $
      c ^. DBCoinAccountWallet ==. val accWallet
        &&. c ^. DBCoinAccountDerivation ==. val accDeriv
  delete . from $ \p ->
    where_ $
      p ^. DBPendingTxAccountWallet ==. val accWallet
        &&. p ^. DBPendingTxAccountDerivation ==. val accDeriv
  P.delete accId

-- When a name is provided, get that account or throw an error if it doesn't
-- exist. When no name is provided, return the account only if there is one
-- account.
getAccountByName ::
  (MonadUnliftIO m) =>
  Maybe Text ->
  ExceptT String (DB m) (DBAccountId, DBAccount)
getAccountByName (Just name) = do
  aM <- lift $ P.getBy $ UniqueName name
  case aM of
    Just a -> return (entityKey a, entityVal a)
    _ -> throwError $ "The account " <> cs name <> " does not exist"
getAccountByName Nothing = do
  as <- lift getAccounts
  case as of
    [a] -> return a
    [] -> throwError "There are no accounts in the wallet"
    _ -> throwError "Specify which account to use"

getAccountById ::
  (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) DBAccount
getAccountById accId = liftMaybe "Invalid account" =<< lift (P.get accId)

getAccounts :: (MonadUnliftIO m) => DB m [(DBAccountId, DBAccount)]
getAccounts =
  (go <$>)
    <$> P.selectList
      []
      [P.Asc DBAccountWallet, P.Asc DBAccountNetwork, P.Asc DBAccountIndex]
  where
    go a = (entityKey a, entityVal a)

getAccountNames :: (MonadUnliftIO m) => DB m [Text]
getAccountNames = do
  res <- select . from $ \a -> do
    orderBy [asc $ a ^. DBAccountCreated]
    return $ a ^. DBAccountName
  return $ unValue <$> res

renameAccount ::
  (MonadUnliftIO m) => Text -> Text -> ExceptT String (DB m) DBAccount
renameAccount oldName newName
  | oldName == newName = throwError "Old and new names are the same"
  | otherwise = do
      e <- lift $ existsAccount newName
      if e
        then throwError $ "The account " <> cs newName <> " already exists"
        else do
          c <-
            lift . updateCount $ \a -> do
              set a [DBAccountName =. val newName]
              where_ $ a ^. DBAccountName ==. val oldName
          if c == 0
            then throwError $ "The account " <> cs oldName <> " does not exist"
            else snd <$> getAccountByName (Just newName)

updateAccountBalances :: (MonadUnliftIO m) => DBAccountId -> DB m DBAccount
updateAccountBalances accId@(DBAccountKey wallet accDeriv) = do
  confirm <- selectSum DBAddressBalanceConfirmed
  unconfirm <- selectSum DBAddressBalanceUnconfirmed
  coins <- selectSum DBAddressBalanceCoins
  update $ \a -> do
    set
      a
      [ DBAccountBalanceConfirmed =. val (unpack confirm),
        DBAccountBalanceUnconfirmed =. val (unpack unconfirm),
        DBAccountBalanceCoins =. val (unpack coins)
      ]
    where_ $ a ^. DBAccountId ==. val accId
  fromJust <$> P.get accId
  where
    unpack m = fromMaybe 0 $ unValue $ fromMaybe (Value $ Just 0) m
    selectSum field =
      selectOne . from $ \a -> do
        where_ $
          a ^. DBAddressAccountWallet ==. val wallet
            &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        return $ sum_ $ a ^. field

{- Addresses -}

data AddrType = AddrInternal | AddrExternal
  deriving (Eq, Show)

isInternal :: AddrType -> Bool
isInternal AddrInternal = True
isInternal AddrExternal = False

dBAccountCount :: AddrType -> DBAccount -> Int
dBAccountCount AddrInternal = dBAccountInternal
dBAccountCount AddrExternal = dBAccountExternal

dBAccountField :: AddrType -> EntityField DBAccount Int
dBAccountField AddrInternal = DBAccountInternal
dBAccountField AddrExternal = DBAccountExternal

addrDeriv :: AddrType -> SoftPath
addrDeriv AddrInternal = intDeriv
addrDeriv AddrExternal = extDeriv

data AddrFree = AddrFree | AddrBusy
  deriving (Eq, Show)

isAddrFree :: AddrFree -> Bool
isAddrFree AddrFree = True
isAddrFree AddrBusy = False

instance ToJSON DBAddress where
  toJSON addr =
    object
      [ "index" .= dBAddressIndex addr,
        "wallet" .= dBAddressAccountWallet addr,
        "account" .= dBAddressAccountDerivation addr,
        "derivation" .= dBAddressDerivation addr,
        "address" .= dBAddressAddress addr,
        "label" .= dBAddressLabel addr,
        "balance"
          .= AddressBalance
            (dBAddressBalanceConfirmed addr)
            (dBAddressBalanceUnconfirmed addr)
            (dBAddressBalanceCoins addr)
            (dBAddressBalanceTxs addr)
            (dBAddressBalanceReceived addr),
        "internal" .= dBAddressInternal addr,
        "free" .= dBAddressFree addr,
        "created" .= dBAddressCreated addr
      ]

instance FromJSON DBAddress where
  parseJSON =
    withObject "DBAddress" $ \o -> do
      bal <- o .: "balance"
      DBAddress
        <$> o .: "index"
        <*> o .: "wallet"
        <*> o .: "account"
        <*> o .: "derivation"
        <*> o .: "address"
        <*> o .: "label"
        <*> bal .: "confirmed"
        <*> bal .: "unconfirmed"
        <*> bal .: "coins"
        <*> bal .: "txs"
        <*> bal .: "received"
        <*> o .: "internal"
        <*> o .: "free"
        <*> o .: "created"

data AddressBalance = AddressBalance
  { -- | confirmed balance
    addrBalanceConfirmed :: !Word64,
    -- | unconfirmed balance
    addrBalanceUnconfirmed :: !Word64,
    -- | number of unspent outputs
    addrBalanceCoins :: !Word64,
    -- | number of transactions
    addrBalanceTxs :: !Word64,
    -- | total amount from all outputs in this address
    addrBalanceReceived :: !Word64
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON AddressBalance where
  toJSON b =
    object
      [ "confirmed" .= addrBalanceConfirmed b,
        "unconfirmed" .= addrBalanceUnconfirmed b,
        "coins" .= addrBalanceCoins b,
        "txs" .= addrBalanceTxs b,
        "received" .= addrBalanceReceived b
      ]

instance FromJSON AddressBalance where
  parseJSON =
    withObject "accountbalance" $ \o ->
      AddressBalance
        <$> o .: "confirmed"
        <*> o .: "unconfirmed"
        <*> o .: "coins"
        <*> o .: "txs"
        <*> o .: "received"

updateAddressBalances ::
  (MonadUnliftIO m) =>
  Network ->
  [Store.Balance] ->
  ExceptT String (DB m) ()
updateAddressBalances net storeBals =
  forM_ storeBals $ \s -> do
    addrT <-
      liftEither $ maybeToEither "Invalid Address" (addrToText net s.address)
    lift . update $ \a -> do
      set
        a
        [ DBAddressBalanceConfirmed =. val s.confirmed,
          DBAddressBalanceUnconfirmed =. val s.unconfirmed,
          DBAddressBalanceCoins =. val s.utxo,
          DBAddressBalanceTxs =. val s.txs,
          DBAddressBalanceReceived =. val s.received
        ]
      where_ $ a ^. DBAddressAddress ==. val addrT

insertAddress ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  SoftPath ->
  Address ->
  AddrFree ->
  ExceptT String (DB m) DBAddress
insertAddress net (DBAccountKey wallet accDeriv) deriv addr free = do
  time <- liftIO getCurrentTime
  addrT <- liftEither $ maybeToEither "Invalid Address" (addrToText net addr)
  let label = if isIntPath deriv then "Internal Address" else ""
      derivS = cs $ pathToStr deriv
      dbAddr =
        DBAddress
          (fromIntegral $ pathIndex deriv)
          wallet
          accDeriv
          derivS
          addrT
          label
          0
          0
          0
          0
          0
          (isIntPath deriv)
          (isAddrFree free)
          time
  lift $ P.insert_ dbAddr
  return dbAddr

-- This is an internal function
genNextAddress ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  DBAccountId ->
  AddrType ->
  AddrFree ->
  Bool ->
  ExceptT String (DB m) DBAddress
genNextAddress ctx cfg accId addrType addrFree testGap = do
  acc <- getAccountById accId
  let net = accountNetwork acc
      pub = accountXPubKey ctx acc
      nextIdx = dBAccountCount addrType acc
      deriv = addrDeriv addrType
  when testGap $ checkGap cfg accId nextIdx addrType
  let (addr, _) = derivePathAddr ctx pub deriv (fromIntegral nextIdx)
  dbAddr <-
    insertAddress net accId (deriv :/ fromIntegral nextIdx) addr addrFree
  lift $ P.update accId [dBAccountField addrType P.=. nextIdx + 1]
  return dbAddr

checkGap ::
  (MonadUnliftIO m) =>
  Config ->
  DBAccountId ->
  Int ->
  AddrType ->
  ExceptT String (DB m) ()
checkGap cfg accId addrIdx addrType = do
  let gap = configGap cfg
  usedIdxM <- lift $ bestAddrWithFunds accId addrType
  let usedIdx = maybe 0 (+ 1) usedIdxM
  when (addrIdx >= usedIdx + fromIntegral gap) $
    throwError $
      "Can not generate addresses beyond the gap of " <> show gap

-- Highest address with a positive transaction count
bestAddrWithFunds ::
  (MonadUnliftIO m) => DBAccountId -> AddrType -> DB m (Maybe Int)
bestAddrWithFunds (DBAccountKey wallet accDeriv) addrType = do
  resM <- (flatMaybe <$>) . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        &&. a ^. DBAddressInternal ==. val (isInternal addrType)
        &&. a ^. DBAddressBalanceTxs >. val 0
    return $ max_ $ a ^. DBAddressIndex
  return $ fromIntegral <$> resM

-- Generate the discovered external and internal addresses
discoverAccGenAddrs ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  DBAccountId ->
  AddrType ->
  Int ->
  ExceptT String (DB m) ()
discoverAccGenAddrs ctx cfg accId addrType newAddrCnt = do
  acc <- getAccountById accId
  let oldAddrCnt = dBAccountCount addrType acc
      cnt = max 0 $ newAddrCnt - oldAddrCnt
  -- False: Don't check the gap while discovering
  replicateM_ cnt $ genNextAddress ctx cfg accId addrType AddrBusy False

genExtAddress ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  DBAccountId ->
  Text ->
  ExceptT String (DB m) DBAddress
genExtAddress ctx cfg accId label = do
  -- True: check the gap
  addr <- genNextAddress ctx cfg accId AddrExternal AddrBusy True
  setAddrLabel accId (dBAddressIndex addr) label

setAddrLabel ::
  (MonadUnliftIO m) =>
  DBAccountId ->
  Int ->
  Text ->
  ExceptT String (DB m) DBAddress
setAddrLabel accId@(DBAccountKey wallet accDeriv) idx label = do
  acc <- getAccountById accId
  let path = cs $ pathToStr $ extDeriv :/ fromIntegral idx
      aKey = DBAddressKey wallet accDeriv path
  unless (fromIntegral idx < dBAccountExternal acc) $
    throwError $
      "Address " <> show idx <> " does not exist"
  lift $ P.updateGet aKey [DBAddressLabel P.=. label]

nextFreeIntAddr ::
  (MonadUnliftIO m) =>
  Ctx ->
  Config ->
  DBAccountId ->
  ExceptT String (DB m) DBAddress
nextFreeIntAddr ctx cfg accId@(DBAccountKey wallet accDeriv) = do
  resM <- lift . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        &&. a ^. DBAddressInternal ==. val True
        &&. a ^. DBAddressFree ==. val True
    orderBy [asc $ a ^. DBAddressIndex]
    limit 1
    return a
  case resM of
    Just (Entity _ a) -> return a
    -- True: Check the gap
    Nothing -> genNextAddress ctx cfg accId AddrInternal AddrFree True

fromDBAddr :: Network -> DBAddress -> Either String (Address, SoftPath)
fromDBAddr net addrDB = do
  let addrT = dBAddressAddress addrDB
      derivT = dBAddressDerivation addrDB
  deriv <- maybeToEither "fromDBAddress deriv" $ parseSoft $ cs derivT
  addr <- maybeToEither "fromDBAddress addr" $ textToAddr net addrT
  return (addr, deriv)

setAddrsFree :: (MonadUnliftIO m) => AddrFree -> [Text] -> DB m Natural
setAddrsFree free addrs = do
  (fromIntegral <$>) . updateCount $ \a -> do
    set a [DBAddressFree =. val (isAddrFree free)]
    where_ $
      a ^. DBAddressAddress `in_` valList addrs
        &&. (a ^. DBAddressBalanceTxs ==. val 0)

addressPage :: (MonadUnliftIO m) => DBAccountId -> Page -> DB m [DBAddress]
addressPage (DBAccountKey wallet accDeriv) (Page lim off) = do
  as <-
    select $
      from $ \a -> do
        where_ $
          a ^. DBAddressAccountWallet ==. val wallet
            &&. a ^. DBAddressAccountDerivation ==. val accDeriv
            &&. a ^. DBAddressInternal ==. val False
        orderBy [desc (a ^. DBAddressIndex)]
        limit $ fromIntegral lim
        offset $ fromIntegral off
        return a
  return $ entityVal <$> as

allAddressesMap ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  ExceptT String (DB m) (Map Address SoftPath, Map Address AddressBalance)
allAddressesMap net (DBAccountKey wallet accDeriv) = do
  dbRes <-
    lift . select $
      from $ \a -> do
        where_ $
          a ^. DBAddressAccountWallet ==. val wallet
            &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        return a
  res <-
    forM dbRes $ \(Entity _ dbAddr) -> do
      a <- liftEither $ textToAddrE net $ dBAddressAddress dbAddr
      d <-
        liftEither . maybeToEither "parsePath failed" $
          parseSoft . cs $
            dBAddressDerivation dbAddr
      let b =
            AddressBalance
              (dBAddressBalanceConfirmed dbAddr)
              (dBAddressBalanceUnconfirmed dbAddr)
              (dBAddressBalanceCoins dbAddr)
              (dBAddressBalanceTxs dbAddr)
              (dBAddressBalanceReceived dbAddr)
      return ((a, d), (a, b))
  return (Map.fromList $ fst <$> res, Map.fromList $ snd <$> res)

getCoinDeriv ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  Store.Unspent ->
  DB m (Either String SoftPath)
getCoinDeriv net accId unspent =
  runExceptT $ do
    addr <-
      liftEither . maybeToEither "getCoinDeriv: no address" $ unspent.address
    liftEither <=< lift $ getAddrDeriv net accId addr

getAddrDeriv ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  Address ->
  DB m (Either String SoftPath)
getAddrDeriv net (DBAccountKey wallet accDeriv) addr =
  runExceptT $ do
    addrT <-
      liftEither . maybeToEither "getAddrDeriv: no address" $ addrToText net addr
    derivM <-
      lift . selectOne . from $ \a -> do
        where_ $
          a ^. DBAddressAddress ==. val addrT
            &&. a ^. DBAddressAccountWallet ==. val wallet
            &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        return $ a ^. DBAddressDerivation
    liftEither . maybeToEither "getAddrDeriv: no derivation" $
      parseSoft . cs . unValue =<< derivM

{- Transactions -}

getConfirmedTxs ::
  (MonadUnliftIO m) => DBAccountId -> Bool -> ExceptT String (DB m) [TxHash]
getConfirmedTxs (DBAccountKey wallet accDeriv) confirm = do
  ts <-
    lift . select $
      from $ \t -> do
        where_ $
          t ^. DBTxInfoAccountWallet ==. val wallet
            &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
            &&. t ^. DBTxInfoConfirmed ==. val confirm
        orderBy [asc (t ^. DBTxInfoBlockRef)]
        return $ t ^. DBTxInfoTxid
  forM ts $ \(Value t) ->
    liftEither $ maybeToEither "getConfirmedTxs invalid TxHash" $ hexToTxHash t

-- Insert a new transaction or replace it, if it already exists
-- Returns True if there was a change or an insert
repsertTxInfo ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  TxInfo ->
  ExceptT String (DB m) (DBTxInfo, Bool)
repsertTxInfo net ctx accId txInfo = do
  time <- liftIO getCurrentTime
  tid <- liftEither $ maybeToEither "TxId" $ txInfoHash txInfo
  let confirmed' = Store.confirmed $ txInfoBlockRef txInfo
      tidT = txHashToHex tid
      bRef = S.encode $ txInfoBlockRef txInfo
      -- Confirmations will get updated when retrieving them
      blob =
        BS.toStrict $
          marshalJSON
            (net, ctx)
            txInfo
              { txInfoConfirmations = 0,
                txInfoPending = Nothing
              }
      (DBAccountKey wallet accDeriv) = accId
      key = DBTxInfoKey wallet accDeriv tidT
      dbInfo =
        DBTxInfo
          { dBTxInfoAccountWallet = wallet,
            dBTxInfoAccountDerivation = accDeriv,
            dBTxInfoTxid = tidT,
            dBTxInfoBlockRef = bRef,
            dBTxInfoConfirmed = confirmed',
            dBTxInfoBlob = blob,
            dBTxInfoCreated = time
          }
  prevM <- lift $ P.get key
  case prevM of
    Just prev -> do
      let newDBInfo =
            prev
              { dBTxInfoBlockRef = bRef,
                dBTxInfoConfirmed = confirmed',
                dBTxInfoBlob = blob
              }
      lift $ P.replace key newDBInfo
      return (newDBInfo, prev /= newDBInfo)
    Nothing -> do
      lift $ P.insert_ dbInfo
      return (dbInfo, True)

txsPage ::
  (MonadUnliftIO m) =>
  Ctx ->
  DBAccountId ->
  Page ->
  ExceptT String (DB m) [TxInfo]
txsPage ctx accId@(DBAccountKey wallet accDeriv) (Page lim off) = do
  acc <- getAccountById accId
  let net = accountNetwork acc
  dbTxs <-
    lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccountWallet ==. val wallet
          &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
      orderBy [asc (t ^. DBTxInfoBlockRef)]
      limit $ fromIntegral lim
      offset $ fromIntegral off
      return $ t ^. DBTxInfoBlob
  res <-
    forM dbTxs $ \(Value dbTx) -> do
      liftMaybe "TxInfo unmarshalJSON Failed" $
        unmarshalJSON (net, ctx) $
          BS.fromStrict dbTx
  resLabels <- lift $ mapM (fillTxInfoLabels net) res
  bestM <- lift $ getBest net
  return $ updateConfirmations (snd <$> bestM) <$> resLabels
  where
    updateConfirmations bestM tif =
      tif {txInfoConfirmations = getConfirmations bestM (txInfoBlockRef tif)}

getConfirmations :: Maybe BlockHeight -> Store.BlockRef -> Natural
getConfirmations _ (Store.MemRef _) = 0
getConfirmations Nothing (Store.BlockRef _ _) = 1
getConfirmations (Just best) (Store.BlockRef height _)
  | best < height = 1
  | otherwise = fromIntegral $ best - height + 1

fillTxInfoLabels :: (MonadUnliftIO m) => Network -> TxInfo -> DB m TxInfo
fillTxInfoLabels net txInfo = do
  o <- mapM fillOutput $ Map.assocs $ txInfoMyOutputs txInfo
  i <- mapM fillInput $ Map.assocs $ txInfoMyInputs txInfo
  return $
    txInfo
      { txInfoMyOutputs = Map.fromList o,
        txInfoMyInputs = Map.fromList i
      }
  where
    fillOutput (a, o) = do
      resM <- runMaybeT $ do
        addrT <- hoistMaybe $ addrToText net a
        (Entity _ dbAddr) <- hoistMaybe =<< lift (P.getBy $ UniqueAddress addrT)
        return (a, o {myOutputsLabel = dBAddressLabel dbAddr})
      return $ fromMaybe (a, o) resM
    fillInput (a, i) = do
      resM <- runMaybeT $ do
        addrT <- hoistMaybe $ addrToText net a
        (Entity _ dbAddr) <- hoistMaybe =<< lift (P.getBy $ UniqueAddress addrT)
        return (a, i {myInputsLabel = dBAddressLabel dbAddr})
      return $ fromMaybe (a, i) resM

{- Coins -}

data JsonCoin = JsonCoin
  { jsonCoinOutpoint :: !OutPoint,
    jsonCoinAddress :: !Address,
    jsonCoinValue :: !Word64,
    jsonCoinBlock :: !Store.BlockRef,
    jsonCoinConfirmations :: !Natural,
    jsonCoinLocked :: !Bool
  }
  deriving (Eq, Show)

instance MarshalJSON Network JsonCoin where
  marshalValue net c =
    object
      [ "outpoint" .= jsonCoinOutpoint c,
        "address" .= marshalValue net (jsonCoinAddress c),
        "value" .= jsonCoinValue c,
        "block" .= jsonCoinBlock c,
        "confirmations" .= jsonCoinConfirmations c,
        "locked" .= jsonCoinLocked c
      ]
  unmarshalValue net =
    withObject "JsonCoin" $ \o ->
      JsonCoin
        <$> o .: "outpoint"
        <*> (unmarshalValue net =<< o .: "address")
        <*> o .: "value"
        <*> o .: "block"
        <*> o .: "confirmations"
        <*> o .: "locked"

toJsonCoin :: Network -> Maybe BlockHeight -> DBCoin -> Either String JsonCoin
toJsonCoin net bestM dbCoin = do
  op <- textToOutpoint $ dBCoinOutpoint dbCoin
  ad <-
    maybeToEither "toJsonCoin: Invalid address" $
      textToAddr net $
        dBCoinAddress dbCoin
  br <- S.decode $ dBCoinBlockRef dbCoin
  let confirmations = getConfirmations bestM br
  return $
    JsonCoin
      { jsonCoinOutpoint = op,
        jsonCoinAddress = ad,
        jsonCoinValue = dBCoinValue dbCoin,
        jsonCoinBlock = br,
        jsonCoinConfirmations = confirmations,
        jsonCoinLocked = dBCoinLocked dbCoin
      }

outpointText :: OutPoint -> Text
outpointText = encodeHex . S.encode

textToOutpoint :: Text -> Either String OutPoint
textToOutpoint t = do
  bs <- maybeToEither "textToOutpoint: invalid input" $ decodeHex t
  S.decode bs

-- Get all coins in an account, spendable or not
coinPage ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  Page ->
  ExceptT String (DB m) [JsonCoin]
coinPage net (DBAccountKey wallet accDeriv) (Page lim off) = do
  coins <-
    lift . select . from $ \c -> do
      where_ $ do
        c ^. DBCoinAccountWallet ==. val wallet
          &&. c ^. DBCoinAccountDerivation ==. val accDeriv
      orderBy [asc (c ^. DBCoinBlockRef), desc (c ^. DBCoinCreated)]
      limit $ fromIntegral lim
      offset $ fromIntegral off
      return c
  bestM <- lift $ getBest net
  mapM (liftEither . toJsonCoin net (snd <$> bestM) . entityVal) coins

-- Spendable coins must be confirmed and not locked
getSpendableCoins ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  Natural ->
  ExceptT String (DB m) [Store.Unspent]
getSpendableCoins net (DBAccountKey wallet accDeriv) minConf = do
  bestM <- lift $ getBest net
  coins <- lift . select . from $ \c -> do
    where_ $
      c ^. DBCoinAccountWallet ==. val wallet
        &&. c ^. DBCoinAccountDerivation ==. val accDeriv
        &&. c ^. DBCoinLocked ==. val False
    return c
  let f c = do
        ref <- liftEither $ S.decode $ dBCoinBlockRef c
        return $ getConfirmations (snd <$> bestM) ref >= minConf
  spendableCoins <- filterM (f . entityVal) coins
  let bss = dBCoinBlob . entityVal <$> spendableCoins
  mapM (liftEither . S.decode) bss

insertCoin ::
  (MonadUnliftIO m) =>
  DBAccountId ->
  Text ->
  Store.Unspent ->
  DB m DBCoin
insertCoin (DBAccountKey wallet accDeriv) addr unspent = do
  time <- liftIO getCurrentTime
  let newCoin =
        DBCoin
          { dBCoinAccountWallet = wallet,
            dBCoinAccountDerivation = accDeriv,
            dBCoinOutpoint = outpointText unspent.outpoint,
            dBCoinAddress = addr,
            dBCoinValue = unspent.value,
            dBCoinBlockRef = S.encode unspent.block,
            dBCoinBlob = S.encode unspent,
            dBCoinConfirmed = Store.confirmed unspent.block,
            dBCoinLocked = False,
            dBCoinCreated = time
          }
  P.insert_ newCoin
  return newCoin

updateCoin :: (MonadUnliftIO m) => Store.Unspent -> DB m ()
updateCoin unspent = do
  let key = DBCoinKey $ outpointText unspent.outpoint
  P.update
    key
    [ DBCoinBlob P.=. S.encode unspent,
      DBCoinConfirmed P.=. Store.confirmed unspent.block,
      DBCoinBlockRef P.=. S.encode unspent.block
    ]

deleteCoin :: (MonadUnliftIO m) => DBCoin -> DB m ()
deleteCoin coin =
  delete . from $ \c ->
    where_ $
      c ^. DBCoinOutpoint ==. val (dBCoinOutpoint coin)

getCoinsByAddr :: (MonadUnliftIO m) => Text -> DB m [DBCoin]
getCoinsByAddr addr = do
  coins <- select . from $ \c -> do
    where_ $ c ^. DBCoinAddress ==. val addr
    return c
  return $ entityVal <$> coins

-- This is the main coin function
-- Either insert, update or delete coins as required. Returns the number of
-- coins that have either been inserted, updated or deleted.
refreshCoins ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  [Address] ->
  [Store.Unspent] ->
  ExceptT String (DB m) (Int, [DBCoin])
refreshCoins net accId addrsToUpdate allUnspent = do
  let storeMap = groupCoins net allUnspent
  addrsToUpdateE <-
    mapM (liftEither . maybeToEither "Addr" . addrToText net) addrsToUpdate
  res <- forM addrsToUpdateE $ \addr -> do
    let storeCoins = fromMaybe [] $ Map.lookup addr storeMap
    localCoins <- lift $ getCoinsByAddr addr
    let storeOps = outpointText . (.outpoint) <$> storeCoins
        localOps = dBCoinOutpoint <$> localCoins
        toDelete = filter ((`notElem` storeOps) . dBCoinOutpoint) localCoins
        toInsert =
          filter ((`notElem` localOps) . outpointText . (.outpoint)) storeCoins
        toUpdate = filter (f localCoins) storeCoins
    lift $ forM_ toDelete deleteCoin
    lift $ forM_ toUpdate updateCoin
    newCoins <- lift $ forM toInsert $ insertCoin accId addr
    return (length toDelete + length toUpdate + length toInsert, newCoins)
  return (sum $ fst <$> res, concatMap snd res)
  where
    f localCoins s =
      let cM = find ((== outpointText s.outpoint) . dBCoinOutpoint) localCoins
       in case cM of
            Just c -> dBCoinBlockRef c /= S.encode s.block
            _ -> False

groupCoins :: Network -> [Store.Unspent] -> Map Text [Store.Unspent]
groupCoins net =
  Map.fromListWith (<>) . mapMaybe f
  where
    f x =
      case x.address of
        Just a -> (,[x]) <$> addrToText net a
        _ -> Nothing

setLockCoin :: (MonadUnliftIO m) => OutPoint -> Bool -> DB m Natural
setLockCoin op locked = do
  cnt <- updateCount $ \c -> do
    set c [DBCoinLocked =. val locked]
    where_ $
      c ^. DBCoinOutpoint ==. val (outpointText op)
        &&. c ^. DBCoinLocked ==. val (not locked)
  return $ fromIntegral cnt

{- Raw Transactions -}

insertRawTx :: (MonadUnliftIO m) => Tx -> DB m ()
insertRawTx tx = do
  let hash = txHashToHex $ txHash tx
      key = DBRawTxKey hash
  P.repsert key $ DBRawTx hash (S.encode tx)

getRawTx :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) Tx
getRawTx hash = do
  let key = DBRawTxKey $ txHashToHex hash
  txM <- lift $ P.get key
  case txM of
    Just tx -> liftEither . S.decode $ dBRawTxBlob tx
    Nothing -> throwError "getRawTx: missing transaction"

{- Pending Transactions -}

data TxOnline = TxOnline | TxOffline
  deriving (Eq, Show)

-- Returns (TxSignData, isOnline)
getPendingTx ::
  (MonadUnliftIO m) =>
  TxHash ->
  DB m (Maybe (DBAccountId, TxSignData, TxInfoPending))
getPendingTx nosigHash = do
  let hashT = txHashToHex nosigHash
      key = DBPendingTxKey hashT
  resM <- P.get key
  case resM of
    Just (DBPendingTx wallet accDeriv _ blob online _) -> do
      let accId = DBAccountKey wallet accDeriv
          tsdM = Json.decode $ BS.fromStrict blob
      return $ do
        tsd <- tsdM
        let pending =
              TxInfoPending
                (nosigTxHash $ txSignDataTx tsd)
                (txSignDataSigned tsd)
                online
        return (accId, tsd, pending)
    _ -> return Nothing

pendingTxPage ::
  (MonadUnliftIO m) =>
  Ctx ->
  DBAccountId ->
  Page ->
  ExceptT String (DB m) [TxInfo]
pendingTxPage ctx accId@(DBAccountKey wallet accDeriv) (Page lim off) = do
  acc <- getAccountById accId
  let pub = accountXPubKey ctx acc
      net = accountNetwork acc
  tsds <-
    lift . select . from $ \p -> do
      where_ $ do
        p ^. DBPendingTxAccountWallet ==. val wallet
          &&. p ^. DBPendingTxAccountDerivation ==. val accDeriv
      orderBy [desc (p ^. DBPendingTxCreated)]
      limit $ fromIntegral lim
      offset $ fromIntegral off
      return p
  forM tsds $ \(Entity _ res) -> do
    tsd <- liftEither . Json.eitherDecode . BS.fromStrict $ dBPendingTxBlob res
    txInfo <- liftEither $ parseTxSignData net ctx pub tsd
    let nosigHash = nosigTxHash $ txSignDataTx tsd
        pending =
          TxInfoPending nosigHash (txSignDataSigned tsd) (dBPendingTxOnline res)
    let txInfoP = txInfo {txInfoPending = Just pending}
    lift $ fillTxInfoLabels net txInfoP

-- Returns the TxHash and NoSigHash of pending transactions. They are compared
-- during a sync in order to delete pending transactions that are now online.
pendingTxHashes ::
  (MonadUnliftIO m) =>
  DBAccountId ->
  ExceptT String (DB m) [(TxHash, DBPendingTxId)]
pendingTxHashes (DBAccountKey wallet accDeriv) = do
  blobs <-
    lift . select . from $ \p -> do
      where_ $ do
        p ^. DBPendingTxAccountWallet ==. val wallet
          &&. p ^. DBPendingTxAccountDerivation ==. val accDeriv
      return (p ^. DBPendingTxBlob, p ^. DBPendingTxId)
  res <- forM blobs $ \(Value blob, Value key) -> do
    tsd <- liftEither . Json.eitherDecode $ BS.fromStrict blob
    if txSignDataSigned tsd
      then return [(txHash $ txSignDataTx tsd, key)]
      else return []
  return $ concat res

-- Imports a pending transaction, locks coins and locks internal addresses
importPendingTx ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  TxSignData ->
  ExceptT String (DB m) TxHash
importPendingTx net ctx accId tsd@(TxSignData tx _ _ _ signed) = do
  acc <- getAccountById accId
  let pub = accountXPubKey ctx acc
      nosigHash = nosigTxHash $ txSignDataTx tsd
      bs = BS.toStrict $ Json.encode tsd
  prevM <- lift $ getPendingTx nosigHash
  case prevM of
    Just (_, TxSignData prevTx _ _ _ prevSigned, pending) -> do
      when (pendingOnline pending) $
        throwError "The transaction is already online"
      when (prevSigned && not signed) $
        throwError "Can not replace a signed transaction with an unsigned one"
      when (prevTx == tx) $
        throwError "The transaction already exists"
      when (not prevSigned && signed) $ do
        let key = DBPendingTxKey $ txHashToHex nosigHash
        lift $ P.update key [DBPendingTxBlob P.=. bs]
      return nosigHash
    Nothing -> do
      txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
      let (outpoints, outIntAddrs, restAddrs) = parseTxInfoU txInfoU
      -- Verify coins and lock them
      forM_ outpoints $ \outpoint -> do
        coinM <- lift $ P.get $ DBCoinKey $ outpointText outpoint
        case coinM of
          Just coin -> do
            when (dBCoinLocked coin) $
              throwError "A coin referenced by the transaction is locked"
            lift $ setLockCoin outpoint True
          _ -> throwError "A coin referenced by the transaction does not exist"
      -- Verify addresses and set internal output addresses to busy
      outIntAddrsT <- mapM (liftMaybe "Addrs" . addrToText net . fst) outIntAddrs
      restAddrsT <- mapM (liftMaybe "Addrs" . addrToText net) restAddrs
      outIntAddrsE <- lift . select . from $ \a -> do
        where_ $ a ^. DBAddressAddress `in_` valList outIntAddrsT
        return a
      restAddrsE <- lift . select . from $ \a -> do
        where_ $ a ^. DBAddressAddress `in_` valList restAddrsT
        return a
      when
        ( length (outIntAddrsT <> restAddrsT)
            /= length (outIntAddrsE <> restAddrsE)
        )
        $ throwError "Some referenced addresses do not exist"
      unless (all (dBAddressFree . entityVal) outIntAddrsE) $
        throwError "Some of the internal output addresses are not free"
      -- Set the output internal addresses to not free
      _ <- lift $ setAddrsFree AddrBusy outIntAddrsT
      -- Insert the pending transaction
      time <- liftIO getCurrentTime
      let ptx =
            DBPendingTx
              (dBAccountWallet acc)
              (dBAccountDerivation acc)
              (txHashToHex nosigHash)
              bs
              False
              time
      lift $ P.insert_ ptx
      return nosigHash

-- (Outpoints, external addrs, internal addrs)
parseTxInfoU :: TxInfo -> ([OutPoint], [(Address, KeyIndex)], [Address])
parseTxInfoU TxInfo {..} =
  (outpoints, nub $ f <$> outIntAddrs, nub $ fst <$> restAddrs)
  where
    outpoints = (.outpoint) <$> concatMap myInputsSigInput (Map.elems txInfoMyInputs)
    outAddrs = second myOutputsPath <$> Map.assocs txInfoMyOutputs
    inAddrs = second myInputsPath <$> Map.assocs txInfoMyInputs
    (outIntAddrs, outExtAddrs) = partition (isIntPath . snd) outAddrs
    restAddrs = outExtAddrs <> inAddrs
    f (a, p) =
      case pathToList p of
        (_ : i : _) -> (a, i)
        _ -> error "parseTxInfoU"

-- Delete a pending transaction, unlocks coins and frees internal addresses
deletePendingTx ::
  (MonadUnliftIO m) =>
  Ctx ->
  TxHash ->
  ExceptT String (DB m) (Natural, Natural)
deletePendingTx ctx nosigHash = do
  let key = DBPendingTxKey $ txHashToHex nosigHash
  tsdM <- lift $ getPendingTx nosigHash
  case tsdM of
    Just (_, _, TxInfoPending _ _ True) -> do
      throwError
        "This pending transaction has been sent to the network.\
        \ Run syncacc to refresh your database."
    -- We only free coins and addresses if the transaction is offline
    Just (accId, tsd, _) -> do
      acc <- getAccountById accId
      let net = accountNetwork acc
          pub = accountXPubKey ctx acc
      txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
      let (outpoints, outIntAddrs, _) = parseTxInfoU txInfoU
      outIntAddrsT <-
        mapM (liftMaybe "Address" . addrToText net . fst) outIntAddrs
      freedCoins <- forM outpoints $ \op -> lift $ setLockCoin op False
      freedAddresses <- lift $ setAddrsFree AddrFree outIntAddrsT
      lift $ P.delete key
      return (sum freedCoins, fromIntegral freedAddresses)
    _ -> throwError "The pending transaction does not exist"

-- When the pending transaction is online, we just delete it
deletePendingTxOnline :: (MonadUnliftIO m) => DBPendingTxId -> DB m ()
deletePendingTxOnline = P.delete

setPendingTxOnline :: (MonadUnliftIO m) => TxHash -> DB m Natural
setPendingTxOnline nosigH = do
  let nosigHT = txHashToHex nosigH
  cnt <- updateCount $ \p -> do
    set p [DBPendingTxOnline =. val True]
    where_ $ p ^. DBPendingTxNosigHash ==. val nosigHT
  return $ fromIntegral cnt

{- Helpers -}

extDeriv :: SoftPath
extDeriv = Deriv :/ 0

intDeriv :: SoftPath
intDeriv = Deriv :/ 1

isExtPath :: SoftPath -> Bool
isExtPath p =
  case pathToList p of
    [0, _] -> True
    _ -> False

isIntPath :: SoftPath -> Bool
isIntPath p =
  case pathToList p of
    [1, _] -> True
    _ -> False

pathIndex :: SoftPath -> KeyIndex
pathIndex p =
  case pathToList p of
    [_, i] -> i
    _ -> error "Invalid pathIndex"

bip44Deriv :: Network -> Natural -> HardPath
bip44Deriv net a = Deriv :| 44 :| net.bip44Coin :| fromIntegral a

xPubIndex :: XPubKey -> Natural
xPubIndex = fromIntegral . xPubChild

addrsDerivPage :: Ctx -> SoftPath -> Page -> XPubKey -> [(Address, SoftPath)]
addrsDerivPage ctx deriv (Page lim off) xpub =
  fmap (\(a, _, i) -> (a, deriv :/ i)) addrs
  where
    addrs =
      take (fromIntegral lim) $
        derivePathAddrs ctx xpub deriv (fromIntegral off)

-- like `maybe` but for a maybe/value sandwich
joinMaybe :: b -> (a -> b) -> Maybe (E.Value (Maybe a)) -> b
joinMaybe d f m =
  case m of
    Just (Value m') -> maybe d f m'
    Nothing -> d

flatMaybe :: Maybe (E.Value (Maybe a)) -> Maybe a
flatMaybe = joinMaybe Nothing Just
