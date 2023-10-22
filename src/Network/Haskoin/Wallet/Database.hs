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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.Database where

import Conduit (MonadUnliftIO, ResourceT)
import Control.Arrow ((&&&))
import Control.Exception (try)
import Control.Monad
  ( MonadPlus (mzero),
    forM,
    forM_,
    unless,
    void,
    when,
    (<=<),
  )
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import qualified Data.Aeson as Json
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
  )
import Data.Bits (Bits (clearBit))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (GeneralCategory (OtherNumber))
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Serialize (decode, encode)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time
import Data.Word (Word32, Word64)
import Database.Esqueleto
import qualified Database.Persist as P
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
import Haskoin
import Haskoin.Crypto (xPubChild)
import Haskoin.Store.Data (confirmed)
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient (GetAddrsBalance (..), apiCall)
import Haskoin.Transaction
import Haskoin.Util (maybeToEither)
import Network.Haskoin.Wallet.Config
import Network.Haskoin.Wallet.FileIO
import Network.Haskoin.Wallet.TxInfo
import Network.Haskoin.Wallet.Util (Page (..), liftExcept, textToAddrE, (</>))
import Numeric.Natural (Natural)
import qualified System.Directory as D
import System.IO.Error (alreadyExistsErrorType)

{- SQL Table Definitions -}

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

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

runDB :: (MonadUnliftIO m) => DB m a -> m a
runDB action = do
  dir <- liftIO hwDataDirectory
  let dbFile = dir </> "accounts.sqlite"
  runSqlite (cs dbFile) $ do
    _ <- runMigrationQuiet migrateAll
    action

{- Meta -}

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
  dM <-
    selectOne . from $ \a -> do
      where_ $
        a ^. DBAccountNetwork ==. val (cs net.name)
          &&. a ^. DBAccountWallet ==. val walletId
      return $ max_ $ a ^. DBAccountIndex
  return $ joinMaybe 0 ((+ 1) . fromIntegral) dM

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
  ExceptT String (DB m) DBAccount
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
          lift $ P.insert_ account
          return account

-- When a name is provided, get that account or throw an error if it doesn't
-- exist. When no name is provided, return the account only if there is one
-- account.
getAccount ::
  (MonadUnliftIO m) =>
  Maybe Text ->
  ExceptT String (DB m) (DBAccountId, DBAccount)
getAccount (Just name) = do
  aM <- lift $ P.getBy $ UniqueName name
  case aM of
    Just a -> return (entityKey a, entityVal a)
    _ -> throwError $ "The account " <> cs name <> " does not exist"
getAccount Nothing = do
  as <- lift getAccounts
  case as of
    [a] -> return a
    [] -> throwError "There are no accounts in the wallet"
    _ -> throwError "Specify which account to use"

getAccountId ::
  (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) DBAccount
getAccountId accId =
  liftEither . maybeToEither "Invalid account" =<< lift (P.get accId)

getAccountVal ::
  (MonadUnliftIO m) => Maybe Text -> ExceptT String (DB m) DBAccount
getAccountVal nameM = snd <$> getAccount nameM

getAccounts :: (MonadUnliftIO m) => DB m [(DBAccountId, DBAccount)]
getAccounts =
  (go <$>) <$> P.selectList [] [P.Asc DBAccountCreated]
  where
    go a = (entityKey a, entityVal a)

getAccountsVal :: (MonadUnliftIO m) => DB m [DBAccount]
getAccountsVal = (entityVal <$>) <$> P.selectList [] [P.Asc DBAccountCreated]

getAccountNames :: (MonadUnliftIO m) => DB m [Text]
getAccountNames = do
  res <- select . from $ \a ->
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
            else getAccountVal (Just newName)

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

-- Insert an address into the database. Does nothing if it already exists.
insertAddress ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  SoftPath ->
  Address ->
  Text ->
  ExceptT String (DB m) DBAddress
insertAddress net (DBAccountKey wallet accDeriv) deriv addr label = do
  time <- liftIO getCurrentTime
  addrT <- liftEither $ maybeToEither "Invalid Address" (addrToText net addr)
  (isInternal, idx) <- parseDeriv
  let label' = if isInternal then "Internal Address" else label
      derivS = cs $ pathToStr deriv
      dbAddr =
        DBAddress
          (fromIntegral idx)
          wallet
          accDeriv
          derivS
          addrT
          label'
          0
          0
          0
          0
          0
          isInternal
          time
  addrM <- lift $ P.get (DBAddressKey wallet accDeriv derivS)
  case addrM of
    Just a -> return a
    Nothing -> do
      lift $ P.insert_ dbAddr
      return dbAddr
  where
    parseDeriv =
      case pathToList deriv of
        [0, x] -> return (False, x) -- Not an internal address
        [1, x] -> return (True, x) -- Internal address
        _ -> throwError "Invalid address SoftPath"

-- Set external or internal account derivation
-- Will also generate and insert the relevant addresses
updateDeriv ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  SoftPath ->
  Natural ->
  ExceptT String (DB m) DBAccount
updateDeriv net ctx accId path deriv = do
  acc <- getAccountId accId
  internal <-
    case pathToList path of
      [0] -> return False
      [1] -> return True
      _ -> throwError "Invalid updateDeriv SoftPath"
  let xPubKey = accountXPubKey ctx acc
      label
        | internal = DBAccountInternal
        | otherwise = DBAccountExternal
      accDeriv
        | internal = fromIntegral . dBAccountInternal
        | otherwise = fromIntegral . dBAccountExternal
  if accDeriv acc >= deriv
    then return acc -- Nothing to do
    else do
      let addrs =
            addrsDerivPage
              ctx
              path
              (Page (deriv - accDeriv acc) (accDeriv acc))
              xPubKey
      forM_ addrs $ \(a, p) -> insertAddress net accId p a ""
      lift $ P.update accId [label P.=. fromIntegral deriv]
      getAccountId accId

receiveAddress ::
  (MonadUnliftIO m) =>
  Ctx ->
  Maybe Text ->
  Text ->
  ExceptT String (DB m) (DBAccount, DBAddress)
receiveAddress ctx nameM label = do
  (accId, acc) <- getAccount nameM
  let net = accountNetwork acc
      xpub = accountXPubKey ctx acc
      idx = fromIntegral $ dBAccountExternal acc :: Natural
      idxW = fromIntegral idx :: Word32
      idxI = fromIntegral idx :: Int
  -- Verify that we respect the gap
  usedIdx <- lift $ lastUsedAddress accId False
  when (idx > usedIdx + gap) $
    throwError $
      "Can not generate addresses beyond the gap of " <> show gap
  -- Create the address and update the account index
  let (addr, _) = derivePathAddr ctx xpub extDeriv idxW
  dbAddr <- insertAddress net accId (extDeriv :/ idxW) addr label
  newAcc <-
    lift $ P.updateGet accId [DBAccountExternal P.=. idxI + 1]
  return (newAcc, dbAddr)

peekInternalAddress ::
  (MonadUnliftIO m) =>
  Ctx ->
  DBAccountId ->
  Natural -> -- Offset
  ExceptT String (DB m) (Address, SoftPath)
peekInternalAddress ctx accId offset'
  | offset' >= gap = throwError "peekInternalAddress: Offset >= gap"
  | otherwise = do
      acc <- getAccountId accId
      let accIdx = dBAccountInternal acc
          idx = fromIntegral accIdx + offset' :: Natural
          idxW = fromIntegral idx :: Word32
      -- Verify that we respect the gap
      usedIdx <- lift $ lastUsedAddress accId True
      when (idx > usedIdx + gap) $
        throwError $
          "Can not generate internal addresses beyond the gap of " <> show gap
      -- Create an internal address but do not update the account index
      let net = accountNetwork acc
          xpub = accountXPubKey ctx acc
          (addr, _) = derivePathAddr ctx xpub intDeriv idxW
      _ <- insertAddress net accId (intDeriv :/ idxW) addr ""
      return (addr, intDeriv :/ idxW)

commitInternalAddress ::
  (MonadUnliftIO m) =>
  DBAccountId ->
  Natural -> -- Offset
  ExceptT String (DB m) DBAccount
commitInternalAddress accId@(DBAccountKey wallet accDeriv) offset' = do
  acc <- getAccountId accId
  let accIdx = dBAccountInternal acc
      idx = fromIntegral accIdx + fromIntegral offset'
  addrM <- lift . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        &&. a ^. DBAddressInternal ==. val True
        &&. a ^. DBAddressIndex ==. val idx
    return a
  case addrM of
    Just _ -> do
      lift $ P.updateGet accId [DBAccountInternal P.=. idx + 1]
    Nothing -> throwError "commitInternalAddress: Invalid offset"

lastUsedAddress :: (MonadUnliftIO m) => DBAccountId -> Bool -> DB m Natural
lastUsedAddress (DBAccountKey wallet accDeriv) internal = do
  resM <- selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        &&. a ^. DBAddressInternal ==. val internal
        &&. a ^. DBAddressBalanceTxs >. val 0
    return $ max_ $ a ^. DBAddressIndex
  return $ joinMaybe 0 fromIntegral resM

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

updateLabel ::
  (MonadUnliftIO m) =>
  Maybe Text ->
  Word32 ->
  Text ->
  ExceptT String (DB m) (DBAccount, DBAddress)
updateLabel nameM idx label = do
  (DBAccountKey wallet accDeriv, acc) <- getAccount nameM
  let path = cs $ pathToStr $ extDeriv :/ idx
      aKey = DBAddressKey wallet accDeriv path
  unless (fromIntegral idx < dBAccountExternal acc) $
    throwError $
      "Address " <> show idx <> " does not exist"
  adr <- lift $ P.updateGet aKey [DBAddressLabel P.=. label]
  return (acc, adr)

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

existsTx :: (MonadUnliftIO m) => DBAccountId -> TxHash -> DB m Bool
existsTx (DBAccountKey wallet accDeriv) txid =
  isJust <$> P.get (DBTxInfoKey wallet accDeriv $ txHashToHex txid)

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
  DB m (DBTxInfo, Bool)
repsertTxInfo net ctx accId tif = do
  time <- liftIO getCurrentTime
  let confirmed' = Store.confirmed $ txInfoBlockRef tif
      tid = txHashToHex $ txInfoHash tif
      bRef = encode $ txInfoBlockRef tif
      -- Confirmations will get updated when retrieving them
      blob = BS.toStrict $ marshalJSON (net, ctx) tif {txInfoConfirmations = 0}
      (DBAccountKey wallet accDeriv) = accId
      key = DBTxInfoKey wallet accDeriv tid
      txInfo =
        DBTxInfo
          { dBTxInfoAccountWallet = wallet,
            dBTxInfoAccountDerivation = accDeriv,
            dBTxInfoTxid = tid,
            dBTxInfoBlockRef = bRef,
            dBTxInfoConfirmed = confirmed',
            dBTxInfoBlob = blob,
            dBTxInfoCreated = time
          }
  resM <- P.get key
  case resM of
    Just res -> do
      let newTxInfo =
            res
              { dBTxInfoBlockRef = bRef,
                dBTxInfoConfirmed = confirmed',
                dBTxInfoBlob = blob
              }
      P.replace key newTxInfo
      return (newTxInfo, res /= newTxInfo)
    Nothing -> do
      P.insert_ txInfo
      return (txInfo, True)

txsPage ::
  (MonadUnliftIO m) =>
  Ctx ->
  Maybe Text ->
  Page ->
  ExceptT String (DB m) (DBAccount, [TxInfo])
txsPage ctx nameM (Page lim off) = do
  (DBAccountKey wallet accDeriv, acc) <- getAccount nameM
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
      liftEither . maybeToEither "TxInfo unmarshalJSON Failed" $
        unmarshalJSON (net, ctx) $
          BS.fromStrict dbTx
  bestM <- lift $ getBest net
  case bestM of
    Just (_, h) -> return (acc, updateConfirmations h <$> res)
    _ -> return (acc, res)

updateConfirmations :: BlockHeight -> TxInfo -> TxInfo
updateConfirmations best tif =
  case txInfoBlockRef tif of
    Store.BlockRef h _ ->
      let bestI = toInteger best :: Integer
          hI = toInteger h :: Integer
          confI = bestI - hI + 1
       in tif {txInfoConfirmations = fromIntegral $ max 0 confI}
    Store.MemRef _ ->
      tif {txInfoConfirmations = 0}

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

toJsonCoin :: Network -> BlockHeight -> DBCoin -> Either String JsonCoin
toJsonCoin net bestHeight dbCoin = do
  op <- textToOutpoint $ dBCoinOutpoint dbCoin
  ad <-
    maybeToEither "toJsonCoin: Invalid address" $
      textToAddr net $
        dBCoinAddress dbCoin
  br <- decode $ dBCoinBlockRef dbCoin
  let confirmations =
        case br of
          Store.BlockRef h _ ->
            if bestHeight < fromIntegral h
              then 0
              else fromIntegral bestHeight - fromIntegral h + 1
          Store.MemRef _ -> 0
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
outpointText = encodeHex . encode

textToOutpoint :: Text -> Either String OutPoint
textToOutpoint t = do
  bs <- maybeToEither "textToOutpoint: invalid input" $ decodeHex t
  decode bs

-- Get all coins in an account, spendable or not
coinPage :: (MonadUnliftIO m) => DBAccountId -> Page -> DB m [DBCoin]
coinPage (DBAccountKey wallet accDeriv) (Page lim off) = do
  coins <-
    select . from $ \c -> do
      where_ $ do
        c ^. DBCoinAccountWallet ==. val wallet
          &&. c ^. DBCoinAccountDerivation ==. val accDeriv
      orderBy [asc (c ^. DBCoinBlockRef)]
      limit $ fromIntegral lim
      offset $ fromIntegral off
      return c
  return $ entityVal <$> coins

-- Spendable coins must be confirmed and not locked
getSpendableCoins ::
  (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) [Store.Unspent]
getSpendableCoins (DBAccountKey wallet accDeriv) = do
  coins <- lift . select . from $ \c -> do
    where_ $
      c ^. DBCoinAccountWallet ==. val wallet
        &&. c ^. DBCoinAccountDerivation ==. val accDeriv
        &&. c ^. DBCoinConfirmed ==. val True
        &&. c ^. DBCoinLocked ==. val False
    return c
  let bss = dBCoinBlob . entityVal <$> coins
  mapM (liftEither . decode) bss

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
            dBCoinBlockRef = encode unspent.block,
            dBCoinBlob = encode unspent,
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
    [ DBCoinBlob P.=. encode unspent,
      DBCoinConfirmed P.=. Store.confirmed unspent.block,
      DBCoinBlockRef P.=. encode unspent.block
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

-- Either insert, update or delete coins as required. Returns the number of
-- coins that have either been inserted, updated or deleted.
updateCoins ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  [Address] ->
  [Store.Unspent] ->
  ExceptT String (DB m) (Int, [DBCoin])
updateCoins net accId addrsToUpdate allUnspent = do
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
            Just c -> dBCoinConfirmed c /= confirmed s.block
            _ -> False

groupCoins :: Network -> [Store.Unspent] -> Map Text [Store.Unspent]
groupCoins net =
  Map.fromListWith (<>) . mapMaybe f
  where
    f x =
      case x.address of
        Just a -> (,[x]) <$> addrToText net a
        _ -> Nothing

setLockCoin :: (MonadUnliftIO m) => OutPoint -> Bool -> DB m ()
setLockCoin op locked = do
  let key = DBCoinKey $ outpointText op
  P.update key [DBCoinLocked P.=. locked]

{- Raw Transactions -}

insertRawTx :: (MonadUnliftIO m) => Tx -> DB m ()
insertRawTx tx = do
  let hash = txHashToHex $ txHash tx
      key = DBRawTxKey hash
  P.repsert key $ DBRawTx hash (encode tx)

getRawTx :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) Tx
getRawTx hash = do
  let key = DBRawTxKey $ txHashToHex hash
  txM <- lift $ P.get key
  case txM of
    Just tx -> liftEither . decode $ dBRawTxBlob tx
    Nothing -> throwError "getRawTx: missing transaction"

{- Pending Transactions -}

repsertPendingTx ::
  (MonadUnliftIO m) =>
  DBAccountId ->
  TxSignData ->
  ExceptT String (DB m) TxHash
repsertPendingTx (DBAccountKey wallet accDeriv) tsd = do
  time <- liftIO getCurrentTime
  let nosigHash = nosigTxHash $ txSignDataTx tsd
      nosigHashT = txHashToHex nosigHash
      key = DBPendingTxKey nosigHashT
      bs = BS.toStrict $ Json.encode tsd
      ptx = DBPendingTx wallet accDeriv nosigHashT bs time
  prevM <- lift $ getPendingTx nosigHash
  case prevM of
    Just prev -> do
      when (not (txSignDataSigned tsd) && txSignDataSigned prev) $
        throwError
          "There is already a signed copy of this transaction in the database"
      lift $ P.update key [DBPendingTxBlob P.=. bs]
    Nothing -> lift $ P.insert_ ptx
  return nosigHash

getPendingTx ::
  (MonadUnliftIO m) => TxHash -> DB m (Maybe TxSignData)
getPendingTx nosigHash = do
  let hashT = txHashToHex nosigHash
      key = DBPendingTxKey hashT
      f = Json.decode . BS.fromStrict . dBPendingTxBlob
  (f =<<) <$> P.get key

pendingTxPage ::
  (MonadUnliftIO m) =>
  DBAccountId ->
  Page ->
  ExceptT String (DB m) [(TxHash, TxSignData)]
pendingTxPage (DBAccountKey wallet accDeriv) (Page lim off) = do
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
    nosigHash <-
      liftEither $
        maybeToEither "TxHash" $
          hexToTxHash $
            dBPendingTxNosigHash res
    return (nosigHash, tsd)

{- Helpers -}

extDeriv :: SoftPath
extDeriv = Deriv :/ 0

intDeriv :: SoftPath
intDeriv = Deriv :/ 1

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
joinMaybe :: b -> (a -> b) -> Maybe (Value (Maybe a)) -> b
joinMaybe d f m =
  case m of
    Just (Value m') -> maybe d f m'
    Nothing -> d
