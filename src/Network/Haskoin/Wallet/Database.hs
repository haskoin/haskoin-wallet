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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.Database where

import Conduit (MonadUnliftIO, ResourceT)
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
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
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
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient (GetAddrsBalance (..), apiCall)
import Haskoin.Transaction
import Haskoin.Util (maybeToEither)
import Network.Haskoin.Wallet.AccountStore
import Network.Haskoin.Wallet.FileIO
import Network.Haskoin.Wallet.Signing (conf)
import Network.Haskoin.Wallet.TxInfo
import Network.Haskoin.Wallet.Util (Page (..), liftExcept, textToAddrE, (</>))
import Numeric.Natural (Natural)
import qualified System.Directory as D
import System.IO.Error (alreadyExistsErrorType)

{- SQL Table Definitions -}

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
DBAccount
  index Int
  network Text
  name Text
  derivation Text
  external Int
  internal Int
  xPubKey Text
  pubKeyFile Text
  balanceConfirmed Word64
  balanceUnconfirmed Word64
  balanceCoins Word64
  created UTCTime default=CURRENT_TIME
  Primary derivation
  UniqueName name
  UniqueXPubKey xPubKey
  UniqueNetworkId network index
  deriving Show

DBAddress
  index Int
  account DBAccountId
  derivation Text
  address Text
  label Text
  received Word64
  txcount Int
  internal Bool
  created UTCTime default=CURRENT_TIME
  Primary account derivation
  UniqueAddress address
  UniqueAccountDeriv account derivation
  deriving Show

DBTxInfo
  txid Text
  account DBAccountId
  blockRef ByteString
  confirmed Bool
  blob ByteString
  created UTCTime default=CURRENT_TIME
  Primary txid account
  deriving Show

DBBest
  account DBAccountId
  bestBlock Text
  bestHeight Int
  Primary account
  deriving Show

DBRawTx
  txid Text
  value ByteString
  Primary txid
  deriving Show
|]

gap :: Int64
gap = 20

type DB m = ReaderT SqlBackend (NoLoggingT (ResourceT m))

liftDB :: (Monad m) => DB m (Either String a) -> ExceptT String (DB m) a
liftDB = liftEither <=< lift

runDB :: (MonadUnliftIO m) => DB m a -> m a
runDB action = do
  dir <- liftIO $ hwDataDirectory Nothing
  let dbFile = dir </> "accounts.sqlite"
  runSqlite (cs dbFile) $ do
    _ <- runMigrationQuiet migrateAll
    action

{- Meta -}

updateBest ::
  (MonadUnliftIO m) => DBAccountId -> BlockHash -> BlockHeight -> DB m ()
updateBest accId hash height = do
  let key = DBBestKey accId
  P.repsert key $ DBBest accId (blockHashToHex hash) (fromIntegral height)

deleteBest ::
  (MonadUnliftIO m) => DBAccountId -> DB m ()
deleteBest accId = P.delete $ DBBestKey accId

getBest ::
     (MonadUnliftIO m) => DBAccountId -> DB m (Maybe (BlockHash, BlockHeight))
getBest accId = do
  resM <- P.get $ DBBestKey accId
  return $ do
    DBBest _ a b <- resM
    hash <- hexToBlockHash a
    return (hash, fromIntegral b)

{- Accounts -}

instance ToJSON DBAccount where
  toJSON (DBAccount idx net name deriv e i xpub keyfile bc bu bo t) =
    object
      [ "index" .= idx,
        "network" .= net,
        "name" .= name,
        "derivation" .= deriv,
        "external" .= e,
        "internal" .= i,
        "xPubKey" .= xpub,
        "pubKeyFile" .= keyfile,
        "balance" .= toJSON (AccountBalance bc bu bo),
        "created" .= t
      ]

instance FromJSON DBAccount where
  parseJSON =
    withObject "DBAccount" $ \o -> do
      bal <- o .: "balance"
      DBAccount
        <$> o .: "index"
        <*> o .: "network"
        <*> o .: "name"
        <*> o .: "derivation"
        <*> o .: "external"
        <*> o .: "internal"
        <*> o .: "xPubKey"
        <*> o .: "pubKeyFile"
        <*> (bal .: "confirmed")
        <*> (bal .: "unconfirmed")
        <*> (bal .: "coins")
        <*> o .: "created"

data AccountBalance = AccountBalance
  { -- | confirmed balance
    balanceConfirmed :: !Word64,
    -- | unconfirmed balance
    balanceUnconfirmed :: !Word64,
    -- | number of unspent outputs
    balanceCoins :: !Word64
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON AccountBalance where
  toJSON b =
    object
      [ "confirmed" .= balanceConfirmed b,
        "unconfirmed" .= balanceUnconfirmed b,
        "coins" .= balanceCoins b
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

accountXPubKey :: Ctx -> DBAccount -> XPubKey
accountXPubKey ctx acc =
  fromMaybe (error "Invalid XPubKey in database") $
    xPubImport (accountNetwork acc) ctx (dBAccountXPubKey acc)

nextAccountDeriv :: (MonadUnliftIO m) => Network -> DB m Natural
nextAccountDeriv net = do
  dM <-
    selectOne $
      from $ \a -> do
        where_ (a ^. DBAccountNetwork ==. val (cs net.name))
        orderBy [desc (a ^. DBAccountIndex)]
        return $ a ^. DBAccountIndex
  return $ maybe 0 ((+ 1) . fromIntegral . unValue) dM

existsAccount :: (MonadUnliftIO m) => Text -> DB m Bool
existsAccount name = P.existsBy $ UniqueName name

existsXPubKey :: (MonadUnliftIO m) => Network -> Ctx -> XPubKey -> DB m Bool
existsXPubKey net ctx key = P.existsBy $ UniqueXPubKey $ xPubExport net ctx key

insertAccount ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Text ->
  XPubKey ->
  Text ->
  DB m (Either String DBAccount)
insertAccount net ctx name xpub keyfile = do
  existsName <- existsAccount name
  if existsName
    then return $ Left $ "Account " <> cs name <> " already exists"
    else do
      existsKey <- existsXPubKey net ctx xpub
      if existsKey
        then return $ Left "The XPubKey already exists"
        else do
          time <- liftIO getCurrentTime
          let path = bip44Deriv net $ fromIntegral $ xPubChild xpub
              idx = fromIntegral $ xPubIndex xpub
              account =
                DBAccount
                  { dBAccountIndex = idx,
                    dBAccountNetwork = cs net.name,
                    dBAccountName = name,
                    dBAccountDerivation = cs $ pathToStr path,
                    dBAccountExternal = 0,
                    dBAccountInternal = 0,
                    dBAccountXPubKey = xPubExport net ctx xpub,
                    dBAccountPubKeyFile = keyfile,
                    dBAccountBalanceConfirmed = 0,
                    dBAccountBalanceUnconfirmed = 0,
                    dBAccountBalanceCoins = 0,
                    dBAccountCreated = time
                  }
          P.insert_ account
          return $ Right account

-- When a name is provided, get that account or throw an error if it doesn't
-- exist. When no name is provided, return the account only if there is one
-- account.
getAccount ::
  (MonadUnliftIO m) =>
  Maybe Text ->
  DB m (Either String (DBAccountId, DBAccount))
getAccount (Just name) = do
  aM <- P.getBy $ UniqueName name
  return $
    case aM of
      Just a -> Right (entityKey a, entityVal a)
      _ -> Left $ "The account " <> cs name <> " does not exist"
getAccount Nothing = do
  as <- getAccounts
  return $
    case as of
      [a] -> Right a
      [] -> Left "There are no accounts in the wallet"
      _ -> Left "Specify which account to use"

getAccountId ::
     (MonadUnliftIO m) => DBAccountId -> DB m (Either String DBAccount)
getAccountId accId = maybeToEither "Invalid account" <$> P.get accId

getAccountVal ::
  (MonadUnliftIO m) => Maybe Text -> DB m (Either String DBAccount)
getAccountVal nameM =
  runExceptT $ do
    (_, acc) <- liftEither =<< lift (getAccount nameM)
    return acc

getAccounts :: (MonadUnliftIO m) => DB m [(DBAccountId, DBAccount)]
getAccounts = (go <$>) <$> P.selectList [] [P.Asc DBAccountCreated]
  where
    go a = (entityKey a, entityVal a)

getAccountsVal :: (MonadUnliftIO m) => DB m [DBAccount]
getAccountsVal = (entityVal <$>) <$> P.selectList [] [P.Asc DBAccountCreated]

renameAccount ::
  (MonadUnliftIO m) => Text -> Text -> DB m (Either String DBAccount)
renameAccount oldName newName
  | oldName == newName = return $ Left "Old and new names are the same"
  | otherwise = do
      e <- existsAccount newName
      if e
        then return $ Left $ "The account " <> cs newName <> " already exists"
        else do
          c <-
            updateCount $ \a -> do
              set a [DBAccountName =. val newName]
              where_ (a ^. DBAccountName ==. val oldName)
          if c == 0
            then return $ Left $ "The account " <> cs oldName <> " does not exist"
            else getAccountVal (Just newName)

{- Addresses -}

instance ToJSON DBAddress where
  toJSON (DBAddress idx acc d addr l r t i c) =
    object
      [ "index" .= idx,
        "account" .= acc,
        "derivation" .= d,
        "address" .= addr,
        "label" .= l,
        "received" .= r,
        "txCount" .= t,
        "internal" .= i,
        "created" .= c
      ]

instance FromJSON DBAddress where
  parseJSON =
    withObject "DBAddress" $ \o ->
      DBAddress
        <$> o .: "index"
        <*> o .: "account"
        <*> o .: "derivation"
        <*> o .: "address"
        <*> o .: "label"
        <*> o .: "received"
        <*> o .: "txCount"
        <*> o .: "internal"
        <*> o .: "created"

-- Insert an address into the database. Does nothing if it already exists.
insertAddress ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  SoftPath ->
  Address ->
  Text ->
  DB m (Either String DBAddress)
insertAddress net accId deriv addr label =
  runExceptT $ do
    time <- liftIO getCurrentTime
    addrT <- liftEither $ maybeToEither "Invalid Address" (addrToText net addr)
    (isInternal, idx) <- parseDeriv
    let label' = if isInternal then "Internal Address" else label
        derivS = cs $ pathToStr deriv
        dbAddr =
          DBAddress
            (fromIntegral idx)
            accId
            derivS
            addrT
            label'
            0
            0
            isInternal
            time
    addrM <- lift $ P.get (DBAddressKey accId derivS)
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
  DB m (Either String DBAccount)
updateDeriv net ctx accId path deriv =
  runExceptT $ do
    acc <- liftEither <=< lift $ getAccountId accId
    internal <-
      case pathToList path of
        [0] -> return False
        [1] -> return True
        _ -> throwError "Invalid updateDeriv SoftPath"
    let xPubKey = accountXPubKey ctx acc
        label | internal = DBAccountInternal
              | otherwise = DBAccountExternal
        accDeriv | internal = dBAccountInternal
                | otherwise = dBAccountExternal
    if accDeriv acc >= fromIntegral deriv
      then return acc --Nothing to do
      else do
        let addrs = addrsDerivPage ctx path (Page deriv 0) xPubKey
        forM_ addrs $ \(a,p) -> lift $ insertAddress net accId p a ""
        lift $ update $ \a -> do
          set a [label =. val (fromIntegral deriv)]
          where_ (a ^. DBAccountId ==. val accId)
        liftEither <=< lift $ getAccountId accId

receiveAddress ::
  (MonadUnliftIO m) =>
  Ctx ->
  Maybe Text ->
  Text ->
  DB m (Either String (DBAccount, DBAddress))
receiveAddress ctx nameM label =
  runExceptT $ do
    (accId, acc) <- liftEither <=< lift $ getAccount nameM
    let net = accountNetwork acc
        xpub = accountXPubKey ctx acc
        ext = fromIntegral $ dBAccountExternal acc
        (addr, _) = derivePathAddr ctx xpub extDeriv ext
    dbAddr <-
      liftEither <=< lift $
        insertAddress net accId (extDeriv :/ ext) addr label
    newAcc <- lift $ P.updateGet accId [DBAccountExternal P.+=. 1]
    return (newAcc, dbAddr)

addressPage :: (MonadUnliftIO m) => DBAccountId -> Page -> DB m [DBAddress]
addressPage accId (Page lim off) = do
  as <-
    select $
      from $ \a -> do
        where_
          ( a ^. DBAddressAccount ==. val accId
              &&. a ^. DBAddressInternal ==. val False
          )
        orderBy [desc (a ^. DBAddressIndex)]
        limit $ fromIntegral lim
        offset $ fromIntegral off
        return a
  return $ entityVal <$> as

allAddressesMap ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  DB m (Either String (Map Address SoftPath))
allAddressesMap net accId =
  runExceptT $ do
    dbRes <-
      lift $
        select $
          from $ \a -> do
            where_ (a ^. DBAddressAccount ==. val accId)
            return (a ^. DBAddressAddress, a ^. DBAddressDerivation)
    res <-
      forM dbRes $ \(Value at, Value dt) -> do
        a <- liftEither $ textToAddrE net at
        d <- liftEither $ maybeToEither "parsePath failed" $ parseSoft $ cs dt
        return (a, d)
    return $ Map.fromList res

updateLabel ::
  (MonadUnliftIO m) =>
  Maybe Text ->
  Word32 ->
  Text ->
  DB m (Either String (DBAccount, DBAddress))
updateLabel nameM idx label = do
  runExceptT $ do
    (accId, acc) <- liftEither <=< lift $ getAccount nameM
    let path = cs $ pathToStr $ extDeriv :/ idx
        aKey = DBAddressKey accId path
    unless (fromIntegral idx < dBAccountExternal acc) $
      throwError $
        "Address " <> show idx <> " does not exist"
    adr <- lift $ P.updateGet aKey [DBAddressLabel P.=. label]
    return (acc, adr)

{- Transactions -}

-- Insert a new transaction or replace if it already exists
repsertTxInfo ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  TxInfo ->
  DB m DBTxInfo
repsertTxInfo net ctx accId tif = do
  time <- liftIO getCurrentTime
  let confirmed =
        case txInfoBlockRef tif of
          Store.BlockRef _ _ -> True
          Store.MemRef _ -> False
      tid = txHashToHex $ txInfoId tif
      bRef = encode $ txInfoBlockRef tif
      -- Confirmations will get updated when retrieving them
      blob = BS.toStrict $ marshalJSON (net, ctx) tif {txInfoConfirmations = 0}
      key = DBTxInfoKey tid accId
      txInfo =
        DBTxInfo
          { dBTxInfoTxid = tid,
            dBTxInfoAccount = accId,
            dBTxInfoBlockRef = bRef,
            dBTxInfoConfirmed = confirmed,
            dBTxInfoBlob = blob,
            dBTxInfoCreated = time
          }
  resM <- P.get key
  case resM of
    Just res -> do
      let newTxInfo =
            res
              { dBTxInfoBlockRef = bRef,
                dBTxInfoConfirmed = confirmed,
                dBTxInfoBlob = blob
              }
      P.replace key newTxInfo
      return newTxInfo
    Nothing -> do
      P.insert_ txInfo
      return txInfo

getTxs ::
  (MonadUnliftIO m) =>
  Ctx ->
  Maybe Text ->
  Page ->
  DB m (Either String (DBAccount, [TxInfo]))
getTxs ctx nameM (Page lim off) =
  runExceptT $ do
    (accId, acc) <- liftEither <=< lift $ getAccount nameM
    let net = accountNetwork acc
    dbTxs <-
      lift $
        select $
          from $ \t -> do
            where_ $ t ^. DBTxInfoAccount ==. val accId
            orderBy [asc (t ^. DBTxInfoBlockRef)]
            limit $ fromIntegral lim
            offset $ fromIntegral off
            return $ t ^. DBTxInfoBlob
    res <-
      forM dbTxs $ \(Value dbTx) -> do
        liftEither $
          maybeToEither "TxInfo unmarshalJSON Failed" $
            unmarshalJSON (net, ctx) $
              BS.fromStrict dbTx
    bestM <- lift $ getBest accId
    case bestM of
      Just (_, h) ->
        return (acc, updateConfirmations h <$> res)
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
