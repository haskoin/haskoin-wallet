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
    unless,
    void,
    when,
    (<=<),
  )
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Data.Aeson (Value (Bool))
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
  )
import Data.Bits (Bits (clearBit))
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Serialize (decode, encode)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time
import Data.Word (Word64)
import Database.Esqueleto
import qualified Database.Persist as P
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
import Haskoin
import Haskoin.Crypto (xPubChild)
import Haskoin.Store.Data (SerialList (..), Unspent, XPubSpec (deriv))
import Haskoin.Store.WebClient (GetAddrsBalance (..), apiCall)
import Haskoin.Transaction
import Haskoin.Util (maybeToEither)
import Network.Haskoin.Wallet.AccountStore
import Network.Haskoin.Wallet.FileIO
import Network.Haskoin.Wallet.Signing (conf)
import Network.Haskoin.Wallet.Util (liftExcept, (</>))
import Numeric.Natural (Natural)
import qualified System.Directory as D

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
  Primary index network
  UniqueName name
  UniqueXPubKey xPubKey
  deriving Show

DBTx
  txid Text
  value BS.ByteString
  Primary txid
  deriving Show

DBAddress
  index Int
  account DBAccountId
  derivation Text
  address Text
  label Text
  received Word64
  txcount Int
  archived Bool
  created UTCTime default=CURRENT_TIME
  Primary index account
  UniqueAddress address
  deriving Show
|]

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
    withObject "accountstore" $ \o -> do
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

accountPubKey :: Ctx -> DBAccount -> XPubKey
accountPubKey ctx acc =
  fromMaybe (error "Invalid XPubKey in database") $
    xPubImport (accountNetwork acc) ctx (dBAccountXPubKey acc)

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
existsAccount name = do
  aM <- P.getBy $ UniqueName name
  return $ isJust aM

existsXPubKey :: (MonadUnliftIO m) => Network -> Ctx -> XPubKey -> DB m Bool
existsXPubKey net ctx key = do
  kM <- P.getBy $ UniqueXPubKey $ xPubExport net ctx key
  return $ isJust kM

newAccount ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Text ->
  XPubKey ->
  Text ->
  DB m (Either String DBAccount)
newAccount net ctx name xpub keyfile = do
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
  (MonadUnliftIO m) => Maybe Text -> DB m (Either String DBAccount)
getAccount (Just name) = do
  aM <- P.getBy $ UniqueName name
  return $
    case aM of
      Just a -> Right $ entityVal a
      _ -> Left $ "The account " <> cs name <> " does not exist"
getAccount Nothing = do
  as <- getAccounts
  return $
    case as of
      [a] -> Right a
      [] -> Left "There are no accounts in the wallet"
      _ -> Left "Specify which account to use"

getAccounts :: (MonadUnliftIO m) => DB m [DBAccount]
getAccounts = (entityVal <$>) <$> P.selectList [] [P.Asc DBAccountCreated]

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
            else getAccount $ Just newName

{- SQL Data Type Marshalling

instance PersistFieldSql TxHash where
  sqlType _ = SqlString

instance PersistField TxHash where
  toPersistValue = PersistText . txHashToHex
  fromPersistValue (PersistText h) = maybeToEither "Invalid hexToTxHash" $ hexToTxHash h
  fromPersistValue _ = Left "Invalid PersistField TxHash"

instance PersistFieldSql Tx where
  sqlType _ = SqlBlob

instance PersistField Tx where
  toPersistValue = PersistByteString . encode
  fromPersistValue (PersistByteString bs) =
    case decode bs of
      Right tx -> return tx
      Left err -> Left $ "Invalid PersistField Tx: " <> cs err
  fromPersistValue _ = Left "Invalid PersistField Tx"
 -}
