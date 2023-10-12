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

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    liftEither,
    runExceptT,
  )
import Control.Monad
    ( MonadPlus(mzero), unless, void, when, unless )
import Conduit (MonadUnliftIO, ResourceT)
import Control.Monad.Except (ExceptT, MonadError (throwError))
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
import Data.ByteString
import qualified Data.Map as M
import Data.Serialize (decode, encode)
import Data.String.Conversions (cs)
import Data.Text (Text)
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
  xpubkey Text
  balance Word64
  Primary index network
  UniqueName name
  deriving Show

DBTx
  txid Text
  value ByteString
  Primary txid
  deriving Show

DBAddress
  index Int
  account DBAccountId
  derivation Text
  addr Text
  label Text
  received Word64
  txcount Int
  archived Bool
  Primary index account
  UniqueAddress addr
  deriving Show
|]

instance ToJSON DBAccount where
  toJSON (DBAccount idx net name deriv e i xpub b) =
    object
      [ "index" .= idx,
        "network" .= net,
        "name" .= name,
        "derivation" .= deriv,
        "external" .= e,
        "internal" .= i,
        "xpubkey" .= xpub,
        "balance" .= b
      ]

instance FromJSON DBAccount where
  parseJSON =
    withObject "accountstore" $ \o -> do
      DBAccount
        <$> o .: "index"
        <*> o .: "network"
        <*> o .: "name"
        <*> o .: "derivation"
        <*> o .: "external"
        <*> o .: "internal"
        <*> o .: "xpubkey"
        <*> o .: "balance"

type DB m = ReaderT SqlBackend (NoLoggingT (ResourceT m))

liftDB :: Monad m => m a -> DB m a
liftDB = lift . lift . lift

runDB :: MonadUnliftIO m => DB m a -> m a
runDB action = do
  dir <- liftIO $ hwDataDirectory Nothing
  let dbFile = dir </> "accounts.sqlite"
  runSqlite (cs dbFile) $ do
    _ <- runMigrationQuiet migrateAll
    action

nextAccountDeriv :: MonadUnliftIO m => Network -> DB m Natural
nextAccountDeriv net = do
  ds <-
    select $
      from $ \a -> do
        where_ (a ^. DBAccountNetwork ==. val (cs net.name))
        orderBy [desc (a ^. DBAccountIndex)]
        limit 1
        return $ a ^. DBAccountIndex
  case ds of
    [d] -> return $ fromIntegral (unValue d) + 1
    _ -> return 0

newAccount :: MonadUnliftIO m => Network -> Ctx -> Text -> Natural -> XPubKey -> DB m DBAccount
newAccount net ctx name deriv xpub = do
  let account =
        DBAccount
          { dBAccountIndex = fromIntegral deriv,
            dBAccountNetwork = cs net.name,
            dBAccountName = name,
            dBAccountDerivation =
              cs $ pathToStr $ bip44Deriv net $ fromIntegral $ xPubChild xpub,
            dBAccountExternal = 0,
            dBAccountInternal = 0,
            dBAccountXpubkey = xPubExport net ctx xpub,
            dBAccountBalance = 0
          }
  _ <- P.insert account
  return account

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
