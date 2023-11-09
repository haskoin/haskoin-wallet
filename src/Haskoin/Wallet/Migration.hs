{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Migration where

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
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO
import qualified Haskoin.Wallet.Migration.V0_9_0 as V0_9_0
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util (Page (Page), textToAddrE)
import Numeric.Natural (Natural)

getDatabaseTables :: (MonadUnliftIO m) => DB m [Text]
getDatabaseTables = do
  s <- rawSql "select name from sqlite_master where type='table'" []
  return $ unSingle <$> s

databaseVersion :: (MonadUnliftIO m) => DB m (Maybe Text)
databaseVersion = do
  tables <- getDatabaseTables
  if null tables
    then return Nothing
    else
      if "d_b_version" `elem` tables
        then Just <$> getVersion
        else return $ Just "0.8" -- Version table was created at v0.9.0

migrateDB :: (MonadUnliftIO m) => Ctx -> Config -> DB m ()
migrateDB ctx cfg = do
  resE <- runExceptT $ do
    verM <- lift databaseVersion
    case verM of
      Nothing ->
        lift $ do
          globalMigration
          setVersion versionString
      Just "0.8" -> do
        liftIO $ putStrLn "Migrating from 0.8.* to 0.9.0"
        V0_9_0.migrateDB ctx
      Just ver ->
        if ver == versionString
          then return () -- Current version, nothing to do
          else error $ "Invalid migration version: " <> cs ver
    return verM
  case resE of
    Left err -> do
      transactionUndo -- Roll back the migration and exit
      error err
    Right verM -> unless (verM == Just versionString) $ migrateDB ctx cfg
