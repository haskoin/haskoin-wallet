{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Migration where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Esqueleto.Legacy as E
import Haskoin
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import qualified Haskoin.Wallet.Migration.V0_9_0 as V0_9_0

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
      if "d_b_version" `elem` tables -- Version table was created at v0.9.0
        then Just <$> getVersion
        else return $ Just V0_9_0.migrateFrom

migrateDB :: (MonadUnliftIO m) => Ctx -> Config -> DB m ()
migrateDB ctx cfg = do
  resE <- runExceptT $ do
    verM <- lift databaseVersion
    case verM of
      Nothing ->
        lift $ do
          globalMigration
          setVersion versionString
      Just ver -> migrateDB_ ctx ver
    return verM
  case resE of
    Left err -> do
      transactionUndo -- Roll back the migration and exit
      error err
    Right verM -> unless (verM == Just versionString) $ migrateDB ctx cfg

migrateDB_ ::
  (MonadUnliftIO m) => Ctx -> Text -> ExceptT String (DB m) ()
migrateDB_ ctx ver
  | ver == V0_9_0.migrateFrom = do
      liftIO $ printDBMigration V0_9_0.migrateFrom V0_9_0.migrateTo
      V0_9_0.migrateDB ctx
  | ver == versionString = return ()
  | otherwise = error $ "Invalid migration version: " <> cs ver

printDBMigration :: Text -> Text -> IO ()
printDBMigration fromV toV =
  putStrLn $ "Migrating from " <> cs fromV <> " to " <> cs toV
