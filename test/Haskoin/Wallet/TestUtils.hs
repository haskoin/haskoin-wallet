{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskoin.Wallet.TestUtils where

import Control.Monad.Trans (liftIO, MonadIO)
import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either
import Data.String.Conversions (cs)
import Database.Persist.Sql (runMigrationQuiet)
import Database.Persist.Sqlite (runSqlite, transactionUndo)
import Haskoin.Wallet.Commands
import Haskoin.Wallet.Database
import Numeric.Natural
import Test.HUnit
import Test.QuickCheck
import Test.Hspec

genNatural :: Test.QuickCheck.Gen Natural
genNatural = arbitrarySizedNatural

forceRight :: Either a b -> b
forceRight = fromRight (error "fromRight")

runDBMemory :: (MonadUnliftIO m) => DB m a -> m ()
runDBMemory action = do
  runSqlite ":memory:" $ do
    _ <- runMigrationQuiet migrateAll
    void action

runDBMemoryE :: (MonadUnliftIO m, Show a) => ExceptT String (DB m) a -> m ()
runDBMemoryE action = do
  runSqlite ":memory:" $ do
    _ <- runMigrationQuiet migrateAll
    resE <- runExceptT action
    liftIO $ resE `shouldSatisfy` isRight

