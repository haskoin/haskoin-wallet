{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.TestUtils where

import Control.Monad.Trans (liftIO)
import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either
import Data.String.Conversions (cs)
import Database.Persist.Sql (runMigrationQuiet)
import Database.Persist.Sqlite (runSqlite, transactionUndo)
import Network.Haskoin.Wallet.Commands
import Network.Haskoin.Wallet.Database
import Numeric.Natural
import Test.HUnit
import Test.QuickCheck
import Test.Hspec

genNatural :: Test.QuickCheck.Gen Natural
genNatural = arbitrarySizedNatural

forceRight :: Either a b -> b
forceRight = fromRight (error "fromRight")

runDBMemory :: DB IO a -> IO ()
runDBMemory action = do
  runSqlite ":memory:" $ do
    _ <- runMigrationQuiet migrateAll
    void action

runDBMemoryE :: (Show a) => ExceptT String (DB IO) a -> Assertion
runDBMemoryE action = do
  runSqlite ":memory:" $ do
    _ <- runMigrationQuiet migrateAll
    resE <- runExceptT action
    liftIO $ resE `shouldSatisfy` isRight

