{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskoin.Wallet.TestUtils where

import Control.Monad
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word
import Database.Persist.Sql (runMigrationQuiet)
import Database.Persist.Sqlite (runSqlite)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Util.Arbitrary
import Haskoin.Wallet.Backup
import Haskoin.Wallet.Commands
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.TxInfo
import Numeric.Natural
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

genNatural :: Test.QuickCheck.Gen Natural
genNatural = arbitrarySizedNatural

forceRight :: Either a b -> b
forceRight = fromRight (error "fromRight")

runDBMemory :: DB IO a -> Assertion
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

arbitraryText :: Gen Text
arbitraryText = cs <$> (arbitrary :: Gen String)

arbitraryPositive :: (Arbitrary a, Integral a) => Gen a
arbitraryPositive = abs <$> arbitrary

arbitraryNatural :: Gen Natural
arbitraryNatural = fromIntegral <$> (arbitraryPositive :: Gen Word64)

arbitraryBlockRef :: Gen Store.BlockRef
arbitraryBlockRef =
  oneof [a, b]
  where
    a = Store.BlockRef <$> arbitraryPositive <*> arbitraryPositive
    b = Store.MemRef <$> arbitrary

arbitraryDBAccount :: Network -> Ctx -> Gen DBAccount
arbitraryDBAccount net ctx =
  DBAccount
    <$> arbitraryText
    <*> (DBWalletKey . fingerprintToText <$> arbitraryFingerprint)
    <*> arbitraryPositive
    <*> (cs . (.name) <$> arbitraryNetwork)
    <*> (cs . pathToStr <$> arbitraryDerivPath)
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> (xPubExport net ctx <$> arbitraryXPubKey ctx)
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryUTCTime

arbitraryDBAddress :: Network -> Gen DBAddress
arbitraryDBAddress net =
  DBAddress
    <$> arbitraryPositive
    <*> (DBWalletKey . fingerprintToText <$> arbitraryFingerprint)
    <*> (cs . pathToStr <$> arbitraryDerivPath)
    <*> (cs . pathToStr <$> arbitraryDerivPath)
    <*> (fromJust . addrToText net <$> arbitraryAddress)
    <*> arbitraryText
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitrary
    <*> arbitrary
    <*> arbitraryUTCTime

arbitraryAddressBalance :: Gen AddressBalance
arbitraryAddressBalance =
  AddressBalance
    <$> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive

arbitraryJsonCoin :: Gen JsonCoin
arbitraryJsonCoin =
  JsonCoin
    <$> arbitraryOutPoint
    <*> arbitraryAddress
    <*> arbitraryPositive
    <*> arbitraryBlockRef
    <*> arbitraryNatural
    <*> arbitrary

arbitraryPubKeyDoc :: Ctx -> Gen PubKeyDoc
arbitraryPubKeyDoc ctx =
  PubKeyDoc
    <$> arbitraryXPubKey ctx
    <*> arbitraryNetwork
    <*> arbitraryText
    <*> arbitraryFingerprint

arbitraryTxSignData :: Network -> Ctx -> Gen TxSignData
arbitraryTxSignData net ctx =
  TxSignData
    <$> arbitraryTx net ctx
    <*> resize 10 (listOf (arbitraryTx net ctx))
    <*> resize 10 (listOf arbitrarySoftPath)
    <*> resize 10 (listOf arbitrarySoftPath)
    <*> arbitrary

arbitraryMyOutputs :: Gen MyOutputs
arbitraryMyOutputs =
  MyOutputs
    <$> arbitraryNatural
    <*> arbitrarySoftPath
    <*> arbitraryText

arbitraryMyInputs :: Network -> Ctx -> Gen MyInputs
arbitraryMyInputs net ctx =
  MyInputs
    <$> arbitraryNatural
    <*> arbitrarySoftPath
    <*> arbitraryText
    <*> resize 10 (listOf $ fst <$> arbitrarySigInput net ctx)

arbitraryOtherInputs :: Network -> Ctx -> Gen OtherInputs
arbitraryOtherInputs net ctx =
  OtherInputs
    <$> arbitraryNatural
    <*> resize 10 (listOf $ fst <$> arbitrarySigInput net ctx)

arbitraryTxType :: Gen TxType
arbitraryTxType = oneof [pure TxDebit, pure TxInternal, pure TxCredit]

arbitraryStoreOutput :: Gen Store.StoreOutput
arbitraryStoreOutput =
  Store.StoreOutput
    <$> arbitrary
    <*> arbitraryBS1
    <*> arbitraryMaybe arbitrarySpender
    <*> arbitraryMaybe arbitraryAddress

arbitrarySpender :: Gen Store.Spender
arbitrarySpender = Store.Spender <$> arbitraryTxHash <*> arbitrary

arbitraryStoreInput :: Gen Store.StoreInput
arbitraryStoreInput =
  oneof
    [ Store.StoreCoinbase
        <$> arbitraryOutPoint
        <*> arbitrary
        <*> arbitraryBS1
        <*> listOf arbitraryBS1,
      Store.StoreInput
        <$> arbitraryOutPoint
        <*> arbitrary
        <*> arbitraryBS1
        <*> arbitraryBS1
        <*> arbitrary
        <*> listOf arbitraryBS1
        <*> arbitraryMaybe arbitraryAddress
    ]

arbitraryTxInfo :: Network -> Ctx -> Gen TxInfo
arbitraryTxInfo net ctx =
  TxInfo
    <$> arbitraryMaybe arbitraryTxHash
    <*> arbitraryTxType
    <*> arbitrary
    <*> ( Map.fromList
            <$> resize
              10
              (listOf $ (,) <$> arbitraryAddress <*> arbitraryMyOutputs)
        )
    <*> ( Map.fromList
            <$> resize
              10
              (listOf $ (,) <$> arbitraryAddress <*> arbitraryNatural)
        )
    <*> resize 10 (listOf arbitraryStoreOutput)
    <*> ( Map.fromList
            <$> resize
              10
              (listOf $ (,) <$> arbitraryAddress <*> arbitraryMyInputs net ctx)
        )
    <*> ( Map.fromList
            <$> resize
              10
              (listOf $ (,) <$> arbitraryAddress <*> arbitraryOtherInputs net ctx)
        )
    <*> resize 10 (listOf arbitraryStoreInput)
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryBlockRef
    <*> arbitraryNatural
    <*> arbitraryMaybe arbitraryTxInfoPending

arbitraryTxInfoPending :: Gen TxInfoPending
arbitraryTxInfoPending =
  TxInfoPending
    <$> arbitraryTxHash
    <*> arbitrary
    <*> arbitrary

arbitraryAccountBackup :: Ctx -> Gen AccountBackup
arbitraryAccountBackup ctx =
  AccountBackup
    <$> arbitraryText
    <*> arbitraryFingerprint
    <*> arbitraryXPubKey ctx
    <*> arbitraryNetwork
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> ( Map.fromList
            <$> resize 20 (listOf $ (,) <$> arbitraryAddress <*> arbitraryText)
        )
    <*> resize 20 (listOf arbitraryAddress)
    <*> arbitraryUTCTime

arbitraryResponse :: Network -> Ctx -> Gen Response
arbitraryResponse net ctx =
  oneof
    [ ResponseError <$> arbitraryText,
      ResponseMnemonic
        <$> arbitraryText
        <*> resize 12 (listOf arbitraryText)
        <*> resize 12 (listOf $ resize 12 $ listOf arbitraryText),
      ResponseAccount <$> arbitraryDBAccount net ctx,
      ResponseTestAcc
        <$> arbitraryDBAccount net ctx
        <*> arbitrary
        <*> arbitraryText,
      ResponseFile <$> (cs <$> arbitraryText),
      ResponseAccounts <$> resize 20 (listOf $ arbitraryDBAccount net ctx),
      ResponseAddress <$> arbitraryDBAccount net ctx <*> arbitraryDBAddress net,
      ResponseAddresses
        <$> arbitraryDBAccount net ctx
        <*> resize 20 (listOf $ arbitraryDBAddress net),
      ResponseTxs
        <$> arbitraryDBAccount net ctx
        <*> resize 20 (listOf $ arbitraryTxInfo net ctx),
      ResponseTx <$> arbitraryDBAccount net ctx <*> arbitraryTxInfo net ctx,
      ResponseDeleteTx
        <$> arbitraryTxHash
        <*> arbitraryNatural
        <*> arbitraryNatural,
      ResponseCoins
        <$> arbitraryDBAccount net ctx
        <*> resize 20 (listOf arbitraryJsonCoin),
      ResponseSync
        <$> arbitraryDBAccount net ctx
        <*> arbitraryBlockHash
        <*> arbitrary
        <*> arbitraryNatural
        <*> arbitraryNatural,
      ResponseRestore
        <$> resize
          20
          ( listOf $
              (,,)
                <$> arbitraryDBAccount net ctx
                <*> arbitraryNatural
                <*> arbitraryNatural
          ),
      ResponseVersion
        <$> arbitraryText
        <*> arbitraryText,
      ResponseRollDice <$> resize 20 (listOf arbitraryNatural) <*> arbitraryText
    ]

arbitraryConfig :: Gen Config
arbitraryConfig =
  Config
    <$> arbitraryText
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitrary
