{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.CommandsSpec where

import Conduit (MonadIO, runResourceT)
import Control.Arrow (second)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    liftEither,
    runExceptT,
  )
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (lift, liftIO)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import Data.Serialize (encode)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Database.Persist.Sqlite (SqlBackend, withSqliteConn)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Util.Arbitrary
import Network.Haskoin.Wallet.Database
import Network.Haskoin.Wallet.FileIO
import Network.Haskoin.Wallet.Signing
import Network.Haskoin.Wallet.SigningSpec
  ( extAddrsT,
    intAddrsT,
    keysT,
    keysT2,
    mnemPass,
    mnemPass2,
    walletFP,
    walletFP2,
    walletFPText,
    walletFPText2,
  )
import Network.Haskoin.Wallet.TestUtils
import Network.Haskoin.Wallet.TxInfo
import Network.Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Random (StdGen, mkStdGen)
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

identityTests :: Ctx -> IdentityTests
identityTests ctx = def

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    describe "Database" $ do
      bestSpec
      accountSpec ctx
      addressSpec ctx

liftTest :: (MonadIO m) => Expectation -> m ()
liftTest = liftIO

testNewAcc :: Ctx -> Text -> ExceptT String (DB IO) (DBAccountId, DBAccount)
testNewAcc ctx name = do
  let fp = forceRight $ walletFingerprint btc ctx mnemPass
  d <- lift $ nextAccountDeriv fp btc
  let prv = forceRight $ signingKey btc ctx mnemPass d
      pub = deriveXPubKey ctx prv
  insertAccount btc ctx fp name pub

testNewAcc2 :: Ctx -> Text -> ExceptT String (DB IO) (DBAccountId, DBAccount)
testNewAcc2 ctx name = do
  let fp = forceRight $ walletFingerprint btc ctx mnemPass2
  d <- lift $ nextAccountDeriv fp btc
  let prv = forceRight $ signingKey btc ctx mnemPass2 d
      pub = deriveXPubKey ctx prv
  insertAccount btc ctx fp name pub

bestSpec :: Spec
bestSpec =
  it "can insert and get the best block" $
    runDBMemory $ do
      res1 <- getBest btcTest
      liftTest $ res1 `shouldBe` Nothing
      updateBest
        btc
        "00000000000000000004727b3cc0946dc2054f59e362369e0437325c0a992efb"
        814037
      updateBest
        btcTest
        "000000000000001400d91785c92efc15ccfc1949e8581f06db7914a707c3f81d"
        2535443
      res2 <- getBest btc
      res3 <- getBest btcTest
      liftTest $ do
        res2
          `shouldBe` Just
            ( "00000000000000000004727b3cc0946dc2054f59e362369e0437325c0a992efb",
              814037
            )
        res3
          `shouldBe` Just
            ( "000000000000001400d91785c92efc15ccfc1949e8581f06db7914a707c3f81d",
              2535443
            )

shouldBeLeft :: (Show a) => ExceptT String (DB IO) a -> DB IO ()
shouldBeLeft action = liftTest . (`shouldSatisfy` isLeft) =<< runExceptT action

dbShouldBe :: (Show a, Eq a) => DB IO a -> a -> DB IO ()
dbShouldBe action a = liftTest . (`shouldBe` a) =<< action

dbShouldBeE ::
  (Show a, Eq a) =>
  ExceptT String (DB IO) a ->
  a ->
  ExceptT String (DB IO) ()
dbShouldBeE action a = liftTest . (`shouldBe` a) =<< action

accountSpec :: Ctx -> Spec
accountSpec ctx =
  it "can create and rename accounts" $ do
    runDBMemoryE $ do
      -- No accounts
      lift $ shouldBeLeft $ getAccountByName $ Just "acc1"
      lift $ shouldBeLeft $ getAccountByName Nothing
      lift $ nextAccountDeriv walletFP btc `dbShouldBe` 0
      lift $ nextAccountDeriv walletFP btc `dbShouldBe` 0 -- Still 0
      lift $ getAccounts `dbShouldBe` []
      -- Check basic account properties
      (accId, acc) <- testNewAcc ctx "acc1"
      lift $ nextAccountDeriv walletFP btc `dbShouldBe` 1
      lift $ nextAccountDeriv walletFP btc `dbShouldBe` 1 -- Still 1
      liftTest $ do
        dBAccountName acc `shouldBe` "acc1"
        dBAccountWallet acc `shouldBe` DBWalletKey walletFPText
        dBAccountIndex acc `shouldBe` 0
        dBAccountNetwork acc `shouldBe` "btc"
        dBAccountDerivation acc `shouldBe` "/44'/0'/0'"
        dBAccountExternal acc `shouldBe` 0
        dBAccountInternal acc `shouldBe` 0
        dBAccountXPubKey acc `shouldBe` snd keysT
        dBAccountBalanceConfirmed acc `shouldBe` 0
        dBAccountBalanceUnconfirmed acc `shouldBe` 0
        dBAccountBalanceCoins acc `shouldBe` 0
      -- Check account retrieval
      lift $ getAccounts `dbShouldBe` [(accId, acc)]
      getAccountByName (Just "acc1") `dbShouldBeE` (accId, acc)
      getAccountByName Nothing `dbShouldBeE` (accId, acc) -- There is just 1 account
      getAccountById accId `dbShouldBeE` acc
      lift $ getAccounts `dbShouldBe` [(accId, acc)]
      -- Check the creation of a second account
      (accId2, acc2) <- testNewAcc ctx "acc2"
      lift $ nextAccountDeriv walletFP btc `dbShouldBe` 2
      getAccountByName (Just "acc2") `dbShouldBeE` (accId2, acc2)
      lift $ getAccounts `dbShouldBe` [(accId, acc), (accId2, acc2)]
      lift $ shouldBeLeft $ getAccountByName Nothing -- There are > 1 accounts
      liftTest $ do
        dBAccountWallet acc2 `shouldBe` DBWalletKey walletFPText
        dBAccountDerivation acc2 `shouldBe` "/44'/0'/1'"
        dBAccountIndex acc2 `shouldBe` 1
      -- Create an account in a new wallet
      (accId3, acc3) <- testNewAcc2 ctx "acc3"
      liftTest $ do
        dBAccountName acc3 `shouldBe` "acc3"
        dBAccountWallet acc3 `shouldBe` DBWalletKey walletFPText2
        dBAccountIndex acc3 `shouldBe` 0
        dBAccountNetwork acc3 `shouldBe` "btc"
        dBAccountDerivation acc3 `shouldBe` "/44'/0'/0'"
        dBAccountXPubKey acc3 `shouldBe` snd keysT2
      lift $ nextAccountDeriv walletFP2 btc `dbShouldBe` 1
      lift $
        getAccounts `dbShouldBe` [(accId, acc), (accId2, acc2), (accId3, acc3)]
      getAccountByName (Just "acc3") `dbShouldBeE` (accId3, acc3)
      lift $ shouldBeLeft $ getAccountByName Nothing -- There are > 1 accounts
      getAccountById accId3 `dbShouldBeE` acc3
      -- Rename an account
      lift $ shouldBeLeft $ renameAccount "acc2" "acc2"
      lift $ shouldBeLeft $ renameAccount "acc2" "acc3"
      lift $ shouldBeLeft $ renameAccount "doesnotexist" "hello world"
      acc2' <- renameAccount "acc2" "hello world"
      liftIO $ acc2' `shouldBe` acc2 {dBAccountName = "hello world"}
      lift $ getAccountNames `dbShouldBe` ["acc1", "hello world", "acc3"]
      _ <- renameAccount "hello world" "acc2"
      lift $ getAccountNames `dbShouldBe` ["acc1", "acc2", "acc3"]

addressSpec :: Ctx -> Spec
addressSpec ctx =
  it "can generate addresses" $ do
    runDBMemoryE $ do
      (accId, acc) <- testNewAcc ctx "test"
      -- No addresses yet
      lift $ bestAddrWithFunds accId AddrInternal `dbShouldBe` Nothing
      lift $ bestAddrWithFunds accId AddrExternal `dbShouldBe` Nothing
      lift $ addressPage accId (Page 5 0) `dbShouldBe` []
      -- Generate external addresses
      ext1 <- genExtAddress ctx accId "Address 1"
      liftTest $ do
        dBAddressIndex ext1 `shouldBe` 0
        dBAddressAccountWallet ext1 `shouldBe` DBWalletKey walletFPText
        dBAddressAccountDerivation ext1 `shouldBe` "/44'/0'/0'"
        dBAddressDerivation ext1 `shouldBe` "/0/0"
        dBAddressAddress ext1 `shouldBe` head extAddrsT
        dBAddressLabel ext1 `shouldBe` "Address 1"
        dBAddressInternal ext1 `shouldBe` False
        dBAddressFree ext1 `shouldBe` False
      lift $ addressPage accId (Page 5 0) `dbShouldBe` [ext1]
      ext2 <- genExtAddress ctx accId "Address 2"
      liftTest $ do
        dBAddressIndex ext2 `shouldBe` 1
        dBAddressAccountWallet ext2 `shouldBe` DBWalletKey walletFPText
        dBAddressAccountDerivation ext2 `shouldBe` "/44'/0'/0'"
        dBAddressDerivation ext2 `shouldBe` "/0/1"
        dBAddressAddress ext2 `shouldBe` extAddrsT !! 1
        dBAddressLabel ext2 `shouldBe` "Address 2"
        dBAddressInternal ext2 `shouldBe` False
        dBAddressFree ext2 `shouldBe` False
      lift $ addressPage accId (Page 5 0) `dbShouldBe` [ext2, ext1]
      lift $ addressPage accId (Page 1 0) `dbShouldBe` [ext2]
      lift $ addressPage accId (Page 1 1) `dbShouldBe` [ext1]
      lift $ addressPage accId (Page 1 2) `dbShouldBe` []
      -- Generate internal addresses
      int1 <- nextFreeIntAddr ctx accId
      liftTest $ do
        dBAddressIndex int1 `shouldBe` 0
        dBAddressAccountWallet int1 `shouldBe` DBWalletKey walletFPText
        dBAddressAccountDerivation int1 `shouldBe` "/44'/0'/0'"
        dBAddressDerivation int1 `shouldBe` "/1/0"
        dBAddressAddress int1 `shouldBe` head intAddrsT
        dBAddressLabel int1 `shouldBe` "Internal Address"
        dBAddressInternal int1 `shouldBe` True
        dBAddressFree int1 `shouldBe` True
      int1' <- nextFreeIntAddr ctx accId -- Should still return the same free addr
      liftTest $ int1' `shouldBe` int1
      -- Set address labels
      _ <- setAddrLabel accId 0 "test address"
      lift $
        addressPage accId (Page 5 0)
          `dbShouldBe` [ext2, ext1 {dBAddressLabel = "test address"}]
