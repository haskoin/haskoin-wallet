{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.CommandsSpec where

import Conduit (MonadIO, runResourceT)
import Control.Arrow (second)
import Control.Monad
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
  ( extAddrs,
    extAddrsT,
    intAddrs,
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
      extAddressSpec ctx
      intAddressSpec ctx

liftTest :: (MonadIO m) => Expectation -> m ()
liftTest = liftIO

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

dbShouldSatisfy :: (Show a) => DB IO a -> (a -> Bool) -> DB IO ()
dbShouldSatisfy action f = liftTest . (`shouldSatisfy` f) =<< action

dbShouldSatisfyE ::
  (Show a) =>
  ExceptT String (DB IO) a ->
  (a -> Bool) ->
  ExceptT String (DB IO) ()
dbShouldSatisfyE action f = liftTest . (`shouldSatisfy` f) =<< action

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

extAddressSpec :: Ctx -> Spec
extAddressSpec ctx =
  it "can generate external addresses" $ do
    runDBMemoryE $ do
      (accId, _) <- testNewAcc ctx "test"
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
      -- Test Paging
      lift $ addressPage accId (Page 5 0) `dbShouldBe` [ext2, ext1]
      lift $ addressPage accId (Page 1 0) `dbShouldBe` [ext2]
      lift $ addressPage accId (Page 1 1) `dbShouldBe` [ext1]
      lift $ addressPage accId (Page 1 2) `dbShouldBe` []
      -- Set address labels
      _ <- setAddrLabel accId 0 "test address"
      lift $
        addressPage accId (Page 5 0)
          `dbShouldBe` [ext2, ext1 {dBAddressLabel = "test address"}]
      -- Test the gap
      replicateM_ 18 $ genExtAddress ctx accId "" -- We have 20 addresses
      lift $ shouldBeLeft $ genExtAddress ctx accId "" -- fail gap
      lift $ addressPage accId (Page 100 0) `dbShouldSatisfy` ((== 20) . length)
      updateAddressBalances btc [Store.Balance (extAddrs !! 2) 0 0 0 1 1]
      updateAddressBalances btc [Store.Balance (extAddrs !! 4) 0 0 0 1 1]
      lift $ bestAddrWithFunds accId AddrExternal `dbShouldBe` Just 4
      replicateM_ 5 $ genExtAddress ctx accId "" -- We have 25 addresses
      lift $ shouldBeLeft $ genExtAddress ctx accId "" -- fail gap
      lift $ addressPage accId (Page 100 0) `dbShouldSatisfy` ((== 25) . length)
      -- Test discoverAccGenAddrs
      updateAddressBalances btc [Store.Balance (extAddrs !! 9) 0 0 0 1 1]
      discoverAccGenAddrs ctx accId AddrExternal 0
      lift $ addressPage accId (Page 100 0) `dbShouldSatisfy` ((== 25) . length)
      discoverAccGenAddrs ctx accId AddrExternal 25
      lift $ addressPage accId (Page 100 0) `dbShouldSatisfy` ((== 25) . length)
      discoverAccGenAddrs ctx accId AddrExternal 26
      lift $ addressPage accId (Page 100 0) `dbShouldSatisfy` ((== 26) . length)
      -- Test getAddrDeriv
      lift $ getAddrDeriv btc accId (head extAddrs)
        `dbShouldBe` Right (Deriv :/ 0 :/ 0)
      lift $ getAddrDeriv btc accId (extAddrs !! 7)
        `dbShouldBe` Right (Deriv :/ 0 :/ 7)
      -- Test accound balances
      acc1 <- lift $ updateAccountBalances accId
      liftTest $ do
        dBAccountBalanceConfirmed acc1 `shouldBe` 0
        dBAccountBalanceUnconfirmed acc1 `shouldBe` 0
        dBAccountBalanceCoins acc1 `shouldBe` 0
      updateAddressBalances btc [Store.Balance (extAddrs !! 1) 2 0 1 1 2]
      updateAddressBalances btc [Store.Balance (extAddrs !! 4) 2 8 2 2 10]
      _ <- nextFreeIntAddr ctx accId
      updateAddressBalances btc [Store.Balance (head intAddrs) 3 2 1 1 5]
      acc1' <- lift $ updateAccountBalances accId
      liftTest $ do
        dBAccountBalanceConfirmed acc1' `shouldBe` 7
        dBAccountBalanceUnconfirmed acc1' `shouldBe` 10
        dBAccountBalanceCoins acc1' `shouldBe` 4
      -- Test account indices
      liftTest $ do
        dBAccountInternal acc1' `shouldBe` 1
        dBAccountExternal acc1' `shouldBe` 26

intAddressSpec :: Ctx -> Spec
intAddressSpec ctx =
  it "can generate internal addresses" $ do
    runDBMemoryE $ do
      (accId, _) <- testNewAcc ctx "test"
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
      -- Test the Free status
      nextFreeIntAddr ctx accId `dbShouldBeE` int1 -- Should return the same address
      lift $ setAddrsFree AddrBusy [head intAddrsT] `dbShouldBe` 1
      lift $ setAddrsFree AddrBusy [intAddrsT !! 1] `dbShouldBe` 0
      (dBAddressIndex <$> nextFreeIntAddr ctx accId) `dbShouldBeE` 1
      _ <- lift $ setAddrsFree AddrFree [head intAddrsT]
      (dBAddressIndex <$> nextFreeIntAddr ctx accId) `dbShouldBeE` 0
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 1]
      (dBAddressIndex <$> nextFreeIntAddr ctx accId) `dbShouldBeE` 0
      _ <- lift $ setAddrsFree AddrBusy [head intAddrsT]
      (dBAddressIndex <$> nextFreeIntAddr ctx accId) `dbShouldBeE` 2
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 2]
      (dBAddressIndex <$> nextFreeIntAddr ctx accId) `dbShouldBeE` 3
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 3]
      (dBAddressIndex <$> nextFreeIntAddr ctx accId) `dbShouldBeE` 4
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 4]
      (dBAddressIndex <$> nextFreeIntAddr ctx accId) `dbShouldBeE` 5
      _ <- lift $ setAddrsFree AddrFree [intAddrsT !! 2]
      _ <- lift $ setAddrsFree AddrFree [intAddrsT !! 4]
      (dBAddressIndex <$> nextFreeIntAddr ctx accId) `dbShouldBeE` 2
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 2]
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 5]
      (dBAddressIndex <$> nextFreeIntAddr ctx accId) `dbShouldBeE` 4
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 4]
      (dBAddressIndex <$> nextFreeIntAddr ctx accId) `dbShouldBeE` 6
      -- Test account indices
      acc <- getAccountById accId
      liftTest $ do
        dBAccountInternal acc `shouldBe` 7
        dBAccountExternal acc `shouldBe` 0
