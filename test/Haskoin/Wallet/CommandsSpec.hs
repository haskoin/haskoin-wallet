{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -fno-warn-orphans #-}

module Haskoin.Wallet.CommandsSpec where

import Conduit (MonadIO, liftIO)
import Control.Arrow (second)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.Default (def)
import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Serialize as S
import Data.Text (Text)
import Database.Esqueleto.Legacy hiding (isNothing)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Util.Arbitrary
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Signing
import Haskoin.Wallet.SigningSpec
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util
import Test.Hspec

identityTests :: Ctx -> IdentityTests
identityTests ctx =
  def
    { jsonTests =
        [ JsonBox $ arbitraryDBAccount btc ctx,
          JsonBox $ arbitraryDBAddress btc,
          JsonBox arbitraryAddressBalance,
          JsonBox $ arbitraryTxSignData btc ctx,
          JsonBox arbitraryConfig
        ],
      marshalJsonTests =
        [ MarshalJsonBox ((btc,) <$> arbitraryJsonCoin),
          MarshalJsonBox ((ctx,) <$> arbitraryPubKeyDoc ctx),
          MarshalJsonBox (((btc, ctx),) <$> arbitraryTxInfo btc ctx),
          MarshalJsonBox ((ctx,) <$> arbitraryResponse btc ctx),
          MarshalJsonBox ((ctx,) <$> arbitraryAccountBackup ctx)
        ]
    }

spec :: Spec
spec = do
  let cfg = def :: Config
  prepareContext $ \ctx -> do
    testIdentity $ identityTests ctx
    describe "Database" $ do
      bestSpec
      accountSpec ctx
      extAddressSpec ctx cfg
      intAddressSpec ctx cfg
      txsSpec ctx
      coinSpec ctx
      pendingTxsSpec ctx cfg

liftTest :: (MonadIO m) => Expectation -> m ()
liftTest = liftIO

shouldBeLeft :: (Show a) => ExceptT String (DB IO) a -> DB IO ()
shouldBeLeft action = liftTest . (`shouldSatisfy` isLeft) =<< runExceptT action

shouldBeLeft' ::
  (Show a, Eq a) => String -> ExceptT String (DB IO) a -> DB IO ()
shouldBeLeft' err action =
  liftTest . (`shouldBe` Left err) =<< runExceptT action

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
accountSpec ctx = do
  it "can find the correct account derivations with smallestUnused" $ do
    smallestUnused [] `shouldBe` 0
    smallestUnused [0] `shouldBe` 1
    smallestUnused [1] `shouldBe` 0
    smallestUnused [2] `shouldBe` 0
    smallestUnused [1,2,3] `shouldBe` 0
    smallestUnused [1,3,4] `shouldBe` 0
    smallestUnused [0,2] `shouldBe` 1
    smallestUnused [0,1,3,4] `shouldBe` 2
    smallestUnused [0,1,3,4,6,7] `shouldBe` 2
    smallestUnused [0,1,2,3,4,5] `shouldBe` 6
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
        getAccounts `dbShouldBe` [(accId3, acc3), (accId, acc), (accId2, acc2)]
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

extAddressSpec :: Ctx -> Config -> Spec
extAddressSpec ctx cfg =
  it "can generate external addresses" $ do
    runDBMemoryE $ do
      (accId, _) <- testNewAcc ctx "test"
      -- No addresses yet
      lift $ bestAddrWithFunds accId AddrInternal `dbShouldBe` Nothing
      lift $ bestAddrWithFunds accId AddrExternal `dbShouldBe` Nothing
      lift $ addressPage accId (Page 5 0) `dbShouldBe` []
      -- Generate external addresses
      ext1 <- genExtAddress ctx cfg accId "Address 1"
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
      ext2 <- genExtAddress ctx cfg accId "Address 2"
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
      replicateM_ 18 $ genExtAddress ctx cfg accId "" -- We have 20 addresses
      lift $ shouldBeLeft $ genExtAddress ctx cfg accId "" -- fail gap
      lift $ addressPage accId (Page 100 0) `dbShouldSatisfy` ((== 20) . length)
      updateAddressBalances btc [Store.Balance (extAddrs !! 2) 0 0 0 1 1]
      updateAddressBalances btc [Store.Balance (extAddrs !! 4) 0 0 0 1 1]
      lift $ bestAddrWithFunds accId AddrExternal `dbShouldBe` Just 4
      replicateM_ 5 $ genExtAddress ctx cfg accId "" -- We have 25 addresses
      lift $ shouldBeLeft $ genExtAddress ctx cfg accId "" -- fail gap
      lift $ addressPage accId (Page 100 0) `dbShouldSatisfy` ((== 25) . length)
      -- Test discoverAccGenAddrs
      updateAddressBalances btc [Store.Balance (extAddrs !! 9) 0 0 0 1 1]
      discoverAccGenAddrs ctx cfg accId AddrExternal 0
      lift $ addressPage accId (Page 100 0) `dbShouldSatisfy` ((== 25) . length)
      discoverAccGenAddrs ctx cfg accId AddrExternal 25
      lift $ addressPage accId (Page 100 0) `dbShouldSatisfy` ((== 25) . length)
      discoverAccGenAddrs ctx cfg accId AddrExternal 26
      lift $ addressPage accId (Page 100 0) `dbShouldSatisfy` ((== 26) . length)
      -- Test getAddrDeriv
      lift $
        getAddrDeriv btc accId (head extAddrs)
          `dbShouldBe` Right (Deriv :/ 0 :/ 0)
      lift $
        getAddrDeriv btc accId (extAddrs !! 7)
          `dbShouldBe` Right (Deriv :/ 0 :/ 7)
      -- Test accound balances
      acc1 <- lift $ updateAccountBalances accId
      liftTest $ do
        dBAccountBalanceConfirmed acc1 `shouldBe` 0
        dBAccountBalanceUnconfirmed acc1 `shouldBe` 0
        dBAccountBalanceCoins acc1 `shouldBe` 0
      updateAddressBalances btc [Store.Balance (extAddrs !! 1) 2 0 1 1 2]
      updateAddressBalances btc [Store.Balance (extAddrs !! 4) 2 8 2 2 10]
      _ <- nextFreeIntAddr ctx cfg accId
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

intAddressSpec :: Ctx -> Config -> Spec
intAddressSpec ctx cfg =
  it "can generate internal addresses" $ do
    runDBMemoryE $ do
      (accId, _) <- testNewAcc ctx "test"
      -- Generate internal addresses
      int1 <- nextFreeIntAddr ctx cfg accId
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
      nextFreeIntAddr ctx cfg accId `dbShouldBeE` int1 -- Should return the same address
      lift $ setAddrsFree AddrBusy [head intAddrsT] `dbShouldBe` 1
      lift $ setAddrsFree AddrBusy [intAddrsT !! 1] `dbShouldBe` 0
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 1
      _ <- lift $ setAddrsFree AddrFree [head intAddrsT]
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 0
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 1]
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 0
      _ <- lift $ setAddrsFree AddrBusy [head intAddrsT]
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 2
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 2]
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 3
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 3]
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 4
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 4]
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 5
      _ <- lift $ setAddrsFree AddrFree [intAddrsT !! 2]
      _ <- lift $ setAddrsFree AddrFree [intAddrsT !! 4]
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 2
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 2]
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 5]
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 4
      _ <- lift $ setAddrsFree AddrBusy [intAddrsT !! 4]
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 6
      -- Test account indices
      acc <- getAccountById accId
      liftTest $ do
        dBAccountInternal acc `shouldBe` 7
        dBAccountExternal acc `shouldBe` 0

emptyTxInfo :: TxInfo
emptyTxInfo =
  TxInfo
    (Just $ txid' 0)
    TxInternal
    0
    Map.empty
    Map.empty
    []
    Map.empty
    Map.empty
    []
    0
    0
    0
    (Store.MemRef 0)
    0
    Nothing

txsSpec :: Ctx -> Spec
txsSpec ctx =
  it "can manage transactions" $ do
    runDBMemoryE $ do
      (accId, _) <- testNewAcc ctx "test"
      -- Simple insert and retrieval
      (dbInfo, change) <- repsertTxInfo btc ctx accId emptyTxInfo
      liftTest $ do
        change `shouldBe` True
        dBTxInfoAccountWallet dbInfo `shouldBe` DBWalletKey walletFPText
        dBTxInfoAccountDerivation dbInfo `shouldBe` "/44'/0'/0'"
        dBTxInfoBlockRef dbInfo `shouldBe` S.encode (Store.MemRef 0)
        dBTxInfoConfirmed dbInfo `shouldBe` False
      -- Reinserting should produce no change
      (dbInfo2, change2) <- repsertTxInfo btc ctx accId emptyTxInfo
      liftTest $ do
        change2 `shouldBe` False
        dbInfo2 `shouldBe` dbInfo
      txsPage ctx accId (Page 5 0) `dbShouldBeE` [emptyTxInfo]
      -- Check that confirmations are updated correctly
      lift $ updateBest btc (bid' 0) 0
      getConfirmedTxs accId True `dbShouldBeE` []
      getConfirmedTxs accId False `dbShouldBeE` [txid' 0]
      txsPage ctx accId (Page 5 0) `dbShouldBeE` [emptyTxInfo]
      (dbInfo', change') <-
        repsertTxInfo
          btc
          ctx
          accId
          emptyTxInfo {txInfoBlockRef = Store.BlockRef 0 0}
      liftTest $ do
        change' `shouldBe` True
        dBTxInfoBlockRef dbInfo' `shouldBe` S.encode (Store.BlockRef 0 0)
        dBTxInfoConfirmed dbInfo' `shouldBe` True
        dBTxInfoCreated dbInfo' `shouldBe` dBTxInfoCreated dbInfo
      txsPage ctx accId (Page 5 0)
        `dbShouldBeE` [ emptyTxInfo
                          { txInfoBlockRef = Store.BlockRef 0 0,
                            txInfoConfirmations = 1
                          }
                      ]
      getConfirmedTxs accId True `dbShouldBeE` [txid' 0]
      getConfirmedTxs accId False `dbShouldBeE` []
      lift $ updateBest btcTest (bid' 0) 10
      lift $ updateBest btc (bid' 0) 20
      txsPage ctx accId (Page 5 0)
        `dbShouldBeE` [ emptyTxInfo
                          { txInfoBlockRef = Store.BlockRef 0 0,
                            txInfoConfirmations = 21
                          }
                      ]
      (dbInfo'', change'') <-
        repsertTxInfo
          btc
          ctx
          accId
          emptyTxInfo {txInfoBlockRef = Store.BlockRef 10 0}
      liftTest $ do
        change'' `shouldBe` True
        dBTxInfoBlockRef dbInfo'' `shouldBe` S.encode (Store.BlockRef 10 0)
        dBTxInfoConfirmed dbInfo'' `shouldBe` True
      txsPage ctx accId (Page 5 0)
        `dbShouldBeE` [ emptyTxInfo
                          { txInfoBlockRef = Store.BlockRef 10 0,
                            txInfoConfirmations = 11
                          }
                      ]

coinSpec :: Ctx -> Spec
coinSpec ctx =
  it "can manage coins" $ do
    runDBMemoryE $ do
      (accId, _) <- testNewAcc ctx "test"
      -- Insert a single coin
      let coin1 = coin' ctx (txid' 0, 0) Nothing (head extAddrs) 10
      (c, dbCoin) <- second head <$> refreshCoins btc accId extAddrs [coin1]
      liftTest $ do
        c `shouldBe` 1
        dBCoinAccountWallet dbCoin `shouldBe` DBWalletKey walletFPText
        dBCoinAccountDerivation dbCoin `shouldBe` "/44'/0'/0'"
        dBCoinAddress dbCoin `shouldBe` head extAddrsT
        dBCoinConfirmed dbCoin `shouldBe` False
        dBCoinLocked dbCoin `shouldBe` False
      -- Test JSON coins
      let jsonCoin1 = forceRight $ toJsonCoin btc Nothing dbCoin
      liftTest $
        jsonCoin1
          `shouldBe` JsonCoin
            { jsonCoinOutpoint = OutPoint (txid' 0) 0,
              jsonCoinAddress = head extAddrs,
              jsonCoinValue = 10,
              jsonCoinBlock = Store.MemRef 0,
              jsonCoinConfirmations = 0,
              jsonCoinLocked = False
            }
      coinPage btc accId (Page 5 0) `dbShouldBeE` [jsonCoin1]
      getSpendableCoins btc accId 0 `dbShouldBeE` [coin1]
      getSpendableCoins btc accId 1 `dbShouldBeE` []
      -- Nothing should happen when refreshing the same data
      refreshCoins btc accId extAddrs [coin1] `dbShouldBeE` (0, [])
      -- Confirm and update the coin
      let coin1' = coin1 {Store.block = Store.BlockRef 1 0} :: Store.Unspent
      refreshCoins btc accId extAddrs [coin1'] `dbShouldBeE` (1, [])
      getSpendableCoins btc accId 0 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 1 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 2 `dbShouldBeE` []
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin1
                          { jsonCoinBlock = Store.BlockRef 1 0,
                            jsonCoinConfirmations = 1
                          }
                      ]
      -- Best = 0
      lift $ updateBest btc (bid' 0) 0
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin1
                          { jsonCoinBlock = Store.BlockRef 1 0,
                            jsonCoinConfirmations = 1
                          }
                      ]
      getSpendableCoins btc accId 0 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 1 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 2 `dbShouldBeE` []
      -- Best = 1
      lift $ updateBest btc (bid' 0) 1
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin1
                          { jsonCoinBlock = Store.BlockRef 1 0,
                            jsonCoinConfirmations = 1
                          }
                      ]
      getSpendableCoins btc accId 0 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 1 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 2 `dbShouldBeE` []
      -- Best = 10
      lift $ updateBest btc (bid' 0) 10
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin1
                          { jsonCoinBlock = Store.BlockRef 1 0,
                            jsonCoinConfirmations = 10
                          }
                      ]
      getSpendableCoins btc accId 9 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 10 `dbShouldBeE` [coin1']
      getSpendableCoins btc accId 11 `dbShouldBeE` []
      let coin1'' = coin1 {Store.block = Store.BlockRef 10 0} :: Store.Unspent
      refreshCoins btc accId extAddrs [coin1''] `dbShouldBeE` (1, [])
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin1
                          { jsonCoinBlock = Store.BlockRef 10 0,
                            jsonCoinConfirmations = 1
                          }
                      ]
      getSpendableCoins btc accId 0 `dbShouldBeE` [coin1'']
      getSpendableCoins btc accId 1 `dbShouldBeE` [coin1'']
      getSpendableCoins btc accId 2 `dbShouldBeE` []
      -- Lock the coin
      lift $ setLockCoin (OutPoint (txid' 0) 0) True `dbShouldBe` 1
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin1
                          { jsonCoinBlock = Store.BlockRef 10 0,
                            jsonCoinConfirmations = 1,
                            jsonCoinLocked = True
                          }
                      ]
      getSpendableCoins btc accId 0 `dbShouldBeE` []
      lift $ setLockCoin (OutPoint (txid' 0) 0) True `dbShouldBe` 0
      lift $ setLockCoin (OutPoint (txid' 0) 0) False `dbShouldBe` 1
      lift $ setLockCoin (OutPoint (txid' 0) 0) False `dbShouldBe` 0
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin1
                          { jsonCoinBlock = Store.BlockRef 10 0,
                            jsonCoinConfirmations = 1,
                            jsonCoinLocked = False
                          }
                      ]
      getSpendableCoins btc accId 0 `dbShouldBeE` [coin1'']
      getSpendableCoins btc accId 1 `dbShouldBeE` [coin1'']
      getSpendableCoins btc accId 2 `dbShouldBeE` []
      -- Delete the coin
      refreshCoins btc accId extAddrs [] `dbShouldBeE` (1, [])
      getSpendableCoins btc accId 0 `dbShouldBeE` []
      coinPage btc accId (Page 5 0) `dbShouldBeE` []

checkFree :: Int -> Bool -> ExceptT String (DB IO) ()
checkFree i free =
  lift $
    ( dBAddressFree . entityVal . fromJust
        <$> P.getBy (UniqueAddress (intAddrsT !! i))
    )
      `dbShouldBe` free

pendingTxsSpec :: Ctx -> Config -> Spec
pendingTxsSpec ctx cfg =
  it "can manage pending transactions" $ do
    runDBMemoryE $ do
      (accId, _) <- testNewAcc ctx "test"
      lift $ updateBest btc (bid' 0) 0
      -- Build some test data coins/txs
      (dBAddressAddress <$> genExtAddress ctx cfg accId "")
        `dbShouldBeE` head extAddrsT
      (dBAddressAddress <$> nextFreeIntAddr ctx cfg accId)
        `dbShouldBeE` head intAddrsT
      checkFree 0 True
      lift $ setAddrsFree AddrBusy [head intAddrsT] `dbShouldBe` 1
      checkFree 0 False
      let fundTx1 = tx' ctx [(txid' 1, 0)] [(addr' 0, 20)]
          fundTx2 = tx' ctx [(txid' 1, 1)] [(iAddr' 0, 10)]
          coin1 = coin' ctx (txHash fundTx1, 0) (Just 0) (addr' 0) 20
          coin2 = coin' ctx (txHash fundTx2, 0) (Just 0) (iAddr' 0) 10
      -- Insert the dependent transactions
      lift $ insertRawTx fundTx1
      lift $ insertRawTx fundTx2
      -- Insert two coins in the database
      (c, dbCoins) <- refreshCoins btc accId (extAddrs <> intAddrs) [coin1, coin2]
      liftTest $ c `shouldBe` 2
      let jsonCoin1 = forceRight $ toJsonCoin btc Nothing (head dbCoins)
          jsonCoin2 = forceRight $ toJsonCoin btc Nothing (dbCoins !! 1)
      tsd <- buildTxSignData btc ctx cfg gen accId [(oAddr' 0, 8)] 0 0 False 1
      liftTest $
        tsd
          `shouldBe` TxSignData
            { txSignDataTx =
                tx'
                  ctx
                  [(txHash fundTx1, 0)]
                  [(iAddr' 1, 12), (oAddr' 0, 8)],
              txSignDataInputs = [fundTx1],
              txSignDataInputPaths = [Deriv :/ 0 :/ 0],
              txSignDataOutputPaths = [Deriv :/ 1 :/ 1],
              txSignDataSigned = False
            }
      -- Import the pending transaction
      checkFree 0 False
      checkFree 1 True
      let h1 = "db1085753d1d9b14005dd2951ff643803d4fa7ffa3bde6841dbb817958cb74e1"
      importPendingTx btc ctx accId tsd `dbShouldBeE` h1
      liftTest $ nosigTxHash (txSignDataTx tsd) `shouldBe` h1
      -- Check locked coins
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin2 {jsonCoinLocked = False},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [Just $ TxInfoPending h1 False False]
      -- Check address free status
      checkFree 0 False
      checkFree 1 False
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 2
      lift $
        shouldBeLeft' "chooseCoins: No solution found" $
          buildTxSignData btc ctx cfg gen accId [(oAddr' 1, 12)] 0 0 False 1
      lift $
        shouldBeLeft' "The transaction already exists" $
          importPendingTx btc ctx accId tsd
      -- Create second pending transaction
      tsd2 <- buildTxSignData btc ctx cfg gen accId [(oAddr' 1, 7)] 0 0 False 1
      liftTest $
        tsd2
          `shouldBe` TxSignData
            { txSignDataTx =
                tx'
                  ctx
                  [(txHash fundTx2, 0)]
                  [(iAddr' 2, 3), (oAddr' 1, 7)],
              txSignDataInputs = [fundTx2],
              txSignDataInputPaths = [Deriv :/ 1 :/ 0],
              txSignDataOutputPaths = [Deriv :/ 1 :/ 2],
              txSignDataSigned = False
            }
      -- Import the second transaction
      checkFree 2 True
      let h2 = "bb0e46f95f90bf0d2b0805655a77e632a0bd5088d5722d0055bc2862158ba747"
      importPendingTx btc ctx accId tsd2 `dbShouldBeE` h2
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin2 {jsonCoinLocked = True},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [ Just $ TxInfoPending h2 False False,
                        Just $ TxInfoPending h1 False False
                      ]
      checkFree 0 False
      checkFree 1 False
      checkFree 2 False
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 3
      lift $
        shouldBeLeft' "chooseCoins: No solution found" $
          buildTxSignData btc ctx cfg gen accId [(oAddr' 2, 1)] 0 0 False 1
      -- Delete first transaction
      deletePendingTx ctx h1 `dbShouldBeE` (1, 1)
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin2 {jsonCoinLocked = True},
                        jsonCoin1 {jsonCoinLocked = False}
                      ]
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [Just $ TxInfoPending h2 False False]
      checkFree 0 False
      checkFree 1 True
      checkFree 2 False
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 1
      -- Create transaction with no change
      tsd3 <- buildTxSignData btc ctx cfg gen accId [(oAddr' 2, 20)] 0 0 False 1
      liftTest $
        tsd3
          `shouldBe` TxSignData
            { txSignDataTx =
                tx'
                  ctx
                  [(txHash fundTx1, 0)]
                  [(oAddr' 2, 20)],
              txSignDataInputs = [fundTx1],
              txSignDataInputPaths = [Deriv :/ 0 :/ 0],
              txSignDataOutputPaths = [],
              txSignDataSigned = False
            }
      let h3 = "abe82b09dddb8abdb5490c3d404d34f2ff3c8f7b893bbe80f0af4018576e1f9e"
      importPendingTx btc ctx accId tsd3 `dbShouldBeE` h3
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin2 {jsonCoinLocked = True},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [ Just $ TxInfoPending h3 False False,
                        Just $ TxInfoPending h2 False False
                      ]
      checkFree 0 False
      checkFree 1 True
      checkFree 2 False
      (dBAddressIndex <$> nextFreeIntAddr ctx cfg accId) `dbShouldBeE` 1
      -- Import a signed transaction
      let resE = signWalletTx btc ctx tsd2 (fst $ keys ctx)
      liftTest $ resE `shouldSatisfy` isRight
      let tsd2' = fst $ forceRight resE
      liftTest $ txSignDataSigned tsd2' `shouldBe` True
      importPendingTx btc ctx accId tsd2' `dbShouldBeE` h2
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin2 {jsonCoinLocked = True},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [ Just $ TxInfoPending h3 False False,
                        Just $ TxInfoPending h2 True False
                      ]
      checkFree 0 False
      checkFree 1 True
      checkFree 2 False
      lift $ do
        shouldBeLeft' "The transaction already exists" $
          importPendingTx btc ctx accId tsd2'
        shouldBeLeft' "Can not replace a signed transaction with an unsigned one" $
          importPendingTx btc ctx accId tsd2
      -- Set tsd2 to online
      lift $ do
        setPendingTxOnline h2 `dbShouldBe` 1
        shouldBeLeft' "The transaction is already online" $
          importPendingTx btc ctx accId tsd2'
        -- Can not delete an online transaction
        shouldBeLeft $ deletePendingTx ctx h2
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [ Just $ TxInfoPending h3 False False,
                        Just $ TxInfoPending h2 True True
                      ]
      lift $ deletePendingTxOnline $ DBPendingTxKey $ txHashToHex h2
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin2 {jsonCoinLocked = True},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      ((txInfoPending <$>) <$> pendingTxPage ctx accId (Page 5 0))
        `dbShouldBeE` [Just $ TxInfoPending h3 False False]
      checkFree 0 False
      checkFree 1 True
      checkFree 2 False
      -- Check for other errors
      let fundTx3 = tx' ctx [(txid' 2, 0)] [(addr' 8, 30)]
          coin3 = coin' ctx (txHash fundTx3, 0) (Just 0) (addr' 8) 30
      lift $ do
        shouldBeLeft' "A coin referenced by the transaction is locked" $
          importPendingTx btc ctx accId $
            TxSignData
              { txSignDataTx =
                  tx'
                    ctx
                    [(txHash fundTx1, 0)]
                    [(oAddr' 3, 20)],
                txSignDataInputs = [fundTx3, fundTx1],
                txSignDataInputPaths = [Deriv :/ 0 :/ 0],
                txSignDataOutputPaths = [],
                txSignDataSigned = False
              }
        shouldBeLeft' "A coin referenced by the transaction does not exist" $
          importPendingTx btc ctx accId $
            TxSignData
              { txSignDataTx =
                  tx'
                    ctx
                    [(txHash fundTx3, 0)]
                    [(oAddr' 3, 20)],
                txSignDataInputs = [fundTx3, fundTx1],
                txSignDataInputPaths = [Deriv :/ 0 :/ 8],
                txSignDataOutputPaths = [],
                txSignDataSigned = False
              }
      (_, dbCoins3) <- refreshCoins btc accId (extAddrs <> intAddrs) [coin1, coin3]
      let jsonCoin3 = forceRight $ toJsonCoin btc Nothing (head dbCoins3)
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin3 {jsonCoinLocked = False},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      lift $
        shouldBeLeft' "Some referenced addresses do not exist" $
          importPendingTx btc ctx accId $
            TxSignData
              { txSignDataTx =
                  tx'
                    ctx
                    [(txHash fundTx3, 0)]
                    [(iAddr' 1, 5), (oAddr' 3, 25)],
                txSignDataInputs = [fundTx3],
                txSignDataInputPaths = [Deriv :/ 0 :/ 8],
                txSignDataOutputPaths = [Deriv :/ 1 :/ 1],
                txSignDataSigned = False
              }
      -- The transaction is not being rolled back so we roll it back manually
      replicateM_ 10 $ genExtAddress ctx cfg accId ""
      _ <- lift $ setLockCoin (jsonCoinOutpoint jsonCoin3) False
      coinPage btc accId (Page 5 0)
        `dbShouldBeE` [ jsonCoin3 {jsonCoinLocked = False},
                        jsonCoin1 {jsonCoinLocked = True}
                      ]
      checkFree 0 False
      checkFree 1 True
      checkFree 2 False
      lift $
        shouldBeLeft' "Some of the internal output addresses are not free" $
          importPendingTx btc ctx accId $
            TxSignData
              { txSignDataTx =
                  tx'
                    ctx
                    [(txHash fundTx3, 0)]
                    [(iAddr' 2, 1), (oAddr' 3, 25)],
                txSignDataInputs = [fundTx3],
                txSignDataInputPaths = [Deriv :/ 0 :/ 8],
                txSignDataOutputPaths = [Deriv :/ 1 :/ 2],
                txSignDataSigned = False
              }
