{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.AccountStoreSpec where

import Control.Monad (replicateM_, void)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, gets)
import Data.Default (def)
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Haskoin.Crypto
  ( Ctx,
    DerivPathI (Deriv, (:|)),
    HardPath,
    derivePath,
    deriveXPubKey,
    makeXPrvKey,
  )
import Haskoin.Network (btc)
import Haskoin.Util (prepareContext)
import Haskoin.Util.Arbitrary
  ( IdentityTests (marshalJsonTests),
    MarshalJsonBox (MarshalJsonBox),
    arbitraryHardPath,
    arbitraryNetwork,
    arbitraryXPubKey,
    testIdentity,
  )
import Network.Haskoin.Wallet.AccountStore
  ( AccountMap (AccountMap),
    AccountStore (AccountStore),
    Commit (NoCommit, commitValue),
    accountStoreAccount,
    emptyAccountStore,
    execAccountMapT,
    execAccountStoreT,
    genExtAddress,
    genIntAddress,
    getAccountStore,
    getAccountStoreByDeriv,
    insertAccountStore,
    renameAccountStore,
    runAccountMapT,
    runAccountStoreT,
  )
import Network.Haskoin.Wallet.TestUtils (forceRight, genNatural)
import Test.HUnit (assertBool, assertEqual)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen)

identityTests :: Ctx -> IdentityTests
identityTests ctx =
  def
    { marshalJsonTests =
        [MarshalJsonBox $ (ctx,) <$> arbitraryAccountStore ctx]
    }

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    testIdentity $ identityTests ctx
    let mast = makeXPrvKey "1"
        pub1 =
          deriveXPubKey ctx $
            derivePath ctx (Deriv :| 44 :| 0 :| 0 :: HardPath) mast
        pub2 =
          deriveXPubKey ctx $
            derivePath ctx (Deriv :| 44 :| 0 :| 1 :: HardPath) mast
        acc1 e i =
          AccountStore pub1 e i (Deriv :| 44 :| 0 :| 0) Haskoin.Network.btc
        acc2 e i =
          AccountStore pub2 e i (Deriv :| 44 :| 0 :| 1) Haskoin.Network.btc
    describe "AccountMap" $ do
      let s0 = AccountMap Map.empty
      it "Reading an account from an empty map fails" $ do
        let res1 = run s0 $ getAccountStore Nothing
            res2 = run s0 $ getAccountStore (Just "acc1")
        assertBool "res1" $ isLeft res1
        assertBool "res2" $ isLeft res2
      let s1 =
            commitValue $
              forceRight $
                exec s0 $
                  insertAccountStore
                    "acc1"
                    (emptyAccountStore Haskoin.Network.btc pub1)
      it "Can insert a new account" $
        s1 == AccountMap (Map.fromList [("acc1", acc1 0 0)])
      it "Can read the new account" $ do
        let a0 = run s1 $ getAccountStore Nothing
            a1 = run s1 $ getAccountStore (Just "acc1")
        assertEqual "acc1 a0" (Right ("acc1", acc1 0 0)) (fst <$> a0)
        assertEqual "acc1 a1" (Right ("acc1", acc1 0 0)) (fst <$> a1)
      it "Can read the new account by derivation" $ do
        let a = run s1 $ getAccountStoreByDeriv Haskoin.Network.btc 0
        assertEqual "acc1" (Right ("acc1", acc1 0 0)) (fst <$> a)
      it "Reading an invalid account fails" $ do
        let a = run s1 $ getAccountStore (Just "acc2")
        assertBool "acc2" $ isLeft a
      it "Inserting duplicate accounts fails" $ do
        let a1 =
              exec s1 $
                insertAccountStore
                  "acc1"
                  (emptyAccountStore Haskoin.Network.btc pub2)
            a2 =
              exec s1 $
                insertAccountStore
                  "acc2"
                  (emptyAccountStore Haskoin.Network.btc pub1)
        assertBool "acc1 a1" $ isLeft a1
        assertBool "acc2 a2" $ isLeft a2
      let s2 =
            commitValue $
              forceRight $
                exec s1 $
                  insertAccountStore
                    "acc2"
                    (emptyAccountStore Haskoin.Network.btc pub2)
      it "Can add a second account" $
        assertEqual
          "map"
          (AccountMap (Map.fromList [("acc1", acc1 0 0), ("acc2", acc2 0 0)]))
          s2
      it "Can query both accounts" $ do
        let a0 = run s2 $ getAccountStore Nothing
            a1 = run s2 $ getAccountStore (Just "acc1")
            a2 = run s2 $ getAccountStore (Just "acc2")
        assertBool "acc1 a0" $ isLeft a0
        assertEqual "acc1 a1" (Right ("acc1", acc1 0 0)) (fst <$> a1)
        assertEqual "acc2 a2" (Right ("acc2", acc2 0 0)) (fst <$> a2)
      it "Can rename accounts" $ do
        let a =
              commitValue $
                forceRight $
                  exec s2 $
                    renameAccountStore "acc1" "acc3"
        assertEqual
          "Rename map"
          (AccountMap (Map.fromList [("acc3", acc1 0 0), ("acc2", acc2 0 0)]))
          a
      it "Can query the account derivations" $ do
        let ((_, a1), _) = forceRight $ run s2 $ getAccountStore (Just "acc1")
            ((_, a2), _) = forceRight $ run s2 $ getAccountStore (Just "acc2")
            (d1, _) = forceRight $ runAcc a1 $ gets accountStoreAccount
            (d2, _) = forceRight $ runAcc a2 $ gets accountStoreAccount
        assertEqual "Derivation d1" (Right 0) d1
        assertEqual "Derivation d2" (Right 1) d2
      it "Produces correct Commit/Nocommit" $ do
        let c1 = forceRight $ exec s2 $ getAccountStore (Just "acc1")
            c2 = forceRight $ exec s2 $ renameAccountStore "acc1" "acc3"
            c3 = forceRight $ execAcc (acc1 0 0) $ gets accountStoreAccount
            c4 =
              forceRight $
                execAcc (acc1 0 0) $
                  Control.Monad.void (genExtAddress ctx)
        assertBool "Commit c1" $ not $ isCommit c1
        assertBool "Commit c2" $ isCommit c2
        assertBool "Commit c3" $ not $ isCommit c3
        assertBool "Commit c4" $ isCommit c4
      it "Updates indices when generating addresses" $ do
        let a1 =
              commitValue $
                forceRight $
                  execAcc (acc1 0 0) $
                    Control.Monad.replicateM_ 10 (genExtAddress ctx)
                      >> Control.Monad.replicateM_ 7 (genIntAddress ctx)
        assertEqual "Gen addrs" (acc1 10 7) a1

isCommit :: Commit a -> Bool
isCommit (NoCommit _) = False
isCommit _ = True

exec ::
  AccountMap ->
  StateT AccountMap (Except String) a ->
  Either String (Commit AccountMap)
exec s a = runExcept $ execAccountMapT a s

run ::
  AccountMap ->
  StateT AccountMap (Except String) a ->
  Either String (a, Commit AccountMap)
run s a = runExcept $ runAccountMapT a s

execAcc ::
  AccountStore ->
  StateT AccountStore (Except String) a ->
  Either String (Commit AccountStore)
execAcc s a = runExcept $ execAccountStoreT a s

runAcc ::
  AccountStore ->
  StateT AccountStore (Except String) a ->
  Either String (a, Commit AccountStore)
runAcc s a = runExcept $ runAccountStoreT a s

arbitraryAccountStore :: Ctx -> Gen AccountStore
arbitraryAccountStore ctx =
  AccountStore
    <$> arbitraryXPubKey ctx
    <*> genNatural
    <*> genNatural
    <*> arbitraryHardPath
    <*> arbitraryNetwork
