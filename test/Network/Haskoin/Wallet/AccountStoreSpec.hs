{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.AccountStoreSpec where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Either
import           Data.Functor.Identity
import qualified Data.Map.Strict                     as Map
import           Data.Word
import           Haskoin.Constants
import           Haskoin.Keys
import           Haskoin.Util.Arbitrary
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Signing
import           Network.Haskoin.Wallet.TestUtils
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

jsonVals :: [JsonBox]
jsonVals =
    [ JsonBox (arbitrary :: Gen AccountStore)
    ]

spec :: Spec
spec = do
    testIdentity [] [] jsonVals []
    let mast = makeXPrvKey "1"
        pub1 = deriveXPubKey $ derivePath (Deriv :| 44 :| 0 :| 0 :: HardPath) mast
        pub2 = deriveXPubKey $ derivePath (Deriv :| 44 :| 0 :| 1 :: HardPath) mast
        acc1 e i = AccountStore pub1 e i (Deriv :| 44 :| 0 :| 0) btc
        acc2 e i = AccountStore pub2 e i (Deriv :| 44 :| 0 :| 1) btc
    describe "AccountMap" $ do
        let s0 = Map.empty
        it "Reading an account from an empty map fails" $ do
            let res1 = run s0 $ getAccountStore Nothing
                res2 = run s0 $ getAccountStore (Just "acc1")
            assertBool "res1" $ isLeft res1
            assertBool "res2" $ isLeft res2
        let (Right (Commit s1)) =
                exec s0 $ insertAccountStore "acc1" (emptyAccountStore btc pub1)
        it "Can insert a new account" $ s1 == Map.fromList [("acc1", acc1 0 0)]
        it "Can read the new account" $ do
            let a0 = run s1 $ getAccountStore Nothing
                a1 = run s1 $ getAccountStore (Just "acc1")
            assertEqual "acc1 a0"  (Right ("acc1", acc1 0 0)) (fst <$> a0)
            assertEqual "acc1 a1"  (Right ("acc1", acc1 0 0)) (fst <$> a1)
        it "Can read the new account by derivation" $ do
            let a = run s1 $ getAccountStoreByDeriv btc 0
            assertEqual "acc1" (Right ("acc1", acc1 0 0)) (fst <$> a)
        it "Reading an invalid account fails" $ do
            let a = run s1 $ getAccountStore (Just "acc2")
            assertBool "acc2" $ isLeft a
        it "Inserting duplicate accounts fails" $ do
            let a1 = exec s1 $ insertAccountStore "acc1" (emptyAccountStore btc pub2)
                a2 = exec s1 $ insertAccountStore "acc2" (emptyAccountStore btc pub1)
            assertBool "acc1 a1" $ isLeft a1
            assertBool "acc2 a2" $ isLeft a2
        let (Right (Commit s2)) =
                exec s1 $ insertAccountStore "acc2" (emptyAccountStore btc pub2)
        it "Can add a second account" $
            assertEqual "map" (Map.fromList [("acc1", acc1 0 0), ("acc2", acc2 0 0)]) s2
        it "Can query both accounts" $ do
            let a0 = run s2 $ getAccountStore Nothing
                a1 = run s2 $ getAccountStore (Just "acc1")
                a2 = run s2 $ getAccountStore (Just "acc2")
            assertBool "acc1 a0" $ isLeft a0
            assertEqual "acc1 a1"  (Right ("acc1", acc1 0 0)) (fst <$> a1)
            assertEqual "acc2 a2"  (Right ("acc2", acc2 0 0)) (fst <$> a2)
        it "Can rename accounts" $ do
            let (Right (Commit a)) = exec s2 $ renameAccountStore "acc1" "acc3"
            assertEqual "Rename map"
                (Map.fromList [("acc3", acc1 0 0), ("acc2", acc2 0 0)]) a
        it "Can query the account derivations" $ do
            let Right ((_, a1), _) = run s2 $ getAccountStore (Just "acc1")
                Right ((_, a2), _) = run s2 $ getAccountStore (Just "acc2")
                Right (d1, _) = runAcc a1 $ gets accountStoreAccount
                Right (d2, _) = runAcc a2 $ gets accountStoreAccount
            assertEqual "Derivation d1" (Right 0) d1
            assertEqual "Derivation d2" (Right 1) d2
        it "Produces correct Commit/Nocommit" $ do
            let Right c1 = exec s2 $ getAccountStore (Just "acc1")
                Right c2 = exec s2 $ renameAccountStore "acc1" "acc3"
                Right c3 = execAcc (acc1 0 0) $ gets accountStoreAccount
                Right c4 = execAcc (acc1 0 0) $ void genExtAddress
            assertBool "Commit c1" $ not $ isCommit c1
            assertBool "Commit c2" $ isCommit c2
            assertBool "Commit c3" $ not $ isCommit c3
            assertBool "Commit c4" $ isCommit c4
        it "Updates indices when generating addresses" $ do
            let Right (Commit a1) = execAcc (acc1 0 0) $
                    replicateM_ 10 genExtAddress >> replicateM_ 7 genIntAddress
            assertEqual "Gen addrs" (acc1 10 7) a1
            
isCommit :: Commit a -> Bool
isCommit (NoCommit _) = False
isCommit _ = True
            
exec :: AccountMap
    -> StateT AccountMap (Except String) a
    -> Either String (Commit AccountMap)
exec s a = runExcept $ execAccountMapT a s

run :: AccountMap
    -> StateT AccountMap (Except String) a
    -> Either String (a, Commit AccountMap)
run s a = runExcept $ runAccountMapT a s

execAcc ::
       AccountStore
    -> StateT AccountStore (Except String) a
    -> Either String (Commit AccountStore)
execAcc s a = runExcept $ execAccountStoreT a s

runAcc ::
       AccountStore
    -> StateT AccountStore (Except String) a
    -> Either String (a, Commit AccountStore)
runAcc s a = runExcept $ runAccountStoreT a s

instance Arbitrary AccountStore where
    arbitrary =
        AccountStore
            <$> (snd <$> arbitraryXPubKey)
            <*> genNatural
            <*> genNatural
            <*> arbitraryHardPath
            <*> arbitraryNetwork

