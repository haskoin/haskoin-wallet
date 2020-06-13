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
import           Numeric.Natural
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

jsonVals :: [JsonBox]
jsonVals =
    [ JsonBox (arbitrary :: Gen AccountStore)
    ]

spec :: Spec
spec = do
    testIdentity [] [] jsonVals []
    let pub1 = deriveXPubKey $ makeXPrvKey "1"
        acc1 e i = AccountStore pub1 e i (Deriv :| 44 :| 0 :| 0) btc
    describe "AccountMap" $ do
        let s0 = Map.empty
        it "Reading an account from an empty map fails" $ do
            let res1 = run s0 $ getAccountStore Nothing
                res2 = run s0 $ getAccountStore (Just "acc1")
            isLeft res1 && isLeft res2
        let (Right (Commit s1)) =
                exec s0 $ insertAccountStore "acc1" (emptyAccountStore btc pub1)
        it "Can insert a new account" $ s1 == Map.fromList [("acc1", acc1 0 0)]
        it "Can read the new account" $ do
            let a1 = run s1 $ getAccountStore Nothing
                a2 = run s1 $ getAccountStore (Just "acc1")
            (fst <$> a1) == Right ("acc1", acc1 0 0) &&
                (fst <$> a2) == Right ("acc1", acc1 0 0)
        it "Can read the new account by derivation" $ do
            let a = run s1 $ getAccountStoreByDeriv btc 0
            (fst <$> a) == Right ("acc1", acc1 0 0)
        it "Reading an invalid account fails" $ do
            let a = run s1 $ getAccountStore (Just "acc2")
            isLeft a

exec :: AccountMap
    -> StateT AccountMap (Except String) a
    -> Either String (Commit AccountMap)
exec s a = runExcept $ execAccountMapT a s

run :: AccountMap
    -> StateT AccountMap (Except String) a
    -> Either String (a, Commit AccountMap)
run s a = runExcept $ runAccountMapT a s

instance Arbitrary AccountStore where
    arbitrary =
        AccountStore
            <$> (snd <$> arbitraryXPubKey)
            <*> genNatural
            <*> genNatural
            <*> arbitraryHardPath
            <*> arbitraryNetwork

