{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -fno-warn-orphans #-}

module Haskoin.Wallet.VersionSpec where

import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Migration.SemVersion
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)

instance Arbitrary SemVersion where
  arbitrary =
    oneof
      [ VerMajor <$> arbitraryNatural
      , VerMinor <$> arbitraryNatural <*> arbitraryNatural
      , VerPatch <$> arbitraryNatural <*> arbitraryNatural <*> arbitraryNatural
      ]

spec :: Spec
spec =
  describe "Semantic Version" $ do
    prop "parseSemVersion . verString identity" $
      forAll (arbitrary :: Gen SemVersion) $ \v ->
        parseSemVersion (verString v) `shouldBe` v
    it "passes equality tests" $ do
      parseSemVersion "0" == parseSemVersion "0" `shouldBe` True
      parseSemVersion "0" == parseSemVersion "0.9" `shouldBe` True
      parseSemVersion "0" == parseSemVersion "0.9.0" `shouldBe` True
      parseSemVersion "0.9" == parseSemVersion "0" `shouldBe` True
      parseSemVersion "0.9.0" == parseSemVersion "0" `shouldBe` True
      parseSemVersion "0.9" == parseSemVersion "0.9" `shouldBe` True
      parseSemVersion "0.9" == parseSemVersion "0.9.1" `shouldBe` True
      parseSemVersion "0.9.1" == parseSemVersion "0.9" `shouldBe` True
      parseSemVersion "0.9.1" == parseSemVersion "0.9.1" `shouldBe` True
      parseSemVersion "0" == parseSemVersion "1" `shouldBe` False
      parseSemVersion "0.9" == parseSemVersion "1" `shouldBe` False
      parseSemVersion "0.9.1" == parseSemVersion "1" `shouldBe` False
      parseSemVersion "1" == parseSemVersion "0.9" `shouldBe` False
      parseSemVersion "1" == parseSemVersion "0.9.1" `shouldBe` False
      parseSemVersion "0.9" == parseSemVersion "0.8" `shouldBe` False
      parseSemVersion "0.9" == parseSemVersion "0.8.1" `shouldBe` False
      parseSemVersion "0.8.1" == parseSemVersion "0.9" `shouldBe` False
      parseSemVersion "0.8.1" == parseSemVersion "0.8.2" `shouldBe` False
    it "passes toMajor and toMinor tests" $ do
      toMajor (parseSemVersion "0") `shouldBe` VerMajor 0
      toMajor (parseSemVersion "0.8") `shouldBe` VerMajor 0
      toMajor (parseSemVersion "0.8.1") `shouldBe` VerMajor 0
      toMinor (parseSemVersion "0.8") `shouldBe` VerMinor 0 8
      toMinor (parseSemVersion "0.8.1") `shouldBe` VerMinor 0 8

