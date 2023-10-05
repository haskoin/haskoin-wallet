{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.AmountsSpec where

import Network.Haskoin.Wallet.Amounts
  ( AmountUnit (..),
    readAmount,
    readIntegerAmount,
    showAmount,
    showIntegerAmount,
  )
import Network.Haskoin.Wallet.TestUtils (genNatural)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), elements, forAll)

spec :: Spec
spec =
  describe "Amount parser" $ do
    prop "readAmount . showAmount identity" $
      forAll genNatural $ \n unit ->
        readAmount unit (showAmount unit n) `shouldBe` Just n
    prop "readIntegerAmount . showIntegerAmount identity" $ \i unit ->
      readIntegerAmount unit (showIntegerAmount unit i) `shouldBe` Just i
    it "Satoshi amounts vectors" $ do
      readAmount UnitSatoshi "0" `shouldBe` Just 0
      readAmount UnitSatoshi "0000" `shouldBe` Just 0
      readAmount UnitSatoshi "0.0" `shouldBe` Nothing
      readAmount UnitSatoshi "8" `shouldBe` Just 8
      readAmount UnitSatoshi "100" `shouldBe` Just 100
      readAmount UnitSatoshi "1234567890" `shouldBe` Just 1234567890
      readAmount UnitSatoshi "1'234'567'890" `shouldBe` Just 1234567890
      readAmount UnitSatoshi "1 234 567 890" `shouldBe` Just 1234567890
      readAmount UnitSatoshi "1_234_567_890" `shouldBe` Just 1234567890
    it "Bit amounts vectors" $ do
      readAmount UnitBit "0" `shouldBe` Just 0
      readAmount UnitBit "0000" `shouldBe` Just 0
      readAmount UnitBit "0.0" `shouldBe` Just 0
      readAmount UnitBit "0.00" `shouldBe` Just 0
      readAmount UnitBit "0.000" `shouldBe` Nothing
      readAmount UnitBit "0.10" `shouldBe` Just 10
      readAmount UnitBit "0.1" `shouldBe` Just 10
      readAmount UnitBit "0.01" `shouldBe` Just 1
      readAmount UnitBit "1" `shouldBe` Just 100
      readAmount UnitBit "100" `shouldBe` Just 10000
      readAmount UnitBit "100.00" `shouldBe` Just 10000
      readAmount UnitBit "100.01" `shouldBe` Just 10001
      readAmount UnitBit "1234567890.9" `shouldBe` Just 123456789090
      readAmount UnitBit "1'234'567'890.90" `shouldBe` Just 123456789090
      readAmount UnitBit "1 234 567 890.90" `shouldBe` Just 123456789090
      readAmount UnitBit "1_234_567_890.90" `shouldBe` Just 123456789090
    it "Bitcoin amounts vectors" $ do
      readAmount UnitBitcoin "0" `shouldBe` Just 0
      readAmount UnitBitcoin "0000" `shouldBe` Just 0
      readAmount UnitBitcoin "0.0" `shouldBe` Just 0
      readAmount UnitBitcoin "0.00000000" `shouldBe` Just 0
      readAmount UnitBitcoin "0.000000000" `shouldBe` Nothing
      readAmount UnitBitcoin "0.1" `shouldBe` Just 10000000
      readAmount UnitBitcoin "0.1000" `shouldBe` Just 10000000
      readAmount UnitBitcoin "0.10000000" `shouldBe` Just 10000000
      readAmount UnitBitcoin "0.100000000" `shouldBe` Nothing
      readAmount UnitBitcoin "1" `shouldBe` Just 100000000
      readAmount UnitBitcoin "100" `shouldBe` Just 10000000000
      readAmount UnitBitcoin "1234567890.9" `shouldBe` Just 123456789090000000
      readAmount UnitBitcoin "1'234'567'890.9009"
        `shouldBe` Just 123456789090090000
      readAmount UnitBitcoin "1 234 567 890.9009"
        `shouldBe` Just 123456789090090000
      readAmount UnitBitcoin "1_234_567_890.9009"
        `shouldBe` Just 123456789090090000

instance Arbitrary AmountUnit where
  arbitrary = elements [UnitBitcoin, UnitBit, UnitSatoshi]
