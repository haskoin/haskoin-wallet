{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.AmountsSpec where

import Network.Haskoin.Wallet.Amounts
import Network.Haskoin.Wallet.TestUtils
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

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
      readAmount UnitBitcoin "1234567890.9"
        `shouldBe` Just 123456789090000000
      readAmount UnitBitcoin "1'234'567'890.9009"
        `shouldBe` Just 123456789090090000
      readAmount UnitBitcoin "1 234 567 890.9009"
        `shouldBe` Just 123456789090090000
      readAmount UnitBitcoin "1_234_567_890.9009"
        `shouldBe` Just 123456789090090000

instance Arbitrary AmountUnit where
  arbitrary = elements [UnitBitcoin, UnitBit, UnitSatoshi]

{-

jsonSpec :: Spec
jsonSpec =
    describe "Json encoding and decoding" $ do
        it "can encode TxSignData type" $
            property $
            forAll (arbitraryTxSignData btc) $ \t ->
                Just t `shouldBe` decodeJson (encodeJson t)
        it "can encode AccountStore type" $
            property $
            forAll (arbitraryAccountStore btc) $ \t ->
                Just t `shouldBe`
                  (parseMaybe (accountStoreFromJSON btc)
                    =<< decodeJson (encodeJson t))

mnemonic:
        -- it "can derive a prvkey from a mnemonic" $ do
        --     let Right xprv = signingKey btc (cs pwd) (cs mnem) 0
        --     xprv `shouldBe` fst keys
        -- it "can derive a pubkey from a mnemonic" $ do
        --     let Right xpub = deriveXPubKey <$> signingKey btc pwd mnem 0
        --     xpub `shouldBe` snd keys
        -- it "can derive addresses from a mnemonic" $ do
        --     let addrs = take 5 $ derivePathAddrs (snd keys) extDeriv 0
        --     fmap fst addrs `shouldBe` take 5 extAddrs

httpSpec :: Spec
httpSpec =
    describe "HTTP service (online test)" $ do
        let addr1 =
                fromMaybe (error "bad address") $
                stringToAddr btc "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"
        it "can receive balance (online test)" $ do
            res <- httpBalance btc [addr1]
            res `shouldSatisfy` (>= 6687607502)
        it "can receive coins (online test)" $ do
            res <- httpUnspent btc [addr1]
            length res `shouldSatisfy` (>= 1290)
            outputAddress btc . snd <$>
                res `shouldSatisfy`
                all (== Right addr1)
        it "can receive a transaction information (online test)" $ do
            res <- httpDetailedTx btc [addr1]
            length res `shouldSatisfy` (>= 1290)
            let res1 = head $ nonEmpty_ res
                as =
                    Map.keys (detailedTxInbound res1) <>
                    Map.keys (detailedTxMyInputs res1)
            as `shouldSatisfy` (addr1 `elem`)
        it "can receive a transaction (online test)" $ do
            let tid =
                    "0e3e2357e806b6cdb1f70b54c3a3a17b6714ee1f0e68bebb44a74b1efd512098"
            res <- httpRawTxs btc [tid]
            (txHash <$> res) `shouldBe` [tid]
        it "can get the best block height (online test)" $ do
            res <- httpBestHeight btc
            res `shouldSatisfy` (> 500000)

-}
