{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.AmountsSpec where

import           Data.Aeson.Types
import           Data.Either
import           Data.List
import qualified Data.Map.Strict                     as Map
import           Data.Word
import           Haskoin
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.Entropy
import           Network.Haskoin.Wallet.Signing
import           Network.Haskoin.Wallet.TestUtils
import           Numeric.Natural
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

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
            readAmount UnitBitcoin "1234567890.9" `shouldBe`
                Just 123456789090000000
            readAmount UnitBitcoin "1'234'567'890.9009" `shouldBe`
                Just 123456789090090000
            readAmount UnitBitcoin "1 234 567 890.9009" `shouldBe`
                Just 123456789090090000
            readAmount UnitBitcoin "1_234_567_890.9009" `shouldBe`
                Just 123456789090090000

instance Arbitrary AmountUnit where
    arbitrary = elements [UnitBitcoin, UnitBit, UnitSatoshi]

{-

walletSpec :: Spec
walletSpec = do
    diceSpec
    mnemonicSpec
    jsonSpec
    amountSpec
    buildSpec
    signingSpec
    httpSpec

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

buildSpec :: Spec
buildSpec =
    describe "Transaction builder" $
    it "can build a transaction" $ do
        let coins =
                [ WalletCoin
                      (OutPoint dummyTid1 0)
                      (PayPKHash $ getAddrHash160 $ just $ extAddrs ! 0)
                      100000000
                , WalletCoin
                      (OutPoint dummyTid1 1)
                      (PayPKHash $ getAddrHash160 $ just $ extAddrs ! 1)
                      200000000
                , WalletCoin
                      (OutPoint dummyTid1 2)
                      (PayPKHash $ getAddrHash160 $ just $ extAddrs ! 1)
                      300000000
                , WalletCoin
                      (OutPoint dummyTid1 3)
                      (PayPKHash $ getAddrHash160 $ just $ extAddrs ! 2)
                      400000000
                ]
            change = (just $ intAddrs ! 0, intDeriv :/ 0, 0)
            rcps =
                Map.fromList
                    [ (just $ othAddrs ! 0, 200000000)
                    , (just $ othAddrs ! 1, 200000000)
                    ]
            allAddrs = Map.fromList $ zip extAddrs $ fmap (extDeriv :/) [0 ..]
            resE = buildWalletTx btc allAddrs coins change rcps 314 10000
        (fmap prevOutput . txIn . (^. _1) <$> resE) `shouldBe`
            Right [OutPoint dummyTid1 2, OutPoint dummyTid1 3]
        (sum . fmap outValue . txOut . (^. _1) <$> resE) `shouldBe`
            Right 699871888
        ((^. _2) <$> resE) `shouldBe` Right [dummyTid1]
        ((^. _3) <$> resE) `shouldBe` Right [extDeriv :/ 1, extDeriv :/ 2]
        ((^. _4) <$> resE) `shouldBe` Right [intDeriv :/ 0]

signingSpec :: Spec
signingSpec =
    describe "Transaction signer" $ do
        it "can sign a simple transaction" $ do
            let fundTx = testTx' [(just $ extAddrs ! 0, 100000000)]
                newTx =
                    testTx
                        [(txHash fundTx, 0)]
                        [ (just $ othAddrs ! 0, 50000000)
                        , (just $ intAddrs ! 0, 40000000)
                        ]
                dat = TxSignData newTx [fundTx] [extDeriv :/ 0] [intDeriv :/ 0]
                xPrv = right $ signingKey btc pwd mnem 0
                (res, tx, isSigned) = right $ signWalletTx btc dat xPrv
            res `shouldBe`
                DetailedTx
                { detailedTxHash = Just $ txHash tx
                , detailedTxSize = Just $ length $ encodeBytes tx
                , detailedTxOutbound =
                      Map.fromList [(just $ othAddrs ! 0, 50000000)]
                , detailedTxNonStdOutputs = 0
                , detailedTxInbound =
                      Map.fromList
                          [ ( just $ intAddrs ! 0
                            , (40000000, Just $ intDeriv :/ 0))
                          ]
                , detailedTxMyInputs =
                      Map.fromList
                          [ ( just $ extAddrs ! 0
                            , (100000000, Just $ extDeriv :/ 0))
                          ]
                , detailedTxOtherInputs = Map.empty
                , detailedTxNonStdInputs = 0
                , detailedTxFee = Just 10000000
                , detailedTxHeight = Nothing
                , detailedTxBlockHash = Nothing
                }
            detailedTxType res `shouldBe` TxOutbound
            detailedTxAmount res `shouldBe` -60000000
            detailedTxFeeByte res `shouldBe` Just 44444.44
            isSigned `shouldBe` True
        it "can partially sign a transaction" $ do
            let fundTx =
                    testTx'
                        [ (just $ extAddrs ! 0, 100000000)
                        , (just $ extAddrs ! 2, 200000000)
                        ]
                newTx =
                    testTx
                        [(txHash fundTx, 0), (txHash fundTx, 1)]
                        [ (just $ othAddrs ! 1, 200000000)
                        , (just $ intAddrs ! 1, 50000000)
                        ]
                dat = TxSignData newTx [fundTx] [extDeriv :/ 2] [intDeriv :/ 1]
                xPrv = right $ signingKey btc pwd mnem 0
                (res, _, isSigned) = right $ signWalletTx btc dat xPrv
            res `shouldBe`
                DetailedTx
                { detailedTxHash = Nothing
                , detailedTxSize = Just $ fromIntegral $ guessTxSize 2 [] 2 0
                , detailedTxOutbound =
                      Map.fromList [(just $ othAddrs ! 1, 200000000)]
                , detailedTxNonStdOutputs = 0
                , detailedTxInbound =
                      Map.fromList
                          [ ( just $ intAddrs ! 1
                            , (50000000, Just $ intDeriv :/ 1))
                          ]
                , detailedTxMyInputs =
                      Map.fromList
                          [ ( just $ extAddrs ! 2
                            , (200000000, Just $ extDeriv :/ 2))
                          ]
                , detailedTxOtherInputs =
                      Map.fromList [(just $ extAddrs ! 0, 100000000)]
                , detailedTxNonStdInputs = 0
                , detailedTxFee = Just 50000000
                , detailedTxHeight = Nothing
                , detailedTxBlockHash = Nothing
                }
            detailedTxType res `shouldBe` TxOutbound
            detailedTxAmount res `shouldBe` -150000000
            detailedTxFeeByte res `shouldBe` Just 133689.84
            isSigned `shouldBe` False
        it "can send coins to your own wallet only" $ do
            let fundTx =
                    testTx'
                        [ (just $ extAddrs ! 0, 100000000)
                        , (just $ extAddrs ! 1, 200000000)
                        ]
                newTx =
                    testTx
                        [(txHash fundTx, 0), (txHash fundTx, 1)]
                        [ (just $ extAddrs ! 2, 200000000)
                        , (just $ intAddrs ! 0, 50000000)
                        ]
                dat =
                    TxSignData
                        newTx
                        [fundTx]
                        [extDeriv :/ 0, extDeriv :/ 1]
                        [intDeriv :/ 0, extDeriv :/ 2]
                xPrv = right $ signingKey btc pwd mnem 0
                (res, tx, isSigned) = right $ signWalletTx btc dat xPrv
            res `shouldBe`
                DetailedTx
                { detailedTxHash = Just $ txHash tx
                , detailedTxSize = Just $ length $ encodeBytes tx
                , detailedTxOutbound = Map.empty
                , detailedTxNonStdOutputs = 0
                , detailedTxInbound =
                      Map.fromList
                          [ ( just $ intAddrs ! 0
                            , (50000000, Just $ intDeriv :/ 0))
                          , ( just $ extAddrs ! 2
                            , (200000000, Just $ extDeriv :/ 2))
                          ]
                , detailedTxMyInputs =
                      Map.fromList
                          [ ( just $ extAddrs ! 1
                            , (200000000, Just $ extDeriv :/ 1))
                          , ( just $ extAddrs ! 0
                            , (100000000, Just $ extDeriv :/ 0))
                          ]
                , detailedTxOtherInputs = Map.empty
                , detailedTxNonStdInputs = 0
                , detailedTxFee = Just 50000000
                , detailedTxHeight = Nothing
                , detailedTxBlockHash = Nothing
                }
            detailedTxType res `shouldBe` TxInternal
            detailedTxAmount res `shouldBe` -50000000
            detailedTxFeeByte res `shouldBe` Just 134408.60
            isSigned `shouldBe` True
        it "can sign a complex transaction" $ do
            let fundTx1 =
                    testTx'
                        [ (just $ extAddrs ! 0, 100000000)
                        , (just $ extAddrs ! 1, 200000000)
                        , (just $ extAddrs ! 1, 300000000)
                        ]
                fundTx2 =
                    testTx'
                        [ (just $ extAddrs ! 3, 400000000)
                        , (just $ extAddrs ! 0, 500000000)
                        , (just $ extAddrs ! 2, 600000000)
                        ]
                newTx =
                    testTx
                        [ (txHash fundTx1, 0)
                        , (txHash fundTx1, 1)
                        , (txHash fundTx1, 2)
                        , (txHash fundTx2, 1)
                        , (txHash fundTx2, 2)
                        ]
                        [ (just $ othAddrs ! 0, 1000000000)
                        , (just $ othAddrs ! 1, 200000000)
                        , (just $ othAddrs ! 1, 100000000)
                        , (just $ intAddrs ! 0, 50000000)
                        , (just $ intAddrs ! 1, 100000000)
                        , (just $ intAddrs ! 0, 150000000)
                        ]
                dat =
                    TxSignData
                        newTx
                        [fundTx1, fundTx2]
                        [extDeriv :/ 0, extDeriv :/ 1, extDeriv :/ 2]
                        [intDeriv :/ 0, intDeriv :/ 1]
                xPrv = right $ signingKey btc pwd mnem 0
                (res, tx, isSigned) = right $ signWalletTx btc dat xPrv
            res `shouldBe`
                DetailedTx
                { detailedTxHash = Just $ txHash tx
                , detailedTxSize = Just $ length $ encodeBytes tx
                , detailedTxOutbound =
                      Map.fromList
                          [ (just $ othAddrs ! 0, 1000000000)
                          , (just $ othAddrs ! 1, 300000000)
                          ]
                , detailedTxNonStdOutputs = 0
                , detailedTxInbound =
                      Map.fromList
                          [ ( just $ intAddrs ! 0
                            , (200000000, Just $ intDeriv :/ 0))
                          , ( just $ intAddrs ! 1
                            , (100000000, Just $ intDeriv :/ 1))
                          ]
                , detailedTxMyInputs =
                      Map.fromList
                          [ ( just $ extAddrs ! 1
                            , (500000000, Just $ extDeriv :/ 1))
                          , ( just $ extAddrs ! 2
                            , (600000000, Just $ extDeriv :/ 2))
                          , ( just $ extAddrs ! 0
                            , (600000000, Just $ extDeriv :/ 0))
                          ]
                , detailedTxOtherInputs = Map.empty
                , detailedTxNonStdInputs = 0
                , detailedTxFee = Just 100000000
                , detailedTxHeight = Nothing
                , detailedTxBlockHash = Nothing
                }
            detailedTxType res `shouldBe` TxOutbound
            detailedTxAmount res `shouldBe` -1400000000
            detailedTxFeeByte res `shouldBe` Just 105152.47
            isSigned `shouldBe` True
        it "can sign a WIF transaction" $ do
            let fundTx = testTx' [(just $ extAddrs ! 0, 100000000)]
                newTx =
                    testTx
                        [(txHash fundTx, 0)]
                        [(just $ extAddrs ! 1, 50000000)]
                dat = TxSignData newTx [fundTx] [] [extDeriv :/ 1]
                prv = just $ fromWif btc $ toText $ wifKey 0
                (res, tx, isSigned) = right $ signSwipeTx btc dat [prv]
            res `shouldBe`
                DetailedTx
                { detailedTxHash = Just $ txHash tx
                , detailedTxSize = Just $ length $ encodeBytes tx
                , detailedTxOutbound =
                      Map.fromList [(just $ extAddrs ! 1, 50000000)]
                , detailedTxNonStdOutputs = 0
                , detailedTxInbound = Map.empty
                , detailedTxMyInputs =
                      Map.empty
                , detailedTxOtherInputs =
                      Map.fromList [(just $ extAddrs ! 0, 100000000)]
                , detailedTxNonStdInputs = 0
                , detailedTxFee = Just 50000000
                , detailedTxHeight = Nothing
                , detailedTxBlockHash = Nothing
                }
            detailedTxType res `shouldBe` TxOutbound
            detailedTxAmount res `shouldBe` 0
            detailedTxFeeByte res `shouldBe` Just 261780.10
            isSigned `shouldBe` True
        it "can show \"Tx is missing inputs from private keys\" error" $ do
            let fundTx1 = testTx' [(just $ extAddrs ! 1, 100000000)]
                newTx =
                    testTx
                        [(txHash fundTx1, 0)]
                        [ (just $ othAddrs ! 0, 50000000)
                        , (just $ intAddrs ! 0, 40000000)
                        ]
                dat =
                    TxSignData
                        newTx
                        [fundTx1]
                        [extDeriv :/ 0, extDeriv :/ 1]
                        [intDeriv :/ 0]
                xPrv = right $ signingKey btc pwd mnem 0
            signWalletTx btc dat xPrv `shouldBe`
                Left "Tx is missing inputs from private keys"
        it "can show \"Tx is missing change outputs\" error" $ do
            let fundTx = testTx' [(just $ extAddrs ! 0, 100000000)]
                newTx =
                    testTx
                        [(txHash fundTx, 0)]
                        [ (just $ othAddrs ! 0, 50000000)
                        , (just $ intAddrs ! 2, 20000000)
                        ]
                dat =
                    TxSignData
                        newTx
                        [fundTx]
                        [extDeriv :/ 0]
                        [intDeriv :/ 1, intDeriv :/ 2]
                xPrv = right $ signingKey btc pwd mnem 0
            signWalletTx btc dat xPrv `shouldBe` Left "Tx is missing change outputs"
        it "can show \"Referenced input transactions are missing\" error" $ do
            let fundTx1 = testTx' [(just $ extAddrs ! 0, 100000000)]
                fundTx2 = testTx' [(just $ extAddrs ! 1, 200000000)]
                newTx =
                    testTx
                        [(txHash fundTx1, 0), (txHash fundTx2, 0)]
                        [ (just $ othAddrs ! 0, 50000000)
                        , (just $ intAddrs ! 0, 40000000)
                        ]
                dat = TxSignData newTx [fundTx2] [extDeriv :/ 0] [intDeriv :/ 0]
                xPrv = right $ signingKey btc pwd mnem 0
            signWalletTx btc dat xPrv `shouldBe`
                Left "Referenced input transactions are missing"


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

{- Test Helpers -}

testTx :: [(TxHash, Word32)] -> [(Address, Word64)] -> Tx
testTx xs ys = Tx 1 txi txo [] 0
  where
    txi =
        fmap
            (\(h, p) ->
                 TxIn (OutPoint h p) (toByteString $ fromList [1]) maxBound)
            xs
    f = encodeOutputBS . PayPKHash
    txo = fmap (\(a, v) -> TxOut v $ f (getAddrHash160 a)) ys

testTx' :: [(Address, Word64)] -> Tx
testTx' = testTx [ (dummyTid1, 0) ]

dummyTid1 :: TxHash
dummyTid1 = dummyTxHash 1

dummyTxHash :: Word8 -> TxHash
dummyTxHash w =
    fromMaybe (error "Could not decode tx hash") $
    decodeBytes $ w `cons` replicate 31 0x00

dummyBlockHash :: Word8 -> BlockHash
dummyBlockHash w =
    fromMaybe (error "Could not decode block hash") $
    decodeBytes $ w `cons` replicate 31 0x00

just :: Maybe a -> a
just = fromMaybe (error "Got Nothing instead of Just")

right :: Either String a -> a
right = either (error "Got Left instead of Right") id

-- Test Constants

pwd :: String
pwd = "correct horse battery staple"

mnem :: String
mnem = "snow senior nerve virus fabric now fringe clip marble interest analyst can"

keys :: (XPrvKey, XPubKey)
keys =
    ( fromJust $
      xPrvImport btc
      "xprv9yHxeaLAZvxXb9VtJNesqk8avfN8misGAW9DUW9eacZJNqsfZxqKLmK5jfmvFideQqGesviJeagzSQYCuQySjgvt7TdfowKja5aJqbgyuNh"
    , fromJust $
      xPubImport btc
      "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah")

extAddrs :: [Address]
extAddrs =
    fromMaybe (error "extAddrs no parse") . stringToAddr btc <$>
    [ "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
    , "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"
    , "1Dg6Kg7kQuyiZz41HRWXKUWKRu6ZyEf1Nr"
    , "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1"
    , "1cWcYiGK7NwjPBJuKRqZxV4aymUnPu1mx"
    , "1MZuimSXigp8oqxkVUvZofqHNtVjdcdAqc"
    , "1JReTkpFnsrMqhSEJwUNZXPAyeTo2HQfnE"
    , "1Hx9xWAHhcjea5uJnyADktCfcLbuBnRnwA"
    , "1HXJhfiD7JFCGMFZnhKRsZxoPF7xDTqWXP"
    , "1MZpAt1FofY69B6fzooFxZqe6SdrVrC3Yw"
    ]

wifKey :: Natural -> String
wifKey i =
    fromText $
    toWif btc $
    wrapSecKey True $
    xPrvKey $ derivePath (extDeriv :/ fromIntegral i) (fst keys)

intAddrs :: [Address]
intAddrs =
    fromMaybe (error "intAddrs no parse") . stringToAddr btc <$>
    [ "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"
    , "1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6"
    , "1MZNPWwFwy2CqVgWBq6unPWBWrZTQ7WTnr"
    , "19XbPiR98wmoJQZ42K8pVMzdCwSXZBh7iz"
    , "1Gkn7EsphiaYuv6XXvG4Kyg3LSfqFMeXHX"
    , "14VkCGcLkNqUwRMVjpLEyodAhXvzUWLqPM"
    , "1PkyVUxPMGTLzUWNFNraMagACA1x3eD4CF"
    , "1M2mmDhWTjEuqPfUdaQH6XPsr5i29gx581"
    , "184JdZjasQUmNo2AimkbKAW2sxXMF9BAvK"
    , "13b1QVnWFRwCrjvhthj4JabpnJ4nyxbBqm"
    ]

othAddrs :: [Address]
othAddrs =
    fromMaybe (error "othAddrs no parse") . stringToAddr btc <$>
    [ "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
    , "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"
    , "191J7K3FaXXyM7C9ceSMRsJNF6aWCvvf1Q"
    , "1FVnYNLRdR5vQkynApupUez6ZfcDqsLHdj"
    , "1PmNJHnbk7Kct5FMqbEVRxqqR2mXVQKK5P"
    , "18CaQNcVwzUkE9KvwmMd6a5UWNgqJFEAh1"
    , "1M2Cv69B7LRud8su2wdd7HV2i6MrXqzdKP"
    , "19xYPmoJ2XV1vJnSkzsrXUJXCgKvPE3ri4"
    , "1N2JAKWVFAoKFEUci3tY3kvrGFY6poRgvm"
    , "15EANoYyJoo1J51ERdQzNwZCyhEtPfcP8g"
    ]
-}
