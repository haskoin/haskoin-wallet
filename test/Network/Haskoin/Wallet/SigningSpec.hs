{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.SigningSpec where

import Control.Arrow (second)
import qualified Data.ByteString as BS
import Data.Either
import Data.Maybe
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word
import Haskoin
import qualified Haskoin.Store.Data as Store
import Network.Haskoin.Wallet.AccountStore
import Network.Haskoin.Wallet.Signing
import Numeric.Natural
import System.Random
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    buildSpec ctx

buildSpec :: Ctx -> Spec
buildSpec ctx =
  describe "Transaction builder" $ do
    it "can build a transaction" $ do
      let coins =
            [ coin' ctx (1, 0) (addr' 0) 100000000,
              coin' ctx (1, 1) (addr' 1) 200000000,
              coin' ctx (1, 2) (addr' 1) 300000000,
              coin' ctx (1, 3) (addr' 2) 400000000
            ]
          change = iAddr' 0
          rcps = [(oAddr' 0, 200000000), (oAddr' 1, 200000000)]
          gen = mkStdGen 0 -- Use a predictable seed for tests
          resE = buildWalletTx btc ctx gen rcps change coins 314 10000 False
      assertEqual
        "Tx"
        ( Right $
            tx'
              ctx
              [(1, 2), (1, 1), (1, 0)] -- Greedy algorithm
              ((rcps !! 1) : (change, 199825416) : [rcps !! 0])
        )
        (fst <$> resE)
      assertEqual "Coins" (Right [coins !! 2, coins !! 1, coins !! 0]) (snd <$> resE)

    it "can fail to build a transaction if funds are insufficient" $ do
      let coins =
            [ coin' ctx (1, 0) (addr' 0) 100000000,
              coin' ctx (1, 1) (addr' 1) 200000000,
              coin' ctx (1, 2) (addr' 1) 300000000,
              coin' ctx (1, 3) (addr' 2) 400000000
            ]
          change = iAddr' 0
          rcps = [(oAddr' 0, 500000000), (oAddr' 1, 500000000)]
          gen = mkStdGen 0
          resE = buildWalletTx btc ctx gen rcps change coins 1 10000 False
      assertEqual "Tx" (Left "chooseCoins: No solution found") resE

    it "will drop the change output if it is dust" $ do
      let coins =
            [ coin' ctx (1, 0) (addr' 0) 100000000,
              coin' ctx (1, 1) (addr' 1) 200000000,
              coin' ctx (1, 2) (addr' 1) 300000000,
              coin' ctx (1, 3) (addr' 2) 400000000
            ]
          change = iAddr' 0
          rcps = [(oAddr' 0, 500000000), (oAddr' 1, 499990000)]
          gen = mkStdGen 0
          resE1 = buildWalletTx btc ctx gen rcps change coins 0 9999 False
          resE2 = buildWalletTx btc ctx gen rcps change coins 0 10000 False
          resE3 = buildWalletTx btc ctx gen rcps change coins 1 9999 False
      assertEqual
        "Tx"
        ( Right $
            tx'
              ctx
              [(1, 3), (1, 2), (1, 1), (1, 0)]
              ((rcps !! 1) : (change, 10000) : [rcps !! 0])
        )
        (fst <$> resE1)
      assertEqual
        "Tx"
        ( Right $
            tx'
              ctx
              [(1, 3), (1, 2), (1, 1), (1, 0)]
              ((rcps !! 1) : [rcps !! 0])
        )
        (fst <$> resE2)
      assertEqual
        "Tx"
        ( Right $
            tx'
              ctx
              [(1, 3), (1, 2), (1, 1), (1, 0)]
              ((rcps !! 1) : [rcps !! 0])
        )
        (fst <$> resE3)

    it "will fail if sending dust" $ do
      let coins =
            [ coin' ctx (1, 0) (addr' 0) 100000000,
              coin' ctx (1, 1) (addr' 1) 200000000,
              coin' ctx (1, 2) (addr' 1) 300000000,
              coin' ctx (1, 3) (addr' 2) 400000000
            ]
          change = iAddr' 0
          rcps = [(oAddr' 0, 500000000), (oAddr' 1, 10000)]
          gen = mkStdGen 0
          resE = buildWalletTx btc ctx gen rcps change coins 1 10000 False
      assertEqual "Tx" (Left "Recipient output is smaller than the dust value") resE

{-

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

-}

-- Test Helpers --

tx' :: Ctx -> [(Word8, Word32)] -> [(Address, Natural)] -> Tx
tx' ctx xs ys = Tx 1 txi txo [] 0
  where
    txi =
      fmap
        (\(h, p) -> TxIn (OutPoint (txid' h) p) BS.empty maxBound)
        xs
    f = marshal ctx . PayPKHash . (.hash160)
    txo = fmap (\(a, v) -> TxOut v $ f a) (second fromIntegral <$> ys)

txid' :: Word8 -> TxHash
txid' w =
  fromRight undefined $ S.decode $ w `BS.cons` BS.replicate 31 0x00

bid' :: Word8 -> BlockHash
bid' w =
  fromMaybe (error "Could not decode block hash") $
    hexToBlockHash $
      cs $
        w `BS.cons` BS.replicate 31 0x00

coin' :: Ctx -> (Word8, Word32) -> Address -> Natural -> Store.Unspent
coin' ctx (h, p) a v =
  Store.Unspent
    { Store.block = Store.MemRef 0,
      Store.outpoint = OutPoint (txid' h) p,
      Store.value = fromIntegral v,
      Store.script = marshal ctx $ PayPKHash $ (.hash160) a,
      Store.address = Just a
    }

addr' :: Int -> Address
addr' i = extAddrs !! i

iAddr' :: Int -> Address
iAddr' i = intAddrs !! i

oAddr' :: Int -> Address
oAddr' i = othAddrs !! i

-- Test Constants

pwd :: String
pwd = "correct horse battery staple"

mnem :: String
mnem = "snow senior nerve virus fabric now fringe clip marble interest analyst can"

keys :: Ctx -> (XPrvKey, XPubKey)
keys ctx =
  ( fromJust $
      xPrvImport
        btc
        "xprv9yHxeaLAZvxXb9VtJNesqk8avfN8misGAW9DUW9eacZJNqsfZxqKLmK5jfmvFideQqGesviJeagzSQYCuQySjgvt7TdfowKja5aJqbgyuNh",
    fromJust $
      xPubImport
        btc
        ctx
        "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah"
  )

extAddrs :: [Address]
extAddrs =
  fromMaybe (error "extAddrs no parse") . textToAddr btc
    <$> [ "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
          "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg",
          "1Dg6Kg7kQuyiZz41HRWXKUWKRu6ZyEf1Nr",
          "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1",
          "1cWcYiGK7NwjPBJuKRqZxV4aymUnPu1mx",
          "1MZuimSXigp8oqxkVUvZofqHNtVjdcdAqc",
          "1JReTkpFnsrMqhSEJwUNZXPAyeTo2HQfnE",
          "1Hx9xWAHhcjea5uJnyADktCfcLbuBnRnwA",
          "1HXJhfiD7JFCGMFZnhKRsZxoPF7xDTqWXP",
          "1MZpAt1FofY69B6fzooFxZqe6SdrVrC3Yw"
        ]

wifKey :: Ctx -> Natural -> Text
wifKey ctx i =
  toWif btc $
    wrapSecKey True $
      (.key) $
        derivePath ctx (extDeriv :/ fromIntegral i) (fst $ keys ctx)

intAddrs :: [Address]
intAddrs =
  fromMaybe (error "intAddrs no parse") . textToAddr btc
    <$> [ "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn",
          "1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6",
          "1MZNPWwFwy2CqVgWBq6unPWBWrZTQ7WTnr",
          "19XbPiR98wmoJQZ42K8pVMzdCwSXZBh7iz",
          "1Gkn7EsphiaYuv6XXvG4Kyg3LSfqFMeXHX",
          "14VkCGcLkNqUwRMVjpLEyodAhXvzUWLqPM",
          "1PkyVUxPMGTLzUWNFNraMagACA1x3eD4CF",
          "1M2mmDhWTjEuqPfUdaQH6XPsr5i29gx581",
          "184JdZjasQUmNo2AimkbKAW2sxXMF9BAvK",
          "13b1QVnWFRwCrjvhthj4JabpnJ4nyxbBqm"
        ]

othAddrs :: [Address]
othAddrs =
  fromMaybe (error "othAddrs no parse") . textToAddr btc
    <$> [ "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
          "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ",
          "191J7K3FaXXyM7C9ceSMRsJNF6aWCvvf1Q",
          "1FVnYNLRdR5vQkynApupUez6ZfcDqsLHdj",
          "1PmNJHnbk7Kct5FMqbEVRxqqR2mXVQKK5P",
          "18CaQNcVwzUkE9KvwmMd6a5UWNgqJFEAh1",
          "1M2Cv69B7LRud8su2wdd7HV2i6MrXqzdKP",
          "19xYPmoJ2XV1vJnSkzsrXUJXCgKvPE3ri4",
          "1N2JAKWVFAoKFEUci3tY3kvrGFY6poRgvm",
          "15EANoYyJoo1J51ERdQzNwZCyhEtPfcP8g"
        ]
