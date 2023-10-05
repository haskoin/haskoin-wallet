{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.SigningSpec where

import Control.Arrow (second)
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Serialize (encode)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Haskoin
  ( Address (hash160),
    BlockHash,
    Ctx,
    DerivPathI ((:/)),
    OutPoint (OutPoint),
    ScriptOutput (PayPKHash),
    SigInput (SigInput),
    Tx (Tx),
    TxHash,
    TxIn (TxIn),
    TxOut (TxOut),
    XPrvKey,
    XPubKey,
    btc,
    deriveXPubKey,
    hexToBlockHash,
    marshal,
    prepareContext,
    sigHashAll,
    textToAddr,
    txHash,
    xPrvImport,
    xPubImport,
  )
import qualified Haskoin.Store.Data as Store
import Network.Haskoin.Wallet.AccountStore (extDeriv, intDeriv)
import Network.Haskoin.Wallet.FileIO
  ( TxSignData (TxSignData, txSignDataSigned, txSignDataTx),
  )
import Network.Haskoin.Wallet.Signing
  ( buildWalletTx,
    signWalletTx,
    signingKey,
  )
import Network.Haskoin.Wallet.TxInfo
  ( MyInputs (MyInputs),
    MyOutputs (MyOutputs),
    TxInfo
      ( TxInfo,
        txInfoAmount,
        txInfoBlockRef,
        txInfoConfirmations,
        txInfoFee,
        txInfoFeeByte,
        txInfoId,
        txInfoMyInputs,
        txInfoMyOutputs,
        txInfoNonStdInputs,
        txInfoNonStdOutputs,
        txInfoOtherInputs,
        txInfoOtherOutputs,
        txInfoSize,
        txInfoType
      ),
    TxType (TxDebit, TxInternal),
  )
import Numeric.Natural (Natural)
import System.Random (mkStdGen)
import Test.HUnit (assertBool, assertEqual)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    buildSpec ctx
    signingSpec ctx

buildSpec :: Ctx -> Spec
buildSpec ctx =
  describe "Transaction builder" $ do
    it "can build a transaction" $ do
      let coins =
            [ coin' ctx (txid' 1, 0) (addr' 0) 100000000,
              coin' ctx (txid' 1, 1) (addr' 1) 200000000,
              coin' ctx (txid' 1, 2) (addr' 1) 300000000,
              coin' ctx (txid' 1, 3) (addr' 2) 400000000
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
              [(txid' 1, 2), (txid' 1, 1), (txid' 1, 0)] -- Greedy algorithm
              ((rcps !! 1) : (change, 199825416) : [head rcps])
        )
        (fst <$> resE)
      assertEqual
        "Coins"
        (Right [coins !! 2, coins !! 1, head coins])
        (snd <$> resE)
    it "can fail to build a transaction if funds are insufficient" $ do
      let coins =
            [ coin' ctx (txid' 1, 0) (addr' 0) 100000000,
              coin' ctx (txid' 1, 1) (addr' 1) 200000000,
              coin' ctx (txid' 1, 2) (addr' 1) 300000000,
              coin' ctx (txid' 1, 3) (addr' 2) 400000000
            ]
          change = iAddr' 0
          rcps = [(oAddr' 0, 500000000), (oAddr' 1, 500000000)]
          gen = mkStdGen 0
          resE = buildWalletTx btc ctx gen rcps change coins 1 10000 False
      assertEqual "Tx" (Left "chooseCoins: No solution found") resE
    it "will drop the change output if it is dust" $ do
      let coins =
            [ coin' ctx (txid' 1, 0) (addr' 0) 100000000,
              coin' ctx (txid' 1, 1) (addr' 1) 200000000,
              coin' ctx (txid' 1, 2) (addr' 1) 300000000,
              coin' ctx (txid' 1, 3) (addr' 2) 400000000
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
              [(txid' 1, 3), (txid' 1, 2), (txid' 1, 1), (txid' 1, 0)]
              ((rcps !! 1) : (change, 10000) : [head rcps])
        )
        (fst <$> resE1)
      assertEqual
        "Tx"
        ( Right $
            tx'
              ctx
              [(txid' 1, 3), (txid' 1, 2), (txid' 1, 1), (txid' 1, 0)]
              ((rcps !! 1) : [head rcps])
        )
        (fst <$> resE2)
      assertEqual
        "Tx"
        ( Right $
            tx'
              ctx
              [(txid' 1, 3), (txid' 1, 2), (txid' 1, 1), (txid' 1, 0)]
              ((rcps !! 1) : [head rcps])
        )
        (fst <$> resE3)
    it "will fail if sending dust" $ do
      let coins =
            [ coin' ctx (txid' 1, 0) (addr' 0) 100000000,
              coin' ctx (txid' 1, 1) (addr' 1) 200000000,
              coin' ctx (txid' 1, 2) (addr' 1) 300000000,
              coin' ctx (txid' 1, 3) (addr' 2) 400000000
            ]
          change = iAddr' 0
          rcps = [(oAddr' 0, 500000000), (oAddr' 1, 10000)]
          gen = mkStdGen 0
          resE = buildWalletTx btc ctx gen rcps change coins 1 10000 False
      assertEqual
        "Tx"
        (Left "Recipient output is smaller than the dust value")
        resE
    it "can make the recipient pay for the fees" $ do
      let coins =
            [ coin' ctx (txid' 1, 0) (addr' 0) 100000000,
              coin' ctx (txid' 1, 1) (addr' 1) 200000000,
              coin' ctx (txid' 1, 2) (addr' 1) 300000000,
              coin' ctx (txid' 1, 3) (addr' 2) 400000000
            ]
          change = iAddr' 0
          rcps = [(oAddr' 0, 200000000), (oAddr' 1, 200000000)]
          gen = mkStdGen 0
          resE = buildWalletTx btc ctx gen rcps change coins 314 10000 True
      assertEqual
        "Tx"
        ( Right $
            tx'
              ctx
              [(txid' 1, 2), (txid' 1, 1), (txid' 1, 0)]
              ( (oAddr' 1, 199912708)
                  : (change, 200000000)
                  : [(oAddr' 0, 199912708)]
              )
        )
        (fst <$> resE)
    it "fails when recipients cannot pay" $ do
      let coins =
            [ coin' ctx (txid' 1, 0) (addr' 0) 100000000,
              coin' ctx (txid' 1, 1) (addr' 1) 200000000,
              coin' ctx (txid' 1, 2) (addr' 1) 300000000,
              coin' ctx (txid' 1, 3) (addr' 2) 400000000
            ]
          change = iAddr' 0
          rcps1 = [(oAddr' 0, 400000000), (oAddr' 1, 87291)] -- fee is 2*87292
          rcps2 = [(oAddr' 0, 400000000), (oAddr' 1, 87292)]
          rcps3 = [(oAddr' 0, 400000000), (oAddr' 1, 97293)]
          gen = mkStdGen 0
          resE1 = buildWalletTx btc ctx gen rcps1 change coins 314 10000 True
          resE2 = buildWalletTx btc ctx gen rcps2 change coins 314 10000 True
          resE3 = buildWalletTx btc ctx gen rcps3 change coins 314 10000 True
      assertEqual "Tx" (Left "Recipients can't pay for the fee") resE1
      assertEqual
        "Tx"
        (Left "Recipient output is smaller than the dust value")
        resE2
      assertEqual
        "Tx"
        ( Right $
            tx'
              ctx
              [(txid' 1, 2), (txid' 1, 1), (txid' 1, 0)]
              ((oAddr' 1, 10001) : (change, 199902707) : [(oAddr' 0, 399912708)])
        )
        (fst <$> resE3)

signingSpec :: Ctx -> Spec
signingSpec ctx =
  describe "Transaction signer" $ do
    it "can derive private signing keys" $ do
      let xPrvE = signingKey btc ctx pwd mnem 0
          xPubE = deriveXPubKey ctx <$> xPrvE
      assertEqual "XPrvKey" (Right $ fst $ keys ctx) xPrvE
      assertEqual "XPubKey" (Right $ snd $ keys ctx) xPubE
    it "can sign a simple transaction" $ do
      let fundTx = tx' ctx [(txid' 1, 0)] [(addr' 0, 100000000)]
          newTx =
            tx'
              ctx
              [(txHash fundTx, 0)]
              [(oAddr' 0, 50000000), (iAddr' 0, 40000000)]
          dat =
            TxSignData
              newTx
              [fundTx]
              [extDeriv :/ 0]
              [intDeriv :/ 0]
              0
              False
              btc
          xPrv = fst $ keys ctx
      let resE = signWalletTx ctx dat xPrv
          (resDat, resTxInfo) = fromRight (error "fromRight") resE
          signedTx = txSignDataTx resDat
      assertEqual
        "TxInfo"
        ( TxInfo
            { txInfoId = txHash signedTx,
              txInfoType = TxDebit,
              txInfoAmount = -60000000,
              txInfoMyOutputs =
                Map.fromList [(iAddr' 0, MyOutputs 40000000 (intDeriv :/ 0))],
              txInfoOtherOutputs = Map.fromList [(oAddr' 0, 50000000)],
              txInfoNonStdOutputs = [],
              txInfoMyInputs =
                Map.fromList
                  [ ( addr' 0,
                      MyInputs
                        100000000
                        (extDeriv :/ 0)
                        [ SigInput
                            (PayPKHash (addr' 0).hash160)
                            100000000
                            (OutPoint (txHash fundTx) 0)
                            sigHashAll
                            Nothing
                        ]
                    )
                  ],
              txInfoOtherInputs = Map.empty,
              txInfoNonStdInputs = [],
              txInfoSize = fromIntegral $ BS.length $ encode signedTx,
              txInfoFee = 10000000,
              txInfoFeeByte = 44247.79,
              txInfoBlockRef = Store.MemRef 0,
              txInfoConfirmations = 0
            }
        )
        resTxInfo
      assertBool "The transaction was not signed" (txSignDataSigned resDat)
    it "can set the correct TxInternal transaction types" $ do
      let fundTx =
            tx' ctx [(txid' 1, 0)] [(addr' 0, 100000000), (addr' 1, 200000000)]
          newTx =
            tx'
              ctx
              [(txHash fundTx, 0), (txHash fundTx, 1)]
              [(iAddr' 0, 50000000), (addr' 2, 200000000)]
          dat =
            TxSignData
              newTx
              [fundTx]
              [extDeriv :/ 0, extDeriv :/ 1]
              [intDeriv :/ 0, extDeriv :/ 2]
              0
              False
              btc
          xPrv = fst $ keys ctx
      let resE = signWalletTx ctx dat xPrv
      assertEqual "TxInternal" (Right TxInternal) $ txInfoType . snd <$> resE
    it "fails when an input is not signed" $ do
      let fundTx =
            tx' ctx [(txid' 1, 0)] [(addr' 0, 100000000), (addr' 1, 100000000)]
          newTx =
            tx'
              ctx
              [(txHash fundTx, 0), (txHash fundTx, 1)]
              [(oAddr' 0, 50000000), (iAddr' 0, 40000000)]
          dat =
            TxSignData
              newTx
              [fundTx]
              [extDeriv :/ 0] -- We omit derivation 1
              [intDeriv :/ 0]
              0
              False
              btc
          xPrv = fst $ keys ctx
      let resE = signWalletTx ctx dat xPrv
      assertEqual "TxSignData" (Left "The transaction could not be signed") resE
    it "fails when referenced input transactions are missing" $ do
      let fundTx = tx' ctx [(txid' 1, 0)] [(addr' 0, 100000000)]
          newTx =
            tx'
              ctx
              [(txHash fundTx, 0), (txHash fundTx, 1)]
              [(oAddr' 0, 50000000), (iAddr' 0, 40000000)]
          dat =
            TxSignData
              newTx
              [fundTx]
              [extDeriv :/ 0, extDeriv :/ 1] -- 1 is missing in fundTx
              [intDeriv :/ 0]
              0
              False
              btc
          xPrv = fst $ keys ctx
      let resE = signWalletTx ctx dat xPrv
      assertEqual
        "TxSignData"
        (Left "Referenced input transactions are missing")
        resE
    it "fails when private key derivations don't match the Tx inputs" $ do
      let fundTx =
            tx' ctx [(txid' 1, 0)] [(addr' 0, 100000000), (addr' 1, 100000000)]
          newTx =
            tx'
              ctx
              [(txHash fundTx, 0), (txHash fundTx, 1)]
              [(oAddr' 0, 50000000), (iAddr' 0, 40000000)]
          dat =
            TxSignData
              newTx
              [fundTx]
              [extDeriv :/ 1, extDeriv :/ 2] -- 1 and 2 instead of 0 and 1
              [intDeriv :/ 0]
              0
              False
              btc
          xPrv = fst $ keys ctx
      let resE = signWalletTx ctx dat xPrv
      assertEqual
        "TxSignData"
        (Left "Private key derivations don't match the transaction inputs")
        resE
    it "fails when output derivations don't match the Tx outputs" $ do
      let fundTx = tx' ctx [(txid' 1, 0)] [(addr' 0, 100000000)]
          newTx =
            tx'
              ctx
              [(txHash fundTx, 0)]
              [(oAddr' 0, 50000000), (iAddr' 0, 40000000)]
          dat =
            TxSignData
              newTx
              [fundTx]
              [extDeriv :/ 0]
              [intDeriv :/ 1] -- 1 instead of 0
              0
              False
              btc
          xPrv = fst $ keys ctx
      let resE = signWalletTx ctx dat xPrv
      assertEqual
        "TxSignData"
        (Left "Output derivations don't match the transaction outputs")
        resE

-- Test Helpers --

tx' :: Ctx -> [(TxHash, Word32)] -> [(Address, Natural)] -> Tx
tx' ctx xs ys = Tx 1 txi txo [] 0
  where
    txi =
      fmap
        (\(h, p) -> TxIn (OutPoint h p) BS.empty maxBound)
        xs
    f = marshal ctx . PayPKHash . (.hash160)
    txo = fmap (\(a, v) -> TxOut v $ f a) (Control.Arrow.second fromIntegral <$> ys)

txid' :: Word8 -> TxHash
txid' w =
  fromRight undefined $ S.decode $ w `BS.cons` BS.replicate 31 0x00

bid' :: Word8 -> BlockHash
bid' w =
  fromMaybe (error "Could not decode block hash") $
    hexToBlockHash $
      cs $
        w `BS.cons` BS.replicate 31 0x00

coin' :: Ctx -> (TxHash, Word32) -> Address -> Natural -> Store.Unspent
coin' ctx (h, p) a v =
  Store.Unspent
    { Store.block = Store.MemRef 0,
      Store.outpoint = OutPoint h p,
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

pwd :: Text
pwd = "correct horse battery staple"

mnem :: Text
mnem =
  "snow senior nerve virus fabric now fringe clip marble interest analyst can"

-- Keys for account 0
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
