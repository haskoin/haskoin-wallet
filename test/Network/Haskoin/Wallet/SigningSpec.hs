{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.SigningSpec where

import Conduit (runResourceT)
import Control.Arrow (second)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (lift, liftIO)
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Serialize (encode)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Database.Persist.Sqlite (SqlBackend, withSqliteConn)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Network.Haskoin.Wallet.Database
import Network.Haskoin.Wallet.FileIO
import Network.Haskoin.Wallet.Signing
import Network.Haskoin.Wallet.TestUtils
import Network.Haskoin.Wallet.TxInfo
import Numeric.Natural (Natural)
import System.Random (StdGen, mkStdGen)
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    buildWalletTxSpec ctx
    signWalletTxSpec ctx

buildWalletTxSpec :: Ctx -> Spec
buildWalletTxSpec ctx =
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
          resE = buildWalletTx btc ctx gen rcps change coins 314 10000 False
      (fst <$> resE)
        `shouldBe` Right
          ( tx'
              ctx
              [(txid' 1, 2), (txid' 1, 1), (txid' 1, 0)] -- Greedy algorithm
              ((rcps !! 1) : (change, 199825416) : [head rcps])
          )
      (snd <$> resE)
        `shouldBe` Right [coins !! 2, coins !! 1, head coins]
    it "can fail to build a transaction if funds are insufficient" $ do
      let coins =
            [ coin' ctx (txid' 1, 0) (addr' 0) 100000000,
              coin' ctx (txid' 1, 1) (addr' 1) 200000000,
              coin' ctx (txid' 1, 2) (addr' 1) 300000000,
              coin' ctx (txid' 1, 3) (addr' 2) 400000000
            ]
          change = iAddr' 0
          rcps = [(oAddr' 0, 500000000), (oAddr' 1, 500000000)]
          resE = buildWalletTx btc ctx gen rcps change coins 1 10000 False
      resE `shouldBe` Left "chooseCoins: No solution found"
    it "will drop the change output if it is dust" $ do
      let coins =
            [ coin' ctx (txid' 1, 0) (addr' 0) 100000000,
              coin' ctx (txid' 1, 1) (addr' 1) 200000000,
              coin' ctx (txid' 1, 2) (addr' 1) 300000000,
              coin' ctx (txid' 1, 3) (addr' 2) 400000000
            ]
          change = iAddr' 0
          rcps = [(oAddr' 0, 500000000), (oAddr' 1, 499990000)]
          resE1 = buildWalletTx btc ctx gen rcps change coins 0 9999 False
          resE2 = buildWalletTx btc ctx gen rcps change coins 0 10000 False
          resE3 = buildWalletTx btc ctx gen rcps change coins 1 9999 False
      (fst <$> resE1)
        `shouldBe` Right
          ( tx'
              ctx
              [(txid' 1, 3), (txid' 1, 2), (txid' 1, 1), (txid' 1, 0)]
              ((rcps !! 1) : (change, 10000) : [head rcps])
          )
      (fst <$> resE2)
        `shouldBe` Right
          ( tx'
              ctx
              [(txid' 1, 3), (txid' 1, 2), (txid' 1, 1), (txid' 1, 0)]
              ((rcps !! 1) : [head rcps])
          )
      (fst <$> resE3)
        `shouldBe` Right
          ( tx'
              ctx
              [(txid' 1, 3), (txid' 1, 2), (txid' 1, 1), (txid' 1, 0)]
              ((rcps !! 1) : [head rcps])
          )
    it "will fail if sending dust" $ do
      let coins =
            [ coin' ctx (txid' 1, 0) (addr' 0) 100000000,
              coin' ctx (txid' 1, 1) (addr' 1) 200000000,
              coin' ctx (txid' 1, 2) (addr' 1) 300000000,
              coin' ctx (txid' 1, 3) (addr' 2) 400000000
            ]
          change = iAddr' 0
          rcps = [(oAddr' 0, 500000000), (oAddr' 1, 10000)]
          resE = buildWalletTx btc ctx gen rcps change coins 1 10000 False
      resE `shouldBe` Left "Recipient output is smaller than the dust value"
    it "can make the recipient pay for the fees" $ do
      let coins =
            [ coin' ctx (txid' 1, 0) (addr' 0) 100000000,
              coin' ctx (txid' 1, 1) (addr' 1) 200000000,
              coin' ctx (txid' 1, 2) (addr' 1) 300000000,
              coin' ctx (txid' 1, 3) (addr' 2) 400000000
            ]
          change = iAddr' 0
          rcps = [(oAddr' 0, 200000000), (oAddr' 1, 200000000)]
          resE = buildWalletTx btc ctx gen rcps change coins 314 10000 True
      (fst <$> resE)
        `shouldBe` Right
          ( tx'
              ctx
              [(txid' 1, 2), (txid' 1, 1), (txid' 1, 0)]
              ( (oAddr' 1, 199912708)
                  : (change, 200000000)
                  : [(oAddr' 0, 199912708)]
              )
          )
    it "fails when recipients cannot pay" $
      do
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
            resE1 = buildWalletTx btc ctx gen rcps1 change coins 314 10000 True
            resE2 = buildWalletTx btc ctx gen rcps2 change coins 314 10000 True
            resE3 = buildWalletTx btc ctx gen rcps3 change coins 314 10000 True
        resE1 `shouldBe` Left "Recipients can't pay for the fee"
        resE2 `shouldBe` Left "Recipient output is smaller than the dust value"
        (fst <$> resE3)
          `shouldBe` Right
            ( tx'
                ctx
                [(txid' 1, 2), (txid' 1, 1), (txid' 1, 0)]
                ((oAddr' 1, 10001) : (change, 199902707) : [(oAddr' 0, 399912708)])
            )

signWalletTxSpec :: Ctx -> Spec
signWalletTxSpec ctx =
  describe "Transaction signer" $ do
    it "can derive private signing keys" $ do
      let xPrvE = signingKey btc ctx mnemPass 0
          xPubE = deriveXPubKey ctx <$> xPrvE
      xPrvE `shouldBe` Right (fst $ keys ctx)
      xPubE `shouldBe` Right (snd $ keys ctx)
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
              False
          xPrv = fst $ keys ctx
      let resE = signWalletTx btc ctx dat xPrv
          (resDat, resTxInfo) = fromRight (error "fromRight") resE
          signedTx = txSignDataTx resDat
      txSignDataSigned resDat `shouldBe` True
      resTxInfo
        `shouldBe` TxInfo
          { txInfoHash =
              "e66b790c73d4e72fe13a07e247e4439bdea210cac2f947040e901f1e0ce59ac2",
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
            txInfoFeeByte = 44247,
            txInfoBlockRef = Store.MemRef 0,
            txInfoConfirmations = 0
          }
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
              False
          xPrv = fst $ keys ctx
      let resE = signWalletTx btc ctx dat xPrv
      (txInfoType . snd <$> resE) `shouldBe` Right TxInternal
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
              False
          xPrv = fst $ keys ctx
      let resE = signWalletTx btc ctx dat xPrv
      resE `shouldBe` Left "The transaction could not be signed"
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
              False
          xPrv = fst $ keys ctx
      let resE = signWalletTx btc ctx dat xPrv
      resE `shouldBe` Left "Referenced input transactions are missing"
    it "fails when private key derivations don't match the Tx inputs" $
      do
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
                False
            xPrv = fst $ keys ctx
        let resE = signWalletTx btc ctx dat xPrv
        resE `shouldBe` Left "Input derivations don't match the transaction inputs"
    it "fails when output derivations don't match the Tx outputs" $
      do
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
                False
            xPrv = fst $ keys ctx
        let resE = signWalletTx btc ctx dat xPrv
        resE `shouldBe` Left "Output derivations don't match the transaction outputs"

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

-- Use a predictable seed for tests
gen :: StdGen
gen = mkStdGen 0

mnemPass :: MnemonicPass
mnemPass =
  MnemonicPass
    "snow senior nerve virus fabric now \
    \fringe clip marble interest analyst can"
    "correct horse battery staple"

mnemPass2 :: MnemonicPass
mnemPass2 =
  MnemonicPass
    "boring auction demand filter frog accuse \
    \company exchange rely slogan trim typical"
    "correct horse battery staple"

walletFPText :: Text
walletFPText = "892eb8e4"

walletFPText2 :: Text
walletFPText2 = "807a5cfb"

walletFP :: Fingerprint
walletFP = forceRight $ textToFingerprint walletFPText

walletFP2 :: Fingerprint
walletFP2 = forceRight $ textToFingerprint walletFPText2

-- Keys for account 0
keys :: Ctx -> (XPrvKey, XPubKey)
keys ctx =
  ( fromJust $
      xPrvImport
        btc
        (fst keysT),
    fromJust $
      xPubImport
        btc
        ctx
        (snd keysT)
  )

-- Account /44'/0'/0' mnemonic 1
keysT :: (Text, Text)
keysT =
  ( "xprv9yHxeaLAZvxXb9VtJNesqk8avfN8misGAW9DUW9eacZJNqsfZxqKLmK5jfmvFideQqGesviJeagzSQYCuQySjgvt7TdfowKja5aJqbgyuNh",
    "xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah"
  )

-- Account /44'/0'/0' mnemonic 2
keysT2 :: (Text, Text)
keysT2 =
  ( "xprv9yXnZpEVdonEtT3strknsAgso5qq1cwRooo6susmzVmB5E2vvNw1KKRBgvwvNLxXdBHnkEN5R5uXi2QDs3tpkoBbL61NE6bnSbcrvH6keGa",
    "xpub6CX8yKmPUBLY6w8LztHoEJdcM7gKR5fHB2ihgJHPYqJ9x2N5TvFFs7jfYCX6So9oYyu6eLDTG5dbQWPncv1PYJtXLJ4cwymhoCpeTEmnZFZ"
  )

extAddrs :: [Address]
extAddrs = fromMaybe (error "extAddrs no parse") . textToAddr btc <$> extAddrsT

extAddrsT :: [Text]
extAddrsT =
  [ "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
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
intAddrs = fromMaybe (error "intAddrs no parse") . textToAddr btc <$> intAddrsT

intAddrsT :: [Text]
intAddrsT =
  [ "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn",
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
