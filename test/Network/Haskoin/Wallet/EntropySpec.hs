{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.EntropySpec where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Either
import Data.Functor.Identity
import Data.Maybe
import Data.Text (Text)
import Haskoin
import Haskoin.Util
import Network.Haskoin.Wallet.AccountStore
import Network.Haskoin.Wallet.Entropy
import Network.Haskoin.Wallet.Signing
import Network.Haskoin.Wallet.TestUtils
import Numeric (showIntAtBase)
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = prepareContext $ \ctx -> do
  diceSpec
  entropySpec
  mnemonicSpec ctx

diceSpec :: Spec
diceSpec =
  describe "Base6 Encoding" $ do
    it "can encode a Word8 to Base6" $ do
      let vs = [6, 1, 2, 3, 4, 5]
          xs = [[a, b, c] | a <- vs, b <- vs, c <- vs]
          ys = [[a, b] | a <- vs, b <- vs]
          as = [0x00 .. 0xd7]
          bs = [0xd8 .. 0xfb]
          cs = [0xfc .. 0xff]
      forM_ (zip as xs) $ \(a, b) -> word8ToBase6 a `shouldBe` b
      forM_ (zip bs ys) $ \(a, b) -> word8ToBase6 a `shouldBe` b
      forM_ cs $ \c -> word8ToBase6 c `shouldBe` []
      -- Checking a few fixed values
      word8ToBase6 0x00 `shouldBe` [6, 6, 6]
      word8ToBase6 0x01 `shouldBe` [6, 6, 1]
      word8ToBase6 0xd7 `shouldBe` [5, 5, 5]
      word8ToBase6 0xd8 `shouldBe` [6, 6]
      word8ToBase6 0xd9 `shouldBe` [6, 1]
      word8ToBase6 0xfb `shouldBe` [5, 5]
    it "can decode base6 to Word8" $ do
      base6ToWord8 [6] [] `shouldBe` ([], [False, False])
      base6ToWord8 [1] [] `shouldBe` ([], [False, True])
      base6ToWord8 [2] [] `shouldBe` ([], [True, False])
      base6ToWord8 [3] [] `shouldBe` ([], [True, True])
      base6ToWord8 [4] [] `shouldBe` ([], [False])
      base6ToWord8 [5] [] `shouldBe` ([], [True])
      base6ToWord8 [5, 6] [] `shouldBe` ([], [True, False, False])
      base6ToWord8 [6, 6, 6, 5] []
        `shouldBe` ([], [False, False, False, False, False, False, True])
      base6ToWord8 [6, 6, 6, 6] [] `shouldBe` ([0x00], [])
      base6ToWord8 [6, 6, 6, 1] [] `shouldBe` ([0x01], [])
      base6ToWord8 [3, 3, 3, 3] [] `shouldBe` ([0xff], [])
      base6ToWord8 [4, 3, 3, 3, 3] [] `shouldBe` ([0xff], [False])
      base6ToWord8 [1, 1, 1, 1, 3, 3, 3, 3] [] `shouldBe` ([0x55, 0xff], [])

entropySpec :: Spec
entropySpec = do
  it "can mix entropy" $ do
    mixEntropy BS.empty (BS.pack [0x00]) `shouldSatisfy` isLeft
    mixEntropy (BS.pack [0x00]) BS.empty `shouldSatisfy` isLeft
    mixEntropy (BS.pack [0x00, 0x00]) (BS.pack [0x00])
      `shouldSatisfy` isLeft
    mixEntropy (BS.pack [0x00]) (BS.pack [0x00, 0x00])
      `shouldSatisfy` isLeft
    mixEntropy (BS.pack [0x00]) (BS.pack [0x00])
      `shouldBe` Right (BS.pack [0x00])
    mixEntropy (BS.pack [0x00]) (BS.pack [0xff])
      `shouldBe` Right (BS.pack [0xff])
    mixEntropy (BS.pack [0xff]) (BS.pack [0x00])
      `shouldBe` Right (BS.pack [0xff])
    mixEntropy (BS.pack [0xff]) (BS.pack [0xff])
      `shouldBe` Right (BS.pack [0x00])
    mixEntropy (BS.pack [0xaa]) (BS.pack [0x55])
      `shouldBe` Right (BS.pack [0xff])
    mixEntropy (BS.pack [0x55, 0xaa]) (BS.pack [0xaa, 0x55])
      `shouldBe` Right (BS.pack [0xff, 0xff])
    mixEntropy (BS.pack [0x7a, 0x54]) (BS.pack [0xd3, 0x8e])
      `shouldBe` Right (BS.pack [0xa9, 0xda])

-- https://github.com/iancoleman/bip39/issues/58
mnemonicSpec :: Ctx -> Spec
mnemonicSpec ctx =
  describe "Mnemonic API" $ do
    it "Can derive iancoleman issue 58" $ do
      let m = "fruit wave dwarf banana earth journey tattoo true farm silk olive fence"
          p = "banana"
          Right xpub = deriveXPubKey ctx <$> signingKey btc ctx p m 0
          (addr0, _) = derivePathAddr ctx xpub extDeriv 0
      addrToText btc addr0 `shouldBe` Just "17rxURoF96VhmkcEGCj5LNQkmN9HVhWb7F"
    it "Passes the bip44 test vectors" $
      mapM_ (testBip44Vector ctx) bip44Vectors

testBip44Vector :: Ctx -> (Text, Text, Text, Text) -> Assertion
testBip44Vector ctx (mnem, pass, addr0, addr1) = do
  assertEqual "Addr External" (Just addr0) (addrToText btc $ fst $ r (genExtAddress ctx))
  assertEqual "Addr Internal" (Just addr1) (addrToText btc $ fst $ r (genIntAddress ctx))
  where
    Right k = signingKey btc ctx pass mnem 0
    p = deriveXPubKey ctx k
    s = emptyAccountStore btc p
    r a = fst $ runIdentity $ runAccountStoreT a s

-- (Mnemonic, BIP38 password, external 0/0, internal 1/0)
bip44Vectors :: [(Text, Text, Text, Text)]
bip44Vectors =
  [ ( "modify truck lens identify brief coffee \
      \gather volcano fatal together muscle elephant",
      "",
      "1KiWbwzHhwH2KdLyreGuJq36SP2pPeGeim",
      "1BY9FShyC6rnEUdPAVQTX37TFZqEkR1enQ"
    ),
    ( "modify truck lens identify brief coffee \
      \gather volcano fatal together muscle elephant",
      "password",
      "1Ha18XQ74YfCnuUAjGTF9rnbS7pm1aCzW4",
      "17SounFZ11k8urp9Pee2FNYjzGU5qwLUfT"
    ),
    ( "modify truck lens identify brief coffee \
      \gather volcano fatal together muscle elephant",
      "Hello world",
      "1KpHPSa9eMohQznkddzmHT9AeMAscYe29Y",
      "16WJrKzdik1HgnoiNiLUW2uGDqry2fSGVx"
    ),
    ( "fiscal gadget drastic coconut awful crime \
      \during common salmon manage random cost \
      \evil owner city",
      "",
      "131dALC6WxwTZzTsHUsifAJ8bkQdDEeeqJ",
      "1JHCmBLzDARCdDgn2WwJQr88tGW8V3EyKj"
    ),
    ( "host mind elephant tone sound apple \
      \service tomato subject attend motion stick \
      \fuel fan rail bamboo tree build",
      "",
      "15nbJ6oHvYiapKu1vBYmyQzVSTjiJNcfuV",
      "12rMvadbhRxFaUP4FoRUmABa1JEPLrrqHL"
    ),
    ( "flight skill wisdom mixture patch dirt \
      \trouble behind chair glad detect swarm \
      \swap truly cruise medal walnut glide \
      \wrestle route defy",
      "",
      "12uDDJU7BJUxJomzbFt5BW3GnukGZz6Y5t",
      "1Gv62T4UkLxJ6dnsmNXENMgjBhLzEzJ1Sc"
    ),
    ( "use turtle trap pause spin venue \
      \hazard hope slot april cattle fork \
      \stand finish arrest nasty acoustic clog \
      \entire course universe evil desert produce",
      "",
      "19fJdt5HjJUrmnvKM8fTS43o9aK3NmFZMT",
      "17Kmofo8651DdydBS7zVVy9JLkixkxj6SK"
    ),
    ( "use turtle trap pause spin venue \
      \hazard hope slot april cattle fork \
      \stand finish arrest nasty acoustic clog \
      \entire course universe evil desert produce",
      "nMCm#Qe7u-*LG^99KS%hSNZJh6w&e&w4S&7HA-22^7vDE#F\
      \@=hNZZZ$M=ZSYG42J8P+U+3sf$xS6YpJf7DemaU#fLEVBW8\
      \!FmugPj=K=E*KDmUuQh_2%7K2W8@RJ4?Pua2HFCb8$@ZB^p\
      \s=T=PNFy5APBBdaz_eTq&r&v3hp6P-tTfvNP9J+xfQ-FKZS\
      \7_?g+P*9MTkpjy*K5=8y+y9db#Sv+b*5bfmV8?@YKrf#AEH\
      \7!ARqBXMhS7d%GYAxB%RG",
      "1L1oKPYJ9j3EFX5CcFXz1d3kkAW9Hoh5ic",
      "1J7jpmP7NVoryofiVSCKMMvMitkCP3U6Qj"
    )
  ]
