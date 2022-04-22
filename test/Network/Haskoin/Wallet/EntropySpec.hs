{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.EntropySpec where

import           Data.Aeson.Types
import qualified Data.ByteString                     as BS
import           Data.Either
import           Data.Functor.Identity
import           Data.List
import qualified Data.Map.Strict                     as Map
import           Data.Maybe
import           Data.String.Conversions             (cs)
import           Data.Text                           (Text)
import           Data.Word
import           Haskoin
import           Haskoin.Util.Arbitrary
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.Entropy
import           Network.Haskoin.Wallet.Signing
import           Network.Haskoin.Wallet.TestUtils
import           Numeric                             (readInt, showIntAtBase)
import           Numeric.Natural
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

spec :: Spec
spec = do
    diceSpec
    mnemonicSpec

diceSpec :: Spec
diceSpec =
    describe "Base6" $ do
        it "Base6 decoding vectors" $ do
            decodeBase6 mempty `shouldBe` Just mempty
            decodeBase6 "6" `shouldBe` Just (BS.singleton 0x00)
            decodeBase6 "666" `shouldBe` Just (BS.singleton 0x00)
            decodeBase6 "666666666666" `shouldBe` Just (BS.singleton 0x00)
            decodeBase6 "1" `shouldBe` Just (BS.singleton 0x01)
            decodeBase6 "661" `shouldBe` Just (BS.singleton 0x01)
            decodeBase6 "5" `shouldBe` Just (BS.singleton 0x05)
            decodeBase6 "16" `shouldBe` Just (BS.singleton 0x06)
            decodeBase6 "6615" `shouldBe` Just (BS.singleton 0x0B)
            decodeBase6 "6645" `shouldBe` Just (BS.singleton 0x1D)
            decodeBase6 "166" `shouldBe` Just (BS.singleton 0x24)
            decodeBase6 "1666" `shouldBe` Just (BS.singleton 0xD8)
            decodeBase6 "1661" `shouldBe` Just (BS.singleton 0xD9)
            decodeBase6 "66456666" `shouldBe` Just (BS.pack [0x92, 0xD0])
            decodeBase6 "111111111111111111111111111111111" `shouldBe`
                decodeHex "07E65FDC244B0133333333"
            decodeBase6 "55555555555555555555555555555555" `shouldBe`
                decodeHex "06954FE21E3E80FFFFFFFF"
            decodeBase6
                "161254362643213454433626115643626632163246612666332415423213664" `shouldBe`
                decodeHex "0140F8D002341BDF377F1723C9EB6C7ACFF134581C"
            decodeBase6 "0" `shouldBe` Nothing
            decodeBase6 "7" `shouldBe` Nothing
            decodeBase6 "a" `shouldBe` Nothing
            decodeBase6 "60" `shouldBe` Nothing
            decodeBase6 "06" `shouldBe` Nothing
            decodeBase6 "01" `shouldBe` Nothing
            decodeBase6 "10" `shouldBe` Nothing
            decodeBase6 "505" `shouldBe` Nothing
        prop "Base6 decoding property" $
            forAll genNatural $ \n ->
                let s = showIntAtBase 6 b6 n ""
                    bs = integerToBS $ fromIntegral n
                 in decodeBase6 s `shouldBe` Just bs
        it "can calculate the required dice rolls for a given entropy" $ do
            requiredRolls 16 `shouldBe` 49
            requiredRolls 20 `shouldBe` 61
            requiredRolls 24 `shouldBe` 74
            requiredRolls 28 `shouldBe` 86
            requiredRolls 32 `shouldBe` 99
        it "can convert dice rolls to entropy" $ do
            diceToEntropy 16 "" `shouldSatisfy` isLeft
            diceToEntropy 16 (replicate 48 '6') `shouldSatisfy` isLeft
            diceToEntropy 16 (replicate 50 '6') `shouldSatisfy` isLeft
            diceToEntropy 16 (replicate 48 '6' <> "7") `shouldSatisfy` isLeft
            diceToEntropy 16 (replicate 48 '6' <> "0") `shouldSatisfy` isLeft
            diceToEntropy 16 (replicate 49 '6') `shouldBe`
                Right (BS.replicate 16 0x00)
            diceToEntropy 20 (replicate 61 '6') `shouldBe`
                Right (BS.replicate 20 0x00)
            diceToEntropy 24 (replicate 74 '6') `shouldBe`
                Right (BS.replicate 24 0x00)
            diceToEntropy 28 (replicate 86 '6') `shouldBe`
                Right (BS.replicate 28 0x00)
            diceToEntropy 32 (replicate 99 '6') `shouldBe`
                Right (BS.replicate 32 0x00)
            diceToEntropy 32 (replicate 99 '1') `shouldBe`
                Right
                    (fromJust $
                     decodeHex
                         "302582058C61D13F1F9AA61CB6B5982DC3D9A42B333333333333333333333333")
            diceToEntropy
                32
                "666655555555555555555544444444444444444444444333333333333333333322222222222222222111111111111111111" `shouldBe`
                Right
                    (fromJust $
                     decodeHex
                         "002F8D57547E01B124FE849EE71CB96CA91478A542F7D4AA833EFAF5255F3333")
            diceToEntropy
                32
                "615243524162543244414631524314243526152432442413461523424314523615243251625434236413615423162365223" `shouldBe`
                Right
                    (fromJust $
                     decodeHex
                         "0CC66852D7580358E47819E37CDAF115E00364724346D83D49E59F094DB4972F")
        it "Entropy is always the correct size (0 padded)" $
            forAll (elements [(49, 16), (61, 20), (74, 24), (86, 28), (99, 32)]) $ \(r, e) ->
                forAll (vectorOf r (elements ['6', '1', '2', '3', '4', '5'])) $ \s ->
                    let bs = diceToEntropy (fromIntegral e) s
                     in BS.length <$> bs `shouldBe` Right e
        it "can mix entropy" $ do
            mixEntropy BS.empty (BS.pack [0x00]) `shouldSatisfy` isLeft
            mixEntropy (BS.pack [0x00]) BS.empty `shouldSatisfy` isLeft
            mixEntropy (BS.pack [0x00, 0x00]) (BS.pack [0x00]) `shouldSatisfy`
                isLeft
            mixEntropy (BS.pack [0x00]) (BS.pack [0x00, 0x00]) `shouldSatisfy`
                isLeft
            mixEntropy (BS.pack [0x00]) (BS.pack [0x00]) `shouldBe`
                Right (BS.pack [0x00])
            mixEntropy (BS.pack [0x00]) (BS.pack [0xff]) `shouldBe`
                Right (BS.pack [0xff])
            mixEntropy (BS.pack [0xff]) (BS.pack [0x00]) `shouldBe`
                Right (BS.pack [0xff])
            mixEntropy (BS.pack [0xff]) (BS.pack [0xff]) `shouldBe`
                Right (BS.pack [0x00])
            mixEntropy (BS.pack [0xaa]) (BS.pack [0x55]) `shouldBe`
                Right (BS.pack [0xff])
            mixEntropy (BS.pack [0x55, 0xaa]) (BS.pack [0xaa, 0x55]) `shouldBe`
                Right (BS.pack [0xff, 0xff])
            mixEntropy (BS.pack [0x7a, 0x54]) (BS.pack [0xd3, 0x8e]) `shouldBe`
                Right (BS.pack [0xa9, 0xda])

-- https://github.com/iancoleman/bip39/issues/58
mnemonicSpec :: Spec
mnemonicSpec =
    describe "Mnemonic API" $ do
        it "Can derive iancoleman issue 58" $ do
            let m = "fruit wave dwarf banana earth journey tattoo true farm silk olive fence"
                p = "banana"
                Right xpub = deriveXPubKey <$> signingKey btc p m 0
                (addr0, _) = derivePathAddr xpub extDeriv 0
            addrToText btc addr0 `shouldBe` Just "17rxURoF96VhmkcEGCj5LNQkmN9HVhWb7F"
        it "Passes the bip44 test vectors" $
            mapM_ testBip44Vector bip44Vectors

testBip44Vector :: (Text, Text, Text, Text) -> Assertion
testBip44Vector (mnem, pass, addr0, addr1) = do
    assertEqual "Addr External" (Just addr0) (addrToText btc $ fst $ r genExtAddress)
    assertEqual "Addr Internal" (Just addr1) (addrToText btc $ fst $ r genIntAddress)
  where
    Right k = signingKey btc pass mnem 0
    p = deriveXPubKey k
    s = emptyAccountStore btc p
    r a = fst $ runIdentity $ runAccountStoreT a s

-- (Mnemonic, BIP38 password, external 0/0, internal 1/0)
bip44Vectors :: [(Text, Text, Text, Text)]
bip44Vectors =
    [ ( "modify truck lens identify brief coffee \
        \gather volcano fatal together muscle elephant"
      , ""
      , "1KiWbwzHhwH2KdLyreGuJq36SP2pPeGeim"
      , "1BY9FShyC6rnEUdPAVQTX37TFZqEkR1enQ"
      )
    , ( "modify truck lens identify brief coffee \
        \gather volcano fatal together muscle elephant"
      , "password"
      , "1Ha18XQ74YfCnuUAjGTF9rnbS7pm1aCzW4"
      , "17SounFZ11k8urp9Pee2FNYjzGU5qwLUfT"
      )
    , ( "modify truck lens identify brief coffee \
        \gather volcano fatal together muscle elephant"
      , "Hello world"
      , "1KpHPSa9eMohQznkddzmHT9AeMAscYe29Y"
      , "16WJrKzdik1HgnoiNiLUW2uGDqry2fSGVx"
      )
    , ( "fiscal gadget drastic coconut awful crime \
        \during common salmon manage random cost \
        \evil owner city"
      , ""
      , "131dALC6WxwTZzTsHUsifAJ8bkQdDEeeqJ"
      , "1JHCmBLzDARCdDgn2WwJQr88tGW8V3EyKj"
      )
    , ( "host mind elephant tone sound apple \
        \service tomato subject attend motion stick \
        \fuel fan rail bamboo tree build"
      , ""
      , "15nbJ6oHvYiapKu1vBYmyQzVSTjiJNcfuV"
      , "12rMvadbhRxFaUP4FoRUmABa1JEPLrrqHL"
      )
    , ( "flight skill wisdom mixture patch dirt \
        \trouble behind chair glad detect swarm \
        \swap truly cruise medal walnut glide \
        \wrestle route defy"
      , ""
      , "12uDDJU7BJUxJomzbFt5BW3GnukGZz6Y5t"
      , "1Gv62T4UkLxJ6dnsmNXENMgjBhLzEzJ1Sc"
      )
    , ( "use turtle trap pause spin venue \
        \hazard hope slot april cattle fork \
        \stand finish arrest nasty acoustic clog \
        \entire course universe evil desert produce"
      , ""
      , "19fJdt5HjJUrmnvKM8fTS43o9aK3NmFZMT"
      , "17Kmofo8651DdydBS7zVVy9JLkixkxj6SK"
      )
    , ( "use turtle trap pause spin venue \
        \hazard hope slot april cattle fork \
        \stand finish arrest nasty acoustic clog \
        \entire course universe evil desert produce"
      , "nMCm#Qe7u-*LG^99KS%hSNZJh6w&e&w4S&7HA-22^7vDE#F\
        \@=hNZZZ$M=ZSYG42J8P+U+3sf$xS6YpJf7DemaU#fLEVBW8\
        \!FmugPj=K=E*KDmUuQh_2%7K2W8@RJ4?Pua2HFCb8$@ZB^p\
        \s=T=PNFy5APBBdaz_eTq&r&v3hp6P-tTfvNP9J+xfQ-FKZS\
        \7_?g+P*9MTkpjy*K5=8y+y9db#Sv+b*5bfmV8?@YKrf#AEH\
        \7!ARqBXMhS7d%GYAxB%RG"
      , "1L1oKPYJ9j3EFX5CcFXz1d3kkAW9Hoh5ic"
      , "1J7jpmP7NVoryofiVSCKMMvMitkCP3U6Qj"
      )
    ]
