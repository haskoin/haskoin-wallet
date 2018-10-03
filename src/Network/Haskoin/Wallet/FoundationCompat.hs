{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.FoundationCompat where

import           Control.Monad                (guard)
import qualified Data.Aeson                   as JSON
import qualified Data.Aeson.Encode.Pretty     as Pretty
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base16       as B16
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Serialize               as Cereal
import           Data.Text                    (Text)
import           Foundation
import           Foundation.Collection
import           Foundation.Compat.ByteString
import           Foundation.Compat.Text
import           Foundation.String
import           Network.Haskoin.Util         (eitherToMaybe)
import qualified Prelude

{- Data.Aeson Compatibility -}

encodeJsonPretty :: JSON.ToJSON a => a -> UArray Word8
encodeJsonPretty =
    fromByteString .
    BL.toStrict .
    Pretty.encodePretty' Pretty.defConfig {Pretty.confIndent = Pretty.Spaces 2}

encodeJson :: JSON.ToJSON a => a -> UArray Word8
encodeJson = fromByteString . BL.toStrict . JSON.encode

decodeJson :: JSON.FromJSON a => UArray Word8 -> Maybe a
decodeJson = JSON.decode . BL.fromStrict . toByteString

{- Data.Serialize Compatibility -}

encodeBytes :: Cereal.Serialize a => a -> UArray Word8
encodeBytes = fromByteString . Cereal.encode

decodeBytes :: Cereal.Serialize a => UArray Word8 -> Maybe a
decodeBytes = eitherToMaybe . Cereal.decode . toByteString

{- LString, Text and ByteString Compatibility -}

toLString :: String -> LString
toLString = toList

fromLString :: LString -> String
fromLString = fromList

bsToString :: BS.ByteString -> Maybe String
bsToString = bytesToString . fromByteString

bsToString_ :: BS.ByteString -> String
bsToString_ = bytesToString_ . fromByteString

stringToBS :: String -> BS.ByteString
stringToBS = toByteString . toBytes UTF8

textToBytes :: Text -> UArray Word8
textToBytes = stringToBytes . fromText

bytesToText :: UArray Word8 -> Maybe Text
bytesToText = fmap toText . bytesToString

eitherString :: Either LString a -> Either String a
eitherString e =
    case e of
        Right res -> Right res
        Left str  -> Left $ fromList str

withBytes :: (BS.ByteString -> a) -> UArray Word8 -> a
withBytes f = f . toByteString

asBytes :: (a -> BS.ByteString) -> a -> UArray Word8
asBytes f = fromByteString . f

{- Helper functions -}

bytesToString :: UArray Word8 -> Maybe String
bytesToString a8 = do
    guard $ isNothing valM
    return str
  where
    (str,valM,_) = fromBytes UTF8 a8

bytesToString_ :: UArray Word8 -> String
bytesToString_ = fst . fromBytesLenient

stringToBytes :: String -> UArray Word8
stringToBytes = toBytes UTF8

encodeHex :: UArray Word8 -> UArray Word8
encodeHex = fromByteString . B16.encode .  toByteString

decodeHex :: UArray Word8 -> Maybe (UArray Word8)
decodeHex bytes = do
    guard (BS.null rest)
    return $ fromByteString res
  where
    (res, rest) = B16.decode $ toByteString bytes

encodeHexStr :: UArray Word8 -> String
encodeHexStr = fst . fromBytesLenient . encodeHex

encodeHexText :: UArray Word8 -> Text
encodeHexText = toText . fst . fromBytesLenient . encodeHex

decodeHexStr :: String -> Maybe (UArray Word8)
decodeHexStr = decodeHex . toBytes UTF8

decodeHexText :: Text -> Maybe (UArray Word8)
decodeHexText = decodeHex . toBytes UTF8 . fromText

toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BL.toStrict

{- List helpers -}

groupIn :: Sequential c => CountOf (Element c) -> c -> [c]
groupIn n xs
    | null xs = []
    | otherwise = uncurry (:) $ second (groupIn n) $ splitAt n xs

groupEnd :: Sequential c => CountOf (Element c) -> c -> [c]
groupEnd n xs
    | null xs = []
    | otherwise =
        uncurry (flip (<>)) $ bimap (: []) (groupEnd n) $ revSplitAt n xs

dropPatternEnd :: (Eq (Element c), Sequential c) => c -> c -> c
dropPatternEnd p xs
    | p `isSuffixOf` xs = revDrop (length p) xs
    | otherwise = xs

padStart :: Sequential c => CountOf (Element c) -> Element c -> c -> c
padStart n p xs = replicate (fromMaybe 0 (n - length xs)) p <> xs

padEnd :: Sequential c => CountOf (Element c) -> Element c -> c -> c
padEnd n p xs = xs <> replicate (fromMaybe 0 (n - length xs)) p

{- Haskoin helper functions -}

integralToNatural :: Prelude.Integral a => a -> Maybe Natural
integralToNatural i
    | i < 0 = Nothing
    | otherwise = Just $ fromIntegral i
