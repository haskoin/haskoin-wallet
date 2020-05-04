{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.Util where

import           Control.Arrow            (second)
import           Control.Monad            (guard)
import qualified Data.Aeson               as JSON
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base16   as B16
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe               (fromMaybe)
import           Data.String.Conversions  (cs)
import           Data.Text                (Text)
import qualified Data.Text                as Text

{- Data.Aeson Compatibility -}

encodeJsonPretty :: JSON.ToJSON a => a -> BS.ByteString
encodeJsonPretty =
    BL.toStrict .
    Pretty.encodePretty' Pretty.defConfig {Pretty.confIndent = Pretty.Spaces 2}

encodeJson :: JSON.ToJSON a => a -> BS.ByteString
encodeJson = BL.toStrict . JSON.encode

decodeJson :: JSON.FromJSON a => BS.ByteString -> Maybe a
decodeJson = JSON.decode . BL.fromStrict

encodeHex :: BS.ByteString -> BS.ByteString
encodeHex = B16.encode

decodeHex :: BS.ByteString -> Maybe (BS.ByteString)
decodeHex bs = do
    guard (BS.null rest)
    return res
  where
    (res, rest) = B16.decode bs

encodeHexStr :: BS.ByteString -> String
encodeHexStr = cs . encodeHex

encodeHexText :: BS.ByteString -> Text
encodeHexText = cs . encodeHex

decodeHexStr :: String -> Maybe (BS.ByteString)
decodeHexStr = decodeHex . cs

decodeHexText :: Text -> Maybe (BS.ByteString)
decodeHexText = decodeHex . cs

toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BL.toStrict

{- Haskoin helper functions -}

(!!?) :: [a] -> Int -> Maybe a
xs !!? i
    | i < length xs = Just $ xs !! i
    | otherwise = Nothing

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
    | null xs = []
    | otherwise = uncurry (:) $ second (chunksOf n) $ splitAt n xs

padStart :: Int -> Text -> Text -> Text
padStart n c t =
    Text.replicate (fromMaybe 0 $ n `safeSubtract` Text.length t) c <> t

padEnd :: Int -> Text -> Text -> Text
padEnd n c t
    = t <> Text.replicate (fromMaybe 0 $ n `safeSubtract` Text.length t) c

dropPatternEnd :: Text -> Text -> Text
dropPatternEnd p t = fromMaybe t $ Text.stripSuffix p t

chunksOfEnd :: Int -> Text -> [Text]
chunksOfEnd n t = reverse $ Text.reverse <$> Text.chunksOf n (Text.reverse t)

safeSubtract :: Integral a => a -> a -> Maybe a
safeSubtract a b
    | b > a = Nothing
    | otherwise = Just $ a - b

