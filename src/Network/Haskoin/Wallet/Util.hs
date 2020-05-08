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
import qualified Data.Serialize           as S
import           Data.String.Conversions  (cs)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Network.Haskoin.Keys
import           Network.Haskoin.Util

{- Data.Aeson Compatibility -}

encodeJsonPretty :: JSON.ToJSON a => a -> BS.ByteString
encodeJsonPretty =
    BL.toStrict .
    Pretty.encodePretty' Pretty.defConfig {Pretty.confIndent = Pretty.Spaces 2}

encodeJsonPrettyLn :: JSON.ToJSON a => a -> BS.ByteString
encodeJsonPrettyLn =
    BL.toStrict .
    Pretty.encodePretty'
        Pretty.defConfig
            { Pretty.confIndent = Pretty.Spaces 2
            , Pretty.confTrailingNewline = True
            }

{- Haskoin helper functions -}

xPubChecksum :: XPubKey -> Text
xPubChecksum = encodeHex . S.encode . xPubFP

(</>) :: String -> String -> String
a </> b = a <> "/" <> b

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

