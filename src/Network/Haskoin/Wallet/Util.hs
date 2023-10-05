{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.Haskoin.Wallet.Util where

import Control.Arrow (second)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Crypto.Secp256k1 (Ctx)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Address (Address, addrToText, textToAddr)
import Haskoin.Crypto.Keys (XPubKey, xPubFP)
import Haskoin.Network (Network)
import qualified Haskoin.Store.Data as Store
import Haskoin.Util (encodeHex, maybeToEither)
import Numeric.Natural (Natural)

{- Data.Aeson Compatibility -}

encodeJsonPretty :: (JSON.ToJSON a) => a -> BS.ByteString
encodeJsonPretty =
  BL.toStrict
    . Pretty.encodePretty' Pretty.defConfig {Pretty.confIndent = Pretty.Spaces 2}

encodeJsonPrettyLn :: (JSON.ToJSON a) => a -> BS.ByteString
encodeJsonPrettyLn =
  BL.toStrict
    . Pretty.encodePretty'
      Pretty.defConfig
        { Pretty.confIndent = Pretty.Spaces 2,
          Pretty.confTrailingNewline = True
        }

{- Haskoin helper functions -}

data Page = Page
  { pageLimit :: !Natural,
    pageOffset :: !Natural
  }
  deriving (Eq, Show)

toPage :: Page -> [a] -> [a]
toPage (Page limit offset) xs =
  take (fromIntegral limit) $ drop (fromIntegral offset) xs

liftExcept :: (MonadError String m) => ExceptT Store.Except m a -> m a
liftExcept action = do
  e <- runExceptT action
  case e of
    Right a -> return a
    Left err -> throwError $ show (err :: Store.Except)

addrToTextE :: Network -> Address -> Either String Text
addrToTextE net a =
  maybeToEither "Invalid Address in addrToTextE" (addrToText net a)

textToAddrE :: Network -> Text -> Either String Address
textToAddrE net a =
  maybeToEither "Invalid Address in textToAddrE" (textToAddr net a)

addrToText2 :: Network -> (Address, v) -> Either String (Text, v)
addrToText2 net (a, v) = (,v) <$> addrToTextE net a

textToAddr2 :: Network -> (Text, v) -> Either String (Address, v)
textToAddr2 net (a, v) = (,v) <$> textToAddrE net a

addrToText3 :: Network -> (Address, v, w) -> Either String (Text, v, w)
addrToText3 net (a, v, w) = (,v,w) <$> addrToTextE net a

textToAddr3 :: Network -> (Text, v, w) -> Either String (Address, v, w)
textToAddr3 net (a, v, w) = (,v,w) <$> textToAddrE net a

lastList :: Natural -> [a] -> [a]
lastList count xs = drop (max 0 $ length xs - fromIntegral count) xs

xPubChecksum :: Ctx -> XPubKey -> Text
xPubChecksum ctx = encodeHex . S.encode . xPubFP ctx

(</>) :: String -> String -> String
a </> b = a <> "/" <> b

(!!?) :: [a] -> Natural -> Maybe a
xs !!? i
  | fromIntegral i < length xs = Just $ xs !! fromIntegral i
  | otherwise = Nothing

chunksOf :: Natural -> [a] -> [[a]]
chunksOf n xs
  | null xs = []
  | otherwise =
      uncurry (:) $ second (chunksOf n) $ splitAt (fromIntegral n) xs

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortBy (flip compare)

padStart :: Int -> Text -> Text -> Text
padStart n c t =
  Text.replicate (fromMaybe 0 $ n `safeSubtract` Text.length t) c <> t

padEnd :: Int -> Text -> Text -> Text
padEnd n c t =
  t <> Text.replicate (fromMaybe 0 $ n `safeSubtract` Text.length t) c

dropPatternEnd :: Text -> Text -> Text
dropPatternEnd p t = fromMaybe t $ Text.stripSuffix p t

chunksOfEnd :: Int -> Text -> [Text]
chunksOfEnd n t = reverse $ Text.reverse <$> Text.chunksOf n (Text.reverse t)

safeSubtract :: (Integral a) => a -> a -> Maybe a
safeSubtract a b
  | b > a = Nothing
  | otherwise = Just $ a - b
