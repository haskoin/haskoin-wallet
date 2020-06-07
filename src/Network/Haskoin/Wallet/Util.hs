{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.Util where

import           Control.Arrow            (second)
import           Control.Monad            (guard)
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson               as JSON
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base16   as B16
import qualified Data.ByteString.Lazy     as BL
import           Data.List                (sortBy)
import           Data.Maybe               (fromMaybe)
import           Data.Ord                 (compare)
import qualified Data.Serialize           as S
import           Data.String.Conversions  (cs)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Keys
import qualified Haskoin.Store.Data       as Store
import           Haskoin.Util
import           Numeric.Natural

type NetworkT = ReaderT Network

network :: MonadReader Network m => m Network
network = ask

withNetwork :: Monad m => Network -> NetworkT m a -> m a
withNetwork net m = runReaderT m net

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

data Page = Page
    { pageOffset :: !Natural
    , pageLimit  :: !Natural
    }
    deriving (Eq, Show)

toPage :: Page -> [a] -> [a]
toPage (Page offset limit) xs =
    take (fromIntegral limit) $ drop (fromIntegral offset) xs

liftExcept :: MonadError String m => ExceptT Store.Except m a -> m a
liftExcept action = do
    e <- runExceptT action
    case e of
        Right a -> return a
        Left err -> throwError $ show (err :: Store.Except)

addrToTextE :: Network -> Address -> Either String Text
addrToTextE net a =
    maybeToEither "Invalid Address in addrToStringE" (addrToString net a)

textToAddrE :: Network -> Text -> Either String Address
textToAddrE net a =
    maybeToEither "Invalid Address in stringToAddrE" (stringToAddr net a)

addrToText2 :: Network -> (Address, v) -> Either String (Text, v)
addrToText2 net (a, v) = (,v) <$> addrToTextE net a

textToAddr2 :: Network -> (Text, v) -> Either String (Address, v)
textToAddr2 net (a, v) = (,v) <$> textToAddrE net a

lastList :: Natural -> [a] -> [a]
lastList count xs = drop (max 0 $ length xs - fromIntegral count) xs

xPubChecksum :: XPubKey -> Text
xPubChecksum = encodeHex . S.encode . xPubFP

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

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

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

