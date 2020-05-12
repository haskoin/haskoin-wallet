{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Haskoin.Wallet.FileIO where

import           Control.Arrow                   (first)
import           Control.Monad
import           Control.Monad.Except
import qualified Data.Aeson                      as Json
import qualified Data.Aeson.Encode.Pretty        as Pretty
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.Aeson.Types                as Json
import qualified Data.ByteString.Char8           as C8
import qualified Data.ByteString.Lazy            as BL
import qualified Data.HashMap.Strict             as HMap
import           Data.List                       (nub)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe, maybe)
import qualified Data.Serialize                  as S
import           Data.String.Conversions         (cs)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Keys
import           Haskoin.Util
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Options.Applicative.Help.Pretty hiding ((</>))
import qualified System.Directory                as D
import           System.IO                       (IOMode (..), withFile)

class HasFilePath a where
    getFilePath :: a -> String

data PubKeyDoc = PubKeyDoc
    { documentPubKey  :: XPubKey
    , documentNetwork :: Network
    } deriving (Eq, Show)

instance FromJSON PubKeyDoc where
    parseJSON =
        withObject "pubkeydocument" $ \o -> do
            net <- maybe mzero return . netByName =<< o .: "network"
            PubKeyDoc <$> (xPubFromJSON net =<< o .: "xpubkey") <*> return net

instance ToJSON PubKeyDoc where
    toJSON (PubKeyDoc k net) =
        object
            [ "xpubkey" .= xPubToJSON net k
            , "network" .= getNetworkName net
            ]

instance HasFilePath PubKeyDoc where
    getFilePath (PubKeyDoc xpub net) =
        getNetworkName net <> "-account-" <> cs (xPubChecksum xpub) <> ".json"

writeDoc :: (Json.ToJSON a, HasFilePath a) => a -> IO FilePath
writeDoc doc = do
    dir <- D.getUserDocumentsDirectory
    let path = dir </> getFilePath doc
    writeJsonFile path doc
    return path

{- JSON IO Helpers-}

writeJsonFile :: Json.ToJSON a => String -> a -> IO ()
writeJsonFile filePath doc = C8.writeFile filePath $ encodeJsonPrettyLn doc

readJsonFile :: Json.FromJSON a => String -> ExceptT String IO a
readJsonFile filePath =
    liftEither =<< liftIO (Json.eitherDecodeFileStrict' filePath)
