{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Haskoin.Wallet.FileIO where

import           Control.Arrow                   (first)
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson                      (FromJSON, ToJSON, object,
                                                  parseJSON, toJSON, withObject,
                                                  (.:), (.=))
import qualified Data.Aeson                      as Json
import qualified Data.Aeson.Encode.Pretty        as Pretty
import           Data.Aeson.TH
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
import           Haskoin.Transaction
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
    }
    deriving (Eq, Show)

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

data TxSignData = TxSignData
    { txSignDataTx          :: Tx
    , txSignDataInputs      :: [Tx]
    , txSignDataInputPaths  :: [SoftPath]
    , txSignDataOutputPaths :: [SoftPath]
    , txSignDataNetwork     :: Network
    }
    deriving (Eq, Show)

instance FromJSON TxSignData where
    parseJSON =
        withObject "txsigndata" $ \o -> do
            net <- maybe mzero return . netByName =<< o .: "network"
            TxSignData <$> o .: "tx"
                       <*> o .: "txinputs"
                       <*> o .: "inputpaths"
                       <*> o .: "outputpaths"
                       <*> pure net

instance ToJSON TxSignData where
    toJSON (TxSignData t i oi op net) =
        object
            [ "tx" .= t
            , "txinputs" .= i
            , "inputpaths" .= oi
            , "outputpaths" .= op
            , "network" .= getNetworkName net
            ]

instance HasFilePath TxSignData where
    getFilePath (TxSignData tx _ _ _ net) =
        getNetworkName net <> "-unsignedtx-" <> cs (txChecksum tx) <> ".json"

data SignedTx = SignedTx
    { signedTxTx      :: Tx
    , signedTxNetwork :: Network
    }
    deriving (Eq, Show)

instance ToJSON SignedTx where
    toJSON (SignedTx tx net) =
        object
            [ "tx" .= tx
            , "network" .= getNetworkName net
            ]

instance FromJSON SignedTx where
    parseJSON =
        withObject "SignedTx" $ \o -> do
            net <- maybe mzero return . netByName =<< o .: "network"
            SignedTx <$> o .: "tx"
                     <*> pure net

instance HasFilePath SignedTx where
    getFilePath (SignedTx tx net) =
        getNetworkName net <> "-signedtx-" <> cs (txChecksum tx) <> ".json"

writeDoc :: (Json.ToJSON a, HasFilePath a) => a -> IO FilePath
writeDoc doc = do
    dir <- D.getUserDocumentsDirectory
    let path = dir </> getFilePath doc
    writeJsonFile path doc
    return path

txChecksum :: Tx -> Text
txChecksum = Text.take 16 . txHashToHex . nosigTxHash

{- JSON IO Helpers-}

writeJsonFile :: Json.ToJSON a => String -> a -> IO ()
writeJsonFile filePath doc = C8.writeFile filePath $ encodeJsonPrettyLn doc

readJsonFile ::
       (MonadError String m, MonadIO m, Json.FromJSON a)
    => String
    -> m a
readJsonFile filePath =
    liftEither =<< liftIO (Json.eitherDecodeFileStrict' filePath)
