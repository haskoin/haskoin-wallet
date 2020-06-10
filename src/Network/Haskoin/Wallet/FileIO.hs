{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Haskoin.Wallet.FileIO where

import           Control.Applicative             ((<|>))
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
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as C8
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Short           as BSS
import qualified Data.HashMap.Strict             as HMap
import           Data.List                       (nub)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe
import qualified Data.Serialize                  as S
import           Data.String.Conversions         (cs)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Crypto
import           Haskoin.Keys
import           Haskoin.Transaction
import           Haskoin.Util
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Options.Applicative.Help.Pretty hiding ((</>))
import qualified System.Directory                as D
import qualified System.IO                       as IO

class HasFilePath a where
    getFilePath :: a -> String

data PubKeyDoc = PubKeyDoc
    { documentPubKey  :: !XPubKey
    , documentNetwork :: !Network
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
    { txSignDataTx          :: !Tx
    , txSignDataInputs      :: ![Tx]
    , txSignDataInputPaths  :: ![SoftPath]
    , txSignDataOutputPaths :: ![SoftPath]
    , txSignDataAccount     :: !Natural
    , txSignDataSigned      :: !Bool
    , txSignDataNetwork     :: !Network
    }
    deriving (Eq, Show)

instance FromJSON TxSignData where
    parseJSON =
        withObject "txsigndata" $ \o -> do
            net <- maybe mzero return . netByName =<< o .: "network"
            let f = eitherToMaybe . S.decode <=< decodeHex
            t <- maybe mzero return . f =<< o .: "tx"
            i <- maybe mzero return . mapM f =<< o .: "txinputs"
            TxSignData <$> pure t
                       <*> pure i
                       <*> o .: "inputpaths"
                       <*> o .: "outputpaths"
                       <*> o .: "account"
                       <*> o .: "signed"
                       <*> pure net

instance ToJSON TxSignData where
    toJSON (TxSignData t i oi op a s net) =
        object
            [ "tx" .= encodeHex (S.encode t)
            , "txinputs" .= (encodeHex . S.encode <$> i)
            , "inputpaths" .= oi
            , "outputpaths" .= op
            , "account" .= a
            , "signed" .= s
            , "network" .= getNetworkName net
            ]

instance HasFilePath TxSignData where
    getFilePath (TxSignData tx _ _ _ _ s net) =
        getNetworkName net <> heading <> cs (txChecksum tx) <> ".json"
      where
        heading = if s then "-signedtx-" else "-unsignedtx-"

hwDataDirectory :: Maybe FilePath -> IO FilePath
hwDataDirectory subDirM = do
    appDir <- D.getAppUserDataDirectory "hw"
    let dir = maybe appDir (appDir </>) subDirM
    D.createDirectoryIfMissing True dir
    return dir

data HWFolder
    = TxFolder
    | PubKeyFolder
    | SweepFolder !String
    deriving (Eq, Show)

toFolder :: HWFolder -> String
toFolder =
    \case
        TxFolder -> "transactions"
        PubKeyFolder -> "pubkeys"
        SweepFolder chksum -> "sweep-" <> chksum

writeDoc :: (Json.ToJSON a, HasFilePath a) => HWFolder -> a -> IO FilePath
writeDoc folder doc = do
    dir <- hwDataDirectory $ Just $ toFolder folder
    let path = dir </> getFilePath doc
    writeJsonFile path doc
    return path

txChecksum :: Tx -> Text
txChecksum = Text.take 8 . txHashToHex . nosigTxHash

txsChecksum :: [Tx] -> Text
txsChecksum txs =
    Text.take 8 $ txHashToHex $ TxHash $ sha256 $ mconcat bss
  where
    bss = BSS.fromShort . getHash256 . getTxHash . nosigTxHash <$> txs

-- JSON IO Helpers--

writeJsonFile :: Json.ToJSON a => String -> a -> IO ()
writeJsonFile filePath doc = C8.writeFile filePath $ encodeJsonPrettyLn doc

readJsonFile ::
       (MonadError String m, MonadIO m, Json.FromJSON a)
    => FilePath
    -> m a
readJsonFile filePath =
    liftEither =<< liftIO (Json.eitherDecodeFileStrict' filePath)

-- Parse wallet dump files for sweeping --

readFileWords :: FilePath -> IO [[Text]]
readFileWords fp = do
    strContents <- liftIO $ IO.readFile fp
    return $ removeComments $ Text.words <$> Text.lines (cs strContents)

parseAddrsFile :: Network -> [[Text]] -> [Address]
parseAddrsFile net =
    withParser $ \w -> eitherToMaybe $ textToAddrE net $ strip "addr=" w
  where
    strip p w = fromMaybe w $ Text.stripPrefix p w

parseSecKeysFile :: Network -> [[Text]] -> [SecKey]
parseSecKeysFile net =
    withParser $ \w -> secKeyData <$> (fromWif net w <|> fromMiniKey (cs w))

withParser :: (Text -> Maybe a) -> [[Text]] -> [a]
withParser parser =
    mapMaybe go
  where
    go [] = Nothing
    go (w:ws) = parser w <|> go ws

removeComments :: [[Text]] -> [[Text]]
removeComments =
    mapMaybe go
  where
    go [] = Nothing
    go ws@(w:_)
        | "#" `Text.isPrefixOf` w = Nothing
        | otherwise = Just ws

