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

-- This was designed to parse the output of bitcoin core dumpwallet command
parseSecKeysFile :: Network -> FilePath -> IO [(SecKey, Address)]
parseSecKeysFile net fp = do
    ls <- readLineFile fp
    let toParse = catMaybes $ skip <$> (Text.words <$> ls)
    return $ catMaybes $ go <$> toParse
  where
    skip [] = Nothing
    skip ws@(w:_)
        | "#" `Text.isPrefixOf` w = Nothing
        | otherwise = Just ws
    parseKey w = secKeyData <$> (fromWif net w <|> fromMiniKey (cs w))
    parseAddr w = eitherToMaybe $ textToAddrE net $ strip "addr=" w
    strip p w = fromMaybe w $ Text.stripPrefix p w
    parseFirst [] _ = Nothing
    parseFirst (w:ws) parser = parser w <|> parseFirst ws parser
    go ws = do
        skey <- parseFirst ws parseKey
        addr <- parseFirst ws parseAddr
        return (skey, addr)

{- JSON IO Helpers-}

writeJsonFile :: Json.ToJSON a => String -> a -> IO ()
writeJsonFile filePath doc = C8.writeFile filePath $ encodeJsonPrettyLn doc

readJsonFile ::
       (MonadError String m, MonadIO m, Json.FromJSON a)
    => FilePath
    -> m a
readJsonFile filePath =
    liftEither =<< liftIO (Json.eitherDecodeFileStrict' filePath)

readLineFile :: FilePath -> IO [Text]
readLineFile fp = do
    strContents <- liftIO $ IO.readFile fp
    return $ Text.lines $ cs strContents

