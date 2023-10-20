{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Haskoin.Wallet.FileIO where

import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    liftEither,
    runExceptT,
  )
import Control.Applicative ((<|>))
import Control.Monad (MonadPlus (mzero), (<=<), unless, when)
import Data.Aeson
  ( object,
    withObject,
    (.:),
    (.=),
  )
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as BSS
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Address (Address)
import Haskoin.Crypto
  ( Ctx,
    Hash256 (get),
    PrivateKey (key),
    SecKey,
    SoftPath,
    XPubKey,
    fromMiniKey,
    fromWif,
    sha256,
    withContext,
    xPubExport, xPubChild, Fingerprint, fingerprintToText,
  )
import Haskoin.Network (Network (name), netByName)
import Haskoin.Transaction
  ( Tx,
    TxHash (TxHash, get),
    nosigTxHash,
    txHashToHex,
  )
import Haskoin.Util
  ( MarshalJSON (marshalValue, unmarshalValue),
    decodeHex,
    eitherToMaybe,
    encodeHex,
  )
import Network.Haskoin.Wallet.Util
  ( encodeJsonPrettyLn,
    textToAddrE,
    xPubChecksum,
    (</>),
  )
import Numeric.Natural (Natural)
import qualified System.Directory as D
import qualified System.IO as IO

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

hwDataDirectory :: Maybe HWFolder -> IO FilePath
hwDataDirectory subDirM = do
  appDir <- D.getAppUserDataDirectory "hw"
  let dir = maybe appDir ((appDir </>) . toFolder) subDirM
  D.createDirectoryIfMissing True dir
  return dir

class HasFilePath a where
  getFilePath :: Ctx -> a -> String

data PubKeyDoc = PubKeyDoc
  { documentPubKey :: !XPubKey,
    documentNetwork :: !Network,
    documentName :: !Text,
    documentWallet :: !Fingerprint
  }
  deriving (Eq, Show)

instance MarshalJSON Ctx PubKeyDoc where
  unmarshalValue ctx =
    withObject "pubkeydocument" $ \o -> do
      net <- maybe mzero return . netByName =<< o .: "network"
      PubKeyDoc
        <$> (unmarshalValue (net, ctx) =<< o .: "xpubkey")
        <*> return net
        <*> (o .: "name")
        <*> (o .: "wallet")

  marshalValue ctx (PubKeyDoc k net name wallet) =
    object
      [ "xpubkey" .= marshalValue (net, ctx) k,
        "network" .= net.name,
        "name" .= name,
        "wallet" .= wallet
      ]

instance HasFilePath PubKeyDoc where
  getFilePath _ (PubKeyDoc key net _ wallet) =
    let i = show $ xPubChild key
        w = fingerprintToText wallet
     in "account-" <> cs w <> "-" <> net.name <> "-" <> i <> ".json"

data TxSignData = TxSignData
  { txSignDataTx :: !Tx,
    txSignDataInputs :: ![Tx],
    txSignDataInputPaths :: ![SoftPath],
    txSignDataOutputPaths :: ![SoftPath],
    txSignDataAccount :: !Natural,
    txSignDataSigned :: !Bool,
    txSignDataNetwork :: !Network
  }
  deriving (Eq, Show)

instance MarshalJSON Ctx TxSignData where
  unmarshalValue _ =
    withObject "txsigndata" $ \o -> do
      net <- maybe mzero return . netByName =<< o .: "network"
      let f = eitherToMaybe . S.decode <=< decodeHex
      t <- maybe mzero return . f =<< o .: "tx"
      i <- maybe mzero return . mapM f =<< o .: "txinputs"
      TxSignData t i
        <$> o .: "inputpaths"
        <*> o .: "outputpaths"
        <*> o .: "account"
        <*> o .: "signed"
        <*> pure net
  marshalValue _ (TxSignData t i oi op a s net) =
    object
      [ "tx" .= encodeHex (S.encode t),
        "txinputs" .= (encodeHex . S.encode <$> i),
        "inputpaths" .= oi,
        "outputpaths" .= op,
        "account" .= a,
        "signed" .= s,
        "network" .= net.name
      ]

instance HasFilePath TxSignData where
  getFilePath _ (TxSignData tx _ _ _ _ s net) =
    net.name <> heading <> cs (txChecksum tx) <> ".json"
    where
      heading = if s then "-signedtx-" else "-unsignedtx-"

docFilePath :: (HasFilePath a) => Ctx -> HWFolder -> a -> IO FilePath
docFilePath ctx folder doc = do
  dir <- hwDataDirectory $ Just folder
  return $ dir </> getFilePath ctx doc

checkFileExists :: MonadIO m => FilePath -> ExceptT String m ()
checkFileExists path = do
  exist <- liftIO $ D.doesFileExist path
  when exist $ throwError $ "File " <> path <> " already exists"

writeDoc ::
  (MarshalJSON Ctx a, HasFilePath a, MonadIO m) =>
  Ctx ->
  HWFolder ->
  a ->
  ExceptT String m FilePath
writeDoc ctx folder doc = do
  path <- liftIO $ docFilePath ctx folder doc
  liftIO $ writeJsonFile path $ marshalValue ctx doc
  return path

txChecksum :: Tx -> Text
txChecksum = Text.take 8 . txHashToHex . nosigTxHash

txsChecksum :: [Tx] -> Text
txsChecksum txs =
  Text.take 8 $ txHashToHex $ TxHash $ sha256 $ mconcat bss
  where
    bss = BSS.fromShort . (.get) . (.get) . nosigTxHash <$> txs

-- JSON IO Helpers--

writeJsonFile :: String -> Json.Value -> IO ()
writeJsonFile filePath doc = C8.writeFile filePath $ encodeJsonPrettyLn doc

readJsonFile :: (Json.FromJSON a) => FilePath -> IO (Either String a)
readJsonFile = Json.eitherDecodeFileStrict'

writeMarshalFile :: (MarshalJSON s a) => s -> String -> a -> IO ()
writeMarshalFile s filePath a = writeJsonFile filePath $ marshalValue s a

readMarshalFile :: (MarshalJSON s a) => s -> FilePath -> IO (Either String a)
readMarshalFile s filePath = do
  vE <- readJsonFile filePath
  return $ Json.parseEither (unmarshalValue s) =<< vE

-- Parse wallet dump files for sweeping --

readFileWords :: FilePath -> IO [[Text]]
readFileWords fp = do
  strContents <- IO.readFile fp
  return $ removeComments $ Text.words <$> Text.lines (cs strContents)

parseAddrsFile :: Network -> [[Text]] -> [Address]
parseAddrsFile net =
  withParser $ \w -> eitherToMaybe $ textToAddrE net $ strip "addr=" w
  where
    strip p w = fromMaybe w $ Text.stripPrefix p w

parseSecKeysFile :: Network -> [[Text]] -> [SecKey]
parseSecKeysFile net =
  withParser $ \w -> (.key) <$> (fromWif net w <|> fromMiniKey (cs w))

withParser :: (Text -> Maybe a) -> [[Text]] -> [a]
withParser parser =
  mapMaybe go
  where
    go [] = Nothing
    go (w : ws) = parser w <|> go ws

removeComments :: [[Text]] -> [[Text]]
removeComments =
  mapMaybe go
  where
    go [] = Nothing
    go ws@(w : _)
      | "#" `Text.isPrefixOf` w = Nothing
      | otherwise = Just ws
