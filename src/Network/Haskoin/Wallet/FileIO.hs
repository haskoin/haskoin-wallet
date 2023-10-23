{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Haskoin.Wallet.FileIO where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    liftEither,
    runExceptT,
  )
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Data.Aeson
import Data.Aeson
  ( object,
    withObject,
    (.:),
    (.=),
  )
import Data.Aeson.Types (parseEither)
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
    Fingerprint,
    Hash256 (get),
    PrivateKey (key),
    SecKey,
    SoftPath,
    XPubKey,
    fingerprintToText,
    fromMiniKey,
    fromWif,
    sha256,
    withContext,
    xPubChild,
    xPubExport,
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
    (</>),
  )
import Numeric.Natural (Natural)
import qualified System.Directory as D
import qualified System.IO as IO

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

data TxSignData = TxSignData
  { txSignDataTx :: !Tx,
    txSignDataInputs :: ![Tx],
    txSignDataInputPaths :: ![SoftPath],
    txSignDataOutputPaths :: ![SoftPath],
    txSignDataSigned :: !Bool
  }
  deriving (Eq, Show)

instance FromJSON TxSignData where
  parseJSON =
    withObject "txsigndata" $ \o -> do
      let f = eitherToMaybe . S.decode <=< decodeHex
      t <- maybe mzero return . f =<< o .: "tx"
      i <- maybe mzero return . mapM f =<< o .: "txinputs"
      TxSignData t i
        <$> o .: "inputpaths"
        <*> o .: "outputpaths"
        <*> o .: "signed"

instance ToJSON TxSignData where
  toJSON (TxSignData t i oi op s) =
    object
      [ "tx" .= encodeHex (S.encode t),
        "txinputs" .= (encodeHex . S.encode <$> i),
        "inputpaths" .= oi,
        "outputpaths" .= op,
        "signed" .= s
      ]

instance MarshalJSON Ctx TxSignData where
  marshalValue _ = toJSON
  unmarshalValue _ = parseJSON

checkPathFree :: (MonadIO m) => FilePath -> ExceptT String m ()
checkPathFree path = do
  exist <- liftIO $ D.doesPathExist path
  when exist $ throwError $ "Path '" <> path <> "' already exists"

txChecksum :: Tx -> Text
txChecksum = Text.take 8 . txHashToHex . nosigTxHash

txsChecksum :: [Tx] -> Text
txsChecksum txs =
  Text.take 8 $ txHashToHex $ TxHash $ sha256 $ mconcat bss
  where
    bss = BSS.fromShort . (.get) . (.get) . nosigTxHash <$> txs

txSignDataFileName :: TxSignData -> FilePath
txSignDataFileName (TxSignData tx _ _ _ s) =
  let heading = if s then "signedtx-" else "unsignedtx-"
   in heading <> cs (txChecksum tx) <> ".json"

initSweepDir ::
     (MonadIO m) => [TxSignData] -> FilePath -> ExceptT String m FilePath
initSweepDir tsds dir = do
  let chksum = cs $ txsChecksum $ txSignDataTx <$> tsds
      sweepDir = dir </> "sweep-" <> chksum
  exists <- liftIO $ D.doesDirectoryExist dir
  unless exists $ throwError $ "Invalid directory " <> dir
  liftIO $ D.createDirectoryIfMissing False sweepDir
  return sweepDir

writeSweepFiles ::
  (MonadIO m) => [TxSignData] -> FilePath -> ExceptT String m [FilePath]
writeSweepFiles tsds sweepDir = do
  exists <- liftIO $ D.doesDirectoryExist sweepDir
  unless exists $ throwError $ "Invalid directory " <> sweepDir
  forM tsds $ \tsd -> do
    let file = sweepDir </> txSignDataFileName tsd
    liftIO $ writeJsonFile file $ toJSON tsd
    return file

-- JSON IO Helpers--

writeJsonFile :: FilePath -> Value -> IO ()
writeJsonFile filePath doc = C8.writeFile filePath $ encodeJsonPrettyLn doc

readJsonFile :: (FromJSON a) => FilePath -> IO (Either String a)
readJsonFile = eitherDecodeFileStrict'

writeMarshalFile :: (MarshalJSON s a) => s -> FilePath -> a -> IO ()
writeMarshalFile s filePath a = writeJsonFile filePath $ marshalValue s a

readMarshalFile :: (MarshalJSON s a) => s -> FilePath -> IO (Either String a)
readMarshalFile s filePath = do
  vE <- readJsonFile filePath
  return $ parseEither (unmarshalValue s) =<< vE

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
