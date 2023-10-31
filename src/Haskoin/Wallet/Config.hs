{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Config where

import Control.Monad (unless)
import Data.Aeson
import Data.Default
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin.Network
import Haskoin.Store.WebClient
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import qualified System.Directory as D

-- | Version of Haskoin Wallet package.
versionString :: (IsString a) => a
#ifdef CURRENT_PACKAGE_VERSION
versionString = CURRENT_PACKAGE_VERSION
#else
versionString = "Unavailable"
#endif

hwDataDirectory :: IO FilePath
hwDataDirectory = do
  dir <- D.getAppUserDataDirectory "hw"
  D.createDirectoryIfMissing True dir
  return dir

data Config = Config
  { configHost :: Text,
    configGap :: Natural,
    configRecoveryGap :: Natural,
    configAddrBatch :: Natural,
    configTxBatch :: Natural,
    configCoinBatch :: Natural,
    configTxFullBatch :: Natural
  }
  deriving (Eq, Show)

instance FromJSON Config where
  parseJSON =
    withObject "config" $ \o ->
      Config
        <$> o .: "host"
        <*> o .: "gap"
        <*> o .: "recovery-gap"
        <*> o .: "addr-batch"
        <*> o .: "tx-batch"
        <*> o .: "coin-batch"
        <*> o .: "tx-full-batch"

instance ToJSON Config where
  toJSON cfg =
    object
      [ "host" .= configHost cfg,
        "gap" .= configGap cfg,
        "recovery-gap" .= configRecoveryGap cfg,
        "addr-batch" .= configAddrBatch cfg,
        "tx-batch" .= configTxBatch cfg,
        "coin-batch" .= configCoinBatch cfg,
        "tx-full-batch" .= configTxFullBatch cfg
      ]

instance Default Config where
  def =
    Config
      { configHost = cs (def :: ApiConfig).host,
        configGap = 20,
        configRecoveryGap = 40,
        configAddrBatch = 100,
        configTxBatch = 100,
        configCoinBatch = 100,
        configTxFullBatch = 100
      }

initConfig :: IO Config
initConfig = do
  dir <- hwDataDirectory
  let configFile = dir </> "config.json"
  exists <- D.doesFileExist configFile
  unless exists $ writeJsonFile configFile $ toJSON (def :: Config)
  resE <- readJsonFile configFile
  either (error "Could not read config.json") return resE

apiHost :: Network -> Config -> ApiConfig
apiHost net = ApiConfig net . cs . configHost
