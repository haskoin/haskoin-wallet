{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Haskoin.Wallet.Config where

import Data.Default
import Data.String (IsString)
import Haskoin.Network
import Haskoin.Store.WebClient
  ( ApiConfig (..),
  )
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

conf :: Network -> ApiConfig
conf net = ApiConfig net (def :: ApiConfig).host

gap :: Int
gap = 20

recoveryGap :: Natural
recoveryGap = 40

addrBatch :: Natural
addrBatch = 100

txBatch :: Natural
txBatch = 100

coinBatch :: Natural
coinBatch = 100

txFullBatch :: Natural
txFullBatch = 100
