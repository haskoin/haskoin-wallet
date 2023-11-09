{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Haskoin.Wallet.Migration.V0_9_0 where

import Conduit (MonadUnliftIO, ResourceT)
import Control.Arrow (Arrow (second))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Aeson as Json
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.List (find, nub, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word64)
import Database.Esqueleto.Legacy as E
import qualified Database.Persist as P
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util (Page (Page), textToAddrE)
import Numeric.Natural (Natural)

{- Migration from 0.8.* to 0.9.0 -}

v0_9_0 :: Text
v0_9_0 = "0.9.0"

data OldTxInfo = OldTxInfo
  { oldTxInfoHash :: !TxHash,
    oldTxInfoType :: !TxType,
    oldTxInfoAmount :: !Integer,
    oldTxInfoMyOutputs :: !(Map Address OldMyOutputs),
    oldTxInfoOtherOutputs :: !(Map Address Natural),
    oldTxInfoNonStdOutputs :: ![Store.StoreOutput],
    oldTxInfoMyInputs :: !(Map Address OldMyInputs),
    oldTxInfoOtherInputs :: !(Map Address OtherInputs),
    oldTxInfoNonStdInputs :: ![Store.StoreInput],
    oldTxInfoSize :: !Natural,
    oldTxInfoFee :: !Natural,
    oldTxInfoFeeByte :: !Natural,
    oldTxInfoBlockRef :: !Store.BlockRef,
    oldTxInfoConfirmations :: !Natural
  }
  deriving (Eq, Show)

data OldMyOutputs = OldMyOutputs
  { oldMyOutputsValue :: !Natural,
    oldMyOutputsPath :: !SoftPath
  }
  deriving (Eq, Show)

instance Json.ToJSON OldMyOutputs where
  toJSON (OldMyOutputs i p) =
    object
      [ "value" .= i,
        "path" .= p
      ]

instance Json.FromJSON OldMyOutputs where
  parseJSON =
    withObject "MyOutputs" $ \o -> do
      i <- o .: "value"
      p <- o .: "path"
      return $ OldMyOutputs i p

data OldMyInputs = OldMyInputs
  { oldMyInputsValue :: !Natural,
    oldMyInputsPath :: !SoftPath,
    oldMyInputsSigInput :: [SigInput]
  }
  deriving (Eq, Show)

instance MarshalJSON Ctx OldMyInputs where
  marshalValue ctx (OldMyInputs i p s) =
    object $
      [ "value" .= i,
        "path" .= p
      ]
        ++ ["siginput" .= (marshalValue ctx <$> s) | not (null s)]

  unmarshalValue ctx =
    withObject "MyInputs" $ \o -> do
      i <- o .: "value"
      p <- o .: "path"
      sM <- o .:? "siginput"
      s <- unmarshalValue ctx `mapM` fromMaybe [] sM
      return $ OldMyInputs i p s

instance MarshalJSON (Network, Ctx) OldTxInfo where
  marshalValue (net, ctx) tx =
    object
      [ "txid" .= oldTxInfoHash tx,
        "type" .= oldTxInfoType tx,
        "amount" .= oldTxInfoAmount tx,
        "myoutputs" .= mapAddrText net (oldTxInfoMyOutputs tx),
        "otheroutputs" .= mapAddrText net (oldTxInfoOtherOutputs tx),
        "nonstdoutputs" .= (marshalValue net <$> oldTxInfoNonStdOutputs tx),
        "myinputs" .= marshalMap net ctx (oldTxInfoMyInputs tx),
        "otherinputs" .= marshalMap net ctx (oldTxInfoOtherInputs tx),
        "nonstdinputs" .= (marshalValue net <$> oldTxInfoNonStdInputs tx),
        "size" .= oldTxInfoSize tx,
        "fee" .= oldTxInfoFee tx,
        "feebyte" .= oldTxInfoFeeByte tx,
        "block" .= oldTxInfoBlockRef tx,
        "confirmations" .= oldTxInfoConfirmations tx
      ]
  unmarshalValue (net, ctx) =
    Json.withObject "TxInfo" $ \o ->
      OldTxInfo
        <$> o .: "txid"
        <*> o .: "type"
        <*> o .: "amount"
        <*> (mapTextAddr net <$> o .: "myoutputs")
        <*> (mapTextAddr net <$> o .: "otheroutputs")
        <*> (mapM (unmarshalValue net) =<< o .: "nonstdoutputs")
        <*> (unmarshalMap net ctx =<< o .: "myinputs")
        <*> (unmarshalMap net ctx =<< o .: "otherinputs")
        <*> (mapM (unmarshalValue net) =<< o .: "nonstdinputs")
        <*> o .: "size"
        <*> o .: "fee"
        <*> o .: "feebyte"
        <*> o .: "block"
        <*> o .: "confirmations"

migrateMyOutputs :: OldMyOutputs -> MyOutputs
migrateMyOutputs OldMyOutputs {..} =
  MyOutputs
    { myOutputsValue = oldMyOutputsValue,
      myOutputsPath = oldMyOutputsPath,
      myOutputsLabel = ""
    }

migrateMyInputs :: OldMyInputs -> MyInputs
migrateMyInputs OldMyInputs {..} =
  MyInputs
    { myInputsValue = oldMyInputsValue,
      myInputsPath = oldMyInputsPath,
      myInputsLabel = "",
      myInputsSigInput = oldMyInputsSigInput
    }

migrateTxInfo :: OldTxInfo -> TxInfo
migrateTxInfo OldTxInfo {..} =
  TxInfo
    { txInfoHash = Just oldTxInfoHash,
      txInfoType = oldTxInfoType,
      txInfoAmount = oldTxInfoAmount,
      txInfoMyOutputs = migrateMyOutputs <$> oldTxInfoMyOutputs,
      txInfoOtherOutputs = oldTxInfoOtherOutputs,
      txInfoNonStdOutputs = oldTxInfoNonStdOutputs,
      txInfoMyInputs = migrateMyInputs <$> oldTxInfoMyInputs,
      txInfoOtherInputs = oldTxInfoOtherInputs,
      txInfoNonStdInputs = oldTxInfoNonStdInputs,
      txInfoSize = oldTxInfoSize,
      txInfoFee = oldTxInfoFee,
      txInfoFeeByte = oldTxInfoFeeByte,
      txInfoBlockRef = oldTxInfoBlockRef,
      txInfoConfirmations = oldTxInfoConfirmations,
      txInfoPending = Nothing
    }

migrateDB :: (MonadUnliftIO m) => Ctx -> ExceptT String (DB m) ()
migrateDB ctx = do
  accs <- lift . select $ from return
  forM_ accs $ \(Entity _ acc) -> do
    let net = accountNetwork acc
    res <- lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccountWallet ==. val (dBAccountWallet acc)
          &&. t ^. DBTxInfoAccountDerivation ==. val (dBAccountDerivation acc)
      return $ t ^. DBTxInfoId
    forM_ res $ \(Value tKey) -> do
      oldDBTx <- liftMaybe "OldTxInfo" =<< lift (P.get tKey)
      oldTxInfo <-
        liftMaybe "OldTxInfo" $
          unmarshalJSON (net, ctx) $
            BS.fromStrict $
              dBTxInfoBlob oldDBTx
      let newTxInfo = migrateTxInfo oldTxInfo
          newBlob = BS.toStrict $ marshalJSON (net, ctx) newTxInfo
      lift $ P.update tKey [DBTxInfoBlob P.=. newBlob]
  -- This will create the missing version table
  lift $ do
    globalMigration
    setVersion v0_9_0
