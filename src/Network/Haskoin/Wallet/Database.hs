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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.Database where

import Conduit (MonadUnliftIO, ResourceT)
import Control.Exception (try)
import Control.Monad
  ( MonadPlus (mzero),
    forM,
    forM_,
    unless,
    void,
    when,
    (<=<),
  )
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Reader (MonadTrans (lift), ReaderT)
import qualified Data.Aeson as Json
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
  )
import Data.Bits (Bits (clearBit))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (GeneralCategory (OtherNumber))
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Serialize (decode, encode)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time
import Data.Word (Word32, Word64)
import Database.Esqueleto
import qualified Database.Persist as P
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
import Haskoin
import Haskoin.Crypto (xPubChild)
import Haskoin.Store.Data (confirmed)
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient (GetAddrsBalance (..), apiCall)
import Haskoin.Transaction
import Haskoin.Util (maybeToEither)
import Network.Haskoin.Wallet.FileIO
import Network.Haskoin.Wallet.Signing (conf, walletFingerprint)
import Network.Haskoin.Wallet.TxInfo
import Network.Haskoin.Wallet.Util (Page (..), liftExcept, textToAddrE, (</>))
import Numeric.Natural (Natural)
import qualified System.Directory as D
import System.IO.Error (alreadyExistsErrorType)

{- SQL Table Definitions -}

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

DBWallet
  fingerprint Text
  Primary fingerprint
  deriving Show

DBAccount
  name Text
  wallet DBWalletId
  index Int
  network Text
  derivation Text
  external Int
  internal Int
  xPubKey Text
  pubKeyFile Text
  balanceConfirmed Word64
  balanceUnconfirmed Word64
  balanceCoins Word64
  created UTCTime default=CURRENT_TIME
  Primary wallet derivation
  UniqueName name
  UniqueXPubKey xPubKey
  UniqueNetworkId wallet network index
  deriving Show

DBAddress
  index Int
  accountWallet DBWalletId
  accountDerivation Text
  derivation Text
  address Text
  label Text
  balanceConfirmed Word64
  balanceUnconfirmed Word64
  balanceCoins Word64
  balanceTxs Word64
  balanceReceived Word64
  internal Bool
  created UTCTime default=CURRENT_TIME
  Primary accountWallet accountDerivation derivation
  UniqueAddress address
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show

DBTxInfo
  accountWallet DBWalletId
  accountDerivation Text
  txid Text
  blockRef ByteString
  confirmed Bool
  blob ByteString
  created UTCTime default=CURRENT_TIME
  Primary accountWallet accountDerivation txid
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show

DBCoin
  accountWallet DBWalletId
  accountDerivation Text
  outpoint Text
  address Text
  blob ByteString
  confirmed Bool
  locked Bool
  created UTCTime default=CURRENT_TIME
  Primary outpoint
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show

DBBest
  network Text
  bestBlock Text
  bestHeight Int
  Primary network
  deriving Show
|]

gap :: Int64
gap = 20

type DB m = ReaderT SqlBackend (NoLoggingT (ResourceT m))

liftDB :: (Monad m) => DB m (Either String a) -> ExceptT String (DB m) a
liftDB = liftEither <=< lift

runDB :: (MonadUnliftIO m) => DB m a -> m a
runDB action = do
  dir <- liftIO $ hwDataDirectory Nothing
  let dbFile = dir </> "accounts.sqlite"
  runSqlite (cs dbFile) $ do
    _ <- runMigrationQuiet migrateAll
    action

{- Meta -}

updateBest ::
  (MonadUnliftIO m) => Network -> BlockHash -> BlockHeight -> DB m ()
updateBest net hash height = do
  let netT = cs net.name
      key = DBBestKey netT
  P.repsert key $ DBBest netT (blockHashToHex hash) (fromIntegral height)

getBest ::
  (MonadUnliftIO m) => Network -> DB m (Maybe (BlockHash, BlockHeight))
getBest net = do
  let netT = cs net.name
  resM <- P.get $ DBBestKey netT
  return $ do
    DBBest _ a b <- resM
    hash <- hexToBlockHash a
    return (hash, fromIntegral b)

{- Accounts -}

instance ToJSON DBAccount where
  toJSON acc =
    object
      [ "name" .= dBAccountName acc,
        "wallet" .= dBAccountWallet acc,
        "index" .= dBAccountIndex acc,
        "network" .= dBAccountNetwork acc,
        "derivation" .= dBAccountDerivation acc,
        "external" .= dBAccountExternal acc,
        "internal" .= dBAccountInternal acc,
        "xPubKey" .= dBAccountXPubKey acc,
        "pubKeyFile" .= dBAccountPubKeyFile acc,
        "balance" .= toJSON (AccountBalance confirmed unconfirmed coins),
        "created" .= dBAccountCreated acc
      ]
    where
      confirmed = dBAccountBalanceConfirmed acc
      unconfirmed = dBAccountBalanceUnconfirmed acc
      coins = dBAccountBalanceCoins acc

instance FromJSON DBAccount where
  parseJSON =
    withObject "DBAccount" $ \o -> do
      bal <- o .: "balance"
      DBAccount
        <$> o .: "name"
        <*> o .: "wallet"
        <*> o .: "index"
        <*> o .: "network"
        <*> o .: "derivation"
        <*> o .: "external"
        <*> o .: "internal"
        <*> o .: "xPubKey"
        <*> o .: "pubKeyFile"
        <*> (bal .: "confirmed")
        <*> (bal .: "unconfirmed")
        <*> (bal .: "coins")
        <*> o .: "created"

data AccountBalance = AccountBalance
  { -- | confirmed balance
    accBalanceConfirmed :: !Word64,
    -- | unconfirmed balance
    accBalanceUnconfirmed :: !Word64,
    -- | number of unspent outputs
    accBalanceCoins :: !Word64
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON AccountBalance where
  toJSON b =
    object
      [ "confirmed" .= accBalanceConfirmed b,
        "unconfirmed" .= accBalanceUnconfirmed b,
        "coins" .= accBalanceCoins b
      ]

instance FromJSON AccountBalance where
  parseJSON =
    withObject "accountbalance" $ \o ->
      AccountBalance
        <$> o .: "confirmed"
        <*> o .: "unconfirmed"
        <*> o .: "coins"

accountIndex :: DBAccount -> Natural
accountIndex = fromIntegral . dBAccountIndex

accountNetwork :: DBAccount -> Network
accountNetwork =
  fromMaybe (error "Invalid Network in database")
    . netByName
    . cs
    . dBAccountNetwork

accountWallet :: DBAccount -> Fingerprint
accountWallet acc =
  let (DBWalletKey walletFP) = dBAccountWallet acc
   in fromRight (error "Invalid WalletId in database") $
        textToFingerprint walletFP

accountXPubKey :: Ctx -> DBAccount -> XPubKey
accountXPubKey ctx acc =
  fromMaybe (error "Invalid XPubKey in database") $
    xPubImport (accountNetwork acc) ctx (dBAccountXPubKey acc)

nextAccountDeriv :: (MonadUnliftIO m) => Fingerprint -> Network -> DB m Natural
nextAccountDeriv walletFP net = do
  let walletId = DBWalletKey $ fingerprintToText walletFP
  dM <-
    selectOne $
      from $ \a -> do
        where_ $
          a ^. DBAccountNetwork ==. val (cs net.name)
            &&. a ^. DBAccountWallet ==. val walletId
        return $ max_ $ a ^. DBAccountIndex
  return $ joinMaybe 0 ((+ 1) . fromIntegral) dM

existsAccount :: (MonadUnliftIO m) => Text -> DB m Bool
existsAccount name = P.existsBy $ UniqueName name

existsXPubKey :: (MonadUnliftIO m) => Network -> Ctx -> XPubKey -> DB m Bool
existsXPubKey net ctx key = P.existsBy $ UniqueXPubKey $ xPubExport net ctx key

getWalletOrCreate :: (MonadUnliftIO m) => Fingerprint -> DB m DBWalletId
getWalletOrCreate fp = do
  let key = DBWalletKey $ fingerprintToText fp
  walletM <- P.get key
  case walletM of
    Just _ -> return key
    _ -> P.insert $ DBWallet $ fingerprintToText fp

insertAccount ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  Fingerprint ->
  Text ->
  XPubKey ->
  Text ->
  DB m (Either String DBAccount)
insertAccount net ctx walletFP name xpub keyfile = do
  existsName <- existsAccount name
  if existsName
    then return $ Left $ "Account " <> cs name <> " already exists"
    else do
      existsKey <- existsXPubKey net ctx xpub
      if existsKey
        then return $ Left "The XPubKey already exists"
        else do
          time <- liftIO getCurrentTime
          walletId <- getWalletOrCreate walletFP
          let path = bip44Deriv net $ fromIntegral $ xPubChild xpub
              idx = fromIntegral $ xPubIndex xpub
              account =
                DBAccount
                  { dBAccountName = name,
                    dBAccountWallet = walletId,
                    dBAccountIndex = idx,
                    dBAccountNetwork = cs net.name,
                    dBAccountDerivation = cs $ pathToStr path,
                    dBAccountExternal = 0,
                    dBAccountInternal = 0,
                    dBAccountXPubKey = xPubExport net ctx xpub,
                    dBAccountPubKeyFile = keyfile,
                    dBAccountBalanceConfirmed = 0,
                    dBAccountBalanceUnconfirmed = 0,
                    dBAccountBalanceCoins = 0,
                    dBAccountCreated = time
                  }
          P.insert_ account
          return $ Right account

-- When a name is provided, get that account or throw an error if it doesn't
-- exist. When no name is provided, return the account only if there is one
-- account.
getAccount ::
  (MonadUnliftIO m) =>
  Maybe Text ->
  DB m (Either String (DBAccountId, DBAccount))
getAccount (Just name) = do
  aM <- P.getBy $ UniqueName name
  return $
    case aM of
      Just a -> Right (entityKey a, entityVal a)
      _ -> Left $ "The account " <> cs name <> " does not exist"
getAccount Nothing = do
  as <- getAccounts
  return $
    case as of
      [a] -> Right a
      [] -> Left "There are no accounts in the wallet"
      _ -> Left "Specify which account to use"

getAccountId ::
  (MonadUnliftIO m) => DBAccountId -> DB m (Either String DBAccount)
getAccountId accId = maybeToEither "Invalid account" <$> P.get accId

getAccountVal ::
  (MonadUnliftIO m) => Maybe Text -> DB m (Either String DBAccount)
getAccountVal nameM =
  runExceptT $ do
    (_, acc) <- liftEither =<< lift (getAccount nameM)
    return acc

getAccounts :: (MonadUnliftIO m) => DB m [(DBAccountId, DBAccount)]
getAccounts = (go <$>) <$> P.selectList [] [P.Asc DBAccountCreated]
  where
    go a = (entityKey a, entityVal a)

getAccountsVal :: (MonadUnliftIO m) => DB m [DBAccount]
getAccountsVal = (entityVal <$>) <$> P.selectList [] [P.Asc DBAccountCreated]

getAccountNames :: (MonadUnliftIO m) => DB m [Text]
getAccountNames = do
  res <- select . from $ \a ->
    return $ a ^. DBAccountName
  return $ unValue <$> res

renameAccount ::
  (MonadUnliftIO m) => Text -> Text -> DB m (Either String DBAccount)
renameAccount oldName newName
  | oldName == newName = return $ Left "Old and new names are the same"
  | otherwise = do
      e <- existsAccount newName
      if e
        then return $ Left $ "The account " <> cs newName <> " already exists"
        else do
          c <-
            updateCount $ \a -> do
              set a [DBAccountName =. val newName]
              where_ $ a ^. DBAccountName ==. val oldName
          if c == 0
            then return $ Left $ "The account " <> cs oldName <> " does not exist"
            else getAccountVal (Just newName)

updateAccountBalances :: (MonadUnliftIO m) => DBAccountId -> DB m DBAccount
updateAccountBalances accId@(DBAccountKey wallet accDeriv) = do
  confirmed <- selectSum DBAddressBalanceConfirmed
  unconfirmed <- selectSum DBAddressBalanceUnconfirmed
  coins <- selectSum DBAddressBalanceCoins
  update $ \a -> do
    set
      a
      [ DBAccountBalanceConfirmed =. val (unpack confirmed),
        DBAccountBalanceUnconfirmed =. val (unpack unconfirmed),
        DBAccountBalanceCoins =. val (unpack coins)
      ]
    where_ $ a ^. DBAccountId ==. val accId
  fromJust <$> P.get accId
  where
    unpack m = fromMaybe 0 $ unValue $ fromMaybe (Value $ Just 0) m
    selectSum field =
      selectOne . from $ \a -> do
        where_ $
          a ^. DBAddressAccountWallet ==. val wallet
            &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        return $ sum_ $ a ^. field

{- Addresses -}

instance ToJSON DBAddress where
  toJSON addr =
    object
      [ "index" .= dBAddressIndex addr,
        "wallet" .= dBAddressAccountWallet addr,
        "account" .= dBAddressAccountDerivation addr,
        "derivation" .= dBAddressDerivation addr,
        "address" .= dBAddressAddress addr,
        "label" .= dBAddressLabel addr,
        "balance"
          .= AddressBalance
            (dBAddressBalanceConfirmed addr)
            (dBAddressBalanceUnconfirmed addr)
            (dBAddressBalanceCoins addr)
            (dBAddressBalanceTxs addr)
            (dBAddressBalanceReceived addr),
        "internal" .= dBAddressInternal addr,
        "created" .= dBAddressCreated addr
      ]

instance FromJSON DBAddress where
  parseJSON =
    withObject "DBAddress" $ \o -> do
      bal <- o .: "balance"
      DBAddress
        <$> o .: "index"
        <*> o .: "wallet"
        <*> o .: "account"
        <*> o .: "derivation"
        <*> o .: "address"
        <*> o .: "label"
        <*> bal .: "confirmed"
        <*> bal .: "unconfirmed"
        <*> bal .: "coins"
        <*> bal .: "txs"
        <*> bal .: "received"
        <*> o .: "internal"
        <*> o .: "created"

data AddressBalance = AddressBalance
  { -- | confirmed balance
    addrBalanceConfirmed :: !Word64,
    -- | unconfirmed balance
    addrBalanceUnconfirmed :: !Word64,
    -- | number of unspent outputs
    addrBalanceCoins :: !Word64,
    -- | number of transactions
    addrBalanceTxs :: !Word64,
    -- | total amount from all outputs in this address
    addrBalanceReceived :: !Word64
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON AddressBalance where
  toJSON b =
    object
      [ "confirmed" .= addrBalanceConfirmed b,
        "unconfirmed" .= addrBalanceUnconfirmed b,
        "coins" .= addrBalanceCoins b,
        "txs" .= addrBalanceTxs b,
        "received" .= addrBalanceReceived b
      ]

instance FromJSON AddressBalance where
  parseJSON =
    withObject "accountbalance" $ \o ->
      AddressBalance
        <$> o .: "confirmed"
        <*> o .: "unconfirmed"
        <*> o .: "coins"
        <*> o .: "txs"
        <*> o .: "received"

updateAddressBalances ::
  (MonadUnliftIO m) =>
  Network ->
  [Store.Balance] ->
  DB m (Either String ())
updateAddressBalances net storeBals =
  runExceptT $
    forM_ storeBals $ \s -> do
      addrT <-
        liftEither $ maybeToEither "Invalid Address" (addrToText net s.address)
      lift $
        update $ \a -> do
          set
            a
            [ DBAddressBalanceConfirmed =. val s.confirmed,
              DBAddressBalanceUnconfirmed =. val s.unconfirmed,
              DBAddressBalanceCoins =. val s.utxo,
              DBAddressBalanceTxs =. val s.txs,
              DBAddressBalanceReceived =. val s.received
            ]
          where_ $ a ^. DBAddressAddress ==. val addrT

-- Insert an address into the database. Does nothing if it already exists.
insertAddress ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  SoftPath ->
  Address ->
  Text ->
  DB m (Either String DBAddress)
insertAddress net (DBAccountKey wallet accDeriv) deriv addr label =
  runExceptT $ do
    time <- liftIO getCurrentTime
    addrT <- liftEither $ maybeToEither "Invalid Address" (addrToText net addr)
    (isInternal, idx) <- parseDeriv
    let label' = if isInternal then "Internal Address" else label
        derivS = cs $ pathToStr deriv
        dbAddr =
          DBAddress
            (fromIntegral idx)
            wallet
            accDeriv
            derivS
            addrT
            label'
            0
            0
            0
            0
            0
            isInternal
            time
    addrM <- lift $ P.get (DBAddressKey wallet accDeriv derivS)
    case addrM of
      Just a -> return a
      Nothing -> do
        lift $ P.insert_ dbAddr
        return dbAddr
  where
    parseDeriv =
      case pathToList deriv of
        [0, x] -> return (False, x) -- Not an internal address
        [1, x] -> return (True, x) -- Internal address
        _ -> throwError "Invalid address SoftPath"

-- Set external or internal account derivation
-- Will also generate and insert the relevant addresses
updateDeriv ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  SoftPath ->
  Natural ->
  DB m (Either String DBAccount)
updateDeriv net ctx accId path deriv =
  runExceptT $ do
    acc <- liftEither <=< lift $ getAccountId accId
    internal <-
      case pathToList path of
        [0] -> return False
        [1] -> return True
        _ -> throwError "Invalid updateDeriv SoftPath"
    let xPubKey = accountXPubKey ctx acc
        label
          | internal = DBAccountInternal
          | otherwise = DBAccountExternal
        accDeriv
          | internal = dBAccountInternal
          | otherwise = dBAccountExternal
    if accDeriv acc >= fromIntegral deriv
      then return acc -- Nothing to do
      else do
        let addrs = addrsDerivPage ctx path (Page deriv 0) xPubKey
        forM_ addrs $ \(a, p) -> lift $ insertAddress net accId p a ""
        lift $ P.update accId [label P.=. fromIntegral deriv]
        liftEither <=< lift $ getAccountId accId

receiveAddress ::
  (MonadUnliftIO m) =>
  Ctx ->
  Maybe Text ->
  Text ->
  DB m (Either String (DBAccount, DBAddress))
receiveAddress ctx nameM label =
  runExceptT $ do
    (accId, acc) <- liftEither <=< lift $ getAccount nameM
    let net = accountNetwork acc
        xpub = accountXPubKey ctx acc
        ext = fromIntegral $ dBAccountExternal acc
        (addr, _) = derivePathAddr ctx xpub extDeriv ext
    dbAddr <-
      liftEither <=< lift $
        insertAddress net accId (extDeriv :/ ext) addr label
    newAcc <- lift $ P.updateGet accId [DBAccountExternal P.+=. 1]
    return (newAcc, dbAddr)

addressPage :: (MonadUnliftIO m) => DBAccountId -> Page -> DB m [DBAddress]
addressPage (DBAccountKey wallet accDeriv) (Page lim off) = do
  as <-
    select $
      from $ \a -> do
        where_ $
          a ^. DBAddressAccountWallet ==. val wallet
            &&. a ^. DBAddressAccountDerivation ==. val accDeriv
            &&. a ^. DBAddressInternal ==. val False
        orderBy [desc (a ^. DBAddressIndex)]
        limit $ fromIntegral lim
        offset $ fromIntegral off
        return a
  return $ entityVal <$> as

allAddressesMap ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  DB m (Either String (Map Address SoftPath, Map Address AddressBalance))
allAddressesMap net (DBAccountKey wallet accDeriv) =
  runExceptT $ do
    dbRes <-
      lift $
        select $
          from $ \a -> do
            where_ $
              a ^. DBAddressAccountWallet ==. val wallet
                &&. a ^. DBAddressAccountDerivation ==. val accDeriv
            return a
    res <-
      forM dbRes $ \(Entity _ dbAddr) -> do
        a <- liftEither $ textToAddrE net $ dBAddressAddress dbAddr
        d <-
          liftEither $
            maybeToEither "parsePath failed" $
              parseSoft $
                cs $
                  dBAddressDerivation dbAddr
        let b =
              AddressBalance
                (dBAddressBalanceConfirmed dbAddr)
                (dBAddressBalanceUnconfirmed dbAddr)
                (dBAddressBalanceCoins dbAddr)
                (dBAddressBalanceTxs dbAddr)
                (dBAddressBalanceReceived dbAddr)
        return ((a, d), (a, b))
    return (Map.fromList $ fst <$> res, Map.fromList $ snd <$> res)

updateLabel ::
  (MonadUnliftIO m) =>
  Maybe Text ->
  Word32 ->
  Text ->
  DB m (Either String (DBAccount, DBAddress))
updateLabel nameM idx label = do
  runExceptT $ do
    (DBAccountKey wallet accDeriv, acc) <-
      liftEither <=< lift $ getAccount nameM
    let path = cs $ pathToStr $ extDeriv :/ idx
        aKey = DBAddressKey wallet accDeriv path
    unless (fromIntegral idx < dBAccountExternal acc) $
      throwError $
        "Address " <> show idx <> " does not exist"
    adr <- lift $ P.updateGet aKey [DBAddressLabel P.=. label]
    return (acc, adr)

{- Transactions -}

existsTx ::
  (MonadUnliftIO m) => DBAccountId -> TxHash -> DB m Bool
existsTx (DBAccountKey wallet accDeriv) txid =
  isJust <$> P.get (DBTxInfoKey wallet accDeriv $ txHashToHex txid)

getConfirmedTxs ::
  (MonadUnliftIO m) => DBAccountId -> Bool -> DB m (Either String [TxHash])
getConfirmedTxs (DBAccountKey wallet accDeriv) confirmed = do
  ts <-
    select $
      from $ \t -> do
        where_ $
          t ^. DBTxInfoAccountWallet ==. val wallet
            &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
            &&. t ^. DBTxInfoConfirmed ==. val confirmed
        orderBy [asc (t ^. DBTxInfoBlockRef)]
        return $ t ^. DBTxInfoTxid
  return $
    forM ts $ \(Value t) ->
      maybeToEither "getUnconfirmedTxs invalid TxHash" $ hexToTxHash t

-- Insert a new transaction or replace it, if it already exists
repsertTxInfo ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  TxInfo ->
  DB m DBTxInfo
repsertTxInfo net ctx accId tif = do
  time <- liftIO getCurrentTime
  let confirmed' = Store.confirmed $ txInfoBlockRef tif
      tid = txHashToHex $ txInfoId tif
      bRef = encode $ txInfoBlockRef tif
      -- Confirmations will get updated when retrieving them
      blob = BS.toStrict $ marshalJSON (net, ctx) tif {txInfoConfirmations = 0}
      (DBAccountKey wallet accDeriv) = accId
      key = DBTxInfoKey wallet accDeriv tid
      txInfo =
        DBTxInfo
          { dBTxInfoAccountWallet = wallet,
            dBTxInfoAccountDerivation = accDeriv,
            dBTxInfoTxid = tid,
            dBTxInfoBlockRef = bRef,
            dBTxInfoConfirmed = confirmed',
            dBTxInfoBlob = blob,
            dBTxInfoCreated = time
          }
  resM <- P.get key
  case resM of
    Just res -> do
      let newTxInfo =
            res
              { dBTxInfoBlockRef = bRef,
                dBTxInfoConfirmed = confirmed',
                dBTxInfoBlob = blob
              }
      P.replace key newTxInfo
      return newTxInfo
    Nothing -> do
      P.insert_ txInfo
      return txInfo

txsPage ::
  (MonadUnliftIO m) =>
  Ctx ->
  Maybe Text ->
  Page ->
  DB m (Either String (DBAccount, [TxInfo]))
txsPage ctx nameM (Page lim off) =
  runExceptT $ do
    (DBAccountKey wallet accDeriv, acc) <-
      liftEither <=< lift $ getAccount nameM
    let net = accountNetwork acc
    dbTxs <-
      lift $
        select $
          from $ \t -> do
            where_ $
              t ^. DBTxInfoAccountWallet ==. val wallet
                &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
            orderBy [asc (t ^. DBTxInfoBlockRef)]
            limit $ fromIntegral lim
            offset $ fromIntegral off
            return $ t ^. DBTxInfoBlob
    res <-
      forM dbTxs $ \(Value dbTx) -> do
        liftEither $
          maybeToEither "TxInfo unmarshalJSON Failed" $
            unmarshalJSON (net, ctx) $
              BS.fromStrict dbTx
    bestM <- lift $ getBest net
    case bestM of
      Just (_, h) -> return (acc, updateConfirmations h <$> res)
      _ -> return (acc, res)

updateConfirmations :: BlockHeight -> TxInfo -> TxInfo
updateConfirmations best tif =
  case txInfoBlockRef tif of
    Store.BlockRef h _ ->
      let bestI = toInteger best :: Integer
          hI = toInteger h :: Integer
          confI = bestI - hI + 1
       in tif {txInfoConfirmations = fromIntegral $ max 0 confI}
    Store.MemRef _ ->
      tif {txInfoConfirmations = 0}

{- Coins -}

outpointText :: OutPoint -> Text
outpointText = encodeHex . encode

-- Spendable coins must be confirmed and not locked
getSpendableCoins :: (MonadUnliftIO m) => DBAccountId -> DB m [DBCoin]
getSpendableCoins (DBAccountKey wallet accDeriv) = do
  coins <- select . from $ \c -> do
    where_ $
      c ^. DBCoinAccountWallet ==. val wallet &&.
      c ^. DBCoinAccountDerivation ==. val accDeriv &&.
      c ^. DBCoinConfirmed ==. val True &&.
      c ^. DBCoinLocked ==. val False
    return c
  return $ entityVal <$> coins

insertCoin ::
  (MonadUnliftIO m) =>
  DBAccountId ->
  Text ->
  Store.Unspent ->
  DB m DBCoin
insertCoin (DBAccountKey wallet accDeriv) addr unspent = do
  time <- liftIO getCurrentTime
  let newCoin =
        DBCoin
          { dBCoinAccountWallet = wallet,
            dBCoinAccountDerivation = accDeriv,
            dBCoinOutpoint = outpointText unspent.outpoint,
            dBCoinAddress = addr,
            dBCoinBlob = encode unspent,
            dBCoinConfirmed = Store.confirmed unspent.block,
            dBCoinLocked = False,
            dBCoinCreated = time
          }
  P.insert_ newCoin
  return newCoin

updateCoin :: (MonadUnliftIO m) => Store.Unspent -> DB m ()
updateCoin unspent = do
  let key = DBCoinKey $ outpointText unspent.outpoint
  P.update
    key
    [ DBCoinBlob P.=. encode unspent,
      DBCoinConfirmed P.=. Store.confirmed unspent.block
    ]

deleteCoin :: (MonadUnliftIO m) => DBCoin -> DB m ()
deleteCoin coin =
  delete . from $ \c ->
    where_ $
      c ^. DBCoinOutpoint ==. val (dBCoinOutpoint coin)

getCoinsByAddr :: (MonadUnliftIO m) => Text -> DB m [DBCoin]
getCoinsByAddr addr = do
  coins <- select . from $ \c -> do
    where_ $ c ^. DBCoinAddress ==. val addr
    return c
  return $ entityVal <$> coins

-- Either insert, update or delete coins as required. Returns the number of
-- coins that have either been inserted, updated or deleted.
updateCoins ::
  (MonadUnliftIO m) => Network -> DBAccountId -> [Store.Unspent] -> DB m Int
updateCoins net accId allUnspent = do
  let storeMap = groupCoins net allUnspent
  res <- forM (Map.assocs storeMap) $ \(addr, storeCoins) -> do
    localCoins <- getCoinsByAddr addr
    let storeOps = outpointText . (.outpoint) <$> storeCoins
        localOps = dBCoinOutpoint <$> localCoins
        toDelete = filter ((`notElem` storeOps) . dBCoinOutpoint) localCoins
        toInsert =
          filter ((`notElem` localOps) . outpointText . (.outpoint)) storeCoins
        toUpdate = filter (f localCoins) storeCoins
    forM_ toDelete deleteCoin
    forM_ toUpdate updateCoin
    forM_ toInsert $ insertCoin accId addr
    return $ length toDelete + length toUpdate + length toInsert
  return $ sum res
  where
    f localCoins s =
      let cM = find ((== outpointText s.outpoint) . dBCoinOutpoint) localCoins
       in case cM of
            Just c -> dBCoinConfirmed c /= confirmed s.block
            _ -> False

groupCoins :: Network -> [Store.Unspent] -> Map Text [Store.Unspent]
groupCoins net =
  Map.fromListWith (<>) . mapMaybe f
  where
    f x =
      case x.address of
        Just a -> (,[x]) <$> addrToText net a
        _ -> Nothing

setLockCoin :: (MonadUnliftIO m) => OutPoint -> Bool -> DB m ()
setLockCoin op locked = do
  let key = DBCoinKey $ outpointText op
  P.update key [ DBCoinLocked P.=. locked ]

{- Helpers -}

extDeriv :: SoftPath
extDeriv = Deriv :/ 0

intDeriv :: SoftPath
intDeriv = Deriv :/ 1

bip44Deriv :: Network -> Natural -> HardPath
bip44Deriv net a = Deriv :| 44 :| net.bip44Coin :| fromIntegral a

xPubIndex :: XPubKey -> Natural
xPubIndex = fromIntegral . xPubChild

addrsDerivPage :: Ctx -> SoftPath -> Page -> XPubKey -> [(Address, SoftPath)]
addrsDerivPage ctx deriv (Page lim off) xpub =
  fmap (\(a, _, i) -> (a, deriv :/ i)) addrs
  where
    addrs =
      take (fromIntegral lim) $
        derivePathAddrs ctx xpub deriv (fromIntegral off)

-- like `maybe` but for a maybe/value sandwich
joinMaybe :: b -> (a -> b) -> Maybe (Value (Maybe a)) -> b
joinMaybe d f m =
  case m of
    Just (Value m') -> maybe d f m'
    Nothing -> d
