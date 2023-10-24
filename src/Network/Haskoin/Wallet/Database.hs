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
import Control.Arrow ((&&&))
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
import Data.Either (fromRight, lefts, rights)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe, isNothing)
import Data.Serialize (decode, encode)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time
import Data.Word (Word32, Word64)
import Database.Esqueleto hiding (isNothing)
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
import Network.Haskoin.Wallet.Config
import Network.Haskoin.Wallet.FileIO
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
  balanceConfirmed Word64
  balanceUnconfirmed Word64
  balanceCoins Word64
  created UTCTime default=CURRENT_TIME
  Primary wallet derivation
  UniqueName name
  UniqueXPubKey xPubKey
  UniqueNetworkId wallet network index
  deriving Show
  deriving Eq

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
  free Bool
  created UTCTime default=CURRENT_TIME
  Primary accountWallet accountDerivation derivation
  UniqueAddress address
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show
  deriving Eq

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
  deriving Eq

DBCoin
  accountWallet DBWalletId
  accountDerivation Text
  outpoint Text
  address Text
  value Word64
  blockRef ByteString
  blob ByteString
  confirmed Bool
  locked Bool
  created UTCTime default=CURRENT_TIME
  Primary outpoint
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show
  deriving Eq

DBRawTx
  hash Text
  blob ByteString
  Primary hash
  deriving Show
  deriving Eq

DBPendingTx
  accountWallet DBWalletId
  accountDerivation Text
  nosigHash Text
  blob ByteString
  online Bool
  created UTCTime default=CURRENT_TIME
  Primary nosigHash
  Foreign DBAccount fk_wallet_derivation accountWallet accountDerivation
  deriving Show
  deriving Eq

DBBest
  network Text
  bestBlock Text
  bestHeight Int
  Primary network
  deriving Show
|]

type DB m = ReaderT SqlBackend (NoLoggingT (ResourceT m))

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
        "balance" .= toJSON (AccountBalance confirm unconfirm coins),
        "created" .= dBAccountCreated acc
      ]
    where
      confirm = dBAccountBalanceConfirmed acc
      unconfirm = dBAccountBalanceUnconfirmed acc
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
    selectOne . from $ \a -> do
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
  ExceptT String (DB m) DBAccount
insertAccount net ctx walletFP name xpub = do
  existsName <- lift $ existsAccount name
  if existsName
    then throwError $ "Account " <> cs name <> " already exists"
    else do
      existsKey <- lift $ existsXPubKey net ctx xpub
      if existsKey
        then throwError "The XPubKey already exists"
        else do
          time <- liftIO getCurrentTime
          walletId <- lift $ getWalletOrCreate walletFP
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
                    dBAccountBalanceConfirmed = 0,
                    dBAccountBalanceUnconfirmed = 0,
                    dBAccountBalanceCoins = 0,
                    dBAccountCreated = time
                  }
          lift $ P.insert_ account
          return account

-- When a name is provided, get that account or throw an error if it doesn't
-- exist. When no name is provided, return the account only if there is one
-- account.
getAccount ::
  (MonadUnliftIO m) =>
  Maybe Text ->
  ExceptT String (DB m) (DBAccountId, DBAccount)
getAccount (Just name) = do
  aM <- lift $ P.getBy $ UniqueName name
  case aM of
    Just a -> return (entityKey a, entityVal a)
    _ -> throwError $ "The account " <> cs name <> " does not exist"
getAccount Nothing = do
  as <- lift getAccounts
  case as of
    [a] -> return a
    [] -> throwError "There are no accounts in the wallet"
    _ -> throwError "Specify which account to use"

getAccountId ::
  (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) DBAccount
getAccountId accId = liftMaybe "Invalid account" =<< lift (P.get accId)

getAccountVal ::
  (MonadUnliftIO m) => Maybe Text -> ExceptT String (DB m) DBAccount
getAccountVal nameM = snd <$> getAccount nameM

getAccounts :: (MonadUnliftIO m) => DB m [(DBAccountId, DBAccount)]
getAccounts =
  (go <$>) <$> P.selectList [] [P.Asc DBAccountCreated]
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
  (MonadUnliftIO m) => Text -> Text -> ExceptT String (DB m) DBAccount
renameAccount oldName newName
  | oldName == newName = throwError "Old and new names are the same"
  | otherwise = do
      e <- lift $ existsAccount newName
      if e
        then throwError $ "The account " <> cs newName <> " already exists"
        else do
          c <-
            lift . updateCount $ \a -> do
              set a [DBAccountName =. val newName]
              where_ $ a ^. DBAccountName ==. val oldName
          if c == 0
            then throwError $ "The account " <> cs oldName <> " does not exist"
            else getAccountVal (Just newName)

updateAccountBalances :: (MonadUnliftIO m) => DBAccountId -> DB m DBAccount
updateAccountBalances accId@(DBAccountKey wallet accDeriv) = do
  confirm <- selectSum DBAddressBalanceConfirmed
  unconfirm <- selectSum DBAddressBalanceUnconfirmed
  coins <- selectSum DBAddressBalanceCoins
  update $ \a -> do
    set
      a
      [ DBAccountBalanceConfirmed =. val (unpack confirm),
        DBAccountBalanceUnconfirmed =. val (unpack unconfirm),
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
        "free" .= dBAddressFree addr,
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
        <*> o .: "free"
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
  ExceptT String (DB m) ()
updateAddressBalances net storeBals =
  forM_ storeBals $ \s -> do
    addrT <-
      liftEither $ maybeToEither "Invalid Address" (addrToText net s.address)
    lift . update $ \a -> do
      set
        a
        [ DBAddressBalanceConfirmed =. val s.confirmed,
          DBAddressBalanceUnconfirmed =. val s.unconfirmed,
          DBAddressBalanceCoins =. val s.utxo,
          DBAddressBalanceTxs =. val s.txs,
          DBAddressBalanceReceived =. val s.received
        ]
      where_ $ a ^. DBAddressAddress ==. val addrT

insertAddress ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  SoftPath ->
  Address ->
  Text ->
  Bool ->
  ExceptT String (DB m) DBAddress
insertAddress net (DBAccountKey wallet accDeriv) deriv addr label free = do
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
          free
          time
  let key = DBAddressKey wallet accDeriv derivS
  prevM <- lift $ P.get key
  when (isNothing prevM) $ lift $ P.insert_ dbAddr
  return dbAddr
  where
    parseDeriv =
      case pathToList deriv of
        [0, x] -> return (False, x) -- Not an internal address
        [1, x] -> return (True, x) -- Internal address
        _ -> throwError "Invalid address SoftPath"

-- Generate the discovered external and internal addresses
discoverAccSetDeriv ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  Natural ->
  Natural ->
  ExceptT String (DB m) DBAccount
discoverAccSetDeriv net ctx accId extCnt intCnt = do
  acc <- getAccountId accId
  let pub = accountXPubKey ctx acc
      accExt = dBAccountExternal acc
      accInt = dBAccountInternal acc
  when (accInt < fromIntegral intCnt) $ go pub DBAccountInternal intDeriv intCnt
  when (accExt < fromIntegral extCnt) $ go pub DBAccountExternal extDeriv extCnt
  getAccountId accId
  where
    go pub label deriv cnt = do
      let as = take (fromIntegral cnt) $ derivePathAddrs ctx pub deriv 0
      forM_ as $ \(a, _, i) -> insertAddress net accId (deriv :/ i) a "" False
      lift $ P.update accId [label P.=. fromIntegral cnt]

receiveAddress ::
  (MonadUnliftIO m) =>
  Ctx ->
  Maybe Text ->
  Text ->
  ExceptT String (DB m) (DBAccount, DBAddress)
receiveAddress ctx nameM label = do
  (accId, acc) <- getAccount nameM
  let net = accountNetwork acc
      pub = accountXPubKey ctx acc
      accExt = dBAccountExternal acc
  -- Verify that we respect the gap
  checkGap accId (fromIntegral accExt) False
  -- Create the address and update the account index
  let (addr, _) = derivePathAddr ctx pub extDeriv (fromIntegral accExt)
  dbAddr <-
    insertAddress net accId (extDeriv :/ fromIntegral accExt) addr label False
  newAcc <- lift $ P.updateGet accId [DBAccountExternal P.=. accExt + 1]
  return (newAcc, dbAddr)

genInternalAddress ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  ExceptT String (DB m) (Address, SoftPath)
genInternalAddress net ctx accId = do
  acc <- getAccountId accId
  let accInt = fromIntegral $ dBAccountInternal acc
      pub = accountXPubKey ctx acc
      (addr,_) = derivePathAddr ctx pub intDeriv accInt
  _ <- insertAddress net accId (intDeriv :/ accInt) addr "" True
  lift $ P.update accId [DBAccountInternal P.=. fromIntegral accInt + 1]
  return (addr, intDeriv :/ accInt)

getFreeInternalAddress ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  Natural -> -- Offset
  ExceptT String (DB m) (Address, SoftPath)
getFreeInternalAddress net ctx accId@(DBAccountKey wallet accDeriv) offset' = do
  -- List the free addresses in ascending index order
  resM <- lift . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        &&. a ^. DBAddressInternal ==. val True
        &&. a ^. DBAddressFree ==. val True
    orderBy [asc $ a ^. DBAddressIndex]
    limit 1
    offset $ fromIntegral offset'
    return (a ^. DBAddressAddress, a ^. DBAddressIndex)
  case resM of
    Just (Value addrT, Value idx) -> do
      addr <- liftMaybe "Address" $ textToAddr net addrT
      return (addr, intDeriv :/ fromIntegral idx)
    Nothing -> do
      -- Generate an address and try again
      _ <- genInternalAddress net ctx accId
      getFreeInternalAddress net ctx accId offset'

lastUsedAddress ::
  (MonadUnliftIO m) => DBAccountId -> Bool -> DB m (Maybe Natural)
lastUsedAddress (DBAccountKey wallet accDeriv) internal = do
  resM <- (flatMaybe <$>) . selectOne . from $ \a -> do
    where_ $
      a ^. DBAddressAccountWallet ==. val wallet
        &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        &&. a ^. DBAddressInternal ==. val internal
        &&. a ^. DBAddressBalanceTxs >. val 0
    return $ max_ $ a ^. DBAddressIndex
  return $ fromIntegral <$> resM

checkGap ::
  (MonadUnliftIO m) =>
  DBAccountId ->
  Natural ->
  Bool ->
  ExceptT String (DB m) ()
checkGap accId adrIdx internal = do
  usedIdxM <- lift $ lastUsedAddress accId internal
  for_ usedIdxM $ \usedIdx ->
    when (fromIntegral adrIdx > usedIdx + gap) $
      throwError $
        "Can not generate addresses beyond the gap of " <> show gap

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
  ExceptT String (DB m) (Map Address SoftPath, Map Address AddressBalance)
allAddressesMap net (DBAccountKey wallet accDeriv) = do
  dbRes <-
    lift . select $
      from $ \a -> do
        where_ $
          a ^. DBAddressAccountWallet ==. val wallet
            &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        return a
  res <-
    forM dbRes $ \(Entity _ dbAddr) -> do
      a <- liftEither $ textToAddrE net $ dBAddressAddress dbAddr
      d <-
        liftEither . maybeToEither "parsePath failed" $
          parseSoft . cs $
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
  ExceptT String (DB m) (DBAccount, DBAddress)
updateLabel nameM idx label = do
  (DBAccountKey wallet accDeriv, acc) <- getAccount nameM
  let path = cs $ pathToStr $ extDeriv :/ idx
      aKey = DBAddressKey wallet accDeriv path
  unless (fromIntegral idx < dBAccountExternal acc) $
    throwError $
      "Address " <> show idx <> " does not exist"
  adr <- lift $ P.updateGet aKey [DBAddressLabel P.=. label]
  return (acc, adr)

getCoinDeriv ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  Store.Unspent ->
  DB m (Either String SoftPath)
getCoinDeriv net accId unspent =
  runExceptT $ do
    addr <-
      liftEither . maybeToEither "getCoinDeriv: no address" $ unspent.address
    liftEither <=< lift $ getAddrDeriv net accId addr

getAddrDeriv ::
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  Address ->
  DB m (Either String SoftPath)
getAddrDeriv net (DBAccountKey wallet accDeriv) addr =
  runExceptT $ do
    addrT <-
      liftEither . maybeToEither "getAddrDeriv: no address" $ addrToText net addr
    derivM <-
      lift . selectOne . from $ \a -> do
        where_ $
          a ^. DBAddressAddress ==. val addrT
            &&. a ^. DBAddressAccountWallet ==. val wallet
            &&. a ^. DBAddressAccountDerivation ==. val accDeriv
        return $ a ^. DBAddressDerivation
    liftEither . maybeToEither "getAddrDeriv: no derivation" $
      parseSoft . cs . unValue =<< derivM

{- Transactions -}

existsTx :: (MonadUnliftIO m) => DBAccountId -> TxHash -> DB m Bool
existsTx (DBAccountKey wallet accDeriv) txid =
  isJust <$> P.get (DBTxInfoKey wallet accDeriv $ txHashToHex txid)

getConfirmedTxs ::
  (MonadUnliftIO m) => DBAccountId -> Bool -> ExceptT String (DB m) [TxHash]
getConfirmedTxs (DBAccountKey wallet accDeriv) confirm = do
  ts <-
    lift . select $
      from $ \t -> do
        where_ $
          t ^. DBTxInfoAccountWallet ==. val wallet
            &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
            &&. t ^. DBTxInfoConfirmed ==. val confirm
        orderBy [asc (t ^. DBTxInfoBlockRef)]
        return $ t ^. DBTxInfoTxid
  forM ts $ \(Value t) ->
    liftEither $ maybeToEither "getConfirmedTxs invalid TxHash" $ hexToTxHash t

-- Insert a new transaction or replace it, if it already exists
-- Returns True if there was a change or an insert
repsertTxInfo ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  TxInfo ->
  DB m (DBTxInfo, Bool)
repsertTxInfo net ctx accId tif = do
  time <- liftIO getCurrentTime
  let confirmed' = Store.confirmed $ txInfoBlockRef tif
      tid = txHashToHex $ txInfoHash tif
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
      return (newTxInfo, res /= newTxInfo)
    Nothing -> do
      P.insert_ txInfo
      return (txInfo, True)

txsPage ::
  (MonadUnliftIO m) =>
  Ctx ->
  Maybe Text ->
  Page ->
  ExceptT String (DB m) (DBAccount, [TxInfo])
txsPage ctx nameM (Page lim off) = do
  (DBAccountKey wallet accDeriv, acc) <- getAccount nameM
  let net = accountNetwork acc
  dbTxs <-
    lift . select . from $ \t -> do
      where_ $
        t ^. DBTxInfoAccountWallet ==. val wallet
          &&. t ^. DBTxInfoAccountDerivation ==. val accDeriv
      orderBy [asc (t ^. DBTxInfoBlockRef)]
      limit $ fromIntegral lim
      offset $ fromIntegral off
      return $ t ^. DBTxInfoBlob
  res <-
    forM dbTxs $ \(Value dbTx) -> do
      liftEither . maybeToEither "TxInfo unmarshalJSON Failed" $
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

data JsonCoin = JsonCoin
  { jsonCoinOutpoint :: !OutPoint,
    jsonCoinAddress :: !Address,
    jsonCoinValue :: !Word64,
    jsonCoinBlock :: !Store.BlockRef,
    jsonCoinConfirmations :: !Natural,
    jsonCoinLocked :: !Bool
  }
  deriving (Eq, Show)

instance MarshalJSON Network JsonCoin where
  marshalValue net c =
    object
      [ "outpoint" .= jsonCoinOutpoint c,
        "address" .= marshalValue net (jsonCoinAddress c),
        "value" .= jsonCoinValue c,
        "block" .= jsonCoinBlock c,
        "confirmations" .= jsonCoinConfirmations c,
        "locked" .= jsonCoinLocked c
      ]
  unmarshalValue net =
    withObject "JsonCoin" $ \o ->
      JsonCoin
        <$> o .: "outpoint"
        <*> (unmarshalValue net =<< o .: "address")
        <*> o .: "value"
        <*> o .: "block"
        <*> o .: "confirmations"
        <*> o .: "locked"

toJsonCoin :: Network -> BlockHeight -> DBCoin -> Either String JsonCoin
toJsonCoin net bestHeight dbCoin = do
  op <- textToOutpoint $ dBCoinOutpoint dbCoin
  ad <-
    maybeToEither "toJsonCoin: Invalid address" $
      textToAddr net $
        dBCoinAddress dbCoin
  br <- decode $ dBCoinBlockRef dbCoin
  let confirmations =
        case br of
          Store.BlockRef h _ ->
            if bestHeight < fromIntegral h
              then 0
              else fromIntegral bestHeight - fromIntegral h + 1
          Store.MemRef _ -> 0
  return $
    JsonCoin
      { jsonCoinOutpoint = op,
        jsonCoinAddress = ad,
        jsonCoinValue = dBCoinValue dbCoin,
        jsonCoinBlock = br,
        jsonCoinConfirmations = confirmations,
        jsonCoinLocked = dBCoinLocked dbCoin
      }

outpointText :: OutPoint -> Text
outpointText = encodeHex . encode

textToOutpoint :: Text -> Either String OutPoint
textToOutpoint t = do
  bs <- maybeToEither "textToOutpoint: invalid input" $ decodeHex t
  decode bs

-- Get all coins in an account, spendable or not
coinPage :: (MonadUnliftIO m) => DBAccountId -> Page -> DB m [DBCoin]
coinPage (DBAccountKey wallet accDeriv) (Page lim off) = do
  coins <-
    select . from $ \c -> do
      where_ $ do
        c ^. DBCoinAccountWallet ==. val wallet
          &&. c ^. DBCoinAccountDerivation ==. val accDeriv
      orderBy [asc (c ^. DBCoinBlockRef)]
      limit $ fromIntegral lim
      offset $ fromIntegral off
      return c
  return $ entityVal <$> coins

-- Spendable coins must be confirmed and not locked
getSpendableCoins ::
  (MonadUnliftIO m) => DBAccountId -> ExceptT String (DB m) [Store.Unspent]
getSpendableCoins (DBAccountKey wallet accDeriv) = do
  coins <- lift . select . from $ \c -> do
    where_ $
      c ^. DBCoinAccountWallet ==. val wallet
        &&. c ^. DBCoinAccountDerivation ==. val accDeriv
        &&. c ^. DBCoinConfirmed ==. val True
        &&. c ^. DBCoinLocked ==. val False
    return c
  let bss = dBCoinBlob . entityVal <$> coins
  mapM (liftEither . decode) bss

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
            dBCoinValue = unspent.value,
            dBCoinBlockRef = encode unspent.block,
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
      DBCoinConfirmed P.=. Store.confirmed unspent.block,
      DBCoinBlockRef P.=. encode unspent.block
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
  (MonadUnliftIO m) =>
  Network ->
  DBAccountId ->
  [Address] ->
  [Store.Unspent] ->
  ExceptT String (DB m) (Int, [DBCoin])
updateCoins net accId addrsToUpdate allUnspent = do
  let storeMap = groupCoins net allUnspent
  addrsToUpdateE <-
    mapM (liftEither . maybeToEither "Addr" . addrToText net) addrsToUpdate
  res <- forM addrsToUpdateE $ \addr -> do
    let storeCoins = fromMaybe [] $ Map.lookup addr storeMap
    localCoins <- lift $ getCoinsByAddr addr
    let storeOps = outpointText . (.outpoint) <$> storeCoins
        localOps = dBCoinOutpoint <$> localCoins
        toDelete = filter ((`notElem` storeOps) . dBCoinOutpoint) localCoins
        toInsert =
          filter ((`notElem` localOps) . outpointText . (.outpoint)) storeCoins
        toUpdate = filter (f localCoins) storeCoins
    lift $ forM_ toDelete deleteCoin
    lift $ forM_ toUpdate updateCoin
    newCoins <- lift $ forM toInsert $ insertCoin accId addr
    return (length toDelete + length toUpdate + length toInsert, newCoins)
  return (sum $ fst <$> res, concatMap snd res)
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

setLockCoin :: (MonadUnliftIO m) => OutPoint -> Bool -> DB m Natural
setLockCoin op locked = do
  cnt <- updateCount $ \c -> do
    set c [DBCoinLocked =. val locked]
    where_ $ c ^. DBCoinOutpoint ==. val (outpointText op)
  return $ fromIntegral cnt

{- Raw Transactions -}

insertRawTx :: (MonadUnliftIO m) => Tx -> DB m ()
insertRawTx tx = do
  let hash = txHashToHex $ txHash tx
      key = DBRawTxKey hash
  P.repsert key $ DBRawTx hash (encode tx)

getRawTx :: (MonadUnliftIO m) => TxHash -> ExceptT String (DB m) Tx
getRawTx hash = do
  let key = DBRawTxKey $ txHashToHex hash
  txM <- lift $ P.get key
  case txM of
    Just tx -> liftEither . decode $ dBRawTxBlob tx
    Nothing -> throwError "getRawTx: missing transaction"

{- Pending Transactions -}

getPendingTx :: (MonadUnliftIO m) => TxHash -> DB m (Maybe (TxSignData, Bool))
getPendingTx nosigHash = do
  let hashT = txHashToHex nosigHash
      key = DBPendingTxKey hashT
  resM <- P.get key
  case resM of
    Just (DBPendingTx _ _ _ blob online _) -> do
      let tsdM = Json.decode $ BS.fromStrict blob
      return $ (,online) <$> tsdM
    _ -> return Nothing

pendingTxPage ::
  (MonadUnliftIO m) =>
  DBAccountId ->
  Page ->
  ExceptT String (DB m) [(TxHash, TxSignData)]
pendingTxPage (DBAccountKey wallet accDeriv) (Page lim off) = do
  tsds <-
    lift . select . from $ \p -> do
      where_ $ do
        p ^. DBPendingTxAccountWallet ==. val wallet
          &&. p ^. DBPendingTxAccountDerivation ==. val accDeriv
      orderBy [desc (p ^. DBPendingTxCreated)]
      limit $ fromIntegral lim
      offset $ fromIntegral off
      return p
  forM tsds $ \(Entity _ res) -> do
    tsd <- liftEither . Json.eitherDecode . BS.fromStrict $ dBPendingTxBlob res
    nosigHash <-
      liftEither $
        maybeToEither "TxHash" $
          hexToTxHash $
            dBPendingTxNosigHash res
    return (nosigHash, tsd)

-- Returns the TxHash and NoSigHash of pending transactions.
-- They are compared during a sync in order to delete pending transactions that
-- are now online.
pendingTxHashes ::
  (MonadUnliftIO m) =>
  DBAccountId ->
  ExceptT String (DB m) [(TxHash, DBPendingTxId)]
pendingTxHashes (DBAccountKey wallet accDeriv) = do
  blobs <-
    lift . select . from $ \p -> do
      where_ $ do
        p ^. DBPendingTxAccountWallet ==. val wallet
          &&. p ^. DBPendingTxAccountDerivation ==. val accDeriv
      return (p ^. DBPendingTxBlob, p ^. DBPendingTxId)
  res <- forM blobs $ \(Value blob, Value key) -> do
    tsd <- liftEither . Json.eitherDecode $ BS.fromStrict blob
    if txSignDataSigned tsd
      then return [(txHash $ txSignDataTx tsd, key)]
      else return []
  return $ concat res

-- Import a pending transaction and locks coins, commits addresses
-- and updates the account internal index
importPendingTx ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  TxSignData ->
  ExceptT String (DB m) TxHash
importPendingTx net ctx accId tsd@(TxSignData _ _ _ _ signed) = do
  acc <- getAccountId accId
  let nosigHash = nosigTxHash $ txSignDataTx tsd
      bs = BS.toStrict $ Json.encode tsd
  prevM <- lift $ getPendingTx nosigHash
  case prevM of
    Just (TxSignData _ _ _ _ prevSigned, online) -> do
      when online $
        throwError "The transaction is already online"
      when (prevSigned && not signed) $
        throwError "Can not replace a signed transaction with an unsigned one"
      when (not prevSigned && signed) $ do
        let key = DBPendingTxKey $ txHashToHex nosigHash
        lift $ P.update key [DBPendingTxBlob P.=. bs]
      return nosigHash
    Nothing -> do
      let (outpoints, addrsE) = findTxSignData ctx acc tsd
      -- Verify coins and lock them
      forM_ outpoints $ \outpoint -> do
        coinM <- lift $ P.get $ DBCoinKey $ outpointText outpoint
        case coinM of
          Just coin -> do
            when (dBCoinLocked coin) $
              throwError "A coin referenced by the transaction is locked"
            lift $ setLockCoin outpoint True
          _ -> throwError "A coin referenced by the transaction does not exist"
      -- Verify addresses
      let intAddrs = lefts addrsE
          extAddrs = rights addrsE
      intAddrsT <- mapM (liftMaybe "Addrs" . addrToText net . fst) intAddrs
      extAddrsT <- mapM (liftMaybe "Addrs" . addrToText net . fst) extAddrs
      intAddrsEnt <- lift . select . from $ \a -> do
        where_ $ a ^. DBAddressAddress `in_` valList intAddrsT
        return a
      extAddrsEnt <- lift . select . from $ \a -> do
        where_ $ a ^. DBAddressAddress `in_` valList extAddrsT
        return a
      when (length intAddrsT /= length intAddrsEnt) $
        throwError "Some of the internal addresses do not exist"
      when (length extAddrsT /= length extAddrsEnt) $
        throwError "Some of the external addresses do not exist"
      unless (all (dBAddressFree . entityVal) intAddrsEnt) $
        throwError "Some of the internal addresses are not free"
      -- Remove the free status of internal addresses
      _ <- lift $ setFreeAddrs False intAddrsT
      -- Check the internal address gap
      let maxM
            | null intAddrs = Nothing
            | otherwise = Just $ maximum $ snd <$> intAddrs
      for_ maxM $ \maxIdx -> checkGap accId (fromIntegral maxIdx) True
      -- Insert the pending transaction
      time <- liftIO getCurrentTime
      let ptx =
            DBPendingTx
              (dBAccountWallet acc)
              (dBAccountDerivation acc)
              (txHashToHex nosigHash)
              bs
              False
              time
      lift $ P.insert_ ptx
      return nosigHash

findTxSignData ::
  Ctx ->
  DBAccount ->
  TxSignData ->
  ([OutPoint], [Either (Address, Word32) (Address, Word32)])
findTxSignData ctx acc (TxSignData tx _ _ op _) =
  let xpub = accountXPubKey ctx acc
      outpoints = (.outpoint) <$> tx.inputs
      f p =
        case pathToList p of
          [0, i] -> Right (fst $ derivePathAddr ctx xpub extDeriv i, i)
          [1, i] -> Left (fst $ derivePathAddr ctx xpub intDeriv i, i)
          _ -> error "Invalid path"
   in (outpoints, f <$> op)

-- Delete a pending transaction, unlocks coins and frees internal addresses
deletePendingTx ::
  (MonadUnliftIO m) =>
  Network ->
  Ctx ->
  DBAccountId ->
  TxHash ->
  ExceptT String (DB m) (Natural, Natural)
deletePendingTx net ctx accId nosigHash = do
  let key = DBPendingTxKey $ txHashToHex nosigHash
  tsdM <- lift $ getPendingTx nosigHash
  case tsdM of
    Just (_, True) -> do
      lift $ P.delete key -- The transaction is online. Just delete it.
      return (0, 0)
    Just (tsd, _) -> do
      acc <- getAccountId accId
      let (outpoints, addrsE) = findTxSignData ctx acc tsd
      intAddrsT <-
        mapM (liftMaybe "Address" . addrToText net) $
          fst <$> lefts addrsE
      -- We only free coins and addresses if the transaction is offline
      freedCoins <- forM outpoints $ \op -> lift $ setLockCoin op False
      freedAddresses <- lift $ setFreeAddrs True intAddrsT
      lift $ P.delete key
      return (sum freedCoins, fromIntegral freedAddresses)
    _ -> throwError "The pending transaction does not exist"

-- When the pending transaction is online, we just delete it
pendingTxOnline :: (MonadUnliftIO m) => DBPendingTxId -> DB m ()
pendingTxOnline = P.delete

setFreeAddrs :: (MonadUnliftIO m) => Bool -> [Text] -> DB m Natural
setFreeAddrs free addrs = do
  (fromIntegral <$>) . updateCount $ \a -> do
    set a [DBAddressFree =. val free]
    where_ $ a ^. DBAddressAddress `in_` valList addrs

setPendingTxOnline :: (MonadUnliftIO m) => TxHash -> DB m Natural
setPendingTxOnline nosigH = do
  let nosigHT = txHashToHex nosigH
  cnt <- updateCount $ \p -> do
    set p [DBPendingTxOnline =. val True]
    where_ $ p ^. DBPendingTxNosigHash ==. val nosigHT
  return $ fromIntegral cnt

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

flatMaybe :: Maybe (Value (Maybe a)) -> Maybe a
flatMaybe = joinMaybe Nothing Just
