{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Haskoin.Wallet.AccountStore where

import Control.Monad (MonadPlus (mzero), unless, void, when)
import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
    execStateT,
    gets,
  )
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
  )
import Data.Bits (clearBit)
import Data.List (find, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin.Address (Address)
import Haskoin.Crypto
  ( Ctx,
    DerivPathI (Deriv, (:/), (:|)),
    HardPath,
    SoftPath,
    XPubKey,
    derivePathAddr,
    derivePathAddrs,
    pathToList,
    xPubChild,
  )
import Haskoin.Network (Network (bip44Coin, name), netByName)
import Haskoin.Util (MarshalJSON (marshalValue, unmarshalValue))
import Network.Haskoin.Wallet.FileIO
  ( hwDataDirectory,
    readJsonFile,
    readMarshalFile,
    writeJsonFile,
  )
import Network.Haskoin.Wallet.Util (Page (Page), (</>))
import Numeric.Natural (Natural)
import qualified System.Directory as D
import Conduit (MonadUnliftIO)
import Data.Word (Word32)

newtype AccountMap = AccountMap {getAccountMap :: Map Text AccountStore}
  deriving (Eq, Show)

instance MarshalJSON Ctx AccountMap where
  marshalValue ctx (AccountMap m) = toJSON $ Map.map (marshalValue ctx) m
  unmarshalValue ctx v = do
    m <- parseJSON v
    AccountMap <$> mapM (unmarshalValue ctx) m

data AccountStore = AccountStore
  { accountStoreXPubKey :: !XPubKey,
    accountStoreExternal :: !Natural,
    accountStoreInternal :: !Natural,
    accountStoreDeriv :: !HardPath,
    accountStoreNetwork :: !Network
  }
  deriving (Eq, Show)

instance MarshalJSON Ctx AccountStore where
  marshalValue ctx (AccountStore k e i d net) =
    object
      [ "xpubkey" .= marshalValue (net, ctx) k,
        "external" .= e,
        "internal" .= i,
        "deriv" .= d,
        "network" .= net.name
      ]
  unmarshalValue ctx =
    withObject "accountstore" $ \o -> do
      net <- maybe mzero return . netByName =<< o .: "network"
      AccountStore
        <$> (unmarshalValue (net, ctx) =<< o .: "xpubkey")
        <*> o .: "external"
        <*> o .: "internal"
        <*> o .: "deriv"
        <*> return net

newtype LabelMap = LabelMap {getLabelMap :: Map Text (Map Natural Text)}
  deriving (Eq, Show)

instance ToJSON LabelMap where
  toJSON (LabelMap m) = toJSON m

instance FromJSON LabelMap where
  parseJSON v = do
    m <- parseJSON v
    return $ LabelMap m

-- LabelMap IO --

labelMapFilePath :: IO FilePath
labelMapFilePath = do
  dir <- hwDataDirectory Nothing
  return $ dir </> "labels.json"

readLabelMap :: IO (Either String LabelMap)
readLabelMap = do
  fp <- labelMapFilePath
  exists <- D.doesFileExist fp
  unless exists $ writeLabelMap $ LabelMap Map.empty
  readJsonFile fp

writeLabelMap :: LabelMap -> IO ()
writeLabelMap labelMap = do
  file <- labelMapFilePath
  writeJsonFile file $ toJSON labelMap

readAccountLabels ::
  (MonadIO m, MonadError String m) => Text -> m (Map Natural Text)
readAccountLabels accName = do
  LabelMap m <- liftEither =<< liftIO readLabelMap
  case Map.lookup accName m of
    Just labels -> return labels
    Nothing -> do
      liftIO $ writeLabelMap $ LabelMap $ Map.insert accName Map.empty m
      return Map.empty

writeAccountLabel ::
  (MonadIO m, MonadError String m) => Text -> Natural -> Text -> m ()
writeAccountLabel accName d label = do
  LabelMap m <- liftEither =<< liftIO readLabelMap
  let lMap = case Map.lookup accName m of
        Just x -> x
        _ -> Map.empty
      resMap = Map.insert d label lMap
  liftIO $ writeLabelMap $ LabelMap $ Map.insert accName resMap m

-- Commit --

data Commit a
  = NoCommit {commitValue :: !a}
  | Commit {commitValue :: !a}
  deriving (Eq, Show)

toCommit :: (Eq a) => a -> a -> Commit a
toCommit old new =
  if old == new
    then NoCommit new
    else Commit new

-- AccountMap State --

commitAccountMap :: Ctx -> Commit AccountMap -> IO AccountMap
commitAccountMap _ (NoCommit val) = return val
commitAccountMap ctx (Commit val) = do
  writeAccountMap ctx val
  return val

runAccountMapT ::
  Monad m =>
  StateT AccountMap m a ->
  AccountMap ->
  m (a, Commit AccountMap)
runAccountMapT m origMap = do
  (a, newMap) <- runStateT m origMap
  return (a, toCommit origMap newMap)

execAccountMapT ::
  Monad m => StateT AccountMap m a -> AccountMap -> m (Commit AccountMap)
execAccountMapT m origMap = do
  newMap <- execStateT m origMap
  return $ toCommit origMap newMap

withAccountMap ::
  (MonadError String m, MonadIO m) => Ctx -> StateT AccountMap m a -> m a
withAccountMap ctx m = do
  accMap <- liftEither =<< liftIO (readAccountMap ctx)
  (res, c) <- runAccountMapT m accMap
  _ <- liftIO $ commitAccountMap ctx c
  return res

-- AccountMap IO --

accountMapFilePath :: IO FilePath
accountMapFilePath = do
  dir <- hwDataDirectory Nothing
  return $ dir </> "bip44accounts.json"

readAccountMap :: Ctx -> IO (Either String AccountMap)
readAccountMap ctx = do
  fp <- accountMapFilePath
  exists <- D.doesFileExist fp
  unless exists $ writeAccountMap ctx $ AccountMap Map.empty
  readMarshalFile ctx fp

writeAccountMap :: Ctx -> AccountMap -> IO ()
writeAccountMap ctx accMap = do
  file <- accountMapFilePath
  writeJsonFile file $ marshalValue ctx accMap

-- AccountMap --

accountMapKeys :: (MonadState AccountMap m, MonadError String m) => m [Text]
accountMapKeys = gets (Map.keys . getAccountMap)

getAccountStore ::
  (MonadState AccountMap m, MonadError String m) =>
  Maybe Text ->
  m (Text, AccountStore)
getAccountStore keyM = do
  accMap <- gets getAccountMap
  case keyM of
    Nothing ->
      case Map.assocs accMap of
        [keyval] -> return keyval
        [] -> throwError "There are no accounts in the wallet"
        _ -> throwError "Specify which account you want to use"
    Just key ->
      case key `Map.lookup` accMap of
        Just val -> return (key, val)
        _ -> throwError $ "The account " <> cs key <> " does not exist"

getAccountStoreByDeriv ::
  (MonadState AccountMap m, MonadError String m) =>
  Network ->
  Natural ->
  m (Text, AccountStore)
getAccountStoreByDeriv net acc = do
  accMap <- gets getAccountMap
  case find ((== path) . accountStoreDeriv . snd) $ Map.assocs accMap of
    Just res -> return res
    Nothing -> throwError $ "No account exists with derivation " <> show acc
  where
    path = bip44Deriv net acc

alterAccountStore ::
  (MonadState AccountMap m, MonadError String m) =>
  Text ->
  (Maybe AccountStore -> Either String (Maybe AccountStore)) ->
  m (Maybe AccountStore)
alterAccountStore key f = do
  accMap <- gets getAccountMap
  accM <- liftEither $ f (key `Map.lookup` accMap)
  let newMap = AccountMap $ Map.alter (const accM) key accMap
  unless (validAccountMap newMap) $
    throwError "Duplicate account public keys"
  put newMap
  return accM

validAccountMap :: AccountMap -> Bool
validAccountMap (AccountMap accMap) =
  length (nub pubKeys) == length pubKeys
  where
    pubKeys = accountStoreXPubKey <$> Map.elems accMap

insertAccountStore ::
  (MonadState AccountMap m, MonadError String m) =>
  Text ->
  AccountStore ->
  m ()
insertAccountStore key store =
  void $ alterAccountStore key $ \case
    Nothing -> return $ Just store
    _ -> Left "The account name already exists"

renameAccountStore ::
  (MonadState AccountMap m, MonadError String m) =>
  Text ->
  Text ->
  m AccountStore
renameAccountStore oldName newName
  | oldName == newName = throwError "Old and new names are the same"
  | otherwise = do
      accMap <- gets getAccountMap
      case Map.lookup oldName accMap of
        Just store -> do
          when (Map.member newName accMap) $
            throwError "New account name already exists"
          put $
            AccountMap $
              Map.insert newName store $
                Map.delete oldName accMap
          return store
        _ -> throwError "Account does not exist"

-- AccountStore State --

commitAccountStore ::
  (MonadError String m, MonadState AccountMap m) =>
  Text ->
  Commit AccountStore ->
  m AccountStore
commitAccountStore _ (NoCommit val) = return val
commitAccountStore key (Commit val) = do
  _ <- alterAccountStore key $ const $ return (Just val)
  return val

runAccountStoreT ::
  (Monad m) =>
  StateT AccountStore m a ->
  AccountStore ->
  m (a, Commit AccountStore)
runAccountStoreT m origStore = do
  (a, newStore) <- runStateT m origStore
  return (a, toCommit origStore newStore)

execAccountStoreT ::
  (Monad m) =>
  StateT AccountStore m a ->
  AccountStore ->
  m (Commit AccountStore)
execAccountStoreT m origStore = do
  newStore <- execStateT m origStore
  return $ toCommit origStore newStore

withAccountStore ::
  (MonadIO m, MonadError String m) =>
  Ctx ->
  Maybe Text ->
  (Text -> StateT AccountStore m a) ->
  m a
withAccountStore ctx accM m =
  withAccountMap ctx $ do
    (key, store) <- getAccountStore accM
    (res, c) <- lift $ runAccountStoreT (m key) store
    _ <- commitAccountStore key c
    return res

-- AccountStore --

accountStoreAccount :: AccountStore -> Either String Natural
accountStoreAccount as =
  case pathToList $ accountStoreDeriv as of
    [] -> Left "Invalid account derivation"
    xs -> return $ fromIntegral $ (`clearBit` 31) $ last xs

emptyAccountStore :: Network -> XPubKey -> AccountStore
emptyAccountStore net xpub =
  AccountStore
    { accountStoreXPubKey = xpub,
      accountStoreExternal = 0,
      accountStoreInternal = 0,
      accountStoreDeriv = bip44Deriv net $ fromIntegral $ xPubChild xpub,
      accountStoreNetwork = net
    }

genExtAddress :: (MonadState AccountStore m) => Ctx -> m (Address, SoftPath)
genExtAddress ctx =
  genAddress_ ctx accountStoreExternal extDeriv $ \f s ->
    s {accountStoreExternal = f s}

genIntAddress :: (MonadState AccountStore m) => Ctx -> m (Address, SoftPath)
genIntAddress ctx =
  genAddress_ ctx accountStoreInternal intDeriv $ \f s ->
    s {accountStoreInternal = f s}

genAddress_ ::
  (MonadState AccountStore m) =>
  Ctx ->
  (AccountStore -> Natural) ->
  SoftPath ->
  ((AccountStore -> Natural) -> AccountStore -> AccountStore) ->
  m (Address, SoftPath)
genAddress_ ctx getIdx deriv updAcc = do
  store <- get
  let idx = getIdx store
      addr =
        derivePathAddr ctx (accountStoreXPubKey store) deriv $
          fromIntegral idx
      newStore = updAcc ((+ 1) . getIdx) store
  put newStore
  return (fst addr, deriv :/ fromIntegral idx)

extAddresses :: Ctx -> AccountStore -> [(Address, SoftPath)]
extAddresses ctx = addresses_ ctx accountStoreExternal extDeriv

intAddresses :: Ctx -> AccountStore -> [(Address, SoftPath)]
intAddresses ctx = addresses_ ctx accountStoreInternal intDeriv

addresses_ ::
  Ctx ->
  (AccountStore -> Natural) ->
  SoftPath ->
  AccountStore ->
  [(Address, SoftPath)]
addresses_ ctx getIdx deriv store =
  fmap (\(a, _, i) -> (a, deriv :/ i)) addrs
  where
    xpub = accountStoreXPubKey store
    idx = fromIntegral $ getIdx store
    addrs = take idx $ derivePathAddrs ctx xpub deriv 0

storeAddressMap :: Ctx -> AccountStore -> Map Address SoftPath
storeAddressMap ctx store =
  Map.fromList $ extAddresses ctx store <> intAddresses ctx store

-- Helpers --

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
