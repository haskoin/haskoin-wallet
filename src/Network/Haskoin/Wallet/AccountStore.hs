{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.AccountStore where

import           Control.Arrow                   (first)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Aeson                      as Json
import qualified Data.Aeson.Encode.Pretty        as Pretty
import           Data.Aeson.Types
import           Data.Bits                       (clearBit)
import qualified Data.ByteString.Char8           as C8
import qualified Data.ByteString.Lazy            as BL
import qualified Data.HashMap.Strict             as HMap
import           Data.List                       (find, nub)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe, maybe)
import qualified Data.Serialize                  as S
import           Data.String.Conversions         (cs)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Keys
import           Haskoin.Util
import           Network.Haskoin.Wallet.FileIO
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Options.Applicative.Help.Pretty hiding ((</>))
import qualified System.Directory                as D

type AccountMap = Map Text AccountStore

data AccountStore = AccountStore
    { accountStoreXPubKey  :: !XPubKey
    , accountStoreExternal :: !Natural
    , accountStoreInternal :: !Natural
    , accountStoreDeriv    :: !HardPath
    , accountStoreNetwork  :: !Network
    }
    deriving (Eq, Show)

instance FromJSON AccountStore where
    parseJSON =
        withObject "accountstore" $ \o -> do
            net <- maybe mzero return . netByName =<< o .: "network"
            AccountStore
                <$> (xPubFromJSON net =<< o .: "xpubkey")
                <*> o .: "external"
                <*> o .: "internal"
                <*> o .: "deriv"
                <*> return net

instance ToJSON AccountStore where
    toJSON (AccountStore k e i d net) =
        object
            [ "xpubkey" .= xPubToJSON net k
            , "external" .= e
            , "internal" .= i
            , "deriv" .= d
            , "network" .= getNetworkName net
            ]

-- Commit --

data Commit a
    = NoCommit { commitValue :: !a }
    | Commit { commitValue :: !a }
    deriving (Eq, Show)

toCommit :: Eq a => a -> a -> Commit a
toCommit old new =
    if old == new
       then NoCommit new
       else Commit new

-- AccountMap State --

commitAccountMap ::
       (MonadIO m, MonadError String m) => Commit AccountMap -> m AccountMap
commitAccountMap (NoCommit val) = return val
commitAccountMap (Commit val) = do
    writeAccountMap val
    return val

runAccountMapT ::
       Monad m
    => StateT AccountMap m a
    -> AccountMap
    -> m (a, Commit AccountMap)
runAccountMapT m origMap = do
    (a, newMap) <- runStateT m origMap
    return (a, toCommit origMap newMap)

execAccountMapT ::
       Monad m => StateT AccountMap m a -> AccountMap -> m (Commit AccountMap)
execAccountMapT m origMap = do
    newMap <- execStateT m origMap
    return $ toCommit origMap newMap

withAccountMap ::
       (MonadError String m, MonadIO m) => StateT AccountMap m a -> m a
withAccountMap m = do
    accMap <- readAccountMap
    (res, c) <- runAccountMapT m accMap
    _ <- commitAccountMap c
    return res

-- AccountMap IO --

accountMapFilePath :: IO FilePath
accountMapFilePath = do
    dir <- hwDataDirectory Nothing
    return $ dir </> "bip44accounts.json"

readAccountMap :: (MonadIO m, MonadError String m) => m AccountMap
readAccountMap = do
    fp <- liftIO accountMapFilePath
    exists <- liftIO $ D.doesFileExist fp
    unless exists $ writeAccountMap Map.empty
    readJsonFile fp

writeAccountMap :: (MonadIO m, MonadError String m) => AccountMap -> m ()
writeAccountMap accMap = do
    liftEither $ validAccountMap accMap
    liftIO $ do
        file <- accountMapFilePath
        writeJsonFile file accMap

validAccountMap :: AccountMap -> Either String ()
validAccountMap accMap
    | length (nub pubKeys) /= length pubKeys =
      Left "Duplicate account public keys"
    | otherwise = return ()
  where
    pubKeys = accountStoreXPubKey <$> Map.elems accMap

-- AccountMap --

accountMapKeys :: (MonadState AccountMap m, MonadError String m) => m [Text]
accountMapKeys = gets Map.keys

getAccountStore ::
       (MonadState AccountMap m, MonadError String m)
    => Maybe Text
    -> m (Text, AccountStore)
getAccountStore keyM = do
    accMap <- get
    case keyM of
        Nothing ->
            case Map.assocs accMap of
                [keyval] -> return keyval
                [] -> throwError "There are no accounts in the wallet"
                _ -> throwError "Specify which account you want to use"
        Just key ->
            case key `Map.lookup` accMap of
                Just val -> return (key, val)
                _ -> throwError $ "The account " <> cs key <> "does not exist"

getAccountStoreByDeriv ::
       (MonadState AccountMap m, MonadError String m)
    => Network
    -> Natural
    -> m (Text, AccountStore)
getAccountStoreByDeriv net acc = do
    accMap <- get
    case find ((== path) . accountStoreDeriv . snd) $ Map.assocs accMap of
        Just res -> return res
        Nothing -> throwError $ "No account exists with derivation " <> show acc
  where
    path = bip44Deriv net acc

alterAccountStore ::
      (MonadState AccountMap m, MonadError String m)
    => Text
    -> (Maybe AccountStore -> Either String (Maybe AccountStore))
    -> m (Maybe AccountStore)
alterAccountStore key f = do
    accMap <- get
    accM <- liftEither $ f (key `Map.lookup` accMap)
    put $ Map.alter (const accM) key accMap
    return accM

insertAccountStore ::
       (MonadState AccountMap m, MonadError String m)
    => Text
    -> AccountStore
    -> m ()
insertAccountStore key store =
    void $ alterAccountStore key $ \case
        Nothing -> return $ Just store
        _ -> Left "The account name already exists"

renameAccountStore ::
       (MonadState AccountMap m, MonadError String m)
    => Text
    -> Text
    -> m AccountStore
renameAccountStore oldName newName
    | oldName == newName = throwError "Old and new names are the same"
    | otherwise = do
        accMap <- get
        case Map.lookup oldName accMap of
            Just store -> do
                when (Map.member newName accMap) $
                    throwError "New account name already exists"
                put $ Map.insert newName store $ Map.delete oldName accMap
                return store
            _ -> throwError "Account does not exist"

-- AccountStore State --

commitAccountStore ::
       (MonadIO m, MonadError String m, MonadState AccountMap m)
    => Text
    -> Commit AccountStore
    -> m AccountStore
commitAccountStore _ (NoCommit val) = return val
commitAccountStore key (Commit val) = do
    _ <- alterAccountStore key $ const $ return (Just val)
    return val

runAccountStoreT ::
       Monad m
    => StateT AccountStore m a
    -> AccountStore
    -> m (a, Commit AccountStore)
runAccountStoreT m origStore = do
    (a, newStore) <- runStateT m origStore
    return (a, toCommit origStore newStore)

execAccountStoreT ::
       Monad m
    => StateT AccountStore m a
    -> AccountStore
    -> m (Commit AccountStore)
execAccountStoreT m origStore = do
    newStore <- execStateT m origStore
    return $ toCommit origStore newStore

withAccountStore ::
       (MonadIO m, MonadError String m)
    => Maybe Text
    -> (Text -> StateT AccountStore m a)
    -> m a 
withAccountStore accM m =
    withAccountMap $ do
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
        { accountStoreXPubKey = xpub
        , accountStoreExternal = 0
        , accountStoreInternal = 0
        , accountStoreDeriv = bip44Deriv net $ fromIntegral $ xPubChild xpub
        , accountStoreNetwork = net
        }

genExtAddress :: MonadState AccountStore m => m (Address, SoftPath)
genExtAddress =
    genAddress_ accountStoreExternal extDeriv $ \f s ->
        s {accountStoreExternal = f s}

genIntAddress :: MonadState AccountStore m => m (Address, SoftPath)
genIntAddress =
    genAddress_ accountStoreInternal intDeriv $ \f s ->
        s {accountStoreInternal = f s}

genAddress_ ::
       MonadState AccountStore m
    => (AccountStore -> Natural)
    -> SoftPath
    -> ((AccountStore -> Natural) -> AccountStore -> AccountStore)
    -> m (Address, SoftPath)
genAddress_ getIdx deriv updAcc = do
    store <- get
    let idx = getIdx store
        addr =
            derivePathAddr (accountStoreXPubKey store) deriv $ fromIntegral idx
        newStore = updAcc ((+ 1) . getIdx) store
    put newStore
    return (fst addr, deriv :/ fromIntegral idx)

extAddresses :: AccountStore -> [(Address, SoftPath)]
extAddresses = addresses_ accountStoreExternal extDeriv

intAddresses :: AccountStore -> [(Address, SoftPath)]
intAddresses = addresses_ accountStoreInternal intDeriv

addresses_ ::
       (AccountStore -> Natural)
    -> SoftPath
    -> AccountStore
    -> [(Address, SoftPath)]
addresses_ getIdx deriv store =
    fmap (\(a, _, i) -> (a, deriv :/ i)) addrs
  where
    xpub = accountStoreXPubKey store
    idx = fromIntegral $ getIdx store
    addrs = take idx $ derivePathAddrs xpub deriv 0

storeAddressMap :: AccountStore -> Map Address SoftPath
storeAddressMap store = Map.fromList $ extAddresses store <> intAddresses store

-- Helpers --

extDeriv :: SoftPath
extDeriv = Deriv :/ 0

intDeriv :: SoftPath
intDeriv = Deriv :/ 1

bip44Deriv :: Network -> Natural -> HardPath
bip44Deriv net a = Deriv :| 44 :| getBip44Coin net :| fromIntegral a

addrsDerivPage :: SoftPath -> Page -> XPubKey -> [(Address, SoftPath)]
addrsDerivPage deriv (Page lim off) xpub =
    fmap (\(a, _, i) -> (a, deriv :/ i)) addrs
  where
    addrs =
        take (fromIntegral lim) $ derivePathAddrs xpub deriv (fromIntegral off)

