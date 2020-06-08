{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.AccountStore where

import           Control.Arrow                   (first)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
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
            AccountStore <$> (xPubFromJSON net =<< o .: "xpubkey") <*>
                o .: "external" <*>
                o .: "internal" <*>
                o .: "deriv" <*>
                return net

instance ToJSON AccountStore where
    toJSON (AccountStore k e i d net) =
        object
            [ "xpubkey" .= xPubToJSON net k
            , "external" .= e
            , "internal" .= i
            , "deriv" .= d
            , "network" .= getNetworkName net
            ]

extDeriv, intDeriv :: SoftPath
extDeriv = Deriv :/ 0
intDeriv = Deriv :/ 1

bip44Deriv :: Network -> Natural -> HardPath
bip44Deriv net a = Deriv :| 44 :| getBip44Coin net :| fromIntegral a

accountStoreAccount :: AccountStore -> Either String Natural
accountStoreAccount as =
    case pathToList $ accountStoreDeriv as of
        [] -> Left "Invalid account derivation"
        xs -> return $ fromIntegral $ (`clearBit` 31) $ last xs

newAccountStore :: Network -> XPubKey -> AccountStore
newAccountStore net xpub =
    AccountStore
        { accountStoreXPubKey = xpub
        , accountStoreExternal = 0
        , accountStoreInternal = 0
        , accountStoreDeriv = bip44Deriv net $ fromIntegral $ xPubChild xpub
        , accountStoreNetwork = net
        }

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

accountMapKeys :: (MonadIO m, MonadError String m) => m [Text]
accountMapKeys = Map.keys <$> readAccountMap

getAccountStore ::
       (MonadIO m, MonadError String m) => Maybe Text -> m (Text, AccountStore)
getAccountStore keyM = do
    accMap <- readAccountMap
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
       (MonadIO m, MonadError String m, MonadReader Network m)
    => Natural
    -> m (Text, AccountStore)
getAccountStoreByDeriv acc = do
    net <- network
    let path = bip44Deriv net acc
    accMap <- readAccountMap
    case find ((== path) . accountStoreDeriv . snd) $ Map.assocs accMap of
        Just res -> return res
        Nothing -> throwError $ "No account exists with derivation " <> show acc

alterAccountStore ::
      (MonadIO m, MonadError String m)
    => Text
    -> (Maybe AccountStore -> Either String (Maybe AccountStore))
    -> m (Maybe AccountStore)
alterAccountStore key f = do
    accMap <- readAccountMap
    accM <- liftEither $ f (key `Map.lookup` accMap)
    let newMap = Map.alter (const accM) key accMap
    when (accMap /= newMap) $ writeAccountMap newMap
    return accM

insertAccountStore ::
       (MonadIO m, MonadError String m) => Text -> AccountStore -> m ()
insertAccountStore key store =
    void $ alterAccountStore key $ \case
        Nothing -> return $ Just store
        _ -> Left "The account name already exists"

adjustAccountStore ::
       (MonadIO m, MonadError String m)
    => Text
    -> (AccountStore -> AccountStore)
    -> m AccountStore
adjustAccountStore key f = do
    accM <- alterAccountStore key $ \case
        Nothing -> Left $ "The account " <> Text.unpack key <> " does not exist"
        Just store -> return $ Just $ f store
    liftEither $ maybeToEither "Account was Nothing" accM

renameAccountStore ::
       (MonadIO m, MonadError String m) => Text -> Text -> m AccountStore
renameAccountStore oldName newName
    | oldName == newName =
        throwError "Old and new names are the same"
    | otherwise = do
        accMap <- readAccountMap
        case Map.lookup oldName accMap of
            Just store -> do
                when (Map.member newName accMap) $
                    throwError "New account name already exists"
                writeAccountMap $
                    Map.insert newName store $
                    Map.delete oldName accMap
                return store
            _ -> throwError "Account does not exist"

data Commit a
    = NoCommit
          { commitValue :: !a
          }
    | Commit
          { commitValue :: !a
          }

commit ::
       (MonadIO m, MonadError String m)
    => Text
    -> Commit AccountStore
    -> m AccountStore
commit _ (NoCommit val) = return val
commit key (Commit val) = do
    void $ alterAccountStore key $ const $ return (Just val)
    return val

genExtAddress, genIntAddress ::
       AccountStore -> ((Address, SoftPath), Commit AccountStore)
genExtAddress =
    genAddress_ accountStoreExternal extDeriv $ \f s ->
        s {accountStoreExternal = f s}
genIntAddress =
    genAddress_ accountStoreInternal intDeriv $ \f s ->
        s {accountStoreInternal = f s}

genAddress_ ::
       (AccountStore -> Natural)
    -> SoftPath
    -> ((AccountStore -> Natural) -> AccountStore -> AccountStore)
    -> AccountStore
    -> ((Address, SoftPath), Commit AccountStore)
genAddress_ getIdx deriv updAcc store =
    ((fst addr, deriv :/ (fromIntegral idx)), Commit newStore)
  where
    idx = getIdx store
    addr = derivePathAddr (accountStoreXPubKey store) deriv $ fromIntegral idx
    newStore = updAcc ((+ 1) . getIdx) store

extAddresses, intAddresses :: AccountStore -> [(Address, SoftPath)]
extAddresses = addresses_ accountStoreExternal extDeriv
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
