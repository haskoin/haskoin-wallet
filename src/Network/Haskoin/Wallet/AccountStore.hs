{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Network.Haskoin.Wallet.AccountStore where

import           Control.Arrow                   (first)
import           Control.Monad
import           Control.Monad.Except
import qualified Data.Aeson                      as Json
import qualified Data.Aeson.Encode.Pretty        as Pretty
import           Data.Aeson.Types
import qualified Data.ByteString.Char8           as C8
import qualified Data.ByteString.Lazy            as BL
import qualified Data.HashMap.Strict             as HMap
import           Data.List                       (nub)
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
    { accountStoreXPubKey  :: XPubKey
    , accountStoreExternal :: Natural
    , accountStoreInternal :: Natural
    , accountStoreDeriv    :: HardPath
    , accountStoreNetwork  :: Network
    } deriving (Eq, Show)

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
    dir <- D.getAppUserDataDirectory "hw"
    D.createDirectoryIfMissing True dir
    return $ dir </> "bip44accounts.json"

readAccountMap :: ExceptT String IO AccountMap
readAccountMap = do
    fp <- liftIO accountMapFilePath
    exists <- liftIO $ D.doesFileExist fp
    unless exists $ writeAccountMap Map.empty
    readJsonFile fp

writeAccountMap :: AccountMap -> ExceptT String IO ()
writeAccountMap accMap = do
    liftEither $ validAccountMap accMap
    liftIO $ do
        file <- accountMapFilePath
        writeJsonFile file accMap

validAccountMap :: AccountMap -> Either String ()
validAccountMap accMap
    | (length $ nub pubKeys) /= length pubKeys =
      Left "Duplicate account public keys"
    | otherwise = return ()
  where
    pubKeys = accountStoreXPubKey <$> Map.elems accMap

accountMapKeys :: ExceptT String IO [Text]
accountMapKeys = Map.keys <$> readAccountMap

getAccountStore :: Maybe Text -> ExceptT String IO (Text, AccountStore)
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

alterAccountStore ::
       Text
    -> (Maybe AccountStore -> Either String (Maybe AccountStore))
    -> ExceptT String IO (Maybe AccountStore)
alterAccountStore key f = do
    accMap <- readAccountMap
    accM <- liftEither $ f (key `Map.lookup` accMap)
    let newMap = Map.alter (const accM) key accMap
    when (accMap /= newMap) $ writeAccountMap newMap
    return accM

insertAccountStore :: Text -> AccountStore -> ExceptT String IO ()
insertAccountStore key store =
    void $ alterAccountStore key $ \case
        Nothing -> return $ Just store
        _ -> Left "The account name already exists"

adjustAccountStore ::
       Text -> (AccountStore -> AccountStore) -> ExceptT String IO AccountStore
adjustAccountStore key f = do
    accM <- alterAccountStore key $ \case
        Nothing -> Left $ "The account " <> Text.unpack key <> " does not exist"
        Just store -> return $ Just $ f store
    liftEither $ maybeToEither "Account was Nothing" accM

renameAccountStore :: Text -> Text -> ExceptT String IO AccountStore
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
    = NoCommit { commitValue :: a }
    | Commit { commitValue :: a }

commit :: Text -> Commit AccountStore -> ExceptT String IO AccountStore
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
genAddress_ getIdx deriv updAcc store = do
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

