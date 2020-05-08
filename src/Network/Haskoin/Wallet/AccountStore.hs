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
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.Util
import           Network.Haskoin.Wallet.FileIO
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

lookupAccountStore :: Text -> ExceptT String IO (Maybe AccountStore)
lookupAccountStore key = Map.lookup key <$> readAccountMap

withAccountStore ::
       Text
    -> (AccountStore -> ExceptT String IO ())
    -> ExceptT String IO ()
withAccountStore key f = do
    storeM <- lookupAccountStore key
    case storeM of
        Just store -> f store
        _          -> throwError "Account does not exist"

alterAccountStore ::
       Text
    -> (Maybe AccountStore -> Either String (Maybe AccountStore))
    -> ExceptT String IO ()
alterAccountStore key f = do
    accMap <- readAccountMap
    accM <- liftEither $ f (key `Map.lookup` accMap)
    let newMap = Map.alter (const accM) key accMap
    when (accMap /= newMap) $ writeAccountMap newMap

insertAccountStore :: Text -> AccountStore -> ExceptT String IO ()
insertAccountStore key store = do
    alterAccountStore key $ \case
        Nothing -> return $ Just store
        _ -> Left "The account name already exists"

adjustAccountStore ::
       Text -> (AccountStore -> AccountStore) -> ExceptT String IO ()
adjustAccountStore key f =
    alterAccountStore key $ \case
        Nothing ->
            Left $ "The account " <> Text.unpack key <> " does not exist"
        Just store -> return $ Just $ f store

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

nextExtAddress :: AccountStore -> ((Address, SoftPath, Natural), AccountStore)
nextExtAddress store =
    ( ( fst $ derivePathAddr (accountStoreXPubKey store) extDeriv idx
      , extDeriv :/ idx
      , nat
      )
    , store{ accountStoreExternal = nat + 1 }
    )
  where
    nat = accountStoreExternal store
    idx = fromIntegral nat

nextIntAddress :: AccountStore -> ((Address, SoftPath, Natural), AccountStore)
nextIntAddress store =
    ( ( fst $ derivePathAddr (accountStoreXPubKey store) intDeriv idx
      , intDeriv :/ idx
      , nat
      )
    , store{ accountStoreInternal = nat + 1 }
    )
  where
    nat = accountStoreInternal store
    idx = fromIntegral nat

allExtAddresses :: AccountStore -> [(Address, SoftPath)]
allExtAddresses store =
    fmap (\(a,_,i) -> (a, extDeriv :/ i)) extAddrs
  where
    xpub     = accountStoreXPubKey store
    extI     = fromIntegral $ accountStoreExternal store
    extAddrs = take extI $ derivePathAddrs xpub extDeriv 0

allIntAddresses :: AccountStore -> [(Address, SoftPath)]
allIntAddresses store =
    fmap (\(a,_,i) -> (a, intDeriv :/ i)) intAddrs
  where
    xpub     = accountStoreXPubKey store
    intI     = fromIntegral $ accountStoreInternal store
    intAddrs = take intI $ derivePathAddrs xpub intDeriv 0

