{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Serialize                  as S
import           Data.String.Conversions         (cs)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Options.Applicative.Help.Pretty hiding ((</>))
import qualified System.Directory                as D

type AccountMap = Map Text AccountStore

accountMapFromJSON :: Network -> Value -> Parser AccountMap
accountMapFromJSON net =
    withObject "accountsfile" $ \o -> do
        res <- forM (HMap.toList o) $ \(k, v) -> do
            a <- accountStoreFromJSON net v
            return (k, a)
        return $ Map.fromList res

accountMapToJSON :: Network -> AccountMap -> Value
accountMapToJSON net accMap =
    object $ Map.assocs $ Map.map (accountStoreToJSON net) accMap

data AccountStore = AccountStore
    { accountStoreXPubKey  :: !XPubKey
    , accountStoreExternal :: !Natural
    , accountStoreInternal :: !Natural
    , accountStoreDeriv    :: !HardPath
    } deriving (Eq, Show)

accountStoreFromJSON :: Network -> Value -> Parser AccountStore
accountStoreFromJSON net =
    withObject "accountstore" $ \o ->
        AccountStore <$> (xPubFromJSON net =<< o .: "xpubkey") <*>
        o .: "external" <*>
        o .: "internal" <*>
        o .: "deriv"

accountStoreToJSON :: Network -> AccountStore -> Value
accountStoreToJSON net (AccountStore k e i d) =
    object
        [ "xpubkey"  .= xPubToJSON net k
        , "external" .= e
        , "internal" .= i
        , "deriv"    .= d
        ]

extDeriv, intDeriv :: SoftPath
extDeriv = Deriv :/ 0
intDeriv = Deriv :/ 1

(</>) :: String -> String -> String
a </> b = a <> "/" <> b

accountMapFilePath :: Network -> IO FilePath
accountMapFilePath net = do
    hwDir <- D.getAppUserDataDirectory "hw"
    let dir = hwDir </> getNetworkName net
    D.createDirectoryIfMissing True dir
    return $ dir </> "accounts.json"

readAccountMap :: Network -> ExceptT String IO AccountMap
readAccountMap net = do
    fp <- liftIO $ accountMapFilePath net
    exists <- liftIO $ D.doesFileExist fp
    unless exists $ writeAccountMap net Map.empty
    bytes <- liftIO $ C8.readFile fp
    maybe err return $
        parseMaybe (accountMapFromJSON net) =<< Json.decodeStrict bytes
  where
    err = throwError "Could not decode accounts file"

writeAccountMap :: Network -> AccountMap -> ExceptT String IO ()
writeAccountMap net accMap = do
    liftEither $ validAccountMap accMap
    liftIO $ do
        file <- accountMapFilePath net
        C8.writeFile file $ encodeJsonPrettyLn $ accountMapToJSON net accMap

validAccountMap :: AccountMap -> Either String ()
validAccountMap accMap
    | (length $ nub pubKeys) /= length pubKeys =
      Left "Duplicate account public keys"
    | otherwise = return ()
  where
    pubKeys = accountStoreXPubKey <$> Map.elems accMap

accountMapKeys :: Network -> ExceptT String IO [Text]
accountMapKeys net = Map.keys <$> readAccountMap net

lookupAccountStore :: Network -> Text -> ExceptT String IO (Maybe AccountStore)
lookupAccountStore net key = Map.lookup key <$> readAccountMap net

withAccountStore ::
       Network
    -> Text
    -> (AccountStore -> ExceptT String IO ())
    -> ExceptT String IO ()
withAccountStore net key f = do
    storeM <- lookupAccountStore net key
    case storeM of
        Just store -> f store
        _ -> throwError "Account does not exist"

alterAccountStore ::
       Network
    -> Text
    -> (Maybe AccountStore -> Either String (Maybe AccountStore))
    -> ExceptT String IO ()
alterAccountStore net key f = do
    accMap <- readAccountMap net
    accM <- liftEither $ f (key `Map.lookup` accMap)
    let newMap = Map.alter (const accM) key accMap
    when (accMap /= newMap) $ writeAccountMap net newMap

insertAccountStore :: Network -> Text -> AccountStore -> ExceptT String IO ()
insertAccountStore net key store = do
    alterAccountStore net key $ \case
        Nothing -> return $ Just store
        _ -> Left "The account name already exists"

adjustAccountStore ::
       Network -> Text -> (AccountStore -> AccountStore) -> ExceptT String IO ()
adjustAccountStore net key f =
    alterAccountStore net key $ \case
        Nothing ->
            Left $ "The account " <> Text.unpack key <> " does not exist"
        Just store -> return $ Just $ f store

renameAccountStore :: Network -> Text -> Text -> ExceptT String IO ()
renameAccountStore net oldName newName
    | oldName == newName =
        throwError "Old and new names are the same"
    | otherwise = do
        accMap <- readAccountMap net
        case Map.lookup oldName accMap of
            Just store -> do
                when (Map.member newName accMap) $
                    throwError "New account name already exists"
                writeAccountMap net $
                    Map.insert newName store $
                    Map.delete oldName accMap
            _ -> throwError "Account does not exist"

xPubChecksum :: XPubKey -> Text
xPubChecksum = encodeHex . S.encode . xPubFP

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

