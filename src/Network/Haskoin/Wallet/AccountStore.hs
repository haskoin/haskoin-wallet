{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.AccountStore where

import           Control.Arrow                   (first)
import           Control.Monad
import qualified Data.Aeson                      as Json
import qualified Data.Aeson.Encode.Pretty        as Pretty
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Char8           as C8
import qualified Data.HashMap.Strict             as HMap
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
import           Network.Haskoin.Wallet.Doc
import           Numeric.Natural
import           Options.Applicative.Help.Pretty hiding ((</>))
import qualified System.Directory                as D

type AccountsMap = Map Text AccountStore

accountsMapFromJSON :: Network -> Value -> Parser AccountsMap
accountsMapFromJSON net =
    withObject "accountsfile" $ \o -> do
        res <- forM (HMap.toList o) $ \(k, v) -> do
            a <- accountStoreFromJSON net v
            return (k, a)
        return $ Map.fromList res

accountsMapToJSON :: Network -> AccountsMap -> Value
accountsMapToJSON net accMap =
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

accountsFilePath :: Network -> IO FilePath
accountsFilePath net = do
    hwDir <- D.getAppUserDataDirectory "hw"
    let dir = hwDir </> getNetworkName net
    D.createDirectoryIfMissing True dir
    return $ dir </> "accounts.json"

readAccountsMap :: Network -> IO AccountsMap
readAccountsMap net = do
    fp <- accountsFilePath net
    exists <- D.doesFileExist fp
    unless exists $ writeAccountsMap net Map.empty
    bytes <- C8.readFile fp
    maybe err return $
        parseMaybe (accountsMapFromJSON net) =<< Json.decodeStrict bytes
  where
    err = exitError "Could not decode accounts file"

writeAccountsMap :: Network -> AccountsMap -> IO ()
writeAccountsMap net dat = do
    file <- accountsFilePath net
    C8.writeFile file $ encPretty $ accountsMapToJSON net dat
  where
    encPretty =
        BL.toStrict .
        Pretty.encodePretty'
            Pretty.defConfig
                { Pretty.confIndent = Pretty.Spaces 2
                , Pretty.confTrailingNewline = True
                }

newAccountStore :: Network -> AccountStore -> IO Text
newAccountStore net store = do
    accMap <- readAccountsMap net
    let xpubs = accountStoreXPubKey <$> Map.elems accMap
        key
            | Map.null accMap = "main"
            | otherwise = xPubChecksum $ accountStoreXPubKey store
    when (accountStoreXPubKey store `elem` xpubs) $
        exitError "This public key is already being watched"
    let f Nothing = Just store
        f _       = exitError "The account name already exists"
    writeAccountsMap net $ Map.alter f key accMap
    return key

getAccountStore :: Network -> Text -> IO (Maybe AccountStore)
getAccountStore net key = Map.lookup key <$> readAccountsMap net

updateAccountStore :: Network -> Text -> (AccountStore -> AccountStore) -> IO ()
updateAccountStore net key f = do
    accMap <- readAccountsMap net
    let g Nothing  = exitError $
            "The account " <> Text.unpack key <> " does not exist"
        g (Just s) = Just $ f s
    writeAccountsMap net $ Map.alter g key accMap

renameAccountStore :: Network -> Text -> Text -> IO ()
renameAccountStore net oldName newName
    | oldName == newName =
        exitError "Both old and new names are the same"
    | otherwise = do
        accMap <- readAccountsMap net
        case Map.lookup oldName accMap of
            Just store -> do
                when (Map.member newName accMap) $
                    exitError "New account name already exists"
                writeAccountsMap net $
                    Map.insert newName store $
                    Map.delete oldName accMap
            _ -> exitError "Old account does not exist"

withAccountStore ::
       Network -> String -> ((String, AccountStore) -> IO ()) -> IO ()
withAccountStore net name f
    | null name = do
        accMap <- readAccountsMap net
        case Map.assocs accMap of
            [val] -> f (first cs val)
            _ ->
                case Map.lookup "main" accMap of
                    Just val -> f ("main", val)
                    _ -> err $ cs <$> Map.keys accMap
    | otherwise = do
        accM <- getAccountStore net (cs name)
        case accM of
            Just acc -> f (name, acc)
            _ -> err . fmap cs . Map.keys =<< readAccountsMap net
  where
    err :: [String] -> IO ()
    err [] = exitError "No accounts have been created"
    err keys =
        exitCustomError $
        vcat
            [ errorDoc
                  "Select one of the following accounts with -a or --account"
            , indent 4 $ vcat $ fmap (accountDoc . text) keys
            ]


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

