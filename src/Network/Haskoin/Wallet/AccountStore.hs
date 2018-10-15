{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.AccountStore where

import           Control.Monad
import           Data.Aeson.Types
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import           Data.Text                               (Text)
import           Foundation
import           Foundation.Compat.Text
import           Foundation.IO
import           Foundation.VFS
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Wallet.Printer
import           Network.Haskoin.Wallet.FoundationCompat
import qualified System.Directory                        as D

type AccountsFile = Map Text AccountStore

accountsFileFromJSON :: Network -> Value -> Parser AccountsFile
accountsFileFromJSON net =
    withObject "accountsfile" $ \o -> do
        res <- forM (toList o) $ \(k, v) -> do
            a <- accountStoreFromJSON net v
            return (k, a)
        return $ fromList res

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

instance ToJSON AccountStore where
    toJSON (AccountStore k e i d) =
        object
            [ "xpubkey" .= toJSON k
            , "external" .= e
            , "internal" .= i
            , "deriv" .= d
            ]

extDeriv, intDeriv :: SoftPath
extDeriv = Deriv :/ 0
intDeriv = Deriv :/ 1

accountsFile :: Network -> IO FilePath
accountsFile net = do
    hwDir <- fromString <$> D.getAppUserDataDirectory "hw"
    let dir = hwDir </> fromString (getNetworkName net)
    D.createDirectoryIfMissing True $ filePathToLString dir
    return $ dir </> "accounts.json"

readAccountsFile :: Network -> IO AccountsFile
readAccountsFile net = do
    file <- accountsFile net
    exists <- D.doesFileExist $ filePathToLString file
    unless exists $ writeAccountsFile net Map.empty
    bytes <- readFile file
    maybe err return $ parseMaybe (accountsFileFromJSON net) =<< decodeJson bytes
  where
    err = printError "Could not decode accounts file"

writeAccountsFile :: Network -> AccountsFile -> IO ()
writeAccountsFile net dat = do
    file <- accountsFile net
    withFile file WriteMode $ \h ->
        hPut h $ encodeJsonPretty dat <> stringToBytes "\n"

newAccountStore :: Network -> AccountStore -> IO String
newAccountStore net store = do
    accMap <- readAccountsFile net
    let xpubs = accountStoreXPubKey <$> Map.elems accMap
        key
            | Map.null accMap = "main"
            | otherwise = xPubChecksum $ accountStoreXPubKey store
    when (accountStoreXPubKey store `elem` xpubs) $
        printError "This public key is already being watched"
    let f Nothing = Just store
        f _ = printError "The account name already exists"
    writeAccountsFile net $ Map.alter f (toText key) accMap
    return key

getAccountStore :: Network -> String -> IO (Maybe AccountStore)
getAccountStore net key = Map.lookup (toText key) <$> readAccountsFile net

updateAccountStore :: Network -> String -> (AccountStore -> AccountStore) -> IO ()
updateAccountStore net key f = do
    accMap <- readAccountsFile net
    let g Nothing  = printError $
            "The account " <> key <> " does not exist"
        g (Just s) = Just $ f s
    writeAccountsFile net $ Map.alter g (toText key) accMap

renameAccountStore :: Network -> String -> String -> IO ()
renameAccountStore net oldName newName
    | oldName == newName =
        printError "Both old and new names are the same"
    | otherwise = do
        accMap <- readAccountsFile net
        case Map.lookup (toText oldName) accMap of
            Just store -> do
                when (Map.member (toText newName) accMap) $
                    printError "New account name already exists"
                writeAccountsFile net $
                    Map.insert (toText newName) store $
                    Map.delete (toText oldName) accMap
            _ -> printError "Old account does not exist"

xPubChecksum :: XPubKey -> String
xPubChecksum = encodeHexStr . encodeBytes . xPubFP

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

