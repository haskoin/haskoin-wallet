{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Haskoin.Wallet.Commands where

import           Control.Monad                       (forM, join, unless, when)
import           Control.Monad.Except
import qualified Data.Aeson                          as Json
import           Data.Aeson.TH
import qualified Data.Aeson.Types                    as Json
import qualified Data.ByteString.Char8               as C8
import           Data.Foldable                       (asum)
import           Data.List                           (isPrefixOf, nub, sort)
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (fromMaybe)
import           Data.String                         (unwords)
import           Data.String.Conversions             (cs)
import           Data.Text                           (Text)
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Util                (dropFieldLabel,
                                                      dropSumLabels,
                                                      maybeToEither)
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.Entropy
import           Network.Haskoin.Wallet.Parser
import           Network.Haskoin.Wallet.Signing
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Options.Applicative
import           Options.Applicative.Help.Pretty     hiding ((</>))
import qualified System.Console.Haskeline            as Haskeline
import qualified System.Directory                    as D
import           System.IO                           (IOMode (..), withFile)

data Response
    = ResponseError
      { responseError         :: Text
      }
    | ResponseMnemonic
      { responseEntropySource :: Text
      , responseWords         :: [Text]
      }
    | ResponseCreateAcc
      { responsePubKey        :: Text
      , responseDerivation    :: Text
      , responsePubKeyFile    :: Text
      }
    | ResponseImportAcc
      { responseName          :: Text
      , responseDerivation    :: Text
      }
    deriving (Eq, Show)

$(deriveJSON (dropSumLabels 8 8 "type") ''Response)

data DocStructure a = DocStructure
    { docStructureNetwork :: !Text
    , docStructurePayload :: !a
    } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 12) ''DocStructure)

catchResponseError :: ExceptT String IO Response -> IO Response
catchResponseError m = do
    resE <- runExceptT m
    case resE of
        Left err  -> return $ ResponseError $ cs err
        Right res -> return res

commandResponse :: Command -> IO Response
commandResponse = \case
    CommandMnemonic d e -> mnemonic d e
    CommandCreateAcc n d -> createAcc n d
    CommandImportAcc n f k -> importAcc n f k

mnemonic :: Bool -> Natural -> IO Response
mnemonic useDice ent =
    catchResponseError $ do
        (orig, ms) <-
            if useDice
                then genMnemonicDice ent =<< askDiceRolls ent
                else genMnemonic ent
        return $ ResponseMnemonic orig (cs <$> words (cs ms))

askDiceRolls :: Natural -> ExceptT String IO String
askDiceRolls ent = do
    roll1 <- liftIO $ askInputLineHidden $
        "Enter your " <> show (requiredRolls ent) <> " dice rolls: "
    roll2 <- liftIO $ askInputLineHidden "Enter your dice rolls again:"
    unless (roll1 == roll2) $
        throwError "Dice rolls do not match"
    return roll1

createAcc :: Network -> Natural -> IO Response
createAcc net deriv =
    catchResponseError $ do
        prvKey <- askSigningKey net deriv
        let xpub = deriveXPubKey prvKey
            fname = "key-" <> xPubChecksum xpub
        path <- liftIO $ writeDoc net (cs fname) $ xPubExport net xpub
        return $
            ResponseCreateAcc
                { responsePubKey = xPubExport net xpub
                , responseDerivation =
                      cs $ show $ ParsedPrv $ toGeneric $ bip44Deriv net deriv
                , responsePubKeyFile = cs path
                }

importAcc :: Network -> FilePath -> Text -> IO Response
importAcc net fp name =
    catchResponseError $ do
        xpub <- readDoc net fp (xPubFromJSON net)
        let store =
                AccountStore
                    xpub
                    0
                    0
                    (bip44Deriv net $ fromIntegral $ xPubChild xpub)
        insertAccountStore net name store
        return $
            ResponseImportAcc
                { responseName = name
                , responseDerivation =
                      cs $
                      show $ ParsedPrv $ toGeneric $ accountStoreDeriv store
                }

{- File IO Helpers -}

writeDoc :: Json.ToJSON a => Network -> String -> a -> IO FilePath
writeDoc net fileName dat = do
    dir <- D.getUserDocumentsDirectory
    let path = dir </> (network <> "-" <> fileName <> ".json")
        val = encodeJsonPretty $ DocStructure (cs network) dat
    withFile path WriteMode (`C8.hPutStrLn` val)
    return path
  where
    network = getNetworkName net

readDoc ::
       Network
    -> FilePath
    -> (Json.Value -> Json.Parser a)
    -> ExceptT String IO a
readDoc net fileName parser = do
    bytes <- liftIO $ C8.readFile fileName
    DocStructure fileNet val <- liftEither $ Json.eitherDecodeStrict' bytes
    if cs fileNet == getNetworkName net
        then liftEither $ Json.parseEither parser val
        else throwError $
             "Bad network. This file has to be used on the network: " <>
             cs fileNet

{- Haskeline Helpers -}

askInputLineHidden :: String -> IO String
askInputLineHidden msg = do
    inputM <-
        Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getPassword (Just '*') msg
    maybe
        (error "No action due to EOF")
        return
        inputM

askInputLine :: String -> IO String
askInputLine msg = do
    inputM <-
        Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getInputLine msg
    maybe
        (error "No action due to EOF")
        return
        inputM

askSigningKey :: Network -> Natural -> ExceptT String IO XPrvKey
askSigningKey net acc = do
    mnm <- liftIO $ askInputLineHidden "Enter your private mnemonic: "
    -- We validate the mnemonic before asking the password
    _ <- liftEither $ mnemonicToSeed "" (cs mnm)
    passStr <- askPassword
    liftEither $ signingKey net (cs passStr) (cs mnm) acc

askPassword :: ExceptT String IO String
askPassword = do
    pass <- liftIO $ askInputLineHidden "Mnemonic password or leave empty: "
    if null pass
        then return pass
        else do
            pass2 <-
                liftIO $ askInputLineHidden "Repeat your mnemonic password: "
            if pass == pass2
                then return pass
                else throwError "The passwords did not match"
