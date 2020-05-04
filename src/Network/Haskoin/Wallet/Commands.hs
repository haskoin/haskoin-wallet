{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Haskoin.Wallet.Commands where

import           Control.Monad                       (forM, join, unless, when)
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
      { responseError         :: Text }
    | ResponseMnemonic
      { responseEntropySource :: Text
      , responseWords         :: [Text]
      }
    | ResponseCreateAcc
      { responsePubKey     :: Text
      , responseDerivation :: Text
      , responsePubKeyFile :: Text
      }
    | ResponseImportAcc
      { responseName       :: Text
      , responseDerivation :: Text
      }
    deriving (Eq, Show)

$(deriveJSON (dropSumLabels 8 8 "type") ''Response)

data DocStructure a = DocStructure
    { docStructureNetwork :: !Text
    , docStructurePayload :: !a
    } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 12) ''DocStructure)

commandResponse :: Command -> IO Response
commandResponse = \case
    CommandMnemonic d e -> mnemonic d e
    CommandCreateAcc n d -> createAcc n d

mnemonic :: Bool -> Natural -> IO Response
mnemonic useDice ent = do
    mnemE <-
        if useDice
            then genMnemonicDice ent =<< askDiceRolls
            else genMnemonic ent
    return $ case mnemE of
        Right (orig, ms) ->
            ResponseMnemonic orig (cs <$> words (cs ms))
        Left err ->
            ResponseError $ cs err
  where
    askDiceRolls =
        askInputLine $
        "Enter your " <> show (requiredRolls ent) <> " dice rolls: "

createAcc :: Network -> Natural -> IO Response
createAcc net deriv = do
    prvKeyE <- askSigningKey net (fromIntegral deriv)
    case prvKeyE of
        Right prvKey -> do
            let xpub = deriveXPubKey prvKey
                fname = "key-" <> xPubChecksum xpub
            path <- writeDoc net (cs fname) $ xPubExport net xpub
            return $
                ResponseCreateAcc
                    { responsePubKey = xPubExport net xpub
                    , responseDerivation =
                          cs $
                          show $ ParsedPrv $ toGeneric $ bip44Deriv net deriv
                    , responsePubKeyFile = cs path
                    }
        Left err -> return $ ResponseError $ cs err

importacc :: Network -> FilePath -> IO Response
importacc net fp = do
    xpubE <- readDoc net fp (xPubFromJSON net)
    case xpubE of
        Right xpub -> do
            let store =
                    AccountStore
                        xpub
                        0
                        0
                        (bip44Deriv net $ fromIntegral $ xPubChild xpub)
            name <- newAccountStore net store
            return $
                ResponseImportAcc
                    { responseName = name
                    , responseDerivation =
                          cs $
                          show $ ParsedPrv $ toGeneric $ accountStoreDeriv store
                    }
        Left err -> return $ ResponseError $ cs err

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
    -> IO (Either String a)
readDoc net fileName parser = do
    bytes <- C8.readFile fileName
    return $ do
        DocStructure fileNet val <- maybeToEither err $ decodeJson bytes
        if cs fileNet == getNetworkName net
            then maybeToEither err $ Json.parseMaybe parser val
            else Left $ badNetErr $ cs fileNet
  where
    err = "Could not read file " <> fileName
    badNetErr fileNet =
        "Bad network. This file has to be used on the network: " <> fileNet

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

askSigningKey :: Network -> Natural -> IO (Either String XPrvKey)
askSigningKey net acc = do
    mnm <- askInputLineHidden "Enter your private mnemonic: "
    case mnemonicToSeed "" (cs mnm) of
        Right _ -> do
            passStrE <- askPassword
            return $ do
                passStr <- passStrE
                signingKey net (cs passStr) (cs mnm) acc
        Left err -> return $ Left err

askPassword :: IO (Either String String)
askPassword = do
    pass <- askInputLineHidden "Mnemonic password or leave empty: "
    if null pass
        then return $ Right pass
        else do
            pass2 <- askInputLineHidden "Repeat your mnemonic password: "
            return $ if pass == pass2
               then Right pass
               else Left "The passwords did not match"
