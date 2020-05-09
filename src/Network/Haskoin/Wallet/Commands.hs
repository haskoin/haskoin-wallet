{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Haskoin.Wallet.Commands where

import           Control.Arrow                       (first)
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
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Util                (dropFieldLabel,
                                                      dropSumLabels,
                                                      maybeToEither)
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.DetailedTx
import           Network.Haskoin.Wallet.Entropy
import           Network.Haskoin.Wallet.FileIO
import           Network.Haskoin.Wallet.HTTP
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
      { responsePubKey     :: Text
      , responseDerivation :: HardPath
      , responsePubKeyFile :: Text
      , responseNetwork    :: Text
      }
    | ResponseImportAcc
      { responseAccountName :: Text
      , responseAccount     :: AccountStore
      }
    | ResponseRenameAcc
      { responseOldName :: Text
      , responseNewName :: Text
      , responseAccount :: AccountStore
      }
    | ResponseAccounts
      { responseAccounts :: AccountMap
      }
    | ResponseAddresses
      { responseAccountName :: Text
      , responseAccount     :: AccountStore
      , responseAddresses   :: [(Text, SoftPath, Natural)]
      }
    | ResponseReceive
      { responseAccountName :: Text
      , responseAccount     :: AccountStore
      , responseAddress     :: (Text, SoftPath, Natural)
      }
    | ResponseTransactions
      { responseAccountName  :: Text
      , responseAccount      :: AccountStore
      , responseTransactions :: [DetailedTx]
      }
    deriving (Eq, Show)

$(deriveJSON (dropSumLabels 8 8 "type") ''Response)

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
    CommandImportAcc f k -> importAcc f k
    CommandRenameAcc old new -> renameAcc old new
    CommandAccounts -> accounts
    CommandAddresses accM c -> addresses accM c
    CommandReceive accM -> receive accM
    CommandTransactions accM -> transactions accM

mnemonic :: Bool -> Natural -> IO Response
mnemonic useDice ent =
    catchResponseError $ do
        (orig, ms) <-
            if useDice
                then genMnemonicDice ent =<< askDiceRolls ent
                else genMnemonic ent
        return $ ResponseMnemonic orig (cs <$> words (cs ms))

-- TODO: Ask the dice rolls in sequences of 5 or so
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
        path <- liftIO $ writeDoc $ PubKeyDoc xpub net
        return $
            ResponseCreateAcc
                { responsePubKey = xPubExport net xpub
                , responseDerivation = bip44Deriv net deriv
                , responsePubKeyFile = cs path
                , responseNetwork = cs $ getNetworkName net
                }

importAcc :: FilePath -> Text -> IO Response
importAcc fp name =
    catchResponseError $ do
        PubKeyDoc xpub net <- readJsonFile fp
        let store = newAccountStore net xpub
        insertAccountStore name store
        return $ ResponseImportAcc name store

renameAcc :: Text -> Text -> IO Response
renameAcc oldName newName =
    catchResponseError $ do
        store <- renameAccountStore oldName newName
        return $ ResponseRenameAcc oldName newName store

accounts :: IO Response
accounts = catchResponseError $ ResponseAccounts <$> readAccountMap

addresses :: Maybe Text -> Natural -> IO Response
addresses accM cnt =
    catchResponseError $ do
        (key, store) <- getAccountStore accM
        let net = accountStoreNetwork store
            addrs = lastList cnt $ extAddresses store
        addrsRes <- liftEither $ mapM (addrText3 net) addrs
        return $ ResponseAddresses key store addrsRes

addrText3 :: Network
  -> (Address, SoftPath, Natural)
  -> Either String (Text, SoftPath, Natural)
addrText3 net (addr, path, i) = do
    addrStr <- addrToStringE net addr
    return (addrStr, path, i)

receive :: Maybe Text -> IO Response
receive accM =
    catchResponseError $ do
        (key, store) <- getAccountStore accM
        let (res, store') = genExtAddress store
        addrRes <- liftEither $ addrText3 (accountStoreNetwork store) res
        newStore <- commit key store'
        return $ ResponseReceive key newStore addrRes

transactions :: Maybe Text -> IO Response
transactions accM =
    catchResponseError $ do
        (key, store) <- getAccountStore accM
        undefined

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
