{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
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
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Keys
import           Haskoin.Util                        (dropFieldLabel,
                                                      dropSumLabels,
                                                      maybeToEither)
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.WalletTx
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
      { responsePubKey     :: XPubKey
      , responseDerivation :: HardPath
      , responsePubKeyFile :: Text
      , responseNetwork    :: Network
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
      , responseAddresses   :: [(Address, SoftPath)]
      }
    | ResponseReceive
      { responseAccountName :: Text
      , responseAccount     :: AccountStore
      , responseAddress     :: (Address, SoftPath)
      }
    | ResponseTransactions
      { responseAccountName  :: Text
      , responseAccount      :: AccountStore
      , responseTransactions :: [WalletTx]
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
    CommandAddresses accM p -> addresses accM p
    CommandReceive accM -> receive accM
    CommandTransactions accM p -> transactions accM p

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
                { responsePubKey = xpub
                , responseDerivation = bip44Deriv net deriv
                , responsePubKeyFile = cs path
                , responseNetwork = net
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

addresses :: Maybe Text -> Page -> IO Response
addresses accM page =
    catchResponseError $ do
        (key, store) <- getAccountStore accM
        let net = accountStoreNetwork store
            addrs = toPage page $ reverse $ extAddresses store
        return $ ResponseAddresses key store addrs

textStr2 :: Network -> (Address, a) -> Either String (Text, a)
textStr2 net (a, b) = (,b) <$> addrToStringE net a

receive :: Maybe Text -> IO Response
receive accM =
    catchResponseError $ do
        (key, store) <- getAccountStore accM
        let (addr, store') = genExtAddress store
            net = accountStoreNetwork store
        newStore <- commit key store'
        return $ ResponseReceive key newStore addr

transactions :: Maybe Text -> Page -> IO Response
transactions accM page =
    catchResponseError $ do
        (key, store) <- getAccountStore accM
        let net = accountStoreNetwork store
        withNetwork net $ do
            let allAddrs = extAddresses store <> intAddresses store
                addrMap = Map.fromList allAddrs
            txs <- httpAddrTxs (fst <$> allAddrs) page
            let walletTxs = fromStoreTransaction addrMap <$> txs
            return $ ResponseTransactions key store walletTxs

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
