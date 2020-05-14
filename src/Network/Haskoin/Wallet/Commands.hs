{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.Commands where

import           Control.Arrow                       (first)
import           Control.Monad                       (forM, join, unless, when)
import           Control.Monad.Except
import           Data.Aeson                          (object, (.:), (.=))
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
import qualified Haskoin.Store.Data                  as Store
import           Haskoin.Util                        (dropFieldLabel,
                                                      dropSumLabels,
                                                      maybeToEither)
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.Entropy
import           Network.Haskoin.Wallet.FileIO
import           Network.Haskoin.Wallet.HTTP
import           Network.Haskoin.Wallet.Parser
import           Network.Haskoin.Wallet.Signing
import           Network.Haskoin.Wallet.Util
import           Network.Haskoin.Wallet.WalletTx
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
      , responseMnemonic      :: [Text]
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
    | ResponseBalance
      { responseAccountName :: Text
      , responseAccount     :: AccountStore
      , responseBalance     :: AccountBalance
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
    | ResponsePrepareTx
      { responseAccountName  :: Text
      , responseAccount      :: AccountStore
      , responseTxFile       :: Text
      , responseTransaction  :: WalletUnsignedTx
      }
    deriving (Eq, Show)

jsonError :: String -> Json.Value
jsonError err = object ["type" .= Json.String "error", "error" .= err]

instance Json.ToJSON Response where
    toJSON =
        \case
            ResponseError err -> jsonError $ cs err
            ResponseMnemonic e w ->
                object
                    [ "type" .= Json.String "mnemonic"
                    , "entropysource" .= e
                    , "mnemonic" .= w
                    ]
            ResponseCreateAcc p d f net ->
                object
                    [ "type" .= Json.String "createacc"
                    , "pubkey" .= xPubToJSON net p
                    , "derivation" .= d
                    , "pubkeyfile" .= f
                    , "network" .= getNetworkName net
                    ]
            ResponseImportAcc n a ->
                object
                    [ "type" .= Json.String "importacc"
                    , "accountname" .= n
                    , "account" .= a
                    ]
            ResponseRenameAcc o n a ->
                object
                    [ "type" .= Json.String "renameacc"
                    , "oldname" .= o
                    , "newname" .= n
                    , "account" .= a
                    ]
            ResponseAccounts a ->
                object ["type" .= Json.String "accounts", "accounts" .= a]
            ResponseBalance n a b ->
                object
                    [ "type" .= Json.String "balance"
                    , "accountname" .= n
                    , "account" .= a
                    , "balance" .= b
                    ]
            ResponseAddresses n a addrs ->
                case mapM (addrToText2 (accountStoreNetwork a)) addrs of
                    Right xs ->
                        object
                            [ "type" .= Json.String "addresses"
                            , "accountname" .= n
                            , "account" .= a
                            , "addresses" .= xs
                            ]
                    Left err -> jsonError err
            ResponseReceive n a addr -> do
                case addrToText2 (accountStoreNetwork a) addr of
                    Right x ->
                        object
                            [ "type" .= Json.String "receive"
                            , "accountname" .= n
                            , "account" .= a
                            , "address" .= x
                            ]
                    Left err -> jsonError err
            ResponseTransactions n a txs ->
                case mapM (walletTxToJSON (accountStoreNetwork a)) txs of
                    Right txsJ ->
                        object
                            [ "type" .= Json.String "transactions"
                            , "accountname" .= n
                            , "account" .= a
                            , "transactions" .= txsJ
                            ]
                    Left err -> jsonError err

instance Json.FromJSON Response where
    parseJSON =
        Json.withObject "response" $ \o -> do
            Json.String resType <- o .: "type"
            case resType of
                "error" -> ResponseError <$> o .: "error"
                "mnemonic" -> do
                    e <- o .: "entropysource"
                    m <- o .: "mnemonic"
                    return $ ResponseMnemonic e m
                "createacc" -> do
                    net <- maybe mzero return . netByName =<< o .: "network"
                    p <- xPubFromJSON net =<< o .: "pubkey"
                    d <- o .: "derivation"
                    f <- o .: "pubkeyfile"
                    return $ ResponseCreateAcc p d f net
                "importacc" -> do
                    n <- o .: "accountname"
                    a <- o .: "account"
                    return $ ResponseImportAcc n a
                "renameacc" -> do
                    old <- o .: "oldname"
                    new <- o .: "newname"
                    a <- o .: "account"
                    return $ ResponseRenameAcc old new a
                "accounts" -> do
                    as <- o .: "accounts"
                    return $ ResponseAccounts as
                "balance" -> do
                    n <- o .: "accountname"
                    a <- o .: "account"
                    b <- o .: "balance"
                    return $ ResponseBalance n a b
                "addresses" -> do
                    n <- o .: "accountname"
                    a <- o .: "account"
                    let f = textToAddr2 (accountStoreNetwork a)
                    xs <- either fail return . mapM f =<< o .: "addresses"
                    return $ ResponseAddresses n a xs
                "receive" -> do
                    n <- o .: "accountname"
                    a <- o .: "account"
                    let f = textToAddr2 (accountStoreNetwork a)
                    x <- either fail return . f =<< o .: "address"
                    return $ ResponseReceive n a x
                "transactions" -> do
                    n <- o .: "accountname"
                    a <- o .: "account"
                    let f = walletTxParseJSON (accountStoreNetwork a)
                    txs <- mapM f =<< o .: "transactions"
                    return $ ResponseTransactions n a txs
                _ -> fail "Invalid JSON response type"

data AccountBalance = AccountBalance
    { balanceAmount        :: !Natural
      -- ^ confirmed balance
    , balanceZero          :: !Natural
      -- ^ unconfirmed balance
    , balanceUnspentCount  :: !Natural
      -- ^ number of unspent outputs
    , balanceTxCount       :: !Natural
      -- ^ number of transactions
    , balanceTotalReceived :: !Natural
      -- ^ total amount from all outputs in this address
    } deriving (Show, Read, Eq, Ord)

instance Json.ToJSON AccountBalance where
    toJSON b =
        object $
        [ "confirmed" .= balanceAmount b
        , "unconfirmed" .= balanceZero b
        , "utxo" .= balanceUnspentCount b
        , "txs" .= balanceTxCount b
        , "received" .= balanceTotalReceived b
        ]

instance Json.FromJSON AccountBalance where
    parseJSON =
        Json.withObject "accountbalance" $ \o -> do
            AccountBalance <$> o .: "confirmed"
                           <*> o .: "unconfirmed"
                           <*> o .: "utxo"
                           <*> o .: "txs"
                           <*> o .: "received"

addrsToAccBalance :: [Store.Balance] -> AccountBalance
addrsToAccBalance xs =
    AccountBalance
        { balanceAmount = fromIntegral $ sum $ Store.balanceAmount <$> xs
        , balanceZero = fromIntegral $ sum $ Store.balanceZero <$> xs
        , balanceUnspentCount =
              fromIntegral $ sum $ Store.balanceUnspentCount <$> xs
        , balanceTxCount = fromIntegral $ sum $ Store.balanceTxCount <$> xs
        , balanceTotalReceived =
              fromIntegral $ sum $ Store.balanceTotalReceived <$> xs
        }

catchResponseError :: ExceptT String IO Response -> IO Response
catchResponseError m = do
    resE <- runExceptT m
    case resE of
        Left err  -> return $ ResponseError $ cs err
        Right res -> return res

commandResponse :: Command -> IO Response
commandResponse =
    \case
        CommandMnemonic d e -> mnemonic d e
        CommandCreateAcc n d -> createAcc n d
        CommandImportAcc f k -> importAcc f k
        CommandRenameAcc old new -> renameAcc old new
        CommandAccounts -> accounts
        CommandBalance accM -> balance accM
        CommandAddresses accM p -> addresses accM p
        CommandReceive accM -> receive accM
        CommandTransactions accM p -> transactions accM p
        CommandPrepareTx accM rcpts unit fee dust ->
            prepareTx accM rcpts unit fee dust

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

balance :: Maybe Text -> IO Response
balance accM =
    catchResponseError $ do
        (key, store) <- getAccountStore accM
        let net = accountStoreNetwork store
        withNetwork net $ do
            let allAddrs = extAddresses store <> intAddresses store
            bals <- httpAddrBalances $ fst <$> allAddrs
            return $ ResponseBalance key store $ addrsToAccBalance bals

addresses :: Maybe Text -> Page -> IO Response
addresses accM page =
    catchResponseError $ do
        (key, store) <- getAccountStore accM
        let addrs = toPage page $ reverse $ extAddresses store
        return $ ResponseAddresses key store addrs

receive :: Maybe Text -> IO Response
receive accM =
    catchResponseError $ do
        (key, store) <- getAccountStore accM
        let (addr, store') = genExtAddress store
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
            best <- bestBlockHeight
            txs <- httpAddrTxs (fst <$> allAddrs) page
            let walletTxs = toWalletTx addrMap best <$> txs
            return $ ResponseTransactions key store walletTxs

prepareTx ::
       Maybe Text
    -> [(Text, Text)]
    -> AmountUnit
    -> Natural
    -> Natural
    -> IO Response
prepareTx accM rcpTxt unit feeByte dust =
    catchResponseError $ do
        (key, store) <- getAccountStore accM
        let net = accountStoreNetwork store
        rcpts <- liftEither $ mapM (toRecipient net unit) rcpTxt
        withNetwork net $ do
            let walletAddrMap = storeAddressMap store
            (signDat, commitStore) <- buildTxSignData store rcpts feeByte dust
            path <- liftIO $ writeDoc signDat
            wTx <- liftEither $ toUnsignedWalletTx walletAddrMap signDat
            newStore <- commit key commitStore
            return $ ResponsePrepareTx key newStore (cs path) wTx

toRecipient ::
       Network
    -> AmountUnit
    -> (Text, Text)
    -> Either String (Address, Natural)
toRecipient net unit (a, v) = do
    addr <- textToAddrE net a 
    val <- maybeToEither (cs badAmnt) (readAmount unit v)
    return (addr, val)
  where
    badAmnt =
        "Could not parse the amount " <> a <> " as " <> showUnit unit 1

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
