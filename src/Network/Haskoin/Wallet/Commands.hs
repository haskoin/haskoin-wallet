{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.Commands where

import           Control.Arrow                       (first, second)
import           Control.Monad                       (forM, join, unless, when)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson                          (object, (.:), (.:?), (.=))
import qualified Data.Aeson                          as Json
import           Data.Aeson.TH
import qualified Data.Aeson.Types                    as Json
import qualified Data.ByteString.Char8               as C8
import           Data.Default                        (def)
import           Data.Either                         (fromRight)
import           Data.Foldable                       (asum)
import           Data.List                           (isPrefixOf, nub, sort,
                                                      sortOn)
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
import           Haskoin.Store.WebClient
import           Haskoin.Transaction
import           Haskoin.Util                        (dropFieldLabel,
                                                      dropSumLabels,
                                                      maybeToEither)
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.Entropy
import           Network.Haskoin.Wallet.FileIO
import           Network.Haskoin.Wallet.Parser
import           Network.Haskoin.Wallet.Signing
import           Network.Haskoin.Wallet.TxInfo
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Options.Applicative
import           Options.Applicative.Help.Pretty     hiding ((</>))
import qualified System.Console.Haskeline            as Haskeline
import qualified System.Directory                    as D
import           System.IO                           (IOMode (..), withFile)

data Response
    = ResponseError
          { responseError :: !Text
          }
    | ResponseMnemonic
          { responseEntropySource :: !Text
          , responseMnemonic      :: ![Text]
          }
    | ResponseCreateAcc
          { responseAccountName :: !Text
          , responseAccount     :: !AccountStore
          , responsePubKeyFile :: !Text
          }
    | ResponseImportAcc
          { responseAccountName :: !Text
          , responseAccount     :: !AccountStore
          }
    | ResponseRenameAcc
          { responseOldName :: !Text
          , responseNewName :: !Text
          , responseAccount :: !AccountStore
          }
    | ResponseAccounts
          { responseAccounts :: !AccountMap
          }
    | ResponseBalance
          { responseAccountName :: !Text
          , responseAccount     :: !AccountStore
          , responseBalance     :: !AccountBalance
          }
    | ResponseResetAcc
          { responseAccountName :: !Text
          , responseAccount     :: !AccountStore
          }
    | ResponseAddresses
          { responseAccountName :: !Text
          , responseAccount     :: !AccountStore
          , responseAddresses   :: ![(Address, SoftPath)]
          }
    | ResponseReceive
          { responseAccountName :: !Text
          , responseAccount     :: !AccountStore
          , responseAddress     :: !(Address, SoftPath)
          }
    | ResponseTransactions
          { responseAccountName  :: !Text
          , responseAccount      :: !AccountStore
          , responseTransactions :: ![TxInfo]
          }
    | ResponsePrepareTx
          { responseAccountName :: !Text
          , responseAccount     :: !AccountStore
          , responseTxFile      :: !Text
          , responseUnsignedTx  :: !UnsignedTxInfo
          }
    | ResponseReview
          { responseAccountName  :: !Text
          , responseAccount      :: !AccountStore
          , responseTransactionM :: !(Maybe TxInfo)
          , responseUnsignedTxM  :: !(Maybe UnsignedTxInfo)
          }
    | ResponseSignTx
          { responseTxFile      :: !Text
          , responseTransaction :: !TxInfo
          , responseNetwork     :: !Network
          }
    | ResponseSendTx
          { responseAccountName :: !Text
          , responseAccount     :: !AccountStore
          , responseTransaction :: !TxInfo
          , responseNetworkTxId :: !TxHash
          }
    | ResponsePrepareSweep
          { responseAccountName :: !Text
          , responseAccount     :: !AccountStore
          , responseTxFiles     :: ![Text]
          , responseUnsignedTxs :: ![UnsignedTxInfo]
          }
    | ResponseSignSweep
          { responseAccountName  :: !Text
          , responseAccount      :: !AccountStore
          , responseTxFiles      :: ![Text]
          , responseTransactions :: ![TxInfo]
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
            ResponseCreateAcc n a f ->
                object
                    [ "type" .= Json.String "createacc"
                    , "accountname" .= n
                    , "account" .= a
                    , "pubkeyfile" .= f
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
            ResponseResetAcc n a ->
                object
                    [ "type" .= Json.String "resetacc"
                    , "accountname" .= n
                    , "account" .= a
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
            ResponseReceive n a addr ->
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
                case mapM (txInfoToJSON (accountStoreNetwork a)) txs of
                    Right txsJ ->
                        object
                            [ "type" .= Json.String "transactions"
                            , "accountname" .= n
                            , "account" .= a
                            , "transactions" .= txsJ
                            ]
                    Left err -> jsonError err
            ResponsePrepareTx n a f t ->
                case unsignedTxInfoToJSON (accountStoreNetwork a) t of
                    Right tJ ->
                        object
                            [ "type" .= Json.String "preparetx"
                            , "accountname" .= n
                            , "account" .= a
                            , "txfile" .= f
                            , "unsignedtx" .= tJ
                            ]
                    Left err -> jsonError err
            ResponseReview n a wTxM uTxM -> do
                let net = accountStoreNetwork a
                    wTxE = txInfoToJSON net <$> wTxM
                    uTxE = unsignedTxInfoToJSON net <$> uTxM
                case (wTxE, uTxE) of
                    (Just (Left err), _) -> jsonError err
                    (_, Just (Left err)) -> jsonError err
                    (wTx, uTx) ->
                        object
                            [ "type" .= Json.String "review"
                            , "accountname" .= n
                            , "account" .= a
                            , "transaction" .=
                              maybe Json.Null (fromRight Json.Null) wTx
                            , "unsignedtx" .=
                              maybe Json.Null (fromRight Json.Null) uTx
                            ]
            ResponseSignTx f t net ->
                case txInfoToJSON net t of
                    Right tJ ->
                        object
                            [ "type" .= Json.String "signtx"
                            , "txfile" .= f
                            , "transaction" .= tJ
                            , "network" .= getNetworkName net
                            ]
                    Left err -> jsonError err
            ResponseSendTx n a t h ->
                case txInfoToJSON (accountStoreNetwork a) t of
                    Right tJ ->
                        object
                            [ "type" .= Json.String "sendtx"
                            , "accountname" .= n
                            , "account" .= a
                            , "transaction" .= tJ
                            , "networktxid" .= h
                            ]
                    Left err -> jsonError err
            ResponsePrepareSweep n a fs ts ->
                case mapM (unsignedTxInfoToJSON (accountStoreNetwork a)) ts of
                    Right tsJ ->
                        object
                            [ "type" .= Json.String "preparesweep"
                            , "accountname" .= n
                            , "account" .= a
                            , "txfiles" .= fs
                            , "unsignedtxs" .= tsJ
                            ]
                    Left err -> jsonError err
            ResponseSignSweep n a fs ts ->
                case mapM (txInfoToJSON (accountStoreNetwork a)) ts of
                    Right tsJ ->
                        object
                            [ "type" .= Json.String "signsweep"
                            , "accountname" .= n
                            , "account" .= a
                            , "txfiles" .= fs
                            , "transactions" .= tsJ
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
                    n <- o .: "accountname"
                    a <- o .: "account"
                    f <- o .: "pubkeyfile"
                    return $ ResponseCreateAcc n a f
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
                "resetacc" -> do
                    n <- o .: "accountname"
                    a <- o .: "account"
                    return $ ResponseResetAcc n a
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
                    let f = txInfoParseJSON (accountStoreNetwork a)
                    txs <- mapM f =<< o .: "transactions"
                    return $ ResponseTransactions n a txs
                "preparetx" -> do
                    n <- o .: "accountname"
                    a <- o .: "account"
                    f <- o .: "txfile"
                    let g = unsignedTxInfoParseJSON (accountStoreNetwork a)
                    t <- g =<< o .: "unsignedtx"
                    return $ ResponsePrepareTx n a f t
                "review" -> do
                    n <- o .: "accountname"
                    a <- o .: "account"
                    let f = txInfoParseJSON (accountStoreNetwork a)
                    let g = unsignedTxInfoParseJSON (accountStoreNetwork a)
                    wTxM <-
                        maybe (return Nothing) ((Just <$>) . f) =<<
                        o .:? "transaction"
                    uTxM <-
                        maybe (return Nothing) ((Just <$>) . g) =<<
                        o .:? "unsignedtx"
                    return $ ResponseReview n a wTxM uTxM
                "signtx" -> do
                    net <- maybe mzero return . netByName =<< o .: "network"
                    f <- o .: "txfile"
                    t <- txInfoParseJSON net =<< o .: "transaction"
                    return $ ResponseSignTx f t net
                "sendtx" -> do
                    n <- o .: "accountname"
                    a <- o .: "account"
                    let f = txInfoParseJSON (accountStoreNetwork a)
                    t <- f =<< o .: "transaction"
                    h <- o .: "networktxid"
                    return $ ResponseSendTx n a t h
                "preparesweep" -> do
                    n <- o .: "accountname"
                    a <- o .: "account"
                    fs <- o .: "txfiles"
                    let g = unsignedTxInfoParseJSON (accountStoreNetwork a)
                    ts <- mapM g =<< o .: "unsignedtxs"
                    return $ ResponsePrepareSweep n a fs ts
                "signsweep" -> do
                    n <- o .: "accountname"
                    a <- o .: "account"
                    fs <- o .: "txfiles"
                    let f = txInfoParseJSON (accountStoreNetwork a)
                    ts <- mapM f =<< o .: "transactions"
                    return $ ResponseSignSweep n a fs ts
                _ -> fail "Invalid JSON response type"

data AccountBalance =
    AccountBalance
        { balanceAmount       :: !Natural
        -- ^ confirmed balance
        , balanceZero         :: !Natural
        -- ^ unconfirmed balance
        , balanceUnspentCount :: !Natural
        -- ^ number of unspent outputs
        }
    deriving (Show, Read, Eq, Ord)

instance Json.ToJSON AccountBalance where
    toJSON b =
        object
            [ "confirmed" .= balanceAmount b
            , "unconfirmed" .= balanceZero b
            , "utxo" .= balanceUnspentCount b
            ]

instance Json.FromJSON AccountBalance where
    parseJSON =
        Json.withObject "accountbalance" $ \o ->
            AccountBalance
                <$> o .: "confirmed"
                <*> o .: "unconfirmed"
                <*> o .: "utxo"

addrsToAccBalance :: [Store.Balance] -> AccountBalance
addrsToAccBalance xs =
    AccountBalance
        { balanceAmount = fromIntegral $ sum $ Store.balanceAmount <$> xs
        , balanceZero = fromIntegral $ sum $ Store.balanceZero <$> xs
        , balanceUnspentCount =
              fromIntegral $ sum $ Store.balanceUnspentCount <$> xs
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
        CommandCreateAcc t n dM -> createAcc t n dM
        CommandImportAcc f -> importAcc f
        CommandRenameAcc old new -> renameAcc old new
        CommandAccounts -> accounts
        CommandBalance accM -> balance accM
        CommandResetAcc accM -> resetAccount accM
        CommandAddresses accM p -> addresses accM p
        CommandReceive accM -> receive accM
        CommandTransactions accM p -> transactions accM p
        CommandPrepareTx rcpts accM unit fee dust rcptPay ->
            prepareTx rcpts accM unit fee dust rcptPay
        CommandReview file -> cmdReview file
        CommandSignTx file -> cmdSignTx file
        CommandSendTx file -> cmdSendTx file
        CommandPrepareSweep as fileM accM fee dust ->
            prepareSweep as fileM accM fee dust
        CommandSignSweep dir keyFile accM -> signSweep dir keyFile accM

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

createAcc :: Text -> Network -> Maybe Natural -> IO Response
createAcc name net derivM =
    catchResponseError $
    withAccountMap $ do
        d <- maybe nextAccountDeriv return derivM
        prvKey <- lift $ askSigningKey net d
        let xpub = deriveXPubKey prvKey
            store = emptyAccountStore net xpub
        path <- liftIO $ writeDoc PubKeyFolder $ PubKeyDoc xpub net name
        insertAccountStore name store
        return $
            ResponseCreateAcc
                { responseAccountName = name
                , responseAccount = store
                , responsePubKeyFile = cs path
                }

importAcc :: FilePath -> IO Response
importAcc fp =
    catchResponseError $ do
        PubKeyDoc xpub net name <- liftEither =<< liftIO (readJsonFile fp)
        let store = emptyAccountStore net xpub
        withAccountMap $ do
            insertAccountStore name store
            return $ ResponseImportAcc name store

renameAcc :: Text -> Text -> IO Response
renameAcc oldName newName =
    catchResponseError $
    withAccountMap $ do
        store <- renameAccountStore oldName newName
        return $ ResponseRenameAcc oldName newName store

accounts :: IO Response
accounts =
    catchResponseError $ do
        accMap <- liftEither =<< liftIO readAccountMap
        return $ ResponseAccounts accMap

balance :: Maybe Text -> IO Response
balance accM =
    catchResponseError $
    withAccountStore accM $ \storeName -> do
        net <- gets accountStoreNetwork
        addrMap <- gets storeAddressMap
        checkHealth net
        let req =GetAddrsBalance (Map.keys addrMap)
        bals <- liftExcept $ apiCall def {configNetwork = net} req
        store <- get
        return $ ResponseBalance storeName store $ addrsToAccBalance bals

addresses :: Maybe Text -> Page -> IO Response
addresses accM page =
    catchResponseError $
    withAccountStore accM $ \storeName -> do
        store <- get
        let addrs = toPage page $ reverse $ extAddresses store
        return $ ResponseAddresses storeName store addrs

receive :: Maybe Text -> IO Response
receive accM =
    catchResponseError $
    withAccountStore accM $ \storeName -> do
        addr <- genExtAddress
        store <- get
        return $ ResponseReceive storeName store addr

transactions :: Maybe Text -> Page -> IO Response
transactions accM page =
    catchResponseError $
    withAccountStore accM $ \storeName -> do
        net <- gets accountStoreNetwork
        addrMap <- gets storeAddressMap
        let allAddrs = Map.keys addrMap
        checkHealth net
        best <-
            Store.blockDataHeight <$>
            liftExcept (apiCall def {configNetwork = net} (GetBlockBest def))
        -- TODO: This only works for small wallets.
        txRefs <-
            liftExcept $
            apiBatch
                20
                def {configNetwork = net}
                (GetAddrsTxs allAddrs def {paramLimit = Just 100})
        let sortedRefs = Store.txRefHash <$> sortDesc txRefs
        txs <-
            liftExcept $
            apiBatch
                20
                def {configNetwork = net}
                (GetTxs (toPage page sortedRefs))
        let txInfos = toTxInfo addrMap (fromIntegral best) <$> txs
        store <- get
        return $ ResponseTransactions storeName store txInfos

prepareTx ::
       [(Text, Text)]
    -> Maybe Text
    -> AmountUnit
    -> Natural
    -> Natural
    -> Bool
    -> IO Response
prepareTx rcpTxt accM unit feeByte dust rcptPay =
    catchResponseError $
    withAccountStore accM $ \storeName -> do
        net <- gets accountStoreNetwork
        pub <- gets accountStoreXPubKey
        rcpts <- liftEither $ mapM (toRecipient net unit) rcpTxt
        checkHealth net
        signDat <- buildTxSignData net rcpts feeByte dust rcptPay
        path <- liftIO $ writeDoc TxFolder signDat
        txInfoU <- liftEither $ parseTxSignData net pub signDat
        store <- get
        return $ ResponsePrepareTx storeName store (cs path) txInfoU

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

cmdSignTx ::
       FilePath
    -> IO Response
cmdSignTx fp =
    catchResponseError $ do
        txSignData <- liftEither =<< liftIO (readJsonFile fp)
        let net = txSignDataNetwork txSignData
            acc = txSignDataAccount txSignData
        prvKey <- askSigningKey net acc
        (newSignData, txInfo) <- liftEither $ signWalletTx txSignData prvKey
        path <- liftIO $ writeDoc TxFolder newSignData
        return $ ResponseSignTx (cs path) txInfo net

cmdReview :: FilePath -> IO Response
cmdReview fp =
    catchResponseError $ do
        tsd@(TxSignData tx _ _ _ acc signed net) <-
            liftEither =<< liftIO (readJsonFile fp)
        withAccountMap $ do
            (storeName, store) <- getAccountStoreByDeriv net acc
            let pub = accountStoreXPubKey store
            txInfoU <- liftEither $ parseTxSignData net pub tsd
            let txInfo = unsignedToTxInfo tx txInfoU
            return $
                if signed
                    then ResponseReview storeName store (Just txInfo) Nothing
                    else ResponseReview storeName store Nothing (Just txInfoU)

cmdSendTx :: FilePath -> IO Response
cmdSendTx fp =
    catchResponseError $ do
        tsd@(TxSignData signedTx _ _ _ acc signed net) <-
            liftEither =<< liftIO (readJsonFile fp)
        unless signed $ throwError "The transaction is not signed"
        checkHealth net
        withAccountMap $ do
            (storeName, store) <- getAccountStoreByDeriv net acc
            let pub = accountStoreXPubKey store
            txInfoU <- liftEither $ parseTxSignData net pub tsd
            let txInfo = unsignedToTxInfo signedTx txInfoU
            Store.TxId netTxId <-
                liftExcept $ apiCall def {configNetwork = net} (PostTx signedTx)
            return $ ResponseSendTx storeName store txInfo netTxId

prepareSweep ::
       [Text]
    -> Maybe FilePath
    -> Maybe Text
    -> Natural
    -> Natural
    -> IO Response
prepareSweep addrsTxt fileM accM feeByte dust =
    catchResponseError $
    withAccountStore accM $ \storeName -> do
        net <- gets accountStoreNetwork
        pub <- gets accountStoreXPubKey
        addrsArg <- liftEither $ mapM (textToAddrE net) addrsTxt
        addrsFile <-
            case fileM of
                Just file -> parseAddrsFile net <$> liftIO (readFileWords file)
                _ -> return []
        let addrs = addrsArg <> addrsFile
        checkHealth net
        signDats <- buildSweepSignData net addrs feeByte dust
        let chksum = cs $ txsChecksum $ txSignDataTx <$> signDats
        !txInfosU <- liftEither $ mapM (parseTxSignData net pub) signDats
        paths <- liftIO $ mapM (writeDoc (SweepFolder chksum)) signDats
        store <- get
        return $ ResponsePrepareSweep storeName store (cs <$> paths) txInfosU

signSweep :: FilePath -> FilePath -> Maybe Text -> IO Response
signSweep sweepDir keyFile accM =
    catchResponseError $
    withAccountStore accM $ \storeName -> do
        pubKey <- gets accountStoreXPubKey
        net <- gets accountStoreNetwork
        acc <- liftEither =<< gets accountStoreAccount
        sweepFiles <- fmap (sweepDir </>) <$> liftIO (D.listDirectory sweepDir)
        tsds <- mapM (liftEither <=< liftIO . readJsonFile) sweepFiles
        when (null tsds) $ throwError "No sweep transactions to sign"
        unless (all (valid acc net) tsds) $
            throwError "Transactions do not match account information"
        secKeys <- parseSecKeysFile net <$> liftIO (readFileWords keyFile)
        when (null secKeys) $ throwError "No private keys to sign"
        !signRes <-
            forM tsds $ \tsd -> liftEither $ signTxWithKeys tsd pubKey secKeys
        let initChksum = cs $ txsChecksum $ txSignDataTx <$> tsds
            chksum = cs $ txsChecksum $ txSignDataTx . fst <$> signRes
        when (initChksum /= chksum) $
            throwError "The transactions checksum do not match"
        res <-
            forM signRes $ \(newTsd, txInfo) -> do
                path <- liftIO $ writeDoc (SweepFolder chksum) newTsd
                return (path, txInfo)
        store <- get
        return $
            ResponseSignSweep storeName store (cs . fst <$> res) (snd <$> res)
  where
    valid acc net tsd =
        txSignDataAccount tsd == acc && txSignDataNetwork tsd == net

resetAccount :: Maybe Text -> IO Response
resetAccount accM =
    catchResponseError $
    withAccountStore accM $ \storeName -> do
        updateAccountIndices
        store <- get
        return $ ResponseResetAcc storeName store

updateAccountIndices ::
       (MonadError String m, MonadIO m, MonadState AccountStore m) => m ()
updateAccountIndices = do
    net <- gets accountStoreNetwork
    pub <- gets accountStoreXPubKey
    checkHealth net
    e <- go net pub extDeriv 0 (Page 20 0)
    i <- go net pub intDeriv 0 (Page 20 0)
    modify $ \s -> s {accountStoreExternal = e, accountStoreInternal = i}
  where
    go net pub deriv d page@(Page lim off) = do
        let addrs = addrsDerivPage deriv page pub
            req = GetAddrsBalance $ fst <$> addrs
        bals <- liftExcept $ apiCall def {configNetwork = net} req
        let vBals = filter ((/= 0) . Store.balanceTxCount) bals
        if null vBals
            then return d
            else do
                let d' = findMax addrs $ Store.balanceAddress <$> vBals
                go net pub deriv (d' + 1) (Page lim (off + lim))
    findMax :: [(Address, SoftPath)] -> [Address] -> Natural
    findMax addrs balAddrs =
        let fAddrs = filter ((`elem` balAddrs) . fst) addrs
         in fromIntegral $ maximum $ last . pathToList . snd <$> fAddrs

-- Utilities --

checkHealth :: (MonadIO m, MonadError String m) => Network -> m () 
checkHealth net = do
    health <- liftExcept $ apiCall def{configNetwork = net} GetHealth
    unless (Store.healthOK health) $
        throwError "The indexer health check has failed"
    unless (Store.healthSynced health) $
        throwError "The indexer is not synced on this network"
    

-- Haskeline Helpers --

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
