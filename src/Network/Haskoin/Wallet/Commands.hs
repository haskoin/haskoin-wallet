{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Haskoin.Wallet.Commands where

import Control.Monad (forM, mzero, unless, when, (<=<))
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    liftEither,
    runExceptT,
  )
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Control.Monad.State (MonadState (get), gets, modify)
import Data.Aeson (object, (.:), (.:?), (.=))
import qualified Data.Aeson as Json
import Data.Bits (clearBit)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Haskoin.Address (Address, addrToText, textToAddr)
import Haskoin.Crypto
  ( Ctx,
    HardPath,
    Mnemonic,
    SoftPath,
    XPrvKey,
    deriveXPubKey,
    fromMnemonic,
    pathToList,
  )
import Haskoin.Network (Network (name), netByName)
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
  ( GetAddrsBalance (GetAddrsBalance),
    GetAddrsTxs (GetAddrsTxs),
    GetBlockBest (GetBlockBest),
    GetHealth (GetHealth),
    GetTxs (GetTxs),
    LimitsParam (limit),
    PostTx (PostTx),
    apiBatch,
    apiCall,
  )
import Haskoin.Transaction (TxHash)
import Haskoin.Util
  ( MarshalJSON (marshalValue, unmarshalValue),
    maybeToEither,
  )
import Network.Haskoin.Wallet.AccountStore
  ( AccountMap,
    accountStoreAccount,
    addrsDerivPage,
    emptyAccountStore,
    extAddresses,
    extDeriv,
    genExtAddress,
    getAccountStoreByDeriv,
    insertAccountStore,
    intDeriv,
    readAccountLabels,
    readAccountMap,
    renameAccountStore,
    storeAddressMap,
    withAccountMap,
    withAccountStore,
    writeAccountLabel,
  )
import Network.Haskoin.Wallet.Amounts
  ( AmountUnit,
    readAmount,
    showUnit,
  )
import Network.Haskoin.Wallet.Database
import Network.Haskoin.Wallet.Entropy
  ( genMnemonic,
    mergeMnemonicParts,
    systemEntropy,
    word8ToBase6,
  )
import Network.Haskoin.Wallet.FileIO
  ( HWFolder (PubKeyFolder, SweepFolder, TxFolder),
    PubKeyDoc (PubKeyDoc),
    TxSignData
      ( TxSignData,
        txSignDataAccount,
        txSignDataNetwork,
        txSignDataTx
      ),
    checkFileExists,
    docFilePath,
    hwDataDirectory,
    parseAddrsFile,
    parseSecKeysFile,
    readFileWords,
    readMarshalFile,
    txsChecksum,
    writeDoc,
  )
import Network.Haskoin.Wallet.Parser (Command (..))
import Network.Haskoin.Wallet.Signing
  ( buildSweepSignData,
    buildTxSignData,
    conf,
    signTxWithKeys,
    signWalletTx,
    signingKey,
  )
import Network.Haskoin.Wallet.TxInfo
  ( TxInfo,
    UnsignedTxInfo,
    parseTxSignData,
    toTxInfo,
    unsignedToTxInfo,
  )
import Network.Haskoin.Wallet.Util
  ( Page (Page),
    addrToText3,
    liftExcept,
    sortDesc,
    textToAddr3
  ,
    textToAddrE,
    toPage,
    (</>),
  )
import Numeric.Natural (Natural)
import qualified System.Console.Haskeline as Haskeline
import qualified System.Directory as D

-- | Version of Haskoin Wallet package.
versionString :: (IsString a) => a

#ifdef CURRENT_PACKAGE_VERSION
versionString = CURRENT_PACKAGE_VERSION
#else
versionString = "Unavailable"
#endif

data Response
  = ResponseError
      { responseError :: !Text
      }
  | ResponseMnemonic
      { responseEntropySource :: !Text,
        responseMnemonic :: ![Text],
        responseSplitMnemonic :: ![[Text]]
      }
  | ResponseCreateAcc
      { responseAccount :: !DBAccount
      }
  | ResponseTestAcc
      { responseAccount :: !DBAccount,
        responseResult :: !Bool,
        responseText :: !Text
      }
  | ResponseImportAcc
      { responseAccount :: !DBAccount
      }
  | ResponseRenameAcc
      { responseOldName :: !Text,
        responseNewName :: !Text,
        responseAccount :: !DBAccount
      }
  | ResponseAccounts
      { responseAccounts :: ![DBAccount]
      }
  | ResponseReceive
      { responseAccount :: !DBAccount,
        responseAddress :: !DBAddress
      }
  | ResponseAddresses
      { responseAccount :: !DBAccount,
        responseAddresses :: ![DBAddress]
      }
  | ResponseTxs
      { responseAccount :: !DBAccount,
        responseTransactions :: ![TxInfo]
      }
  | ResponsePrepareTx
      { responseAccount :: !DBAccount,
        responseTxFile :: !Text,
        responseUnsignedTx :: !UnsignedTxInfo
      }
  | ResponseReview
      { responseAccount :: !DBAccount,
        responseTransactionM :: !(Maybe TxInfo),
        responseUnsignedTxM :: !(Maybe UnsignedTxInfo)
      }
  | ResponseSignTx
      { responseTxFile :: !Text,
        responseTransaction :: !TxInfo,
        responseNetwork :: !Network
      }
  | ResponseSendTx
      { responseAccount :: !DBAccount,
        responseTransaction :: !TxInfo,
        responseNetworkTxId :: !TxHash
      }
  | ResponseScanAcc
      { responseAccount :: !DBAccount
      }
  | ResponseVersion
      {responseVersion :: !Text}
  | ResponsePrepareSweep
      { responseAccount :: !DBAccount,
        responseTxFiles :: ![Text],
        responseUnsignedTxs :: ![UnsignedTxInfo]
      }
  | ResponseSignSweep
      { responseAccount :: !DBAccount,
        responseTxFiles :: ![Text],
        responseTransactions :: ![TxInfo]
      }
  | ResponseRollDice
      { responseRollDice :: ![Natural],
        responseEntropySource :: !Text
      }
  deriving (Show)

jsonError :: String -> Json.Value
jsonError err = object ["type" .= Json.String "error", "error" .= err]

instance MarshalJSON Ctx Response where
  marshalValue ctx =
    \case
      ResponseError err -> jsonError $ cs err
      ResponseMnemonic e w ws ->
        object
          [ "type" .= Json.String "mnemonic",
            "entropysource" .= e,
            "mnemonic" .= w,
            "splitmnemonic" .= ws
          ]
      ResponseCreateAcc a ->
        object
          [ "type" .= Json.String "createacc",
            "account" .= a
          ]
      ResponseTestAcc a b t ->
        object
          [ "type" .= Json.String "testacc",
            "account" .= a,
            "result" .= b,
            "text" .= t
          ]
      ResponseImportAcc a ->
        object
          [ "type" .= Json.String "importacc",
            "account" .= a
          ]
      ResponseRenameAcc o n a ->
        object
          [ "type" .= Json.String "renameacc",
            "oldname" .= o,
            "newname" .= n,
            "account" .= a
          ]
      ResponseAccounts a ->
        object
          [ "type" .= Json.String "accounts",
            "accounts" .= a
          ]
      ResponseReceive a addr ->
        object
          [ "type" .= Json.String "receive",
            "account" .= a,
            "address" .= addr
          ]
      ResponseAddresses a addrs ->
        object
          [ "type" .= Json.String "addresses",
            "account" .= a,
            "addresses" .= addrs
          ]
      ResponseTxs a txs ->
        object
          [ "type" .= Json.String "txs",
            "account" .= a,
            "transactions"
              .= Json.toJSON (marshalValue (accountNetwork a, ctx) <$> txs)
          ]
      ResponsePrepareTx a f t ->
        object
          [ "type" .= Json.String "preparetx",
            "account" .= a,
            "txfile" .= f,
            "unsignedtx" .= marshalValue (accountNetwork a, ctx) t
          ]
      ResponseReview a wTxM uTxM -> do
        let net = accountNetwork a
            wTx = marshalValue (net, ctx) <$> wTxM
            uTx = marshalValue (net, ctx) <$> uTxM
         in object
              [ "type" .= Json.String "review",
                "account" .= a,
                "transaction" .= fromMaybe Json.Null wTx,
                "unsignedtx" .= fromMaybe Json.Null uTx
              ]
      ResponseSignTx f t net ->
        object
          [ "type" .= Json.String "signtx",
            "txfile" .= f,
            "transaction" .= marshalValue (net, ctx) t,
            "network" .= net.name
          ]
      ResponseSendTx a t h ->
        object
          [ "type" .= Json.String "sendtx",
            "account" .= a,
            "transaction" .= marshalValue (accountNetwork a, ctx) t,
            "networktxid" .= h
          ]
      ResponseScanAcc a ->
        object
          [ "type" .= Json.String "scanacc",
            "account" .= a
          ]
      ResponseVersion v ->
        object ["type" .= Json.String "version", "version" .= v]
      ResponsePrepareSweep a fs ts ->
        object
          [ "type" .= Json.String "preparesweep",
            "account" .= a,
            "txfiles" .= fs,
            "unsignedtxs"
              .= Json.toJSON (marshalValue (accountNetwork a, ctx) <$> ts)
          ]
      ResponseSignSweep a fs ts ->
        object
          [ "type" .= Json.String "signsweep",
            "account" .= a,
            "txfiles" .= fs,
            "transactions"
              .= Json.toJSON (marshalValue (accountNetwork a, ctx) <$> ts)
          ]
      ResponseRollDice ns e ->
        object
          ["type" .= Json.String "rolldice", "entropysource" .= e, "dice" .= ns]
  unmarshalValue ctx =
    Json.withObject "response" $ \o -> do
      Json.String resType <- o .: "type"
      case resType of
        "error" -> ResponseError <$> o .: "error"
        "mnemonic" -> do
          e <- o .: "entropysource"
          m <- o .: "mnemonic"
          ms <- o .: "splitmnemonic"
          return $ ResponseMnemonic e m ms
        "createacc" -> do
          a <- o .: "account"
          return $ ResponseCreateAcc a
        "testacc" -> do
          a <- o .: "account"
          b <- o .: "result"
          t <- o .: "text"
          return $ ResponseTestAcc a b t
        "importacc" -> do
          a <- o .: "account"
          return $ ResponseImportAcc a
        "renameacc" -> do
          old <- o .: "oldname"
          new <- o .: "newname"
          a <- o .: "account"
          return $ ResponseRenameAcc old new a
        "accounts" -> do
          as <- o .: "accounts"
          return $ ResponseAccounts as
        "receive" -> do
          a <- o .: "account"
          x <- o .: "address"
          return $ ResponseReceive a x
        "addresses" -> do
          a <- o .: "account"
          addrs <- o .: "addresses"
          return $ ResponseAddresses a addrs
        "txs" -> do
          a <- o .: "account"
          let f = unmarshalValue (accountNetwork a, ctx)
          txs <- mapM f =<< o .: "transactions"
          return $ ResponseTxs a txs
        "preparetx" -> do
          a <- o .: "account"
          f <- o .: "txfile"
          let g = unmarshalValue (accountNetwork a, ctx)
          t <- g =<< o .: "unsignedtx"
          return $ ResponsePrepareTx a f t
        "review" -> do
          a <- o .: "account"
          let f = unmarshalValue (accountNetwork a, ctx)
              g = unmarshalValue (accountNetwork a, ctx)
          wTxM <-
            maybe (return Nothing) ((Just <$>) . f) =<< o .:? "transaction"
          uTxM <- maybe (return Nothing) ((Just <$>) . g) =<< o .:? "unsignedtx"
          return $ ResponseReview a wTxM uTxM
        "signtx" -> do
          net <- maybe mzero return . netByName =<< o .: "network"
          f <- o .: "txfile"
          t <- unmarshalValue (net, ctx) =<< o .: "transaction"
          return $ ResponseSignTx f t net
        "sendtx" -> do
          a <- o .: "account"
          let f = unmarshalValue (accountNetwork a, ctx)
          t <- f =<< o .: "transaction"
          h <- o .: "networktxid"
          return $ ResponseSendTx a t h
        "scanacc" -> do
          a <- o .: "account"
          return $ ResponseScanAcc a
        "version" -> do
          v <- o .: "version"
          return $ ResponseVersion v
        "preparesweep" -> do
          a <- o .: "account"
          fs <- o .: "txfiles"
          let g = unmarshalValue (accountNetwork a, ctx)
          ts <- mapM g =<< o .: "unsignedtxs"
          return $ ResponsePrepareSweep a fs ts
        "signsweep" -> do
          a <- o .: "account"
          fs <- o .: "txfiles"
          let f = unmarshalValue (accountNetwork a, ctx)
          ts <- mapM f =<< o .: "transactions"
          return $ ResponseSignSweep a fs ts
        "rolldice" -> do
          ns <- o .: "dice"
          e <- o .: "entropysource"
          return $ ResponseRollDice ns e
        _ -> fail "Invalid JSON response type"

addrsToAccBalance :: [Store.Balance] -> AccountBalance
addrsToAccBalance xs =
  AccountBalance
    { balanceConfirmed = fromIntegral $ sum $ (.confirmed) <$> xs,
      balanceUnconfirmed = fromIntegral $ sum $ (.unconfirmed) <$> xs,
      balanceCoins = fromIntegral $ sum $ (.utxo) <$> xs
    }

catchResponseError :: (Monad m) => ExceptT String m Response -> m Response
catchResponseError m = do
  resE <- runExceptT m
  case resE of
    Left err -> return $ ResponseError $ cs err
    Right res -> return res

commandResponse :: Ctx -> Command -> IO Response
commandResponse ctx =
  \case
    CommandMnemonic e d s -> mnemonic e d s
    CommandCreateAcc t n dM s -> createAcc ctx t n dM s
    CommandTestAcc accM s -> testAcc ctx accM s
    CommandImportAcc f -> importAcc ctx f
    CommandRenameAcc old new -> renameAcc ctx old new
    CommandAccounts accM -> accounts accM
    CommandReceive accM labM -> receive ctx accM labM
    CommandAddrs accM p -> addrs accM p
--  CommandTxs accM p -> txs ctx accM p
--  CommandPrepareTx rcpts accM unit fee dust rcptPay ->
--    prepareTx ctx rcpts accM unit fee dust rcptPay
--  CommandReview file -> cmdReview ctx file
--  CommandSignTx file s -> cmdSignTx ctx file s
--  CommandSendTx file -> cmdSendTx ctx file
--  CommandScanAcc accM -> scanAccount ctx accM
--  CommandVersion -> cmdVersion
--  CommandPrepareSweep as fileM accM fee dust ->
--    prepareSweep ctx as fileM accM fee dust
--  CommandSignSweep dir keyFile accM -> signSweep ctx dir keyFile accM
--  CommandRollDice n -> rollDice n

-- runDB . catchResponseError Monad Stack:
-- ExceptT String (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))

liftEitherIO :: (MonadIO m) => IO (Either String a) -> ExceptT String m a
liftEitherIO = liftEither <=< liftIO

mnemonic :: Natural -> Bool -> Natural -> IO Response
mnemonic ent useDice splitIn =
  catchResponseError $ do
    (orig, ms, splitMs) <- genMnemonic ent useDice splitIn
    return $ ResponseMnemonic orig (T.words ms) (T.words <$> splitMs)

createAcc :: Ctx -> Text -> Network -> Maybe Natural -> Natural -> IO Response
createAcc ctx name net derivM splitIn =
  runDB $
    catchResponseError $ do
      d <- maybe (lift $ nextAccountDeriv net) return derivM
      prvKey <- askSigningKey ctx net d splitIn
      let xpub = deriveXPubKey ctx prvKey
          doc = PubKeyDoc xpub net name
      path <- liftIO $ docFilePath ctx PubKeyFolder doc
      account <- liftDB $ insertAccount net ctx name xpub (cs path)
      _ <- writeDoc ctx PubKeyFolder (PubKeyDoc xpub net name)
      return $ ResponseCreateAcc account

testAcc :: Ctx -> Maybe Text -> Natural -> IO Response
testAcc ctx nameM splitIn =
  runDB $
    catchResponseError $ do
      acc <- liftDB $ getAccountVal nameM
      let net = accountNetwork acc
          xPubKey = accountXPubKey ctx acc
          d = accountIndex acc
      xPrvKey <- askSigningKey ctx net d splitIn
      return $
        if deriveXPubKey ctx xPrvKey == xPubKey
          then
            ResponseTestAcc
              { responseAccount = acc,
                responseResult = True,
                responseText =
                  "The mnemonic and passphrase matched the account"
              }
          else
            ResponseTestAcc
              { responseAccount = acc,
                responseResult = False,
                responseText =
                  "The mnemonic and passphrase did not match the account"
              }

importAcc :: Ctx -> FilePath -> IO Response
importAcc ctx fp =
  runDB $
    catchResponseError $ do
      doc@(PubKeyDoc xpub net name) <- liftEitherIO $ readMarshalFile ctx fp
      path <- liftIO $ docFilePath ctx PubKeyFolder doc
      acc <- liftDB $ insertAccount net ctx name xpub (cs path)
      _ <- writeDoc ctx PubKeyFolder doc
      return $ ResponseImportAcc acc

renameAcc :: Ctx -> Text -> Text -> IO Response
renameAcc ctx oldName newName =
  runDB $
    catchResponseError $ do
      acc <- liftDB $ renameAccount oldName newName
      let xpub = accountXPubKey ctx acc
          net = accountNetwork acc
      _ <- writeDoc ctx PubKeyFolder $ PubKeyDoc xpub net newName
      return $ ResponseRenameAcc oldName newName acc

accounts :: Maybe Text -> IO Response
accounts nameM =
  runDB $
    catchResponseError $ do
      case nameM of
        Just _ -> do
          acc <- liftDB $ getAccountVal nameM
          return $ ResponseAccounts [acc]
        _ -> ResponseAccounts <$> lift getAccountsVal

receive :: Ctx -> Maybe Text -> Maybe Text -> IO Response
receive ctx nameM labelM =
  runDB $
    catchResponseError $ do
      (acc, addr) <- liftDB $ receiveAddress ctx nameM $ fromMaybe "" labelM
      return $ ResponseReceive acc addr

addrs :: Maybe Text -> Page -> IO Response
addrs nameM page =
  runDB $
    catchResponseError $ do
      (accId, acc) <- liftDB $ getAccount nameM
      as <- liftDB $ getAddresses accId page
      return $ ResponseAddresses acc as

{-

zipLabels ::
  [(Address, SoftPath)] -> Map Natural Text -> [AddressResponse]
zipLabels addrs m =
  f <$> addrs
  where
    f (a, p) =
      AddressResponse a p $
        fromMaybe "No Label" $
          Map.lookup (fromIntegral $ last $ pathToList p) m

transactions :: Ctx -> Maybe Text -> Page -> IO Response
transactions ctx accM page =
  catchResponseError $
    withAccountStore ctx accM $ \storeName -> do
      net <- gets accountNetwork
      addrMap <- gets (storeAddressMap ctx)
      let allAddrs = Map.keys addrMap
      checkHealth ctx net
      best <-
        (.height)
          <$> liftExcept (apiCall ctx (conf net) (GetBlockBest def))
      -- TODO: This only works for small wallets.
      Store.SerialList txRefs <-
        liftExcept $
          apiBatch
            ctx
            20
            (conf net)
            (GetAddrsTxs allAddrs def {limit = Just 100})
      let sortedRefs = (.txid) <$> sortDesc txRefs
      Store.SerialList txs <-
        liftExcept $
          apiBatch ctx 20 (conf net) (GetTxs (toPage page sortedRefs))
      let txInfos = toTxInfo addrMap (fromIntegral best) <$> txs
      store <- get
      return $ ResponseTransactions storeName store txInfos

prepareTx ::
  Ctx ->
  [(Text, Text)] ->
  Maybe Text ->
  AmountUnit ->
  Natural ->
  Natural ->
  Bool ->
  IO Response
prepareTx ctx rcpTxt accM unit feeByte dust rcptPay =
  catchResponseError $
    withAccountStore ctx accM $ \storeName -> do
      net <- gets accountNetwork
      pub <- gets accountStoreXPubKey
      rcpts <- liftEither $ mapM (toRecipient net unit) rcpTxt
      checkHealth ctx net
      signDat <- buildTxSignData net ctx rcpts feeByte dust rcptPay
      path <- liftIO $ writeDoc TxFolder signDat
      txInfoU <- liftEither $ parseTxSignData net ctx pub signDat
      store <- get
      return $ ResponsePrepareTx storeName store (cs path) txInfoU

toRecipient ::
  Network ->
  AmountUnit ->
  (Text, Text) ->
  Either String (Address, Natural)
toRecipient net unit (a, v) = do
  addr <- textToAddrE net a
  val <- maybeToEither (cs badAmnt) (readAmount unit v)
  return (addr, val)
  where
    badAmnt =
      "Could not parse the amount " <> a <> " as " <> showUnit unit 1

cmdSignTx :: Ctx -> FilePath -> Natural -> IO Response
cmdSignTx ctx fp splitIn =
  catchResponseError $ do
    txSignData <- liftEither =<< liftIO (readMarshalFile ctx fp)
    let net = txSignDataNetwork txSignData
        acc = txSignDataAccount txSignData
    prvKey <- askSigningKey ctx net acc splitIn
    (newSignData, txInfo) <- liftEither $ signWalletTx ctx txSignData prvKey
    path <- liftIO $ writeDoc TxFolder newSignData
    return $ ResponseSignTx (cs path) txInfo net

cmdReview :: Ctx -> FilePath -> IO Response
cmdReview ctx fp =
  catchResponseError $ do
    tsd@(TxSignData tx _ _ _ acc signed net) <-
      liftEither =<< liftIO (readMarshalFile ctx fp)
    withAccountMap ctx $ do
      (storeName, store) <- getAccountStoreByDeriv net acc
      let pub = accountStoreXPubKey store
      txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
      let txInfo = unsignedToTxInfo tx txInfoU
      return $
        if signed
          then ResponseReview storeName store (Just txInfo) Nothing
          else ResponseReview storeName store Nothing (Just txInfoU)

cmdSendTx :: Ctx -> FilePath -> IO Response
cmdSendTx ctx fp =
  catchResponseError $ do
    tsd@(TxSignData signedTx _ _ _ acc signed net) <-
      liftEither =<< liftIO (readMarshalFile ctx fp)
    unless signed $ throwError "The transaction is not signed"
    checkHealth ctx net
    withAccountMap ctx $ do
      (storeName, store) <- getAccountStoreByDeriv net acc
      let pub = accountStoreXPubKey store
      txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
      let txInfo = unsignedToTxInfo signedTx txInfoU
      Store.TxId netTxId <-
        liftExcept $ apiCall ctx (conf net) (PostTx signedTx)
      return $ ResponseSendTx storeName store txInfo netTxId

scanAccount :: Ctx -> Maybe Text -> IO Response
scanAccount ctx accM =
  catchResponseError $
    withAccountStore ctx accM $ \storeName -> do
      updateAccountIndices ctx storeName
      gets (ResponseScanAcc storeName)

updateAccountIndices ::
  (MonadError String m, MonadIO m, MonadState AccountStore m) =>
  Ctx ->
  Text ->
  m ()
updateAccountIndices ctx storeName = do
  net <- gets accountNetwork
  pub <- gets accountStoreXPubKey
  checkHealth ctx net
  e <- go net pub extDeriv 0 (Page 20 0)
  i <- go net pub intDeriv 0 (Page 20 0)
  m <- readAccountLabels storeName
  let eMax = maximum $ e : ((+ 1) <$> Map.keys m)
  modify $ \s -> s {accountStoreExternal = eMax, accountStoreInternal = i}
  where
    go net pub deriv d page@(Page lim off) = do
      let addrs = addrsDerivPage ctx deriv page pub
          req = GetAddrsBalance $ fst <$> addrs
      Store.SerialList bals <-
        liftExcept $ apiCall ctx (conf net) req
      let vBals = filter ((/= 0) . (.txs)) bals
      if null vBals
        then return d
        else do
          let d' = findMax addrs $ (.address) <$> vBals
          go net pub deriv (d' + 1) (Page lim (off + lim))
    findMax :: [(Address, SoftPath)] -> [Address] -> Natural
    findMax addrs balAddrs =
      let fAddrs = filter ((`elem` balAddrs) . fst) addrs
       in fromIntegral $ maximum $ last . pathToList . snd <$> fAddrs

cmdVersion :: IO Response
cmdVersion = return $ ResponseVersion versionString

prepareSweep ::
  Ctx ->
  [Text] ->
  Maybe FilePath ->
  Maybe Text ->
  Natural ->
  Natural ->
  IO Response
prepareSweep ctx addrsTxt fileM accM feeByte dust =
  catchResponseError $
    withAccountStore ctx accM $ \storeName -> do
      net <- gets accountNetwork
      pub <- gets accountStoreXPubKey
      addrsArg <- liftEither $ mapM (textToAddrE net) addrsTxt
      addrsFile <-
        case fileM of
          Just file -> parseAddrsFile net <$> liftIO (readFileWords file)
          _ -> return []
      let addrs = addrsArg <> addrsFile
      checkHealth ctx net
      signDats <- buildSweepSignData net ctx addrs feeByte dust
      let chksum = cs $ txsChecksum $ txSignDataTx <$> signDats
      !txInfosU <- liftEither $ mapM (parseTxSignData net ctx pub) signDats
      paths <- liftIO $ mapM (writeDoc (SweepFolder chksum)) signDats
      store <- get
      return $ ResponsePrepareSweep storeName store (cs <$> paths) txInfosU

signSweep :: Ctx -> FilePath -> FilePath -> Maybe Text -> IO Response
signSweep ctx sweepDir keyFile accM =
  catchResponseError $
    withAccountStore ctx accM $ \storeName -> do
      publicKey <- gets accountStoreXPubKey
      net <- gets accountNetwork
      acc <- liftEither =<< gets accountStoreAccount
      sweepFiles <- fmap (sweepDir </>) <$> liftIO (D.listDirectory sweepDir)
      tsds <- mapM (liftEither <=< liftIO . readMarshalFile ctx) sweepFiles
      when (null tsds) $ throwError "No sweep transactions to sign"
      unless (all (valid acc net) tsds) $
        throwError "Transactions do not match account information"
      secKeys <- parseSecKeysFile net <$> liftIO (readFileWords keyFile)
      when (null secKeys) $ throwError "No private keys to sign"
      !signRes <-
        forM tsds $ \tsd ->
          liftEither $ signTxWithKeys ctx tsd publicKey secKeys
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

rollDice :: Natural -> IO Response
rollDice n = do
  (res, origEnt) <- go [] ""
  return $ ResponseRollDice (take (fromIntegral n) res) origEnt
  where
    go acc orig
      | length acc >= fromIntegral n = return (acc, orig)
      | otherwise = do
          (origEnt, sysEnt) <- systemEntropy 1
          go (word8ToBase6 (head $ BS.unpack sysEnt) <> acc) origEnt

-- Utilities --

checkHealth :: (MonadIO m, MonadError String m) => Ctx -> Network -> m ()
checkHealth ctx net = do
  health <- liftExcept $ apiCall ctx (conf net) GetHealth
  unless (Store.isOK health) $
    throwError "The indexer health check has failed"

-}

-- Haskeline Helpers --

askInputLineHidden :: String -> IO String
askInputLineHidden message = do
  inputM <-
    Haskeline.runInputT Haskeline.defaultSettings $
      Haskeline.getPassword (Just '*') message
  maybe
    (error "No action due to EOF")
    return
    inputM

askInputLine :: String -> IO String
askInputLine message = do
  inputM <-
    Haskeline.runInputT Haskeline.defaultSettings $
      Haskeline.getInputLine message
  maybe
    (error "No action due to EOF")
    return
    inputM

askMnemonic :: String -> IO Mnemonic
askMnemonic txt = do
  mnm <- askInputLineHidden txt
  case fromMnemonic (cs mnm) of -- validate the mnemonic
    Right _ -> return $ cs mnm
    Left _ -> do
      liftIO $ putStrLn "Invalid mnemonic"
      askMnemonic txt

askSigningKey ::
  (MonadError String m, MonadIO m) =>
  Ctx ->
  Network ->
  Natural ->
  Natural ->
  m XPrvKey
askSigningKey _ _ _ 0 = throwError "Mnemonic split can not be 0"
askSigningKey ctx net acc splitIn = do
  mnm <-
    if splitIn == 1
      then liftIO $ askMnemonic "Enter your mnemonic: "
      else do
        ms <- forM [1 .. splitIn] $ \n ->
          liftIO $ askMnemonic $ "Split mnemonic part #" <> show n <> ": "
        liftEither $ mergeMnemonicParts ms
  passStr <- liftIO askPassword
  liftEither $ signingKey net ctx (cs passStr) (cs mnm) acc

askPassword :: IO String
askPassword = do
  pass <- askInputLineHidden "Mnemonic passphrase or leave empty: "
  if null pass
    then return pass
    else do
      pass2 <- askInputLineHidden "Repeat your mnemonic passphrase: "
      if pass == pass2
        then return pass
        else do
          putStrLn "The passphrases did not match"
          askPassword
