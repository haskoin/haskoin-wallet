{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Haskoin.Wallet.Commands where

import Conduit (MonadUnliftIO, ResourceT)
import Control.Applicative (Alternative (some))
import Control.Monad (forM, forM_, mzero, unless, when, (<=<))
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
import Data.List (nub, sort, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Haskoin.Address (Address, addrToText, textToAddr)
import Haskoin.Block (BlockHash (..), headerHash)
import Haskoin.Crypto
  ( Ctx,
    Fingerprint,
    HardPath,
    Hash256,
    Mnemonic,
    SoftPath,
    XPrvKey,
    deriveXPubKey,
    fromMnemonic,
    pathToList,
    xPubFP,
  )
import Haskoin.Network (Network (name), netByName)
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
  ( GetAddrTxs (GetAddrTxs),
    GetAddrsBalance (GetAddrsBalance),
    GetAddrsTxs (GetAddrsTxs),
    GetBlockBest (GetBlockBest),
    GetHealth (GetHealth),
    GetTxs (GetTxs),
    LimitsParam (..),
    PostTx (PostTx),
    StartParam (StartParamHash),
    apiBatch,
    apiCall,
  )
import Haskoin.Transaction (TxHash (..))
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
  ( MnemonicPass (..),
    buildSweepSignData,
    buildTxSignData,
    conf,
    signTxWithKeys,
    signWalletTx,
    signingKey,
    walletFingerprint,
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
    chunksOf,
    liftExcept,
    sortDesc,
    textToAddr3,
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
  | ResponseLabel
      { responseAccount :: !DBAccount,
        responseAddress :: !DBAddress
      }
  | ResponseTxs
      { responseAccount :: !DBAccount,
        responseTxs :: ![TxInfo]
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
  | ResponseSyncAcc
      { responseAccount :: !DBAccount,
        responseBestBlock :: !BlockHash,
        responseBestHeight :: !Natural,
        responseTxCount :: !Natural
      }
  | ResponseDiscoverAcc
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
      ResponseAccounts as ->
        object
          [ "type" .= Json.String "accounts",
            "accounts" .= as
          ]
      ResponseReceive a addr ->
        object
          [ "type" .= Json.String "receive",
            "account" .= a,
            "address" .= addr
          ]
      ResponseAddresses a adrs ->
        object
          [ "type" .= Json.String "addresses",
            "account" .= a,
            "addresses" .= adrs
          ]
      ResponseLabel a adr ->
        object
          [ "type" .= Json.String "label",
            "account" .= a,
            "address" .= adr
          ]
      ResponseTxs a txs ->
        object
          [ "type" .= Json.String "txs",
            "account" .= a,
            "txs" .= (marshalValue (accountNetwork a, ctx) <$> txs)
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
      ResponseSyncAcc as bb bh tc ->
        object
          [ "type" .= Json.String "syncacc",
            "account" .= as,
            "bestblock" .= bb,
            "bestheight" .= bh,
            "newtxs" .= tc
          ]
      ResponseDiscoverAcc a ->
        object
          [ "type" .= Json.String "discoveracc",
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
        "mnemonic" ->
          ResponseMnemonic
            <$> o .: "entropysource"
            <*> o .: "mnemonic"
            <*> o .: "splitmnemonic"
        "createacc" ->
          ResponseCreateAcc
            <$> o .: "account"
        "testacc" ->
          ResponseTestAcc
            <$> o .: "account"
            <*> o .: "result"
            <*> o .: "text"
        "importacc" ->
          ResponseImportAcc
            <$> o .: "account"
        "renameacc" ->
          ResponseRenameAcc
            <$> o .: "oldname"
            <*> o .: "newname"
            <*> o .: "account"
        "accounts" ->
          ResponseAccounts
            <$> o .: "accounts"
        "receive" ->
          ResponseReceive
            <$> o .: "account"
            <*> o .: "address"
        "addresses" ->
          ResponseAddresses
            <$> o .: "account"
            <*> o .: "addresses"
        "label" ->
          ResponseLabel
            <$> o .: "account"
            <*> o .: "address"
        "txs" -> do
          a <- o .: "account"
          let f = unmarshalValue (accountNetwork a, ctx)
          txs <- mapM f =<< o .: "txs"
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
        "syncacc" ->
          ResponseSyncAcc
            <$> o .: "account"
            <*> o .: "bestblock"
            <*> o .: "bestheight"
            <*> o .: "newtxs"
        "discoveracc" -> do
          a <- o .: "account"
          return $ ResponseDiscoverAcc a
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

catchResponseError :: (Monad m) => ExceptT String m Response -> m Response
catchResponseError m = do
  resE <- runExceptT m
  case resE of
    Left err -> return $ ResponseError $ cs err
    Right res -> return res

commandResponse :: Ctx -> Command -> IO Response
commandResponse ctx =
  \case
    CommandMnemonic e d s -> cmdMnemonic e d s
    CommandCreateAcc t n dM s -> cmdCreateAcc ctx t n dM s
    CommandTestAcc accM s -> cmdTestAcc ctx accM s
    CommandImportAcc f -> cmdImportAcc ctx f
    CommandRenameAcc old new -> cmdRenameAcc ctx old new
    CommandAccounts accM -> cmdAccounts accM
    CommandReceive accM labM -> cmdReceive ctx accM labM
    CommandAddrs accM p -> cmdAddrs accM p
    CommandLabel accM i l -> cmdLabel accM i l
    CommandTxs accM p -> cmdTxs ctx accM p
    CommandSyncAcc accM -> cmdSyncAcc ctx accM
    CommandDiscoverAcc accM -> cmdDiscoverAccount ctx accM

--  CommandPrepareTx rcpts accM unit fee dust rcptPay ->
--    prepareTx ctx rcpts accM unit fee dust rcptPay
--  CommandReview file -> cmdReview ctx file
--  CommandSignTx file s -> cmdSignTx ctx file s
--  CommandSendTx file -> cmdSendTx ctx file
--  CommandVersion -> cmdVersion
--  CommandPrepareSweep as fileM accM fee dust ->
--    prepareSweep ctx as fileM accM fee dust
--  CommandSignSweep dir keyFile accM -> signSweep ctx dir keyFile accM
--  CommandRollDice n -> rollDice n

-- runDB . catchResponseError Monad Stack:
-- ExceptT String (ReaderT SqlBackend (NoLoggingT (ResourceT IO)))

liftEitherIO :: (MonadIO m) => IO (Either String a) -> ExceptT String m a
liftEitherIO = liftEither <=< liftIO

cmdMnemonic :: Natural -> Bool -> Natural -> IO Response
cmdMnemonic ent useDice splitIn =
  catchResponseError $ do
    (orig, ms, splitMs) <- genMnemonic ent useDice splitIn
    return $ ResponseMnemonic orig (T.words ms) (T.words <$> splitMs)

cmdCreateAcc :: Ctx -> Text -> Network -> Maybe Natural -> Natural -> IO Response
cmdCreateAcc ctx name net derivM splitIn =
  runDB $
    catchResponseError $ do
      (mnem, walletFP) <- askMnemonicPass net ctx splitIn
      d <- maybe (lift $ nextAccountDeriv walletFP net) return derivM
      prvKey <- liftEither $ signingKey net ctx mnem d
      let xpub = deriveXPubKey ctx prvKey
          doc = PubKeyDoc xpub net name walletFP
      path <- liftIO $ docFilePath ctx PubKeyFolder doc
      account <- liftDB $ insertAccount net ctx walletFP name xpub (cs path)
      _ <- writeDoc ctx PubKeyFolder doc
      return $ ResponseCreateAcc account

cmdTestAcc :: Ctx -> Maybe Text -> Natural -> IO Response
cmdTestAcc ctx nameM splitIn =
  runDB $
    catchResponseError $ do
      acc <- liftDB $ getAccountVal nameM
      let net = accountNetwork acc
          xPubKey = accountXPubKey ctx acc
          d = accountIndex acc
      (mnem, _) <- askMnemonicPass net ctx splitIn
      xPrvKey <- liftEither $ signingKey net ctx mnem d
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

cmdImportAcc :: Ctx -> FilePath -> IO Response
cmdImportAcc ctx fp =
  runDB $
    catchResponseError $ do
      doc@(PubKeyDoc xpub net name wallet) <-
        liftEitherIO $ readMarshalFile ctx fp
      path <- liftIO $ docFilePath ctx PubKeyFolder doc
      acc <- liftDB $ insertAccount net ctx wallet name xpub (cs path)
      _ <- writeDoc ctx PubKeyFolder doc
      return $ ResponseImportAcc acc

cmdRenameAcc :: Ctx -> Text -> Text -> IO Response
cmdRenameAcc ctx oldName newName =
  runDB $
    catchResponseError $ do
      acc <- liftDB $ renameAccount oldName newName
      let xpub = accountXPubKey ctx acc
          net = accountNetwork acc
          wallet = accountWallet acc
      _ <- writeDoc ctx PubKeyFolder $ PubKeyDoc xpub net newName wallet
      return $ ResponseRenameAcc oldName newName acc

cmdAccounts :: Maybe Text -> IO Response
cmdAccounts nameM =
  runDB $
    catchResponseError $ do
      case nameM of
        Just _ -> do
          acc <- liftDB $ getAccountVal nameM
          return $ ResponseAccounts [acc]
        _ -> do
          accs <- lift getAccountsVal
          return $ ResponseAccounts accs

cmdReceive :: Ctx -> Maybe Text -> Maybe Text -> IO Response
cmdReceive ctx nameM labelM =
  runDB $
    catchResponseError $ do
      (acc, addr) <- liftDB $ receiveAddress ctx nameM $ fromMaybe "" labelM
      return $ ResponseReceive acc addr

cmdAddrs :: Maybe Text -> Page -> IO Response
cmdAddrs nameM page =
  runDB $
    catchResponseError $ do
      (accId, acc) <- liftDB $ getAccount nameM
      as <- lift $ addressPage accId page
      return $ ResponseAddresses acc as

cmdLabel :: Maybe Text -> Natural -> Text -> IO Response
cmdLabel nameM idx lab =
  runDB $
    catchResponseError $ do
      (acc, adr) <- liftDB $ updateLabel nameM (fromIntegral idx) lab
      return $ ResponseLabel acc adr

cmdTxs :: Ctx -> Maybe Text -> Page -> IO Response
cmdTxs ctx nameM page =
  runDB $
    catchResponseError $ do
      (acc, txInfos) <- liftDB $ txsPage ctx nameM page
      return $ ResponseTxs acc txInfos

addrBatch :: Natural
addrBatch = 100

txBatch :: Natural
txBatch = 100

txFullBatch :: Natural
txFullBatch = 100

cmdSyncAcc :: Ctx -> Maybe Text -> IO Response
cmdSyncAcc ctx nameM = do
  runDB $
    catchResponseError $ do
      (accId, acc) <- liftDB $ getAccount nameM
      let net = accountNetwork acc
      -- Check API health
      checkHealth ctx net
      -- Get the new best block before starting the sync
      best <- liftExcept $ apiCall ctx (conf net) (GetBlockBest def)
      -- Get the addresses from our local database
      (addrPathMap, addrBalMap) <- liftDB $ allAddressesMap net accId
      -- Fetch the address balances online
      let req = GetAddrsBalance $ Map.keys addrBalMap
      Store.SerialList storeBals <-
        liftExcept $ apiBatch ctx addrBatch (conf net) req
      -- Filter only those addresses whose balances have changed
      balsToUpdate <- liftEither $ filterAddresses storeBals addrBalMap
      -- Update balances
      liftDB $ updateAddressBalances net balsToUpdate
      newAcc <- lift $ updateAccountBalances accId
      -- Get a list of our confirmed txs in the local database
      confirmedTxs <- liftDB $ getConfirmedTxs accId True
      -- Fetch the txids of the addresses to update
      tids <- searchAddrTxs net ctx confirmedTxs $ (.address) <$> balsToUpdate
      -- Fetch the full transactions
      Store.SerialList txs <-
        liftExcept $ apiBatch ctx txFullBatch (conf net) (GetTxs tids)
      -- Convert them to TxInfo and store them in the local database
      let txInfos = toTxInfo addrPathMap (fromIntegral best.height) <$> txs
      lift $ forM_ txInfos $ repsertTxInfo net ctx accId
      -- Update the best block for this network
      lift $ updateBest net (headerHash best.header) best.height
      return $
        ResponseSyncAcc
          newAcc
          (headerHash best.header)
          (fromIntegral best.height)
          (fromIntegral $ length txInfos)

-- Filter addresses that do not need to be updated
filterAddresses ::
  [Store.Balance] ->
  Map Address AddressBalance ->
  Either String [Store.Balance]
filterAddresses sBals aMap
  | sort ((.address) <$> sBals) /= sort (Map.keys aMap) =
      Left "Sync: addresses do not match"
  | otherwise =
      Right $ filter f sBals
  where
    f s =
      let b = fromJust $ s.address `Map.lookup` aMap
       in s.txs /= addrBalanceTxs b
            || s.confirmed /= addrBalanceConfirmed b
            || s.unconfirmed /= addrBalanceUnconfirmed b

searchAddrTxs ::
  (MonadIO m) =>
  Network ->
  Ctx ->
  [TxHash] ->
  [Address] ->
  ExceptT String m [TxHash]
searchAddrTxs _ _ _ [] = return []
searchAddrTxs net ctx confirmedTxs as
  | length as > fromIntegral addrBatch =
      nub . concat <$> mapM (go Nothing 0) (chunksOf addrBatch as)
  | otherwise =
      nub <$> go Nothing 0 as
  where
    go hashM offset xs = do
      Store.SerialList txRefs <-
        liftExcept $
          apiCall
            ctx
            (conf net)
            ( GetAddrsTxs
                xs
                def
                  { limit = Just $ fromIntegral txBatch,
                    start = StartParamHash <$> hashM,
                    offset = offset
                  }
            )
      -- Remove txs that we already have
      let tids = ((.txid) <$> txRefs) \\ confirmedTxs
      -- Either we have reached the end of the stream, or we have hit some
      -- txs in confirmedTxs. In both cases, we can stop the search.
      if length tids < fromIntegral txBatch
        then return tids
        else do
          let lastId = (last tids).get
          rest <- go (Just lastId) 1 xs
          return $ tids <> rest

cmdDiscoverAccount :: Ctx -> Maybe Text -> IO Response
cmdDiscoverAccount ctx nameM =
  runDB $
    catchResponseError $ do
      (accId, acc) <- liftDB $ getAccount nameM
      let net = accountNetwork acc
          pub = accountXPubKey ctx acc
      checkHealth ctx net
      e <- go net pub extDeriv 0 (Page (fromIntegral gap) 0)
      i <- go net pub intDeriv 0 (Page (fromIntegral gap) 0)
      _ <- liftDB $ updateDeriv net ctx accId extDeriv e
      newAcc <- liftDB $ updateDeriv net ctx accId intDeriv i
      return $ ResponseDiscoverAcc newAcc
  where
    go net pub path d page@(Page lim off) = do
      let addrs = addrsDerivPage ctx path page pub
          req = GetAddrsBalance $ fst <$> addrs
      Store.SerialList bals <- liftExcept $ apiCall ctx (conf net) req
      let vBals = filter ((/= 0) . (.txs)) bals
      if null vBals
        then return d
        else do
          let d' = findMax addrs $ (.address) <$> vBals
          go net pub path (d' + 1) (Page lim (off + lim))
    -- Find the largest ID amongst the addresses that have a positive balance
    findMax :: [(Address, SoftPath)] -> [Address] -> Natural
    findMax addrs balAddrs =
      let fAddrs = filter ((`elem` balAddrs) . fst) addrs
       in fromIntegral $ maximum $ last . pathToList . snd <$> fAddrs

{-

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

-}

-- Utilities --

checkHealth :: (MonadIO m, MonadError String m) => Ctx -> Network -> m ()
checkHealth ctx net = do
  health <- liftExcept $ apiCall ctx (conf net) GetHealth
  unless (Store.isOK health) $
    throwError "The indexer health check has failed"

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

askMnemonicWords :: String -> IO Mnemonic
askMnemonicWords txt = do
  mnm <- askInputLineHidden txt
  case fromMnemonic (cs mnm) of -- validate the mnemonic
    Right _ -> return $ cs mnm
    Left _ -> do
      liftIO $ putStrLn "Invalid mnemonic"
      askMnemonicWords txt

askMnemonicPass ::
  (MonadError String m, MonadIO m) =>
  Network ->
  Ctx ->
  Natural ->
  m (MnemonicPass, Fingerprint)
askMnemonicPass net ctx splitIn = do
  mnm <-
    if splitIn == 1
      then liftIO $ askMnemonicWords "Enter your mnemonic words: "
      else do
        ms <- forM [1 .. splitIn] $ \n ->
          liftIO $ askMnemonicWords $ "Split mnemonic part #" <> show n <> ": "
        liftEither $ mergeMnemonicParts ms
  passStr <- liftIO askPassword
  let mnem =
        MnemonicPass
          { mnemonicWords = mnm,
            mnemonicPass = cs passStr
          }
  walletFP <- liftEither $ walletFingerprint net ctx mnem
  return (mnem, walletFP)

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
