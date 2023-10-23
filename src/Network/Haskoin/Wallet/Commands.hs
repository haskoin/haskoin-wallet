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
import Control.Monad (forM, forM_, mzero, unless, void, when, (<=<))
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
import Data.Maybe (fromJust, fromMaybe, isJust, maybeToList)
import Data.Serialize (decode, encode)
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
  ( ApiConfig (..),
    GetAddrTxs (GetAddrTxs),
    GetAddrsBalance (GetAddrsBalance),
    GetAddrsTxs (GetAddrsTxs),
    GetAddrsUnspent (GetAddrsUnspent),
    GetBlockBest (GetBlockBest),
    GetHealth (GetHealth),
    GetTxs (GetTxs),
    GetTxsRaw (GetTxsRaw),
    LimitsParam (..),
    PostTx (PostTx),
    StartParam (StartParamHash),
    apiBatch,
    apiCall,
  )
import Haskoin.Transaction (OutPoint (..), TxHash (..), nosigTxHash, txHashToHex)
import Haskoin.Util
  ( MarshalJSON (marshalValue, unmarshalValue),
    decodeHex,
    eitherToMaybe,
    maybeToEither,
    snd3,
  )
import Network.Haskoin.Wallet.Amounts
  ( AmountUnit,
    readAmount,
    showUnit,
  )
import Network.Haskoin.Wallet.Config
import Network.Haskoin.Wallet.Database
import Network.Haskoin.Wallet.Entropy
  ( genMnemonic,
    mergeMnemonicParts,
    systemEntropy,
    word8ToBase6,
  )
import Network.Haskoin.Wallet.FileIO
import Network.Haskoin.Wallet.Parser
import Network.Haskoin.Wallet.Signing
import Network.Haskoin.Wallet.TxInfo
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
  | ResponseExportAcc
      { responseAccount :: !DBAccount,
        responseAccountFile :: !FilePath
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
        responsePendingTx :: !NoSigTxInfo
      }
  | ResponsePendingTxs
      { responseAccount :: !DBAccount,
        responsePendingTxs :: ![NoSigTxInfo]
      }
  | ResponseReviewTx
      { responseAccount :: !DBAccount,
        responsePendingTx :: !NoSigTxInfo
      }
  | ResponseExportTx
      { responseTxFile :: !FilePath
      }
  | ResponseDeleteTx
      { responseFreedCoins :: !Natural,
        responseFreedAddrs :: !Natural
      }
  | ResponseSignTx
      { responseAccount :: !DBAccount,
        responsePendingTx :: !NoSigTxInfo
      }
  | ResponseCoins
      { responseAccount :: !DBAccount,
        responseCoins :: ![JsonCoin]
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
        responseTxCount :: !Natural,
        responseCoinCount :: !Natural
      }
  | ResponseDiscoverAcc
      { responseAccount :: !DBAccount
      }
  | ResponseVersion
      {responseVersion :: !Text}
  | ResponsePrepareSweep
      { responseAccount :: !DBAccount,
        responseTxFiles :: ![Text],
        responsePendingTxs :: ![NoSigTxInfo]
      }
  | ResponseSignSweep
      { responseAccount :: !DBAccount,
        responseTxFiles :: ![Text],
        responsePendingTxs :: ![NoSigTxInfo]
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
      ResponseExportAcc a f ->
        object
          [ "type" .= Json.String "exportacc",
            "account" .= a,
            "accountfile" .= f
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
      ResponsePrepareTx a t -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "preparetx",
            "account" .= a,
            "pendingtx" .= marshalValue (net, ctx) t
          ]
      ResponsePendingTxs a ts -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "pendingtxs",
            "account" .= a,
            "pendingtxs" .= (marshalValue (net, ctx) <$> ts)
          ]
      ResponseReviewTx a tx -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "reviewtx",
            "account" .= a,
            "pendingtx" .= marshalValue (net, ctx) tx
          ]
      ResponseExportTx f ->
        object
          [ "type" .= Json.String "exporttx",
            "txfile" .= f
          ]
      ResponseDeleteTx c a ->
        object
          [ "type" .= Json.String "deletetx",
            "freedcoins" .= c,
            "freedaddrs" .= a
          ]
      ResponseSignTx a t -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "signtx",
            "account" .= a,
            "pendingtx" .= marshalValue (net, ctx) t
          ]
      ResponseCoins a coins -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "coins",
            "account" .= a,
            "coins" .= (marshalValue net <$> coins)
          ]
      ResponseSendTx a t h -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "sendtx",
            "account" .= a,
            "transaction" .= marshalValue (net, ctx) t,
            "networktxid" .= h
          ]
      ResponseSyncAcc as bb bh tc cc ->
        object
          [ "type" .= Json.String "syncacc",
            "account" .= as,
            "bestblock" .= bb,
            "bestheight" .= bh,
            "txupdates" .= tc,
            "coinupdates" .= cc
          ]
      ResponseDiscoverAcc a ->
        object
          [ "type" .= Json.String "discoveracc",
            "account" .= a
          ]
      ResponseVersion v ->
        object ["type" .= Json.String "version", "version" .= v]
      ResponsePrepareSweep a fs ts -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "preparesweep",
            "account" .= a,
            "txfiles" .= fs,
            "pendingtxs" .= (marshalValue (net, ctx) <$> ts)
          ]
      ResponseSignSweep a fs ts -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "signsweep",
            "account" .= a,
            "txfiles" .= fs,
            "pendingtxs" .= (marshalValue (net, ctx) <$> ts)
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
        "exportacc" ->
          ResponseExportAcc
            <$> o .: "account"
            <*> o .: "accountfile"
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
          let net = accountNetwork a
          txs <- mapM (unmarshalValue (net, ctx)) =<< o .: "txs"
          return $ ResponseTxs a txs
        "preparetx" -> do
          a <- o .: "account"
          let net = accountNetwork a
          t <- unmarshalValue (net, ctx) =<< o .: "pendingtx"
          return $ ResponsePrepareTx a t
        "pendingtxs" -> do
          a <- o .: "account"
          let net = accountNetwork a
          ts <- mapM (unmarshalValue (net, ctx)) =<< o .: "pendingtxs"
          return $ ResponsePendingTxs a ts
        "reviewtx" -> do
          a <- o .: "account"
          let net = accountNetwork a
          t <- unmarshalValue (net, ctx) =<< o .: "pendingtx"
          return $ ResponseReviewTx a t
        "exporttx" ->
          ResponseExportTx
            <$> o .: "txfile"
        "deletetx" ->
          ResponseDeleteTx
            <$> o .: "freedcoins"
            <*> o .: "freedaddrs"
        "signtx" -> do
          a <- o .: "account"
          let net = accountNetwork a
          t <- unmarshalValue (net, ctx) =<< o .: "pendingtx"
          return $ ResponseSignTx a t
        "coins" -> do
          a <- o .: "account"
          xs <- o .: "coins"
          coins <- mapM (unmarshalValue (accountNetwork a)) xs
          return $ ResponseCoins a coins
        "sendtx" -> do
          a <- o .: "account"
          let net = accountNetwork a
          t <- unmarshalValue (net, ctx) =<< o .: "transaction"
          i <- o .: "networktxid"
          return $ ResponseSendTx a t i
        "syncacc" ->
          ResponseSyncAcc
            <$> o .: "account"
            <*> o .: "bestblock"
            <*> o .: "bestheight"
            <*> o .: "txupdates"
            <*> o .: "coinupdates"
        "discoveracc" ->
          ResponseDiscoverAcc
            <$> o .: "account"
        "version" ->
          ResponseVersion
            <$> o .: "version"
        "preparesweep" -> do
          a <- o .: "account"
          let net = accountNetwork a
          fs <- o .: "txfiles"
          ts <- mapM (unmarshalValue (net, ctx)) =<< o .: "pendingtxs"
          return $ ResponsePrepareSweep a fs ts
        "signsweep" -> do
          a <- o .: "account"
          let net = accountNetwork a
          fs <- o .: "txfiles"
          ts <- mapM (unmarshalValue (net, ctx)) =<< o .: "pendingtxs"
          return $ ResponseSignSweep a fs ts
        "rolldice" ->
          ResponseRollDice
            <$> o .: "dice"
            <*> o .: "entropysource"
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
    -- Mnemonic and account management
    CommandMnemonic e d s -> cmdMnemonic e d s
    CommandCreateAcc t n dM s -> cmdCreateAcc ctx t n dM s
    CommandTestAcc nameM s -> cmdTestAcc ctx nameM s
    CommandRenameAcc old new -> cmdRenameAcc old new
    CommandAccounts nameM -> cmdAccounts nameM
    -- Address management
    CommandReceive nameM labM -> cmdReceive ctx nameM labM
    CommandAddrs nameM p -> cmdAddrs nameM p
    CommandLabel nameM i l -> cmdLabel nameM i l
    -- Transaction management
    CommandTxs nameM p -> cmdTxs ctx nameM p
    CommandPrepareTx rcpts nameM unit fee dust rcptPay o ->
      cmdPrepareTx ctx rcpts nameM unit fee dust rcptPay o
    CommandPendingTxs nameM p -> cmdPendingTxs ctx nameM p
    CommandSignTx nameM h i o s -> cmdSignTx ctx nameM h i o s
    CommandDeleteTx h -> cmdDeleteTx h
    CommandCoins nameM p -> cmdCoins nameM p
    -- Import/export commands
    CommandImportAcc f -> cmdImportAcc ctx f
    CommandExportAcc nameM f -> cmdExportAcc ctx nameM f
    CommandReviewTx nameM file -> cmdReviewTx ctx nameM file
    CommandExportTx h f -> cmdExportTx h f
    -- Online commands
    CommandSendTx nameM h file -> cmdSendTx ctx nameM h file
    CommandSyncAcc nameM full -> cmdSyncAcc ctx nameM full
    CommandDiscoverAcc nameM -> cmdDiscoverAccount ctx nameM
    -- Utilities
    CommandVersion -> cmdVersion
    CommandPrepareSweep as fileM nameM fee dust dir ->
      prepareSweep ctx as fileM nameM fee dust dir
    CommandSignSweep nameM dir keyFile -> signSweep ctx nameM dir keyFile
    CommandRollDice n -> rollDice n

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
  runDB . catchResponseError $ do
    (mnem, walletFP) <- askMnemonicPass net ctx splitIn
    d <- maybe (lift $ nextAccountDeriv walletFP net) return derivM
    prvKey <- liftEither $ signingKey net ctx mnem d
    let xpub = deriveXPubKey ctx prvKey
    account <- insertAccount net ctx walletFP name xpub
    return $ ResponseCreateAcc account

cmdTestAcc :: Ctx -> Maybe Text -> Natural -> IO Response
cmdTestAcc ctx nameM splitIn =
  runDB . catchResponseError $ do
    acc <- getAccountVal nameM
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
  runDB . catchResponseError $ do
    (PubKeyDoc xpub net name wallet) <- liftEitherIO $ readMarshalFile ctx fp
    acc <- insertAccount net ctx wallet name xpub
    return $ ResponseImportAcc acc

cmdExportAcc :: Ctx -> Maybe Text -> FilePath -> IO Response
cmdExportAcc ctx nameM file =
  runDB . catchResponseError $ do
    acc <- getAccountVal nameM
    checkPathFree file
    let xpub = accountXPubKey ctx acc
        net = accountNetwork acc
        name = dBAccountName acc
        wallet = accountWallet acc
        doc = PubKeyDoc xpub net name wallet
    liftIO $ writeMarshalFile ctx file doc
    return $ ResponseExportAcc acc file

cmdDeleteTx :: TxHash -> IO Response
cmdDeleteTx nosigH =
  runDB . catchResponseError $ do
    (coins, addrs) <- deletePendingTx (DBPendingTxKey $ txHashToHex nosigH) True
    return $ ResponseDeleteTx coins addrs

cmdRenameAcc :: Text -> Text -> IO Response
cmdRenameAcc oldName newName =
  runDB . catchResponseError $ do
    acc <- renameAccount oldName newName
    return $ ResponseRenameAcc oldName newName acc

cmdAccounts :: Maybe Text -> IO Response
cmdAccounts nameM =
  runDB . catchResponseError $ do
    case nameM of
      Just _ -> do
        acc <- getAccountVal nameM
        return $ ResponseAccounts [acc]
      _ -> do
        accs <- lift getAccountsVal
        return $ ResponseAccounts accs

cmdReceive :: Ctx -> Maybe Text -> Maybe Text -> IO Response
cmdReceive ctx nameM labelM =
  runDB . catchResponseError $ do
    (acc, addr) <- receiveAddress ctx nameM $ fromMaybe "" labelM
    return $ ResponseReceive acc addr

cmdAddrs :: Maybe Text -> Page -> IO Response
cmdAddrs nameM page =
  runDB . catchResponseError $ do
    (accId, acc) <- getAccount nameM
    as <- lift $ addressPage accId page
    return $ ResponseAddresses acc as

cmdLabel :: Maybe Text -> Natural -> Text -> IO Response
cmdLabel nameM idx lab =
  runDB . catchResponseError $ do
    (acc, adr) <- updateLabel nameM (fromIntegral idx) lab
    return $ ResponseLabel acc adr

cmdTxs :: Ctx -> Maybe Text -> Page -> IO Response
cmdTxs ctx nameM page =
  runDB . catchResponseError $ do
    (acc, txInfos) <- txsPage ctx nameM page
    return $ ResponseTxs acc txInfos

cmdPrepareTx ::
  Ctx ->
  [(Text, Text)] ->
  Maybe Text ->
  AmountUnit ->
  Natural ->
  Natural ->
  Bool ->
  Maybe FilePath ->
  IO Response
cmdPrepareTx ctx rcpTxt nameM unit feeByte dust rcptPay fileM =
  runDB . catchResponseError $ do
    (accId, acc) <- getAccount nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    rcpts <- liftEither $ mapM (toRecipient net) rcpTxt
    (signDat, changeM, commitDB) <-
      buildTxSignData net ctx accId rcpts feeByte dust rcptPay
    txInfoU <- liftEither $ parseTxSignData net ctx pub signDat
    nosigHash <- repsertPendingTx net accId signDat (maybeToList changeM)
    case fileM of
      Just file -> do
        checkPathFree file
        liftIO $ writeJsonFile file $ Json.toJSON signDat
      _ -> return ()
    commitDB
    newAcc <- getAccountId accId
    return $ ResponsePrepareTx newAcc $ NoSigUnsigned nosigHash txInfoU
  where
    toRecipient net (a, v) = do
      addr <- textToAddrE net a
      val <- maybeToEither (cs $ badAmnt v) (readAmount unit v)
      return (addr, val)
    badAmnt v =
      "Could not parse the amount " <> v <> " as " <> showUnit unit 1

cmdPendingTxs :: Ctx -> Maybe Text -> Page -> IO Response
cmdPendingTxs ctx nameM page =
  runDB . catchResponseError $ do
    (accId, acc) <- getAccount nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    tsds <- pendingTxPage accId page
    txs <- forM tsds $ \(nosigH, tsd@(TxSignData tx _ _ _ signed)) -> do
      txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
      return $
        if signed
          then NoSigSigned nosigH $ unsignedToTxInfo tx txInfoU
          else NoSigUnsigned nosigH txInfoU
    return $ ResponsePendingTxs acc txs

cmdReviewTx :: Ctx -> Maybe Text -> FilePath -> IO Response
cmdReviewTx ctx nameM fp =
  runDB . catchResponseError $ do
    acc <- getAccountVal nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    tsd@(TxSignData tx _ _ _ signed) <- liftEitherIO $ readJsonFile fp
    txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
    let txInfo = unsignedToTxInfo tx txInfoU
        nosigHash = nosigTxHash tx
    return $
      ResponseReviewTx acc $
        if signed
          then NoSigSigned nosigHash txInfo
          else NoSigUnsigned nosigHash txInfoU

cmdExportTx :: TxHash -> FilePath -> IO Response
cmdExportTx nosigH fp =
  runDB . catchResponseError $ do
    pendingTxM <- lift $ getPendingTx nosigH
    case pendingTxM of
      Just tsd -> do
        checkPathFree fp
        liftIO $ writeJsonFile fp $ Json.toJSON tsd
        return $ ResponseExportTx fp
      _ -> throwError "The pending transaction does not exist"

cmdSignTx ::
  Ctx ->
  Maybe Text ->
  Maybe TxHash ->
  Maybe FilePath ->
  Maybe FilePath ->
  Natural ->
  IO Response
cmdSignTx ctx nameM nosigHM inputM outputM splitIn =
  runDB . catchResponseError $ do
    tsd <- getInput
    when (txSignDataSigned tsd) $
      throwError "The transaction is already signed"
    (accId, acc) <- getAccount nameM
    let net = accountNetwork acc
        idx = fromIntegral $ dBAccountIndex acc
        accPub = accountXPubKey ctx acc
    (mnem, _) <- askMnemonicPass net ctx splitIn
    prvKey <- liftEither $ signingKey net ctx mnem idx
    let pubKey = deriveXPubKey ctx prvKey
    unless (accPub == pubKey) $
      throwError "The mnemonic did not match the provided account"
    (newSignData, txInfo) <- liftEither $ signWalletTx net ctx tsd prvKey
    let nosigH = nosigTxHash $ txSignDataTx newSignData
    unless (Just nosigH == nosigHM) $
      throwError "The nosigHash did not match"
    case outputM of
      Just o -> do
        checkPathFree o
        liftIO $ writeJsonFile o $ Json.toJSON newSignData
      _ -> return ()
    when (isJust nosigHM) $ void $ repsertPendingTx net accId newSignData []
    return $ ResponseSignTx acc (NoSigSigned nosigH txInfo)
  where
    getInput =
      case (nosigHM, inputM, outputM) of
        (Nothing, Nothing, _) ->
          throwError
            "Provide either a TXHASH or both a --input file and a --output file"
        (Just _, Just _, _) ->
          throwError "Can not specify both a TXHASH and a --input file"
        (_, Just _, Nothing) ->
          throwError "When using a --input file, also provide a --output file"
        (Just h, _, _) -> do
          resM <- lift $ getPendingTx h
          case resM of
            Just res -> return res
            _ -> throwError "The nosigHash does not exist in the wallet"
        (_, Just i, _) -> do
          exist <- liftIO $ D.doesFileExist i
          unless exist $ throwError "Input file does not exist"
          liftEitherIO $ readJsonFile i

cmdCoins :: Maybe Text -> Page -> IO Response
cmdCoins nameM page =
  runDB . catchResponseError $ do
    (accId, acc) <- getAccount nameM
    let net = accountNetwork acc
    coins <- lift $ coinPage accId page
    bestM <- lift $ getBest net
    let best = maybe 0 snd bestM
    jsonCoins <- mapM (liftEither . toJsonCoin net best) coins
    return $ ResponseCoins acc jsonCoins

cmdSendTx :: Ctx -> Maybe Text -> Maybe TxHash -> Maybe FilePath -> IO Response
cmdSendTx ctx nameM nosigHM inputM =
  runDB . catchResponseError $ do
    acc <- getAccountVal nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    tsd@(TxSignData signedTx _ _ _ signed) <- getInput
    txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
    let txInfo = unsignedToTxInfo signedTx txInfoU
        verify = verifyTxInfo net ctx signedTx txInfo
    unless (signed && verify) $ throwError "The transaction is not signed"
    checkHealth ctx net
    Store.TxId netTxId <- liftExcept $ apiCall ctx (conf net) (PostTx signedTx)
    return $ ResponseSendTx acc txInfo netTxId
  where
    getInput =
      case (nosigHM, inputM) of
        (Nothing, Nothing) ->
          throwError "Provide either a TXHASH or a --input file"
        (Just _, Just _) ->
          throwError "Can not provide both a TXHASH and a --input file"
        (Just h, _) -> do
          resM <- lift $ getPendingTx h
          case resM of
            Just res -> return res
            _ -> throwError "The nosigHash does not exist in the wallet"
        (_, Just i) -> do
          exist <- liftIO $ D.doesFileExist i
          unless exist $ throwError "Input file does not exist"
          liftEitherIO $ readJsonFile i

cmdSyncAcc :: Ctx -> Maybe Text -> Bool -> IO Response
cmdSyncAcc ctx nameM full = do
  runDB . catchResponseError $ do
    (accId, acc) <- getAccount nameM
    let net = accountNetwork acc
    -- Check API health
    checkHealth ctx net
    -- Get the new best block before starting the sync
    best <- liftExcept $ apiCall ctx (conf net) (GetBlockBest def)
    -- Get the addresses from our local database
    (addrPathMap, addrBalMap) <- allAddressesMap net accId
    -- Fetch the address balances online
    Store.SerialList storeBals <-
      liftExcept . apiBatch ctx addrBatch (conf net) $
        GetAddrsBalance (Map.keys addrBalMap)
    -- Filter only those addresses whose balances have changed
    balsToUpdate <-
      if full
        then return storeBals
        else liftEither $ filterAddresses storeBals addrBalMap
    let addrsToUpdate = (.address) <$> balsToUpdate
    -- Update balances
    updateAddressBalances net balsToUpdate
    newAcc <- lift $ updateAccountBalances accId
    -- Get a list of our confirmed txs in the local database
    -- Use an empty list when doing a full sync
    confirmedTxs <- if full then return [] else getConfirmedTxs accId True
    -- Fetch the txids of the addresses to update
    aTids <- searchAddrTxs net ctx confirmedTxs addrsToUpdate
    -- We also want to check if there is any change in unconfirmed txs
    uTids <- getConfirmedTxs accId False
    let tids = nub $ uTids <> aTids
    -- Fetch the full transactions
    Store.SerialList txs <-
      liftExcept $ apiBatch ctx txFullBatch (conf net) (GetTxs tids)
    -- Convert them to TxInfo and store them in the local database
    let txInfos = toTxInfo addrPathMap (fromIntegral best.height) <$> txs
    resTxInfo <- lift $ forM txInfos $ repsertTxInfo net ctx accId
    -- Fetch and update coins
    Store.SerialList storeCoins <-
      liftExcept . apiBatch ctx coinBatch (conf net) $
        GetAddrsUnspent addrsToUpdate def
    (coinCount, newCoins) <- updateCoins net accId addrsToUpdate storeCoins
    -- Get the dependent tranactions of the new coins
    depTxsHash <-
      if full
        then return $ (.outpoint.hash) <$> storeCoins
        else mapM (liftEither . coinToTxHash) newCoins
    Store.RawResultList rawTxs <-
      liftExcept . apiBatch ctx txFullBatch (conf net) $
        GetTxsRaw $
          nub depTxsHash
    lift $ forM_ rawTxs insertRawTx
    -- Remove pending transactions if they are online
    pendingTids <- pendingTxHashes accId
    let toRemove = filter ((`elem` tids) . fst) pendingTids
    forM_ toRemove $ \(_, key) -> deletePendingTx key False
    -- Update the best block for this network
    lift $ updateBest net (headerHash best.header) best.height
    return $
      ResponseSyncAcc
        newAcc
        (headerHash best.header)
        (fromIntegral best.height)
        (fromIntegral $ length $ filter id $ snd <$> resTxInfo)
        (fromIntegral coinCount)

coinToTxHash :: DBCoin -> Either String TxHash
coinToTxHash coin =
  maybeToEither "coinToTxHash: Invalid outpoint" $ do
    bs <- decodeHex $ dBCoinOutpoint coin
    op <- eitherToMaybe (decode bs) :: Maybe OutPoint
    return op.hash

-- Filter addresses that need to be updated
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
            || s.utxo /= addrBalanceCoins b

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
  runDB . catchResponseError $ do
    (accId, acc) <- getAccount nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    checkHealth ctx net
    e <- go net pub extDeriv 0 (Page recoveryGap 0)
    i <- go net pub intDeriv 0 (Page recoveryGap 0)
    _ <- updateDeriv net ctx accId extDeriv e
    newAcc <- updateDeriv net ctx accId intDeriv i
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
          let dMax = findMax addrs $ (.address) <$> vBals
          go net pub path (dMax + 1) (Page lim (off + lim))
    -- Find the largest ID amongst the addresses that have a positive balance
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
  FilePath ->
  IO Response
prepareSweep ctx addrsTxt fileM nameM feeByte dust dir =
  runDB . catchResponseError $ do
    (accId, acc) <- getAccount nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    addrsArg <- liftEither $ mapM (textToAddrE net) addrsTxt
    addrsFile <-
      case fileM of
        Just file -> parseAddrsFile net <$> liftIO (readFileWords file)
        _ -> return []
    let addrs = addrsArg <> addrsFile
    checkHealth ctx net
    (tsds, commitDB) <- buildSweepSignData net ctx accId addrs feeByte dust
    let f tsd@(TxSignData tx _ _ _ _) = do
          infoU <- parseTxSignData net ctx pub tsd
          return $ NoSigUnsigned (nosigTxHash tx) infoU
    txInfosU <- liftEither $ mapM f tsds
    sweepDir <- initSweepDir tsds dir
    files <- writeSweepFiles tsds sweepDir
    commitDB
    newAcc <- getAccountId accId
    return $ ResponsePrepareSweep newAcc (cs <$> files) txInfosU

signSweep :: Ctx -> Maybe Text -> FilePath -> FilePath -> IO Response
signSweep ctx nameM sweepDir keyFile =
  runDB . catchResponseError $ do
    acc <- getAccountVal nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    -- Read the files containing the transactions to sweep
    sweepFiles <- fmap (sweepDir </>) <$> liftIO (D.listDirectory sweepDir)
    tsdsU <- mapM (liftEitherIO . readMarshalFile ctx) sweepFiles
    when (null tsdsU) $ throwError "No sweep transactions to sign"
    -- Read the file containing the private keys
    secKeys <- parseSecKeysFile net <$> liftIO (readFileWords keyFile)
    when (null secKeys) $ throwError "No private keys to sign"
    -- Sign the transactions
    signRes <-
      forM tsdsU $ \tsdU -> do
        (newTsd, txInfo) <- liftEither $ signTxWithKeys net ctx tsdU pub secKeys
        return (newTsd, NoSigSigned (nosigTxHash $ txSignDataTx newTsd) txInfo)
    -- Checksum validation
    let oldChksum = txsChecksum $ txSignDataTx <$> tsdsU
        newChksum = txsChecksum $ txSignDataTx . fst <$> signRes
    when (oldChksum /= newChksum) $
      throwError "The transactions checksum do not match"
    -- Write the signed transactions to JSON files
    signedFiles <- writeSweepFiles (fst <$> signRes) sweepDir
    return $ ResponseSignSweep acc (cs <$> signedFiles) (snd <$> signRes)

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
