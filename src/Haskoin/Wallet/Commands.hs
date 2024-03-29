{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Commands where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Data.Aeson (object, (.:), (.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sqlite (transactionUndo)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Backup
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Entropy
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Migration.SemVersion
import Haskoin.Wallet.Parser
import Haskoin.Wallet.Signing
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import qualified System.Console.Haskeline as Haskeline
import qualified System.Directory as D
import System.Random (initStdGen)

data Response
  = ResponseError
      { responseError :: !Text
      }
  | ResponseMnemonic
      { responseEntropySource :: !Text,
        responseMnemonic :: ![Text],
        responseSplitMnemonic :: ![[Text]]
      }
  | ResponseAccount
      { responseAccount :: !DBAccount
      }
  | ResponseAccResult
      { responseAccount :: !DBAccount,
        responseResult :: !Bool,
        responseText :: !Text
      }
  | ResponseFile
      { responseTxFile :: !FilePath
      }
  | ResponseAccounts
      { responseAccounts :: ![DBAccount]
      }
  | ResponseAddress
      { responseAccount :: !DBAccount,
        responseAddress :: !DBAddress
      }
  | ResponseAddresses
      { responseAccount :: !DBAccount,
        responseAddresses :: ![DBAddress]
      }
  | ResponseTxs
      { responseAccount :: !DBAccount,
        responseTxs :: ![TxInfo]
      }
  | ResponseTx
      { responseAccount :: !DBAccount,
        responsePendingTx :: !TxInfo
      }
  | ResponseDeleteTx
      { responseNoSigHash :: !TxHash,
        responseFreedCoins :: !Natural,
        responseFreedAddrs :: !Natural
      }
  | ResponseCoins
      { responseAccount :: !DBAccount,
        responseCoins :: ![JsonCoin]
      }
  | ResponseSync {responseSync :: [SyncRes]}
  | ResponseRestore
      { responseRestore :: ![(DBAccount, Natural, Natural)]
      }
  | ResponseVersion
      { responseVersion :: !Text,
        responseDBVersion :: !Text
      }
  | ResponseRollDice
      { responseRollDice :: ![Natural],
        responseEntropySource :: !Text
      }
  deriving (Eq, Show)

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
      ResponseAccount a ->
        object
          [ "type" .= Json.String "account",
            "account" .= a
          ]
      ResponseAccResult a b t ->
        object
          [ "type" .= Json.String "accresult",
            "account" .= a,
            "result" .= b,
            "text" .= t
          ]
      ResponseFile f ->
        object
          [ "type" .= Json.String "file",
            "file" .= f
          ]
      ResponseAccounts as ->
        object
          [ "type" .= Json.String "accounts",
            "accounts" .= as
          ]
      ResponseAddress a addr ->
        object
          [ "type" .= Json.String "address",
            "account" .= a,
            "address" .= addr
          ]
      ResponseAddresses a adrs ->
        object
          [ "type" .= Json.String "addresses",
            "account" .= a,
            "addresses" .= adrs
          ]
      ResponseTxs a txs ->
        object
          [ "type" .= Json.String "txs",
            "account" .= a,
            "txs" .= (marshalValue (accountNetwork a, ctx) <$> txs)
          ]
      ResponseTx a t -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "tx",
            "account" .= a,
            "tx" .= marshalValue (net, ctx) t
          ]
      ResponseDeleteTx h c a ->
        object
          [ "type" .= Json.String "deletetx",
            "nosighash" .= h,
            "freedcoins" .= c,
            "freedaddrs" .= a
          ]
      ResponseCoins a coins -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "coins",
            "account" .= a,
            "coins" .= (marshalValue net <$> coins)
          ]
      ResponseSync xs -> do
        let f (SyncRes as bb bh tc cc) =
              object
                [ "account" .= as,
                  "bestblock" .= bb,
                  "bestheight" .= bh,
                  "txupdates" .= tc,
                  "coinupdates" .= cc
                ]
        object
          [ "type" .= Json.String "sync",
            "syncres" .= (f <$> xs)
          ]
      ResponseRestore xs ->
        let f (acc, t, c) =
              object
                [ "account" .= acc,
                  "txupdates" .= t,
                  "coinupdates" .= c
                ]
         in object
              [ "type" .= Json.String "restore",
                "restore" .= (f <$> xs)
              ]
      ResponseVersion v dbv ->
        object
          [ "type" .= Json.String "version",
            "version" .= v,
            "dbversion" .= dbv
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
        "account" ->
          ResponseAccount
            <$> o .: "account"
        "accresult" ->
          ResponseAccResult
            <$> o .: "account"
            <*> o .: "result"
            <*> o .: "text"
        "file" ->
          ResponseFile
            <$> o .: "file"
        "accounts" ->
          ResponseAccounts
            <$> o .: "accounts"
        "address" ->
          ResponseAddress
            <$> o .: "account"
            <*> o .: "address"
        "addresses" ->
          ResponseAddresses
            <$> o .: "account"
            <*> o .: "addresses"
        "txs" -> do
          a <- o .: "account"
          let net = accountNetwork a
          txs <- mapM (unmarshalValue (net, ctx)) =<< o .: "txs"
          return $ ResponseTxs a txs
        "tx" -> do
          a <- o .: "account"
          let net = accountNetwork a
          t <- unmarshalValue (net, ctx) =<< o .: "tx"
          return $ ResponseTx a t
        "deletetx" ->
          ResponseDeleteTx
            <$> o .: "nosighash"
            <*> o .: "freedcoins"
            <*> o .: "freedaddrs"
        "coins" -> do
          a <- o .: "account"
          xs <- o .: "coins"
          coins <- mapM (unmarshalValue (accountNetwork a)) xs
          return $ ResponseCoins a coins
        "sync" -> do
          xs <- o .: "syncres"
          res <- forM xs $ \x ->
            SyncRes
              <$> x .: "account"
              <*> x .: "bestblock"
              <*> x .: "bestheight"
              <*> x .: "txupdates"
              <*> x .: "coinupdates"
          return $ ResponseSync res
        "restore" -> do
          let f =
                Json.withObject "account" $ \o' ->
                  (,,)
                    <$> o' .: "account"
                    <*> o' .: "txupdates"
                    <*> o' .: "coinupdates"
          ResponseRestore <$> (mapM f =<< o .: "restore")
        "version" ->
          ResponseVersion
            <$> o .: "version"
            <*> o .: "dbversion"
        "rolldice" ->
          ResponseRollDice
            <$> o .: "dice"
            <*> o .: "entropysource"
        _ -> fail "Invalid JSON response type"

runDBResponse ::
  (MonadUnliftIO m) => Config -> ExceptT String (DB m) Response -> m Response
runDBResponse cfg action = do
  runDB cfg $ do
    resE <- runExceptT action
    case resE of
      Left err -> do
        transactionUndo -- Roll back the current sqlite transaction
        return $ ResponseError $ cs err
      Right res -> return res

catchResponseError :: (Monad m) => ExceptT String m Response -> m Response
catchResponseError m = do
  resE <- runExceptT m
  case resE of
    Left err -> return $ ResponseError $ cs err
    Right res -> return res

commandResponse :: Ctx -> Config -> AmountUnit -> Command -> IO Response
commandResponse ctx cfg unit cmd =
  case cmd of
    -- Mnemonic and account management
    CommandMnemonic e d s -> cmdMnemonic e d s
    CommandCreateAcc t n dM s -> cmdCreateAcc ctx cfg t n dM s
    CommandTestAcc nameM s -> cmdTestAcc ctx cfg nameM s
    CommandRenameAcc old new -> cmdRenameAcc cfg old new
    CommandAccounts nameM -> cmdAccounts cfg nameM
    CommandSyncAcc nameM full -> cmdSyncAcc ctx cfg nameM full
    CommandDeleteAcc name net deriv -> cmdDeleteAcc cfg name net deriv
    -- Address management
    CommandReceive nameM labM -> cmdReceive ctx cfg nameM labM
    CommandAddrs nameM p -> cmdAddrs cfg nameM p
    CommandLabel nameM i l -> cmdLabel cfg nameM i l
    -- Transaction management
    CommandTxs nameM p -> cmdTxs ctx cfg nameM p
    CommandPrepareTx rcpts nameM fee dust rcptPay minConf o ->
      cmdPrepareTx ctx cfg rcpts nameM unit fee dust rcptPay minConf o
    CommandPendingTxs nameM p -> cmdPendingTxs ctx cfg nameM p
    CommandSignTx nameM h i o s -> cmdSignTx ctx cfg nameM h i o s
    CommandSendTx h -> cmdSendTx ctx cfg h
    CommandDeleteTx h -> cmdDeleteTx ctx cfg h
    CommandCoins nameM p -> cmdCoins cfg nameM p
    -- Import/export commands
    CommandExportAcc nameM f -> cmdExportAcc ctx cfg nameM f
    CommandImportAcc f -> cmdImportAcc ctx cfg f
    CommandReviewTx nameM file -> cmdReviewTx ctx cfg nameM file
    CommandExportTx h f -> cmdExportTx cfg h f
    CommandImportTx nameM file -> cmdImportTx ctx cfg nameM file
    -- Backup and Restore
    CommandBackup f -> cmdBackup ctx cfg f
    CommandRestore f -> cmdRestore ctx cfg f
    CommandDiscoverAcc nameM -> cmdDiscoverAccount ctx cfg nameM
    -- Utilities
    CommandVersion -> cmdVersion cfg
    CommandPrepareSweep nameM prvKey st outputM f d ->
      prepareSweep ctx cfg nameM prvKey st outputM f d
    CommandSignSweep nameM h i o k -> signSweep ctx cfg nameM h i o k
    CommandRollDice n -> rollDice n

-- runDBResponse Monad Stack:
-- ExceptT String (ReaderT SqlBackend (NoLoggingT (ResourceT (IO))))

liftEitherIO :: (MonadIO m) => IO (Either String a) -> ExceptT String m a
liftEitherIO = liftEither <=< liftIO

cmdMnemonic :: Natural -> Bool -> Natural -> IO Response
cmdMnemonic ent useDice splitMnemIn =
  catchResponseError $ do
    (orig, ms, splitMs) <- genMnemonic ent useDice splitMnemIn
    return $ ResponseMnemonic orig (Text.words ms) (Text.words <$> splitMs)

cmdCreateAcc ::
  Ctx -> Config -> Text -> Network -> Maybe Natural -> Natural -> IO Response
cmdCreateAcc ctx cfg name net derivM splitMnemIn = do
  runDBResponse cfg $ do
    mnem <- askMnemonicPass splitMnemIn
    walletFP <- liftEither $ walletFingerprint net ctx mnem
    d <- maybe (lift $ nextAccountDeriv walletFP net) return derivM
    prvKey <- liftEither $ signingKey net ctx mnem d
    let xpub = deriveXPubKey ctx prvKey
    (_, acc) <- insertAccount net ctx walletFP name xpub
    return $ ResponseAccount acc

cmdTestAcc :: Ctx -> Config -> Maybe Text -> Natural -> IO Response
cmdTestAcc ctx cfg nameM splitMnemIn =
  runDBResponse cfg $ do
    (_, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        xPubKey = accountXPubKey ctx acc
        d = accountIndex acc
    mnem <- askMnemonicPass splitMnemIn
    xPrvKey <- liftEither $ signingKey net ctx mnem d
    return $
      if deriveXPubKey ctx xPrvKey == xPubKey
        then
          ResponseAccResult
            { responseAccount = acc,
              responseResult = True,
              responseText =
                "The mnemonic and passphrase matched the account"
            }
        else
          ResponseAccResult
            { responseAccount = acc,
              responseResult = False,
              responseText =
                "The mnemonic and passphrase did not match the account"
            }

cmdImportAcc :: Ctx -> Config -> FilePath -> IO Response
cmdImportAcc ctx cfg fp =
  runDBResponse cfg $ do
    (PubKeyDoc xpub net name wallet) <- liftEitherIO $ readMarshalFile ctx fp
    (_, acc) <- insertAccount net ctx wallet name xpub
    return $ ResponseAccount acc

cmdExportAcc :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdExportAcc ctx cfg nameM file =
  runDBResponse cfg $ do
    (_, acc) <- getAccountByName nameM
    checkPathFree file
    let xpub = accountXPubKey ctx acc
        net = accountNetwork acc
        name = dBAccountName acc
        wallet = accountWallet acc
        doc = PubKeyDoc xpub net name wallet
    liftIO $ writeMarshalFile ctx file doc
    return $ ResponseFile file

cmdRenameAcc :: Config -> Text -> Text -> IO Response
cmdRenameAcc cfg oldName newName =
  runDBResponse cfg $ do
    acc <- renameAccount oldName newName
    return $ ResponseAccount acc

cmdDeleteAcc :: Config -> Text -> Network -> HardPath -> IO Response
cmdDeleteAcc cfg name net path =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName $ Just name
    unless (accountNetwork acc == net) $
      throwError "The network of the account to delete did not match"
    accPath <- liftMaybe "HardPath" $ parseHard $ cs $ dBAccountDerivation acc
    unless (path == accPath) $
      throwError
        "The full derivation path of the account to delete did not match"
    lift $ deleteAccount accId
    return $
      ResponseAccResult acc True $
        "The account " <> name <> " has been deleted"

cmdAccounts :: Config -> Maybe Text -> IO Response
cmdAccounts cfg nameM =
  runDBResponse cfg $ do
    case nameM of
      Just _ -> do
        (_, acc) <- getAccountByName nameM
        return $ ResponseAccounts [acc]
      _ -> do
        accs <- lift getAccounts
        return $ ResponseAccounts $ snd <$> accs

cmdReceive :: Ctx -> Config -> Maybe Text -> Maybe Text -> IO Response
cmdReceive ctx cfg nameM labelM =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    addr <- genExtAddress ctx cfg accId $ fromMaybe "" labelM
    return $ ResponseAddress acc addr

cmdAddrs :: Config -> Maybe Text -> Page -> IO Response
cmdAddrs cfg nameM page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    as <- lift $ addressPage accId page
    return $ ResponseAddresses acc as

cmdLabel :: Config -> Maybe Text -> Natural -> Text -> IO Response
cmdLabel cfg nameM idx lab =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    adr <- setAddrLabel accId (fromIntegral idx) lab
    return $ ResponseAddress acc adr

cmdTxs :: Ctx -> Config -> Maybe Text -> Page -> IO Response
cmdTxs ctx cfg nameM page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    txInfos <- txsPage ctx accId page
    return $ ResponseTxs acc txInfos

cmdPrepareTx ::
  Ctx ->
  Config ->
  [(Text, Text)] ->
  Maybe Text ->
  AmountUnit ->
  Natural ->
  Natural ->
  Bool ->
  Natural ->
  Maybe FilePath ->
  IO Response
cmdPrepareTx ctx cfg rcpTxt nameM unit feeByte dust rcptPay minConf fileM =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    rcpts <- liftEither $ mapM (toRecipient net) rcpTxt
    gen <- liftIO initStdGen
    signDat <- buildTxSignData net ctx cfg gen accId rcpts feeByte dust rcptPay minConf
    txInfo <- liftEither $ parseTxSignData net ctx pub signDat
    txInfoL <- lift $ fillTxInfoLabels net txInfo
    for_ fileM checkPathFree
    _ <- importPendingTx net ctx accId signDat
    for_ fileM $ \file -> liftIO $ writeJsonFile file $ Json.toJSON signDat
    newAcc <- getAccountById accId
    return $ ResponseTx newAcc txInfoL
  where
    toRecipient net (a, v) = do
      addr <- textToAddrE net a
      val <- maybeToEither (cs $ badAmnt v) (readAmount unit v)
      return (addr, val)
    badAmnt v =
      "Could not parse the amount " <> v <> " as " <> showUnit unit 1

cmdPendingTxs :: Ctx -> Config -> Maybe Text -> Page -> IO Response
cmdPendingTxs ctx cfg nameM page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    txInfos <- pendingTxPage ctx accId page
    return $ ResponseTxs acc txInfos

cmdReviewTx :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdReviewTx ctx cfg nameM fp =
  runDBResponse cfg $ do
    (_, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    tsd <- liftEitherIO $ readJsonFile fp
    txInfo <- liftEither $ parseTxSignData net ctx pub tsd
    txInfoL <- lift $ fillTxInfoLabels net txInfo
    return $ ResponseTx acc txInfoL

cmdExportTx :: Config -> TxHash -> FilePath -> IO Response
cmdExportTx cfg nosigH fp =
  runDBResponse cfg $ do
    pendingTxM <- lift $ getPendingTx nosigH
    case pendingTxM of
      Just (_, tsd, _) -> do
        checkPathFree fp
        liftIO $ writeJsonFile fp $ Json.toJSON tsd
        return $ ResponseFile fp
      _ -> throwError "The pending transaction does not exist"

cmdImportTx :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdImportTx ctx cfg nameM fp =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    tsd <- liftEitherIO $ readJsonFile fp
    txInfo <- liftEither $ parseTxSignData net ctx pub tsd
    txInfoL <- lift $ fillTxInfoLabels net txInfo
    _ <- importPendingTx net ctx accId tsd
    return $ ResponseTx acc txInfoL

cmdDeleteTx :: Ctx -> Config -> TxHash -> IO Response
cmdDeleteTx ctx cfg nosigH =
  runDBResponse cfg $ do
    (coins, addrs) <- deletePendingTx ctx nosigH
    return $ ResponseDeleteTx nosigH coins addrs

cmdSignTx ::
  Ctx ->
  Config ->
  Maybe Text ->
  Maybe TxHash ->
  Maybe FilePath ->
  Maybe FilePath ->
  Natural ->
  IO Response
cmdSignTx ctx cfg nameM nosigHM inputM outputM splitMnemIn =
  runDBResponse cfg $ do
    (tsd, online) <- parseSignInput nosigHM inputM outputM
    when online $
      throwError "The transaction is already online"
    when (txSignDataSigned tsd) $
      throwError "The transaction is already signed"
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        idx = fromIntegral $ dBAccountIndex acc
        accPub = accountXPubKey ctx acc
    for_ outputM checkPathFree
    mnem <- askMnemonicPass splitMnemIn
    prvKey <- liftEither $ signingKey net ctx mnem idx
    let xpub = deriveXPubKey ctx prvKey
    unless (accPub == xpub) $
      throwError "The mnemonic did not match the provided account"
    (newSignData, txInfo) <- liftEither $ signWalletTx net ctx tsd prvKey
    txInfoL <- lift $ fillTxInfoLabels net txInfo
    when (isJust nosigHM) $ void $ importPendingTx net ctx accId newSignData
    for_ outputM $ \o -> liftIO $ writeJsonFile o $ Json.toJSON newSignData
    return $ ResponseTx acc txInfoL

parseSignInput ::
  (MonadUnliftIO m) =>
  Maybe TxHash ->
  Maybe FilePath ->
  Maybe FilePath ->
  ExceptT String (DB m) (TxSignData, Bool)
parseSignInput nosigHM inputM outputM =
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
        Just (_, t, TxInfoPending _ _ online) -> return (t, online)
        _ -> throwError "The nosigHash does not exist in the wallet"
    (_, Just i, _) -> do
      exist <- liftIO $ D.doesFileExist i
      unless exist $ throwError "Input file does not exist"
      tsd <- liftEitherIO (readJsonFile i)
      return (tsd, False)

cmdCoins :: Config -> Maybe Text -> Page -> IO Response
cmdCoins cfg nameM page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
    coins <- coinPage net accId page
    return $ ResponseCoins acc coins

cmdSendTx :: Ctx -> Config -> TxHash -> IO Response
cmdSendTx ctx cfg nosigH =
  runDBResponse cfg $ do
    tsdM <- lift $ getPendingTx nosigH
    case tsdM of
      Just (accId, tsd@(TxSignData signedTx _ _ _ signed), _) -> do
        acc <- getAccountById accId
        let net = accountNetwork acc
            pub = accountXPubKey ctx acc
        txInfo <- liftEither $ parseTxSignData net ctx pub tsd
        txInfoL <- lift $ fillTxInfoLabels net txInfo
        let verify = verifyTxInfo net ctx signedTx txInfoL
        unless (signed && verify) $ throwError "The transaction is not signed"
        checkHealth ctx net cfg
        let host = apiHost net cfg
        Store.TxId netTxId <- liftExcept $ apiCall ctx host (PostTx signedTx)
        unless (netTxId == txHash signedTx) $
          throwError $
            "The server returned the wrong TxHash: "
              <> cs (txHashToHex netTxId)
        _ <- lift $ setPendingTxOnline nosigH
        lift $ insertRawTx signedTx
        return $ ResponseTx acc $ setTxInfoOnline txInfoL
      _ -> throwError "The nosigHash does not exist in the wallet"

setTxInfoOnline :: TxInfo -> TxInfo
setTxInfoOnline txInfo =
  case txInfoPending txInfo of
    Just p -> txInfo {txInfoPending = Just p {pendingOnline = True}}
    _ -> txInfo

cmdSyncAcc :: Ctx -> Config -> Maybe Text -> Bool -> IO Response
cmdSyncAcc ctx cfg nameM full =
  runDBResponse cfg $ do
    accs <-
      case nameM of
        Just _ -> (:[]) <$> getAccountByName nameM
        _ -> lift getAccounts
    when (null accs) $ throwError "There are no accounts in the wallet"
    res <- forM accs $ \(accId, acc) -> do
      let net = accountNetwork acc
      sync ctx cfg net accId full
    return $ ResponseSync res

cmdDiscoverAccount :: Ctx -> Config -> Maybe Text -> IO Response
cmdDiscoverAccount ctx cfg nameM = do
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    checkHealth ctx net cfg
    (e, i) <- discoverAddrs net ctx cfg pub
    discoverAccGenAddrs ctx cfg accId AddrExternal $ fromIntegral e
    discoverAccGenAddrs ctx cfg accId AddrInternal $ fromIntegral i
    -- Perform a full sync after discovery
    res <- sync ctx cfg net accId True
    return $ ResponseSync [res]

cmdBackup :: Ctx -> Config -> FilePath -> IO Response
cmdBackup ctx cfg fp =
  runDBResponse cfg $ do
    backup <- createBackup ctx
    checkPathFree fp
    liftIO $ writeJsonFile fp $ marshalValue ctx backup
    return $ ResponseFile fp

cmdRestore :: Ctx -> Config -> FilePath -> IO Response
cmdRestore ctx cfg fp =
  runDBResponse cfg $ do
    backup <- liftEitherIO $ readMarshalFile ctx fp
    let f (SyncRes a _ _ t c) = (a, t, c)
    ResponseRestore . (f <$>) <$> restoreBackup ctx cfg backup

cmdVersion :: Config -> IO Response
cmdVersion cfg = do
  runDBResponse cfg $ do
    dbv <- lift getVersion
    return $ ResponseVersion currentVersionStr (cs $ verString dbv)

prepareSweep ::
  Ctx ->
  Config ->
  Maybe Text ->
  FilePath ->
  [Text] ->
  Maybe FilePath ->
  Natural ->
  Natural ->
  IO Response
prepareSweep ctx cfg nameM prvKeyFile sweepToT outputM feeByte dust =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    secKeys <- parseSecKeysFile net <$> liftIO (readFileWords prvKeyFile)
    sweepTo <- liftEither $ mapM (textToAddrE net) sweepToT
    checkHealth ctx net cfg
    tsd <- buildSweepSignData net ctx cfg accId secKeys sweepTo feeByte dust
    txInfo <- liftEither $ parseTxSignData net ctx pub tsd
    txInfoL <- lift $ fillTxInfoLabels net txInfo
    for_ outputM checkPathFree
    _ <- importPendingTx net ctx accId tsd
    for_ outputM $ \file -> liftIO $ writeJsonFile file $ Json.toJSON tsd
    return $ ResponseTx acc txInfoL

signSweep ::
  Ctx ->
  Config ->
  Maybe Text ->
  Maybe TxHash ->
  Maybe FilePath ->
  Maybe FilePath ->
  FilePath ->
  IO Response
signSweep ctx cfg nameM nosigHM inputM outputM keyFile =
  runDBResponse cfg $ do
    (tsd, online) <- parseSignInput nosigHM inputM outputM
    when online $
      throwError "The transaction is already online"
    when (txSignDataSigned tsd) $
      throwError "The transaction is already signed"
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    for_ outputM checkPathFree
    -- Read the file containing the private keys
    secKeys <- parseSecKeysFile net <$> liftIO (readFileWords keyFile)
    when (null secKeys) $ throwError "No private keys to sign"
    -- Sign the transactions
    (newTsd, txInfo) <- liftEither $ signTxWithKeys net ctx tsd pub secKeys
    txInfoL <- lift $ fillTxInfoLabels net txInfo
    when (isJust nosigHM) $ void $ importPendingTx net ctx accId newTsd
    for_ outputM $ \o -> liftIO $ writeJsonFile o $ Json.toJSON newTsd
    return $ ResponseTx acc txInfoL

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

askMnemonicPass :: (MonadError String m, MonadIO m) => Natural -> m MnemonicPass
askMnemonicPass splitMnemIn = do
  mnm <-
    if splitMnemIn == 1
      then liftIO $ askMnemonicWords "Enter your mnemonic words: "
      else do
        ms <- forM [1 .. splitMnemIn] $ \n ->
          liftIO $ askMnemonicWords $ "Split mnemonic part #" <> show n <> ": "
        liftEither $ mergeMnemonicParts ms
  passStr <- liftIO askPassword
  return
    MnemonicPass
      { mnemonicWords = mnm,
        mnemonicPass = cs passStr
      }

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
