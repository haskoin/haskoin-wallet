{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Haskoin.Wallet.Commands where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Data.Aeson (object, (.:), (.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Foldable (for_)
import Data.List (nub, sort, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sqlite (runMigrationQuiet, runSqlite, transactionUndo)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Entropy
import Haskoin.Wallet.FileIO
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
  | ResponseTestAcc
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
  | ResponseTxInfo
      { responseAccount :: !DBAccount,
        responsePendingTx :: !NoSigTxInfo
      }
  | ResponseTxInfos
      { responseAccount :: !DBAccount,
        responsePendingTxs :: ![NoSigTxInfo]
      }
  | ResponseDeleteTx
      { responseFreedCoins :: !Natural,
        responseFreedAddrs :: !Natural
      }
  | ResponseCoins
      { responseAccount :: !DBAccount,
        responseCoins :: ![JsonCoin]
      }
  | ResponseSync
      { responseAccount :: !DBAccount,
        responseBestBlock :: !BlockHash,
        responseBestHeight :: !Natural,
        responseTxCount :: !Natural,
        responseCoinCount :: !Natural
      }
  | ResponseVersion
      {responseVersion :: !Text}
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
      ResponseTestAcc a b t ->
        object
          [ "type" .= Json.String "testacc",
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
      ResponseTxInfo a t -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "txinfo",
            "account" .= a,
            "txinfo" .= marshalValue (net, ctx) t
          ]
      ResponseTxInfos a ts -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "txinfos",
            "account" .= a,
            "txinfos" .= (marshalValue (net, ctx) <$> ts)
          ]
      ResponseDeleteTx c a ->
        object
          [ "type" .= Json.String "deletetx",
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
      ResponseSync as bb bh tc cc ->
        object
          [ "type" .= Json.String "sync",
            "account" .= as,
            "bestblock" .= bb,
            "bestheight" .= bh,
            "txupdates" .= tc,
            "coinupdates" .= cc
          ]
      ResponseVersion v ->
        object ["type" .= Json.String "version", "version" .= v]
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
        "testacc" ->
          ResponseTestAcc
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
        "txinfo" -> do
          a <- o .: "account"
          let net = accountNetwork a
          t <- unmarshalValue (net, ctx) =<< o .: "txinfo"
          return $ ResponseTxInfo a t
        "txinfos" -> do
          a <- o .: "account"
          let net = accountNetwork a
          ts <- mapM (unmarshalValue (net, ctx)) =<< o .: "txinfos"
          return $ ResponseTxInfos a ts
        "deletetx" ->
          ResponseDeleteTx
            <$> o .: "freedcoins"
            <*> o .: "freedaddrs"
        "coins" -> do
          a <- o .: "account"
          xs <- o .: "coins"
          coins <- mapM (unmarshalValue (accountNetwork a)) xs
          return $ ResponseCoins a coins
        "sync" ->
          ResponseSync
            <$> o .: "account"
            <*> o .: "bestblock"
            <*> o .: "bestheight"
            <*> o .: "txupdates"
            <*> o .: "coinupdates"
        "version" ->
          ResponseVersion
            <$> o .: "version"
        "rolldice" ->
          ResponseRollDice
            <$> o .: "dice"
            <*> o .: "entropysource"
        _ -> fail "Invalid JSON response type"

runDB :: (MonadUnliftIO m) => ExceptT String (DB m) Response -> m Response
runDB action = do
  dir <- liftIO hwDataDirectory
  let dbFile = dir </> "accounts.sqlite"
  runSqlite (cs dbFile) $ do
    _ <- runMigrationQuiet migrateAll
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

commandResponse :: Ctx -> Config -> Command -> IO Response
commandResponse ctx cfg cmd =
  case cmd of
    -- Mnemonic and account management
    CommandMnemonic e d s -> cmdMnemonic e d s
    CommandCreateAcc t n dM s -> cmdCreateAcc ctx t n dM s
    CommandTestAcc nameM s -> cmdTestAcc ctx nameM s
    CommandRenameAcc old new -> cmdRenameAcc old new
    CommandAccounts nameM -> cmdAccounts nameM
    -- Address management
    CommandReceive nameM labM -> cmdReceive ctx cfg nameM labM
    CommandAddrs nameM p -> cmdAddrs nameM p
    CommandLabel nameM i l -> cmdLabel nameM i l
    -- Transaction management
    CommandTxs nameM p -> cmdTxs ctx nameM p
    CommandPrepareTx rcpts nameM unit fee dust rcptPay o ->
      cmdPrepareTx ctx cfg rcpts nameM unit fee dust rcptPay o
    CommandPendingTxs nameM p -> cmdPendingTxs ctx nameM p
    CommandSignTx nameM h i o s -> cmdSignTx ctx nameM h i o s
    CommandDeleteTx nameM h -> cmdDeleteTx ctx nameM h
    CommandCoins nameM p -> cmdCoins nameM p
    -- Import/export commands
    CommandExportAcc nameM f -> cmdExportAcc ctx nameM f
    CommandImportAcc f -> cmdImportAcc ctx f
    CommandReviewTx nameM file -> cmdReviewTx ctx nameM file
    CommandExportTx h f -> cmdExportTx h f
    CommandImportTx nameM file -> cmdImportTx ctx nameM file
    -- Online commands
    CommandSendTx nameM h -> cmdSendTx ctx cfg nameM h
    CommandSyncAcc nameM full -> cmdSyncAcc ctx cfg nameM full
    CommandDiscoverAcc nameM -> cmdDiscoverAccount ctx cfg nameM
    -- Utilities
    CommandVersion -> cmdVersion
    CommandPrepareSweep nameM sf fileM st outputM f d ->
      prepareSweep ctx cfg nameM sf fileM st outputM f d
    CommandSignSweep nameM h i o k -> signSweep ctx nameM h i o k
    CommandRollDice n -> rollDice n

-- runDB Monad Stack:
-- ExceptT String (ReaderT SqlBackend (NoLoggingT (ResourceT (IO))))

liftEitherIO :: (MonadIO m) => IO (Either String a) -> ExceptT String m a
liftEitherIO = liftEither <=< liftIO

cmdMnemonic :: Natural -> Bool -> Natural -> IO Response
cmdMnemonic ent useDice splitMnemIn =
  catchResponseError $ do
    (orig, ms, splitMs) <- genMnemonic ent useDice splitMnemIn
    return $ ResponseMnemonic orig (Text.words ms) (Text.words <$> splitMs)

cmdCreateAcc ::
  Ctx -> Text -> Network -> Maybe Natural -> Natural -> IO Response
cmdCreateAcc ctx name net derivM splitMnemIn = do
  runDB $ do
    mnem <- askMnemonicPass splitMnemIn
    walletFP <- liftEither $ walletFingerprint net ctx mnem
    d <- maybe (lift $ nextAccountDeriv walletFP net) return derivM
    prvKey <- liftEither $ signingKey net ctx mnem d
    let xpub = deriveXPubKey ctx prvKey
    (_, acc) <- insertAccount net ctx walletFP name xpub
    return $ ResponseAccount acc

cmdTestAcc :: Ctx -> Maybe Text -> Natural -> IO Response
cmdTestAcc ctx nameM splitMnemIn =
  runDB $ do
    (_, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        xPubKey = accountXPubKey ctx acc
        d = accountIndex acc
    mnem <- askMnemonicPass splitMnemIn
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
  runDB $ do
    (PubKeyDoc xpub net name wallet) <- liftEitherIO $ readMarshalFile ctx fp
    (_, acc) <- insertAccount net ctx wallet name xpub
    return $ ResponseAccount acc

cmdExportAcc :: Ctx -> Maybe Text -> FilePath -> IO Response
cmdExportAcc ctx nameM file =
  runDB $ do
    (_, acc) <- getAccountByName nameM
    checkPathFree file
    let xpub = accountXPubKey ctx acc
        net = accountNetwork acc
        name = dBAccountName acc
        wallet = accountWallet acc
        doc = PubKeyDoc xpub net name wallet
    liftIO $ writeMarshalFile ctx file doc
    return $ ResponseFile file

cmdRenameAcc :: Text -> Text -> IO Response
cmdRenameAcc oldName newName =
  runDB $ do
    acc <- renameAccount oldName newName
    return $ ResponseAccount acc

cmdAccounts :: Maybe Text -> IO Response
cmdAccounts nameM =
  runDB $ do
    case nameM of
      Just _ -> do
        (_, acc) <- getAccountByName nameM
        return $ ResponseAccounts [acc]
      _ -> do
        accs <- lift getAccounts
        return $ ResponseAccounts $ snd <$> accs

cmdReceive :: Ctx -> Config -> Maybe Text -> Maybe Text -> IO Response
cmdReceive ctx cfg nameM labelM =
  runDB $ do
    (accId, acc) <- getAccountByName nameM
    addr <- genExtAddress ctx cfg accId $ fromMaybe "" labelM
    return $ ResponseAddress acc addr

cmdAddrs :: Maybe Text -> Page -> IO Response
cmdAddrs nameM page =
  runDB $ do
    (accId, acc) <- getAccountByName nameM
    as <- lift $ addressPage accId page
    return $ ResponseAddresses acc as

cmdLabel :: Maybe Text -> Natural -> Text -> IO Response
cmdLabel nameM idx lab =
  runDB $ do
    (accId, acc) <- getAccountByName nameM
    adr <- setAddrLabel accId (fromIntegral idx) lab
    return $ ResponseAddress acc adr

cmdTxs :: Ctx -> Maybe Text -> Page -> IO Response
cmdTxs ctx nameM page =
  runDB $ do
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
  Maybe FilePath ->
  IO Response
cmdPrepareTx ctx cfg rcpTxt nameM unit feeByte dust rcptPay fileM =
  runDB $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    rcpts <- liftEither $ mapM (toRecipient net) rcpTxt
    gen <- liftIO initStdGen
    signDat <- buildTxSignData net ctx cfg gen accId rcpts feeByte dust rcptPay
    txInfoU <- liftEither $ parseTxSignData net ctx pub signDat
    for_ fileM checkPathFree
    nosigHash <- importPendingTx net ctx accId signDat
    for_ fileM $ \file -> liftIO $ writeJsonFile file $ Json.toJSON signDat
    newAcc <- getAccountById accId
    return $ ResponseTxInfo newAcc $ NoSigUnsigned nosigHash txInfoU
  where
    toRecipient net (a, v) = do
      addr <- textToAddrE net a
      val <- maybeToEither (cs $ badAmnt v) (readAmount unit v)
      return (addr, val)
    badAmnt v =
      "Could not parse the amount " <> v <> " as " <> showUnit unit 1

cmdPendingTxs :: Ctx -> Maybe Text -> Page -> IO Response
cmdPendingTxs ctx nameM page =
  runDB $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    tsds <- pendingTxPage accId page
    txs <- forM tsds $ \(nosigH, tsd@(TxSignData tx _ _ _ signed)) -> do
      txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
      return $
        if signed
          then NoSigSigned nosigH $ unsignedToTxInfo tx txInfoU
          else NoSigUnsigned nosigH txInfoU
    return $ ResponseTxInfos acc txs

cmdReviewTx :: Ctx -> Maybe Text -> FilePath -> IO Response
cmdReviewTx ctx nameM fp =
  runDB $ do
    (_, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    tsd@(TxSignData tx _ _ _ signed) <- liftEitherIO $ readJsonFile fp
    txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
    let txInfo = unsignedToTxInfo tx txInfoU
        nosigHash = nosigTxHash tx
    return $
      ResponseTxInfo acc $
        if signed
          then NoSigSigned nosigHash txInfo
          else NoSigUnsigned nosigHash txInfoU

cmdExportTx :: TxHash -> FilePath -> IO Response
cmdExportTx nosigH fp =
  runDB $ do
    pendingTxM <- lift $ getPendingTx nosigH
    case pendingTxM of
      Just (tsd, _) -> do
        checkPathFree fp
        liftIO $ writeJsonFile fp $ Json.toJSON tsd
        return $ ResponseFile fp
      _ -> throwError "The pending transaction does not exist"

cmdImportTx :: Ctx -> Maybe Text -> FilePath -> IO Response
cmdImportTx ctx nameM fp =
  runDB $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    tsd@(TxSignData tx _ _ _ signed) <- liftEitherIO $ readJsonFile fp
    txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
    let txInfo = unsignedToTxInfo tx txInfoU
    nosigHash <- importPendingTx net ctx accId tsd
    return $
      ResponseTxInfo acc $
        if signed
          then NoSigSigned nosigHash txInfo
          else NoSigUnsigned nosigHash txInfoU

cmdDeleteTx :: Ctx -> Maybe Text -> TxHash -> IO Response
cmdDeleteTx ctx nameM nosigH =
  runDB $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
    (coins, addrs) <- deletePendingTx net ctx accId nosigH
    return $ ResponseDeleteTx coins addrs

cmdSignTx ::
  Ctx ->
  Maybe Text ->
  Maybe TxHash ->
  Maybe FilePath ->
  Maybe FilePath ->
  Natural ->
  IO Response
cmdSignTx ctx nameM nosigHM inputM outputM splitMnemIn =
  runDB $ do
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
    let nosigH = nosigTxHash $ txSignDataTx newSignData
    when (isJust nosigHM && Just nosigH /= nosigHM) $
      throwError "The nosigHash did not match"
    when (isJust nosigHM) $ void $ importPendingTx net ctx accId newSignData
    for_ outputM $ \o -> liftIO $ writeJsonFile o $ Json.toJSON newSignData
    return $ ResponseTxInfo acc (NoSigSigned nosigH txInfo)

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
        Just res -> return res
        _ -> throwError "The nosigHash does not exist in the wallet"
    (_, Just i, _) -> do
      exist <- liftIO $ D.doesFileExist i
      unless exist $ throwError "Input file does not exist"
      (,False) <$> liftEitherIO (readJsonFile i)

cmdCoins :: Maybe Text -> Page -> IO Response
cmdCoins nameM page =
  runDB $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
    coins <- coinPage net accId page
    return $ ResponseCoins acc coins

cmdSendTx :: Ctx -> Config -> Maybe Text -> TxHash -> IO Response
cmdSendTx ctx cfg nameM nosigH =
  runDB $ do
    (_, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    tsdM <- lift $ getPendingTx nosigH
    case tsdM of
      Just (tsd@(TxSignData signedTx _ _ _ signed), _) -> do
        txInfoU <- liftEither $ parseTxSignData net ctx pub tsd
        let txInfo = unsignedToTxInfo signedTx txInfoU
            verify = verifyTxInfo net ctx signedTx txInfo
        unless (signed && verify) $ throwError "The transaction is not signed"
        checkHealth ctx net cfg
        let host = apiHost net cfg
        Store.TxId netTxId <- liftExcept $ apiCall ctx host (PostTx signedTx)
        unless (netTxId == txHash signedTx) $
          throwError $
            "The server returned the wrong TxHash: "
              <> cs (txHashToHex netTxId)
        _ <- lift $ setPendingTxOnline nosigH
        return $ ResponseTxInfo acc $ NoSigSigned nosigH txInfo
      _ -> throwError "The nosigHash does not exist in the wallet"

cmdSyncAcc :: Ctx -> Config -> Maybe Text -> Bool -> IO Response
cmdSyncAcc ctx cfg nameM full =
  runDB $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        host = apiHost net cfg
    -- Check API health
    checkHealth ctx net cfg
    -- Get the new best block before starting the sync
    best <- liftExcept $ apiCall ctx host (GetBlockBest def)
    -- Get the addresses from our local database
    (addrPathMap, addrBalMap) <- allAddressesMap net accId
    -- Fetch the address balances online
    Store.SerialList storeBals <-
      liftExcept . apiBatch ctx (configAddrBatch cfg) host $
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
    aTids <- searchAddrTxs net ctx cfg confirmedTxs addrsToUpdate
    -- We also want to check if there is any change in unconfirmed txs
    uTids <- getConfirmedTxs accId False
    let tids = nub $ uTids <> aTids
    -- Fetch the full transactions
    Store.SerialList txs <-
      liftExcept $ apiBatch ctx (configTxFullBatch cfg) host (GetTxs tids)
    -- Convert them to TxInfo and store them in the local database
    let txInfos = toTxInfo addrPathMap (fromIntegral best.height) <$> txs
    resTxInfo <- lift $ forM txInfos $ repsertTxInfo net ctx accId
    -- Fetch and update coins
    Store.SerialList storeCoins <-
      liftExcept . apiBatch ctx (configCoinBatch cfg) host $
        GetAddrsUnspent addrsToUpdate def
    (coinCount, newCoins) <- refreshCoins net accId addrsToUpdate storeCoins
    -- Get the dependent tranactions of the new coins
    depTxsHash <-
      if full
        then return $ (.outpoint.hash) <$> storeCoins
        else mapM (liftEither . coinToTxHash) newCoins
    Store.RawResultList rawTxs <-
      liftExcept
        . apiBatch ctx (configTxFullBatch cfg) host
        $ GetTxsRaw
        $ nub depTxsHash
    lift $ forM_ rawTxs insertRawTx
    -- Remove pending transactions if they are online
    pendingTids <- pendingTxHashes accId
    let toRemove = filter ((`elem` tids) . fst) pendingTids
    forM_ toRemove $ \(_, key) -> lift $ deletePendingTxOnline key
    -- Update the best block for this network
    lift $ updateBest net (headerHash best.header) best.height
    return $
      ResponseSync
        newAcc
        (headerHash best.header)
        (fromIntegral best.height)
        (fromIntegral $ length $ filter id $ snd <$> resTxInfo)
        (fromIntegral coinCount)

coinToTxHash :: DBCoin -> Either String TxHash
coinToTxHash coin =
  maybeToEither "coinToTxHash: Invalid outpoint" $ do
    bs <- decodeHex $ dBCoinOutpoint coin
    op <- eitherToMaybe (S.decode bs) :: Maybe OutPoint
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
  Config ->
  [TxHash] ->
  [Address] ->
  ExceptT String m [TxHash]
searchAddrTxs _ _ _ _ [] = return []
searchAddrTxs net ctx cfg confirmedTxs as
  | length as > fromIntegral (configAddrBatch cfg) =
      nub . concat <$> mapM (go Nothing 0) (chunksOf (configAddrBatch cfg) as)
  | otherwise =
      nub <$> go Nothing 0 as
  where
    go hashM offset xs = do
      Store.SerialList txRefs <-
        liftExcept $
          apiCall
            ctx
            (apiHost net cfg)
            ( GetAddrsTxs
                xs
                def
                  { limit = Just $ fromIntegral (configTxBatch cfg),
                    start = StartParamHash <$> hashM,
                    offset = offset
                  }
            )
      -- Remove txs that we already have
      let tids = ((.txid) <$> txRefs) \\ confirmedTxs
      -- Either we have reached the end of the stream, or we have hit some
      -- txs in confirmedTxs. In both cases, we can stop the search.
      if length tids < fromIntegral (configTxBatch cfg)
        then return tids
        else do
          let lastId = (last tids).get
          rest <- go (Just lastId) 1 xs
          return $ tids <> rest

cmdDiscoverAccount :: Ctx -> Config -> Maybe Text -> IO Response
cmdDiscoverAccount ctx cfg nameM = do
  res <- runDB $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    checkHealth ctx net cfg
    let recoveryGap = configRecoveryGap cfg
    e <- go net pub extDeriv 0 (Page recoveryGap 0)
    i <- go net pub intDeriv 0 (Page recoveryGap 0)
    discoverAccGenAddrs ctx cfg accId AddrExternal e
    discoverAccGenAddrs ctx cfg accId AddrInternal i
    return $ ResponseSync acc "" 0 0 0
  -- Perform a full sync after discovery
  case res of
    ResponseError _ -> return res
    _ -> cmdSyncAcc ctx cfg nameM True
  where
    go net pub path d page@(Page lim off) = do
      let addrs = addrsDerivPage ctx path page pub
          req = GetAddrsBalance $ fst <$> addrs
      let host = apiHost net cfg
      Store.SerialList bals <- liftExcept $ apiCall ctx host req
      let vBals = filter ((/= 0) . (.txs)) bals
      if null vBals
        then return d
        else do
          let dMax = findMax addrs $ (.address) <$> vBals
          go net pub path (dMax + 1) (Page lim (off + lim))
    -- Find the largest ID amongst the addresses that have a positive balance
    findMax :: [(Address, SoftPath)] -> [Address] -> Int
    findMax addrs balAddrs =
      let fAddrs = filter ((`elem` balAddrs) . fst) addrs
       in fromIntegral $ maximum $ last . pathToList . snd <$> fAddrs

cmdVersion :: IO Response
cmdVersion = return $ ResponseVersion versionString

prepareSweep ::
  Ctx ->
  Config ->
  Maybe Text ->
  [Text] ->
  Maybe FilePath ->
  [Text] ->
  Maybe FilePath ->
  Natural ->
  Natural ->
  IO Response
prepareSweep ctx cfg nameM sweepFromT sweepFromFileM sweepToT outputM feeByte dust =
  runDB $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    sweepFromArg <- liftEither $ mapM (textToAddrE net) sweepFromT
    sweepTo <- liftEither $ mapM (textToAddrE net) sweepToT
    addrsFile <-
      case sweepFromFileM of
        Just file -> parseAddrsFile net <$> liftIO (readFileWords file)
        _ -> return []
    let sweepFrom = sweepFromArg <> addrsFile
    checkHealth ctx net cfg
    tsd <- buildSweepSignData net ctx cfg accId sweepFrom sweepTo feeByte dust
    info <- liftEither $ parseTxSignData net ctx pub tsd
    for_ outputM checkPathFree
    nosigHash <- importPendingTx net ctx accId tsd
    for_ outputM $ \file -> liftIO $ writeJsonFile file $ Json.toJSON tsd
    return $ ResponseTxInfo acc (NoSigUnsigned nosigHash info)

signSweep ::
  Ctx ->
  Maybe Text ->
  Maybe TxHash ->
  Maybe FilePath ->
  Maybe FilePath ->
  FilePath ->
  IO Response
signSweep ctx nameM nosigHM inputM outputM keyFile =
  runDB $ do
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
    let nosigH = nosigTxHash $ txSignDataTx newTsd
    when (isJust nosigHM && Just nosigH /= nosigHM) $
      throwError "The nosigHash did not match"
    when (isJust nosigHM) $ void $ importPendingTx net ctx accId newTsd
    for_ outputM $ \o -> liftIO $ writeJsonFile o $ Json.toJSON newTsd
    return $ ResponseTxInfo acc (NoSigSigned nosigH txInfo)

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

checkHealth ::
  (MonadIO m) =>
  Ctx ->
  Network ->
  Config ->
  ExceptT String (DB m) ()
checkHealth ctx net cfg = do
  let host = apiHost net cfg
  health <- liftExcept $ apiCall ctx host GetHealth
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
