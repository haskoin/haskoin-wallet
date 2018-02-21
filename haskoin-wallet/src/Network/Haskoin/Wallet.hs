{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Haskoin.Wallet where

import           Control.Arrow                              (right, (&&&))
import           Control.Monad                              (unless, when)
import qualified Data.Aeson                                 as Json
import           Data.Aeson.TH
import           Data.List                                  (sortOn)
import           Data.Map.Strict                            (Map)
import qualified Data.Map.Strict                            as Map
import           Data.Text                                  (Text)
import           Data.Tree                                  (Tree (Node))
import           Foundation
import           Foundation.Collection
import           Foundation.Compat.Text
import           Foundation.IO
import           Foundation.String
import           Foundation.VFS
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto                     hiding
                                                             (addrToBase58,
                                                             base58ToAddr,
                                                             xPubExport)
import           Network.Haskoin.Transaction                hiding (txHashToHex)
import           Network.Haskoin.Util                       (dropFieldLabel)
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.ConsolePrinter
import           Network.Haskoin.Wallet.Entropy
import           Network.Haskoin.Wallet.FoundationCompat
import           Network.Haskoin.Wallet.HTTP
import           Network.Haskoin.Wallet.HTTP.BlockchainInfo
import           Network.Haskoin.Wallet.HTTP.Haskoin
import           Network.Haskoin.Wallet.HTTP.Insight
import           Network.Haskoin.Wallet.Signing
import           Network.Haskoin.Wallet.TxInformation
import           Network.Haskoin.Wallet.UsageInfo
import qualified System.Console.Argument                    as Argument
import qualified System.Console.Haskeline                   as Haskeline
import qualified System.Console.Program                     as Program
import qualified System.Directory                           as D

data DocStructure a = DocStructure
    { docStructureNetwork :: !Text
    , docStructurePayload :: !a
    } deriving (Eq, Show)

$(deriveJSON (dropFieldLabel 12) ''DocStructure)

{- Options -}

diceOpt :: Option Bool
diceOpt =
    option
        'd'
        "dice"
        ["True"]
        False
        Argument.boolean
        "Provide additional entropy using 6-sided dice."

entOpt :: Option Natural
entOpt =
    option
        'e'
        "entropy"
        ["[16,20..32]"]
        16
        (fromIntegral <$> Argument.natural)
        "Use more entropy to generate a mnemonic."

derivOpt :: Option Natural
derivOpt =
    option
        'd'
        "deriv"
        ["1"]
        0
        (fromIntegral <$> Argument.natural)
        "Specify a different bip44 account derivation."

netOpt :: Option String
netOpt =
    option
        'n'
        "network"
        ["bitcoin", "testnet3", "bitcoincash", "cashtest"]
        "bitcoin"
        (fromLString <$> Argument.string)
        ""

accOpt :: Option String
accOpt =
    option
        'a'
        "account"
        ["main"]
        ""
        (fromLString <$> Argument.string)
        "Specify a different account to use for this command."

cntOpt :: Option Natural
cntOpt =
    option
        'c'
        "count"
        ["10"]
        5
        (fromIntegral <$> Argument.natural)
        "Number of addresses to display."

feeOpt :: Option Natural
feeOpt =
    option
        'f'
        "fee"
        ["50"]
        200
        (fromIntegral <$> Argument.natural)
        "Fee to pay in sat/bytes"

dustOpt :: Option Natural
dustOpt =
    option
        'd'
        "dust"
        ["8000"]
        5430
        (fromIntegral <$> Argument.natural)
        "Smallest allowed satoshi value for change outputs."

unitOpt :: Option String
unitOpt =
    option
        'u'
        "unit"
        ["bitcoin", "bit", "satoshi"]
        "bitcoin"
        (fromLString <$> Argument.string)
        "Specify the unit for displayed amounts."

serviceOpt :: Option String
serviceOpt =
    option
        's'
        "service"
        ["haskoin", "blockchain", "insight"]
        "haskoin"
        (fromLString <$> Argument.string)
        "HTTP data service."

verboseOpt :: Option Bool
verboseOpt =
    option
        'v'
        "verbose"
        ["True"]
        False
        Argument.boolean
        "Produce a more detailed output for this command."

setOptNet :: String -> IO ()
setOptNet name
    | name == fromLString (getNetworkName bitcoinNetwork) = do
        setBitcoinNetwork
        renderIO $ formatBitcoin "--- Bitcoin ---"
    | name == fromLString (getNetworkName bitcoinCashNetwork) = do
        setBitcoinCashNetwork
        renderIO $ formatCash "--- Bitcoin Cash ---"
    | name == fromLString (getNetworkName testnet3Network) = do
        setTestnet3Network
        renderIO $ formatTestnet "--- Testnet ---"
    | name == fromLString (getNetworkName cashTestNetwork) = do
        setCashTestNetwork
        renderIO $ formatTestnet "--- Bitcoin Cash Testnet ---"
    | otherwise =
        consoleError $
        vcat
            [ formatError "Invalid network name. Select one of the following:"
            , nest 4 $
              vcat $
              fmap
                  (formatStatic . fromLString)
                  [ getNetworkName bitcoinNetwork
                  , getNetworkName bitcoinCashNetwork
                  , getNetworkName testnet3Network
                  , getNetworkName cashTestNetwork
                  ]
            ]

parseUnit :: String -> AmountUnit
parseUnit unit =
    case unit of
        "bitcoin" -> UnitBitcoin
        "bit" -> UnitBit
        "satoshi" -> UnitSatoshi
        _ ->
            consoleError $
            vcat
                [ formatError "Invalid unit value. Choose one of:"
                , nest 4 $
                  vcat $ fmap formatStatic ["bitcoin", "bit", "satoshi"]
                ]

parseBlockchainService :: String -> Service
parseBlockchainService service =
    case service of
        "" -> defaultBlockchainService
        "haskoin" -> Service HaskoinService
        "blockchain" -> Service BlockchainInfoService
        "insight" -> Service InsightService
        _ ->
            consoleError $
            vcat
                [ formatError
                      "Invalid service name. Select one of the following:"
                , nest 4 $
                  vcat $ fmap formatStatic ["haskoin", "blockchain", "insight"]
                ]

defaultBlockchainService :: Service
defaultBlockchainService = Service HaskoinService

{- Commands -}

clientMain :: IO ()
clientMain = Program.single (wrappedCommand <$> hwCommands)

hwCommands :: Commands IO
hwCommands =
    Node
        hw
        [ Node help []
        , Node mnemonic []
        , Node createacc []
        , Node importacc []
        , Node renameacc []
        , Node preparetx []
        , Node signtx []
        , Node prepareswipetx []
        , Node signswipetx []
        , Node sendtx []
        , Node receive []
        , Node addresses []
        , Node balance []
        , Node transactions []
        ]

hw :: Command IO
hw =
    command "hw" "Lightweight Bitcoin and Bitcoin Cash Wallet" Nothing [] $
    io $ renderIO (usage hwCommands)

help :: Command IO
help =
    command "help" "Display this information" Nothing [] $
    io $ renderIO (usage hwCommands)

mnemonic :: Command IO
mnemonic =
    command
        "mnemonic"
        "Generate a mnemonic using your systems entropy"
        (Just CommandOffline)
        [createacc, signtx] $
    withOption entOpt $ \reqEnt ->
    withOption diceOpt $ \useDice ->
        io $ do
            let ent = fromIntegral reqEnt
            mnemE <-
                if useDice
                    then genMnemonicDice ent =<< askDiceRolls ent
                    else genMnemonic ent
            case right (second bsToString) mnemE of
                Right (orig, Just ms) ->
                    renderIO $
                    vcat
                        [ formatTitle "System Entropy Source"
                        , nest 4 $ formatFilePath orig
                        , formatTitle "Private Mnemonic"
                        , nest 4 $ mnemonicPrinter 4 (words ms)
                        ]
                Right _ ->
                    consoleError $
                    formatError "There was a problem generating the mnemonic"
                Left err -> consoleError $ formatError err
  where
    askDiceRolls reqEnt =
        askInputLine $
        "Enter your " <> show (requiredRolls reqEnt) <> " dice rolls: "

mnemonicPrinter :: CountOf (Natural, String) -> [String] -> ConsolePrinter
mnemonicPrinter n ws =
    vcat $
    fmap (mconcat . fmap formatWord) $ groupIn n $ zip ([1 ..] :: [Natural]) ws
  where
    formatWord (i, w) =
        mconcat
            [formatKey $ block 4 $ show i <> ".", formatMnemonic $ block 10 w]

createacc :: Command IO
createacc =
    command
        "createacc"
        "Create a new account from a mnemonic"
        (Just CommandOffline)
        [importacc] $
    withOption derivOpt $ \deriv ->
    withOption netOpt $ \network ->
        io $ do
            setOptNet network
            xpub <- deriveXPubKey <$> askSigningKey (fromIntegral deriv)
            let fname = fromString $ "key-" <> toLString (xPubChecksum xpub)
            path <- writeDoc fname $ toText $ xPubExport xpub
            renderIO $
                vcat
                    [ formatTitle "Public Key"
                    , nest 4 $ formatPubKey $ xPubExport xpub
                    , formatTitle "Derivation"
                    , nest 4 $
                      formatDeriv $
                      show $
                      ParsedPrv $
                      toGeneric $ bip44Deriv deriv
                    , formatTitle "Public Key File"
                    , nest 4 $ formatFilePath $ filePathToString path
                    ]

importacc :: Command IO
importacc =
    command
        "importacc"
        "Import an account file into the wallet"
        Nothing
        [receive] $
    withOption netOpt $ \network ->
    withNonOption Argument.file "Filename" $ \fp ->
        io $ do
            setOptNet network
            xpub <- readDoc $ fromString fp :: IO XPubKey
            let store =
                    AccountStore
                        xpub
                        0
                        0
                        (bip44Deriv $ fromIntegral $ xPubChild xpub)
            name <- newAccountStore store
            renderIO $
                vcat
                    [ formatTitle "New Account Created"
                    , nest 4 $
                      vcat
                          [ formatKey (block 13 "Name:") <>
                            formatAccount name
                          , formatKey (block 13 "Derivation:") <>
                            formatDeriv
                                (show $
                                  ParsedPrv $
                                  toGeneric $ accountStoreDeriv store)
                          ]
                    ]

renameacc :: Command IO
renameacc =
    command
        "renameacc"
        "Rename an account"
        Nothing
        [] $
    withOption netOpt $ \network ->
    withNonOption Argument.string "OldName" $ \oldName ->
    withNonOption Argument.string "NewName" $ \newName ->
        io $ do
            setOptNet network
            renameAccountStore
                (fromLString oldName)
                (fromLString newName)
            renderIO $
                formatStatic "Account" <+>
                formatAccount (fromLString oldName) <+>
                formatStatic "renamed to" <+>
                formatAccount (fromLString newName)

preparetx :: Command IO
preparetx =
    command
        "preparetx"
        "Prepare a new unsigned transaction"
        (Just CommandOnline)
        [signtx] $
    withOption accOpt $ \acc ->
    withOption feeOpt $ \feeByte ->
    withOption dustOpt $ \dust ->
    withOption unitOpt $ \u ->
    withOption netOpt $ \network ->
    withOption serviceOpt $ \s ->
    withNonOptions Argument.string "Address Value [Address2 Value2 ...]" $ \as ->
        io $ do
            setOptNet network
            let !unit = parseUnit u
                !rcps =
                    Map.fromList $
                    fromMaybe rcptErr $ mapM (toRecipient unit) $
                    groupIn 2 $ fmap fromLString as
                !service = parseBlockchainService s
            withAccountStore acc $ \(k, store) -> do
                resE <- buildTxSignData service store rcps feeByte dust
                case resE of
                    Right (signDat, store') -> do
                        savePrepareTx store unit "tx" signDat
                        when (store /= store') $
                            updateAccountStore k $ const store'
                    Left err  -> consoleError $ formatError err
  where
    rcptErr = consoleError $ formatError "Could not parse the recipients"
    toRecipient :: AmountUnit -> [String] -> Maybe (Address, Satoshi)
    toRecipient unit [a, v] = (,) <$> base58ToAddr a <*> readAmount unit v
    toRecipient _ _         = Nothing

savePrepareTx ::
       AccountStore
    -> AmountUnit
    -> LString
    -> TxSignData
    -> IO ()
savePrepareTx store unit str signDat =
    case pubTxInfo signDat (accountStoreXPubKey store) of
        Right info -> do
            let chsum = txChksum $ txSignDataTx signDat
                fname =
                    fromString $ str <> "-" <> toLString chsum <> "-unsigned"
            path <- writeDoc fname signDat
            renderIO $
                vcat
                    [ txInfoFormat
                          (accountStoreDeriv store)
                          unit
                          (Just False)
                          Nothing
                          info
                    , formatTitle "Unsigned Tx Data File"
                    , nest 4 $ formatFilePath $ filePathToString path
                    ]
        Left err -> consoleError $ formatError err

txChksum :: Tx -> String
txChksum = take 16 . txHashToHex . nosigTxHash

signtx :: Command IO
signtx =
    command
        "signtx"
        "Sign a transaction that was created with preparetx"
        (Just CommandOffline)
        [sendtx] $
    withOption derivOpt $ \d ->
    withOption unitOpt $ \u ->
    withOption netOpt $ \network ->
    withNonOption Argument.file "Filename" $ \fp ->
        io $ do
            setOptNet network
            let !unit = parseUnit u
            dat <- readDoc $ fromString fp :: IO TxSignData
            signKey <- askSigningKey $ fromIntegral d
            case signWalletTx dat signKey of
                Right res -> saveSignedTx d unit "tx" res
                Left err  -> consoleError $ formatError err

prepareswipetx :: Command IO
prepareswipetx =
    command
        "prepareswipetx"
        "Prepare a transaction that swipes all the funds from a list of addresses"
        (Just CommandOnline)
        [signswipetx] $
    withOption accOpt $ \acc ->
    withOption feeOpt $ \feeByte ->
    withOption unitOpt $ \u ->
    withOption netOpt $ \network ->
    withOption serviceOpt $ \s ->
    withNonOptions Argument.string "Address [Address2 ...]" $ \as ->
        io $ do
            setOptNet network
            let !unit = parseUnit u
                !rcps =
                    fromMaybe
                        rcptErr $
                        mapM base58ToAddr (fromLString <$> as)
                !service = parseBlockchainService s
            withAccountStore acc $ \(k, store) -> do
                resE <- buildSwipeTx service store rcps feeByte
                case resE of
                    Right (signDat, store') -> do
                        savePrepareTx store unit "swipetx" signDat
                        when (store /= store') $
                            updateAccountStore k $ const store'
                    Left err  -> consoleError $ formatError err
  where
    rcptErr = consoleError $ formatError "Could not parse addresses"

signswipetx :: Command IO
signswipetx =
    command
        "signswipetx"
        "Sign a transaction that was created with prepareswipetx"
        (Just CommandOffline)
        [sendtx] $
    withOption derivOpt $ \d ->
    withOption unitOpt $ \u ->
    withOption netOpt $ \network ->
    withNonOption Argument.file "Filename" $ \fp ->
        io $ do
            setOptNet network
            let !unit = parseUnit u
            dat <- readDoc $ fromString fp :: IO TxSignData
            prvKeys <- askInputs "WIF Key" (fromWif . stringToBS)
            case signSwipeTx dat prvKeys of
                Right res -> saveSignedTx d unit "swipetx" res
                Left err  -> consoleError $ formatError err

askInputs :: String -> (String -> Maybe a) -> IO [a]
askInputs msg f = go []
  where
    go acc = do
        inputM <-
            Haskeline.runInputT Haskeline.defaultSettings $
            Haskeline.getPassword (Just '*') (toLString msg <> " (Enter when done):")
        case inputM of
            Just input ->
                maybe noParse (go . (:acc)) (f $ fromLString input)
            _ -> if null acc then noInput else return acc
    noInput = consoleError $ formatError "No input provided"
    noParse = consoleError $ formatError "Could not parse the input"

saveSignedTx ::
       Natural -> AmountUnit -> LString -> (TxInformation, Tx, Bool) -> IO ()
saveSignedTx d unit str (info, signedTx, isSigned) = do
    renderIO $ txInfoFormat (bip44Deriv d) unit (Just isSigned) Nothing info
    let signedHex = encodeHexText $ encodeBytes signedTx
        chsum = txChksum signedTx
        fname = fromString $ str <> "-" <> toLString chsum <> "-signed"
    path <- writeDoc fname signedHex
    renderIO $
        vcat
            [ formatTitle "Signed Tx File"
            , nest 4 $ formatFilePath $ filePathToString path
            ]

sendtx :: Command IO
sendtx =
    command
        "sendtx"
        "Broadcast a signed transaction"
        (Just CommandOnline)
        [] $
    withOption netOpt $ \network ->
    withOption serviceOpt $ \s ->
    withNonOption Argument.file "Filename" $ \fp ->
        io $ do
            setOptNet network
            let !service = parseBlockchainService s
            tx <- readDoc $ fromString fp :: IO Tx
            httpBroadcast service tx
            renderIO $
                formatStatic "Tx" <+>
                formatTxHash (txHashToHex $ txHash tx) <+>
                formatStatic "has been broadcast"

receive :: Command IO
receive =
    command
        "receive"
        "Generate a new address for receiving a payment"
        Nothing
        [] $
    withOption accOpt $ \acc ->
    withOption netOpt $ \network ->
        io $ do
            setOptNet network
            withAccountStore acc $ \(k, store) -> do
                let (addr, store') = nextExtAddress store
                updateAccountStore k $ const store'
                renderIO $
                    addressFormat $ nonEmpty_ [(thd &&& fst) addr]

addresses :: Command IO
addresses =
    command
        "addresses"
        "List the latest receiving addresses in your account"
        Nothing
        [] $
    withOption accOpt $ \acc ->
    withOption cntOpt $ \cnt ->
    withOption netOpt $ \network ->
        io $ do
            setOptNet network
            withAccountStore acc $ \(_, store) -> do
                let xpub = accountStoreXPubKey store
                    idx = accountStoreExternal store
                    start = fromMaybe 0 (idx - cnt)
                    count = fromIntegral $ fromMaybe 0 (idx - start)
                let addrsM =
                        nonEmpty $
                        take count $
                        derivePathAddrs xpub extDeriv $
                        fromIntegral start
                case addrsM of
                    Just addrs ->
                        renderIO $
                        addressFormat $
                        nonEmptyFmap (fromIntegral . thd &&& fst) addrs
                    _ ->
                        consoleError $
                        formatError "No addresses have been generated"

addressFormat :: NonEmpty [(Natural, Address)] -> ConsolePrinter
addressFormat as = vcat $ getNonEmpty $ nonEmptyFmap toFormat as
  where
    toFormat :: (Natural, Address) -> ConsolePrinter
    toFormat (i, a) =
        mconcat
            [ formatKey $ block (n + 2) $ show i <> ":"
            , formatAddress $ addrToBase58 a
            ]
    n = length $ show $ maximum $ nonEmptyFmap fst as

balance :: Command IO
balance =
    command
        "balance"
        "Display the account balance"
        (Just CommandOnline)
        [] $
    withOption accOpt $ \acc ->
    withOption unitOpt $ \u ->
    withOption netOpt $ \network ->
    withOption serviceOpt $ \s ->
        io $ do
            setOptNet network
            let !unit = parseUnit u
                !service = parseBlockchainService s
            withAccountStore acc $ \(_, store) -> do
                let addrs = allExtAddresses store <> allIntAddresses store
                bal <- httpBalance service $ fmap fst addrs
                renderIO $
                    vcat
                        [ formatTitle "Account Balance"
                        , nest 4 $ formatAmount unit bal
                        ]

transactions :: Command IO
transactions =
    command
        "transactions"
        "Display the account transactions"
        (Just CommandOnline)
        [] $
    withOption accOpt $ \acc ->
    withOption unitOpt $ \u ->
    withOption netOpt $ \network ->
    withOption serviceOpt $ \s ->
    withOption verboseOpt $ \verbose ->
        io $ do
            setOptNet network
            let !unit = parseUnit u
                !service = parseBlockchainService s
            withAccountStore acc $ \(_, store) -> do
                let walletAddrs = allExtAddresses store <> allIntAddresses store
                    walletAddrMap = Map.fromList walletAddrs
                txInfs <- httpTxInformation service $ fmap fst walletAddrs
                currHeight <- httpBestHeight service
                forM_ (sortOn txInfoHeight txInfs) $ \txInf -> do
                    let format =
                            if verbose
                                then txInfoFormat
                                else txInfoFormatCompact
                        txInfPath = txInfoFillPath walletAddrMap txInf
                    renderIO $
                        format
                            (accountStoreDeriv store)
                            unit
                            Nothing
                            (Just currHeight)
                            txInfPath

{- Command Line Helpers -}

writeDoc :: Json.ToJSON a => FileName -> a -> IO FilePath
writeDoc fileName dat = do
    dir <- fromString <$> D.getUserDocumentsDirectory
    let path = dir </> (fromString networkName <> "-" <> fileName <> ".json")
        val = encodeJsonPretty $ DocStructure (fromString networkName) dat
    withFile path WriteMode (`hPut` (val <> stringToBytes "\n"))
    return path

readDoc :: Json.FromJSON a => FilePath -> IO a
readDoc fileName = do
    bytes <- readFile fileName
    let m = fromMaybe err $ decodeJson bytes :: Map Text Json.Value
    case m Map.!? toText "network" of
        Just (Json.String net) ->
            if net == fromString networkName
                then case decodeJson bytes of
                         Just (DocStructure _ res) -> return res
                         _                         -> err
                else badNetErr $ fromText net
        _ -> err
  where
    err =
        consoleError $
        formatError $ "Could not read file " <> filePathToString fileName
    badNetErr net =
        consoleError $
        formatError $
        "Bad network. This file has to be used on the network: " <> net

withAccountStore :: String -> ((String, AccountStore) -> IO ()) -> IO ()
withAccountStore name f
    | null name = do
        accMap <- readAccountsFile
        case Map.assocs accMap of
            [val] -> f (first fromText val)
            _ ->
                case Map.lookup "main" accMap of
                    Just val -> f ("main", val)
                    _        -> err $ fromText <$> Map.keys accMap
    | otherwise = do
        accM <- getAccountStore name
        case accM of
            Just acc -> f (name, acc)
            _        -> err . fmap fromText . Map.keys =<< readAccountsFile
  where
    err :: [String] -> IO ()
    err [] = consoleError $ formatError "No accounts have been created"
    err keys =
        consoleError $
        vcat
            [ formatError
                  "Select one of the following accounts with -a or --account"
            , nest 4 $ vcat $ fmap formatAccount keys
            ]

askInputLineHidden :: String -> IO String
askInputLineHidden msg = do
    inputM <-
        Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getPassword (Just '*') (toLString msg)
    maybe
        (consoleError $ formatError "No action due to EOF")
        (return . fromLString)
        inputM

askInputLine :: String -> IO String
askInputLine msg = do
    inputM <-
        Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getInputLine (toLString msg)
    maybe
        (consoleError $ formatError "No action due to EOF")
        (return . fromLString)
        inputM

askSigningKey :: Natural -> IO XPrvKey
askSigningKey acc = do
    str <- askInputLineHidden "Enter your private mnemonic: "
    case mnemonicToSeed "" (stringToBS str) of
        Right _ -> do
            passStr <- askPassword
            either (consoleError . formatError) return $
                signingKey passStr str acc
        Left err -> consoleError $ formatError $ fromLString err

askPassword :: IO String
askPassword = do
    pass <- askInputLineHidden "Mnemonic password or leave empty: "
    unless (null pass) $ do
        pass2 <- askInputLineHidden "Repeat your mnemonic password: "
        when (pass /= pass2) $
            consoleError $ formatError "The passwords did not match"
    return pass
