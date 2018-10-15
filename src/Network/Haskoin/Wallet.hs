{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Haskoin.Wallet where

import           Control.Arrow                           (right, (&&&))
import           Control.Monad                           (unless, when)
import qualified Data.Aeson                              as Json
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.List                               (sortOn)
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import           Data.Text                               (Text)
import           Data.Tree                               (Tree (Node))
import           Foundation
import           Foundation.Collection
import           Foundation.Compat.Text
import           Foundation.IO
import           Foundation.String
import           Foundation.VFS
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util                    (dropFieldLabel)
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.Printer
import           Network.Haskoin.Wallet.Entropy
import           Network.Haskoin.Wallet.FoundationCompat
import           Network.Haskoin.Wallet.HTTP
import           Network.Haskoin.Wallet.Signing
import           Network.Haskoin.Wallet.DetailedTx
import           Network.Haskoin.Wallet.UsageInfo
import qualified System.Console.Argument                 as Argument
import qualified System.Console.Haskeline                as Haskeline
import qualified System.Console.Program                  as Program
import qualified System.Directory                        as D

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
        ["true"]
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
        ["btc", "btc-test", "bch", "bch-test"]
        "btc"
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

verboseOpt :: Option Bool
verboseOpt =
    option
        'v'
        "verbose"
        ["true"]
        False
        Argument.boolean
        "Produce a more detailed output for this command."

parseNetwork :: String -> Network
parseNetwork str =
    case netByName (toLString str) of
        Just net -> net
        _ ->
            printCustomError $
            vcat
                [ formatError
                      "Invalid network name. Select one of the following:"
                , nest 4 $
                  vcat $
                  fmap (formatStatic . fromLString . getNetworkName) allNets
                ]

printNetworkHeader :: Network -> IO ()
printNetworkHeader net =
    renderIO $
    formatNetwork $ "--- " <> fromLString (getNetworkIdent net) <> " ---"

parseUnit :: String -> AmountUnit
parseUnit unit =
    case unit of
        "bitcoin" -> UnitBitcoin
        "bit" -> UnitBit
        "satoshi" -> UnitSatoshi
        _ ->
            printCustomError $
            vcat
                [ formatError "Invalid unit value. Choose one of:"
                , nest 4 $
                  vcat $ fmap formatStatic ["bitcoin", "bit", "satoshi"]
                ]

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
            case right (second fromText) mnemE of
                Right (orig, ms) ->
                    renderIO $
                    vcat
                        [ formatTitle "System Entropy Source"
                        , nest 4 $ formatFilePath orig
                        , formatTitle "Private Mnemonic"
                        , nest 4 $ mnemonicPrinter 4 (words ms)
                        ]
                Left err -> printError err
  where
    askDiceRolls reqEnt =
        askInputLine $
        "Enter your " <> show (requiredRolls reqEnt) <> " dice rolls: "

mnemonicPrinter :: CountOf (Natural, String) -> [String] -> Printer
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
    withOption netOpt $ \netStr ->
        io $ do
            let !net = parseNetwork netStr
            printNetworkHeader net
            xpub <- deriveXPubKey <$> askSigningKey net (fromIntegral deriv)
            let fname = fromString $ "key-" <> toLString (xPubChecksum xpub)
            path <- writeDoc net fname $ xPubExport xpub
            renderIO $
                vcat
                    [ formatTitle "Public Key"
                    , nest 4 $ formatPubKey $ fromText $ xPubExport xpub
                    , formatTitle "Derivation"
                    , nest 4 $
                      formatDeriv $
                      show $ ParsedPrv $ toGeneric $ bip44Deriv net deriv
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
    withOption netOpt $ \netStr -> 
    withNonOption Argument.file "{filename}" $ \fp ->
        io $ do
            let !net = parseNetwork netStr
            printNetworkHeader net
            xpub <- readDoc net (fromString fp) (xPubFromJSON net)
            let store =
                    AccountStore
                        xpub
                        0
                        0
                        (bip44Deriv net $ fromIntegral $ xPubChild xpub)
            name <- newAccountStore net store
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
    withOption netOpt $ \netStr ->
    withNonOption Argument.string "{oldname}" $ \oldName ->
    withNonOption Argument.string "{newname}" $ \newName ->
        io $ do
            let !net = parseNetwork netStr
            printNetworkHeader net
            renameAccountStore
                net
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
    withOption netOpt $ \netStr ->
    withNonOptions Argument.string "{address value ...}" $ \as ->
        io $ do
            let !net = parseNetwork netStr
                !unit = parseUnit u
                !rcps =
                    Map.fromList $
                    fromMaybe rcptErr $ mapM (toRecipient net unit) $
                    groupIn 2 $ fmap fromLString as
            printNetworkHeader net
            withAccountStore net acc $ \(k, store) -> do
                resE <- buildTxSignData net store rcps feeByte dust
                case resE of
                    Right (signDat, store') -> do
                        savePrepareTx net store unit "tx" signDat
                        when (store /= store') $
                            updateAccountStore net k $ const store'
                    Left err  -> printError err
  where
    rcptErr = printError "Could not parse the recipients"
    toRecipient :: Network -> AmountUnit -> [String] -> Maybe (Address, Satoshi)
    toRecipient net unit [a, v] =
        (,) <$> stringToAddr net (toText a) <*> readAmount unit v
    toRecipient _ _ _ = Nothing

savePrepareTx ::
       Network -> AccountStore -> AmountUnit -> LString -> TxSignData -> IO ()
savePrepareTx net store unit str signDat =
    case pubDetailedTx net signDat (accountStoreXPubKey store) of
        Right info -> do
            let chsum = txChksum $ txSignDataTx signDat
                fname =
                    fromString $ str <> "-" <> toLString chsum <> "-unsigned"
            path <- writeDoc net fname signDat
            renderIO $
                vcat
                    [ detailedTxFormat
                          (accountStoreDeriv store)
                          unit
                          (Just False)
                          Nothing
                          info
                    , formatTitle "Unsigned Tx Data File"
                    , nest 4 $ formatFilePath $ filePathToString path
                    ]
        Left err -> printError err

txChksum :: Tx -> String
txChksum = take 16 . fromText . txHashToHex . nosigTxHash

signtx :: Command IO
signtx =
    command
        "signtx"
        "Sign a transaction that was created with preparetx"
        (Just CommandOffline)
        [sendtx] $
    withOption derivOpt $ \d ->
    withOption unitOpt $ \u ->
    withOption netOpt $ \netStr ->
    withNonOption Argument.file "{filename}" $ \fp ->
        io $ do
            let !net = parseNetwork netStr
                !unit = parseUnit u
            printNetworkHeader net
            dat <- readDoc net (fromString fp) Json.parseJSON
            signKey <- askSigningKey net $ fromIntegral d
            case signWalletTx net dat signKey of
                Right res -> saveSignedTx net d unit "tx" res
                Left err  -> printError err

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
    withOption netOpt $ \netStr ->
    withNonOptions Argument.string "{address...}" $ \as ->
        io $ do
            let !net = parseNetwork netStr
                !unit = parseUnit u
                !rcps =
                    fromMaybe
                        rcptErr $
                        mapM (stringToAddr net) (fromString <$> as)
            printNetworkHeader net
            withAccountStore net acc $ \(k, store) -> do
                resE <- buildSwipeTx net store rcps feeByte
                case resE of
                    Right (signDat, store') -> do
                        savePrepareTx net store unit "swipetx" signDat
                        when (store /= store') $
                            updateAccountStore net k $ const store'
                    Left err  -> printError err
  where
    rcptErr = printError "Could not parse addresses"

signswipetx :: Command IO
signswipetx =
    command
        "signswipetx"
        "Sign a transaction that was created with prepareswipetx"
        (Just CommandOffline)
        [sendtx] $
    withOption unitOpt $ \u ->
    withOption netOpt $ \netStr ->
    withNonOption Argument.file "{filename}" $ \fp ->
        io $ do
            let !net = parseNetwork netStr
                !unit = parseUnit u
            printNetworkHeader net
            dat <- readDoc net (fromString fp) Json.parseJSON
            prvKeys <- askInputs "WIF or MiniKey" (decKey net)
            case signSwipeTx net dat prvKeys of
                Right res -> saveSignedTx net 0 unit "swipetx" res
                Left err  -> printError err
  where
    decKey net str =
        fromWif net (toText str) <|> fromMiniKey (stringToBS str)

askInputs :: String -> (String -> Maybe a) -> IO [a]
askInputs msg f = go []
  where
    go acc = do
        inputM <-
            Haskeline.runInputT Haskeline.defaultSettings $
            Haskeline.getPassword
                (Just '*')
                (toLString msg <> " (Enter when done):")
        case inputM of
            Just input
                | not (null input) ->
                    maybe noParse (go . (: acc)) (f $ fromLString input)
            _ ->
                if null acc
                    then noInput
                    else return acc
    noInput = printError "No input provided"
    noParse = printError "Could not parse the input"

saveSignedTx ::
       Network
    -> Natural
    -> AmountUnit
    -> LString
    -> (DetailedTx, Tx, Bool)
    -> IO ()
saveSignedTx net d unit str (info, signedTx, isSigned) = do
    renderIO $ detailedTxFormat (bip44Deriv net d) unit (Just isSigned) Nothing info
    let signedHex = encodeHexText $ encodeBytes signedTx
        chsum = txChksum signedTx
        fname = fromString $ str <> "-" <> toLString chsum <> "-signed"
    path <- writeDoc net fname signedHex
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
    withOption netOpt $ \netStr ->
    withNonOption Argument.file "{filename}" $ \fp ->
        io $ do
            let !net = parseNetwork netStr
            printNetworkHeader net
            tx <- readDoc net (fromString fp) Json.parseJSON
            httpBroadcastTx net tx
            renderIO $
                formatStatic "Tx" <+>
                formatTxHash (fromText $ txHashToHex $ txHash tx) <+>
                formatStatic "has been broadcast"

receive :: Command IO
receive =
    command
        "receive"
        "Generate a new address for receiving a payment"
        Nothing
        [] $
    withOption accOpt $ \acc ->
    withOption netOpt $ \netStr ->
        io $ do
            let !net = parseNetwork netStr
            printNetworkHeader net
            withAccountStore net acc $ \(k, store) -> do
                let (addr, store') = nextExtAddress store
                updateAccountStore net k $ const store'
                renderIO $ addressFormat $ nonEmpty_ [(thd &&& fst) addr]

addresses :: Command IO
addresses =
    command
        "addresses"
        "List the latest receiving addresses in your account"
        Nothing
        [] $
    withOption accOpt $ \acc ->
    withOption cntOpt $ \cnt ->
    withOption netOpt $ \netStr ->
        io $ do
            let !net = parseNetwork netStr
            printNetworkHeader net
            withAccountStore net acc $ \(_, store) -> do
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
                    _ -> printError "No addresses have been generated"

addressFormat :: NonEmpty [(Natural, Address)] -> Printer
addressFormat as = vcat $ getNonEmpty $ nonEmptyFmap toFormat as
  where
    toFormat :: (Natural, Address) -> Printer
    toFormat (i, a) =
        mconcat
            [ formatKey $ block (n + 2) $ show i <> ":"
            , formatAddress $ fromText $ addrToString a
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
    withOption netOpt $ \netStr ->
        io $ do
            let !net = parseNetwork netStr
                !unit = parseUnit u
            printNetworkHeader net
            withAccountStore net acc $ \(_, store) -> do
                let addrs = allExtAddresses store <> allIntAddresses store
                bal <- httpBalance net $ fst <$> addrs
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
    withOption verboseOpt $ \verbose ->
    withOption netOpt $ \netStr ->
        io $ do
            let !net = parseNetwork netStr
                !unit = parseUnit u
            printNetworkHeader net
            withAccountStore net acc $ \(_, store) -> do
                let walletAddrs = allExtAddresses store <> allIntAddresses store
                    walletAddrMap = Map.fromList walletAddrs
                txInfs <- httpDetailedTx net $ fmap fst walletAddrs
                currHeight <- httpBestHeight net
                forM_ (sortOn detailedTxHeight txInfs) $ \txInf -> do
                    let format =
                            if verbose
                                then detailedTxFormat
                                else detailedTxFormatCompact
                        txInfPath = detailedTxFillPath walletAddrMap txInf
                    renderIO $
                        format
                            (accountStoreDeriv store)
                            unit
                            Nothing
                            (Just currHeight)
                            txInfPath

{- Command Line Helpers -}

writeDoc :: Json.ToJSON a => Network -> FileName -> a -> IO FilePath
writeDoc net fileName dat = do
    dir <- fromString <$> D.getUserDocumentsDirectory
    let path = dir </> (fromString network <> "-" <> fileName <> ".json")
        val = encodeJsonPretty $ DocStructure (fromString network) dat
    withFile path WriteMode (`hPut` (val <> stringToBytes "\n"))
    return path
  where
    network = getNetworkName net

readDoc :: Network -> FilePath -> (Json.Value -> Parser a) -> IO a
readDoc net fileName parser = do
    bytes <- readFile fileName
    let m = fromMaybe err $ decodeJson bytes :: Map Text Json.Value
    case m Map.!? toText "network" of
        Just (Json.String fileNet) ->
            if fileNet == fromString (getNetworkName net)
                then case decodeJson bytes of
                         Just (DocStructure _ val) ->
                             maybe err return $ parseMaybe parser val
                         _ -> err
                else badNetErr $ fromText fileNet
        _ -> err
  where
    err = printError $ "Could not read file " <> filePathToString fileName
    badNetErr fileNet =
        printError $
        "Bad network. This file has to be used on the network: " <> fileNet

withAccountStore ::
       Network -> String -> ((String, AccountStore) -> IO ()) -> IO ()
withAccountStore net name f
    | null name = do
        accMap <- readAccountsFile net
        case Map.assocs accMap of
            [val] -> f (first fromText val)
            _ ->
                case Map.lookup "main" accMap of
                    Just val -> f ("main", val)
                    _ -> err $ fromText <$> Map.keys accMap
    | otherwise = do
        accM <- getAccountStore net name
        case accM of
            Just acc -> f (name, acc)
            _ -> err . fmap fromText . Map.keys =<< readAccountsFile net
  where
    err :: [String] -> IO ()
    err [] = printError "No accounts have been created"
    err keys =
        printCustomError $
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
        (printError "No action due to EOF")
        (return . fromLString)
        inputM

askInputLine :: String -> IO String
askInputLine msg = do
    inputM <-
        Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getInputLine (toLString msg)
    maybe
        (printError "No action due to EOF")
        (return . fromLString)
        inputM

askSigningKey :: Network -> Natural -> IO XPrvKey
askSigningKey net acc = do
    str <- askInputLineHidden "Enter your private mnemonic: "
    case mnemonicToSeed "" (toText str) of
        Right _ -> do
            passStr <- askPassword
            either printError return $ signingKey net passStr str acc
        Left err -> printError $ fromLString err

askPassword :: IO String
askPassword = do
    pass <- askInputLineHidden "Mnemonic password or leave empty: "
    unless (null pass) $ do
        pass2 <- askInputLineHidden "Repeat your mnemonic password: "
        when (pass /= pass2) $
            printError "The passwords did not match"
    return pass
