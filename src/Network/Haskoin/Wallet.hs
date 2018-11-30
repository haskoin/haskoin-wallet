{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Haskoin.Wallet where

import           Control.Arrow                           (right, (&&&))
import           Control.Monad                           (join, unless, when)
import qualified Data.Aeson                              as Json
import           Data.Aeson.TH
import qualified Data.Aeson.Types                        as Json
import           Data.Foldable                           (asum)
import           Data.List                               (nub, sort, sortOn)
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import           Data.String                             (unwords)
import           Data.String.Conversions                 (cs)
import           Data.Text                               (Text)
import           Data.Tree                               (Tree (Node))
import           Foundation
import           Foundation.Collection
import           Foundation.Compat.Text
import           Foundation.IO
import           Foundation.String
import           Foundation.String.Read
import           Foundation.VFS
import           Network.Haskoin.Address
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util                    (dropFieldLabel)
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.DetailedTx
import           Network.Haskoin.Wallet.Doc
import           Network.Haskoin.Wallet.Entropy
import           Network.Haskoin.Wallet.FoundationCompat
import           Network.Haskoin.Wallet.HTTP
import           Network.Haskoin.Wallet.Signing
import           Options.Applicative
import           Options.Applicative.Help.Pretty         hiding ((</>))
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

diceOption :: Parser Bool
diceOption =
    switch $
    mconcat
        [ short 'd'
        , long "dice"
        , help "Provide additional entropy using 6-sided dice."
        , showDefault
        ]

entropyOption :: Parser (CountOf Word8)
entropyOption =
    option (maybeReader f) $
    mconcat
        [ short 'e'
        , long "entropy"
        , help
              "Amount of entropy to use in bytes. Valid values are [16,20..32]."
        , metavar "INT"
        , value 16
        , showDefaultWith (toLString . show . fromCount)
        , completeWith valid
        ]
  where
    valid = ["16", "20", "24", "28", "32"]
    f s
        | s `elem` valid = fromIntegral <$> readNatural (fromLString s)
        | otherwise = Nothing

derivationOption :: Parser Natural
derivationOption =
    option (maybeReader $ readNatural . fromLString) $
    mconcat
        [ short 'd'
        , long "derivation"
        , help "Specify a different bip44 account derivation."
        , metavar "INT"
        , value 0
        , showDefault
        ]

networkOption :: Parser Network
networkOption =
    option (eitherReader (f . netByName)) $
    mconcat
        [ short 'n'
        , long "network"
        , help "Specify which coin network to use."
        , metavar "TEXT"
        , value btc
        , showDefault
        , completeWith (getNetworkName <$> allNets)
        ]
  where
    f :: Maybe Network -> Either LString Network
    f Nothing =
        Left $
        unwords $
        "Invalid network name. Select one of the following:" :
        (getNetworkName <$> allNets)
    f (Just res) = Right res

accountOption :: Parser String
accountOption =
    option str $
    mconcat
        [ short 'a'
        , long "account"
        , help "Specify a different account to use for this command."
        , metavar "TEXT"
        , value "main"
        , showDefault
        , completer (mkCompleter accountCompleter)
        ]
  where
    accountCompleter :: LString -> IO [LString]
    accountCompleter pref = do
        keys <- mconcat <$> forM allNets ((Map.keys <$>) . readAccountsFile)
        return $ sort $ nub $ filter (pref `isPrefixOf`) (cs <$> keys)

filepathArgument :: Parser FilePath
filepathArgument =
    argument str $
    mconcat
        [ help "Specify a filename"
        , metavar "FILENAME"
        , action "file"
        ]

{--

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

parseUnit :: String -> AmountUnit
parseUnit unit =
    case unit of
        "bitcoin" -> UnitBitcoin
        "bit" -> UnitBit
        "satoshi" -> UnitSatoshi
        _ ->
            exitCustomError $
            vcat
                [ formatError "Invalid unit value. Choose one of:"
                , indent 4 $
                  vcat $ fmap formatStatic ["bitcoin", "bit", "satoshi"]
                ]

--}

networkHeaderDoc :: Network -> Doc
networkHeaderDoc net =
    "---" <+> networkDoc (text $ getNetworkName net) <+> "---"

{- Commands -}

clientMain :: IO ()
clientMain = join $ customExecParser p opts
  where
    opts =
        info (commandParser <**> helper) $
        mconcat
            [ fullDesc
            , progDesc "Lightweight Bitcoin and Bitcoin Cash Wallet."
            ]
    p = prefs showHelpOnEmpty

commandParser :: Parser (IO ())
commandParser =
    asum
        [ hsubparser $
            mconcat
            [ commandGroup "Mnemonic and account management"
            , mnemonicDef
            , createaccDef
            , importaccDef
            ]
        , hsubparser $
            mconcat
            [ commandGroup "Address management"
            , hidden
            ]
        ]

mnemonicDef :: Mod CommandFields (IO ())
mnemonicDef = command "mnemonic" $
    info (mnemonic <$> diceOption <*> entropyOption) $
    mconcat
        [ progDesc "Generate a mnemonic using your systems entropy"
        , footer "Next commands: createacc, signtx"
        ]

mnemonic :: Bool -> CountOf Word8 -> IO ()
mnemonic useDice ent = do
    mnemE <-
        if useDice
            then genMnemonicDice ent =<< askDiceRolls
            else genMnemonic ent
    case right (second fromText) mnemE of
        Right (orig, ms) ->
            printDoc $
            vcat
                [ titleDoc "System Entropy Source"
                , indent 4 $ filePathDoc (doc orig)
                , titleDoc "Private Mnemonic"
                , indent 4 $ mnemonicPrinter 4 (words ms)
                ]
        Left err -> exitError err
  where
    askDiceRolls =
        askInputLine $
        "Enter your " <> show (requiredRolls ent) <> " dice rolls: "

mnemonicPrinter :: CountOf (Natural, String) -> [String] -> Doc
mnemonicPrinter n ws =
    vcat $
    fmap (mconcat . fmap wordDoc) $ groupIn n $ zip ([1 ..] :: [Natural]) ws
  where
    wordDoc (i, w) =
        mconcat
            [keyDoc 4 $ doc (show i) <> ".", mnemonicDoc $ fill 10 (doc w)]

createaccDef :: Mod CommandFields (IO ())
createaccDef =
    command "createacc" $
    info (createacc <$> networkOption <*> derivationOption) $
    mconcat
        [ progDesc "Create a new account from a mnemonic"
        , footer "Next command: importacc"
        ]

createacc :: Network -> Natural -> IO ()
createacc net deriv = do
    printDoc $ networkHeaderDoc net
    xpub <- deriveXPubKey <$> askSigningKey net (fromIntegral deriv)
    let fname = fromString $ "key-" <> toLString (xPubChecksum xpub)
    path <- writeDoc net fname $ xPubExport xpub
    printDoc $
        vcat
            [ titleDoc "Public Key"
            , indent 4 $ pubKeyDoc $ textDoc $ xPubExport xpub
            , titleDoc "Derivation"
            , indent 4 $
              derivationDoc $
              doc $ show $ ParsedPrv $ toGeneric $ bip44Deriv net deriv
            , titleDoc "Public Key File"
            , indent 4 $ filePathDoc $ doc $ filePathToString path
            ]

importaccDef :: Mod CommandFields (IO ())
importaccDef =
    command "importacc" $
    info (importacc <$> networkOption <*> filepathArgument) $
    mconcat
        [ progDesc "Import an account file into the wallet"
        , footer "Next command: receive"
        ]

importacc :: Network -> FilePath -> IO ()
importacc net fp = do
    printDoc $ networkHeaderDoc net
    xpub <- readDoc net fp (xPubFromJSON net)
    let store =
            AccountStore
                xpub
                0
                0
                (bip44Deriv net $ fromIntegral $ xPubChild xpub)
    name <- newAccountStore net store
    printDoc $
        vcat
            [ titleDoc "New Account Created"
            , indent 4 $
              vcat
                  [ (keyDoc 13 "Name:") <> accountDoc (doc name)
                  , (keyDoc 13 "Derivation:") <>
                    derivationDoc
                        (doc $
                         show $ ParsedPrv $ toGeneric $ accountStoreDeriv store)
                  ]
            ]


{--

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
                    Left err  -> exitError err
  where
    rcptErr = exitError "Could not parse the recipients"
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
                    , indent 4 $ formatFilePath $ filePathToString path
                    ]
        Left err -> exitError err

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
                Left err  -> exitError err

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
                    Left err  -> exitError err
  where
    rcptErr = exitError "Could not parse addresses"

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
                Left err  -> exitError err
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
    noInput = exitError "No input provided"
    noParse = exitError "Could not parse the input"

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
            , indent 4 $ formatFilePath $ filePathToString path
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
                    _ -> exitError "No addresses have been generated"

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
                        , indent 4 $ formatAmount unit bal
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

--}

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

readDoc :: Network -> FilePath -> (Json.Value -> Json.Parser a) -> IO a
readDoc net fileName parser = do
    bytes <- readFile fileName
    let m = fromMaybe err $ decodeJson bytes :: Map Text Json.Value
    case m Map.!? toText "network" of
        Just (Json.String fileNet) ->
            if fileNet == fromString (getNetworkName net)
                then case decodeJson bytes of
                         Just (DocStructure _ val) ->
                             maybe err return $ Json.parseMaybe parser val
                         _ -> err
                else badNetErr $ fromText fileNet
        _ -> err
  where
    err = exitError $ "Could not read file " <> filePathToString fileName
    badNetErr fileNet =
        exitError $
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
                    _        -> err $ fromText <$> Map.keys accMap
    | otherwise = do
        accM <- getAccountStore net name
        case accM of
            Just acc -> f (name, acc)
            _ -> err . fmap fromText . Map.keys =<< readAccountsFile net
  where
    err :: [String] -> IO ()
    err [] = exitError "No accounts have been created"
    err keys =
        exitCustomError $
        vcat
            [ errorDoc
                  "Select one of the following accounts with -a or --account"
            , indent 4 $ vcat $ fmap (accountDoc . doc) keys
            ]

askInputLineHidden :: String -> IO String
askInputLineHidden msg = do
    inputM <-
        Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getPassword (Just '*') (toLString msg)
    maybe
        (exitError "No action due to EOF")
        (return . fromLString)
        inputM

askInputLine :: String -> IO String
askInputLine msg = do
    inputM <-
        Haskeline.runInputT Haskeline.defaultSettings $
        Haskeline.getInputLine (toLString msg)
    maybe
        (exitError "No action due to EOF")
        (return . fromLString)
        inputM

askSigningKey :: Network -> Natural -> IO XPrvKey
askSigningKey net acc = do
    str <- askInputLineHidden "Enter your private mnemonic: "
    case mnemonicToSeed "" (toText str) of
        Right _ -> do
            passStr <- askPassword
            either exitError return $ signingKey net passStr str acc
        Left err -> exitError $ fromLString err

askPassword :: IO String
askPassword = do
    pass <- askInputLineHidden "Mnemonic password or leave empty: "
    unless (null pass) $ do
        pass2 <- askInputLineHidden "Repeat your mnemonic password: "
        when (pass /= pass2) $
            exitError "The passwords did not match"
    return pass
