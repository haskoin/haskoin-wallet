{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.Haskoin.Wallet where

import           Control.Lens                               ((^?))
import           Control.Monad
import qualified Data.Aeson                                 as J
import           Data.Aeson.Lens
import qualified Data.ByteString                            as BS
import qualified Data.ByteString.Lazy                       as BL
import qualified Data.Map.Strict                            as M
import           Data.Maybe
import           Data.Monoid                                ((<>))
import qualified Data.Serialize                             as S
import           Data.String.Conversions                    (cs)
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T (null)
import           Data.Word
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.ConsolePrinter
import           Network.Haskoin.Wallet.Entropy
import           Network.Haskoin.Wallet.HTTP
import           Network.Haskoin.Wallet.HTTP.BlockchainInfo
import           Network.Haskoin.Wallet.HTTP.Insight
import           Network.Haskoin.Wallet.HTTP.Haskoin
import qualified Network.Haskoin.Wallet.PrettyJson          as Pretty
import           Network.Haskoin.Wallet.Signing
import qualified System.Console.Argument                    as Argument
import           System.Console.Command
import qualified System.Console.Haskeline                   as Haskeline
import           System.Console.Program                     (showUsage, single)
import qualified System.Directory                           as D

clientMain :: IO ()
clientMain = single hwCommands

hwCommands :: Commands IO
hwCommands =
    Node hw [ Node mnemonic []
            , Node pubkey []
            , Node watch []
            , Node rename []
            , Node receive []
            , Node history []
            , Node send []
            , Node sign []
            , Node balance []
            , Node broadcast []
            , Node help []
            ]

hw :: Command IO
hw = command "hw" "bitcoin wallet management" $ io $ showUsage hwCommands

mnemonic :: Command IO
mnemonic = command "mnemonic" "Generate a mnemonic" $
    withOption entOpt $ \reqEnt ->
    withOption diceOpt $ \useDice ->io $ do
        rollsM <- if useDice
                    then Just <$> askDiceRolls (fromIntegral reqEnt)
                    else return Nothing
        mnemE <- genMnemonic (fromIntegral reqEnt) rollsM
        case mnemE of
            Right (orig, ms) ->
                renderIO $ vcat
                    [ formatTitle "System Entropy Source"
                    , nest 4 $ formatFilePath orig
                    , formatTitle "Private Mnemonic"
                    , nest 4 $ mnemonicPrinter 4 (words $ cs ms)
                    ]
            Left err -> consoleError $ formatError err
  where
    entOpt = Argument.option ['e'] ["entropy"] Argument.integer 16
             "Entropy in bytes to generate the mnemonic [16,20..32]"
    diceOpt = Argument.option ['d'] ["dice"] Argument.boolean False
              "Provide additional entropy from 6-sided dice rolls"
    askDiceRolls reqEnt = askInputLine $ "Enter your " <>
                                         show (requiredRolls reqEnt) <>
                                         " dice rolls: "

mnemonicPrinter :: Int -> [String] -> ConsolePrinter
mnemonicPrinter n ws =
    vcat $ map (mconcat . map formatWord) $ groupIn n $ zip ([1..] :: [Int]) ws
  where
    formatWord (i, w) = mconcat
        [ formatKey $ block 4 $ show i <> "."
        , formatMnemonic $ block 10 w
        ]

derOpt :: Argument.Option Integer
derOpt = Argument.option ['d'] ["deriv"] Argument.integer 0
         "Bip44 account derivation"

netOpt :: Argument.Option String
netOpt = Argument.option ['n'] ["network"] Argument.string "bitcoin"
          "Set the network (=bitcoin|testnet3|bitcoincash|cashtest)"

unitOpt :: Argument.Option String
unitOpt = Argument.option ['u'] ["unit"] Argument.string "bitcoin"
           "Set the units for amounts (=bitcoin|bit|satoshi)"

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
                , nest 4 $ vcat $ map formatStatic ["bitcoin", "bit", "satoshi"]
                ]

setOptNet :: String -> IO ()
setOptNet name
    | name == getNetworkName bitcoinNetwork = do
        setBitcoinNetwork
        renderIO $ formatCyan "--- Bitcoin ---"
    | name == getNetworkName bitcoinCashNetwork = do
        setBitcoinCashNetwork
        renderIO $ formatGreen "--- Bitcoin Cash ---"
    | name == getNetworkName testnet3Network = do
        setTestnet3Network
        renderIO $ formatWarn "--- Testnet ---"
    | name == getNetworkName cashTestNetwork = do
        setCashTestNetwork
        renderIO $ formatWarn "--- Bitcoin Cash Testnet ---"
    | otherwise =
        consoleError $
        vcat
            [ formatError "Invalid network name. Select one of the following:"
            , nest 4 $
              vcat $
              map
                  formatStatic
                  [ getNetworkName bitcoinNetwork
                  , getNetworkName bitcoinCashNetwork
                  , getNetworkName testnet3Network
                  , getNetworkName cashTestNetwork
                  ]
            ]

serOpt :: Argument.Option String
serOpt = Argument.option ['s'] ["service"] Argument.string ""
          "Blockchain service (=haskoin|blockchain|insight)"

parseBlockchainService :: String -> BlockchainService
parseBlockchainService service =
    case service of
        "" -> defaultBlockchainService
        "haskoin" -> haskoinService
        "blockchain" -> blockchainInfoService
        "insight" -> insightService
        _ ->
            consoleError $
            vcat
                [ formatError
                      "Invalid service name. Select one of the following:"
                , nest 4 $
                  vcat $ map formatStatic ["haskoin", "blockchain", "insight"]
                ]

defaultBlockchainService :: BlockchainService
defaultBlockchainService
    | getNetwork == bitcoinNetwork = blockchainInfoService
    | getNetwork == testnet3Network = haskoinService
    | getNetwork == bitcoinCashNetwork = insightService
    | getNetwork == cashTestNetwork = insightService
    | otherwise = consoleError $ formatError $
        "No blockchain service for network " <> networkName

pubkey :: Command IO
pubkey = command "pubkey" "Derive a public key from a mnemonic" $
    withOption derOpt $ \deriv ->
    withOption netOpt $ \network -> io $ do
        setOptNet network
        xpub <- deriveXPubKey <$> askSigningKey (fromIntegral deriv)
        let fname = "key-" <> cs (xPubChecksum xpub)
        path <- writeDoc fname $ J.String $ cs $ xPubExport xpub
        renderIO $ vcat
            [ formatTitle "Public Key"
            , nest 4 $ formatPubKey $ cs $ xPubExport xpub
            , formatTitle "Derivation"
            , nest 4 $ formatDeriv $ show $
                ParsedPrv $ toGeneric $ bip44Deriv $ fromIntegral deriv
            , formatTitle "Public Key File"
            , nest 4 $ formatFilePath path
            ]

watch :: Command IO
watch = command "watch" "Create a new read-only account from an xpub file" $
    withOption netOpt $ \network ->
    withNonOption Argument.file $ \fp -> io $ do
        setOptNet network
        val <- readDoc fp
        case J.fromJSON val of
            J.Success xpub -> do
                let store = AccountStore xpub 0 0 (bip44Deriv $ xPubChild xpub)
                name <- newAccountStore store
                renderIO $ vcat
                    [ formatTitle "New Account Created"
                    , nest 4 $ vcat
                        [ formatKey (block 13 "Name:") <>
                          formatAccount (cs name)
                        , formatKey (block 13 "Derivation:") <>
                          formatDeriv
                            (show $ ParsedPrv $ toGeneric $ accountStoreDeriv store)
                        ]
                    ]
            _ -> consoleError $ formatError "Could not parse the public key"

rename :: Command IO
rename = command "rename" "Rename an account" $
    withOption netOpt $ \network ->
    withNonOption Argument.string $ \oldName ->
    withNonOption Argument.string $ \newName -> io $ do
        setOptNet network
        renameAccountStore (cs oldName) (cs newName)
        renderIO $
            formatStatic "Account" <+> formatAccount oldName <+>
            formatStatic "renamed to" <+> formatAccount newName

accOpt :: Argument.Option String
accOpt = Argument.option ['a'] ["account"] Argument.string ""
         "Account name"

receive :: Command IO
receive = command "receive" "Generate a new address to receive coins" $
    withOption accOpt $ \acc ->
    withOption netOpt $ \network -> io $ do
        setOptNet network
        withAccountStore (cs acc) $ \(k, store) -> do
            let (addr, store') = nextExtAddress store
            updateAccountStore k $ const store'
            renderIO $ addressFormat [(lst3 addr, fst3 addr)]

history :: Command IO
history = command "history" "Display historical addresses" $
    withOption accOpt $ \acc ->
    withOption cntOpt $ \cnt ->
    withOption netOpt $ \network -> io $ do
        setOptNet network
        withAccountStore (cs acc) $ \(_, store) -> do
            let xpub = accountStoreXPubKey store
                idx  = accountStoreExternal store
            when (idx == 0) $ consoleError $ formatError
                "No addresses have been generated"
            let start = fromIntegral $ max (0 :: Integer)
                                           (fromIntegral idx - cnt)
                count = fromIntegral $ idx - start
                addrs = take count $ derivePathAddrs xpub extDeriv start
            renderIO $ addressFormat $ map (\(a,_,i) -> (i,a)) addrs
  where
    cntOpt = Argument.option ['i'] ["number"] Argument.natural 5
             "Number of addresses to display"

addressFormat :: [(Word32, Address)] -> ConsolePrinter
addressFormat as =
    vcat $ map toFormat as
  where
    toFormat :: (Word32, Address) -> ConsolePrinter
    toFormat (i, a) = mconcat
        [ formatKey $ block (n+2) $ show i <> ":"
        , formatAddress $ cs $ addrToBase58 a
        ]
    n = length $ show $ maximum $ map fst as

send :: Command IO
send = command "send" "Send coins (hw send address amount [address amount..])" $
    withOption accOpt $ \acc ->
    withOption feeOpt $ \feeByte ->
    withOption dustOpt $ \dust ->
    withOption unitOpt $ \u ->
    withOption netOpt $ \network ->
    withOption serOpt $ \s ->
    withNonOptions Argument.string $ \as -> io $ do
        setOptNet network
        withAccountStore (cs acc) $ \(k, store) -> do
            let !unit = parseUnit u
                !rcps = fromMaybe rcptErr $ mapM (toRecipient unit) $ groupIn 2 as
                feeW = fromIntegral feeByte
                dustW = fromIntegral dust
                service = parseBlockchainService s
            resE <- buildTxSignData service store rcps feeW dustW
            let (!signDat, !store') = either (consoleError . formatError) id resE
                infoE = pubSignInfo signDat (accountStoreXPubKey store)
                !info = either (consoleError . formatError) id infoE
            when (store /= store') $ updateAccountStore k $ const store'
            let chsum = txChksum $ txSignDataTx signDat
                fname = "tx-" <> cs chsum <> "-unsigned"
            path <- writeDoc fname $ J.toJSON signDat
            renderIO $ vcat
                [ signingInfoFormat (accountStoreDeriv store) unit info Nothing
                , formatTitle "Unsigned Tx Data File"
                , nest 4 $ formatFilePath path
                ]
  where
    feeOpt = Argument.option ['f'] ["fee"] Argument.natural 200
             "Fee per byte"
    dustOpt = Argument.option ['d'] ["dust"] Argument.natural 5430
             "Do not create change outputs below this value"
    rcptErr = consoleError $ formatError "Could not parse the recipients"

txChksum :: Tx -> BS.ByteString
txChksum = BS.take 8 . txHashToHex . nosigTxHash

groupIn :: Int -> [a] -> [[a]]
groupIn n xs
    | length xs <= n = [xs]
    | otherwise = [take n xs] <> groupIn n (drop n xs)

toRecipient :: AmountUnit -> [String] -> Maybe (Address, Word64)
toRecipient unit [a, v] = (,) <$> base58ToAddr (cs a) <*> readAmount unit v
toRecipient _  _        = Nothing

sign :: Command IO
sign = command "sign" "Sign the output of the \"send\" command" $
    withOption derOpt $ \d ->
    withOption unitOpt $ \u ->
    withOption netOpt $ \network ->
    withNonOption Argument.file $ \fp -> io $ do
        setOptNet network
        val <- readDoc fp
        case J.fromJSON val of
            J.Success dat -> do
                signKey <- askSigningKey $ fromIntegral d
                let resE = signWalletTx dat signKey
                    !unit = parseUnit u
                case resE of
                    Right (info, signedTx) -> do
                        renderIO $
                            signingInfoFormat
                                (bip44Deriv $ fromIntegral d) unit info
                                (Just $ BS.length $ S.encode signedTx)
                        confirmAmount unit $ signingInfoAmount info
                        let signedHex = encodeHex $ S.encode signedTx
                            chsum = cs $ txChksum signedTx
                            fname = "tx-" <> chsum <> "-signed"
                        path <- writeDoc fname $ J.String $ cs signedHex
                        renderIO $ vcat
                            [ formatTitle "Signed Tx File"
                            , nest 4 $ formatFilePath path
                            ]
                    Left err -> consoleError $ formatError err
            _ -> consoleError $ formatError "Could not decode transaction data"
  where
    confirmAmount unit txAmnt = do
        userAmnt <- askInputLine "Type the tx amount to continue signing: "
        when (readIntegerAmount unit userAmnt /= Just txAmnt) $ do
            renderIO $ formatError "Invalid tx amount"
            confirmAmount unit txAmnt

signingInfoFormat :: HardPath
                  -> AmountUnit
                  -> SigningInfo
                  -> Maybe Int
                  -> ConsolePrinter
signingInfoFormat accDeriv unit SigningInfo{..} sizeM =
    vcat [ summary, recips, change, mycoins ]
  where
    summary = vcat
        [ formatTitle "Tx Summary"
        , nest 4 $ vcat
            [ case signingInfoTxHash of
                Just tid ->
                    formatKey (block 12 "Tx hash:") <>
                    formatTxHash (cs $ txHashToHex tid)
                _ -> mempty
            , formatKey (block 12 "Amount:") <>
              formatIntegerAmount unit signingInfoAmount
            , formatKey (block 12 "Fee:") <>
              formatAmount unit signingInfoFee
            , case sizeM of
                Just size ->
                  formatKey (block 12 "Tx size:") <>
                  formatStatic (show size <> " bytes")
                _ -> mempty
            , formatKey (block 12 "Fee/byte:") <>
              formatAmount UnitSatoshi signingInfoFeeByte
            , formatKey (block 12 "Signed:") <>
              if signingInfoIsSigned
                 then formatTrue "Yes"
                 else formatFalse "No"
            ]
        ]
    recips
        | signingInfoNonStd == 0 && null signingInfoRecipients = mempty
        | otherwise = vcat
            [ formatTitle "Tx Recipients"
            , nest 4 $ vcat $
                map addrFormat signingInfoRecipients <>
                [nonStdRcp]
            ]
    nonStdRcp
        | signingInfoNonStd == 0 = mempty
        | otherwise = templ (formatStatic "Non-standard recipients")
                        signingInfoNonStd Nothing
    change
        | null signingInfoChange = mempty
        | otherwise = vcat
            [ formatTitle "Your Tx Change"
            , nest 4 $ vcat $ map addrFormat' signingInfoChange
            ]
    mycoins
        | null signingInfoMyCoins = mempty
        | otherwise = vcat
            [ formatTitle "Your Tx Input Coins"
            , nest 4 $ vcat $ map addrFormat' signingInfoMyCoins
            ]
    addrFormat' (a, (v, p)) =
        templ (formatAddress $ cs $ addrToBase58 a) v (Just p)
    addrFormat (a, v) =
        templ (formatAddress $ cs $ addrToBase58 a) v Nothing
    templ :: ConsolePrinter -> Word64 -> Maybe SoftPath -> ConsolePrinter
    templ f v pM = vcat
        [ f
        , nest 4 $ vcat
            [ formatKey (block 8 "Value:") <> formatAmount unit v
            , case pM of
                Just p -> mconcat
                    [ formatKey $ block 8 "Deriv:"
                    , formatDeriv $ show $ ParsedPrv $
                        toGeneric $ accDeriv ++/ p
                    ]
                _ -> mempty
            ]
        ]

balance :: Command IO
balance = command "balance" "Display the account balance" $
    withOption accOpt $ \acc ->
    withOption unitOpt $ \u ->
    withOption netOpt $ \network ->
    withOption serOpt $ \s -> io $ do
        setOptNet network
        withAccountStore (cs acc) $ \(_, store) -> do
            let addrs = allExtAddresses store <> allIntAddresses store
                !unit = parseUnit u
                service = parseBlockchainService s
            bal <- httpBalance service $ map fst addrs
            renderIO $ vcat
                [ formatTitle "Account Balance"
                , nest 4 $ formatAmount unit bal
                ]

broadcast :: Command IO
broadcast = command "broadcast" "broadcast a tx from a file in hex format" $
    withOption netOpt $ \network ->
    withOption serOpt $ \s ->
    withNonOption Argument.file $ \fp -> io $ do
        setOptNet network
        val <- readDoc fp
        case J.fromJSON val of
            J.Success tx -> do
                let service = parseBlockchainService s
                httpBroadcast service tx
                renderIO $
                    formatStatic "Tx" <+>
                    formatTxHash (cs $ txHashToHex $ txHash tx) <+>
                    formatStatic "has been broadcast"
            _ -> consoleError $ formatError "Could not parse the transaction"

help :: Command IO
help = command "help" "Show usage info" $ io $ showUsage hwCommands

{- Command Line Helpers -}

writeDoc :: FilePath -> J.Value -> IO FilePath
writeDoc fileName dat = do
    dir <- D.getUserDocumentsDirectory
    let path = dir <> "/" <> networkName <> "-" <> fileName <> ".json"
        val = Pretty.encodePretty $ J.object [ "network" J..= networkName
                                             , "payload" J..= dat
                                             ]
    BL.writeFile path $ val <> "\n"
    return path

readDoc :: FilePath -> IO J.Value
readDoc fileName = do
    bs <- BL.readFile fileName
    case J.decode bs :: Maybe J.Value of
        Just v -> do
            let !net = fromMaybe noName $ v ^? key "network" . _String
                !payload = fromMaybe noPayload $ v ^? key "payload" . _Value
            if net == cs networkName
                then return payload
                else badNet $ cs net
        _ -> consoleError $ formatError $ "Could not read file " <> fileName
  where
    noName = consoleError $ formatError "The file did not contain a network"
    noPayload = consoleError $ formatError "The file did not contain a payload"
    badNet net =
        consoleError $ formatError $
        "Bad network. This file has to be used in the network " <>
        net

withAccountStore :: Text -> ((Text, AccountStore) -> IO ()) -> IO ()
withAccountStore name f
    | T.null name = do
        accMap <- readAccountsFile
        case M.assocs accMap of
            [val] -> f val
            _ -> case M.lookup "main" accMap of
                    Just val -> f ("main", val)
                    _        -> err $ M.keys accMap
    | otherwise = do
        accM <- getAccountStore name
        case accM of
            Just acc -> f (name, acc)
            _        -> err . M.keys =<< readAccountsFile
  where
    err :: [Text] -> IO ()
    err [] = consoleError $ formatError "No accounts have been created"
    err keys = consoleError $ vcat
        [ formatError
            "Select one of the following accounts with -a or --account"
        , nest 4 $ vcat $ map (formatAccount . cs) keys
        ]

askInputLineHidden :: String -> IO String
askInputLineHidden msg = do
    inputM <- Haskeline.runInputT
              Haskeline.defaultSettings $
              Haskeline.getPassword (Just '*') msg
    maybe (consoleError $ formatError "No action due to EOF") return inputM

askInputLine :: String -> IO String
askInputLine msg = do
    inputM <- Haskeline.runInputT
              Haskeline.defaultSettings $
              Haskeline.getInputLine msg
    maybe (consoleError $ formatError "No action due to EOF") return inputM

askSigningKey :: KeyIndex -> IO XPrvKey
askSigningKey acc = do
    str <- askInputLineHidden "Enter your private mnemonic: "
    case mnemonicToSeed "" (cs str) of
        Right _ -> do
            passStr <- askPassword
            either (consoleError . formatError) return $
                signingKey (cs passStr) (cs str) acc
        Left err -> consoleError $ formatError err

askPassword :: IO String
askPassword = do
    pass <- askInputLineHidden "Mnemonic password or leave empty: "
    unless (null pass) $ do
        pass2 <- askInputLineHidden "Repeat your mnemonic password: "
        when (pass /= pass2) $ consoleError $ formatError
            "The passwords did not match"
    return pass
