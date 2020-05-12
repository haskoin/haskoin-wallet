{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Haskoin.Wallet where

import           Control.Monad                       (forM, join, unless, when)
import qualified Data.Aeson                          as Json
import           Data.Aeson.TH
import qualified Data.Aeson.Types                    as Json
import qualified Data.ByteString.Char8               as C8
import           Data.Foldable                       (asum)
import           Data.List                           (isPrefixOf, nub, sort)
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (fromMaybe)
import           Data.String                         (unwords)
import           Data.String.Conversions             (cs)
import           Data.Text                           (Text)
import           Haskoin.Constants
import           Haskoin.Keys
import           Haskoin.Util                        (dropFieldLabel)
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.Commands
import           Network.Haskoin.Wallet.Doc
import           Network.Haskoin.Wallet.Entropy
import           Network.Haskoin.Wallet.Parser
import           Network.Haskoin.Wallet.Signing
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Options.Applicative
import           Options.Applicative.Help.Pretty     hiding ((</>))
import qualified System.Console.Haskeline            as Haskeline
import qualified System.Directory                    as D
import           System.IO                           (IOMode (..), withFile)

clientMain :: IO ()
clientMain = do
    cmd <- customExecParser (prefs showHelpOnEmpty) programParser
    res <- commandResponse cmd
    jsonPrinter res

jsonPrinter :: Response -> IO ()
jsonPrinter = C8.putStrLn . encodeJsonPretty

{--

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

