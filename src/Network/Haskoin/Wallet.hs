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

--}

