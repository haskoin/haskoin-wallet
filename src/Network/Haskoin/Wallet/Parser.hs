{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.Haskoin.Wallet.Parser where

import           Control.Monad                       (forM, join, unless, when)
import           Control.Monad.Except
import           Data.Aeson.TH
import           Data.Either                         (rights, either)
import           Data.Foldable                       (asum)
import           Data.List                           (isPrefixOf, nub, sort)
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.String                         (unwords)
import           Data.String.Conversions             (cs)
import           Data.Text                           (Text)
import           Network.Haskoin.Constants
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Options.Applicative
import           Options.Applicative.Help.Pretty     hiding ((</>))

{- Command Parsers -}

data Command
    = CommandMnemonic Bool Natural
    | CommandCreateAcc Network Natural
    | CommandImportAcc FilePath Text
    | CommandRenameAcc Text Text
    | CommandAccounts

programParser :: ParserInfo Command
programParser =
    info (commandParser <**> helper) $
    mconcat
        [ fullDesc
        , progDesc "Lightweight Bitcoin and Bitcoin Cash Wallet."
        ]

commandParser :: Parser Command
commandParser =
    asum
        [ hsubparser $
            mconcat
            [ commandGroup "Mnemonic and account management"
            , command "mnemonic" mnemonicParser
            , command "createacc" createAccParser
            , command "importacc" importAccParser
            , command "renameacc" renameAccParser
            , command "accounts" accountsParser
            ]
        , hsubparser $
            mconcat
            [ commandGroup "Address management"
            , hidden
            ]
        ]

mnemonicParser :: ParserInfo Command
mnemonicParser =
    info (CommandMnemonic <$> diceOption <*> entropyOption) $
    mconcat
        [ progDesc "Generate a mnemonic using your systems entropy"
        , footer "Next commands: createacc, signtx"
        ]

createAccParser :: ParserInfo Command
createAccParser =
    info (CommandCreateAcc <$> networkOption <*> derivationOption) $
    mconcat
        [ progDesc "Create a new account from a mnemonic"
        , footer "Next command: importacc"
        ]

importAccParser :: ParserInfo Command
importAccParser =
    info
        (CommandImportAcc <$> filepathArgument <*>
         textArg "Name of the new account") $
    mconcat
        [ progDesc "Import an account file into the wallet"
        , footer "Next command: receive"
        ]

renameAccParser :: ParserInfo Command
renameAccParser =
    info
        (CommandRenameAcc <$> accountArg "Old account name" <*>
         textArg "New account name") $
    mconcat [progDesc "Rename an account"]

accountsParser :: ParserInfo Command
accountsParser =
    info (pure CommandAccounts) $ mconcat [progDesc "Return all accounts"]

{- Option Parsers -}

textArg :: String -> Parser Text
textArg desc = argument str $ mconcat [help desc, metavar "TEXT"]

filepathArgument :: Parser FilePath
filepathArgument =
    argument str $
    mconcat
        [ help "Specify a filename"
        , metavar "FILENAME"
        , action "file"
        ]

diceOption :: Parser Bool
diceOption =
    switch $
    mconcat
        [ short 'd'
        , long "dice"
        , help "Provide additional entropy using 6-sided dice."
        , showDefault
        ]

entropyOption :: Parser Natural
entropyOption =
    option (maybeReader f) $
    mconcat
        [ short 'e'
        , long "entropy"
        , help
              "Amount of entropy to use in bytes. Valid values are [16,20..32]."
        , metavar "INT"
        , value 16
        , showDefault
        , completeWith valid
        ]
  where
    valid = ["16", "20", "24", "28", "32"]
    f s
        | s `elem` valid = fromIntegral <$> readNatural (cs s)
        | otherwise = Nothing

derivationOption :: Parser Natural
derivationOption =
    option (maybeReader $ readNatural . cs) $
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
    f :: Maybe Network -> Either String Network
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
        , help "Specify the account to use for this command."
        , metavar "TEXT"
        , completer (mkCompleter accountCompleter)
        ]

accountArg :: String -> Parser Text
accountArg desc =
    argument str $
    mconcat
        [ help desc
        , metavar "TEXT"
        , completer (mkCompleter accountCompleter)
        ]

accountCompleter :: String -> IO [String]
accountCompleter pref = do
    keys <- either (const []) id <$> runExceptT accountMapKeys
    return $ sort $ nub $ filter (pref `isPrefixOf`) (cs <$> keys)

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
