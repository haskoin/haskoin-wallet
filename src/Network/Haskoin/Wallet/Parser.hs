{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.Parser where

import           Control.Monad                       (forM, join, unless, when)
import           Control.Monad.Except
import           Data.Aeson.TH
import           Data.Either                         (either, rights)
import           Data.Foldable                       (asum)
import           Data.List                           (isPrefixOf, nub, sort)
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.String                         (unwords)
import           Data.String.Conversions             (cs)
import           Data.Text                           (Text)
import           Haskoin.Address
import           Haskoin.Constants
import           Haskoin.Util
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Options.Applicative
import           Options.Applicative.Help.Pretty     hiding ((</>))

{- Command Parsers -}

data Command
    = CommandMnemonic
          { commandUseDice :: !Bool
          , commandEntropy :: !Natural
          }
    | CommandCreateAcc
          { commandNetwork    :: !Network
          , commandDerivation :: !Natural
          }
    | CommandImportAcc
          { commandFilePath :: !FilePath
          , commandAccount  :: !Text
          }
    | CommandRenameAcc
          { commandOldName :: !Text
          , commandNewName :: !Text
          }
    | CommandAccounts
    | CommandResetAcc
          { commandMaybeAcc :: !(Maybe Text)
          }
    | CommandBalance
          { commandMaybeAcc :: !(Maybe Text)
          }
    | CommandAddresses
          { commandMaybeAcc :: !(Maybe Text)
          , commandPage     :: !Page
          }
    | CommandReceive
          { commandMaybeAcc :: !(Maybe Text)
          }
    | CommandTransactions
          { commandMaybeAcc :: !(Maybe Text)
          , commandPage     :: !Page
          }
    | CommandPrepareTx
          { commandRecipients :: ![(Text, Text)]
          , commandMaybeAcc   :: !(Maybe Text)
          , commandUnit       :: !AmountUnit
          , commandFeeByte    :: !Natural
          , commandDust       :: !Natural
          , commandRcptPay    :: !Bool
          }
    | CommandReview
          { commandFilePath :: !FilePath
          }
    | CommandSignTx
          { commandFilePath :: !FilePath
          }
    | CommandSendTx
          { commandFilePath :: !FilePath
          }
    | CommandPrepareSweep
          { commandSweepAddrs    :: ![Text]
          , commandMaybeFilePath :: !(Maybe FilePath)
          , commandMaybeAcc      :: !(Maybe Text)
          , commandFeeByte       :: !Natural
          , commandDust          :: !Natural
          }
    | CommandSignSweep
          { commandSweepPath  :: !FilePath
          , commandSecKeyPath :: !FilePath
          , commandMaybeAcc   :: !(Maybe Text)
          }
    deriving (Eq, Show)

programParser :: ParserInfo Command
programParser =
    info (commandParser <**> helper) $
    mconcat
        [ fullDesc
        , progDesc "Lightweight Bitcoin and Bitcoin Cash Wallet"
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
            , command "balance" balanceParser
            ]
        , hsubparser $
            mconcat
            [ commandGroup "Address management"
            , command "addresses" addressesParser
            , command "receive" receiveParser
            ]
        , hsubparser $
            mconcat
            [ commandGroup "Transaction management"
            , command "transactions" transactionsParser
            , command "preparetx" prepareTxParser
            , command "review" reviewParser
            , command "signtx" signTxParser
            , command "sendtx" sendTxParser
            ]
        , hsubparser $
            mconcat
            [ commandGroup "Utilities"
            , command "preparesweep" prepareSweepParser
            , command "signsweep" signSweepParser
            , command "resetacc" resetAccParser
            ]
        ]

mnemonicParser :: ParserInfo Command
mnemonicParser =
    info (CommandMnemonic <$> diceOption <*> entropyOption) $
    mconcat
        [ progDesc "Generate a mnemonic using your systems entropy"
        , footer "Next commands: createacc"
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
        (CommandImportAcc <$> fileArgument "Path of the account file"
                          <*> textArg "Name of the new account") $
    mconcat
        [ progDesc "Import an account file into the wallet"
        , footer "Next command: receive"
        ]

renameAccParser :: ParserInfo Command
renameAccParser =
    info
        (CommandRenameAcc <$> accountArg "Old account name"
                          <*> textArg "New account name") $
    mconcat [progDesc "Rename an account"]

accountsParser :: ParserInfo Command
accountsParser =
    info (pure CommandAccounts) $ mconcat [progDesc "Return all accounts"]

balanceParser :: ParserInfo Command
balanceParser =
    info (CommandBalance <$> accountOption) $
    mconcat [progDesc "Get the account balance"]

resetAccParser :: ParserInfo Command
resetAccParser =
    info (CommandResetAcc <$> accountOption) $
    mconcat [progDesc "Reset the external and internal derivation indices"]

addressesParser :: ParserInfo Command
addressesParser =
    info
        (CommandAddresses <$> accountOption
                          <*> (Page <$> limitOption <*> offsetOption)) $
    mconcat [progDesc "List the latest receiving addresses in the account"]

receiveParser :: ParserInfo Command
receiveParser =
    info (CommandReceive <$> accountOption) $
    mconcat [progDesc "Get a new address for receiving a payment"]

transactionsParser :: ParserInfo Command
transactionsParser =
    info
        (CommandTransactions <$> accountOption
                             <*> (Page <$> limitOption <*> offsetOption)) $
    mconcat [progDesc "Display the transactions in an account"]

prepareTxParser :: ParserInfo Command
prepareTxParser =
    info
        (CommandPrepareTx
            <$> some recipientArg
            <*> accountOption
            <*> unitOption
            <*> feeOption
            <*> dustOption
            <*> rcptPayOption) $
    mconcat
        [ progDesc "Prepare a new unsigned transaction"
        , footer "Next command: review, signtx"
        ]

reviewParser :: ParserInfo Command
reviewParser =
    info (CommandReview <$> fileArgument "Path of the transaction file") $
    mconcat
        [ progDesc "Review the contents of a transaction file"
        , footer "Next command: signtx, sendtx"
        ]

signTxParser :: ParserInfo Command
signTxParser =
    info (CommandSignTx <$> fileArgument "Path of the transaction file") $
    mconcat
        [ progDesc "Sign a transaction that was created with preparetx"
        , footer "Next command: sendtx"
        ]

sendTxParser :: ParserInfo Command
sendTxParser =
    info (CommandSendTx <$> fileArgument "Path of the transaction file") $
    mconcat [progDesc "Broadcast a signed transaction to the network"]

prepareSweepParser :: ParserInfo Command
prepareSweepParser =
    info
        (CommandPrepareSweep
            <$> many (addressArg "List of addresses to sweep")
            <*> maybeFileOption "File containing addresses to sweep"
            <*> accountOption
            <*> feeOption
            <*> dustOption) $
    mconcat
        [ progDesc "Sweep funds into this wallet"
        , footer "Next command: signsweep"
        ]

signSweepParser :: ParserInfo Command
signSweepParser =
    info
        (CommandSignSweep
            <$> dirArgument "Folder containing the sweep transactions"
            <*> fileArgument "Path to the file containing the private keys"
            <*> accountOption) $
    mconcat
        [ progDesc "Sign all the transactions contained in a sweep folder"
        , footer "Next command: sendtx"
        ]

{- Option Parsers -}

textArg :: String -> Parser Text
textArg desc = argument str $ mconcat [help desc, metavar "TEXT"]

fileArgument :: String -> Parser FilePath
fileArgument desc =
    strArgument $
    mconcat
        [ help desc
        , metavar "FILENAME"
        , action "file"
        ]

dirArgument :: String -> Parser FilePath
dirArgument desc =
    strArgument $
    mconcat
        [ help desc
        , metavar "DIRNAME"
        , action "file"
        ]

maybeFileOption :: String -> Parser (Maybe FilePath)
maybeFileOption desc =
    optional $
    strOption $
    mconcat
        [ long "file"
        , help desc
        , metavar "FILENAME"
        , action "file"
        ]

diceOption :: Parser Bool
diceOption =
    switch $
    mconcat
        [ short 'd'
        , long "dice"
        , help "Provide additional entropy using 6-sided dice"
        , showDefault
        ]

entropyOption :: Parser Natural
entropyOption =
    option (maybeReader f) $
    mconcat
        [ short 'e'
        , long "entropy"
        , help
              "Amount of entropy to use in bytes. Valid values are [16,20..32]"
        , metavar "BYTES"
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
        , help "Specify a different bip44 account derivation"
        , metavar "NATURAL"
        , value 0
        , showDefault
        ]

networkOption :: Parser Network
networkOption =
    option (eitherReader (f . netByName)) $
    mconcat
        [ short 'n'
        , long "network"
        , help "Specify which coin network to use"
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

accountOption :: Parser (Maybe Text)
accountOption =
    optional $
    strOption $
    mconcat
        [ short 'a'
        , long "account"
        , help "Specify the account to use for this command"
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
    keys <- either (const []) id <$> run
    return $ sort $ nub $ filter (pref `isPrefixOf`) (cs <$> keys)
  where
    run = runExceptT $ withAccountMap accountMapKeys

recipientArg :: Parser (Text, Text)
recipientArg =
    (,) <$> addressArg "Recipient address" <*> amountArg

amountArg :: Parser Text
amountArg =
    strArgument $
    mconcat
        [ help "Recipient amount"
        , metavar "AMOUNT"
        ]

addressArg :: String -> Parser Text
addressArg desc =
    strArgument $
    mconcat
        [ help desc
        , metavar "ADDRESS"
        ]

rcptPayOption :: Parser Bool
rcptPayOption =
    switch $
    mconcat
        [ short 'r'
        , long "recipientpay"
        , help "The transaction fee will be deducted from the recipient amounts"
        , showDefault
        ]

offsetOption :: Parser Natural
offsetOption =
    option (maybeReader $ readNatural . cs) $
    mconcat
        [ short 'o'
        , long "offset"
        , help "Offset the result set"
        , metavar "INT"
        , value 0
        , showDefault
        ]

limitOption :: Parser Natural
limitOption =
    option (maybeReader $ readNatural . cs) $
    mconcat
        [ short 'l'
        , long "limit"
        , help "Limit the result set"
        , metavar "INT"
        , value 5
        , showDefault
        ]

feeOption :: Parser Natural
feeOption =
    option (maybeReader $ readNatural . cs) $
    mconcat
        [ short 'f'
        , long "fee"
        , help "Fee to pay in satoshi/bytes"
        , metavar "INT"
        , value 200
        , showDefault
        ]

dustOption :: Parser Natural
dustOption =
    option (maybeReader $ readNatural . cs) $
    mconcat
        [ short 'd'
        , long "dust"
        , help "Smallest allowed satoshi value for change outputs"
        , metavar "INT"
        , value 5430
        , showDefault
        ]

unitOption :: Parser AmountUnit
unitOption = satoshiOption <|> bitOption

satoshiOption :: Parser AmountUnit
satoshiOption =
    flag UnitBitcoin UnitSatoshi $
    mconcat
        [ short 's'
        , long "satoshi"
        , help "Use satoshis for parsing amounts (default: bitcoin)"
        ]

bitOption :: Parser AmountUnit
bitOption =
    flag UnitBitcoin UnitBit $
    mconcat
        [ short 'b'
        , long "bit"
        , help "Use bits for parsing amounts (default: bitcoin)"
        ]

