{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Haskoin.Wallet.Parser where

import Control.Monad (forM, join, unless, when)
import Control.Monad.Except
import Data.Aeson.TH
import Data.Either (either, fromRight, rights)
import Data.Foldable (asum)
import Data.List (isPrefixOf, nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (unwords)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin.Address
import Haskoin.Crypto
import Haskoin.Network
import Haskoin.Util
import Network.Haskoin.Wallet.AccountStore
import Network.Haskoin.Wallet.Amounts
import Network.Haskoin.Wallet.Util
import Numeric.Natural
import Options.Applicative
import Options.Applicative.Help.Pretty hiding ((</>))

{- Command Parsers -}

data Command
  = CommandMnemonic
      { commandEntropy :: !Natural,
        commandUseDice :: !Bool,
        commandSplitIn :: !Natural
      }
  | CommandCreateAcc
      { commandName :: !Text,
        commandNetwork :: !Network,
        commandDerivation :: !(Maybe Natural),
        commandSplitIn :: !Natural
      }
  | CommandImportAcc
      { commandFilePath :: !FilePath
      }
  | CommandRenameAcc
      { commandOldName :: !Text,
        commandNewName :: !Text
      }
  | CommandAccounts
  | CommandResetAcc
      { commandMaybeAcc :: !(Maybe Text)
      }
  | CommandBalance
      { commandMaybeAcc :: !(Maybe Text)
      }
  | CommandAddresses
      { commandMaybeAcc :: !(Maybe Text),
        commandPage :: !Page
      }
  | CommandReceive
      { commandMaybeAcc :: !(Maybe Text)
      }
  | CommandTransactions
      { commandMaybeAcc :: !(Maybe Text),
        commandPage :: !Page
      }
  | CommandPrepareTx
      { commandRecipients :: ![(Text, Text)],
        commandMaybeAcc :: !(Maybe Text),
        commandUnit :: !AmountUnit,
        commandFeeByte :: !Natural,
        commandDust :: !Natural,
        commandRcptPay :: !Bool
      }
  | CommandReview
      { commandFilePath :: !FilePath
      }
  | CommandSignTx
      { commandFilePath :: !FilePath,
        commandSplitIn :: !Natural
      }
  | CommandSendTx
      { commandFilePath :: !FilePath
      }
  | CommandPrepareSweep
      { commandSweepAddrs :: ![Text],
        commandMaybeFilePath :: !(Maybe FilePath),
        commandMaybeAcc :: !(Maybe Text),
        commandFeeByte :: !Natural,
        commandDust :: !Natural
      }
  | CommandSignSweep
      { commandSweepPath :: !FilePath,
        commandSecKeyPath :: !FilePath,
        commandMaybeAcc :: !(Maybe Text)
      }
  | CommandRollDice
      { commandCount :: !Natural
      }
  deriving (Eq, Show)

programParser :: Ctx -> ParserInfo Command
programParser ctx =
  info (commandParser ctx <**> helper) $
    mconcat
      [ fullDesc,
        progDesc "Lightweight Bitcoin and Bitcoin Cash BIP-44 Wallet"
      ]

commandParser :: Ctx -> Parser Command
commandParser ctx =
  asum
    [ hsubparser $
        mconcat
          [ commandGroup "Mnemonic and account management",
            command "mnemonic" mnemonicParser,
            command "createacc" createAccParser,
            command "importacc" importAccParser,
            command "renameacc" (renameAccParser ctx),
            command "accounts" accountsParser,
            command "balance" (balanceParser ctx)
          ],
      hsubparser $
        mconcat
          [ commandGroup "Address management",
            command "addresses" (addressesParser ctx),
            command "receive" (receiveParser ctx)
          ],
      hsubparser $
        mconcat
          [ commandGroup "Transaction management",
            command "transactions" (transactionsParser ctx),
            command "preparetx" (prepareTxParser ctx),
            command "review" reviewParser,
            command "signtx" signTxParser,
            command "sendtx" sendTxParser
          ],
      hsubparser $
        mconcat
          [ commandGroup "Utilities",
            command "preparesweep" (prepareSweepParser ctx),
            command "signsweep" (signSweepParser ctx),
            command "resetacc" (resetAccParser ctx),
            command "rolldice" rollDiceParser
          ]
    ]

mnemonicParser :: ParserInfo Command
mnemonicParser =
  info (CommandMnemonic <$> entropyOption <*> diceOption <*> splitInOption) $
    mconcat
      [ progDesc "Generate a mnemonic using your systems entropy",
        footer "Next commands: createacc"
      ]

createAccParser :: ParserInfo Command
createAccParser =
  info
    ( CommandCreateAcc
        <$> textArg "Name of the new account"
        <*> networkOption
        <*> derivationOption
        <*> splitInOption
    )
    $ mconcat
      [ progDesc "Create a new account from a mnemonic",
        footer "Next command: importacc"
      ]

importAccParser :: ParserInfo Command
importAccParser =
  info
    (CommandImportAcc <$> fileArgument "Path of the account file")
    $ mconcat
      [ progDesc "Import an account file into the wallet",
        footer "Next command: receive"
      ]

renameAccParser :: Ctx -> ParserInfo Command
renameAccParser ctx =
  info
    ( CommandRenameAcc
        <$> accountArg ctx "Old account name"
        <*> textArg "New account name"
    )
    $ mconcat [progDesc "Rename an account"]

accountsParser :: ParserInfo Command
accountsParser =
  info (pure CommandAccounts) $ mconcat [progDesc "Return all accounts"]

balanceParser :: Ctx -> ParserInfo Command
balanceParser ctx =
  info (CommandBalance <$> accountOption ctx) $
    mconcat [progDesc "Get the account balance"]

resetAccParser :: Ctx -> ParserInfo Command
resetAccParser ctx =
  info (CommandResetAcc <$> accountOption ctx) $
    mconcat [progDesc "Reset the external and internal derivation indices"]

addressesParser :: Ctx -> ParserInfo Command
addressesParser ctx =
  info
    ( CommandAddresses
        <$> accountOption ctx
        <*> (Page <$> limitOption <*> offsetOption)
    )
    $ mconcat [progDesc "List the latest receiving addresses in the account"]

receiveParser :: Ctx -> ParserInfo Command
receiveParser ctx =
  info (CommandReceive <$> accountOption ctx) $
    mconcat [progDesc "Get a new address for receiving a payment"]

transactionsParser :: Ctx -> ParserInfo Command
transactionsParser ctx =
  info
    ( CommandTransactions
        <$> accountOption ctx
        <*> (Page <$> limitOption <*> offsetOption)
    )
    $ mconcat [progDesc "Display the transactions in an account"]

prepareTxParser :: Ctx -> ParserInfo Command
prepareTxParser ctx =
  info
    ( CommandPrepareTx
        <$> some recipientArg
        <*> accountOption ctx
        <*> unitOption
        <*> feeOption
        <*> dustOption
        <*> rcptPayOption
    )
    $ mconcat
      [ progDesc "Prepare a new unsigned transaction",
        footer "Next command: review, signtx"
      ]

reviewParser :: ParserInfo Command
reviewParser =
  info (CommandReview <$> fileArgument "Path of the transaction file") $
    mconcat
      [ progDesc "Review the contents of a transaction file",
        footer "Next command: signtx, sendtx"
      ]

signTxParser :: ParserInfo Command
signTxParser =
  info (CommandSignTx <$> fileArgument "Path of the transaction file" <*> splitInOption) $
    mconcat
      [ progDesc "Sign a transaction that was created with preparetx",
        footer "Next command: sendtx"
      ]

sendTxParser :: ParserInfo Command
sendTxParser =
  info (CommandSendTx <$> fileArgument "Path of the transaction file") $
    mconcat [progDesc "Broadcast a signed transaction to the network"]

prepareSweepParser :: Ctx -> ParserInfo Command
prepareSweepParser ctx =
  info
    ( CommandPrepareSweep
        <$> many (addressArg "List of addresses to sweep")
        <*> maybeFileOption "File containing addresses to sweep"
        <*> accountOption ctx
        <*> feeOption
        <*> dustOption
    )
    $ mconcat
      [ progDesc "Sweep funds into this wallet",
        footer "Next command: signsweep"
      ]

signSweepParser :: Ctx -> ParserInfo Command
signSweepParser ctx =
  info
    ( CommandSignSweep
        <$> dirArgument "Folder containing the sweep transactions"
        <*> fileArgument "Path to the file containing the private keys"
        <*> accountOption ctx
    )
    $ mconcat
      [ progDesc "Sign all the transactions contained in a sweep folder",
        footer "Next command: sendtx"
      ]

rollDiceParser :: ParserInfo Command
rollDiceParser =
  info (CommandRollDice <$> diceCountArgument) $
    mconcat
      [progDesc "Roll dice with the systems internal entropy"]

{- Option Parsers -}

textArg :: String -> Parser Text
textArg desc = argument str $ mconcat [help desc, metavar "TEXT"]

fileArgument :: String -> Parser FilePath
fileArgument desc =
  strArgument $
    mconcat
      [ help desc,
        metavar "FILENAME",
        action "file"
      ]

dirArgument :: String -> Parser FilePath
dirArgument desc =
  strArgument $
    mconcat
      [ help desc,
        metavar "DIRNAME",
        action "file"
      ]

maybeFileOption :: String -> Parser (Maybe FilePath)
maybeFileOption desc =
  optional $
    strOption $
      mconcat
        [ long "file",
          help desc,
          metavar "FILENAME",
          action "file"
        ]

diceOption :: Parser Bool
diceOption =
  switch $
    mconcat
      [ short 'd',
        long "dice",
        help "Provide additional entropy using 6-sided dice",
        showDefault
      ]

splitInOption :: Parser Natural
splitInOption =
  option (maybeReader $ readNatural . cs) $
    mconcat
      [ short 's',
        long "split",
        help
          "Split the mnemonic into different pieces using an xor algorithm. \
          \All the pieces will be required for signing.",
        metavar "NATURAL",
        value 1
      ]

entropyOption :: Parser Natural
entropyOption =
  option (maybeReader f) $
    mconcat
      [ short 'e',
        long "entropy",
        help
          "Amount of entropy to use in bytes. Valid values are [16,20..32]",
        metavar "BYTES",
        value 16,
        showDefault,
        completeWith valid
      ]
  where
    valid = ["16", "20", "24", "28", "32"]
    f s
      | s `elem` valid = fromIntegral <$> readNatural (cs s)
      | otherwise = Nothing

derivationOption :: Parser (Maybe Natural)
derivationOption =
  optional $
    option (maybeReader $ readNatural . cs) $
      mconcat
        [ short 'd',
          long "derivation",
          help "Specify a different bip44 account derivation",
          metavar "NATURAL"
        ]

networkOption :: Parser Network
networkOption =
  option (eitherReader (f . netByName)) $
    mconcat
      [ short 'n',
        long "network",
        help "Specify which coin network to use",
        metavar "TEXT",
        value btc,
        showDefault,
        completeWith ((.name) <$> allNets)
      ]
  where
    f :: Maybe Network -> Either String Network
    f Nothing =
      Left $
        unwords $
          "Invalid network name. Select one of the following:"
            : ((.name) <$> allNets)
    f (Just res) = Right res

accountOption :: Ctx -> Parser (Maybe Text)
accountOption ctx =
  optional $
    strOption $
      mconcat
        [ short 'a',
          long "account",
          help "Specify the account to use for this command",
          metavar "TEXT",
          completer (mkCompleter $ accountCompleter ctx)
        ]

accountArg :: Ctx -> String -> Parser Text
accountArg ctx desc =
  argument str $
    mconcat
      [ help desc,
        metavar "TEXT",
        completer (mkCompleter $ accountCompleter ctx)
      ]

accountCompleter :: Ctx -> String -> IO [String]
accountCompleter ctx pref = do
  keys <- fromRight [] <$> run
  return $ sort $ nub $ filter (pref `isPrefixOf`) (cs <$> keys)
  where
    run = runExceptT $ withAccountMap ctx accountMapKeys

recipientArg :: Parser (Text, Text)
recipientArg =
  (,) <$> addressArg "Recipient address" <*> amountArg

amountArg :: Parser Text
amountArg =
  strArgument $
    mconcat
      [ help "Recipient amount",
        metavar "AMOUNT"
      ]

addressArg :: String -> Parser Text
addressArg desc =
  strArgument $
    mconcat
      [ help desc,
        metavar "ADDRESS"
      ]

diceCountArgument :: Parser Natural
diceCountArgument =
  argument (maybeReader $ readNatural . cs) $
    mconcat
      [ help "Number of dice to roll",
        metavar "INT"
      ]

rcptPayOption :: Parser Bool
rcptPayOption =
  switch $
    mconcat
      [ short 'r',
        long "recipientpay",
        help "The transaction fee will be deducted from the recipient amounts",
        showDefault
      ]

offsetOption :: Parser Natural
offsetOption =
  option (maybeReader $ readNatural . cs) $
    mconcat
      [ short 'o',
        long "offset",
        help "Offset the result set",
        metavar "INT",
        value 0,
        showDefault
      ]

limitOption :: Parser Natural
limitOption =
  option (maybeReader $ readNatural . cs) $
    mconcat
      [ short 'l',
        long "limit",
        help "Limit the result set",
        metavar "INT",
        value 5,
        showDefault
      ]

feeOption :: Parser Natural
feeOption =
  option (maybeReader $ readNatural . cs) $
    mconcat
      [ short 'f',
        long "fee",
        help "Fee to pay in satoshi/bytes",
        metavar "INT",
        value 200,
        showDefault
      ]

dustOption :: Parser Natural
dustOption =
  option (maybeReader $ readNatural . cs) $
    mconcat
      [ short 'd',
        long "dust",
        help "Smallest allowed satoshi value for change outputs",
        metavar "INT",
        value 5430,
        showDefault
      ]

unitOption :: Parser AmountUnit
unitOption = satoshiOption <|> bitOption

satoshiOption :: Parser AmountUnit
satoshiOption =
  flag UnitBitcoin UnitSatoshi $
    mconcat
      [ short 's',
        long "satoshi",
        help "Use satoshis for parsing amounts (default: bitcoin)"
      ]

bitOption :: Parser AmountUnit
bitOption =
  flag UnitBitcoin UnitBit $
    mconcat
      [ short 'b',
        long "bit",
        help "Use bits for parsing amounts (default: bitcoin)"
      ]
