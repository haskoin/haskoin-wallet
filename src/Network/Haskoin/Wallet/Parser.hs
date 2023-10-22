{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Haskoin.Wallet.Parser where

import Control.Monad.Except (runExceptT)
import Data.Either (fromRight)
import Data.List (isPrefixOf, nub, sort)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin.Crypto (Ctx)
import Haskoin.Network (Network (name), allNets, btc, netByName)
import Network.Haskoin.Wallet.Amounts
  ( AmountUnit (..),
    readNatural,
  )
import Network.Haskoin.Wallet.Database
import Network.Haskoin.Wallet.Util (Page (Page))
import Numeric.Natural (Natural)
import Options.Applicative
  ( Alternative (many, some, (<|>)),
    Parser,
    ParserInfo,
    action,
    argument,
    asum,
    command,
    commandGroup,
    completeWith,
    completer,
    eitherReader,
    flag,
    footer,
    fullDesc,
    help,
    helper,
    hidden,
    hsubparser,
    info,
    long,
    maybeReader,
    metavar,
    mkCompleter,
    option,
    optional,
    progDesc,
    progDescDoc,
    short,
    showDefault,
    showDefaultWith,
    str,
    strArgument,
    strOption,
    style,
    switch,
    value,
    (<**>),
  )
import Options.Applicative.Help.Pretty
  ( Color (Red),
    Doc,
    annotate,
    color,
  )

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
  | CommandTestAcc
      { commandMaybeAcc :: !(Maybe Text),
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
      { commandMaybeAcc :: !(Maybe Text)
      }
  | CommandReceive
      { commandMaybeAcc :: !(Maybe Text),
        commandMaybeLabel :: !(Maybe Text)
      }
  | CommandAddrs
      { commandMaybeAcc :: !(Maybe Text),
        commandPage :: !Page
      }
  | CommandLabel
      { commandMaybeAcc :: !(Maybe Text),
        commandAddrIndex :: !Natural,
        commandLabel :: !Text
      }
  | CommandTxs
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
      { commandMaybeAcc :: !(Maybe Text),
        commandFilePath :: !FilePath
      }
  | CommandSignTx
      { commandFilePath :: !FilePath,
        commandSplitIn :: !Natural
      }
  | CommandCoins
      { commandMaybeAcc :: !(Maybe Text),
        commandPage :: !Page
      }
  | CommandSendTx
      { commandMaybeAcc :: !(Maybe Text),
        commandFilePath :: !FilePath
      }
  | CommandSyncAcc
      { commandMaybeAcc :: !(Maybe Text),
        commandFull :: !Bool
      }
  | CommandDiscoverAcc
      { commandMaybeAcc :: !(Maybe Text)
      }
  | CommandVersion
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

programParser :: ParserInfo Command
programParser =
  info (commandParser <**> helper) $
    mconcat
      [ fullDesc,
        progDesc
          "hw is a lightweight BIP44 wallet for bitcoin and bitcoin-cash. \
          \ It is designed so that mnemonic generation, account creation and\
          \ transaction signing can happen on an offline, air-gapped computer.\
          \ It will always ask for the mnemonic when signing and never store\
          \ private keys on disk."
      ]

commandParser :: Parser Command
commandParser =
  asum
    [ hsubparser $
        mconcat
          [ commandGroup "Mnemonic and account management",
            command "mnemonic" mnemonicParser,
            command "createacc" createAccParser,
            command "testacc" testAccParser,
            command "importacc" importAccParser,
            command "renameacc" renameAccParser,
            command "accounts" accountsParser,
            metavar "COMMAND",
            style (const "COMMAND --help")
          ],
      hsubparser $
        mconcat
          [ commandGroup "Address management",
            command "receive" receiveParser,
            command "addrs" addrsParser,
            command "label" labelParser,
            hidden
          ],
      hsubparser $
        mconcat
          [ commandGroup "Transaction management",
            command "txs" txsParser,
            command "preparetx" prepareTxParser,
            command "review" reviewParser,
            command "signtx" signTxParser,
            command "coins" coinsParser,
            hidden
          ],
      hsubparser $
        mconcat
          [ commandGroup "Online (Blockchain) commands",
            command "sendtx" sendTxParser,
            command "syncacc" syncAccParser,
            command "discoveracc" discoverAccParser,
            hidden
          ],
      hsubparser $
        mconcat
          [ commandGroup "Utilities",
            command "version" versionParser,
            command "preparesweep" prepareSweepParser,
            command "signsweep" signSweepParser,
            command "rolldice" rollDiceParser,
            hidden
          ]
    ]

offline :: Doc -> Maybe Doc
offline s = Just $ s <> annotate (color Red) " (Offline)"

{- Mnemonic Parser -}

mnemonicParser :: ParserInfo Command
mnemonicParser =
  info (CommandMnemonic <$> entropyOption <*> diceOption <*> splitInOption) $
    mconcat
      [ progDescDoc $ offline "Generate a mnemonic",
        footer
          "Generate a mnemonic using the systems entropy pool.\
          \ By default it should be /dev/random on linux machines.\
          \ If you use the --dice option, the additional dice entropy will be\
          \ mixed with the system entropy. You should ideally run this\
          \ command on an offline (air-gapped) computer. The mnemonic will NOT\
          \ be stored on disk. It will only be printed to the screen and you\
          \ should write it down and keep it in a secure location. If you\
          \ choose to use the --split option, the mnemonic will be split into\
          \ different pieces that you can write down and store in separate\
          \ locations. ALL the pieces will be required for signing so make\
          \ sure you have copies of all of them."
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
  option (eitherReader $ f . cs) $
    mconcat
      [ short 's',
        long "split",
        help
          "Split the mnemonic into different pieces using an xor algorithm.\
          \ All the pieces will be required for signing.",
        metavar "INT",
        value 1
      ]
  where
    f s =
      case readNatural s of
        Just n ->
          if n >= 2 && n <= 12
            then Right n
            else Left "Split value has to be in the range [2-12]"
        Nothing -> Left "Could not parse the split option"

{- CreateAcc Parser -}

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
      [ progDescDoc $ offline "Create a new account",
        footer
          "Create a new account with a given name. This command requires you\
          \ to type your mnemonic and should ideally be run on an offline\
          \ computer. If you have a split mnemonic, you will need to use the\
          \ --split option. This command will derive the public component of\
          \ the account and store it on disk. No private keys will be stored\
          \ on disk. You will be asked for the mnemonic again when you will\
          \ sign transactions. The public key file for the account will be\
          \ stored in ~/.hw/pubkeys which can be imported on an online\
          \ computer."
      ]

networkOption :: Parser Network
networkOption =
  option (eitherReader (f . netByName)) $
    mconcat
      [ short 'n',
        long "network",
        help $
          unwords $
            "Specify which coin network to use: " : ((.name) <$> allNets),
        metavar "TEXT",
        value btc,
        showDefaultWith (.name),
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

derivationOption :: Parser (Maybe Natural)
derivationOption =
  optional $
    option (maybeReader $ readNatural . cs) $
      mconcat
        [ short 'd',
          long "derivation",
          help "Specify a different bip44 account derivation",
          metavar "INT"
        ]

{- TestAcc Parser -}

testAccParser :: ParserInfo Command
testAccParser =
  info (CommandTestAcc <$> accountOption <*> splitInOption) $
    mconcat
      [ progDescDoc $ offline "Test your mnemonic and passphrase",
        footer
          "You should test regularly the mnemonic and passphrase of your account.\
          \ This command will derive the public key associated with the account\
          \ and make sure that it matches the account on file."
      ]

accountOption :: Parser (Maybe Text)
accountOption =
  optional $
    strOption $
      mconcat
        [ short 'a',
          long "account",
          help "Specify an account to use for this command",
          metavar "TEXT",
          completer (mkCompleter accountCompleter)
        ]

{- ImportAcc Parser -}

importAccParser :: ParserInfo Command
importAccParser =
  info (CommandImportAcc <$> fileArgument "Path of the account file") $
    mconcat
      [ progDesc "Import a public key account file",
        footer
          "When creating a new account using the createacc command, a\
          \ public key account file will be generated in ~/.hw/pubkeys\
          \ which can be imported into a different computer with importacc.\
          \ This will allow you to monitor the transactions of that account."
      ]

{- RenameAcc Parser -}

renameAccParser :: ParserInfo Command
renameAccParser =
  info
    ( CommandRenameAcc
        <$> accountArg "Old account name"
        <*> textArg "New account name"
    )
    $ mconcat [progDesc "Rename an account"]

accountArg :: String -> Parser Text
accountArg desc =
  argument str $
    mconcat
      [ help desc,
        metavar "TEXT",
        completer (mkCompleter accountCompleter)
      ]

accountCompleter :: String -> IO [String]
accountCompleter pref = do
  names <- runDB getAccountNames
  return $ sort $ nub $ filter (pref `isPrefixOf`) (cs <$> names)

{- Accounts Parser -}

accountsParser :: ParserInfo Command
accountsParser =
  info (CommandAccounts <$> accountOption) $
    mconcat [progDesc "Display account information"]

{- Receive Parser -}

receiveParser :: ParserInfo Command
receiveParser =
  info
    (CommandReceive <$> accountOption <*> labelOption)
    $ mconcat [progDesc "Get a new address for receiving a payment"]

labelOption :: Parser (Maybe Text)
labelOption =
  optional $
    strOption $
      mconcat
        [ short 'l',
          long "label",
          help "Specify a label for the address",
          metavar "TEXT"
        ]

{- Addrs Parser -}

addrsParser :: ParserInfo Command
addrsParser =
  info
    ( CommandAddrs
        <$> accountOption
        <*> (Page <$> limitOption <*> offsetOption)
    )
    $ mconcat [progDesc "List the latest receiving addresses in the account"]

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

{- Label Parser-}

labelParser :: ParserInfo Command
labelParser =
  info
    ( CommandLabel
        <$> accountOption
        <*> addrIndexArg
        <*> textArg "The new address label"
    )
    $ mconcat [progDesc "Set the label of an address"]

addrIndexArg :: Parser Natural
addrIndexArg =
  argument (maybeReader $ readNatural . cs) $
    mconcat
      [ help "The index of the address to update",
        metavar "INT"
      ]

{- Txs Parser -}

txsParser :: ParserInfo Command
txsParser =
  info
    ( CommandTxs
        <$> accountOption
        <*> (Page <$> limitOption <*> offsetOption)
    )
    $ mconcat [progDesc "Display the transactions in an account"]

{- PrepareTx Parser -}

prepareTxParser :: ParserInfo Command
prepareTxParser =
  info
    ( CommandPrepareTx
        <$> some recipientArg
        <*> accountOption
        <*> unitOption
        <*> feeOption
        <*> dustOption
        <*> rcptPayOption
    )
    $ mconcat
      [ progDesc "Prepare a new unsigned transaction",
        footer
          "You can call preparetx on an online computer to create an unsigned\
          \ transaction that spends funds from your account. This can be done\
          \ without having access to your mnemonic. A file containing your\
          \ unsigned transaction will be created in ~/.hw/transactions.\
          \ You can then inspect the transaction using the review command.\
          \ If you are unhappy with the transaction, you can simply delete the\
          \ file in ~/.hw/transactions. Otherwise, you can move the transaction\
          \ file to an offline computer and sign it with the signtx command.\
          \ Once signed, you can move the signed transaction file back to an\
          \ online computer and broadcast it using the sendtx command."
      ]

recipientArg :: Parser (Text, Text)
recipientArg =
  (,) <$> addressArg "Recipient address" <*> amountArg

addressArg :: String -> Parser Text
addressArg desc =
  strArgument $
    mconcat
      [ help desc,
        metavar "ADDRESS"
      ]

amountArg :: Parser Text
amountArg =
  strArgument $
    mconcat
      [ help "Recipient amount",
        metavar "AMOUNT"
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

rcptPayOption :: Parser Bool
rcptPayOption =
  switch $
    mconcat
      [ short 'r',
        long "recipientpay",
        help "The transaction fee will be deducted from the recipient amounts",
        showDefault
      ]

{- Review Parser -}

reviewParser :: ParserInfo Command
reviewParser =
  info
    ( CommandReview
        <$> accountOption
        <*> fileArgument "Path of the transaction file"
    )
    $ mconcat
      [ progDesc "Review the contents of a transaction file",
        footer
          "The review command allows you to inspect the details of a transaction\
          \ file that was created using the preparetx command. This might be\
          \ useful to check that everything is correct before signing (signtx) and\
          \ broadcasting (sendtx) the transaction."
      ]

{- SignTx Parser -}

signTxParser :: ParserInfo Command
signTxParser =
  info
    ( CommandSignTx
        <$> fileArgument "Path of the transaction file"
        <*> splitInOption
    )
    $ mconcat
      [ progDescDoc $
          offline "Sign a transaction that was created with preparetx",
        footer
          "The signtx command allows you to sign an unsigned transaction file\
          \ that was created using the preparetx command. Ideally you want to\
          \ run signtx on an offline computer as you will have to type the\
          \ mnemonic. Once signed, a new signed transaction file will be created\
          \ in ~/.hw/transactions. You can move this file to an online computer\
          \ and broadcast it using the sendtx command. The mnemonic is only used\
          \ to sign the transaction. The mnemonic or the private keys will not\
          \ be stored on disk. If you want to sign another transaction, you will\
          \ have to enter the mnemonic again. If you have a split mnemonic, you\
          \ will have to use the --split option."
      ]

{- Coins Parser -}

coinsParser :: ParserInfo Command
coinsParser =
  info
    ( CommandCoins
        <$> accountOption
        <*> (Page <$> limitOption <*> offsetOption)
    )
    $ mconcat
      [progDesc "List all the coins in an account"]

{- SendTx Parser -}

sendTxParser :: ParserInfo Command
sendTxParser =
  info
    ( CommandSendTx
        <$> accountOption
        <*> fileArgument "Path of the transaction file"
    )
    $ mconcat [progDesc "Broadcast a signed transaction file to the network"]

{- SyncAcc Parser -}

syncAccParser :: ParserInfo Command
syncAccParser =
  info (CommandSyncAcc <$> accountOption <*> fullOption) $
    mconcat
      [ progDesc "Download new transactions, balances and coins",
        footer
          "The wallet data is stored in a local database. Most commands\
          \ will query this database to show results. To refresh your local\
          \ database with the latest information from the blockchain, you must\
          \ call syncacc. This will pull the latest transactions, balances and\
          \ coins into your wallet. Unles you specify the --full option, a\
          \ partial sync is performed by comparing address balances and\
          \ downloading only the data from addresses that have changed. If you\
          \ specify the --full option, everything will be downloaded again but\
          \ this can take time depending on the size of your wallet."
      ]

fullOption :: Parser Bool
fullOption =
  switch $
    mconcat
      [ long "full",
        help "Perform a full sync instead of a regular sync",
        showDefault
      ]

{- DiscoverAcc Parser -}

discoverAccParser :: ParserInfo Command
discoverAccParser =
  info (CommandDiscoverAcc <$> accountOption) $
    mconcat
      [ progDesc "Scan the blockchain to generate missing addresses",
        footer
          "If you are recovering an account or there are addresses missing in\
          \ your account, you can call discoveracc to search the blockchain for\
          \ missing addresses. Addresses will be scanned and generated until\
          \ a gap (default = 20) of unused addresses is found."
      ]

{- Version Parser -}

versionParser :: ParserInfo Command
versionParser =
  info (pure CommandVersion) $
    mconcat
      [progDesc "Display the version of hw"]

{- PrepareSweep Parser -}

prepareSweepParser :: ParserInfo Command
prepareSweepParser =
  info
    ( CommandPrepareSweep
        <$> many (addressArg "List of addresses to sweep")
        <*> maybeFileOption "File containing addresses to sweep"
        <*> accountOption
        <*> feeOption
        <*> dustOption
    )
    $ mconcat
      [ progDesc "Sweep funds into this wallet",
        footer
          "This utility command will prepare a set of unsigned transactions\
          \ that will send all the funds available in the given addresses to\
          \ your hw account. The typical use case for this command is to\
          \ migrate an old wallet to hw. You can pass the addresses on the\
          \ command line or they can be parsed from a file. The preparesweep\
          \ command will randomize all the coins and create a number of transactions\
          \ containing between 1 and 5 inputs and 2 outputs. The transactions\
          \ will be available in the ~/.hw/sweep-[id] folder. You can then\
          \ use the signsweep command to sign the transactions."
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

{- SignSweep Parser -}

signSweepParser :: ParserInfo Command
signSweepParser =
  info
    ( CommandSignSweep
        <$> dirArgument "Folder containing the sweep transactions"
        <*> fileArgument "Path to the file containing the private keys"
        <*> accountOption
    )
    $ mconcat
      [ progDesc "Sign all the transactions contained in a sweep folder",
        footer
          "The private keys have to be provided in a separate file.\
          \ The currently supported formats are WIF and MiniKey."
      ]

{- RollDice Parser -}

rollDiceParser :: ParserInfo Command
rollDiceParser =
  info (CommandRollDice <$> diceCountArg) $
    mconcat
      [progDesc "Roll dice with the systems internal entropy"]

diceCountArg :: Parser Natural
diceCountArg =
  argument (maybeReader $ readNatural . cs) $
    mconcat
      [ help "Number of dice to roll",
        metavar "INT"
      ]

{- Argument parser helpers -}

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
