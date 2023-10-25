{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Network.Haskoin.Wallet.Parser where

import Control.Monad.Except (runExceptT)
import Data.Either (fromRight)
import Data.List (intercalate, isPrefixOf, nub, sort)
import Data.String.Conversions (cs)
import Data.Text (Text, toCaseFold)
import Haskoin (bech32Const)
import Haskoin.Crypto (Ctx)
import Haskoin.Network (Network (name), allNets, btc, netByName)
import Haskoin.Transaction (TxHash, hexToTxHash)
import Network.Haskoin.Wallet.Amounts
  ( AmountUnit (..),
    readNatural,
  )
import Network.Haskoin.Wallet.Database
import Network.Haskoin.Wallet.Util (Page (Page))
import Numeric.Natural (Natural)
import Options.Applicative
import Options.Applicative.Help.Pretty
  ( Color (Red),
    Doc,
    annotate,
    color,
    parens,
  )
import Text.RawString.QQ

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
  | CommandExportAcc
      { commandMaybeAcc :: !(Maybe Text),
        commandFilePath :: !FilePath
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
        commandRcptPay :: !Bool,
        commandOutputFileMaybe :: !(Maybe FilePath)
      }
  | CommandPendingTxs
      { commandMaybeAcc :: !(Maybe Text),
        commandPage :: !Page
      }
  | CommandReviewTx
      { commandMaybeAcc :: !(Maybe Text),
        commandFilePath :: !FilePath
      }
  | CommandImportTx
      { commandMaybeAcc :: !(Maybe Text),
        commandFilePath :: !FilePath
      }
  | CommandExportTx
      { commandNoSigHash :: !TxHash,
        commandFilePath :: !FilePath
      }
  | CommandDeleteTx
      { commandMaybeAcc :: !(Maybe Text),
        commandNoSigHash :: !TxHash
      }
  | CommandSignTx
      { commandMaybeAcc :: !(Maybe Text),
        commandNoSigHashMaybe :: !(Maybe TxHash),
        commandInputFileMaybe :: !(Maybe FilePath),
        commandOutputFileMaybe :: !(Maybe FilePath),
        commandSplitIn :: !Natural
      }
  | CommandCoins
      { commandMaybeAcc :: !(Maybe Text),
        commandPage :: !Page
      }
  | CommandSendTx
      { commandMaybeAcc :: !(Maybe Text),
        commandNoSigHash :: !TxHash
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
      { commandMaybeAcc :: !(Maybe Text),
        commandSweepFrom :: ![Text],
        commandSweepFileMaybe :: !(Maybe FilePath),
        commandSweepTo :: ![Text],
        commandOutputFileMaybe :: !(Maybe FilePath),
        commandFeeByte :: !Natural,
        commandDust :: !Natural
      }
  | CommandSignSweep
      { commandMaybeAcc :: !(Maybe Text),
        commandNoSigHashMaybe :: !(Maybe TxHash),
        commandInputFileMaybe :: !(Maybe FilePath),
        commandOutputFileMaybe :: !(Maybe FilePath),
        commandSecKeyPath :: !FilePath
      }
  | CommandRollDice
      { commandCount :: !Natural
      }
  deriving (Eq, Show)

parserMain :: IO Command
parserMain =
  customExecParser
    (prefs $ showHelpOnEmpty <> helpIndent 25)
    programParser

programParser :: ParserInfo Command
programParser = do
  let cmd = commandParser <**> helper
  info cmd $
    fullDesc
      <> progDesc
        [r|
hw is a BIP-44 command-line wallet for bitcoin and bitcoin-cash. It allows
sensitive commands (!) to be run on a separate offline computer. For more
information on a command, type "hw COMMAND --help".
|]

commandParser :: Parser Command
commandParser =
  asum
    [ hsubparser $
        mconcat
          [ commandGroup "Mnemonic and account management",
            command "mnemonic" mnemonicParser,
            command "createacc" createAccParser,
            command "testacc" testAccParser,
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
            command "pendingtxs" pendingTxsParser,
            command "signtx" signTxParser,
            command "deletetx" deleteTxParser,
            command "coins" coinsParser,
            hidden
          ],
      hsubparser $
        mconcat
          [ commandGroup "Import/export commands",
            command "exportacc" exportAccParser,
            command "importacc" importAccParser,
            command "reviewtx" reviewTxParser,
            command "exporttx" exportTxParser,
            command "importtx" importTxParser,
            hidden
          ],
      hsubparser $
        mconcat
          [ commandGroup "Network commands",
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
offline s = Just $ annotate (color Red) "! " <> s

{- Mnemonic Parser -}

mnemonicParser :: ParserInfo Command
mnemonicParser = do
  let cmd =
        CommandMnemonic
          <$> entropyOption
          <*> diceOption
          <*> splitInOption
  info cmd $
    progDescDoc (offline "Generate a mnemonic")
      <> footer
        [r|
Generate a mnemonic using the systems entropy pool. By default it should be
/dev/random on linux machines. If you use the --dice option, the additional dice
entropy will be mixed with the system entropy. You should ideally run this
command on an offline (air-gapped) computer. The mnemonic will NOT be stored on
disk. It will only be printed to the screen and you should write it down and
keep it in a secure location. Make multiple copies of your mnemonic. Do not
store your mnemonic on a digital medium. If you lose your mnemonic, you lose
your funds. If you choose to use the --split option, the mnemonic will be split
into different pieces such that ALL the pieces will be required for signing.
|]

entropyOption :: Parser Natural
entropyOption =
  option (maybeReader f) $
    short 'e'
      <> long "entropy"
      <> metavar "BYTES"
      <> value 16
      <> showDefault
      <> completeWith valid
      <> help
        [r|
Amount of entropy to use in bytes. Valid values are 16, 20, 24, 28 or 32.
|]
  where
    valid = ["16", "20", "24", "28", "32"]
    f s
      | s `elem` valid = fromIntegral <$> readNatural (cs s)
      | otherwise = Nothing

diceOption :: Parser Bool
diceOption =
  switch $
    short 'd'
      <> long "dice"
      <> help
        [r|
Provide additional entropy using 6-sided dice. The entropy will be mixed with
the system entropy.
|]

splitInOption :: Parser Natural
splitInOption =
  option (eitherReader $ f . cs) $
    short 's'
      <> long "split"
      <> metavar "INT"
      <> value 1
      <> help
        [r|
The mnemonic is split in different pieces and reconstructed using bitwise xor.
All the pieces are required for signing. The ordering of the pieces is not
important.
|]
  where
    f s =
      case readNatural s of
        Just n ->
          if n >= 2 && n <= 12
            then Right n
            else Left "The --split value has to be between 2 and 12"
        Nothing -> Left "Could not parse the --split option"

{- CreateAcc Parser -}

createAccParser :: ParserInfo Command
createAccParser = do
  let cmd =
        CommandCreateAcc
          <$> textArg "Name of the new account"
          <*> networkOption
          <*> derivationOption
          <*> splitInOption
  info cmd $
    progDescDoc (offline "Create a new account")
      <> footer
        [r|
An account corresponds to the BIP-32 extended key derivation
m/44'/coin'/account'. For example, the bitcoin account 0 would be m/44'/0'/0'.
An account is tied to a private key and thus to a mnemonic. The derived account
m/44'/0'/0' would be different given two different mnemonics. To help manage
your accounts in hw, they are identified by a name. The command `createacc` will
ask for a mnemonic and save the derived public key M/44'/coin'/account' on disk.
The private keys are never stored anywhere while using hw. If you are using an
offline computer, you can then export your account and import it on an online
computer.
|]

networkOption :: Parser Network
networkOption =
  option (eitherReader (f . netByName)) $
    short 'n'
      <> long "network"
      <> metavar "TEXT"
      <> value btc
      <> showDefaultWith (.name)
      <> completeWith ((.name) <$> allNets)
      <> help ("Specify one of the following networks to use: " <> nets)
  where
    nets = intercalate ", " ((.name) <$> allNets)
    f :: Maybe Network -> Either String Network
    f Nothing =
      Left $ "Invalid network name. Select one of the following: " <> nets
    f (Just res) = Right res

derivationOption :: Parser (Maybe Natural)
derivationOption =
  optional . option (maybeReader $ readNatural . cs) $
    short 'd'
      <> long "derivation"
      <> metavar "INT"
      <> help
        [r|
Specify a different account derivation to use (the last part of
m/44'/coin'/account'). By default, account derivations are chosen sequentially
starting from 0.
|]

{- TestAcc Parser -}

testAccParser :: ParserInfo Command
testAccParser = do
  let cmd =
        CommandTestAcc
          <$> accountOption
          <*> splitInOption
  info cmd $
    progDescDoc (offline "Check the validity of your mnemonic/passphrase")
      <> footer
        [r|
`testacc` will prompt for your mnemonic/passphrase and check that the derived
public key M/44'/coin'/account' matches with the account stored on disk.
|]

accountOption :: Parser (Maybe Text)
accountOption =
  optional . strOption $
    short 'a'
      <> long "account"
      <> metavar "TEXT"
      <> completer (mkCompleter accountCompleter)
      <> help
        [r|
Specify the account name if the wallet has more than one account.
|]

{- ImportAcc Parser -}

importAccParser :: ParserInfo Command
importAccParser = do
  let cmd =
        CommandImportAcc
          <$> fileArgument "Path to the account file"
  info cmd $
    progDesc "Import an account file"
      <> footer importExportAccFooter

importExportAccFooter :: String
importExportAccFooter =
  [r|
When working in an online/offline environment, you can `importacc` a public key
file (on an online computer) that was exported with `exportacc` (on an offline
computer). This will allow the online computer to monitor transactions without
requiring access to the private keys. The file contains the public key
derivation M/44'/coin'/account' along with the account name, the network and a
wallet identifier. The wallet identifier is a fingerprint of the first public
account derivation M/44'/coin'/0'.
|]

{- ExportAcc Parser -}

exportAccParser :: ParserInfo Command
exportAccParser = do
  let cmd =
        CommandExportAcc
          <$> accountOption
          <*> fileArgument "File where the account data will be saved"
  info cmd $
    progDesc "Export account data to a file"
      <> footer importExportAccFooter

{- RenameAcc Parser -}

renameAccParser :: ParserInfo Command
renameAccParser = do
  let cmd =
        CommandRenameAcc
          <$> accountArg "Old account name"
          <*> textArg "New account name"
  info cmd $ progDesc "Rename an account"

accountArg :: String -> Parser Text
accountArg desc =
  argument str $
    metavar "TEXT"
      <> completer (mkCompleter accountCompleter)
      <> help desc

accountCompleter :: String -> IO [String]
accountCompleter _ = return []

{- TODO: Fix this
accountCompleter :: String -> IO [String]
accountCompleter pref = do
  names <- runDB getAccountNames
  return $ sort $ nub $ filter (pref `isPrefixOf`) (cs <$> names)
-}

{- Accounts Parser -}

accountsParser :: ParserInfo Command
accountsParser = do
  let cmd = CommandAccounts <$> accountOption
  info cmd $ progDesc "Display account information"

{- Receive Parser -}

receiveParser :: ParserInfo Command
receiveParser = do
  let cmd =
        CommandReceive <$> accountOption <*> labelOption
  info cmd $
    progDesc "Get a new address for receiving a payment"
      <> footer
        [r|
There are two types of addresses in a BIP-44 wallet: internal and external addresses.
Internal addresses are not exposed to the user and are managed internally by the
wallet (mostly for producing change outputs in transactions). The `receive` command
produces external addresses that are meant for receiving payments. Internal addresses
are derived under the BIP-32 path M/44'/coin'/account'/1/address. External addresses
use the path M/44'/coin'/account'/0/address.
|]

labelOption :: Parser (Maybe Text)
labelOption =
  optional . strOption $
    short 'l'
      <> long "label"
      <> metavar "TEXT"
      <> help "Specify a label for the address"

{- Addrs Parser -}

addrsParser :: ParserInfo Command
addrsParser = do
  let cmd =
        CommandAddrs
          <$> accountOption
          <*> (Page <$> limitOption <*> offsetOption)
  info cmd $
    progDesc "List the receiving addresses of an account"

offsetOption :: Parser Natural
offsetOption =
  option (maybeReader $ readNatural . cs) $
    short 'o'
      <> long "offset"
      <> metavar "INT"
      <> value 0
      <> showDefault
      <> help "Offset the result set"

limitOption :: Parser Natural
limitOption =
  option (maybeReader $ readNatural . cs) $
    short 'l'
      <> long "limit"
      <> metavar "INT"
      <> value 5
      <> showDefault
      <> help
        [r|
Limit the result set. If the result set is very large, you can specify the
--limit and --offset options to view the required data. For example, to skip the
first 20 values and then display the following 10, use --limit=10 and
--offset=20.
|]

{- Label Parser-}

labelParser :: ParserInfo Command
labelParser = do
  let cmd =
        CommandLabel
          <$> accountOption
          <*> addrIndexArg
          <*> textArg "The new label for the address"
  info cmd $ progDesc "Set the label of an address"

addrIndexArg :: Parser Natural
addrIndexArg =
  argument (maybeReader $ readNatural . cs) $
    metavar "INT"
      <> help
        [r|
The index of the external address to update. The first address has an index of
0.
|]

{- Txs Parser -}

txsParser :: ParserInfo Command
txsParser = do
  let cmd =
        CommandTxs
          <$> accountOption
          <*> (Page <$> limitOption <*> offsetOption)
  info cmd $ progDesc "Display the transactions of an account"

{- PrepareTx Parser -}

prepareTxParser :: ParserInfo Command
prepareTxParser = do
  let cmd =
        CommandPrepareTx
          <$> some recipientArg
          <*> accountOption
          <*> unitOption
          <*> feeOption
          <*> dustOption
          <*> rcptPayOption
          <*> outputFileMaybeOption
  info cmd $
    progDesc "Prepare an unsigned transaction for making a payment"
      <> footer
        [r|
In hw, sending a transaction happens in separate steps. First, an unsigned
transaction is prepared using `preparetx`. No mnemonic or private keys are
required for this step and `preparetx` can be run on an online computer. The new
transaction will be stored in the local wallet database as a "pending"
transaction but it can also be exported as a file using the --output option. The
pending transactions can be inspected either with `reviewtx` or `pendingtxs`.
The new transaction is not signed. The next step will be to sign it. Any coins
spent by it will be locked to prevent spending them twice. If you change your
mind about sending this transaction, you can call `deletetx` to remove it and
free up the locked coins. All the pending transactions are referenced by their
noSigHash (a hash of the transaction without its signatures).
|]

recipientArg :: Parser (Text, Text)
recipientArg = (,) <$> addressArg <*> amountArg

addressArg :: Parser Text
addressArg =
  strArgument $
    metavar "ADDRESS"
      <> help
        [r|
Recipient address. By can provide multiple "ADDRESS AMOUNT" pairs. 
|]

amountArg :: Parser Text
amountArg =
  strArgument $
    metavar "AMOUNT"
      <> help
        [r|
Recipient amount. By default, amounts are parsed as bitcoins. You can also use the
--satoshi or --bit option to specify amounts in satoshis or bits. For example,
"0.00001" bitcoins is equivalent to "10.00" bits or "1000" satoshi. Bitcoins can
have up to 8 decimal places, bits up to 2 and satoshi are whole numbers.
|]

feeOption :: Parser Natural
feeOption =
  option (maybeReader $ readNatural . cs) $
    short 'f'
      <> long "fee"
      <> metavar "INT"
      <> value 200
      <> showDefault
      <> help "Fee to pay in satoshi/bytes"

dustOption :: Parser Natural
dustOption =
  option (maybeReader $ readNatural . cs) $
    short 'd'
      <> long "dust"
      <> metavar "INT"
      <> value 5430
      <> showDefault
      <> help "Amount (in satoshi) below which an output is considered dust"

unitOption :: Parser AmountUnit
unitOption = satoshiOption <|> bitOption

satoshiOption :: Parser AmountUnit
satoshiOption =
  flag UnitBitcoin UnitSatoshi $
    short 's'
      <> long "satoshi"
      <> help "Use satoshis for parsing amounts (default: bitcoin)"

bitOption :: Parser AmountUnit
bitOption =
  flag UnitBitcoin UnitBit $
    short 'b'
      <> long "bit"
      <> help "Use bits for parsing amounts (default: bitcoin)"

rcptPayOption :: Parser Bool
rcptPayOption =
  switch $
    short 'r'
      <> long "recipientpay"
      <> showDefault
      <> help "The transaction fee will be deducted from the recipient amounts"

outputFileMaybeOption :: Parser (Maybe FilePath)
outputFileMaybeOption =
  optional . strOption $
    short 'o'
      <> long "output"
      <> metavar "FILENAME"
      <> action "file"
      <> help "Write the result to this file"

{- PendingTxs Parser -}

pendingTxsParser :: ParserInfo Command
pendingTxsParser = do
  let cmd =
        CommandPendingTxs
          <$> accountOption
          <*> (Page <$> limitOption <*> offsetOption)
  info cmd $
    progDesc "Display the pending transactions of an account"
      <> footer
        [r|
Pending transactions have been created with the `preparetx` command. They are
either unsigned and waiting to be signed, or signed and waiting to be sent to
the network.
|]

{- ReviewTx Parser -}

reviewTxParser :: ParserInfo Command
reviewTxParser = do
  let cmd =
        CommandReviewTx
          <$> accountOption
          <*> fileArgument "Path to the transaction file to review"
  info cmd $
    progDesc "Review a transaction file"
      <> footer
        [r|
Review a pending transaction file that has been created with the --output option
of the `preparetx` command or the `exporttx` command. You can review a
transaction file this way before signing it on an offline computer for example.
|]

{- ExportTx Parser -}

exportTxParser :: ParserInfo Command
exportTxParser = do
  let cmd =
        CommandExportTx
          <$> nosigHashArg
          <*> fileArgument "File where the transaction data will be saved"
  info cmd $
    progDesc "Export pending transaction data to a file"
      <> footer importExportTxFooter

importExportTxFooter :: String
importExportTxFooter = [r|
A transaction that has been prepared with `preparetx` can be exported to a file
so that it can be signed on an offline computer. The transaction is identified
by its nosigHash (a hash of the transaction without its signatures) as this
identifier doesn't change when the transaction is unsigned or signed. Once
signed, you can `importtx` the transaction back into the computer that
prepared the transaction. 
|]

nosigHashArg :: Parser TxHash
nosigHashArg =
  argument (maybeReader $ hexToTxHash . cs) $
    metavar "NOSIGHASH"
      <> help "The nosigHash of the transaction"

{- ImportTx Parser -}

importTxParser :: ParserInfo Command
importTxParser = do
  let cmd =
        CommandImportTx
          <$> accountOption
          <*> fileArgument "Path to the transaction file to import"
  info cmd $
    progDesc "Import a pending transaction"
      <> footer importExportTxFooter

{- DeleteTx Parser -}

deleteTxParser :: ParserInfo Command
deleteTxParser = do
  let cmd =
        CommandDeleteTx
          <$> accountOption
          <*> nosigHashArg
  info cmd $
    progDesc "Delete a pending transaction"
      <> footer
        [r|
A transaction that has been prepared with `preparetx` will lock the coins that
it has spent to prevent double-spending them. A pending transaction can be
deleted with `deletetx` to permanently remove it from the database. This will
also free any coins that have been locked by it. This will only be possible as
long as the transaction hasn't been signed and sent to the network.
|]

{- SignTx Parser -}

signTxParser :: ParserInfo Command
signTxParser = do
  let cmd =
        CommandSignTx
          <$> accountOption
          <*> optional nosigHashArg
          <*> inputFileMaybeOption
          <*> outputFileMaybeOption
          <*> splitInOption
  info cmd $
    progDescDoc (offline "Sign a transaction")
      <> footer
        [r|
The next step after preparing a transaction is to sign it. This step can happen
on an offline computer if you export the unsigned transaction to a file. If the
transaction is stored in the wallet, it can be signed by using its nosigHash.
This will sign and replace the transaction in-place. Otherwise, you must specify
the --input file of the unsigned transaction and the --output file where the
signed transaction will be saved. The transaction still hasn't been uploaded to
the network. You may inspect it using either the `reviewtx` or the `pendingtxs`
command. The transaction can then be sent to the network with `sendtx` or
deleted with `deletetx`.
|]

inputFileMaybeOption :: Parser (Maybe FilePath)
inputFileMaybeOption =
  optional . strOption $
    short 'i'
      <> long "input"
      <> metavar "FILENAME"
      <> action "file"
      <> help "Read the input from this file"

{- Coins Parser -}

coinsParser :: ParserInfo Command
coinsParser = do
  let cmd =
        CommandCoins
          <$> accountOption
          <*> (Page <$> limitOption <*> offsetOption)
  info cmd $
    progDesc "List the account coins (unspent outputs)"
      <> footer
        [r|
These are all the coins in the account. Coins can be locked when preparing new
transactions. When sending those transactions to the network, the locked coins
will be spent and removed. Calling `deletetx` will remove a pending transaction
and free up any locked coins.
|]

{- SendTx Parser -}

sendTxParser :: ParserInfo Command
sendTxParser = do
  let cmd =
        CommandSendTx
          <$> accountOption
          <*> nosigHashArg
  info cmd $
    progDesc "Send (upload) a signed transaction to the network"
      <> footer
        [r|
Once a transaction is signed, it can be sent to the network. This step is
irreversible and the transaction will become effective. Make sure everything is
correct with your transaction before sending it. You must `importtx` the
transaction before sending it if you have it in a file.
|]

{- SyncAcc Parser -}

syncAccParser :: ParserInfo Command
syncAccParser = do
  let cmd = CommandSyncAcc <$> accountOption <*> fullOption
  info cmd $
    progDesc "Download account data from the network"
      <> footer
        [r|
The `syncacc` command will download the latest address balances, transactions
and coins from the network. It will also update the account balances.
|]

fullOption :: Parser Bool
fullOption =
  switch $
    long "full"
      <> showDefault
      <> help
        [r|
Force the syncacc command to re-download all the account data instead of only
the deltas (the data that has changed). This should normally not be required
unless some information is wrong, for example, when a previous syncacc was
called on a partially discovered account (the account was missing some
addresses).
|]

{- DiscoverAcc Parser -}

discoverAccParser :: ParserInfo Command
discoverAccParser = do
  let cmd = CommandDiscoverAcc <$> accountOption
  info cmd $
    progDesc "Scan the blockchain to generate missing addresses"
      <> footer
        [r|
`discoveracc` is typically called when restoring an account in a new wallet.
Initially, there will be no addresses stored in the account. `discoveracc` will
scan the blockchain and generate all the addresses (both external and internal)
that have received coins at some point. The search is stopped when a gap
(default = 20) of empty addresses is found. A full sync (syncacc --full) is
run automatically at the end of the `discoveracc` command. When restoring an
old wallet, it is important to discover it first before generating addresses
and receiving payments. Otherwise some addresses might be reused.
|]

{- Version Parser -}

versionParser :: ParserInfo Command
versionParser = do
  let cmd = pure CommandVersion
  info cmd $ progDesc "Display the version of hw"

{- PrepareSweep Parser -}

prepareSweepParser :: ParserInfo Command
prepareSweepParser = do
  let cmd =
        CommandPrepareSweep
          <$> accountOption
          <*> many sweepFromOption -- many: not optional
          <*> addressFileOption
          <*> some sweepToOption -- some: optional
          <*> outputFileMaybeOption
          <*> feeOption
          <*> dustOption
  info cmd $
    progDesc "Prepare a transaction to sweep funds"
      <> footer
        [r|
`preparesweep` will prepare a transaction that sweeps the funds available in the
given --sweepfrom addresses and sends them to the --sweepto addresses. The
typical use case for this command is to migrate an old wallet to a new mnemonic.
The addresses can also be parsed from a --addrfile. The best way to pass
multiple addresses on the command line is with the shorthand -s ADDR1 -s ADDR2
for --sweepfrom addresses and -t ADDR1 -t ADDR2 for --sweepto addresses. You
can generate addresses to sweep to with the `receive` command.
|]

sweepFromOption :: Parser Text
sweepFromOption =
  strOption $
    short 's'
      <> long "sweepfrom"
      <> metavar "ADDRESS"
      <> help "Addresses to sweep from"

addressFileOption :: Parser (Maybe FilePath)
addressFileOption =
  optional . strOption $
    long "addrfile"
      <> metavar "FILENAME"
      <> action "file"
      <> help "File containing addresses to sweep from"

sweepToOption :: Parser Text
sweepToOption =
  strOption $
    short 't'
      <> long "sweepto"
      <> metavar "ADDRESS"
      <> help "Addresses to sweep to"

{- SignSweep Parser -}

signSweepParser :: ParserInfo Command
signSweepParser = do
  let cmd =
        CommandSignSweep
          <$> accountOption
          <*> optional nosigHashArg
          <*> inputFileMaybeOption
          <*> outputFileMaybeOption
          <*> prvKeyFileOption
  info cmd $
    progDesc "Sign a sweep transaction"
      <> footer
        [r|
Sign a transaction that was prepared with the `preparesweep` command. As the
coins of this transaction are not from this wallet, the private keys for signing
must be passed in a separate --prvkeys file. The sweep transaction can be
referenced by its nosigHash if it exists in the wallet or you can pass it as a
file --input.
|]

prvKeyFileOption :: Parser FilePath
prvKeyFileOption =
  strOption $
    short 'k'
      <> long "prvkeys"
      <> metavar "FILENAME"
      <> action "file"
      <> help "File containing the private keys in WIF or MiniKey format"

{- RollDice Parser -}

rollDiceParser :: ParserInfo Command
rollDiceParser = do
  let cmd = CommandRollDice <$> diceCountArg
  info cmd $
    progDesc "Roll 6-sided dice using the systems internal entropy"
      <> footer
        [r|
By default, /dev/random is used on linux machines. The mapping from a byte to
dice rolls ensures that all dice rolls have the same probability of occuring.
Bytes 0x00 to 0xfb are used and anything above is dropped.
|]

diceCountArg :: Parser Natural
diceCountArg =
  argument (maybeReader $ readNatural . cs) $
    metavar "INT"
      <> help "Number of 6-sided dice to roll"

{- Argument parser helpers -}

textArg :: String -> Parser Text
textArg desc =
  argument str $
    metavar "TEXT"
      <> help desc

fileArgument :: String -> Parser FilePath
fileArgument desc =
  strArgument $
    metavar "FILENAME"
      <> action "file"
      <> help desc

