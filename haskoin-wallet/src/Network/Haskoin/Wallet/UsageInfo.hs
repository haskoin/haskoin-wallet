{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.UsageInfo where

import           Foundation
import           Network.Haskoin.Wallet.ConsolePrinter
import           Network.Haskoin.Wallet.FoundationCompat

usage :: ConsolePrinter
usage =
    vcat
        [ ConsoleNonEmpty
        , formatTitle "hw" <+>
          formatCommand "command" <+>
          formatStatic "[" <> formatArgument "Args" <> formatStatic "]" <+>
          formatStatic "[--" <> formatOption "options" <> formatStatic "]"
        , formatStatic "Lightweight Bitcoin and Bitcoin Cash wallet."
        , ConsoleNonEmpty
        , nest 2 printerCommands
        , ConsoleNonEmpty
        ]

printerCommands :: ConsolePrinter
printerCommands =
    vcat $ intersperse ConsoleNonEmpty
    [ cmdHelpFormat
    , cmdMnemonicFormat
    , cmdCreateAccFormat
    , cmdImportAccFormat
    , cmdRenameAccFormat
    , cmdReceiveFormat
    , cmdAddressesFormat
    , cmdPrepareTxFormat
    , cmdSignTxFormat
    , cmdSendTxFormat
    , cmdBalanceFormat
    , cmdTransactionsFormat
    ]

cmdHelp :: String
cmdHelp = "help"

cmdMnemonic :: String
cmdMnemonic = "mnemonic"

cmdCreateAcc :: String
cmdCreateAcc = "createacc"

cmdImportAcc :: String
cmdImportAcc = "importacc"

cmdRenameAcc :: String
cmdRenameAcc = "renameacc"

cmdReceive :: String
cmdReceive = "receive"

cmdAddresses :: String
cmdAddresses = "addresses"

cmdPrepareTx :: String
cmdPrepareTx = "preparetx"

cmdSignTx :: String
cmdSignTx = "signtx"

cmdSendTx :: String
cmdSendTx = "sendtx"

cmdBalance :: String
cmdBalance = "balance"

cmdTransactions :: String
cmdTransactions = "transactions"

cmdHelpFormat :: ConsolePrinter
cmdHelpFormat =
    vcat
        [ formatCommand cmdHelp
        , formatStatic "Display this information."
        ]

cmdMnemonicFormat :: ConsolePrinter
cmdMnemonicFormat =
    vcat
        [ formatCommand cmdMnemonic
        , fOffline <+> followup [cmdCreateAcc, cmdSignTx]
        , formatStatic "Generate a mnemonic using your systems entropy."
        , nest 2 $ vcat [fOpt getDiceOpt, fOpt getEntOpt]
        ]

cmdCreateAccFormat :: ConsolePrinter
cmdCreateAccFormat =
    vcat
        [ formatCommand cmdCreateAcc
        , fOffline <+> followup [cmdImportAcc]
        , formatStatic "Create a new account from a mnemonic."
        , nest 2 $ vcat [fOpt getDerOpt, fOpt getNetOpt]
        ]

cmdImportAccFormat :: ConsolePrinter
cmdImportAccFormat =
    vcat
        [ formatCommand cmdImportAcc <+> formatArgument "Filename"
        , followup [cmdReceive]
        , formatStatic "Import an account file into the wallet."
        , nest 2 $ fOpt getNetOpt
        ]

cmdRenameAccFormat :: ConsolePrinter
cmdRenameAccFormat =
    vcat
        [ formatCommand cmdRenameAcc <+>
          formatArgument "OldName" <+> formatArgument "NewName"
        , formatStatic "Rename an account."
        , nest 2 $ fOpt getNetOpt
        ]

cmdReceiveFormat :: ConsolePrinter
cmdReceiveFormat =
    vcat
        [ formatCommand cmdReceive
        , formatStatic "Generate a new address for receiving a payment."
        , nest 2 $ vcat [fOpt getAccOpt, fOpt getNetOpt]
        ]

cmdAddressesFormat :: ConsolePrinter
cmdAddressesFormat =
    vcat
        [ formatCommand cmdAddresses
        , formatStatic "List the latest receiving addresses in your account."
        , nest 2 $ vcat [fOpt getCntOpt, fOpt getAccOpt, fOpt getNetOpt]
        ]

cmdPrepareTxFormat :: ConsolePrinter
cmdPrepareTxFormat =
    vcat
        [ formatCommand cmdPrepareTx <+>
          formatArgument "address value [address2 value2 ...]"
        , fOnline <+> followup [cmdSignTx]
        , formatStatic "Prepare a new unsigned transaction."
        , nest 2 $
          vcat
              [ fOpt getFeeOpt
              , fOpt getDustOpt
              , fOpt getUnitOpt
              , fOpt getAccOpt
              , fOpt getNetOpt
              , fOpt getSerOpt
              ]
        ]

cmdSignTxFormat :: ConsolePrinter
cmdSignTxFormat =
    vcat
        [ formatCommand cmdSignTx <+> formatArgument "Filename"
        , fOffline <+> followup [cmdSendTx]
        , formatStatic "Sign a transaction that was created with preparetx."
        , nest 2 $ vcat [ fOpt getDerOpt, fOpt getUnitOpt, fOpt getNetOpt ]
        ]

cmdSendTxFormat :: ConsolePrinter
cmdSendTxFormat =
    vcat
        [ formatCommand cmdSendTx <+> formatArgument "Filename"
        , fOnline
        , formatStatic "Broadcast a transaction that was signed with signtx."
        , nest 2 $ vcat [ fOpt getNetOpt, fOpt getSerOpt ]
        ]

cmdBalanceFormat :: ConsolePrinter
cmdBalanceFormat =
    vcat
        [ formatCommand cmdBalance 
        , fOnline
        , formatStatic "Display the account balance."
        , nest 2 $
          vcat [fOpt getAccOpt, fOpt getUnitOpt, fOpt getNetOpt, fOpt getSerOpt]
        ]

cmdTransactionsFormat :: ConsolePrinter
cmdTransactionsFormat =
    vcat
        [ formatCommand cmdTransactions
        , fOnline
        , formatStatic "Display the account transactions."
        , nest 2 $
          vcat
              [ fOpt getAccOpt
              , fOpt getUnitOpt
              , fOpt getVerbOpt
              , fOpt getNetOpt
              , fOpt getSerOpt
              ]
        ]

data ConsoleOption a = ConsoleOption
    { consoleOptionShort       :: !Char
    , consoleOptionLong        :: !String
    , consoleOptionExample     :: [String]
    , consoleOptionDefault     :: a
    , consoleOptionDescription :: !String
    }

getDiceOpt :: ConsoleOption Bool
getDiceOpt =
    ConsoleOption
        'd'
        "dice"
        ["True"]
        False
        "Provide additional entropy using 6-sided dice."

getEntOpt :: ConsoleOption Natural
getEntOpt =
    ConsoleOption
        'e'
        "entropy"
        ["[16,20..32]"]
        16
        "Use more entropy to generate a mnemonic."

getDerOpt :: ConsoleOption Natural
getDerOpt =
    ConsoleOption
        'd'
        "deriv"
        ["1"]
        0
        "Specify a different bip44 account derivation."

getNetOpt :: ConsoleOption String
getNetOpt =
    ConsoleOption
        'n'
        "network"
        ["bitcoin", "testnet3", "bitcoincash", "cashtest"]
        "bitcoin"
        ""

getAccOpt :: ConsoleOption String
getAccOpt =
    ConsoleOption
        'a'
        "account"
        ["main"]
        ""
        "Specify a different account to use for this command."

getCntOpt :: ConsoleOption Natural
getCntOpt = ConsoleOption 'c' "count" ["10"] 5 "Number of addresses to display."

getFeeOpt :: ConsoleOption Natural
getFeeOpt = ConsoleOption 'f' "fee" ["50"] 200 "Fee to pay in sat/bytes"

getDustOpt :: ConsoleOption Natural
getDustOpt =
    ConsoleOption
        'd'
        "dust"
        ["8000"]
        5430
        "Smallest allowed satoshi value for change outputs."

getUnitOpt :: ConsoleOption String
getUnitOpt =
    ConsoleOption
        'u'
        "unit"
        ["bitcoin", "bit", "satoshi"]
        "bitcoin"
        "Specify the unit for displayed amounts."

getSerOpt :: ConsoleOption String
getSerOpt =
    ConsoleOption
        's'
        "service"
        ["haskoin", "blockchain", "insight"]
        "haskoin"
        "HTTP data service."

getVerbOpt :: ConsoleOption Bool
getVerbOpt =
    ConsoleOption
        'v'
        "verbose"
        ["True"]
        False
        "Produce a more detailed output for this command."

{- Helpers -}

fOpt :: Show a => ConsoleOption a -> ConsolePrinter
fOpt (ConsoleOption short long examples def str) =
    hsep
        [ formatStatic "-" <> formatOption (fromLString [short])
        , formatStatic "--" <> formatOption long <> formatStatic "=" <> fExample
        , formatStatic "(Default:" <+>
          formatOptExample (show def) <> formatStatic ")"
        , formatStatic str
        ]
  where
    fExample =
        case examples of
            [] -> mempty
            [e] -> formatOptExample e
            es ->
                formatStatic "[" <>
                intercalate (formatStatic ",") (formatOptExample <$> es) <>
                formatStatic "]"

followup :: [String] -> ConsolePrinter
followup cmds =
    formatStatic next <+>
    mconcat (intersperse (formatStatic ", ") fCmds) <>
    formatStatic ")"
  where
    fCmds = formatCommand <$> cmds
    next | length cmds > 1 = "(Next commands:"
         | otherwise = "(Next command:"

fOnline :: ConsolePrinter
fOnline = formatOnline "Online" <+> formatStatic "command"

fOffline :: ConsolePrinter
fOffline = formatOffline "Offline" <+> formatStatic "command"
