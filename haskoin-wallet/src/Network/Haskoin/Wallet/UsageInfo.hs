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

cmdHelp :: (String, String)
cmdHelp = ("help", "Display this information.")

cmdHelpFormat :: ConsolePrinter
cmdHelpFormat =
    vcat
        [ formatCommand (cmdName cmdHelp)
        , formatStatic (cmdDesc cmdHelp)
        ]

cmdMnemonic :: (String, String)
cmdMnemonic = ("mnemonic", "Generate a mnemonic using your systems entropy.")

cmdMnemonicFormat :: ConsolePrinter
cmdMnemonicFormat =
    vcat

    [ formatCommand (cmdName cmdMnemonic)
        , fOffline <+> followup [cmdName cmdCreateAcc, cmdName cmdSignTx]
        , formatStatic (cmdDesc cmdMnemonic)
        , nest 2 $ vcat [fOpt getDiceOpt, fOpt getEntOpt]
        ]

cmdCreateAcc :: (String, String)
cmdCreateAcc = ("createacc", "Create a new account from a mnemonic.")

cmdCreateAccFormat :: ConsolePrinter
cmdCreateAccFormat =
    vcat
        [ formatCommand (cmdName cmdCreateAcc)
        , fOffline <+> followup [cmdName cmdImportAcc]
        , formatStatic (cmdDesc cmdCreateAcc)
        , nest 2 $ vcat [fOpt getDerOpt, fOpt getNetOpt]
        ]

cmdImportAcc :: (String, String)
cmdImportAcc = ("importacc", "Import an account file into the wallet.")

cmdImportAccFormat :: ConsolePrinter
cmdImportAccFormat =
    vcat
        [ formatCommand (cmdName cmdImportAcc) <+> formatArgument "Filename"
        , followup [cmdName cmdReceive]
        , formatStatic (cmdDesc cmdImportAcc)
        , nest 2 $ fOpt getNetOpt
        ]

cmdRenameAcc :: (String, String)
cmdRenameAcc = ("renameacc", "Rename an account.")

cmdRenameAccFormat :: ConsolePrinter
cmdRenameAccFormat =
    vcat
        [ formatCommand (cmdName cmdRenameAcc) <+>
          formatArgument "OldName" <+> formatArgument "NewName"
        , formatStatic (cmdDesc cmdRenameAcc)
        , nest 2 $ fOpt getNetOpt
        ]

cmdReceive :: (String, String)
cmdReceive = ("receive", "Generate a new address for receiving a payment.")

cmdReceiveFormat :: ConsolePrinter
cmdReceiveFormat =
    vcat
        [ formatCommand (cmdName cmdReceive)
        , formatStatic (cmdDesc cmdReceive)
        , nest 2 $ vcat [fOpt getAccOpt, fOpt getNetOpt]
        ]

cmdAddresses :: (String, String)
cmdAddresses =
    ("addresses", "List the latest receiving addresses in your account.")

cmdAddressesFormat :: ConsolePrinter
cmdAddressesFormat =
    vcat
        [ formatCommand (cmdName cmdAddresses)
        , formatStatic (cmdDesc cmdAddresses)
        , nest 2 $ vcat [fOpt getCntOpt, fOpt getAccOpt, fOpt getNetOpt]
        ]

cmdPrepareTx :: (String, String)
cmdPrepareTx = ("preparetx", "Prepare a new unsigned transaction.")

cmdPrepareTxFormat :: ConsolePrinter
cmdPrepareTxFormat =
    vcat
        [ formatCommand (cmdName cmdPrepareTx) <+>
          formatArgument "address value [address2 value2 ...]"
        , fOnline <+> followup [cmdName cmdSignTx]
        , formatStatic (cmdDesc cmdPrepareTx)
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

cmdSignTx :: (String, String)
cmdSignTx = ("signtx", "Sign a transaction that was created with preparetx.")

cmdSignTxFormat :: ConsolePrinter
cmdSignTxFormat =
    vcat
        [ formatCommand (cmdName cmdSignTx) <+> formatArgument "Filename"
        , fOffline <+> followup [cmdName cmdSendTx]
        , formatStatic (cmdDesc cmdSignTx)
        , nest 2 $ vcat [fOpt getDerOpt, fOpt getUnitOpt, fOpt getNetOpt]
        ]

cmdSendTx :: (String, String)
cmdSendTx = ("sendtx", "Broadcast a transaction that was signed with signtx.")

cmdSendTxFormat :: ConsolePrinter
cmdSendTxFormat =
    vcat
        [ formatCommand (cmdName cmdSendTx) <+> formatArgument "Filename"
        , fOnline
        , formatStatic (cmdDesc cmdSendTx)
        , nest 2 $ vcat [fOpt getNetOpt, fOpt getSerOpt]
        ]

cmdBalance :: (String, String)
cmdBalance = ("balance", "Display the account balance.")

cmdBalanceFormat :: ConsolePrinter
cmdBalanceFormat =
    vcat
        [ formatCommand (cmdName cmdBalance)
        , fOnline
        , formatStatic (cmdDesc cmdBalance)
        , nest 2 $
          vcat [fOpt getAccOpt, fOpt getUnitOpt, fOpt getNetOpt, fOpt getSerOpt]
        ]

cmdTransactions :: (String, String)
cmdTransactions = ("transactions", "Display the account transactions.")

cmdTransactionsFormat :: ConsolePrinter
cmdTransactionsFormat =
    vcat
        [ formatCommand (cmdName cmdTransactions)
        , fOnline
        , formatStatic (cmdDesc cmdTransactions)
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

cmdName :: (String, String) -> String
cmdName = fst

cmdNameL :: (String, String) -> LString
cmdNameL = toLString . cmdName

cmdDesc :: (String, String) -> String
cmdDesc = snd

cmdDescL :: (String, String) -> LString
cmdDescL = toLString . cmdDesc

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

