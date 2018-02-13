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
    [ cmdHelp
    , cmdMnemonic
    , cmdCreateAcc
    , cmdImportAcc
    , cmdRenameAcc
    , cmdReceive
    , cmdAddresses
    , cmdPrepareTx
    , cmdSignTx
    , cmdSendTx
    , cmdBalance
    , cmdTransactions
    ]

cmdHelp :: ConsolePrinter
cmdHelp =
    vcat
        [ formatCommand "help"
        , formatStatic "Display this information."
        ]

cmdMnemonic :: ConsolePrinter
cmdMnemonic =
    vcat
        [ formatCommand "mnemonic"
        , fOffline <+> followup ["createacc", "signtx"]
        , formatStatic "Generate a mnemonic using your systems entropy."
        , nest 2 $ vcat [fOpt getDiceOpt, fOpt getEntOpt]
        ]

cmdCreateAcc :: ConsolePrinter
cmdCreateAcc =
    vcat
        [ formatCommand "createacc"
        , fOffline <+> followup ["importacc"]
        , formatStatic "Create a new account from a mnemonic."
        , nest 2 $ vcat [fOpt getDerOpt, fOpt getNetOpt]
        ]

cmdImportAcc :: ConsolePrinter
cmdImportAcc =
    vcat
        [ formatCommand "importacc" <+> formatArgument "Filename"
        , followup ["receive"]
        , formatStatic "Import an account file into the wallet."
        , nest 2 $ fOpt getNetOpt
        ]

cmdRenameAcc :: ConsolePrinter
cmdRenameAcc =
    vcat
        [ formatCommand "renameacc" <+>
          formatArgument "OldName" <+> formatArgument "NewName"
        , formatStatic "Rename an account."
        , nest 2 $ fOpt getNetOpt
        ]

cmdReceive :: ConsolePrinter
cmdReceive =
    vcat
        [ formatCommand "receive"
        , formatStatic "Generate a new address for receiving a payment."
        , nest 2 $ vcat [fOpt getAccOpt, fOpt getNetOpt]
        ]

cmdAddresses :: ConsolePrinter
cmdAddresses =
    vcat
        [ formatCommand "addresses"
        , formatStatic "List the latest receiving addresses in your account."
        , nest 2 $ vcat [fOpt getCntOpt, fOpt getAccOpt, fOpt getNetOpt]
        ]

cmdPrepareTx :: ConsolePrinter
cmdPrepareTx =
    vcat
        [ formatCommand "preparetx" <+>
          formatArgument "address value [address2 value2 ...]"
        , fOnline <+> followup ["signtx"]
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

cmdSignTx :: ConsolePrinter
cmdSignTx =
    vcat
        [ formatCommand "signtx" <+> formatArgument "Filename"
        , fOffline <+> followup ["sendtx"]
        , formatStatic "Sign a transaction that was created with preparetx."
        , nest 2 $ vcat [ fOpt getDerOpt, fOpt getUnitOpt, fOpt getNetOpt ]
        ]

cmdSendTx :: ConsolePrinter
cmdSendTx =
    vcat
        [ formatCommand "sendtx" <+> formatArgument "Filename"
        , fOnline
        , formatStatic "Broadcast a transaction that was signed with signtx."
        , nest 2 $ vcat [ fOpt getNetOpt, fOpt getSerOpt ]
        ]

cmdBalance :: ConsolePrinter
cmdBalance =
    vcat
        [ formatCommand "balance"
        , fOnline
        , formatStatic "Display the account balance."
        , nest 2 $
          vcat [fOpt getAccOpt, fOpt getUnitOpt, fOpt getNetOpt, fOpt getSerOpt]
        ]

cmdTransactions :: ConsolePrinter
cmdTransactions =
    vcat
        [ formatCommand "transactions"
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
