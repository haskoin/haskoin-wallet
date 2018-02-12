{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.UsageInfo (usage) where

import Foundation
import Foundation.String
import Network.Haskoin.Wallet.ConsolePrinter

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
        , nest 2 commands
        , ConsoleNonEmpty
        ]

commands :: ConsolePrinter
commands =
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
        , nest 2 $ vcat [fDiceOpt, fEntOpt]
        ]

cmdCreateAcc :: ConsolePrinter
cmdCreateAcc =
    vcat
        [ formatCommand "createacc"
        , fOffline <+> followup ["importacc"]
        , formatStatic "Create a new account from a mnemonic."
        , nest 2 $ vcat [fDerOpt, fNetOpt]
        ]

cmdImportAcc :: ConsolePrinter
cmdImportAcc =
    vcat
        [ formatCommand "importacc" <+> formatArgument "Filename"
        , followup ["receive"]
        , formatStatic "Import an account file into the wallet."
        , nest 2 $ fNetOpt
        ]

cmdRenameAcc :: ConsolePrinter
cmdRenameAcc =
    vcat
        [ formatCommand "renameacc" <+>
          formatArgument "OldName" <+> formatArgument "NewName"
        , formatStatic "Rename an account."
        , nest 2 $ fNetOpt
        ]

cmdReceive :: ConsolePrinter
cmdReceive =
    vcat
        [ formatCommand "receive"
        , formatStatic "Generate a new address for receiving a payment."
        , nest 2 $ vcat [ fAccOpt, fNetOpt ]
        ]

cmdAddresses :: ConsolePrinter
cmdAddresses =
    vcat
        [ formatCommand "addresses"
        , formatStatic "List the latest receiving addresses in your account."
        , nest 2 $ vcat [ fCntOpt, fAccOpt, fNetOpt ]
        ]

cmdPrepareTx :: ConsolePrinter
cmdPrepareTx =
    vcat
        [ formatCommand "preparetx" <+>
          formatArgument "address value [address2 value2 ...]"
        , fOnline <+> followup ["signtx"]
        , formatStatic "Prepare a new unsigned transaction."
        , nest 2 $ vcat [ fFeeOpt, fDustOpt, fUnitOpt, fAccOpt, fNetOpt, fSerOpt ]
        ]

cmdSignTx :: ConsolePrinter
cmdSignTx =
    vcat
        [ formatCommand "signtx" <+> formatArgument "Filename"
        , fOffline <+> followup ["sendtx"]
        , formatStatic "Sign a transaction that was created with preparetx."
        , nest 2 $ vcat [ fDerOpt, fUnitOpt, fNetOpt ]
        ]

cmdSendTx :: ConsolePrinter
cmdSendTx =
    vcat
        [ formatCommand "sendtx" <+> formatArgument "Filename"
        , fOnline
        , formatStatic "Broadcast a transaction that was signed with signtx."
        , nest 2 $ vcat [ fNetOpt, fSerOpt ]
        ]

cmdBalance :: ConsolePrinter
cmdBalance =
    vcat
        [ formatCommand "balance"
        , fOnline
        , formatStatic "Display the account balance."
        , nest 2 $ vcat [ fAccOpt, fUnitOpt, fNetOpt, fSerOpt ]
        ]

cmdTransactions :: ConsolePrinter
cmdTransactions =
    vcat
        [ formatCommand "transactions"
        , fOnline
        , formatStatic "Display the account transactions."
        , nest 2 $ vcat [ fAccOpt, fUnitOpt, fVerOpt, fNetOpt, fSerOpt ]
        ]

fDiceOpt :: ConsolePrinter
fDiceOpt =
  fOpt "d" "dice" "True" "False" <+>
  formatStatic "Provide additional entropy using 6-sided dice."

fEntOpt :: ConsolePrinter
fEntOpt =
  fOpt "e" "entropy" "[16,20..32]" "16" <+>
  formatStatic "Use more entropy to generate a mnemonic."

fDerOpt :: ConsolePrinter
fDerOpt =
    fOpt "d" "deriv" "1" "0" <+>
    formatStatic "Specify a different bip44 account derivation."

fNetOpt :: ConsolePrinter
fNetOpt =
    fOpt "n" "network" "[bitcoin,testnet3,bitcoincash,cashtest]" "bitcoin"

fAccOpt :: ConsolePrinter
fAccOpt =
    fOpt "a" "account" "main" "" <+>
    formatStatic "Specify a different account to use for this command."

fCntOpt :: ConsolePrinter
fCntOpt =
    fOpt "c" "count" "10" "5" <+>
    formatStatic "Number of addresses to display."

fFeeOpt :: ConsolePrinter
fFeeOpt =
    fOpt "f" "fee" "50" "200" <+>
    formatStatic "Fee to pay in sat/bytes"

fDustOpt :: ConsolePrinter
fDustOpt =
    fOpt "d" "dust" "8000" "5430" <+>
    formatStatic "Smallest allowed satoshi value for change outputs."

fUnitOpt :: ConsolePrinter
fUnitOpt =
    fOpt "u" "unit" "[bitcoin,bit,satoshi]" "bitcoin" <+>
    formatStatic "Specify the unit for displayed amounts."

fSerOpt :: ConsolePrinter
fSerOpt =
    fOpt "s" "service" "[haskoin,blockchain,insight]" "haskoin" <+>
    formatStatic "HTTP data service."

fVerOpt :: ConsolePrinter
fVerOpt =
    fOpt "v" "verbose" "True" "False" <+>
    formatStatic "Produce a more detailed output for this command."

{- Helpers -}

fOpt :: String -> String -> String -> String -> ConsolePrinter
fOpt short long example def =
    hsep
        [ formatStatic "-" <> formatOption short
        , formatStatic "--" <> formatOption long <> formatStatic "=" <>
          formatOptExample example
        , if null def
              then mempty
              else formatStatic "(Default:" <+>
                   formatOptExample def <> formatStatic ")"
        ]

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
