{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.UsageInfo where

import           Data.Tree                               (Tree(..))
import           Foundation
import           Foundation.Monad
import           Network.Haskoin.Wallet.Printer
import           Network.Haskoin.Wallet.FoundationCompat
import qualified System.Console.Argument                 as Argument
import qualified System.Console.Command                  as Command

type Commands m = Tree (Command m)

data Command m =
    Command
    { wrappedCommand :: Command.Command m
    , commandFormat  :: !Printer
    }

data Action m =
    Action
    { wrappedAction :: Command.Action m
    , actionOptions :: [Printer]
    , actionNonOptions :: [Printer]
    }

data Option a = Option
    { wrappedOption :: Argument.Option a
    , optionFormat  :: !Printer
    }

data CommandType
    = CommandOnline
    | CommandOffline

io :: MonadIO m => m () -> Action m
io m = Action (Command.io m) [] []

usageTitle :: Command m -> Printer
usageTitle cmd = 
    vcat
        [ formatTitle (fromLString $ Command.name $ wrappedCommand cmd) <+>
          formatCommand "command" <+>
          formatStatic "[" <> formatArgument "Args" <> formatStatic "]" <+>
          formatStatic "[--" <> formatOption "options" <> formatStatic "]"
        , formatStatic (fromLString $ Command.description $ wrappedCommand cmd)
        ]

usage :: Commands m -> Printer
usage (Node rootCmd rootNs) =
    vcat
        [ PNonEmpty
        , usageTitle rootCmd
        , PNonEmpty
        , nest 2 $ vcat $ go <$> rootNs
        ]
  where
    go (Node cmd ns) =
        vcat [commandFormat cmd, PNonEmpty, nest 2 $ vcat $ go <$> ns]

command ::
       String
    -> String
    -> Maybe CommandType
    -> [Command m]
    -> Action m
    -> Command m
command name desc cmdType next action =
    Command
    { wrappedCommand =
          Command.command
              (toLString name)
              (toLString desc)
              (wrappedAction action)
    , commandFormat =
          commandPrinter
              name
              (actionNonOptions action)
              desc
              cmdType
              next
              (actionOptions action)
    }

withOption :: MonadIO m => Option a -> (a -> Action m) -> Action m
withOption o f =
    Action
    { wrappedAction = Command.withOption (wrappedOption o) (wrappedAction . f)
    , actionOptions = optionFormat o : actionOptions (f undefined)
    , actionNonOptions = actionNonOptions (f undefined)
    }

withNonOption ::
       MonadIO m => Argument.Type x -> String -> (x -> Action m) -> Action m
withNonOption t str f =
    Action
    { wrappedAction = Command.withNonOption t (wrappedAction . f)
    , actionOptions = actionOptions (f undefined)
    , actionNonOptions = formatArgument str : actionNonOptions (f undefined)
    }

withNonOptions ::
       MonadIO m => Argument.Type x -> String -> ([x] -> Action m) -> Action m
withNonOptions t str f =
    Action
    { wrappedAction = Command.withNonOptions t (wrappedAction . f)
    , actionOptions = actionOptions (f undefined)
    , actionNonOptions = formatArgument str : actionNonOptions (f undefined)
    }

commandPrinter ::
       String
    -> [Printer]
    -> String
    -> Maybe CommandType
    -> [Command m]
    -> [Printer]
    -> Printer
commandPrinter name args desc cmdType next opts =
    vcat
        [ formatCommand name <+> hsep args
        , fCmdType <+>
          followup (fromLString . Command.name . wrappedCommand <$> next)
        , formatStatic desc
        , nest 2 $ vcat opts
        ]
  where
    followup [] = mempty
    followup cmds =
        formatStatic fNext <+>
        mconcat (intersperse (formatStatic ", ") fCmds) <> formatStatic ")"
      where
        fCmds = formatCommand <$> cmds
        fNext
            | length cmds > 1 = "(Next commands:"
            | otherwise = "(Next command:"
    fCmdType =
        case cmdType of
            Just CommandOnline ->
                formatOnline "Online" <+> formatStatic "command"
            Just CommandOffline ->
                formatOffline "Offline" <+> formatStatic "command"
            _ -> mempty

option ::
       Show a
    => Char
    -> String
    -> [String]
    -> a
    -> Argument.Type a
    -> String
    -> Option a
option short long examples def t desc =
    Option
    { wrappedOption =
          Argument.option [short] [toLString long] t def (toLString desc)
    , optionFormat = optionPrinter (short, long, examples, def, desc)
    }

optionPrinter :: Show a => (Char, String, [String], a, String) -> Printer
optionPrinter (short, long, examples, def, str) =
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

