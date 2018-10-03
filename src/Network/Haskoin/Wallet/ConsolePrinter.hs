{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.ConsolePrinter where

import           Control.Monad         (when)
import           Data.Monoid
import           Foundation
import           Foundation.Collection
import           Foundation.IO
import           System.Console.ANSI
import           System.Exit
import           System.IO.Unsafe

data ConsolePrinter
    = ConsoleConcat !ConsolePrinter !ConsolePrinter
    | ConsoleNewline !ConsolePrinter
    | ConsoleNest !(CountOf (Element String)) !ConsolePrinter
    | ConsoleText !ConsoleFormat
    | ConsoleEmpty
    | ConsoleNonEmpty

instance Semigroup ConsolePrinter where
    a <> ConsoleEmpty = a
    ConsoleEmpty <> b = b
    a <> b            = ConsoleConcat a b

instance Monoid ConsolePrinter where
    mempty = ConsoleEmpty

isEmptyPrinter :: ConsolePrinter -> Bool
isEmptyPrinter ConsoleEmpty       = True
isEmptyPrinter (ConsoleText f)    = null $ getFormat f
isEmptyPrinter (ConsoleNest _ n)  = isEmptyPrinter n
isEmptyPrinter (ConsoleNewline n) = isEmptyPrinter n
isEmptyPrinter _                  = False

text :: ConsoleFormat -> ConsolePrinter
text = ConsoleText

(<+>) :: ConsolePrinter -> ConsolePrinter -> ConsolePrinter
p1 <+> p2
    | isEmptyPrinter p1 = p2
    | isEmptyPrinter p2 = p1
    | otherwise = p1 <> text (FormatStatic " ") <> p2

vcat :: [ConsolePrinter] -> ConsolePrinter
vcat = go . filter (not . isEmptyPrinter)
  where
    go []     = ConsoleEmpty
    go [x]    = x
    go (x:xs) = x <> ConsoleNewline (go xs)

hsep :: [ConsolePrinter] -> ConsolePrinter
hsep = go . filter (not . isEmptyPrinter)
  where
    go []     = ConsoleEmpty
    go [x]    = x
    go (x:xs) = x <+> go xs

nest :: CountOf (Element String) -> ConsolePrinter -> ConsolePrinter
nest = ConsoleNest

block :: CountOf (Element String) -> String -> String
block n str =
    case n - length str of
        Just missing -> str <> replicate missing ' '
        _            -> str

renderIO :: ConsolePrinter -> IO ()
renderIO cp = go 0 0 cp >> putStrLn ""
  where
    go :: CountOf (Element String)
       -> CountOf (Element String)
       -> ConsolePrinter
       -> IO (CountOf (Element String))
    go l n p
        | isEmptyPrinter p = return l
        | otherwise = case p of
            ConsoleConcat p1 p2 -> do
                l2 <- go l n p1
                go l2 n p2
            ConsoleNewline p1 -> do
                putStrLn ""
                putStr $ replicate n ' '
                go n n p1
            ConsoleNest i p1 -> do
                putStr $ replicate i ' '
                go (l + i) (n + i) p1
            ConsoleText f -> do
                printFormat f
                return $ l + length (getFormat f)
            ConsoleEmpty -> return l
            ConsoleNonEmpty -> return l

data ConsoleFormat
    = FormatTitle { getFormat :: !String }
    | FormatCommand { getFormat :: !String }
    | FormatOption { getFormat :: !String }
    | FormatOptExample { getFormat :: !String }
    | FormatArgument { getFormat :: !String }
    | FormatStatic { getFormat :: !String }
    | FormatAccount { getFormat :: !String }
    | FormatPubKey { getFormat :: !String }
    | FormatFilePath { getFormat :: !String }
    | FormatKey { getFormat :: !String }
    | FormatDeriv { getFormat :: !String }
    | FormatMnemonic { getFormat :: !String }
    | FormatAddress { getFormat :: !String }
    | FormatInternalAddress { getFormat :: !String }
    | FormatTxHash { getFormat :: !String }
    | FormatBlockHash { getFormat :: !String }
    | FormatPosAmount { getFormat :: !String }
    | FormatNegAmount { getFormat :: !String }
    | FormatFee { getFormat :: !String }
    | FormatTrue { getFormat :: !String }
    | FormatFalse { getFormat :: !String }
    | FormatOnline { getFormat :: !String }
    | FormatOffline { getFormat :: !String }
    | FormatNetwork { getFormat :: !String }
    | FormatError { getFormat :: !String }

formatTitle :: String -> ConsolePrinter
formatTitle = text . FormatTitle

formatCommand :: String -> ConsolePrinter
formatCommand = text . FormatCommand

formatOption :: String -> ConsolePrinter
formatOption = text . FormatOption

formatOptExample :: String -> ConsolePrinter
formatOptExample = text . FormatOptExample

formatArgument :: String -> ConsolePrinter
formatArgument = text . FormatArgument

formatStatic :: String -> ConsolePrinter
formatStatic = text . FormatStatic

formatAccount :: String -> ConsolePrinter
formatAccount = text . FormatAccount

formatPubKey :: String -> ConsolePrinter
formatPubKey = text . FormatPubKey

formatFilePath :: String -> ConsolePrinter
formatFilePath = text . FormatFilePath

formatKey :: String -> ConsolePrinter
formatKey = text . FormatKey

formatDeriv :: String -> ConsolePrinter
formatDeriv = text . FormatDeriv

formatMnemonic :: String -> ConsolePrinter
formatMnemonic = text . FormatMnemonic

formatAddress :: String -> ConsolePrinter
formatAddress = text . FormatAddress

formatInternalAddress :: String -> ConsolePrinter
formatInternalAddress = text . FormatInternalAddress

formatTxHash :: String -> ConsolePrinter
formatTxHash = text . FormatTxHash

formatBlockHash :: String -> ConsolePrinter
formatBlockHash = text . FormatBlockHash

formatPosAmount :: String -> ConsolePrinter
formatPosAmount = text . FormatPosAmount

formatNegAmount :: String -> ConsolePrinter
formatNegAmount = text . FormatNegAmount

formatFee :: String -> ConsolePrinter
formatFee = text . FormatFee

formatTrue :: String -> ConsolePrinter
formatTrue = text . FormatTrue

formatFalse :: String -> ConsolePrinter
formatFalse = text . FormatFalse

formatOnline :: String -> ConsolePrinter
formatOnline = text . FormatOnline

formatOffline :: String -> ConsolePrinter
formatOffline = text . FormatOffline

formatNetwork :: String -> ConsolePrinter
formatNetwork = text . FormatNetwork

formatError :: String -> ConsolePrinter
formatError = text . FormatError

formatSGR :: ConsoleFormat -> [SGR]
formatSGR frm = case frm of
    FormatTitle _           -> [ SetConsoleIntensity BoldIntensity ]
    FormatCommand _         -> [ SetConsoleIntensity BoldIntensity
                               , SetColor Foreground Dull Blue
                               ]
    FormatOption _          -> [ SetConsoleIntensity BoldIntensity
                               , SetColor Foreground Dull Magenta
                               ]
    FormatOptExample _      -> [ SetConsoleIntensity BoldIntensity
                               ]
    FormatArgument _        -> [ SetItalicized True
                               ]
    FormatStatic _          -> []
    FormatAccount _         -> [ SetConsoleIntensity BoldIntensity
                               , SetColor Foreground Dull White
                               ]
    FormatPubKey _          -> [ SetColor Foreground Dull Magenta ]
    FormatFilePath _        -> [ SetItalicized True
                               , SetColor Foreground Dull White
                               ]
    FormatKey _             -> []
    FormatDeriv _           -> []
    FormatMnemonic _        -> [ SetConsoleIntensity BoldIntensity
                               , SetColor Foreground Dull Cyan
                               ]
    FormatAddress _         -> [ SetConsoleIntensity BoldIntensity
                               , SetColor Foreground Dull Blue
                               ]
    FormatInternalAddress _ -> [ SetConsoleIntensity BoldIntensity
                               , SetColor Foreground Vivid Black
                               ]
    FormatTxHash _          -> [ SetColor Foreground Vivid Magenta ]
    FormatBlockHash _       -> [ SetColor Foreground Vivid Magenta ]
    FormatPosAmount  _      -> [ SetConsoleIntensity BoldIntensity
                               , SetColor Foreground Dull Green
                               ]
    FormatNegAmount _       -> [ SetConsoleIntensity BoldIntensity
                               , SetColor Foreground Dull Red
                               ]
    FormatFee _             -> []
    FormatTrue _            -> [ SetConsoleIntensity BoldIntensity
                               , SetColor Foreground Dull Green
                               ]
    FormatFalse _           -> [ SetConsoleIntensity BoldIntensity
                               , SetColor Foreground Dull Red
                               ]
    FormatOnline _          -> [ SetConsoleIntensity BoldIntensity
                               , SetColor Foreground Dull Green
                               ]
    FormatOffline _         -> [ SetConsoleIntensity BoldIntensity
                               , SetColor Foreground Dull Red
                               ]
    FormatNetwork _         -> [ SetColor Foreground Dull Green
                               ]
    FormatError _           -> [ SetColor Foreground Dull Red ]

printFormat :: ConsoleFormat -> IO ()
printFormat f = do
    support <- hSupportsANSI stdout
    when support $ setSGR $ formatSGR f
    putStr $ getFormat f
    when support $ setSGR []

consoleError :: ConsolePrinter -> a
consoleError prt = unsafePerformIO $ renderIO prt >> exitFailure

