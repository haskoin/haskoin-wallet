{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Haskoin.Wallet.Printer where

import           Control.Monad         (when)
import           Data.Monoid
import           Foundation
import           Foundation.Collection
import           Foundation.IO
import           System.Console.ANSI
import           System.Exit
import           System.IO.Unsafe

data Printer
    = PConcat !Printer !Printer
    | PNewline !Printer
    | PNest !(CountOf (Element String)) !Printer
    | PText  [SGR] !String
    | PEmpty
    | PNonEmpty

instance Semigroup Printer where
    a <> PEmpty = a
    PEmpty <> b = b
    a <> b = PConcat a b

instance Monoid Printer where
    mempty = PEmpty

isEmptyPrinter :: Printer -> Bool
isEmptyPrinter prt =
    case prt of
        PEmpty -> True
        (PText _ s) -> null s
        (PNest _ n) -> isEmptyPrinter n
        (PNewline n) -> isEmptyPrinter n
        _ -> False

text :: String -> Printer
text = PText []

(<+>) :: Printer -> Printer -> Printer
p1 <+> p2
    | isEmptyPrinter p1 = p2
    | isEmptyPrinter p2 = p1
    | otherwise = p1 <> text " " <> p2

vcat :: [Printer] -> Printer
vcat = go . filter (not . isEmptyPrinter)
  where
    go []     = PEmpty
    go [x]    = x
    go (x:xs) = x <> PNewline (go xs)

hsep :: [Printer] -> Printer
hsep = go . filter (not . isEmptyPrinter)
  where
    go []     = PEmpty
    go [x]    = x
    go (x:xs) = x <+> go xs

nest :: CountOf (Element String) -> Printer -> Printer
nest = PNest

block :: CountOf (Element String) -> String -> String
block n str =
    case n - length str of
        Just missing -> str <> replicate missing ' '
        _            -> str

renderIO :: Printer -> IO ()
renderIO cp = go 0 0 cp >> putStrLn ""
  where
    go :: CountOf (Element String)
       -> CountOf (Element String)
       -> Printer
       -> IO (CountOf (Element String))
    go l n p
        | isEmptyPrinter p = return l
        | otherwise = case p of
            PConcat p1 p2 -> do
                l2 <- go l n p1
                go l2 n p2
            PNewline p1 -> do
                putStrLn ""
                putStr $ replicate n ' '
                go n n p1
            PNest i p1 -> do
                putStr $ replicate i ' '
                go (l + i) (n + i) p1
            PText xs s -> do
                printFormat xs s
                return $ l + length s
            PEmpty -> return l
            PNonEmpty -> return l

formatTitle :: String -> Printer
formatTitle = PText [SetConsoleIntensity BoldIntensity]

formatCommand :: String -> Printer
formatCommand =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Blue
        ]

formatOption :: String -> Printer
formatOption =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Magenta
        ]

formatOptExample :: String -> Printer
formatOptExample = PText [SetConsoleIntensity BoldIntensity]

formatArgument :: String -> Printer
formatArgument = PText [SetItalicized True]

formatStatic :: String -> Printer
formatStatic = PText []

formatAccount :: String -> Printer
formatAccount =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull White
        ]

formatPubKey :: String -> Printer
formatPubKey = PText [SetColor Foreground Dull Magenta]

formatFilePath :: String -> Printer
formatFilePath =
    PText
        [ SetItalicized True
        , SetColor Foreground Dull White
        ]

formatKey :: String -> Printer
formatKey = PText []

formatDeriv :: String -> Printer
formatDeriv = PText []

formatMnemonic :: String -> Printer
formatMnemonic =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Cyan
        ]

formatAddress :: String -> Printer
formatAddress =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Blue
        ]

formatInternalAddress :: String -> Printer
formatInternalAddress =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Vivid Black
        ]

formatTxHash :: String -> Printer
formatTxHash = PText [SetColor Foreground Vivid Magenta]

formatBlockHash :: String -> Printer
formatBlockHash = PText [SetColor Foreground Vivid Magenta]

formatPosAmount :: String -> Printer
formatPosAmount =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Green
        ]

formatNegAmount :: String -> Printer
formatNegAmount =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Red
        ]

formatFee :: String -> Printer
formatFee = PText []

formatTrue :: String -> Printer
formatTrue =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Green
        ]

formatFalse :: String -> Printer
formatFalse =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Red
        ]

formatOnline :: String -> Printer
formatOnline =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Green
        ]

formatOffline :: String -> Printer
formatOffline =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Red
        ]

formatNetwork :: String -> Printer
formatNetwork = PText [SetColor Foreground Dull Green]

formatError :: String -> Printer
formatError = PText [SetColor Foreground Dull Red]

printFormat :: [SGR] -> String -> IO ()
printFormat sgr str = do
    support <- hSupportsANSI stdout
    when support $ setSGR sgr
    putStr str
    when support $ setSGR []

printError :: String -> a
printError = printCustomError . formatError

printCustomError :: Printer -> a
printCustomError prt = unsafePerformIO $ renderIO prt >> exitFailure
