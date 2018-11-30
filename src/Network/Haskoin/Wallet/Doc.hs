{-# LANGUAGE NoImplicitPrelude #-}
module Network.Haskoin.Wallet.Doc where

import           Data.String.Conversions                 (cs)
import           Data.Text                               (Text)
import           Foundation
import           Network.Haskoin.Wallet.FoundationCompat
import           Options.Applicative.Help.Pretty
import           System.Exit
import           System.IO.Unsafe

doc :: String -> Doc
doc = text . toLString

textDoc :: Text -> Doc
textDoc = text . cs

titleDoc :: Doc -> Doc
titleDoc = bold

keyDoc :: Int -> Doc -> Doc
keyDoc = fill

networkDoc :: Doc -> Doc
networkDoc = dullgreen

posAmountDoc :: Doc -> Doc
posAmountDoc = bold . dullgreen

negAmountDoc :: Doc -> Doc
negAmountDoc = bold . dullred

feeDoc :: Doc -> Doc
feeDoc = id

txHashDoc :: Doc -> Doc
txHashDoc = magenta

blockHashDoc :: Doc -> Doc
blockHashDoc = magenta

addressDoc :: Doc -> Doc
addressDoc = bold . dullblue

internalAddressDoc :: Doc -> Doc
internalAddressDoc = bold . black

trueDoc :: Doc -> Doc
trueDoc = bold . dullgreen

falseDoc :: Doc -> Doc
falseDoc = bold . dullred

derivationDoc :: Doc -> Doc
derivationDoc = id

filePathDoc :: Doc -> Doc
filePathDoc = underline . dullwhite

mnemonicDoc :: Doc -> Doc
mnemonicDoc = bold . dullcyan

pubKeyDoc :: Doc -> Doc
pubKeyDoc = dullmagenta

accountDoc :: Doc -> Doc
accountDoc = bold . dullwhite

printDoc :: Doc -> IO ()
printDoc d = putDoc $ d <> hardline

errorDoc :: Doc -> Doc
errorDoc = dullred

exitError :: String -> a
exitError = exitCustomError . errorDoc . doc

exitCustomError :: Doc -> a
exitCustomError err = unsafePerformIO $ putDoc err >> exitFailure

{-
formatOptExample :: String -> Doc
formatOptExample = PText [SetConsoleIntensity BoldIntensity]

formatArgument :: String -> Doc
formatArgument = PText [SetItalicized True]

formatOnline :: String -> Doc
formatOnline =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Green
        ]

formatOffline :: String -> Doc
formatOffline =
    PText
        [ SetConsoleIntensity BoldIntensity
        , SetColor Foreground Dull Red
        ]

-}
