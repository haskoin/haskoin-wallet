{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Haskoin.Wallet.PrettyPrinter where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO (..), MonadTrans (lift))
import Data.Aeson (object, (.:), (.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Foldable (for_)
import Data.List (intersperse, nub, sort, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import Database.Persist.Sqlite (runMigrationQuiet, runSqlite, transactionUndo)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebClient
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Commands
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Entropy
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Parser
import Haskoin.Wallet.Signing
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import System.Console.ANSI
import qualified System.Console.Haskeline as Haskeline
import qualified System.Directory as D
import System.Exit
import System.IO
import System.Random (initStdGen)
import Data.Time.Format

data Printer
  = PConcat !Printer !Printer
  | PNewline !Printer
  | PNest !Natural !Printer
  | PText [SGR] !String
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
    go [] = PEmpty
    go [x] = x
    go (x : xs) = x <> PNewline (go xs)

hsep :: [Printer] -> Printer
hsep = go . filter (not . isEmptyPrinter)
  where
    go [] = PEmpty
    go [x] = x
    go (x : xs) = x <+> go xs

nest :: Natural -> Printer -> Printer
nest = PNest

block :: Natural -> String -> String
block n str =
  case n `safeSubtract` fromIntegral (length str) of
    Just missing -> str <> replicate (fromIntegral missing) ' '
    _ -> str

renderIO :: Printer -> IO ()
renderIO cp = go 0 0 cp >> putStrLn ""
  where
    go :: Natural -> Natural -> Printer -> IO Natural
    go l n p
      | isEmptyPrinter p = return l
      | otherwise =
          case p of
            PConcat p1 p2 -> do
              l2 <- go l n p1
              go l2 n p2
            PNewline p1 -> do
              putStrLn ""
              putStr $ replicate (fromIntegral n) ' '
              go n n p1
            PNest i p1 -> do
              putStr $ replicate (fromIntegral i) ' '
              go (l + i) (n + i) p1
            PText xs s -> do
              printFormat xs s
              return $ l + fromIntegral (length s)
            PEmpty -> return l
            PNonEmpty -> return l

formatTitle :: String -> Printer
formatTitle = PText [SetConsoleIntensity BoldIntensity]

formatCommand :: String -> Printer
formatCommand =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Blue
    ]

formatOption :: String -> Printer
formatOption =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Magenta
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
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Cyan
    ]

formatPubKey :: String -> Printer
formatPubKey = PText [SetColor Foreground Dull Magenta]

formatFilePath :: String -> Printer
formatFilePath =
  PText
    [ SetItalicized True,
      SetColor Foreground Dull White
    ]

formatKey :: String -> Printer
formatKey = PText []

formatValue :: String -> Printer
formatValue =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull White
    ]

formatDeriv :: String -> Printer
formatDeriv = PText []

formatMnemonic :: Bool -> String -> Printer
formatMnemonic split =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull (if split then Yellow else Cyan)
    ]

formatAddress :: String -> Printer
formatAddress =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Blue
    ]

formatInternalAddress :: String -> Printer
formatInternalAddress =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Vivid Black
    ]

formatTxHash :: String -> Printer
formatTxHash = PText [SetColor Foreground Vivid Magenta]

formatBlockHash :: String -> Printer
formatBlockHash = PText [SetColor Foreground Vivid Magenta]

formatPosAmount :: String -> Printer
formatPosAmount =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Green
    ]

formatNegAmount :: String -> Printer
formatNegAmount =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Red
    ]

formatFee :: String -> Printer
formatFee = PText []

formatTrue :: String -> Printer
formatTrue =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Green
    ]

formatFalse :: String -> Printer
formatFalse =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Red
    ]

formatOnline :: String -> Printer
formatOnline =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Green
    ]

formatOffline :: String -> Printer
formatOffline =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Red
    ]

formatError :: String -> Printer
formatError = PText [SetColor Foreground Dull Red]

printFormat :: [SGR] -> String -> IO ()
printFormat sgr str = do
  support <- hSupportsANSI stdout
  when support $ setSGR sgr
  putStr str
  when support $ setSGR []

{- Pretty Response -}

mnemonicPrinter :: Bool -> [Text] -> Printer
mnemonicPrinter split ws =
  vcat $
    fmap (mconcat . fmap formatWord) $
      chunksOf 4 $
        zip ([1 ..] :: [Natural]) ws
  where
    formatWord (i, w) =
      mconcat
        [ formatKey $ block 4 $ show i <> ".",
          formatMnemonic split $ block 10 $ cs w
        ]

partsPrinter :: [[Text]] -> [Printer]
partsPrinter [] = mempty
partsPrinter xs =
  concatMap formatPart $ zip ([1 ..] :: [Natural]) xs
  where
    formatPart (i, ws) =
      [ formatTitle $ "Private Mnemonic Part #" <> show i,
        nest 2 $ mnemonicPrinter True ws
      ]

formatFeeBytes :: Natural -> Printer
formatFeeBytes fee = formatFee (show fee) <+> text "sat/bytes"

formatAmount :: AmountUnit -> Word64 -> Printer
formatAmount unit = formatIntegerAmount unit . fromIntegral

formatIntegerAmount :: AmountUnit -> Integer -> Printer
formatIntegerAmount unit amnt
  | amnt >= 0 = formatIntegerAmountWith formatPosAmount unit amnt
  | otherwise = formatIntegerAmountWith formatNegAmount unit amnt

formatIntegerAmountWith ::
  (String -> Printer) -> AmountUnit -> Integer -> Printer
formatIntegerAmountWith f unit amnt =
  f (cs $ showIntegerAmount unit amnt) <+> formatUnit unit amnt

formatUnit :: AmountUnit -> Integer -> Printer
formatUnit unit = text . cs . showUnit unit

accountsPrinter :: DBAccount -> Printer
accountsPrinter acc =
  vcat
    [ formatTitle "Account " <> formatAccount (cs $ dBAccountName acc),
      nest 2 $ mconcat [formatKey (block 10 "Wallet: "), formatValue $ cs fp],
      nest 2 $ mconcat [formatKey (block 10 "Deriv: "), formatValue deriv],
      nest 2 $ mconcat [formatKey (block 10 "Network: "), formatValue net],
      nest 2 $
        mconcat
          [ formatKey (block 10 "External: "),
            formatValue $ show $ dBAccountExternal acc
          ],
      nest 2 $
        mconcat
          [ formatKey (block 10 "Internal: "),
            formatValue $ show $ dBAccountInternal acc
          ],
      nest 2 $
        mconcat
          [ formatKey (block 10 "Created: "),
            formatValue created
          ],
      nest 2 $ formatKey (block 10 "Balance:"),
      nest 4 $
        mconcat
          [ formatKey (block 13 "Confirmed: "),
            formatAmount UnitBitcoin $ dBAccountBalanceConfirmed acc
          ],
      nest 4 $
        mconcat
          [ formatKey (block 13 "Unconfirmed: "),
            formatAmount UnitBitcoin $ dBAccountBalanceUnconfirmed acc
          ],
      nest 4 $
        mconcat
          [ formatKey (block 13 "Coins: "),
            formatValue $ show $ dBAccountBalanceCoins acc
          ]
    ]
  where
    DBWalletKey fp = dBAccountWallet acc
    deriv = cs $ dBAccountDerivation acc
    net = (accountNetwork acc).name
    utctime = dBAccountCreated acc
    created = formatTime defaultTimeLocale "%d %b %Y" utctime

prettyPrinter :: Ctx -> Response -> IO ()
prettyPrinter ctx =
  \case
    ResponseError err -> do
      renderIO . mconcat $
        [formatError "Error: ", formatStatic $ cs err]
      exitFailure
    ResponseMnemonic orig mnem parts ->
      renderIO . vcat $
        [ formatTitle "System Entropy Source",
          nest 2 $ formatFilePath $ cs orig,
          formatTitle "Private Mnemonic",
          nest 2 $ mnemonicPrinter False mnem
        ]
          <> partsPrinter parts
    ResponseAccount acc -> renderIO $ accountsPrinter acc
    ResponseAccounts accs ->
      renderIO . vcat $ intersperse (text "-") $ accountsPrinter <$> accs
    ResponseTestAcc _ res txt -> do
      let p = if res then formatTrue "Success" else formatFalse "Failure"
      renderIO . vcat $
        [ formatKey "Result:" <+> p
        , text $ cs txt
        ]
    ResponseFile f -> renderIO $ formatKey "File:" <+> formatFilePath f
