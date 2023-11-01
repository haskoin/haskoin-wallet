{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Time.Format
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

formatIndex :: String -> Printer
formatIndex =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull White
    ]

formatValue :: String -> Printer
formatValue = PText []

formatDeriv :: String -> Printer
formatDeriv = PText []

formatLabel :: String -> Printer
formatLabel = PText []

formatMnemonic :: Bool -> String -> Printer
formatMnemonic split =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull (if split then Yellow else Cyan)
    ]

formatAddress :: Bool -> String -> Printer
formatAddress True = PText []
formatAddress False =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Cyan
    ]

formatTxHash :: String -> Printer
formatTxHash = PText [SetColor Foreground Vivid Magenta]

formatNoSigTxHash :: String -> Printer
formatNoSigTxHash = PText [SetColor Foreground Dull Yellow]

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

formatZeroAmount :: String -> Printer
formatZeroAmount = PText []

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

amountPrinter :: AmountUnit -> Word64 -> Printer
amountPrinter unit = integerAmountPrinter unit . fromIntegral

integerAmountPrinter :: AmountUnit -> Integer -> Printer
integerAmountPrinter unit amnt
  | amnt == 0 = integerAmountPrinterWith formatZeroAmount unit amnt
  | amnt > 0 = integerAmountPrinterWith formatPosAmount unit amnt
  | otherwise = integerAmountPrinterWith formatNegAmount unit amnt

integerAmountPrinterWith ::
  (String -> Printer) -> AmountUnit -> Integer -> Printer
integerAmountPrinterWith f unit amnt =
  f (cs $ showIntegerAmount unit amnt) <+> unitPrinter unit amnt

unitPrinter :: AmountUnit -> Integer -> Printer
unitPrinter unit = text . cs . showUnit unit

naturalPrinter :: Natural -> Printer
naturalPrinter nat
  | nat == 0 = formatZeroAmount $ show nat
  | otherwise = formatPosAmount $ show nat

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
      nest 2 $ formatKey "Balances:",
      nest 4 $
        mconcat
          [ formatKey (block 13 "Confirmed: "),
            amountPrinter UnitBitcoin $ dBAccountBalanceConfirmed acc
          ],
      nest 4 $
        mconcat
          [ formatKey (block 13 "Unconfirmed: "),
            amountPrinter UnitBitcoin $ dBAccountBalanceUnconfirmed acc
          ],
      nest 4 $
        mconcat
          [ formatKey (block 13 "Coins: "),
            naturalPrinter $ fromIntegral $ dBAccountBalanceCoins acc
          ]
    ]
  where
    DBWalletKey fp = dBAccountWallet acc
    deriv = cs $ dBAccountDerivation acc
    net = (accountNetwork acc).name
    utctime = dBAccountCreated acc
    created = formatTime defaultTimeLocale "%d %b %Y" utctime

addressPrinter :: Natural -> DBAddress -> Printer
addressPrinter pad addr =
  vcat
    [ mconcat
        [ formatIndex $ block pad $ show (dBAddressIndex addr) <> ":",
          formatAddress
            (dBAddressBalanceTxs addr > 0)
            $ cs
            $ dBAddressAddress addr
        ],
      if Text.null $ dBAddressLabel addr
        then PEmpty
        else
          nest 2 $
            mconcat
              [ formatKey (block 10 "Label: "),
                formatLabel $ cs $ dBAddressLabel addr
              ],
      nest 2 $
        mconcat
          [ formatKey (block 10 "Txs: "),
            naturalPrinter $ fromIntegral $ dBAddressBalanceTxs addr
          ],
      nest 2 $
        mconcat
          [ formatKey (block 10 "Received: "),
            amountPrinter UnitBitcoin $ dBAddressBalanceReceived addr
          ]
    ]

txInfoPrinter ::
  Network ->
  TxType ->
  Integer ->
  Natural ->
  Natural ->
  Map Address MyOutputs ->
  Map Address Natural ->
  Printer ->
  Printer
txInfoPrinter net tType amount feeN feeByteN myOutputs otherOutputs custom =
  vcat [title, nest 2 $ vcat [custom, fee, debit, credit]]
  where
    title =
      case tType of
        TxDebit -> formatTitle "Debit" <+> total
        TxInternal -> formatTitle "Internal"
        TxCredit -> formatTitle "Credit" <+> total
    total = integerAmountPrinter UnitBitcoin amount
    debit
      | tType /= TxDebit = mempty
      | otherwise =
          vcat $ fmap (addrFormat True) (Map.assocs otherOutputs)
    credit
      | tType /= TxCredit = mempty
      | otherwise =
          vcat $
            fmap
              (addrFormat False)
              (Map.assocs $ Map.map myOutputsValue myOutputs)
    fee
      | tType `elem` [TxDebit, TxInternal] =
          formatKey "Fee:" <+> feePrinter UnitBitcoin feeN feeByteN
      | otherwise = mempty
    addrFormat isDebit (a, v) =
      formatAddress True (parseAddr a)
        <> text ":"
          <+> if isDebit
            then
              integerAmountPrinterWith
                formatNegAmount
                UnitBitcoin
                (fromIntegral v)
            else
              integerAmountPrinterWith
                formatPosAmount
                UnitBitcoin
                (fromIntegral v)
    parseAddr = cs . fromMaybe "Invalid Address" . addrToText net

noSigTxInfoPrinter :: Network -> NoSigTxInfo -> Printer
noSigTxInfoPrinter net ns =
  case ns of
    NoSigSigned nosigH TxInfo {..} ->
      txInfoPrinter
        net
        txInfoType
        txInfoAmount
        txInfoFee
        txInfoFeeByte
        txInfoMyOutputs
        txInfoOtherOutputs
        $ vcat
          [ formatKey (block 11 "TxHash:")
              <+> formatTxHash (cs $ txHashToHex txInfoHash),
            formatKey (block 11 "NoSigHash:")
              <+> formatNoSigTxHash (cs $ txHashToHex nosigH),
            text "This pending transaction is" <+> formatFalse "not signed",
            formatKey "Confirmations:" <+> text (show txInfoConfirmations)
          ]
    NoSigUnsigned nosigH UnsignedTxInfo {..} ->
      txInfoPrinter
        net
        unsignedTxInfoType
        unsignedTxInfoAmount
        unsignedTxInfoFee
        unsignedTxInfoFeeByte
        unsignedTxInfoMyOutputs
        unsignedTxInfoOtherOutputs
        $ vcat
          [ formatKey "NoSigHash:" <+> formatNoSigTxHash (cs $ txHashToHex nosigH),
            text "This pending transaction is" <+> formatFalse "not signed"
          ]

feePrinter :: AmountUnit -> Natural -> Natural -> Printer
feePrinter unit fee feeBytes =
  integerAmountPrinterWith formatZeroAmount unit (fromIntegral fee)
    <+> text "("
    <> text (show feeBytes)
      <+> text "sat/bytes"
    <> text ")"

prettyPrinter :: Ctx -> Response -> IO ()
prettyPrinter ctx =
  \case
    ResponseError err -> do
      renderIO . mconcat $ [formatError "Error: ", text $ cs err]
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
    ResponseAccounts [] -> renderIO $ text "There are no accounts in the wallet"
    ResponseAccounts accs ->
      renderIO . vcat $ intersperse (text " ") $ accountsPrinter <$> accs
    ResponseTestAcc _ res txt -> do
      let p =
            if res
              then formatTrue "Success"
              else formatFalse "Failure"
      renderIO . vcat $ [formatKey "Result:" <+> p, text $ cs txt]
    ResponseFile f -> renderIO $ formatKey "File:" <+> formatFilePath f
    ResponseAddress _ addr -> do
      let l = length $ show $ dBAddressIndex addr
      renderIO $ addressPrinter (fromIntegral l + 2) addr
    ResponseAddresses _ [] ->
      renderIO $ text "There are no addresses in the account"
    ResponseAddresses _ addrs -> do
      let l = length $ show $ maximum $ dBAddressIndex <$> addrs
      renderIO $ vcat $ addressPrinter (fromIntegral l + 2) <$> addrs
    ResponseTxs _ [] ->
      renderIO $ text "There are no transactions in the account"
    ResponseTxs acc txs -> do
      let net = accountNetwork acc
          f TxInfo {..} =
            txInfoPrinter
              net
              txInfoType
              txInfoAmount
              txInfoFee
              txInfoFeeByte
              txInfoMyOutputs
              txInfoOtherOutputs
              $ vcat
                [ formatTxHash (cs $ txHashToHex txInfoHash),
                  formatKey "Confirmations:" <+> text (show txInfoConfirmations)
                ]
      renderIO $ vcat $ intersperse (text " ") $ f <$> txs
    ResponseTxInfo acc txInfo -> do
      let net = accountNetwork acc
      renderIO $ noSigTxInfoPrinter net txInfo
    ResponseTxInfos _ [] ->
      renderIO $ text "There are no pending transactions in the account"
    ResponseTxInfos acc txInfos -> do
      let net = accountNetwork acc
      renderIO $
        vcat $
          intersperse (text " ") $
            noSigTxInfoPrinter net <$> txInfos
    ResponseDeleteTx h c a ->
      renderIO $
        vcat
          [ formatKey "Deleted:" <+> formatNoSigTxHash (cs $ txHashToHex h),
            nest 2 $ formatKey "Freed coins:" <+> formatValue (show c),
            nest 2 $
              formatKey "Freed internal addresses:" <+> formatValue (show a)
          ]
