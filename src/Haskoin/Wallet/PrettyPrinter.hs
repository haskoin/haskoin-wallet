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

rblock :: Natural -> String -> String
rblock n str =
  case n `safeSubtract` fromIntegral (length str) of
    Just missing -> replicate (fromIntegral missing) ' ' <> str
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
formatTitle =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Vivid White
    ]

formatAccount :: String -> Printer
formatAccount =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull Cyan
    ]

formatFilePath :: String -> Printer
formatFilePath =
  PText
    [ SetItalicized True,
      SetColor Foreground Vivid White
    ]

formatKey :: String -> Printer
formatKey = PText [SetColor Foreground Dull White]

formatValue :: String -> Printer
formatValue = PText [SetColor Foreground Vivid White]

formatLabel :: String -> Printer
formatLabel =
  PText
    [ SetItalicized True,
      SetColor Foreground Vivid White
    ]

formatMnemonic :: Bool -> String -> Printer
formatMnemonic split =
  PText
    [ SetConsoleIntensity BoldIntensity,
      SetColor Foreground Dull (if split then Yellow else Cyan)
    ]

formatAddress :: Bool -> String -> Printer
formatAddress False = PText [SetColor Foreground Dull White]
formatAddress True = PText [SetColor Foreground Vivid White]

formatTxHash :: String -> Printer
formatTxHash = PText [SetColor Foreground Dull White]

formatNoSigTxHash :: String -> Printer
formatNoSigTxHash = PText [SetColor Foreground Vivid Magenta]

formatBlockHash :: String -> Printer
formatBlockHash = PText [SetColor Foreground Dull White]

formatPosAmount :: String -> Printer
formatPosAmount = PText [SetColor Foreground Vivid Green]

formatNegAmount :: String -> Printer
formatNegAmount = PText [SetColor Foreground Vivid Red]

formatZeroAmount :: String -> Printer
formatZeroAmount = PText [SetColor Foreground Vivid White]

formatTrue :: String -> Printer
formatTrue = PText [SetColor Foreground Vivid Green]

formatFalse :: String -> Printer
formatFalse = PText [SetColor Foreground Vivid Red]

formatError :: String -> Printer
formatError = PText [SetColor Foreground Vivid Red]

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

keyPrinter :: Natural -> Text -> Printer
keyPrinter n txt = formatKey (block n (cs txt)) <> text ": "

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

accountPrinter :: DBAccount -> Printer
accountPrinter acc =
  vcat
    [ keyPrinter 8 "Account" <> formatAccount (cs $ dBAccountName acc),
      mconcat [keyPrinter 8 "Wallet", formatValue $ cs fp],
      mconcat [keyPrinter 8 "Deriv", formatValue deriv],
      mconcat [keyPrinter 8 "Network", formatValue net],
      mconcat
        [keyPrinter 8 "External", formatValue $ show $ dBAccountExternal acc],
      mconcat
        [keyPrinter 8 "Internal", formatValue $ show $ dBAccountInternal acc],
      mconcat [keyPrinter 8 "Created", formatValue created],
      formatKey "Balances:",
      nest 2 $
        mconcat
          [ keyPrinter 11 "Confirmed",
            amountPrinter UnitBitcoin $ dBAccountBalanceConfirmed acc
          ],
      nest 2 $
        mconcat
          [ keyPrinter 11 "Unconfirmed",
            amountPrinter UnitBitcoin $ dBAccountBalanceUnconfirmed acc
          ],
      nest 2 $
        mconcat
          [ keyPrinter 11 "Coins",
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
        [ keyPrinter pad $ cs $ show (dBAddressIndex addr),
          formatAddress True $ cs $ dBAddressAddress addr
        ],
      if Text.null $ dBAddressLabel addr
        then PEmpty
        else
          nest 2 $
            mconcat
              [ keyPrinter 8 "Label",
                formatLabel $ cs $ dBAddressLabel addr
              ],
      nest 2 $
        mconcat
          [ keyPrinter 8 "Txs",
            naturalPrinter $ fromIntegral $ dBAddressBalanceTxs addr
          ],
      nest 2 $
        mconcat
          [ keyPrinter 8 "Received",
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
  vcat
    [ formatTitle (block 9 title) <> text ": " <> total,
      custom,
      fee,
      debit,
      credit
    ]
  where
    title =
      case tType of
        TxDebit -> "Debit"
        TxInternal -> "Internal"
        TxCredit -> "Credit"
    total = integerAmountPrinter UnitBitcoin amount
    fee
      | tType `elem` [TxDebit, TxInternal] =
          keyPrinter 9 "Fee" <> feePrinter UnitBitcoin feeN feeByteN
      | otherwise = mempty
    debit
      | tType /= TxDebit = mempty
      | otherwise =
          vcat $
            [formatKey "Paid to addresses:"]
              <> (nest 2 . addrPrinter False <$> Map.assocs otherOutputs)
    credit
      | tType /= TxCredit = mempty
      | otherwise =
          vcat $
            [formatKey "My credited addresses:"]
              <> ( nest 2 . addrPrinter True
                     <$> Map.assocs (Map.map myOutputsValue myOutputs)
                 )
    addrPrinter isCredit (a, v) =
      formatAddress isCredit (parseAddr net a)
        <> text ":"
          <+> if isCredit
            then
              integerAmountPrinterWith
                formatPosAmount
                UnitBitcoin
                (fromIntegral v)
            else
              integerAmountPrinterWith
                formatZeroAmount
                UnitBitcoin
                (fromIntegral v)

parseAddr :: Network -> Address -> String
parseAddr net = cs . fromMaybe "Invalid Address" . addrToText net

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
          [ keyPrinter 9 "TxHash" <> formatTxHash (cs $ txHashToHex txInfoHash),
            keyPrinter 9 "NoSigHash"
              <> formatNoSigTxHash (cs $ txHashToHex nosigH),
            keyPrinter 9 "Signed"
              <> text "This pending transaction is"
                <+> formatTrue "signed",
            keyPrinter 9 "Confs" <> formatValue (show txInfoConfirmations)
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
          [ keyPrinter 9 "NoSigHash"
              <> formatNoSigTxHash (cs $ txHashToHex nosigH),
            keyPrinter 9 "Signed"
              <> text "This pending transaction is"
                <+> formatFalse "not signed"
          ]

feePrinter :: AmountUnit -> Natural -> Natural -> Printer
feePrinter unit fee feeBytes =
  integerAmountPrinterWith formatZeroAmount unit (fromIntegral fee)
    <+> text "("
    <> formatValue (show feeBytes)
      <+> text "sat/bytes"
    <> text ")"

coinPrinter :: Network -> JsonCoin -> Printer
coinPrinter net JsonCoin {..} =
  vcat
    [ formatKey (block 7 "TxHash")
        <> text ":"
          <+> formatTxHash (cs $ txHashToHex jsonCoinOutpoint.hash),
      formatKey (block 7 "Index")
        <> text ":"
          <+> formatValue (show jsonCoinOutpoint.index),
      formatKey (block 7 "Value")
        <> text ":"
          <+> amountPrinter UnitBitcoin jsonCoinValue,
      formatKey (block 7 "Address")
        <> text ":"
          <+> formatAddress True (parseAddr net jsonCoinAddress),
      formatKey (block 7 "Confs")
        <> text ":"
          <+> formatValue (show jsonCoinConfirmations),
      formatKey (block 7 "Status")
        <> text ":"
          <+> if jsonCoinLocked
            then formatFalse "Locked"
            else formatTrue "Free"
    ]

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
    ResponseAccount acc -> renderIO $ accountPrinter acc
    ResponseAccounts [] -> renderIO $ text "There are no accounts in the wallet"
    ResponseAccounts accs ->
      renderIO . vcat $ intersperse (text " ") $ accountPrinter <$> accs
    ResponseTestAcc _ res txt -> do
      let p =
            if res
              then formatTrue "Success"
              else formatFalse "Failure"
      renderIO . vcat $ [formatKey "Result:" <+> p, text $ cs txt]
    ResponseFile f -> renderIO $ formatKey "File:" <+> formatFilePath f
    ResponseAddress _ addr -> do
      let l = length $ show $ dBAddressIndex addr
      renderIO $ addressPrinter (fromIntegral l) addr
    ResponseAddresses _ [] ->
      renderIO $ text "There are no addresses in the account"
    ResponseAddresses _ addrs -> do
      let l = length $ show $ maximum $ dBAddressIndex <$> addrs
      renderIO $ vcat $ addressPrinter (fromIntegral l) <$> addrs
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
                [ keyPrinter 9 "TxHash"
                    <> formatTxHash (cs $ txHashToHex txInfoHash),
                  keyPrinter 9 "Confs" <> formatValue (show txInfoConfirmations)
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
    ResponseCoins acc coins -> do
      let net = accountNetwork acc
      renderIO $ vcat $ intersperse (text " ") $ coinPrinter net <$> coins
    ResponseSync acc bh h t c -> do
      renderIO $
        vcat
          [ accountPrinter acc,
            formatTitle "Sync Results:",
            nest 2 $
              keyPrinter 12 "Best Block"
                <> formatBlockHash (cs $ blockHashToHex bh),
            nest 2 $ keyPrinter 12 "Best Height" <> formatValue (show h),
            nest 2 $ keyPrinter 12 "Tx updates" <> formatValue (show t),
            nest 2 $ keyPrinter 12 "Coin updates" <> formatValue (show c)
          ]
    ResponseVersion v ->
      renderIO $ formatKey "Version:" <+> formatValue (cs v)
    ResponseRollDice ds e ->
      renderIO . vcat $
        [ formatKey "System Entropy Source:" <+> formatFilePath (cs e),
          formatKey "Dice rolls:" <+> formatValue (show ds)
        ]
