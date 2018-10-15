{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Amounts where

import           Control.Monad
import           Data.Decimal
import           Foundation
import           Foundation.String.Read
import           Network.Haskoin.Wallet.Printer
import           Network.Haskoin.Wallet.FoundationCompat

type Satoshi = Natural

data AmountUnit
    = UnitBitcoin
    | UnitBit
    | UnitSatoshi
    deriving (Eq)

{- Printer functions -}

formatFeeBytes :: Decimal -> Printer
formatFeeBytes fee = formatFee (show fee) <+> text "sat/bytes"

formatAmount :: AmountUnit -> Satoshi -> Printer
formatAmount unit = formatIntegerAmount unit . fromIntegral

formatIntegerAmount :: AmountUnit -> Integer -> Printer
formatIntegerAmount unit amnt
    | amnt >= 0 = formatIntegerAmountWith formatPosAmount unit amnt
    | otherwise = formatIntegerAmountWith formatNegAmount unit amnt

formatIntegerAmountWith ::
       (String -> Printer) -> AmountUnit -> Integer -> Printer
formatIntegerAmountWith f unit amnt =
    f (showIntegerAmount unit amnt) <+> formatUnit unit amnt

formatUnit :: AmountUnit -> Integer -> Printer
formatUnit unit = text . showUnit unit

showUnit :: AmountUnit -> Integer -> String
showUnit unit amnt
    | unit == UnitSatoshi = strUnit -- satoshi is always singular
    | abs amnt == 1 = strUnit
    | otherwise = strUnit <> "s" -- plural form bitcoins and bits
  where
    strUnit =
        case unit of
            UnitBitcoin -> "bitcoin"
            UnitBit     -> "bit"
            UnitSatoshi -> "satoshi"

{- Amount Parsing -}

showAmount :: AmountUnit -> Satoshi -> String
showAmount unit amnt =
    case unit of
        UnitBitcoin ->
            let (q, r) = amnt `divMod` 100000000
            in addSep (show q) <> "." <>
               stripEnd (padStart 8 '0' (show r))
        UnitBit ->
            let (q, r) = amnt `divMod` 100
            in addSep (show q) <> "." <> padStart 2 '0' (show r)
        UnitSatoshi -> addSep (show amnt)
  where
    stripEnd = dropPatternEnd "0000" . dropPatternEnd "000000"
    addSep = intercalate "'" . groupEnd 3

readAmount :: AmountUnit -> String -> Maybe Satoshi
readAmount unit amntStr =
    case unit of
        UnitBitcoin -> do
            guard $ length r <= 8
            a <- readNatural q
            b <- readNatural $ padEnd 8 '0' r
            return $ a * 100000000 + b
        UnitBit -> do
            guard $ length r <= 2
            a <- readNatural q
            b <- readNatural $ padEnd 2 '0' r
            return $ a * 100 + b
        UnitSatoshi -> readNatural str
  where
    str = dropAmountSep amntStr
    (q, r) = second (drop 1) $ breakElem '.' str

dropAmountSep :: String -> String
dropAmountSep = filter (`notElem` [' ', '_', '\''])

-- | Like 'showAmount' but will display a minus sign for negative amounts
showIntegerAmount :: AmountUnit -> Integer -> String
showIntegerAmount unit i
    | i < 0 = "-" <> showAmount unit (fromIntegral $ abs i)
    | otherwise = showAmount unit $ fromIntegral i


-- | Like 'readAmount' but can parse a negative amount
readIntegerAmount :: AmountUnit -> String -> Maybe Integer
readIntegerAmount unit str =
    case uncons str of
        Just ('-', rest) -> negate . toInteger <$> readAmount unit rest
        _                -> toInteger <$> readAmount unit str

