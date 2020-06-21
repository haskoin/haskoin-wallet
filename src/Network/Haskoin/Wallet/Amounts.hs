{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Amounts where

import           Control.Arrow                   (second)
import           Control.Monad
import           Data.Decimal
import           Data.String.Conversions         (cs)
import           Data.Text                       as Text
import           Data.Text.Read                  as Read
import           Network.Haskoin.Wallet.Util
import           Numeric.Natural
import           Options.Applicative.Help.Pretty

data AmountUnit
    = UnitBitcoin
    | UnitBit
    | UnitSatoshi
    deriving (Eq, Show)

-- Amount Parsing --

showUnit :: AmountUnit -> Integer -> Text
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

showAmount :: AmountUnit -> Natural -> Text
showAmount unit amnt =
    case unit of
        UnitBitcoin ->
            let (q, r) = amnt `divMod` 100000000
            in addSep (showT q) <> "." <> removeEnd (padStart 8 "0" (showT r))
        UnitBit ->
            let (q, r) = amnt `divMod` 100
            in addSep (showT q) <> "." <> padStart 2 "0" (showT r)
        UnitSatoshi -> addSep (showT amnt)
  where
    removeEnd = dropPatternEnd "0000" . dropPatternEnd "000000"
    addSep = Text.intercalate "'" . chunksOfEnd 3
    showT = pack . show

readAmount :: AmountUnit -> Text -> Maybe Natural
readAmount unit amntStr =
    case unit of
        UnitBitcoin -> do
            guard $ Text.length r <= 8
            a <- readNatural q
            b <- readNatural $ padEnd 8 "0" r
            return $ a * 100000000 + b
        UnitBit -> do
            guard $ Text.length r <= 2
            a <- readNatural q
            b <- readNatural $ padEnd 2 "0" r
            return $ a * 100 + b
        UnitSatoshi -> readNatural str
  where
    str = dropAmountSep amntStr
    (q, r) = second (Text.drop 1) $ breakOn "." str

readNatural :: Text -> Maybe Natural
readNatural txt =
    case Read.decimal txt of
        Right (res, "") -> Just res
        _               -> Nothing

dropAmountSep :: Text -> Text
dropAmountSep = Text.filter (`notElem` [' ', '_', '\''])

-- | Like 'showAmount' but will display a minus sign for negative amounts
showIntegerAmount :: AmountUnit -> Integer -> Text
showIntegerAmount unit i
    | i < 0 = "-" <> showAmount unit (fromIntegral $ abs i)
    | otherwise = showAmount unit $ fromIntegral i

-- | Like 'readAmount' but can parse a negative amount
readIntegerAmount :: AmountUnit -> Text -> Maybe Integer
readIntegerAmount unit txt =
    case uncons txt of
        Just ('-', rest) -> negate . toInteger <$> readAmount unit rest
        _                -> toInteger <$> readAmount unit txt

