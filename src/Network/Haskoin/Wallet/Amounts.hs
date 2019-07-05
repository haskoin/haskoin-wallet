{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Network.Haskoin.Wallet.Amounts where

import           Control.Arrow                   (second)
import           Control.Monad
import           Data.Decimal
import           Data.Maybe
import           Data.String.Conversions         (cs)
import           Data.Text                       as Text
import           Data.Text.Read                  as Read
import           Numeric.Natural
import           Network.Haskoin.Wallet.Doc
import           Options.Applicative.Help.Pretty
import           Prelude                         as Prelude

type Satoshi = Natural

data AmountUnit
    = UnitBitcoin
    | UnitBit
    | UnitSatoshi
    deriving (Eq)

{- Printer functions -}

amountDoc :: AmountUnit -> Satoshi -> Doc
amountDoc unit = integerAmountDoc unit . fromIntegral

integerAmountDoc :: AmountUnit -> Integer -> Doc
integerAmountDoc unit amnt
    | amnt >= 0 = integerAmountWithDoc posAmountDoc unit amnt
    | otherwise = integerAmountWithDoc negAmountDoc unit amnt

integerAmountWithDoc :: (Doc -> Doc) -> AmountUnit -> Integer -> Doc
integerAmountWithDoc f unit amnt =
    f (text $ cs $ showIntegerAmount unit amnt) <+> unitDoc unit amnt

unitDoc :: AmountUnit -> Integer -> Doc
unitDoc unit = text . unpack . showUnit unit

feeBytesDoc :: Decimal -> Doc
feeBytesDoc fee = feeDoc (text $ show fee) <+> text "sat/bytes"

{- Amount Parsing -}

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

showAmount :: AmountUnit -> Satoshi -> Text
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

readAmount :: AmountUnit -> Text -> Maybe Satoshi
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

readNatural :: Text -> Maybe Satoshi
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

{- Utilities -}

padStart :: Int -> Text -> Text -> Text
padStart n c t =
    Text.replicate (fromMaybe 0 $ n `safeSubtract` Text.length t) c <> t

padEnd :: Int -> Text -> Text -> Text
padEnd n c t
    = t <> Text.replicate (fromMaybe 0 $ n `safeSubtract` Text.length t) c

dropPatternEnd :: Text -> Text -> Text
dropPatternEnd p t = fromMaybe t $ stripSuffix p t

chunksOfEnd :: Int -> Text -> [Text]
chunksOfEnd n t = Prelude.reverse $ Text.reverse <$> chunksOf n (Text.reverse t)

safeSubtract :: Integral a => a -> a -> Maybe a
safeSubtract a b
    | b > a = Nothing
    | otherwise = Just $ a - b

