{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.Haskoin.Wallet.Entropy where

import Control.Monad (when, (>=>))
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Bits
import qualified Data.ByteString as BS
import Data.Either (either)
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Word (Word8)
import Haskoin.Crypto (Mnemonic, toMnemonic)
import Haskoin.Util
import Network.Haskoin.Wallet.Util
import Numeric (readInt, showIntAtBase)
import Numeric.Natural
import qualified System.Directory as D
import System.Entropy (getEntropy)
import System.IO

-- Base 6 decoding for dice entropy --

b6Data :: String
b6Data = "612345"

b6 :: Int -> Char
b6 = (b6Data !!)

b6' :: Char -> Maybe Int
b6' = (`findIndex` b6Data) . (==)

-- Does not preserve leading 0s
decodeBase6 :: String -> Maybe BS.ByteString
decodeBase6 str
  | null str = Just mempty
  | otherwise =
      case listToMaybe $ readInt 6 (isJust . b6') f str of
        Just (i, []) -> Just $ integerToBS i
        _ -> Nothing
  where
    f = fromMaybe (error "Could not decode base6") . b6'

-- Does not preserve leading 0s
encodeBase6 :: BS.ByteString -> String
encodeBase6 bs
  | BS.null bs = mempty
  | otherwise = showIntAtBase 6 b6 (bsToInteger bs) ""

-- This produces uniformily distributed dice rolls
-- 0,   0x00 -> [6,6,6]
-- 1,   0x01 -> [6,6,1]
-- 215, 0xd7 -> [5,5,5]
-- 216, 0xd8 -> [6,6]
-- 217, 0xd9 -> [6,1]
-- 251, 0xfb -> [5,5]
-- Anything above 0xfb is dropped
char8ToBase6 :: Word8 -> [Natural]
char8ToBase6 w
  | w > 0xfb = [] -- Drop otherwise we don't have uniform distribution
  | w > 0xd7 = 
      let res = f <$> encodeBase6 (BS.pack [w-0xd8])
       in replicate (2 - length res) 6 <> res
  | otherwise =
      let res = f <$> encodeBase6 (BS.pack [w])
       in replicate (3 - length res) 6 <> res
  where
    f :: Char -> Natural
    f = read . (: [])

-- Mix entropy of same length by xoring them
mixEntropy ::
  BS.ByteString ->
  BS.ByteString ->
  Either String BS.ByteString
mixEntropy ent1 ent2
  | BS.length ent1 == BS.length ent2 =
      Right $ BS.pack $ BS.zipWith xor ent1 ent2
  | otherwise = Left "Entropy is not of the same length"

diceToEntropy :: Natural -> String -> Either String BS.ByteString
diceToEntropy ent rolls
  | fromIntegral (length rolls) /= requiredRolls ent =
      Left $ show (requiredRolls ent) <> " dice rolls are required"
  | otherwise = do
      bytes <- maybeToEither "Could not decode base6" $ decodeBase6 rolls
      case fromIntegral ent `safeSubtract` BS.length bytes of
        Just n -> return $ BS.replicate n 0x00 <> bytes
        -- This should probably never happend
        _ -> Left "Invalid entropy length"

-- The number of dice rolls required to reach a given amount of entropy
-- Example: 32 bytes of entropy require 99 dice rolls (255.9 bits)
requiredRolls :: Natural -> Natural
requiredRolls ent = floor $ fromIntegral ent * log2o6
  where
    log2o6 = 3.09482245788 :: Double -- 8 * log 2 / log 6

genMnemonic :: Natural -> ExceptT String IO (Text, Mnemonic)
genMnemonic reqEnt = genMnemonicGen reqEnt Nothing

genMnemonicDice :: Natural -> String -> ExceptT String IO (Text, Mnemonic)
genMnemonicDice reqEnt rolls = genMnemonicGen reqEnt (Just rolls)

genMnemonicGen :: Natural -> Maybe String -> ExceptT String IO (Text, Mnemonic)
genMnemonicGen reqEnt rollsM
  | reqEnt `elem` [16, 20 .. 32] = do
      (origEnt, sysEnt) <- liftIO $ systemEntropy reqEnt
      ent <-
        liftEither $
          case rollsM of
            Just rolls -> do
              diceEnt <- diceToEntropy reqEnt rolls
              mixEntropy sysEnt diceEnt
            _ -> return sysEnt
      when (BS.length ent /= fromIntegral reqEnt) $
        throwError "Something went wrong with the entropy size"
      mnem <- liftEither $ toMnemonic ent
      return (origEnt, mnem)
  | otherwise = throwError "The entropy value can only be in [16,20..32]"

systemEntropy :: Natural -> IO (Text, BS.ByteString)
systemEntropy bytes = do
  exists <- D.doesFileExist "/dev/random"
  if exists
    then ("/dev/random",) <$> devRandom bytes
    else ("System.Entropy.getEntropy",) <$> getEntropy (fromIntegral bytes)

devRandom :: Natural -> IO BS.ByteString
devRandom bytes =
  withBinaryFile "/dev/random" ReadMode (`BS.hGet` (fromIntegral bytes))
