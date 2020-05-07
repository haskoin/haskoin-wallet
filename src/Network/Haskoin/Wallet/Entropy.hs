{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.Entropy where

import           Control.Monad               (when, (>=>))
import           Control.Monad.Except
import           Data.Bits
import qualified Data.ByteString             as BS
import           Data.Either                 (either)
import           Data.List
import           Data.Maybe
import           Data.Text                   (Text)
import           Network.Haskoin.Keys        (Mnemonic, toMnemonic)
import           Network.Haskoin.Util
import           Network.Haskoin.Wallet.Util
import           Numeric                     (readInt)
import           Numeric.Natural
import qualified System.Directory            as D
import           System.Entropy              (getEntropy)
import           System.IO

{- Base 6 decoding for dice entropy -}

b6Data :: String
b6Data = "612345"

b6 :: Int -> Char
b6 = (b6Data !!)

b6' :: Char -> Maybe Int
b6' = (`findIndex` b6Data) . (==)

decodeBase6 :: String -> Maybe BS.ByteString
decodeBase6 str
    | null str = Just mempty
    | otherwise =
        case readInt 6 (isJust . b6') (fromInteger . toInteger . f) str of
            ((i, []):_) -> Just $ integerToBS i
            _ -> Nothing
  where
    f = fromMaybe (error "Could not decode base6") . b6'

-- Mix entropy of same length by xoring them
mixEntropy :: BS.ByteString
           -> BS.ByteString
           -> Either String BS.ByteString
mixEntropy ent1 ent2
    | BS.length ent1 == BS.length ent2 =
        Right $ BS.pack $ BS.zipWith xor ent1 ent2
    | otherwise = Left "Entropy is not of the same length"

diceToEntropy :: Natural -> String -> Either String BS.ByteString
diceToEntropy ent rolls
    | (fromIntegral $ length rolls) /= requiredRolls ent =
        Left $ show (requiredRolls ent) <> " dice rolls are required"
    | otherwise = do
        bytes <- maybeToEither "Could not decode base6" $ decodeBase6 rolls
        case fromIntegral ent `safeSubtract` BS.length bytes of
            Just n -> return $ BS.replicate n 0x00 <> bytes
            -- This should probably never happend
            _      -> Left "Invalid entropy length"

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
    | reqEnt `elem` [16,20 .. 32] = do
        (origEnt, sysEnt) <- liftIO $ systemEntropy reqEnt
        ent <-
            liftEither $
            case rollsM of
                Just rolls -> do
                    diceEnt <- diceToEntropy reqEnt rolls
                    mixEntropy sysEnt diceEnt
                _ -> return sysEnt
        when (BS.length ent /= (fromIntegral reqEnt)) $
            throwError "Something went wrong with the entropy size"
        mnem <- liftEither $ toMnemonic ent
        return (origEnt, mnem)
    | otherwise = throwError "The entropy value can only be in [16,20..32]"

systemEntropy :: Natural -> IO (Text, BS.ByteString)
systemEntropy bytes = do
    exists <- D.doesFileExist "/dev/random"
    if exists
        then ("/dev/random", ) <$> devRandom bytes
        else ("System.Entropy.getEntropy", ) <$> getEntropy (fromIntegral bytes)

devRandom :: Natural -> IO BS.ByteString
devRandom bytes =
    withBinaryFile "/dev/random" ReadMode (`BS.hGet` (fromIntegral bytes))
