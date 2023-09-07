{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.Haskoin.Wallet.Entropy where

import Control.Monad (when, (>=>))
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Bifoldable (bifoldl1)
import Data.Bits
import qualified Data.ByteString as BS
import Data.Either (either)
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Word (Word8)
import Haskoin.Crypto (Mnemonic, toMnemonic, xPrvIsHard)
import Haskoin.Util
import Network.Haskoin.Wallet.Util
import Numeric (readInt, showIntAtBase)
import Numeric.Natural
import qualified System.Console.Haskeline as Haskeline
import qualified System.Console.Haskeline as Haskline
import qualified System.Directory as D
import System.Entropy (getEntropy)
import System.IO
import Text.Read (readMaybe)

-- Produces uniformily distributed dice rolls from a random byte
-- 0,   0x00 -> [6,6,6]
-- 1,   0x01 -> [6,6,1]
-- 215, 0xd7 -> [5,5,5]
-- 216, 0xd8 -> [6,6]
-- 217, 0xd9 -> [6,1]
-- 251, 0xfb -> [5,5]
-- Anything above 0xfb is dropped
word8ToBase6 :: Word8 -> [Natural]
word8ToBase6 w
  | w > 0xfb = [] -- Drop otherwise we don't have uniform distribution
  | w > 0xd7 =
      let res = f <$> encodeBase6 (BS.pack [w - 0xd8])
       in replicate (2 - length res) 6 <> res
  | otherwise =
      let res = f <$> encodeBase6 (BS.pack [w])
       in replicate (3 - length res) 6 <> res
  where
    f :: Char -> Natural
    f = read . (: [])
    b6Data :: String
    b6Data = "612345"
    b6 :: Int -> Char
    b6 = (b6Data !!)
    encodeBase6 :: BS.ByteString -> String
    encodeBase6 bs
      | BS.null bs = mempty
      | otherwise = showIntAtBase 6 b6 (bsToInteger bs) ""

-- Produces Bools using base6ToBool. Once 8 Bools have been
-- accumulated, it is transformed to a Word8 using boolsToWord8.
-- Any unused Bools are returned along with the Word8s.
base6ToWord8 :: [Natural] -> [Bool] -> ([Word8], [Bool])
base6ToWord8 ns obs
  | length bs < 8 = ([], bs)
  | otherwise =
      let (ws, rest) = f bs []
       in (boolsToWord8 <$> ws, rest)
  where
    bs = concatMap base6ToBool ns <> obs
    f :: [Bool] -> [[Bool]] -> ([[Bool]], [Bool])
    f xs acc
      | length xs < 8 = (acc, xs)
      | otherwise =
          let (l, r) = splitAt (length xs - 8) xs
           in f l ([r] <> acc)

-- Produce uniformily distributed bits from dice rolls
-- 6 -> [0,0]
-- 1 -> [0,1]
-- 2 -> [1,0]
-- 3 -> [1,1]
-- 4 -> [0]
-- 5 -> [1]
base6ToBool :: Natural -> [Bool]
base6ToBool =
  \case
    6 -> [False, False]
    1 -> [False, True]
    2 -> [True, False]
    3 -> [True, True]
    4 -> [False]
    5 -> [True]
    _ -> error "Invalid base6ToBool"

-- Big Endian
-- [T,F,F,F,F,F,F,F] -> 128
-- [F,F,F,F,F,F,F,T] -> 1
boolsToWord8 :: [Bool] -> Word8
boolsToWord8 xs
  | length xs /= 8 = error "Invalid boolsToWord8"
  | otherwise =
      foldl' setBit 0 ys
  where
    ys = fmap fst $ filter snd $ zip [0 ..] $ reverse xs

-- Packs bools into a ByteString
packBools :: [Bool] -> BS.ByteString
packBools bs
  | length bs `mod` 8 /= 0 = error "Invalid packBools length"
  | otherwise = BS.pack $ boolsToWord8 <$> chunksOf 8 bs

-- Mix entropy of same length by xoring them
mixEntropy ::
  BS.ByteString ->
  BS.ByteString ->
  Either String BS.ByteString
mixEntropy ent1 ent2
  | BS.length ent1 == BS.length ent2 =
      Right $ BS.pack $ BS.zipWith xor ent1 ent2
  | otherwise = Left "Entropy is not of the same length"

askInputDice :: IO Natural
askInputDice = do
  inputM <- Haskeline.runInputT Haskline.defaultSettings $ Haskeline.getInputLine "Dice Roll: "
  case readMaybe =<< inputM of
    Just n ->
      if n `elem` [1 .. 6]
        then return n
        else badInput
    _ -> badInput
  where
    badInput = do
      putStrLn "Invalid dice roll. Must be in range [1..6]"
      askInputDice

-- TODO: Ask the dice rolls in sequences of 5 or so
getDiceEntropy :: Natural -> IO BS.ByteString
getDiceEntropy ent =
  f [] []
  where
    f :: [Word8] -> [Bool] -> IO BS.ByteString
    f res acc
      | length res == fromIntegral ent =
          return $ BS.pack res
      | otherwise = do
          d <- askInputDice
          let (ws, bs) = base6ToWord8 [d] acc
          f (ws <> res) bs

genMnemonic :: Natural -> ExceptT String IO (Text, Mnemonic)
genMnemonic reqEnt
  | reqEnt `elem` [16, 20 .. 32] = do
      (origEnt, ent) <- liftIO $ systemEntropy reqEnt
      when (BS.length ent /= fromIntegral reqEnt) $
        throwError "Something went wrong with the entropy size"
      mnem <- liftEither $ toMnemonic ent
      return (origEnt, mnem)
  | otherwise = throwError "The entropy value can only be in [16,20..32]"

genMnemonicDice :: Natural -> ExceptT String IO (Text, Mnemonic)
genMnemonicDice reqEnt
  | reqEnt `elem` [16, 20 .. 32] = do
      (origEnt, sysEnt) <- liftIO $ systemEntropy reqEnt
      diceEnt <- liftIO $ getDiceEntropy reqEnt
      ent <- liftEither $ mixEntropy sysEnt diceEnt
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
