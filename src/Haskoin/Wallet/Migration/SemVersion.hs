{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Migration.SemVersion where

import Data.List.Split
import Data.String (IsString)
import Numeric.Natural

-- | Version of Haskoin Wallet package.
currentVersionStr :: (IsString a) => a
#ifdef CURRENT_PACKAGE_VERSION
currentVersionStr = CURRENT_PACKAGE_VERSION
#else
currentVersionStr = error "No version string"
#endif

data SemVersion
  = VerMajor !Natural
  | VerMinor !Natural !Natural
  | VerPatch !Natural Natural Natural
  deriving (Show)

-- 0 == 0.9 == 0.9.0
instance Eq SemVersion where
  (VerMajor j1) == (VerMajor j2) = j1 == j2
  (VerMajor j1) == (VerMinor j2 _) = j1 == j2
  (VerMajor j1) == (VerPatch j2 _ _) = j1 == j2
  (VerMinor j1 m1) == (VerMinor j2 m2) = j1 == j2 && m1 == m2
  (VerMinor j1 m1) == (VerPatch j2 m2 _) = j1 == j2 && m1 == m2
  (VerPatch j1 m1 p1) == (VerPatch j2 m2 p2) = j1 == j2 && m1 == m2 && p1 == p2
  a == b = b == a

toMajor :: SemVersion -> SemVersion
toMajor (VerMajor a) = VerMajor a
toMajor (VerMinor a _) = VerMajor a
toMajor (VerPatch a _ _) = VerMajor a

toMinor :: SemVersion -> SemVersion
toMinor (VerMajor _) = error "VerMajor in toMinor"
toMinor (VerMinor a b) = VerMinor a b
toMinor (VerPatch a b _) = VerMinor a b

parseSemVersion :: String -> SemVersion
parseSemVersion str =
  case read <$> splitOn "." str of
    [a] -> VerMajor a
    [a, b] -> VerMinor a b
    [a, b, c] -> VerPatch a b c
    _ -> error $ "Bad version: " <> str

verString :: SemVersion -> String
verString (VerMajor a) = show a
verString (VerMinor a b) = show a <> "." <> show b
verString (VerPatch a b c) = show a <> "." <> show b <> "." <> show c

currentVersion :: SemVersion
currentVersion =
  case parseSemVersion currentVersionStr of
    res@(VerPatch {}) -> res
    _ -> error $ "Bad version: " <> currentVersionStr
