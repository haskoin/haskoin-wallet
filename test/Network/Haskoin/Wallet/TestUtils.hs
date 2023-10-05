{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.TestUtils where

import Data.Either (fromRight)
import Numeric.Natural (Natural)
import Test.QuickCheck (Gen, arbitrarySizedNatural)

genNatural :: Test.QuickCheck.Gen Natural
genNatural = Test.QuickCheck.arbitrarySizedNatural

forceRight :: Either a b -> b
forceRight = fromRight (error "fromRight")
