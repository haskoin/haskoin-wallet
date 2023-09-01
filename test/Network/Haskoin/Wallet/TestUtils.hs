{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.TestUtils where

import Numeric.Natural
import Test.Hspec
import Test.QuickCheck

genNatural :: Gen Natural
genNatural = arbitrarySizedNatural
