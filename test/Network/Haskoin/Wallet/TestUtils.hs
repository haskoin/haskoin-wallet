{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Haskoin.Wallet.TestUtils where

import qualified Data.Aeson                          as A
import           Data.Proxy
import qualified Data.Serialize                      as S
import qualified Data.Typeable                       as T
import           Data.Word
import           Haskoin.Constants
import           Haskoin.Util.Arbitrary
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Signing
import           Numeric.Natural
import           Test.Hspec
import           Test.QuickCheck

genNatural :: Gen Natural
genNatural = arbitrarySizedNatural

