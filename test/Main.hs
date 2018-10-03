{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Foundation
import qualified Network.Haskoin.Wallet.Spec
import           Test.Hspec                  (hspec)

main :: IO ()
main = hspec Network.Haskoin.Wallet.Spec.walletSpec

