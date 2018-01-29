module Main where

import           Network.Haskoin.Constants
import qualified Network.Haskoin.Wallet.Spec
import           Test.Hspec                  (hspec)

main :: IO ()
main = setBitcoinNetwork >> hspec Network.Haskoin.Wallet.Spec.walletSpec
