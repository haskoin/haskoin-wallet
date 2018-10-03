{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Network.Haskoin.Wallet.Arbitrary where

import           Foundation
import           Network.Haskoin.Constants
import           Network.Haskoin.Test
import           Network.Haskoin.Wallet.AccountStore
import           Network.Haskoin.Wallet.Signing
import           Test.QuickCheck

arbitraryTxSignData :: Network -> Gen TxSignData
arbitraryTxSignData net =
    TxSignData
        <$> arbitraryTx net
        <*> (flip vectorOf (arbitraryTx net) =<< choose (0, 5))
        <*> listOf arbitrarySoftPath
        <*> listOf arbitrarySoftPath

arbitraryAccountStore :: Network -> Gen AccountStore
arbitraryAccountStore net =
    AccountStore
        <$> (snd <$> arbitraryXPubKey net)
        <*> (fromIntegral <$> (arbitrary :: Gen Word64))
        <*> (fromIntegral <$> (arbitrary :: Gen Word64))
        <*> arbitraryHardPath

