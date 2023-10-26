{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Haskoin.Wallet.CommandsSpec where

import Conduit (runResourceT)
import Control.Arrow (second)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (lift, liftIO)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Either (fromRight)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Serialize (encode)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Database.Persist.Sqlite (SqlBackend, withSqliteConn)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Util.Arbitrary
import Network.Haskoin.Wallet.Database
import Network.Haskoin.Wallet.FileIO
import Network.Haskoin.Wallet.Signing
import Network.Haskoin.Wallet.SigningSpec (extAddrsT, intAddrsT, keysT, mnemPass)
import Network.Haskoin.Wallet.TestUtils
import Network.Haskoin.Wallet.TxInfo
import Numeric.Natural (Natural)
import System.Random (StdGen, mkStdGen)
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

identityTests :: Ctx -> IdentityTests
identityTests ctx = def

spec :: Spec
spec =
  prepareContext $ \ctx -> do
    buildSpec ctx

buildSpec :: Ctx -> Spec
buildSpec ctx =
  describe "Database" $
    it "can create an account" $
      runDBMemoryE $ do
        let walletFP = forceRight $ walletFingerprint btc ctx mnemPass
        d <- maybe (lift $ nextAccountDeriv walletFP btc) return Nothing
        let prv = forceRight $ signingKey btc ctx mnemPass 0
            pub = deriveXPubKey ctx prv
        (accId, acc) <- insertAccount btc ctx walletFP "test" pub
        -- Check basic account properties
        liftIO $ do
          dBAccountName acc `shouldBe` "test"
          dBAccountIndex acc `shouldBe` 0
          dBAccountDerivation acc `shouldBe` "/44'/0'/0'"
          dBAccountXPubKey acc `shouldBe` snd keysT
