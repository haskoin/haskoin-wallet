{-# LANGUAGE ApplicativeDo #-}

module Haskoin.Wallet where

import qualified Data.ByteString.Char8 as C8
import Haskoin.Crypto (Ctx, withContext)
import Haskoin.Util (MarshalJSON (..))
import Haskoin.Wallet.Commands
import Haskoin.Wallet.Parser
import Haskoin.Wallet.Util (encodeJsonPretty)
import Haskoin.Wallet.Config

clientMain :: IO ()
clientMain =
  withContext $ \ctx -> do
    cfg <- initConfig
    cmd <- parserMain
    res <- commandResponse ctx cfg cmd
    jsonPrinter ctx res

jsonPrinter :: Ctx -> Response -> IO ()
jsonPrinter ctx = C8.putStrLn . encodeJsonPretty . marshalValue ctx
