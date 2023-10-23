{-# LANGUAGE ApplicativeDo #-}

module Network.Haskoin.Wallet where

import qualified Data.ByteString.Char8 as C8
import Haskoin.Crypto (Ctx, withContext)
import Haskoin.Util (MarshalJSON (..))
import Network.Haskoin.Wallet.Commands
import Network.Haskoin.Wallet.Parser
import Network.Haskoin.Wallet.Util (encodeJsonPretty)

clientMain :: IO ()
clientMain =
  withContext $ \ctx -> do
    cmd <- parserMain
    res <- commandResponse ctx cmd
    jsonPrinter ctx res

jsonPrinter :: Ctx -> Response -> IO ()
jsonPrinter ctx = C8.putStrLn . encodeJsonPretty . marshalValue ctx
