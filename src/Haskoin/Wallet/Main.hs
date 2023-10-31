module Haskoin.Wallet.Main where

import qualified Data.ByteString.Char8 as C8
import Haskoin (Ctx, MarshalJSON (..), withContext)
import Haskoin.Wallet.Commands (Response, commandResponse)
import Haskoin.Wallet.Config (initConfig)
import Haskoin.Wallet.Parser (parserMain)
import Haskoin.Wallet.Util (encodeJsonPretty)

clientMain :: IO ()
clientMain =
  withContext $ \ctx -> do
    cfg <- initConfig
    cmd <- parserMain
    res <- commandResponse ctx cfg cmd
    jsonPrinter ctx res

jsonPrinter :: Ctx -> Response -> IO ()
jsonPrinter ctx = C8.putStrLn . encodeJsonPretty . marshalValue ctx
