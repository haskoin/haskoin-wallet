module Haskoin.Wallet.Main where

import qualified Data.ByteString.Char8 as C8
import Haskoin (Ctx, MarshalJSON (..), withContext)
import Haskoin.Wallet.Commands (Response, commandResponse)
import Haskoin.Wallet.Config (initConfig)
import Haskoin.Wallet.Parser (parserMain)
import Haskoin.Wallet.Util (encodeJsonPretty)
import Haskoin.Wallet.Database (runDB)
import Haskoin.Wallet.Migration (migrateDB)
import Haskoin.Wallet.PrettyPrinter (prettyPrinter)

clientMain :: IO ()
clientMain =
  withContext $ \ctx -> do
    cfg <- initConfig
    runDB cfg $ migrateDB ctx cfg
    (cmd, unit, json) <- parserMain
    res <- commandResponse ctx cfg unit cmd
    if json
      then jsonPrinter ctx res
      else prettyPrinter unit res

jsonPrinter :: Ctx -> Response -> IO ()
jsonPrinter ctx = C8.putStrLn . encodeJsonPretty . marshalValue ctx
