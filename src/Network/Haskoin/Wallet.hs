{-# LANGUAGE ApplicativeDo #-}

module Network.Haskoin.Wallet where

import qualified Data.ByteString.Char8 as C8
import Haskoin.Crypto (Ctx, withContext)
import Haskoin.Util (MarshalJSON (marshalValue))
import Network.Haskoin.Wallet.Commands
  ( Response,
    commandResponse,
  )
import Network.Haskoin.Wallet.Parser (programParser)
import Network.Haskoin.Wallet.Util (encodeJsonPretty)
import Options.Applicative
  ( customExecParser,
    prefs,
    showHelpOnEmpty,
  )

clientMain :: IO ()
clientMain =
  withContext $ \ctx -> do
    cmd <- customExecParser (prefs showHelpOnEmpty) (programParser ctx)
    res <- commandResponse ctx cmd
    jsonPrinter ctx res

jsonPrinter :: Ctx -> Response -> IO ()
jsonPrinter ctx = C8.putStrLn . encodeJsonPretty . marshalValue ctx
