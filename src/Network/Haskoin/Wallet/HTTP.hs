{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TupleSections             #-}
module Network.Haskoin.Wallet.HTTP
( httpBalance
, httpUnspent
, httpTxInformation
, httpTxs
, httpRawTxs
, httpBestHeight
, httpBroadcastTx
)
where

import           Control.Lens                            ((&), (.~), (?~), (^.),
                                                          (^..), (^?))
import           Data.Aeson                              as Json
import qualified Data.Aeson                              as J
import           Data.Aeson.Lens
import           Data.ByteString.Lazy                    (ByteString)
import           Data.List                               (nub, sum)
import           Data.Map                                (Map)
import qualified Data.Map                                as Map
import           Foundation
import           Foundation.Collection
import           Foundation.Compat.Text
import           Network.Haskoin.Address
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util                    (eitherToMaybe)
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.ConsolePrinter
import           Network.Haskoin.Wallet.FoundationCompat
import           Network.Haskoin.Wallet.TxInformation
import           Network.HTTP.Types.Status
import qualified Network.Wreq                            as HTTP
import           Network.Wreq.Types                      (ResponseChecker)

rootUrl :: Network -> LString
rootUrl net
    | net == btc = "https://btc.haskoin.com/api"
    | net == btcTest = "https://btctest.haskoin.com/api"
    | net == bch = "https://bch.haskoin.com/api"
    | net == bchTest = "https://bchtest.haskoin.com/api"
    | otherwise =
        consoleError $
        formatError $
        "Haskoin does not support the network " <>
        fromLString (getNetworkName net)

httpBalance :: Network -> [Address] -> IO Satoshi
httpBalance net = (sum <$>) . mapM (httpBalance_ net) . groupIn 50

httpBalance_ :: Network -> [Address] -> IO Satoshi
httpBalance_ net addrs = do
    v <- httpJsonGet opts url
    return $ fromMaybe err $ integralToNatural $ sum $ v ^.. values .
        key "confirmed" .
        _Integer
  where
    url = rootUrl net <> "/address/balances"
    opts = HTTP.defaults & HTTP.param "addresses" .~ [toText aList]
    aList = intercalate "," $ fromText . addrToString <$> addrs
    err = consoleError $ formatError "Balance was negative"

httpUnspent :: Network -> [Address] -> IO [(OutPoint, ScriptOutput, Satoshi)]
httpUnspent net = (mconcat <$>) . mapM (httpUnspent_ net) . groupIn 50

httpUnspent_ :: Network -> [Address] -> IO [(OutPoint, ScriptOutput, Satoshi)]
httpUnspent_ net addrs = do
    v <- httpJsonGet opts url
    let resM = mapM parseCoin $ v ^.. values
    maybe (consoleError $ formatError "Could not parse coin") return resM
  where
    url = rootUrl net <> "/address/unspent"
    opts = HTTP.defaults & HTTP.param "addresses" .~ [toText aList]
    aList = intercalate "," $ fromText . addrToString <$> addrs
    parseCoin v = do
        tid <- hexToTxHash =<< v ^? key "txid" . _String
        pos <- v ^? key "index" . _Integral
        val <- v ^? key "output" . key "value" . _Integral
        scpHex <- v ^? key "output" . key "pkscript" . _String
        scp <- eitherToMaybe . withBytes decodeOutputBS =<< decodeHexText scpHex
        return (OutPoint tid pos, scp, val)

httpTxInformation :: Network -> [Address] -> IO [TxInformation]
httpTxInformation net addrs = do
    aTxs <- httpAddressTxs net addrs
    txVals <- httpTxs net $ nub $ snd <$> aTxs
    return $ toTxInformation net (fst <$> aTxs) <$> txVals

toTxInformation :: Network -> [Address] -> Json.Value -> TxInformation
toTxInformation net addrs v =
    TxInformation
    { txInfoTxHash = hexToTxHash =<< v ^? key "txid" . _String
    , txInfoTxSize = toCount <$> v ^? key "size" . _Integral
    , txInfoOutbound = othOs
    , txInfoNonStdOutputs = os Map.! Nothing
    , txInfoInbound = Map.map (,Nothing) myOs
    , txInfoMyInputs = Map.map (,Nothing) myIs
    , txInfoOtherInputs = othIs
    , txInfoNonStdInputs = is Map.! Nothing
    , txInfoFee = v ^? key "fee" . _Integral
    , txInfoHeight = v ^? key "block" . key "height" . _Integral
    , txInfoBlockHash =
          hexToBlockHash =<< v ^? key "block" . key "hash" . _String
    }
  where
    is, os :: Map (Maybe Address) Satoshi
    is = Map.fromListWith (+) $ mapMaybe f $ v ^.. key "inputs" . values
    os = Map.fromListWith (+) $ mapMaybe f $ v ^.. key "outputs" . values
    f x =
        (stringToAddr net =<< x ^? key "address" . _String, ) <$>
        (fromIntegral <$> x ^? key "value" . _Integer)
    (myIs, othIs) = Map.partitionWithKey isMine $ g is
    (myOs, othOs) = Map.partitionWithKey isMine $ g os
    g = Map.fromList . catMaybes . (h <$>) . Map.toList
    h (Nothing,_) = Nothing
    h (Just a,b)  = Just (a,b)
    isMine k _ = k `elem` addrs

httpAddressTxs :: Network -> [Address] -> IO [(Address, TxHash)]
httpAddressTxs net = (mconcat <$>) . mapM (httpAddressTxs_ net) . groupIn 50

httpAddressTxs_ :: Network -> [Address] -> IO [(Address, TxHash)]
httpAddressTxs_ net addrs = do
    v <- httpJsonGet opts url
    let resM = mapM parseAddrTx $ v ^.. values
    maybe (consoleError $ formatError "Could not parse addrTx") return resM
  where
    url = rootUrl net <> "/address/transactions"
    aList = intercalate "," $ addrToString <$> addrs
    opts = HTTP.defaults & HTTP.param "addresses" .~ [aList]
    parseAddrTx v = do
        tid <- hexToTxHash =<< v ^? key "txid" . _String
        addrB58 <- v ^? key "address" . _String
        addr <- stringToAddr net addrB58
        return (addr, tid)

httpTxs :: Network -> [TxHash] -> IO [Json.Value]
httpTxs net = (mconcat <$>) . mapM (httpTxs_ net) . groupIn 50

httpTxs_ :: Network -> [TxHash] -> IO [Json.Value]
httpTxs_ _ [] = return []
httpTxs_ net tids = do
    v <- httpJsonGet opts url
    return $ v ^.. values
  where
    url = rootUrl net <> "/transactions"
    opts = HTTP.defaults & HTTP.param "txids" .~ [tList]
    tList = intercalate "," $ txHashToHex <$> tids

httpRawTxs :: Network -> [TxHash] -> IO [Tx]
httpRawTxs net = (mconcat <$>) . mapM (httpRawTxs_ net) . groupIn 100

httpRawTxs_ :: Network -> [TxHash] -> IO [Tx]
httpRawTxs_ _ [] = return []
httpRawTxs_ net tids = do
    v <- httpJsonGet opts url
    let xs = mapM parseTx $ v ^.. values
    maybe
        (consoleError $ formatError "Could not decode the transaction")
        return
        xs
  where
    url = rootUrl net <> "/transactions/hex"
    opts = HTTP.defaults & HTTP.param "txids" .~ [tList]
    tList = intercalate "," $ txHashToHex <$> tids
    parseTx = (decodeBytes =<<) . decodeHexText . (^. _String)

httpBestHeight :: Network -> IO Natural
httpBestHeight net = do
    v <- httpJsonGet HTTP.defaults url
    let resM = integralToNatural =<< v ^? key "height" . _Integer
    maybe err return resM
  where
    url = rootUrl net <> "/block/best"
    err = consoleError $ formatError "Could not get the best block height"

httpBroadcastTx :: Network -> Tx -> IO ()
httpBroadcastTx net tx = do
    _ <- HTTP.postWith (addStatusCheck HTTP.defaults) url val
    return ()
  where
    url = rootUrl net <> "/transaction"
    val =
        J.object ["transaction" J..= J.String (encodeHexText $ encodeBytes tx)]

httpJsonGet :: HTTP.Options -> LString -> IO Json.Value
httpJsonGet = httpJsonGen id

httpJsonGen ::
       (HTTP.Response ByteString -> HTTP.Response ByteString)
    -> HTTP.Options
    -> LString
    -> IO Json.Value
httpJsonGen f opts url = do
    r <- HTTP.asValue . f =<< HTTP.getWith (addStatusCheck opts) url
    return $ r ^. HTTP.responseBody

checkStatus :: ResponseChecker
checkStatus _ r
    | statusIsSuccessful status = return ()
    | otherwise =
        consoleError $
        vcat
            [ formatError "Received an HTTP error response:"
            , nest 4 $
              vcat
                  [ formatKey (block 10 "Status:") <> formatError (show code)
                  , formatKey (block 10 "Message:") <>
                    formatStatic
                        (fromMaybe "Could not decode the message" $
                         bsToString message)
                  ]
            ]
  where
    code = r ^. HTTP.responseStatus . HTTP.statusCode
    message = r ^. HTTP.responseStatus . HTTP.statusMessage
    status = mkStatus code message

addStatusCheck :: HTTP.Options -> HTTP.Options
addStatusCheck opts = opts & (HTTP.checkResponse ?~ checkStatus)

