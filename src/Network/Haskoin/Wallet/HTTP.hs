{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
module Network.Haskoin.Wallet.HTTP
( AddressTx(..)
, httpBalance
, httpUnspent
, httpTxInformation
, httpTxs
, httpBestHeight
, httpBroadcastTx
)
where

import           Control.Arrow                           ((&&&))
import           Control.Lens                            ((&), (.~), (^.),
                                                          (^..), (^?))
import           Data.Aeson                              as Json
import qualified Data.Aeson                              as J
import           Data.Aeson.Lens
import           Data.ByteString.Lazy                    (ByteString)
import           Data.List                               (nub, sortOn, sum)
import           Data.Map                                (Map)
import qualified Data.Map                                as Map
import           Foundation
import           Foundation.Collection
import           Foundation.Compat.Text
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto                  hiding (addrToBase58,
                                                          base58ToAddr)
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction             hiding (hexToTxHash,
                                                          txHashToHex)
import           Network.Haskoin.Util                    (eitherToMaybe)
import           Network.Haskoin.Wallet.Amounts
import           Network.Haskoin.Wallet.ConsolePrinter
import           Network.Haskoin.Wallet.FoundationCompat
import           Network.Haskoin.Wallet.TxInformation
import           Network.HTTP.Types.Status
import qualified Network.Wreq                            as HTTP
import           Network.Wreq.Types                      (ResponseChecker)

rootUrl :: LString
rootUrl
    | getNetwork == btcTest = "https://testnet3.haskoin.com/api"
    | getNetwork == bchTest = "https://cashtest.haskoin.com/api"
    | getNetwork == bch = "https://bitcoincash.haskoin.com/api"
    | getNetwork == btc = "https://bitcoin.haskoin.com/api"
    | otherwise =
        consoleError $
        formatError $
        "Haskoin does not support the network " <> fromLString networkName

httpBalance :: [Address] -> IO Satoshi
httpBalance = (sum <$>) . mapM httpBalance_ . groupIn 50

httpBalance_ :: [Address] -> IO Satoshi
httpBalance_ addrs = do
    v <- httpJsonGet opts url
    return $ fromMaybe err $ integralToNatural $ sum $ v ^.. values .
        key "confirmed" .
        _Integer
  where
    url = rootUrl <> "/address/balances"
    opts = HTTP.defaults & HTTP.param "addresses" .~ [toText aList]
    aList = intercalate "," $ addrToBase58 <$> addrs
    err = consoleError $ formatError "Balance was negative"

httpUnspent :: [Address] -> IO [(OutPoint, ScriptOutput, Satoshi)]
httpUnspent = (mconcat <$>) . mapM httpUnspent_ . groupIn 50

httpUnspent_ :: [Address] -> IO [(OutPoint, ScriptOutput, Satoshi)]
httpUnspent_ addrs = do
    v <- httpJsonGet opts url
    let resM = mapM parseCoin $ v ^.. values
    maybe (consoleError $ formatError "Could not parse coin") return resM
  where
    url = rootUrl <> "/address/unspent"
    opts = HTTP.defaults & HTTP.param "addresses" .~ [toText aList]
    aList = intercalate "," $ addrToBase58 <$> addrs
    parseCoin v = do
        tid <- hexToTxHash . fromText =<< v ^? key "txid" . _String
        pos <- v ^? key "vout" . _Integral
        val <- v ^? key "value" . _Integral
        scpHex <- v ^? key "pkscript" . _String
        scp <- eitherToMaybe . withBytes decodeOutputBS =<< decodeHexText scpHex
        return (OutPoint tid pos, scp, val)

httpTxInformation :: [Address] -> IO [TxInformation]
httpTxInformation addrs = do
    txInfs <- mergeAddressTxs <$> httpAddressTxs addrs
    txs <- httpTxs $ nub $ mapMaybe txInfoTxHash txInfs
    let !tMap = Map.fromList $ (txHash . fst &&& id) <$> txs
    return $ sortOn txInfoHeight $ mergeWith tMap <$> txInfs
  where
    mergeWith :: Map TxHash (Tx, Natural) -> TxInformation -> TxInformation
    mergeWith tMap txInf =
        maybe
            txInf
            (`addData` txInf)
            ((`Map.lookup` tMap) =<< txInfoTxHash txInf)
    addData :: (Tx, Natural) -> TxInformation -> TxInformation
    addData (tx, fee) txInf = txInfoFillTx tx txInf {txInfoFee = Just fee}

httpAddressTxs :: [Address] -> IO [AddressTx]
httpAddressTxs = (mconcat <$>) . mapM httpAddressTxs_ . groupIn 50

httpAddressTxs_ :: [Address] -> IO [AddressTx]
httpAddressTxs_ addrs = do
    v <- httpJsonGet opts url
    let resM = mapM parseAddrTx $ v ^.. values
    maybe (consoleError $ formatError "Could not parse addrTx") return resM
  where
    url = rootUrl <> "/address/transactions"
    aList = intercalate "," $ addrToBase58 <$> addrs
    opts = HTTP.defaults & HTTP.param "addresses" .~ [toText aList]
    parseAddrTx v = do
        tid <- hexToTxHash . fromText =<< v ^? key "txid" . _String
        addrB58 <- v ^? key "address" . _String
        addr <- base58ToAddr $ fromText addrB58
        amnt <- v ^? key "amount" . _Integer
        let heightM = integralToNatural =<< v ^? key "height" . _Integer
            blockM = hexToBlockHash . fromText =<< v ^? key "block" . _String
        return
            AddressTx
            { addrTxAddress = addr
            , addrTxTxHash = tid
            , addrTxAmount = amnt
            , addrTxHeight = heightM
            , addrTxBlockHash = blockM
            }

httpTxs :: [TxHash] -> IO [(Tx, Natural)]
httpTxs = (mconcat <$>) . mapM httpTxs_ . groupIn 100

httpTxs_ :: [TxHash] -> IO [(Tx, Natural)]
httpTxs_ [] = return []
httpTxs_ tids = do
    v <- httpJsonGet opts url
    let xs = mapM parseTx $ v ^.. values
    maybe
        (consoleError $ formatError "Could not decode the transaction")
        return
        xs
  where
    url = rootUrl <> "/transactions"
    opts = HTTP.defaults & HTTP.param "txids" .~ [toText tList]
    tList = intercalate "," $ txHashToHex <$> tids
    parseTx v = do
        tx <- decodeBytes =<< decodeHexText =<< v ^? key "hex" . _String
        fee <- integralToNatural =<< v ^? key "fee" . _Integer
        return (tx, fee)

httpBestHeight :: IO Natural
httpBestHeight = do
    v <- httpJsonGet HTTP.defaults url
    let resM = integralToNatural =<< v ^? key "height" . _Integer
    maybe err return resM
  where
    url = rootUrl <> "/block/best"
    err = consoleError $ formatError "Could not get the best block height"

httpBroadcastTx :: Tx -> IO ()
httpBroadcastTx tx = do
    _ <- HTTP.postWith (addStatusCheck HTTP.defaults) url val
    return ()
  where
    url = rootUrl <> "/transaction"
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
addStatusCheck opts = opts & HTTP.checkResponse .~ Just checkStatus

