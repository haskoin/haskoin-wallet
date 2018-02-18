{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.HTTP.BlockchainInfo
( BlockchainInfoService(..)
) where

import           Control.Lens                            ((&), (.~), (^..),
                                                          (^?))
import           Control.Monad                           (guard)
import           Data.Aeson.Lens
import           Data.List                               (sum)
import qualified Data.Map.Strict                         as Map
import           Foundation
import           Foundation.Collection
import           Foundation.Compat.ByteString
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
import           Network.Haskoin.Wallet.HTTP
import           Network.Haskoin.Wallet.TxInformation
import qualified Network.Wreq                            as HTTP

data BlockchainInfoService = BlockchainInfoService

getURL :: LString
getURL
    | getNetwork == bitcoinNetwork = "https://blockchain.info"
    | getNetwork == testnet3Network = "https://testnet.blockchain.info"
    | otherwise =
        consoleError $
        formatError $
        "blockchain.info does not support the network " <>
        fromLString networkName

instance BlockchainService BlockchainInfoService where
    httpBalance _ = getBalance
    httpUnspent _ = getUnspent
    httpTxInformation _ = getTxInformation
    httpTx _ = getTx
    httpBestHeight _ = getBestHeight
    httpBroadcast _ = broadcastTx

getBalance :: [Address] -> IO Satoshi
getBalance = (sum <$>) . mapM getBalance_ . groupIn 50

getBalance_ :: [Address] -> IO Satoshi
getBalance_ addrs = do
    v <- httpJsonGet opts url
    return $ fromMaybe err $ integralToNatural $ sum $ v ^.. members .
        key "final_balance" .
        _Integer
  where
    url = getURL <> "/balance"
    opts = HTTP.defaults & HTTP.param "active" .~ [toText aList]
    aList = intercalate "|" $ addrToBase58 <$> addrs
    err = consoleError $ formatError "Balance was negative"

getUnspent :: [Address] -> IO [(OutPoint, ScriptOutput, Satoshi)]
getUnspent = (mconcat <$>) . mapM getUnspent_ . groupIn 50

getUnspent_ :: [Address] -> IO [(OutPoint, ScriptOutput, Satoshi)]
getUnspent_ addrs = do
    v <- httpJsonGet opts url
    let resM = mapM parseCoin $ v ^.. key "unspent_outputs" . values
    maybe (consoleError $ formatError "Could not parse coin") return resM
  where
    url = getURL <> "/unspent"
    opts =
        HTTP.defaults & HTTP.param "active" .~ [toText aList] &
        HTTP.param "confirmations" .~
        ["1"]
    aList = intercalate "|" $ addrToBase58 <$> addrs
    parseCoin v = do
        tid <- hexToTxHash' . fromText =<< v ^? key "tx_hash" . _String
        pos <- v ^? key "tx_output_n" . _Integral
        val <- v ^? key "value" . _Integral
        scpHex <- v ^? key "script" . _String
        scp <- eitherToMaybe . withBytes decodeOutputBS =<< decodeHexText scpHex
        return (OutPoint tid pos, scp, val)

getTxInformation :: [Address] -> IO [TxInformation]
getTxInformation = (mconcat <$>) . mapM getTxInformation_ . groupIn 50

getTxInformation_ :: [Address] -> IO [TxInformation]
getTxInformation_ addrs = do
    v <- httpJsonGet opts url
    let resM = mapM parseTxInformation $ v ^.. key "txs" . values
        txInfs =
            fromMaybe
                (consoleError $ formatError "Could not parse TxInformation")
                resM
    forM txInfs $ \txInf ->
        case txInfoTxHash txInf of
            Just tid -> do
                tx <- getTx tid
                return $ txInfoFillTx tx txInf
            _ -> return txInf
  where
    url = getURL <> "/multiaddr"
    opts = HTTP.defaults & HTTP.param "active" .~ [toText aList]
    aList = intercalate "|" $ addrToBase58 <$> addrs
    parseTxInformation v = do
        tid <- hexToTxHash . fromText =<< v ^? key "hash" . _String
        size <- integralToNatural =<< v ^? key "size" . _Integer
        fee <- integralToNatural =<< v ^? key "fee" . _Integer
        let heightM = integralToNatural =<< v ^? key "block_height" . _Integer
            is =
                Map.fromList $ mapMaybe go $ v ^.. key "inputs" . values .
                key "prev_out"
            os = Map.fromList $ mapMaybe go $ v ^.. key "out" . values
        return
            TxInformation
            { txInfoTxHash = Just tid
            , txInfoTxSize = Just $ fromIntegral size
            , txInfoOutbound = Map.empty
            , txInfoNonStd = 0
            , txInfoInbound = Map.map (, Nothing) os
            , txInfoMyInputs = Map.map (, Nothing) is
            , txInfoOtherInputs = Map.empty
            , txInfoFee = Just fee
            , txInfoHeight = heightM
            , txInfoBlockHash = Nothing
            }
    go v = do
        addr <- base58ToAddr . fromText =<< v ^? key "addr" . _String
        guard $ addr `elem` addrs
        amnt <- v ^? key "value" . _Integer
        let err = consoleError $ formatError "Encountered a negative value"
        return (addr, fromMaybe err $ integralToNatural amnt)

getTx :: TxHash -> IO Tx
getTx tid = do
    bytes <- httpBytesGet opts url
    maybe (consoleError $ formatError "Could not decode tx") return $
        decodeBytes =<< decodeHex bytes
  where
    url = getURL <> "/rawtx/" <> toLString (txHashToHex tid)
    opts = HTTP.defaults & HTTP.param "format" .~ ["hex"]

broadcastTx :: Tx -> IO ()
broadcastTx tx = do
    _ <- HTTP.postWith (addStatusCheck HTTP.defaults) url $ HTTP.partBS "tx" dat
    return ()
  where
    url = getURL <> "/pushtx"
    dat = toByteString $ encodeHex $ encodeBytes tx

getBestHeight :: IO Natural
getBestHeight = do
    v <- httpJsonGet HTTP.defaults url
    let resM = integralToNatural =<< v ^? key "height" . _Integer
    maybe err return resM
  where
    url = getURL <> "/latestblock"
    err = consoleError $ formatError "Could not get the best block height"

hexToTxHash' :: String -> Maybe TxHash
hexToTxHash' = decodeHexStr >=> decodeBytes >=> return . TxHash

