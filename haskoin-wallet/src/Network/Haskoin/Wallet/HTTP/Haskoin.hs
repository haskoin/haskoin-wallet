{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.HTTP.Haskoin
( HaskoinService(..)
, AddressTx(..)
, mergeAddressTxs
) where

import           Control.Arrow                           ((&&&))
import           Control.Lens                            ((&), (.~), (^..),
                                                          (^?))
import qualified Data.Aeson                              as J
import           Data.Aeson.Lens
import           Data.List                               (nub, sortOn, sum)
import           Data.Map                                (Map)
import qualified Data.Map                                as Map
import           Foundation
import           Foundation.Collection
import           Foundation.Compat.Text
import           Network.Haskoin.Block                   hiding (hexToBlockHash)
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

data HaskoinService = HaskoinService

data AddressTx = AddressTx
    { addrTxAddress   :: !Address
    , addrTxTxHash    :: !TxHash
    , addrTxAmount    :: !Integer
    , addrTxHeight    :: Maybe Natural
    , addrTxBlockHash :: Maybe BlockHash
    }
    deriving (Eq, Show)

getURL :: LString
getURL
    | getNetwork == testnet3Network = "https://testnet3.haskoin.com/api"
    | getNetwork == cashTestNetwork = "https://cashtest.haskoin.com/api"
    | getNetwork == bitcoinCashNetwork = "https://bitcoincash.haskoin.com/api"
    | getNetwork == bitcoinNetwork = "https://bitcoin.haskoin.com/api"
    | otherwise =
        consoleError $
        formatError $
        "Haskoin does not support the network " <> fromLString networkName

instance BlockchainService HaskoinService where
    httpBalance _ = getBalance
    httpUnspent _ = getUnspent
    httpTxInformation _ = getTxInformation
    httpTxs _ tids = (fst <$>) <$> getTxs tids
    httpBroadcast _ = broadcastTx
    httpBestHeight _ = getBestHeight

getBalance :: [Address] -> IO Satoshi
getBalance = (sum <$>) . mapM getBalance_ . groupIn 50

getBalance_ :: [Address] -> IO Satoshi
getBalance_ addrs = do
    v <- httpJsonGet opts url
    return $ fromMaybe err $ integralToNatural $ sum $ v ^.. values .
        key "confirmed" .
        _Integer
  where
    url = getURL <> "/address/balances"
    opts = HTTP.defaults & HTTP.param "addresses" .~ [toText aList]
    aList = intercalate "," $ addrToBase58 <$> addrs
    err = consoleError $ formatError "Balance was negative"

getUnspent :: [Address] -> IO [(OutPoint, ScriptOutput, Satoshi)]
getUnspent = (mconcat <$>) . mapM getUnspent_ . groupIn 50

getUnspent_ :: [Address] -> IO [(OutPoint, ScriptOutput, Satoshi)]
getUnspent_ addrs = do
    v <- httpJsonGet opts url
    let resM = mapM parseCoin $ v ^.. values
    maybe (consoleError $ formatError "Could not parse coin") return resM
  where
    url = getURL <> "/address/unspent"
    opts = HTTP.defaults & HTTP.param "addresses" .~ [toText aList]
    aList = intercalate "," $ addrToBase58 <$> addrs
    parseCoin v = do
        tid <- hexToTxHash . fromText =<< v ^? key "txid" . _String
        pos <- v ^? key "vout" . _Integral
        val <- v ^? key "value" . _Integral
        scpHex <- v ^? key "pkscript" . _String
        scp <- eitherToMaybe . withBytes decodeOutputBS =<< decodeHexText scpHex
        return (OutPoint tid pos, scp, val)

getTxInformation :: [Address] -> IO [TxInformation]
getTxInformation addrs = do
    txInfs <- mergeAddressTxs <$> getAddressTxs addrs
    txs <- getTxs $ nub $ mapMaybe txInfoTxHash txInfs
    let !tMap = Map.fromList $ (txHash . fst &&& id) <$> txs
    return $ sortOn txInfoHeight $ mergeWith tMap <$> txInfs
  where
    mergeWith :: Map TxHash (Tx, Natural) -> TxInformation -> TxInformation
    mergeWith tMap txInf =
        maybe txInf (`addData` txInf) ((`Map.lookup` tMap) =<< txInfoTxHash txInf)
    addData :: (Tx, Natural) -> TxInformation -> TxInformation
    addData (tx, fee) txInf = txInfoFillTx tx txInf {txInfoFee = Just fee}

getAddressTxs :: [Address] -> IO (Map TxHash [AddressTx])
getAddressTxs = (Map.unionsWith (<>) <$>) . mapM getAddressTxs_ . groupIn 50

getAddressTxs_ :: [Address] -> IO (Map TxHash [AddressTx])
getAddressTxs_ addrs = do
    v <- httpJsonGet opts url
    let resM = mapM parseAddrTx $ v ^.. values
    maybe
        (consoleError $ formatError "Could not parse addrTx")
        (return . Map.fromListWith (<>))
        resM
  where
    url = getURL <> "/address/transactions"
    opts = HTTP.defaults & HTTP.param "addresses" .~ [toText aList]
    aList = intercalate "," $ addrToBase58 <$> addrs
    parseAddrTx v = do
        tid <- hexToTxHash . fromText =<< v ^? key "txid" . _String
        addrB58 <- v ^? key "address" . _String
        addr <- base58ToAddr $ fromText addrB58
        amnt <- v ^? key "amount" . _Integer
        let heightM = integralToNatural =<< v ^? key "height" . _Integer
            blockM = hexToBlockHash . fromText =<< v ^? key "block" . _String
        return
            ( tid
            , [ AddressTx
                { addrTxAddress = addr
                , addrTxTxHash = tid
                , addrTxAmount = amnt
                , addrTxHeight = heightM
                , addrTxBlockHash = blockM
                }
              ])

mergeAddressTxs :: Map TxHash [AddressTx] -> [TxInformation]
mergeAddressTxs aMap =
    Map.elems $ Map.mapMaybeWithKey toMvt aMap
  where
    toMvt :: TxHash -> [AddressTx] -> Maybe TxInformation
    toMvt tid atxs =
        case head <$> nonEmpty atxs of
            Just a ->
                let (os, is) = partition ((< 0) . addrTxAmount) atxs
                in Just
                       TxInformation
                       { txInfoTxHash = Just tid
                       , txInfoTxSize = Nothing
                       , txInfoOutbound = Map.empty
                       , txInfoNonStd = 0
                       , txInfoInbound = toAddrMap is
                       , txInfoMyInputs = toAddrMap os
                       , txInfoOtherInputs = Map.empty
                       , txInfoFee = Nothing
                       , txInfoHeight = addrTxHeight a
                       , txInfoBlockHash = addrTxBlockHash a
                       }
            _ -> Nothing
    toAddrMap :: [AddressTx] -> Map Address (Satoshi, Maybe SoftPath)
    toAddrMap = Map.map (, Nothing) . Map.fromListWith (+) . fmap toAddrVal
    toAddrVal :: AddressTx -> (Address, Satoshi)
    toAddrVal = addrTxAddress &&& fromIntegral . abs . addrTxAmount

getTxs :: [TxHash] -> IO [(Tx, Natural)]
getTxs = (mconcat <$>) . mapM getTxs_ . groupIn 100

getTxs_ :: [TxHash] -> IO [(Tx, Natural)]
getTxs_ [] = return []
getTxs_ tids = do
    v <- httpJsonGet opts url
    let xs = mapM parseTx $ v ^.. values
    maybe
        (consoleError $ formatError "Could not decode the transaction")
        return
        xs
  where
    url = getURL <> "/transactions"
    opts = HTTP.defaults & HTTP.param "txids" .~ [toText tList]
    tList = intercalate "," $ txHashToHex <$> tids
    parseTx v = do
        tx <- decodeBytes =<< decodeHexText =<< v ^? key "hex" . _String
        fee <- integralToNatural =<< v ^? key "fee" . _Integer
        return (tx, fee)

broadcastTx :: Tx -> IO ()
broadcastTx tx = do
    _ <- HTTP.postWith (addStatusCheck HTTP.defaults) url val
    return ()
  where
    url = getURL <> "/transaction"
    val =
        J.object ["transaction" J..= J.String (encodeHexText $ encodeBytes tx)]

getBestHeight :: IO Natural
getBestHeight = do
    v <- httpJsonGet HTTP.defaults url
    let resM = integralToNatural =<< v ^? key "height" . _Integer
    maybe err return resM
  where
    url = getURL <> "/block/best"
    err = consoleError $ formatError "Could not get the best block height"

