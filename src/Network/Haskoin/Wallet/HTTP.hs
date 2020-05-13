{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Network.Haskoin.Wallet.HTTP

where

import           Control.Lens                    ((&), (.~), (?~), (^.), (^..),
                                                  (^?))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson                      as Json
import qualified Data.Aeson                      as J
import           Data.Aeson.Lens
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as BL
import           Data.List                       (nub, sum, sort)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Serialize                  as S
import           Data.String.Conversions         (cs)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Haskoin.Address
import           Haskoin.Block
import           Haskoin.Constants
import           Haskoin.Script
import qualified Haskoin.Store.Data              as Store
import           Haskoin.Transaction
import           Haskoin.Util
import           Network.Haskoin.Wallet.Util
import           Network.HTTP.Types.Status
import qualified Network.Wreq                    as HTTP
import           Network.Wreq.Types              (ResponseChecker)
import           Numeric.Natural
import           Options.Applicative.Help.Pretty hiding ((</>))

{- Application -}

httpAddrTxs ::
       (MonadIO m, MonadReader Network m, MonadError String m)
    => [Address]
    -> Page
    -> m [Store.Transaction]
httpAddrTxs addrs p = do
    blockTxs <- batchQuery 20 $ AddressTransactions addrs Nothing (Just 500)
    let sortedTxs = sortDesc blockTxs
    batchQuery 20 $ Transactions $ Store.blockTxHash <$> toPage p sortedTxs

httpAddrBalances ::
       (MonadIO m, MonadReader Network m, MonadError String m)
    => [Address]
    -> m [Store.Balance]
httpAddrBalances addrs = batchQuery 20 $ AddressBalances addrs

bestBlockHeight ::
       (MonadIO m, MonadReader Network m, MonadError String m)
    => m Natural
bestBlockHeight =
    fromIntegral . Store.blockDataHeight <$> apiQuery (BlockBest True)

{- Helper API -}

data ApiPoint a where
    AddressTransactions ::
        { addrTxAddresses :: [Address]
        , addrTxHeight    :: Maybe String
        , addrTxLimit     :: Maybe Natural
        }                 -> ApiPoint [Store.BlockTx]
    AddressBalances ::
        { addrBalAddresses :: [Address]
        }                  -> ApiPoint [Store.Balance]
    Transactions ::
        { txsTxids :: [TxHash]
        }          -> ApiPoint [Store.Transaction]
    TransactionsRaw ::
        { txsRawTxids :: [TxHash]
        }             -> ApiPoint [Tx]
    BlockBest ::
        { blockBestNoTx :: Bool } -> ApiPoint Store.BlockData
    Health :: ApiPoint Store.HealthCheck

apiHost :: Network -> String
apiHost net = "https://api.haskoin.com" </> getNetworkName net

batchQuery ::
       (MonadIO m, MonadReader Network m, MonadError String m, Monoid a)
    =>  Natural -> ApiPoint a -> m a
batchQuery i a = mconcat <$> mapM apiQuery (batchApiPoint i a)

batchApiPoint :: Natural -> ApiPoint a -> [ApiPoint a]
batchApiPoint i =
    \case
        AddressTransactions xs h l ->
            AddressTransactions <$> chunksOf i xs <*> pure h <*> pure l
        AddressBalances xs ->
            AddressBalances <$> chunksOf i xs
        Transactions xs -> Transactions <$> chunksOf i xs
        TransactionsRaw xs -> TransactionsRaw <$> chunksOf i xs
        _ -> error "This ApiPoint does not support batching"

apiQuery :: (MonadIO m, MonadReader Network m, MonadError String m) =>
            ApiPoint a -> m a
apiQuery point = do
    net <- network
    case point of
        AddressTransactions addrs heightM limitM -> do
            addrsTxt <- liftEither $ addrToStringE net `mapM` addrs
            let url = apiHost net </> "address" </> "transactions"
                opts = applyOpt "addresses" addrsTxt
                    <> applyOptM "height" ((:[]) . cs <$> heightM)
                    <> applyOptM "limit" ((:[]) . cs . show <$> limitM)
            httpBinary opts url
        AddressBalances addrs -> do
            addrsTxt <- liftEither $ addrToStringE net `mapM` addrs
            let url = apiHost net </> "address" </> "balances"
                opts = applyOpt "addresses" addrsTxt
            httpBinary opts url
        Transactions tids -> do
            let txsTxt = txHashToHex <$> tids
                url = apiHost net </> "transactions"
                opts = applyOpt "txids" txsTxt
            httpBinary opts url
        TransactionsRaw tids -> do
            let txsTxt = txHashToHex <$> tids
                url = apiHost net </> "transactions" </> "raw"
                opts = applyOpt "txids" txsTxt
            httpBinary opts url
        BlockBest notx -> do
            let url = apiHost net </> "block" </> "best"
                opts = optBool "notx" notx
            httpBinary opts url
        Health -> do
            let url = apiHost net </> "health"
            httpBinary (Endo id) url

httpBinary ::
       (MonadIO m, MonadError String m, S.Serialize a)
    => Endo HTTP.Options
    -> String
    -> m a
httpBinary opts url = do
    res <- liftIO $ HTTP.getWith opts' url
    liftEither $ S.decodeLazy $ res ^. HTTP.responseBody
  where
    opts' = appEndo (opts <> accept <> stat) HTTP.defaults
    accept = Endo $ HTTP.header "Accept" .~ ["application/octet-stream"]
    stat   = Endo $ HTTP.checkResponse ?~ checkStatus

-- TODO: Capture this and return JSON error
checkStatus :: ResponseChecker
checkStatus _ r
    | statusIsSuccessful status = return ()
    | otherwise = error $ "HTTP Error " <> show code <> ": " <> cs message
  where
    code = r ^. HTTP.responseStatus . HTTP.statusCode
    message = r ^. HTTP.responseStatus . HTTP.statusMessage
    status = mkStatus code message

{- Helpers -}

applyOpt :: Text -> [Text] -> Endo HTTP.Options
applyOpt p t = Endo $ HTTP.param p .~ t

applyOptM :: Text -> Maybe [Text] -> Endo HTTP.Options
applyOptM p =
    \case
        Just t -> applyOpt p t
        _ -> Endo id

optBool :: Text -> Bool -> Endo HTTP.Options
optBool _ False = Endo id
optBool p True  = applyOpt p ["true"]

{-

httpAddressTxs :: Network -> [Address] -> IO [(Address, TxHash)]
httpAddressTxs net = (mconcat <$>) . mapM (httpAddressTxs_ net) . chunksOf 50

httpAddressTxs_ :: Network -> [Address] -> IO [(Address, TxHash)]
httpAddressTxs_ net addrs = do
    v <- httpJsonGet opts url
    let resM = mapM parseAddrTx $ v ^.. values
    maybe (exitError "Could not parse addrTx") return resM
  where
    url = baseUrl net <> "/address/transactions"
    aList = Text.intercalate "," $ addrStr net <$> addrs
    opts = HTTP.defaults & HTTP.param "addresses" .~ [aList]
    parseAddrTx v = do
        tid <- hexToTxHash =<< v ^? key "txid" . _String
        addrB58 <- v ^? key "address" . _String
        addr <- stringToAddr net addrB58
        return (addr, tid)

httpTxs :: Network -> [TxHash] -> IO [Json.Value]
httpTxs net = (mconcat <$>) . mapM (httpTxs_ net) . chunksOf 50

httpTxs_ :: Network -> [TxHash] -> IO [Json.Value]
httpTxs_ _ [] = return []
httpTxs_ net tids = do
    v <- httpJsonGet opts url
    return $ v ^.. values
  where
    url = baseUrl net <> "/transactions"
    opts = HTTP.defaults & HTTP.param "txids" .~ [tList]
    tList = Text.intercalate "," $ txHashToHex <$> tids

httpRawTxs :: Network -> [TxHash] -> IO [Tx]
httpRawTxs net = (mconcat <$>) . mapM (httpRawTxs_ net) . chunksOf 100

httpRawTxs_ :: Network -> [TxHash] -> IO [Tx]
httpRawTxs_ _ [] = return []
httpRawTxs_ net tids = do
    v <- httpJsonGet opts url
    let xs = mapM parseTx $ v ^.. values
    maybe (exitError "Could not decode the transaction") return xs
  where
    url = baseUrl net <> "/transactions/hex"
    opts = HTTP.defaults & HTTP.param "txids" .~ [tList]
    tList = Text.intercalate "," $ txHashToHex <$> tids
    parseTx = (eitherToMaybe . S.decode =<<) . decodeHex . (^. _String)

httpBalance :: Network -> [Address] -> IO Natural
httpBalance net = (sum <$>) . mapM (httpBalance_ net) . chunksOf 50

httpBalance_ :: Network -> [Address] -> IO Natural
httpBalance_ net addrs = do
    v <- httpJsonGet opts url
    return $ fromIntegral $ sum $ v ^.. values . key "confirmed" . _Integer
  where
    url = baseUrl net <> "/address/balances"
    opts = HTTP.defaults & HTTP.param "addresses" .~ [aList]
    aList = Text.intercalate "," $ addrStr net <$> addrs

httpUnspent :: Network -> [Address] -> IO [(OutPoint, ScriptOutput, Natural)]
httpUnspent net = (mconcat <$>) . mapM (httpUnspent_ net) . chunksOf 50

httpUnspent_ :: Network -> [Address] -> IO [(OutPoint, ScriptOutput, Natural)]
httpUnspent_ net addrs = do
    v <- httpJsonGet opts url
    let resM = mapM parseCoin $ v ^.. values
    maybe (exitError "Could not parse coin") return resM
  where
    url = baseUrl net <> "/address/unspent"
    opts = HTTP.defaults & HTTP.param "addresses" .~ [aList]
    aList = Text.intercalate "," $ addrStr net <$> addrs
    parseCoin v = do
        tid <- hexToTxHash =<< v ^? key "txid" . _String
        pos <- v ^? key "index" . _Integral
        val <- v ^? key "output" . key "value" . _Integral
        scpHex <- v ^? key "output" . key "pkscript" . _String
        scp <- eitherToMaybe . decodeOutputBS =<< decodeHex scpHex
        return (OutPoint tid pos, scp, val)

httpDetailedTx :: Network -> [Address] -> IO [DetailedTx]
httpDetailedTx net addrs = do
    aTxs <- httpAddressTxs net addrs
    txVals <- httpTxs net $ nub $ snd <$> aTxs
    return $ toDetailedTx net (fst <$> aTxs) <$> txVals

toDetailedTx :: Network -> [Address] -> Json.Value -> DetailedTx
toDetailedTx net addrs v =
    DetailedTx
    { detailedTxHash = hexToTxHash =<< v ^? key "txid" . _String
    , detailedTxSize = v ^? key "size" . _Integral
    , detailedTxOutbound = othOs
    , detailedTxNonStdOutputs = os Map.! Nothing
    , detailedTxInbound = Map.map (,Nothing) myOs
    , detailedTxMyInputs = Map.map (,Nothing) myIs
    , detailedTxOtherInputs = othIs
    , detailedTxNonStdInputs = is Map.! Nothing
    , detailedTxFee = v ^? key "fee" . _Integral
    , detailedTxHeight = v ^? key "block" . key "height" . _Integral
    , detailedTxBlockHash =
          hexToBlockHash =<< v ^? key "block" . key "hash" . _String
    }
  where
    is, os :: Map (Maybe Address) Natural
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

httpBestHeight :: Network -> IO Natural
httpBestHeight net = do
    v <- httpJsonGet HTTP.defaults url
    let resM = fromIntegral <$> v ^? key "height" . _Integer
    maybe err return resM
  where
    url = baseUrl net <> "/block/best"
    err = exitError "Could not get the best block height"

httpBroadcastTx :: Network -> Tx -> IO ()
httpBroadcastTx net tx = do
    _ <- HTTP.postWith (addStatusCheck HTTP.defaults) url val
    return ()
  where
    url = baseUrl net <> "/transaction"
    val =
        J.object ["transaction" J..= J.String (encodeHex $ S.encode tx)]

httpJsonGet :: HTTP.Options -> String -> IO Json.Value
httpJsonGet = httpJsonGen id

httpJsonGen ::
       (HTTP.Response BL.ByteString -> HTTP.Response BL.ByteString)
    -> HTTP.Options
    -> String
    -> IO Json.Value
httpJsonGen f opts url = do
    r <- HTTP.asValue . f =<< HTTP.getWith (addStatusCheck opts) url
    return $ r ^. HTTP.responseBody

checkStatus :: ResponseChecker
checkStatus _ r
    | statusIsSuccessful status = return ()
    | otherwise =
        exitCustomError $
        vcat
            [ errorDoc "Received an HTTP error response:"
            , nest 4 $
              vcat
                  [ keyDoc 10 "Status:" <> errorDoc (text $ show code)
                  , keyDoc 10 "Message:" <>
                    text (cs message)
                  ]
            ]
  where
    code = r ^. HTTP.responseStatus . HTTP.statusCode
    message = r ^. HTTP.responseStatus . HTTP.statusMessage
    status = mkStatus code message

addStatusCheck :: HTTP.Options -> HTTP.Options
addStatusCheck opts = opts & (HTTP.checkResponse ?~ checkStatus)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i xs = take i xs : chunksOf i (drop i xs)

-}
