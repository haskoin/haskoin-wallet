{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
module Network.Haskoin.Wallet.HTTP

where

import           Control.Lens                ((.~), (?~), (^.))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Monoid
import qualified Data.Serialize              as S
import           Data.String.Conversions     (cs)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Haskoin.Address
import           Haskoin.Block
import           Haskoin.Constants
import           Haskoin.Keys
import qualified Haskoin.Store.Data          as Store
import           Haskoin.Transaction
import           Network.Haskoin.Wallet.Util
import           Network.HTTP.Types.Status
import qualified Network.Wreq                as HTTP
import           Network.Wreq.Types          (ResponseChecker)
import           Numeric.Natural

apiHost :: Network -> String
apiHost net = "https://api.haskoin.com" </> getNetworkName net

-- | Make a call to the haskoin-store API.
--
-- Usage:
-- > apiCall (AddressTxs addrs) noOpts
--
-- Add an option:
-- > apiCall (AddressUnspent addrs) (optLimit 10)
--
-- Multiple options can be passed with <>:
-- > apiCall (AddressUnspent addrs) (optOffset 5 <> optLimit 10)
--
-- Options are of type (Endo Network.Wreq.Options) so you can customize them.
--
-- Batch commands that have a large list of arguments:
-- > apiBatch 20 (AddressTxs addrs) noOpts
apiCall ::
       (MonadIO m, MonadError String m, MonadReader Network m, S.Serialize a)
    => ApiResource a
    -> Endo HTTP.Options
    -> m a
apiCall res opts = do
    net <- network
    args <- liftEither $ resourceArgs net res
    let url = apiHost net <> resourcePath net res
    case res of
        PostTx tx -> postBinary (args <> opts) url tx
        _ -> getBinary (args <> opts) url

apiBatch ::
       ( MonadIO m
       , MonadError String m
       , MonadReader Network m
       , S.Serialize a
       , Monoid a
       )
    => Natural
    -> ApiResource a
    -> Endo HTTP.Options
    -> m a
apiBatch i res opts = mconcat <$> mapM (`apiCall` opts) (resourceBatch i res)

{- API Resources -}

data ApiResource a where
    AddressTxs :: [Address] -> ApiResource [Store.TxRef]
    AddressTxsFull :: [Address] -> ApiResource [Store.Transaction]
    AddressBalances :: [Address] -> ApiResource [Store.Balance]
    AddressUnspent :: [Address] -> ApiResource [Store.Unspent]
    XPubEvict :: XPubKey -> ApiResource (Store.GenericResult Bool)
    XPubSummary :: XPubKey -> ApiResource Store.XPubSummary
    XPubTxs :: XPubKey -> ApiResource [Store.TxRef]
    XPubTxsFull :: XPubKey -> ApiResource [Store.Transaction]
    XPubBalances :: XPubKey -> ApiResource [Store.XPubBal]
    XPubUnspent :: XPubKey -> ApiResource [Store.XPubUnspent]
    TxsDetails :: [TxHash] -> ApiResource [Store.Transaction]
    PostTx :: Tx -> ApiResource Store.TxId
    TxsRaw :: [TxHash] -> ApiResource [Tx]
    TxAfter :: TxHash -> Natural -> ApiResource (Store.GenericResult Bool)
    TxsBlock :: BlockHash -> ApiResource [Store.Transaction]
    TxsBlockRaw :: BlockHash -> ApiResource [Tx]
    Mempool :: ApiResource [TxHash]
    Events :: ApiResource [Store.Event]
    BlockBest :: ApiResource Store.BlockData
    BlockBestRaw :: ApiResource (Store.GenericResult Block)
    BlockLatest :: ApiResource [Store.BlockData]
    Blocks :: [BlockHash] -> ApiResource [Store.BlockData]
    BlockRaw :: BlockHash -> ApiResource (Store.GenericResult Block)
    BlockHeight :: Natural -> ApiResource [Store.BlockData]
    BlockHeightRaw :: Natural -> ApiResource [Block]
    BlockHeights :: [Natural] -> ApiResource [Store.BlockData]
    BlockTime :: Natural -> ApiResource Store.BlockData
    BlockTimeRaw :: Natural -> ApiResource (Store.GenericResult Block)
    Health :: ApiResource Store.HealthCheck
    Peers :: ApiResource [Store.PeerInformation]

{- API Options -}

noOpts :: Endo HTTP.Options
noOpts = Endo id

optBlockHeight :: Natural -> Endo HTTP.Options 
optBlockHeight h = applyOpt "height" [cs $ show h]

optBlockHash :: BlockHash -> Endo HTTP.Options 
optBlockHash h = applyOpt "height" [blockHashToHex h]

optUnix :: Natural -> Endo HTTP.Options 
optUnix u = applyOpt "height" [cs $ show u]

optTxHash :: TxHash -> Endo HTTP.Options 
optTxHash h = applyOpt "height" [txHashToHex h]

optOffset :: Natural -> Endo HTTP.Options 
optOffset o = applyOpt "offset" [cs $ show o]

optLimit :: Natural -> Endo HTTP.Options 
optLimit l = applyOpt "limit" [cs $ show l]

optStandard :: Endo HTTP.Options 
optStandard = applyOpt "derive" ["standard"]

optSegwit :: Endo HTTP.Options 
optSegwit = applyOpt "derive" ["segwit"]

optCompat :: Endo HTTP.Options 
optCompat = applyOpt "derive" ["compat"]

optNoCache :: Endo HTTP.Options 
optNoCache = applyOpt "nocache" ["true"]

optNoTx :: Endo HTTP.Options 
optNoTx = applyOpt "notx" ["true"]

{- API Internal -}

resourceArgs :: Network -> ApiResource a -> Either String (Endo HTTP.Options)
resourceArgs net =
    \case
        AddressTxs as -> argAddrs net as
        AddressTxsFull as -> argAddrs net as
        AddressBalances as -> argAddrs net as
        AddressUnspent as -> argAddrs net as
        TxsDetails hs -> return $ applyOpt "txids" (txHashToHex <$> hs)
        TxsRaw hs -> return $ applyOpt "txids" (txHashToHex <$> hs)
        TxAfter h i ->
            return $
            applyOpt "txid" [txHashToHex h] <> applyOpt "height" [cs $ show i]
        TxsBlock b -> return $ applyOpt "block" [blockHashToHex b]
        TxsBlockRaw b -> return $ applyOpt "block" [blockHashToHex b]
        Blocks bs -> return $ applyOpt "blocks" (blockHashToHex <$> bs)
        BlockRaw b -> return $ applyOpt "block" [blockHashToHex b]
        BlockHeight i -> return $ applyOpt "height" [cs $ show i]
        BlockHeightRaw i -> return $ applyOpt "height" [cs $ show i]
        BlockHeights is -> return $ applyOpt "heights" (cs . show <$> is)
        BlockTime i -> return $ applyOpt "time" [cs $ show i]
        BlockTimeRaw i -> return $ applyOpt "time" [cs $show i]
        _ -> return noOpts

argAddrs :: Network -> [Address] -> Either String (Endo HTTP.Options)
argAddrs net addrs = do
    addrsTxt <- liftEither $ addrToTextE net `mapM` addrs
    return $ applyOpt "addresses" addrsTxt

resourcePath :: Network -> ApiResource a -> String
resourcePath net =
    \case
        AddressTxs {} -> "/address/transactions"
        AddressTxsFull {} -> "/address/transactions/full"
        AddressBalances {} -> "/address/balances"
        AddressUnspent {} -> "/address/unspent"
        XPubEvict pub -> "/xpub/" <> cs (xPubExport net pub) <> "/evict"
        XPubSummary pub -> "/xpub/" <> cs (xPubExport net pub)
        XPubTxs pub -> "/xpub/" <> cs (xPubExport net pub) <> "/transactions"
        XPubTxsFull pub ->
            "/xpub/" <> cs (xPubExport net pub) <> "/transactions/full"
        XPubBalances pub -> "/xpub/" <> cs (xPubExport net pub) <> "/balances"
        XPubUnspent pub -> "/xpub/" <> cs (xPubExport net pub) <> "/unspent"
        TxsDetails {} -> "/transactions"
        PostTx {} -> "/transactions"
        TxsRaw {} -> "/transactions"
        TxAfter h i ->
            "/transactions/" <> cs (txHashToHex h) <> "/after/" <> show i
        TxsBlock b -> "/transactions/block/" <> cs (blockHashToHex b)
        TxsBlockRaw b ->
            "/transactions/block/" <> cs (blockHashToHex b) <> "/raw"
        Mempool -> "/mempool"
        Events -> "/events"
        BlockBest -> "/block/best"
        BlockBestRaw -> "/block/best/raw"
        BlockLatest -> "/block/latest"
        Blocks {} -> "/blocks"
        BlockRaw b -> "/block/" <> cs (blockHashToHex b) <> "/raw"
        BlockHeight i -> "/block/height/" <> show i
        BlockHeightRaw i -> "/block/height/" <> show i <> "/raw"
        BlockHeights {} -> "/block/heights"
        BlockTime t -> "/block/time/" <> show t
        BlockTimeRaw t -> "/block/time/" <> show t <> "/raw"
        Health -> "/health"
        Peers -> "/peers"

resourceBatch :: Natural -> ApiResource a -> [ApiResource a]
resourceBatch i =
    \case
        AddressTxs xs -> AddressTxs <$> chunksOf i xs
        AddressTxsFull xs -> AddressTxsFull <$> chunksOf i xs
        AddressBalances xs -> AddressBalances <$> chunksOf i xs
        AddressUnspent xs -> AddressUnspent <$> chunksOf i xs
        TxsDetails xs -> TxsDetails <$> chunksOf i xs
        TxsRaw xs -> TxsRaw <$> chunksOf i xs
        Blocks xs -> Blocks <$> chunksOf i xs
        BlockHeights xs -> BlockHeights <$> chunksOf i xs
        res -> [res]

{- API Helpers -}

applyOpt :: Text -> [Text] -> Endo HTTP.Options
applyOpt p t = Endo $ HTTP.param p .~ [Text.intercalate "," t]

getBinary ::
       (MonadIO m, MonadError String m, S.Serialize a)
    => Endo HTTP.Options
    -> String
    -> m a
getBinary opts url = do
    res <- liftIO $ HTTP.getWith (binaryOpts opts) url
    liftEither $ S.decodeLazy $ res ^. HTTP.responseBody

postBinary ::
       (MonadIO m, MonadError String m, S.Serialize a, S.Serialize r)
    => Endo HTTP.Options
    -> String
    -> a
    -> m r
postBinary opts url body = do
    res <- liftIO $ HTTP.postWith (binaryOpts opts) url (S.encode body)
    liftEither $ S.decodeLazy $ res ^. HTTP.responseBody

binaryOpts :: Endo HTTP.Options -> HTTP.Options
binaryOpts opts =
    appEndo (opts <> accept <> stat) HTTP.defaults
  where
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

