{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE OverloadedStrings  #-}
module Redis where

import Data.Either
import Data.Maybe
import Data.Traversable
import Database.Redis
import Control.Lens ((^.), to, Lens')
import Control.Arrow
import Control.Monad
import Control.Exception
--import Data.Extensible
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding
import StoreSql
import Record
import Common
import Model

executeRedis :: Redis () -> IO ()
executeRedis redis = bracket (checkedConnect defaultConnectInfo) (`runRedis` redis) disconnect

uploadRateRedis :: Rate -> Redis ()
uploadRateRedis rate = do
  s <- hmset (rate ^. #symbol . to encodeUtf8<> "_rate") $ second encodeUtf8 <$>
    [
      ("ask", rate ^. #ask),
      ("bid", rate ^. #bid),
      ("high", rate ^. #high),
      ("last", rate ^. #last),
      ("low", rate ^. #low),
      ("symbol", rate ^. #symbol),
      ("timestamp", rate ^. #timestamp),
      ("volume", rate ^. #volume)    
    ]
  liftIO $ print s


getRateRedis :: B.ByteString -> Redis Rate'
getRateRedis key = do
  all <- hgetall key
  let xs = fromRight [] all
      lookup_ = B.unpack . fromMaybe "" . flip lookup xs
      rate = Rate' 
        (lookup_ "ask" ^. to read)
        (lookup_ "bid" ^. to read)
        (lookup_ "high" ^. to read)
        (lookup_ "low" ^. to read)
        (lookup_ "symbol" ^. to T.pack)
        (lookup_ "timestamp" ^. to T.pack)
        (lookup_ "volume" ^. to read)
  return rate

getRatesRedis :: Redis [Rate']
getRatesRedis = do
  let xs = getRateRedis . B.pack . (<> "_rate") . show <$> ([minBound .. maxBound] :: [Coin])
  sequenceA xs

updateRank :: Integer -> B.ByteString -> (Rate' -> Double) -> Redis ()
updateRank i tag f = do
  rates <- getRatesRedis
  select i 
  zrem tag $ B.pack . show <$> ([minBound .. maxBound] :: [Coin])
  zadd tag $ (f &&& encodeUtf8 . _rateSymbol) <$> rates
  return ()


