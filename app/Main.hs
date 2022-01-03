{-# LANGUAGE OverloadedStrings #-}  
module Main where

import App.Reply
import App.Notate
import App.Seed
import Gmo.ToRecord
import Gmo.WsApi
import Common
import qualified Data.Vector as V
import Redis
import Model
import StoreSql
import Record
import DrawChart
import Rabbit.Consumer
import Rabbit.Producer
import Database.Redis
import Database.SQLite.Simple
import Control.Concurrent

{-main = do
  xs <- selectData "select * from ETH_year_4hour;" "klines.db" []
  drawKlines "eth1.png" xs-}

main = do
  forkIO consumer'
  threadDelay (1 * 10^6) >> produceCommand "redisExchg" "get.some" "test"
  

--mapM_ storeYear_4hour [minBound .. maxBound]


{-main:: IO ()
main =  extractRatesFromWs BTC regist_ 
  where 
    regist_ :: Maybe Rate -> IO ()
    regist_ (Just rate) = do
      conn <- checkedConnect defaultConnectInfo 
      runRedis conn (uploadRate 0 rate)
    regist_ _ = print "error" -}


