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
import Record
import Database.Redis

main :: IO ()
main = mapM_ (seeds ETH "_year_4hour" "4hour") ["2018", "2019", "2020"]


{-main:: IO ()
main =  extractRatesFromWs BTC regist_ 
  where 
    regist_ :: Maybe Rate -> IO ()
    regist_ (Just rate) = do
      conn <- checkedConnect defaultConnectInfo 
      runRedis conn (uploadRate 0 rate)
    regist_ _ = print "error" -}


