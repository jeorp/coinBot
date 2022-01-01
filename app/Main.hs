{-# LANGUAGE OverloadedStrings #-}  
module Main where

import App.Reply
import App.Notate
import Gmo.ToRecord
import Gmo.WsApi
import Common
import qualified Data.Vector as V
import Redis
import Model
import Record
import Database.Redis


main:: IO ()
main =  extractRatesFromWs BTC regist_ 
  where 
    regist_ :: Maybe Rate -> IO ()
    regist_ (Just rate) = do
      conn <- checkedConnect defaultConnectInfo 
      runRedis conn (uploadRate 0 rate)
    regist_ _ = print "error"


