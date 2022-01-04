{-# LANGUAGE OverloadedStrings #-}  
module Main where

import App.WsApp
import App.Reply
import App.Notate
import App.Seed
import Gmo.ToRecord
import Gmo.WsApi
import Common
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V
import Redis
import Model
import Time
import StoreSql
import Record
import DrawChart
import Rabbit.Consumer
import Rabbit.Producer
import Database.Redis
import Database.SQLite.Simple
import Control.Concurrent


main :: IO ()
main = do
  forkIO consumer'
  mapM_ startWsApp [minBound .. maxBound]
  broadcast 


