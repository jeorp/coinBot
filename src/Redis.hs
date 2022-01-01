{-# LANGUAGE OverloadedStrings #-}
module Redis where

import Database.Redis
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString as B
import StoreSql

redis :: IO ()
redis = do  
  conn <- checkedConnect defaultConnectInfo 
  runRedis conn $ do
     set "hello" "hello"
     set "world" "world"
     hello <- get "hello"
     world <- get "world"
     liftIO $ print (hello,world)
  disconnect conn