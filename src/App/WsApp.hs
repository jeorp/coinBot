{-# LANGUAGE OverloadedStrings #-}  
module App.WsApp where

import Common
import Gmo.WsApi
import Rabbit.Producer
import Rabbit.Consumer
import App.Reply
import qualified Data.Text as T
import Control.Monad
import Control.Concurrent
--import Control.Concurrent.Async

startWsApp :: Coin -> IO ()
startWsApp coin = do
  void $ forkIO $ runStreamWS $ getRateStream coin $ produceData "redisExchg" ("set." <> T.pack (show coin) ) 