{-# LANGUAGE OverloadedStrings #-}
module Rabbit.Producer where

import Network.AMQP
import Control.Monad
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL

producerIO :: (Channel -> IO ()) -> IO ()
producerIO io = do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn
    io chan
    closeConnection conn

producer' :: T.Text -> T.Text -> String -> IO ()
producer' exName ex ms = producerIO $ \chan -> sendMs chan exName ex ms

sendMs :: Channel -> T.Text -> T.Text -> String -> IO ()
sendMs chan exName ex ms = 
    void $ publishMsg chan exName ex (newMsg {msgBody = BL.pack ms, msgDeliveryMode = Just NonPersistent})