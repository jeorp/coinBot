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

produceCommand :: T.Text -> T.Text -> String -> IO ()
produceCommand exName ex ms = producerIO $ \chan -> sendMs chan exName ex ms

sendMs :: Channel -> T.Text -> T.Text -> String -> IO ()
sendMs chan exName ex ms = do
    putStrLn $ "send : " <> ms 
    void $ publishMsg chan exName ex (newMsg {msgBody = BL.pack ms, msgDeliveryMode = Just NonPersistent})

sendData :: Channel -> T.Text -> T.Text -> BL.ByteString -> IO ()
sendData chan exName ex ms = do
    putStrLn "send some data ..."
    void $ publishMsg chan exName ex (newMsg {msgBody = ms, msgDeliveryMode = Just NonPersistent})