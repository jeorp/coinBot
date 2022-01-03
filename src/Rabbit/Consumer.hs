{-# LANGUAGE OverloadedStrings #-}
module Rabbit.Consumer where

import Redis
import Notate
import Command
import Network.AMQP
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL


consumer' :: IO ()
consumer' = do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn

    
    --declare queues, exchanges and bindings
    registRedisQueue chan
    
    
    getLine -- wait for keypress
    closeConnection conn
    putStrLn "connection closed"


registRedisQueue :: Channel -> IO ()
registRedisQueue chan = void $ do
    declareQueue chan newQueue {queueName = "redisSet"}
    declareQueue chan newQueue {queueName = "redisGet"}
    
    declareExchange chan newExchange {exchangeName = "redisExchg", exchangeType = "topic"}
    bindQueue chan "redisSet" "redisExchg" "set.*"
    bindQueue chan "redisGet" "redisExchg" "get.*"
    
    consumeMsgs chan "redisSet" Ack myCallbackSet
    consumeMsgs chan "redisGet" Ack myCallbackGet
    where
      myCallbackSet :: (Message,Envelope) -> IO ()
      myCallbackSet (msg, env) = do
        putStrLn $ "received from Redis.Set: "++(BL.unpack $ msgBody msg)
        ackEnv env

      myCallbackGet :: (Message,Envelope) -> IO ()
      myCallbackGet (msg, env) = do
        print $ envRoutingKey env
        putStrLn $ "received from Redis.Get: "++(BL.unpack $ msgBody msg)
        ackEnv env