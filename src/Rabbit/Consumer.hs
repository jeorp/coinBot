{-# LANGUAGE OverloadedStrings #-}
module Rabbit.Consumer where

import Redis
import Notate
import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL


consumer' :: IO ()
consumer' = do
    conn <- openConnection "127.0.0.1" "/" "guest" "guest"
    chan <- openChannel conn

    
    --declare queues, exchanges and bindings
    declareQueue chan newQueue {queueName = "myQueueDE"}
    declareQueue chan newQueue {queueName = "myQueueEN"}
    
    declareExchange chan newExchange {exchangeName = "topicExchg", exchangeType = "topic"}
    bindQueue chan "myQueueDE" "topicExchg" "de.*"
    bindQueue chan "myQueueEN" "topicExchg" "en.*"
    

    --subscribe to the queues
    consumeMsgs chan "myQueueDE" Ack myCallbackDE
    consumeMsgs chan "myQueueEN" Ack myCallbackEN
    
    
    getLine -- wait for keypress
    closeConnection conn
    putStrLn "connection closed"

    
    

myCallbackDE :: (Message,Envelope) -> IO ()
myCallbackDE (msg, env) = do
    putStrLn $ "received from DE: "++(BL.unpack $ msgBody msg)
    ackEnv env


myCallbackEN :: (Message,Envelope) -> IO ()
myCallbackEN (msg, env) = do
    print $ envRoutingKey env
    putStrLn $ "received from EN: "++(BL.unpack $ msgBody msg)
    ackEnv env