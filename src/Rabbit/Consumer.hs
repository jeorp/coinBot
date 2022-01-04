{-# LANGUAGE OverloadedStrings #-}
module Rabbit.Consumer where
import Common
import Record
import Redis
import Notate
import Command
import qualified Database.Redis as R
import Network.AMQP
import Control.Monad
import Data.Monoid
import Data.Maybe
import qualified Data.Aeson as A
import Data.Default.Class
import qualified Data.Text as T 
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
    
    consumeMsgs chan "redisSet" Ack callbackRedisSet
    consumeMsgs chan "redisGet" Ack callbackRedisGet
    where
      callbackRedisSet :: (Message,Envelope) -> IO ()
      callbackRedisSet (msg, env) = do
        putStrLn "received from Redis.Set"
        let input = T.unpack . T.drop (T.length "set.") $ envRoutingKey env
            isCoinSelect = getLast $ lookupFromRegisteredA input (def :: Coin)
        when (isJust isCoinSelect) $ doRedisSet isCoinSelect (msgBody msg)
        ackEnv env

      callbackRedisGet :: (Message,Envelope) -> IO ()
      callbackRedisGet (msg, env) = do
        print $ envRoutingKey env
        putStrLn $ "received from Redis.Get: "++(BL.unpack $ msgBody msg)
        ackEnv env

doRedisSet :: Maybe Coin -> BL.ByteString -> IO ()
doRedisSet (Just c) bl = do
  let rate = A.decode bl :: Maybe Rate
  storeRateInRedis rate
  where 
    storeRateInRedis :: Maybe Rate -> IO ()
    storeRateInRedis (Just r) = do
      putStrLn "doRedisSet : decode success"
      executeRedis $ do
        R.select 0
        uploadRateRedis r
    storeRateInRedis _ = putStrLn "doRedisSet : decode failure"

doRedisSet Nothing _ = putStrLn "doRedisSet : invalid coin select"