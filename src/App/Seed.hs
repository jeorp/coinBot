{-# LANGUAGE OverloadedStrings #-}
module App.Seed where

import Control.Exception.Safe
import Database.SQLite.Simple
import Control.Monad.IO.Class
import qualified Data.Text as T 
import qualified Data.Vector as V
import Data.Time.Calendar
import Gmo.ToRecord
import StoreSql
import Model
import Common
import Time


db :: String
db = "klines.db"

seeds :: (MonadIO m, MonadCatch m) => Coin -> String -> String -> String -> m ()
seeds coin option interval date = do
  let q = Query $ T.pack $ show coin
      option_q = Query $ T.pack option
      table = q <> option_q
  migrateKline table db (liftIO . print)
  klines <- liftIO $ extractKlines coin interval date
  insertKlines db table (fromKline <$> klines) []--[Handler errorHandle]

errorHandle :: MonadIO m => SQLError -> m ()
errorHandle = liftIO . print

storeYear_4hour :: (MonadIO m, MonadCatch m) => Coin -> m ()
storeYear_4hour coin = mapM_ (seeds coin "_year_4hour" "4hour") ["2018", "2019", "2020"]

storeDayKline :: (MonadIO m, MonadCatch m) => Coin -> String -> String -> Day -> m ()
storeDayKline coin option_q interval day = seeds coin option_q interval (dayToString day)

storeFromDay :: (MonadIO m, MonadCatch m) => Coin -> String -> String -> Day -> m ()
storeFromDay coin option_q interval day = do
  today <- liftIO getToday
  loop day today
  where
    loop :: (MonadIO m, MonadCatch m) => Day -> Day -> m ()
    loop d t = if d > t 
      then liftIO $ putStrLn "StoreFromDay : end"
      else storeDayKline coin option_q interval d >> loop (addDays 1 d) t
      
storeFromNmonthAgo :: (MonadIO m, MonadCatch m) => Coin -> String -> String -> Int -> m ()
storeFromNmonthAgo coin option_q interval n = do
  day <- liftIO $ getNmonthAgo n
  storeFromDay coin option_q interval day



