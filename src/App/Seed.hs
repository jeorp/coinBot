{-# LANGUAGE OverloadedStrings #-}
module App.Seed where

import Control.Exception.Safe
import Database.SQLite.Simple
import Control.Monad.IO.Class
import qualified Data.Text as T 
import qualified Data.Vector as V
import Gmo.ToRecord
import StoreSql
import Model
import Common

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

