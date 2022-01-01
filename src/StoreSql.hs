{-# LANGUAGE OverloadedStrings #-}

module StoreSql where

import Control.Exception.Safe
import Control.Applicative ()
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow 

import Model

instance FromRow Kline' where
  fromRow = Kline' <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Kline' where
  toRow (Kline' time open high low close volume) = toRow (time, open, high, low, close, volume)

klineMigrateQuery :: Query -> Query 
klineMigrateQuery table = "create table " <> table <> " (_openTime text primary key, _open real not null, _high real not null, _low real not null, _close real not null, _volue real not null)"


