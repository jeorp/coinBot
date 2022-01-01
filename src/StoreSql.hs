{-# LANGUAGE OverloadedStrings #-}

module StoreSql where

import System.Directory 
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Applicative 
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow 

import Model

instance FromRow Kline' where
  fromRow = Kline' <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Kline' where
  toRow (Kline' time open high low close volume) = toRow (time, open, high, low, close, volume)

klineMigrateQuery :: Query -> Query 
klineMigrateQuery table = "create table " <> table <> " (_openTime text primary key, _open real not null, _high real not null, _low real not null, _close real not null, _volue real not null)"


openDatabase :: MonadIO m => m Connection -> (Connection -> m a ) -> m a
openDatabase ioConnect connectIO = ioConnect >>= (\conn -> connectIO conn <* liftIO (close conn))


migrateModel :: (MonadIO m, MonadCatch m) => Query -> String -> (SQLError -> m ()) -> m ()
migrateModel query path errorHandle = do
    -- does File exists then return () else action
    bool <- liftIO $ doesFileExist path
    if bool then return () 
        else openDatabase (liftIO $ open path) $ \conn ->
            liftIO (execute_ conn query) `catch` errorHandle

selectData :: (FromRow q, MonadIO m) => Query -> String -> m [q]
selectData query path = do
    -- does File exists then action else return ()
    bool <- liftIO $ doesFileExist path
    if bool then openDatabase (liftIO $ open path) $ liftIO . flip query_ query
        else return [] 


testKline :: Kline'
testKline = Kline' "2021/11/24" 1.11111 2.2989999 3.3333 4.0 5.0


migrateKline :: (MonadIO m, MonadCatch m) => Query -> String -> (SQLError -> m ()) -> m ()
migrateKline = migrateModel . klineMigrateQuery


insertKline :: MonadIO m => String -> Query -> Kline' -> m ()
insertKline path table kline = do
    openDatabase (liftIO $ open path) $ \conn -> liftIO $ execute conn ("insert into" <> table <> "values (?, ?, ?, ?, ?, ?)") kline
