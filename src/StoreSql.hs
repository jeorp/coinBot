{-# LANGUAGE OverloadedStrings #-}

module StoreSql where

import System.Directory 
import Control.Exception.Safe
import Control.Monad.IO.Class
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow 
import qualified Data.Text as T 
import qualified Data.Vector as V 
import Model

instance FromRow Kline' where
  fromRow = Kline' <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Kline' where
  toRow (Kline' time open high low close volume) = toRow (time, open, high, low, close, volume)

klineMigrateQuery :: Query -> Query 
klineMigrateQuery table = 
  "create table " <> 
  table <> 
  " ( opentime text not null, open real not null, high real not null, low real not null, close real not null, volume real not null)"


openDatabase :: (MonadIO m, MonadCatch m) => m Connection -> (Connection -> m a ) -> m a
openDatabase ioConnect connectIO = ioConnect >>= (\conn -> connectIO conn <* liftIO (close conn))


migrateModel :: (MonadIO m, MonadCatch m) => Query -> String -> (SQLError -> m ()) -> m ()
migrateModel query path errorHandle = do
    openDatabase (liftIO $ open path) $ \conn ->
      liftIO (execute_ conn query) `catch` errorHandle

flushTableData :: (MonadIO m, MonadCatch m) => String -> String -> (SQLError -> m ()) -> m ()
flushTableData table_name path errorHandler = do
    openDatabase (liftIO $ open path) $ \conn ->
      liftIO (execute_ conn ("delete from " <> Query (T.pack table_name) <> " ;")) `catch` errorHandler

--select example
selectData :: (FromRow q, MonadIO m, MonadCatch m) => Query -> String -> [Handler m [q]] -> m [q]
selectData query path errorHnadle = do
    -- does File exists then action else return ()
    bool <- liftIO $ doesFileExist path
    if bool 
      then openDatabase (liftIO $ open path) $ 
        \c -> liftIO (query_ c query )`catches` errorHnadle
      else return [] 

-- input 
-- @ name of table 
-- @ db path 
-- @ if error
migrateKline :: (MonadIO m, MonadCatch m) => Query -> String -> (SQLError -> m ()) -> m ()
migrateKline = migrateModel . klineMigrateQuery


-- insert example 
insertKline :: (MonadIO m, MonadCatch m) => String -> Query -> Kline' -> [Handler m ()] -> m ()
insertKline path table kline errorHandl = do
    openDatabase (liftIO $ open path) $ \conn -> 
      liftIO (execute conn ("insert into " <> table <> " values (?, ?, ?, ?, ?, ?)") kline)
      `catches` 
      errorHandl

insertKlines :: (MonadIO m, MonadCatch m) => String -> Query -> V.Vector Kline' -> [Handler m ()] -> m ()
insertKlines path table klines errorHandl = do
    openDatabase (liftIO $ open path) $ \conn -> mapM_ (store conn) klines
    where 
      store conn kline = liftIO (execute conn ("insert into " <> table <> " values (?, ?, ?, ?, ?, ?)") kline)
       `catches` 
       errorHandl