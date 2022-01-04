{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE OverloadedStrings  #-}
module Redis where

import Data.Either
import Data.Maybe
import Database.Redis
import Control.Lens ((^.), to, Lens')
import Control.Arrow
import Control.Monad
import Control.Exception
--import Data.Extensible
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding
import StoreSql
import Record
import Common

executeRedis :: Redis () -> IO ()
executeRedis redis = bracket (checkedConnect defaultConnectInfo) (`runRedis` redis) disconnect

uploadRateRedis :: Rate -> Redis ()
uploadRateRedis rate = do
  s <- hmset (rate ^. #symbol . to encodeUtf8<> "_rate") $ second encodeUtf8 <$>
    [
      ("ask", rate ^. #ask),
      ("bid", rate ^. #bid),
      ("high", rate ^. #high),
      ("last", rate ^. #last),
      ("low", rate ^. #low),
      ("symbol", rate ^. #symbol),
      ("timestamp", rate ^. #timestamp),
      ("volume", rate ^. #volume)    
    ]
  liftIO $ print s

data Rate' = Rate' 
  {
    _ask :: Float,
    _bid :: Float, 
    _high :: Float,
    _low :: Float,
    _symbol :: T.Text,
    _timestamp :: T.Text,
    _volume :: Float
  }

fromRate :: Rate -> Rate'
fromRate r =
  Rate' 
    (r ^. #ask . to T.unpack  . to read)
    (r ^. #bid . to T.unpack  . to read) 
    (r ^. #high . to T.unpack  . to read)
    (r ^. #low . to T.unpack  . to read)
    (r ^. #symbol)
    (r ^. #timestamp)
    (r ^. #volume . to T.unpack . to read)

getRateRedis :: B.ByteString -> Redis Rate'
getRateRedis key = do
  all <- hgetall key
  let xs = fromRight [] all
      lookup_ = B.unpack . fromMaybe "" . flip lookup xs
      rate = Rate' 
        (lookup_ "ask" ^. to read)
        (lookup_ "bid" ^. to read)
        (lookup_ "high" ^. to read)
        (lookup_ "low" ^. to read)
        (lookup_ "symbol" ^. to T.pack)
        (lookup_ "timestamp" ^. to T.pack)
        (lookup_ "volume" ^. to read)
  return rate

