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
import Data.Extensible
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding
import StoreSql
import Record
import Common

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
fromRate r = undefined

getRateRedis :: B.ByteString -> Redis Rate'
getRateRedis key = undefined




{-
getRateRedis :: B.ByteString -> Redis Rate
getRateRedis key = do
  all <- hgetall key
  let xs = fromRight [] all
      lookup_ = T.pack . B.unpack . fromMaybe "" . flip lookup xs
      rate = #ask @= lookup_ "ask"
          <: #bid @= lookup_ "bid" 
          <: #high @= lookup_ "high"
          <: #last @= lookup_ "last"
          <: #low @= lookup_ "low"
          <: #symbol @= lookup_ "symbol"
          <: #timestamp @= lookup_ "timestamp"
          <: #volume @= lookup_ "volume" 
          <: emptyRecord

  return rate
-}
