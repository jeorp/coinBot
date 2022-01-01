{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RankNTypes #-}
module Redis where

import Database.Redis
import Control.Lens ((^.), to, Lens')
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding
import StoreSql
import Record
import Common

uploadRate :: Integer -> Rate -> Redis ()
uploadRate index rate = do
  select index
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
