{-# LANGUAGE OverloadedLabels  #-}
module Model where

import Record

import Data.Extensible
import Control.Lens hiding ((:>))
import qualified Data.Text as T 

toFloat :: T.Text -> Float 
toFloat = read . T.unpack

data Order' = Order' 
  {
    _price :: Float,
    _size :: Float
  } deriving (Show, Eq)

fromOrder :: Order -> Order'
fromOrder order = Order' (toFloat (order ^. #price)) (toFloat (order ^. #size))

data Kline' = Kline' 
  {
    _openTime :: T.Text,
    _open :: Float,
    _high :: Float,
    _low :: Float,
    _close :: Float,
    _volume :: Float
  } deriving (Show, Eq)


fromKline :: Kline -> Kline'
fromKline kl = Kline' 
  (kl ^. #openTime) 
  (toFloat (kl ^. #open)) 
  (toFloat (kl ^. #high))
  (toFloat (kl ^. #low))
  (toFloat (kl ^. #close)) 
  (toFloat (kl ^. #volume))