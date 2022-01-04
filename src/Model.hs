{-# LANGUAGE OverloadedLabels  #-}
module Model where

import Record

import Data.Extensible
import Control.Lens hiding ((:>))
import qualified Data.Text as T 
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Int

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
    _klineOpenTime :: UTCTime ,
    _klineOpen :: Float,
    _klineHigh :: Float,
    _klineLow :: Float,
    _klineClose :: Float,
    _klineVolume :: Float
  } deriving (Show, Eq)


fromKline :: Kline -> Kline'
fromKline kl = 
  let unix = read (T.unpack (kl ^. #openTime)) :: Int64
      sysTime = MkSystemTime (unix `div` 1000) 0
  in Kline' 
      (systemToUTCTime sysTime) 
      (toFloat (kl ^. #open)) 
      (toFloat (kl ^. #high))
      (toFloat (kl ^. #low))
      (toFloat (kl ^. #close)) 
      (toFloat (kl ^. #volume))
  
data Rate' = Rate' 
  {
    _rateAsk :: Float,
    _rateBid :: Float, 
    _rateHigh :: Float,
    _rateLow :: Float,
    _rateSymbol :: T.Text,
    _rateTimestamp :: T.Text,
    _rateVolume :: Float
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