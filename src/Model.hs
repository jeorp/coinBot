{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE TemplateHaskell #-}
module Model where

import Record

import Data.Extensible
import Control.Lens hiding ((:>))
import qualified Data.Text as T 
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Int


toDouble :: T.Text -> Double 
toDouble = read . T.unpack

data Order' = Order' 
  {
    _price :: Double,
    _size :: Double
  } deriving (Show, Eq)

fromOrder :: Order -> Order'
fromOrder order = Order' (toDouble (order ^. #price)) (toDouble (order ^. #size))

data Kline' = Kline' 
  {
    _klineOpenTime :: UTCTime ,
    _klineOpen :: Double,
    _klineHigh :: Double,
    _klineLow :: Double,
    _klineClose :: Double,
    _klineVolume :: Double
  } deriving (Show, Eq)

makeLenses ''Kline'

fromKline :: Kline -> Kline'
fromKline kl = 
  let unix = read (T.unpack (kl ^. #openTime)) :: Int64
      sysTime = MkSystemTime (unix `div` 1000) 0
  in Kline' 
      (systemToUTCTime sysTime) 
      (toDouble (kl ^. #open)) 
      (toDouble (kl ^. #high))
      (toDouble (kl ^. #low))
      (toDouble (kl ^. #close)) 
      (toDouble (kl ^. #volume))
  
data Rate' = Rate' 
  {
    _rateAsk :: Double,
    _rateBid :: Double, 
    _rateHigh :: Double,
    _rateLow :: Double,
    _rateSymbol :: T.Text,
    _rateTimestamp :: T.Text,
    _rateVolume :: Double
  }

makeLenses ''Rate'

fromRate :: Rate -> Rate'
fromRate r =
  Rate' 
    (r ^. #ask . to toDouble)
    (r ^. #bid . to toDouble) 
    (r ^. #high . to toDouble)
    (r ^. #low . to toDouble)
    (r ^. #symbol)
    (r ^. #timestamp)
    (r ^. #volume . to toDouble)

data Margin' = Margin' 
  {
    _MactualProfitLoss :: Double,
    _MavailableAmount :: Double,
    _Mmargin :: Double,
    _MmarginCallStatus :: T.Text,
    _MprofitLoss :: Double
  }

fromMargin :: Margin -> Margin'
fromMargin m = 
  Margin' 
    (m ^. #actualProfitLoss . to toDouble)
    (m ^. #availableAmount . to toDouble)
    (m ^. #margin . to toDouble)
    (m ^. #marginCallStatus)
    (m ^. #profitLoss . to toDouble)

data Assets' = Assets'
  {
    _Aamount :: Double,
    _Aavailable :: Double,
    _AconversionRate :: Double,
    _Asymbol :: T.Text
  }

fromAssets :: Assets -> Assets'
fromAssets a = 
  Assets' 
    (a ^. #amount . to toDouble)
    (a ^. #available . to toDouble)
    (a ^. #conversionRate . to toDouble)
    (a ^. #symbol)

