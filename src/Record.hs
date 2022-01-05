{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE OverloadedStrings  #-}

module Record  where

import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Text
import Data.Aeson
import Time
import Data.Time.Clock

type Rate = Record
  [ "ask" :> Text,
    "bid" :> Text,
    "high" :> Text,
    "last" :> Text,
    "low" :> Text,
    "symbol" :> Text,
    "timestamp" :> Text,
    "volume" :> Text
  ]


rateToText :: Rate -> Text
rateToText r = intercalate "\n" 
  [
    "symbol : " <> r ^. #symbol,
    "ask : " <> r ^. #ask,
    "bid : " <> r ^. #bid,
    "high : " <> r ^. #high,
    "last : " <> r ^. #last,
    "low : " <> r ^. #low,
    "timestamp : " <>  (r ^. #timestamp . to (pack . show . utcToTokyoTime . parseUtc)),
    "volume : " <> r ^. #volume
  ]
            

type Order = Record
  [ "price" :> Text,
    "size" :> Text
  ]

type OrderBooks = Record
  [
    "asks" :> [Order],
    "bids" :> [Order]
  ]

type Trades = Record
  [ "price" :> Text,
    "side" :> Text,
    "size" :> Text,
    "timestamp" :> Text
  ]

type Kline = Record
  [ "openTime" :> Text,
    "open" :> Text,
    "high" :> Text,
    "low" :> Text,
    "close" :> Text,
    "volume" :> Text
  ]


type Margin = Record
  [ 
    "actualProfitLoss":> Text,
    "availableAmount":> Text,
    "margin":> Text,
    "marginCallStatus":> Text,
    "profitLoss":> Text
  ]

marginToText :: Margin -> Text
marginToText m = intercalate "\n"
  [
    "actualProfitLoss : " <> m ^. #actualProfitLoss,
    "availableAmount : " <> m ^. #availableAmount,
    "margin : " <> m ^. #margin ,
    "marginCallStatus : " <> m ^. #marginCallStatus,
    "profitLoss : " <> m ^. #profitLoss
  ]

type Assets = Record 
  [
    "amount":> Text,
    "available":> Text,
    "conversionRate":> Text,
    "symbol":> Text
  ]

assetsToText :: Assets -> Text
assetsToText a = intercalate "\n"
  [
    "amount : " <> a ^. #amount,
    "available : " <> a ^. #available,
    "conversionRate : " <> a ^. #conversionRate,
    "symbol : " <> a ^. #symbol
  ]

type OrderInfo = Record
  [
    "orderId":> Text,
    "rootOrderId":> Text,
    "symbol":> Text,
    "side":> Text,
    "orderType":> Text,
    "executionType":> Text,
    "settleType":> Text,
    "size":> Text,
    "executedSize":> Text,
    "price":> Text,
    "losscutPrice":> Text,
    "status":> Text,
    "timeInForce":> Text,
    "timestamp":> Text
  ]


type Execution = Record
  [
    "executionId":> Text,
    "orderId":> Text,
    "positionId":> Text,
    "symbol":> Text,
    "side":> Text,
    "settleType":> Text,
    "size":> Text,
    "price":> Text,
    "lossGain":> Text,
    "fee":> Text,
    "timestamp":> Text
  ]
