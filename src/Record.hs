{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Record (Rate(..), Trades(..), Kline(..), Margin(..), Assets(..), 
                Order(..), Execution(..), symbol
) where

import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Text

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

ask :: Associated s ("ask" ':> Text) => Lens' (Record s) Text 
ask = #ask

bid :: Associated s ("bid" ':> Text) => Lens' (Record s) Text 
bid = #bid

high :: Associated s ("high" ':> Text) => Lens' (Record s) Text 
high = #high

last :: Associated s ("last" ':> Text) => Lens' (Record s) Text 
last = #last

low :: Associated s ("low" ':> Text) => Lens' (Record s) Text 
low = #low

symbol :: Associated s ("symbol" ':> Text) => Lens' (Record s) Text 
symbol = #symbol

timestamp :: Associated s ("timestamp" ':> Text) => Lens' (Record s) Text 
timestamp = #timestamp

volume :: Associated s ("volume" ':> Text) => Lens' (Record s) Text 
volume = #volume


type Trades = Record
  [ "price" :> Text,
    "side" :> Text,
    "size" :> Text,
    "timestamp" :> Text
  ]

price :: Associated s ("price" ':> Text) => Lens' (Record s) Text 
price = #price

side :: Associated s ("side" ':> Text) => Lens' (Record s) Text 
side = #side

size :: Associated s ("size" ':> Text) => Lens' (Record s) Text 
size = #size

type Kline = Record
  [ "openTime" :> Text,
    "open" :> Text,
    "high" :> Text,
    "low" :> Text,
    "close" :> Text,
    "volume" :> Text
  ]

openTime :: Associated s ("openTime" ':> Text) => Lens' (Record s) Text 
openTime = #openTime

open :: Associated s ("open" ':> Text) => Lens' (Record s) Text 
open = #open

close :: Associated s ("close" ':> Text) => Lens' (Record s) Text 
close = #close

type Margin = Record
  [ 
    "actualProfitLoss":> Text,
    "availableAmount":> Text,
    "margin":> Text,
    "marginCallStatus":> Text,
    "marginRatio":> Text,
    "profitLoss":> Text
  ]

actualProfitLoss :: Associated s ("actualProfitLoss" ':> Text) => Lens' (Record s) Text 
actualProfitLoss = #actualProfitLoss

availableAmount :: Associated s ("availableAmount" ':> Text) => Lens' (Record s) Text 
availableAmount = #availableAmount

margin :: Associated s ("margin" ':> Text) => Lens' (Record s) Text 
margin = #margin

marginCallStatus :: Associated s ("marginCallStatus" ':> Text) => Lens' (Record s) Text 
marginCallStatus = #marginCallStatus

marginRatio :: Associated s ("marginRatio" ':> Text) => Lens' (Record s) Text 
marginRatio = #marginRatio

profitLoss :: Associated s ("profitLoss" ':> Text) => Lens' (Record s) Text 
profitLoss = #profitLoss

type Assets = Record 
  [
    "amount":> Text,
    "available":> Text,
    "conversionRate":> Text,
    "symbol":> Text
  ]

amout :: Associated s ("amout" ':> Text) => Lens' (Record s) Text 
amout = #amout

available :: Associated s ("available" ':> Text) => Lens' (Record s) Text 
available = #available

conversionRate :: Associated s ("conversionRate" ':> Text) => Lens' (Record s) Text 
conversionRate = #conversionRate


type Order = Record
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

orderId :: Associated s ("orderId" ':> Text) => Lens' (Record s) Text 
orderId = #orderId

rootOrderId :: Associated s ("rootOrderId" ':> Text) => Lens' (Record s) Text 
rootOrderId = #rootOrderId

orderType :: Associated s ("orderType" ':> Text) => Lens' (Record s) Text 
orderType = #orderType

executionType :: Associated s ("executionType" ':> Text) => Lens' (Record s) Text 
executionType = #executionType

settleType :: Associated s ("settleType" ':> Text) => Lens' (Record s) Text 
settleType = #settleType

executedSize :: Associated s ("executedSize" ':> Text) => Lens' (Record s) Text 
executedSize = #executedSize

losscutPrice :: Associated s ("losscutPrice" ':> Text) => Lens' (Record s) Text 
losscutPrice = #losscutPrice

status :: Associated s ("status" ':> Text) => Lens' (Record s) Text 
status = #status

timeInForce :: Associated s ("timeInForce" ':> Text) => Lens' (Record s) Text 
timeInForce = #timeInForce


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

executionId :: Associated s ("executionId" ':> Text) => Lens' (Record s) Text 
executionId = #executionId

positionId :: Associated s ("positionId" ':> Text) => Lens' (Record s) Text 
positionId = #positionId

lossGain :: Associated s ("lossGain" ':> Text) => Lens' (Record s) Text 
lossGain = #lossGain

fee :: Associated s ("fee" ':> Text) => Lens' (Record s) Text 
fee = #fee