{-# LANGUAGE OverloadedStrings #-}
module Analysis.Technical where

import Model
import Control.Lens
import StoreSql
import Control.Exception.Safe
import Control.Monad.IO.Class
import Database.SQLite.Simple

avAskBid :: Rate' -> Double
avAskBid r = (r ^. rateAsk + r ^. rateBid) / 2

difHighLow :: Rate' -> Double
difHighLow r = (r ^. rateHigh - r ^. rateLow) / avAskBid r

difToHigh :: Rate' -> Double
difToHigh r = (r ^. rateHigh - avAskBid r) / avAskBid r

difFromLow :: Rate' -> Double
difFromLow r = (avAskBid r - r ^. rateLow) / avAskBid r

-----------------------------------------------------

