{-# LANGUAGE OverloadedStrings #-}  
module Main where

import App.Reply
import App.Notate
import App.Seed
import Gmo.ToRecord
import Gmo.WsApi
import Common
import qualified Data.Vector as V
import Redis
import Model
import StoreSql
import Record
import DrawChart
import Database.Redis
import Database.SQLite.Simple


import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Time.LocalTime


import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.141592/5))) | x <- xs ]

main = toFile def "example1_big.png" $ do
    layout_title .= "Amplitude Modulation"
    setColors [opaque blue, opaque red]
    plot (line "am" [signal [0,(0.5)..400]])
    plot (points "am points" (signal [0,7..400]))

{-main :: IO ()
main = do
  xs <- selectData "select * from ETH_year_4hour;" "klines.db" []
  drawKlines "eth1.png" xs -}


{-main:: IO ()
main =  extractRatesFromWs BTC regist_ 
  where 
    regist_ :: Maybe Rate -> IO ()
    regist_ (Just rate) = do
      conn <- checkedConnect defaultConnectInfo 
      runRedis conn (uploadRate 0 rate)
    regist_ _ = print "error" -}


