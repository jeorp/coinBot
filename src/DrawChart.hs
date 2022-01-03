{-# LANGUAGE OverloadedLabels  #-}
module DrawChart where

import StoreSql
import Model
import Data.Default.Class
import qualified Data.Int as I
import qualified Data.Text as T
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Time.LocalTime
import Data.Time.Clock.System

temp :: FilePath 
temp = "temp/"


drawKlines :: FilePath -> [Kline'] -> IO ()
drawKlines path xs = do
  toFile def (temp <> path) $ do
    layout_title .= "Chart"
    setColors [opaque red]
    plot (line "price" [ [ (klineToLocalTime k, _close k :: Float) |  k <- xs] ])
  where
    klineToLocalTime :: Kline' -> LocalTime
    klineToLocalTime k = 
        let i = read (T.unpack (_openTime k)) :: I.Int64
            utcTime = systemToUTCTime (MkSystemTime i 0)
        in utcToLocalTime utc utcTime
