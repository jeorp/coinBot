{-# LANGUAGE OverloadedStrings #-}  
module Main where

import App.Reply
import App.Notate
import Gmo.ToRecord
import Gmo.WsApi
import Common
import qualified Data.Vector as V
import Model

main :: IO ()
main = extractKlines BTC "1hour" "20211120" >>= (print . fromKline . V.head)



