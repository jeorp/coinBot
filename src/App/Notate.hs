
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
module App.Notate where

import Common

import Control.Lens
import Control.Monad
import Record
import Gmo.ToRecord
import Notate

notateExample :: String -> Coin -> IO ()
notateExample url c = extractRatesFromWs c exampleDo
  where
    exampleDo :: Maybe Rate -> IO ()
    exampleDo (Just r) = discordHook url ("ask: " <> r ^. #ask <> ", bid: " <> r ^. #bid) >>= \t -> unless (t=="") $ print t
    exampleDo _ = print "error"

notateExample2 :: String -> Coin -> IO ()
notateExample2 url c = extractOrderBooksFromWs c exampleDo2
  where
    exampleDo2 :: Maybe OrderBooks -> IO ()
    exampleDo2 (Just ob) = discordHook url ("ask: " <> head (ob ^. #asks) ^. #price <> ", bid: " <> head (ob ^. #bids) ^. #price) >>= \t -> unless (t=="") $ print t
    exampleDo2 _ = print "error"