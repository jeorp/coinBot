{-# LANGUAGE OverloadedStrings #-} 

module Gmo.RestApiSpec where
import Test.Hspec
import Gmo.RestApi
import Data.Aeson.Lens
import Data.Aeson

import Control.Lens
import Data.Text
import Data.Maybe
import Common

main :: IO ()
main = hspec spec

extractTag :: Text -> IO (Maybe Value) -> IO (Maybe String)
extractTag tag obj = do
    value <- obj
    let res = (value ^. key "data" . key tag :: Maybe String) in return res

extractFirstTag :: Text -> IO (Maybe Value) -> IO (Maybe String)
extractFirstTag tag obj = do
    value <- obj
    let res = (value ^. key "data" . nth 0 . key tag :: Maybe String) in return res

extractTraversalOneElement :: Text -> IO (Maybe Value) -> IO [String]
extractTraversalOneElement tag obj = do
    val <- obj
    let vs = val ^. key "data" :: Maybe Value
        xs = vs & catMaybes . toListOf (traverseArray . key tag) :: [String]
        in return xs 

extractRateStatus :: IO (Maybe Value) -> IO (Maybe String)
extractRateStatus =  extractTag "status"

extractRateSymbol :: IO (Maybe Value) -> IO (Maybe String)
extractRateSymbol = extractFirstTag "symbol" 

extractRateSymbols :: IO (Maybe Value) -> IO [String]
extractRateSymbols = extractTraversalOneElement "symbol" 

extractOrderBooksSymbol :: IO (Maybe Value) -> IO (Maybe String)
extractOrderBooksSymbol =  extractTag "symbol"

spec :: Spec
spec = do 
    describe "test Gmo.RestApi" $ do
      describe "test getStatus" $ do
        it "status 0 is ok" $ do
          extractRateStatus checkStatus `shouldReturn` Just "OPEN"
      
      describe "test getRates" $ do
        it "first rate symbol is BTC" $ do
          extractRateSymbol (getRates Nothing) `shouldReturn` Just "BTC"

        it "colect symbol from objects" $ do
          extractRateSymbols (getRates Nothing) 
          `shouldReturn` ["BTC","ETH","BCH","LTC","XRP","XEM","XLM","BTC_JPY","ETH_JPY","BCH_JPY","LTC_JPY","XRP_JPY","XYM","MONA"]
      
      describe "test getOrderBooks" $ do
        it "BTC orderbooks symbol is BTC" $ do
          extractOrderBooksSymbol (getOrderBooks BTC) `shouldReturn` Just "BTC"

