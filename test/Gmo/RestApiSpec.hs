{-# LANGUAGE OverloadedStrings #-} 

module Gmo.RestApiSpec where
import Test.Hspec
import Gmo.RestApi
import Data.Aeson.Lens
import Data.Aeson

import Control.Lens
import Data.Text
import Common

main :: IO ()
main = hspec spec

extractTag :: Text -> IO (Maybe Value) -> IO (Maybe String)
extractTag tag obj = do
    value <- obj
    let res = (value ^. key "data" . key tag :: Maybe String) in print value >> return res

extractRateStatus :: IO (Maybe Value) -> IO (Maybe String)
extractRateStatus =  extractTag "status"

extractRateSymbol :: IO (Maybe Value) -> IO (Maybe String)
extractRateSymbol obj = do
    value <- obj
    let res = (value ^. key "data" . nth 0 . key "symbol" :: Maybe String) in print value >> return res

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
      
      describe "test getOrderBooks" $ do
        it "BTC orderbooks symbol is BTC" $ do
          extractOrderBooksSymbol (getOrderBooks BTC) `shouldReturn` Just "BTC"

