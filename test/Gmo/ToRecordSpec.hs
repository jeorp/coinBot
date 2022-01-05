{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Gmo.ToRecordSpec where

import Test.Hspec
import Control.Arrow
import Data.Text
import Data.Text.Encoding
import qualified Data.ByteString.Lazy  as B 
import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Maybe
import qualified Data.Vector as V  
import Data.Aeson
import Data.Aeson.Lens
import Record
import Common
import GetToken
import Gmo.RestApi
import Gmo.ToRecord



testRateFirstSymbol :: IO (Maybe Rate) -> IO Text
testRateFirstSymbol io = do
  mr <- io
  let s = maybe "" (^. #symbol) mr
  return s

testRatesAllSymbol :: IO (V.Vector Rate) -> IO (V.Vector Text)
testRatesAllSymbol io = do
  v <- io
  return $ (^. #symbol) <$> v

gmoToken :: IO GMOToken
gmoToken = do
  bb <- either (const ("", ""))  ((^. api_token) &&& (^. api_token_secret)) <$> extract
  print bb
  pure $ both %~ encodeUtf8 $ bb

testExtractMargin :: IO (Maybe Margin)
testExtractMargin = extractMargin

testExtractAssets :: IO (V.Vector Assets)
testExtractAssets = extractAssets

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "test Gmo.ToRecord" $ do
      describe "test extractRate" $ do
        it "fist Rate symbol is BTC" $ do
          testRateFirstSymbol (extractRate Nothing) `shouldReturn` "BTC"

      describe "test extractRates" $ do
        it "all Rates is .." $ do
          testRatesAllSymbol extractRates 
            `shouldReturn` 
            V.fromList ["BTC","ETH","BCH","LTC","XRP","XEM","XLM","BTC_JPY","ETH_JPY","BCH_JPY","LTC_JPY","XRP_JPY","XYM","MONA"]

      describe "test extractMargin" $ do
        it "Margin marginCallStatus is Normal" $ do
          fmap (^. #marginCallStatus) <$> testExtractMargin `shouldReturn` Just "NORMAL"
  
      describe "test extractAssets" $ do
        it "first Asset JPY" $ do
          (^. #symbol) . V.head <$> testExtractAssets `shouldReturn` "JPY"