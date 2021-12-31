{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Gmo.ToRecord where
import Data.Text
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL 
import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Maybe
import qualified Data.Vector as V  
import Data.Aeson
import Data.Aeson.Lens
import Control.Monad
import Record
import Common
import Gmo.RestApi
import Gmo.WsApi



extractTag :: Text -> IO B.ByteString -> IO (Maybe Value)
extractTag tag obj = do
    val <- obj
    let res = (val ^? key "data" . key tag . _Value) in return res

extractFirstTag :: IO B.ByteString -> IO (Maybe Value)
extractFirstTag obj = do
    val <- obj
    let res = (val ^? key "data" . nth 0 . _Value) in return res

extractTraversal :: IO B.ByteString -> IO (V.Vector Value)
extractTraversal obj = do
    value <- obj
    let val = value ^? key "data" . _Array
        xs = fromMaybe V.empty val
        in return xs

extractRate :: Maybe Coin -> IO (Maybe Rate)
extractRate c = do
  mval <- extractFirstTag $ getRates c
  let val = encode $ fromMaybe Null mval :: BL.ByteString
  pure $ decode val

extractRates :: IO (V.Vector Rate)
extractRates = do
  mval <- extractTraversal $ getRates Nothing
  let xs = decode . encode <$> mval :: (V.Vector (Maybe Rate))
  pure $ V.catMaybes xs

extractKlines :: Coin -> String -> String -> IO (V.Vector Kline)
extractKlines c interval date = do
  mval <- extractTraversal $ getKlines c interval date
  let xs = decode . encode <$> mval :: (V.Vector (Maybe Kline))
  print xs
  pure $ V.catMaybes xs


-------------------------------------------

extractRatesFromWs :: Coin -> (Maybe Rate -> IO ()) -> IO ()
extractRatesFromWs c io = runGetRateWS $ getRateStream c $ flip getRates io
  where
    getRates :: BL.ByteString -> (Maybe Rate -> IO ()) -> IO ()
    getRates b io = do
      let rate = decode b :: Maybe Rate
      io rate

extractOrderBooksFromWs :: Coin -> (Maybe OrderBooks -> IO ()) -> IO ()
extractOrderBooksFromWs c io = runGetRateWS $ getOrderBookStream c $ flip getRates io
  where
    getRates :: BL.ByteString -> (Maybe OrderBooks -> IO ()) -> IO ()
    getRates b io = do
      let ob = decode b :: Maybe OrderBooks
      io ob