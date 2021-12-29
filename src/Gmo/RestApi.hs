{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE Rank2Types #-}

module Gmo.RestApi (checkStatus, getApi, getRates, getOrderBooks,
                     getTrades, getKlines) where
import Common
import Control.Lens

import           Data.Aeson                 (Value)

import           Network.HTTP.Simple
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS


--makeLenses
show' :: forall a. Show a=> a -> Maybe BS.ByteString 
show' = Just . BS.pack . show

checkStatus :: IO B.ByteString
checkStatus = do
  response <- httpBS "https://api.coin.z.com/public/v1/status"
  pure $ getResponseBody response

getApi :: String -> Query ->IO B.ByteString
getApi url query = do
  request' <- parseRequest url
  let request
        = setRequestMethod "GET"
        $ setRequestSecure True
        $ setRequestPort 443
        $ setRequestQueryString query
        $ request'
  response <- httpBS request
  pure $ getResponseBody response

getRates :: Maybe Coin -> IO B.ByteString
getRates coin = 
    let url = baseUrl <> "public/v1/ticker"
        query = [("symbol", BS.pack . show <$> coin)] :: Query
        in getApi url query

getOrderBooks :: Coin -> IO B.ByteString
getOrderBooks coin = 
    let url = baseUrl <> "public/v1/orderbooks"
        query = [("symbol", show' coin)] :: Query
        in getApi url query

getTrades :: Coin -> Int -> Int -> IO B.ByteString
getTrades coin page limit =
    let url = baseUrl <> "public/v1/trades"
        query =  [("symbol", show' coin),
            ("page", show' page), ("count", show' limit)]
        in getApi url query

getKlines :: Coin -> String -> String -> IO B.ByteString
getKlines coin interval date =
    let url = baseUrl <> "public/v1/klines"
        query =  [("symbol", show' coin),
            ("interval", show' interval), ("date", show' date)]
        in getApi url query



