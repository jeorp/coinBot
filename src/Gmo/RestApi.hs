{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE Rank2Types #-}

module Gmo.RestApi (checkStatus, getApi, getRates, getOrderBooks,
                     getTrades, getKlines) where
import Common
import Control.Lens

import           Data.Aeson                 (Value)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.ByteString.Char8 as SB
import           Network.HTTP.Simple


--makeLenses


checkStatus :: IO (Maybe Value)
checkStatus = do
  response <- httpJSON "https://api.coin.z.com/public/v1/status"
  pure (getResponseBody response :: Maybe Value)


show' :: forall a. Show a=> a -> Maybe SB.ByteString 
show' = Just . SB.pack . show

getApi :: String -> Query ->IO (Maybe Value)
getApi url query = do
  request' <- parseRequest url
  let request
        = setRequestMethod "GET"
        $ setRequestSecure True
        $ setRequestPort 443
        $ setRequestQueryString query
        $ request'
  response <- httpJSON request
  pure (getResponseBody response :: Maybe Value)

getRates :: Maybe Coin -> IO (Maybe Value)
getRates coin = 
    let url = baseUrl <> "public/v1/ticker"
        query = [("symbol", SB.pack . show <$> coin)] :: Query
        in getApi url query

getOrderBooks :: Coin -> IO (Maybe Value)
getOrderBooks coin = 
    let url = baseUrl <> "public/v1/orderbooks"
        query = [("symbol", show' coin)] :: Query
        in getApi url query

getTrades :: Coin -> Int -> Int -> IO (Maybe Value) 
getTrades coin page limit =
    let url = baseUrl <> "public/v1/trades"
        query =  [("symbol", show' coin),
            ("page", show' page), ("count", show' limit)]
        in getApi url query

getKlines :: Coin -> String -> String -> IO (Maybe Value) 
getKlines coin interval date =
    let url = baseUrl <> "public/v1/klines"
        query =  [("symbol", show' coin),
            ("interval", show' interval), ("date", show' date)]
        in getApi url query



