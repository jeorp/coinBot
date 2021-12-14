{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}  
{-# LANGUAGE Rank2Types #-}

module Gmo.GetInfo where
import Common
import Control.Lens

import           Crypto.Hash.SHA256         as SHA256
import           Data.Aeson                 (Value)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as S8
import           Data.Monoid                ((<>))
import qualified Data.String                as S
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Network.HTTP.Simple
--makeLenses

endPoint :: S.String
endPoint = baseUrl <> "/private/"

type GMOToken = (B.ByteString, B.ByteString)

show' :: forall a. Show a=> a -> Maybe BS.ByteString 
show' = Just . BS.pack . show

getPrivateInfo :: S.String -> Query -> GMOToken -> IO (Maybe Value)
getPrivateInfo path query api = do
  posixTime <- getPOSIXTime
  let epochTime = BS.pack . show . round $ posixTime
  let timestamp = epochTime <> "000"
  let sign = B16.encode $ SHA256.hmac (snd api) (timestamp <> "GET" <> BS.pack path)
  let url = endPoint <> path

  request' <- parseRequest url
  let request
        = setRequestMethod "GET"
        $ setRequestSecure True
        $ setRequestPort 443
        $ setRequestHeader "API-KEY" [fst api]
        $ setRequestHeader "API-TIMESTAMP" [timestamp]
        $ setRequestHeader "API-SIGN" [sign]
        $ setRequestQueryString query
        $ request'
  response <- httpJSON request
  pure (getResponseBody response :: Maybe Value)

getMargin :: GMOToken -> IO (Maybe Value)
getMargin = getPrivateInfo "v1/account/margin" []

getAssets :: GMOToken -> IO (Maybe Value)
getAssets = getPrivateInfo "v1/account/assets" []

getOrders :: [Integer] -> GMOToken -> IO (Maybe Value)
getOrders ids = 
      let query = B.intercalate "," . map (BS.pack . show) :: ([Integer] -> B.ByteString)
      in getPrivateInfo "v1/orders" [("orderId", Just $ query ids)]

getActiveOrder :: Coin -> Int -> Int -> GMOToken -> IO (Maybe Value)
getActiveOrder coin page count = 
      let query = [("order", show' coin), ("page", show' page), ("count", show' count)] 
      in getPrivateInfo "v1/account/assets" []

getLatestExecutions :: Coin -> Int -> Int -> GMOToken -> IO (Maybe Value)
getLatestExecutions coin page count = 
      let query = [("order", show' coin), ("page", show' page), ("count", show' count)]
      in getPrivateInfo "v1/latetExecutions" query