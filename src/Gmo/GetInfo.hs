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

show' :: forall a. Show a=> a -> Maybe BS.ByteString 
show' = Just . BS.pack . show

getPrivateInfo :: S.String -> Query -> (B.ByteString, B.ByteString) -> IO S8.ByteString 
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
  pure $ encodePretty (getResponseBody response :: Value)

getMargin :: (B.ByteString, B.ByteString) -> IO S8.ByteString
getMargin = getPrivateInfo "v1/account/margin" []

getAssets :: (B.ByteString, B.ByteString) -> IO S8.ByteString
getAssets = getPrivateInfo "v1/account/assets" []

getOrders :: [Integer] -> (B.ByteString, B.ByteString) -> IO S8.ByteString
getOrders ids = 
      let query = B.intercalate "," . map (BS.pack . show) :: ([Integer] -> B.ByteString)
      in getPrivateInfo "v1/orders" [("orderId", Just $ query ids)]

getActiveOrder :: Coin -> Int -> Int -> (B.ByteString, B.ByteString) -> IO S8.ByteString
getActiveOrder coin page count = 
      let query = [("order", show' coin), ("page", show' page), ("count", show' count)] 
      in getPrivateInfo "v1/account/assets" []

getLatestExecutions :: Coin -> Int -> Int -> (B.ByteString, B.ByteString) -> IO S8.ByteString
getLatestExecutions coin page count = 
      let query = [("order", show' coin), ("page", show' page), ("count", show' count)]
      in getPrivateInfo "v1/latetExecutions" query