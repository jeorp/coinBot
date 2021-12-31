{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Notate where

import Common
import Data.Maybe
import           Data.Aeson               
import qualified Data.Aeson.QQ              as Aeson.QQ
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as S8
import           Data.Monoid                ((<>))
import qualified Data.String                as S
import qualified Data.Text as T
import Data.Text.Encoding
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Network.HTTP.Simple
import Control.Lens
import Control.Monad
import Record
import Gmo.ToRecord

postJson :: S.String -> Value -> IO BS.ByteString 
postJson url json = do

  request' <- parseRequest url
  let request
        = setRequestMethod "POST"
        $ setRequestSecure True
        $ setRequestPort 443
        $ setRequestBodyJSON json
        $ request'
  response <- httpBS request
  pure $ getResponseBody response

discordHook :: String -> T.Text -> IO BS.ByteString 
discordHook url msg =
    let t = decode ("{\"content\": \"" <> S8.pack (BS.unpack (encodeUtf8 msg)) <> "\"}") :: Maybe Value
        json = fromMaybe Null t
        in postJson url json

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