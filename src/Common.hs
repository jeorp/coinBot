{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Common (
  baseUrl,configFile, GMOToken, Coin(..), Side(..), ExecutionType(..),
  TimelnForce(..), Config(..)
  ) where
import Control.Lens
import Data.Text (Text)
import qualified Data.ByteString as B
import Data.Aeson
import GHC.Generics

baseUrl = "https://api.coin.z.com/"
configFile = "config.env"

type GMOToken = (B.ByteString, B.ByteString)

data Coin = BTC | ETH | BCH | LTC | XRP | XEM | XLM | XYM | MONA |
          BTC_JPY | ETH_JPY | BCH_JPY | LTC_JPY | XRP_JPY deriving (Eq, Ord, Enum, Show, Bounded, Generic)

instance FromJSON Coin
instance ToJSON Coin

data Side = BUY | SELL deriving (Show, Eq, Generic)

instance FromJSON Side
instance ToJSON Side

data ExecutionType = MARKET | LIMIT | STOP deriving (Show, Eq, Generic)

instance FromJSON ExecutionType
instance ToJSON ExecutionType

data TimelnForce = FAK | FAS | FOK deriving (Show, Eq, Generic)

instance FromJSON TimelnForce
instance ToJSON TimelnForce

data Config = Config {
  _entry :: Text,
  _bot_token :: Text,
  _api_token :: Text,
  _api_token_secret :: Text
}deriving (Show, Eq, Generic)

instance FromJSON Config
instance ToJSON Config
