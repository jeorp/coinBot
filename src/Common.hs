{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Common where

import Control.Lens
import Data.Text (Text)
import qualified Data.ByteString as B
import Data.Aeson
import Data.Default.Class
import GHC.Generics

baseUrl = "https://api.coin.z.com/"
configFile = "config.env"

type GMOToken = (B.ByteString, B.ByteString)

data Coin = BTC | ETH | BCH | LTC | XRP | XEM | XLM | XYM | MONA |
          BTC_JPY | ETH_JPY | BCH_JPY | LTC_JPY | XRP_JPY deriving (Eq, Ord, Enum, Show, Bounded, Generic)

instance FromJSON Coin
instance ToJSON Coin

instance Default Coin where
  def = BTC


btc_ :: Lens' Coin Coin
btc_ = lens (const BTC) (const id)

eth_ :: Lens' Coin Coin
eth_ = lens (const ETH) (const id)

bch_ :: Lens' Coin Coin
bch_ = lens (const BCH) (const id)

ltc_ :: Lens' Coin Coin
ltc_ = lens (const LTC) (const id)

xrp_ :: Lens' Coin Coin
xrp_ = lens (const XRP) (const id)

xem_ :: Lens' Coin Coin
xem_ = lens (const XEM) (const id)

xlm_ :: Lens' Coin Coin
xlm_ = lens (const XLM) (const id)

xym_ :: Lens' Coin Coin
xym_ = lens (const XYM) (const id)

mona_ :: Lens' Coin Coin
mona_ = lens (const MONA) (const id)

btc_jpy_ :: Lens' Coin Coin
btc_jpy_ = lens (const BTC_JPY) (const id)

eth_jpy_ :: Lens' Coin Coin
eth_jpy_ = lens (const ETH_JPY) (const id)

bch_jpy_ :: Lens' Coin Coin
bch_jpy_ = lens (const BCH_JPY) (const id)

ltc_jpy_ :: Lens' Coin Coin
ltc_jpy_ = lens (const LTC_JPY) (const id)

xrp_jpy_ :: Lens' Coin Coin
xrp_jpy_ = lens (const XRP_JPY) (const id)


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

makeLenses ''Config

instance FromJSON Config
instance ToJSON Config
