{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Common (
  baseUrl,configFile, GMOToken, Coin(..), Side(..), ExecutionType(..),
  TimelnForce(..), Kline(..), Curve, Ticker(..),
  OrderBook(..), OrderBooks(..), Config(..),
  ProfitLoss(..), Assets(..)
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
          BTC_JPY | ETH_JPY | BCH_JPY | LTC_JPY | XRP_JPY deriving (Show, Eq, Generic)

instance FromJSON Coin
instance ToJSON Coin

data Side = BUY | SELL deriving (Show, Eq, Generic)

data ExecutionType = MARKET | LIMIT | STOP deriving (Show, Eq, Generic)

data TimelnForce = FAK | FAS | FOK deriving (Show, Eq, Generic)

data Kline = Kline {
  _openTime :: Text,
  _open :: Float,
  _high :: Float,
  _low :: Float,
  _close :: Float,
  _volume :: Float
}deriving (Show, Eq, Generic)

type Curve = (Coin, [Kline])

data Ticker = Ticker{
  _coin :: Coin,
  _ask :: Float,
  _bid :: Float,
  _kline :: Kline
}deriving (Show, Eq, Generic)

data OrderBook = OrderBook {
  _book_price :: Float,
  _size :: Double
}deriving (Show, Eq, Generic)

data OrderBooks = OrderBooks{
  _asks :: [OrderBook],
  _bids :: [OrderBook]
}deriving (Show, Eq, Generic)

data Config = Config {
  _entry :: Text,
  _bot_token :: Text,
  _api_token :: Text,
  _api_token_secret :: Text
}deriving (Show, Eq, Generic)

data ProfitLoss = ProfitLoss {
  _actualProfitLoss :: Integer,
  _availableAmount :: Integer,
  _profitLoss :: Integer
}deriving (Show, Eq, Generic)

data Assets = Assets {
  _amout :: Double,
  _available :: Double,
  _conversionRate :: Float,
  _asset_symbol :: Coin
}deriving (Show, Eq, Generic)


data OrderInfo = OrderInfo {
  _order_symbol :: Coin,
  _side :: Side,
  _executionType :: ExecutionType,
  _timelnForce :: Maybe TimelnForce,
  _order_price :: Maybe Double,
  _order_size :: Integer 

}deriving (Show, Eq, Generic)

