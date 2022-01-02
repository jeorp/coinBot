{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
module OutputCsv where

import Record
import Control.Lens
import Control.Arrow
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv

data Order' = Order'
  {
    _tag :: T.Text,
    _price :: Float,
    _size :: Float
  }

instance ToRecord Order' where
    toRecord (Order' tag price size) = record [
        toField tag, toField price, toField size]

fromOrderBooks :: OrderBooks -> [Order']
fromOrderBooks obs =
    let convert = \tag -> uncurry (Order' tag) . over both (read . T.unpack) . ((^. #price) &&& (^. #size)) :: Order -> Order'    
    in (convert "asks" <$> obs ^. #asks) ++ (convert "bids" <$> obs ^. #bids)

orderBooksEncode :: OrderBooks -> BL.ByteString
orderBooksEncode = encode . fromOrderBooks


