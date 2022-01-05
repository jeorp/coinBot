{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
module OutputCsv where

import Model
import Record
import Control.Lens
import Control.Arrow
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv

data Order'' = Order''
  {
    _tag :: T.Text,
    _Orderprice :: Float,
    _Ordersize :: Float
  }

fromOrderBooks :: OrderBooks -> [Order'']
fromOrderBooks obs =
    let convert = \tag -> uncurry (Order'' tag) . over both (read . T.unpack) . ((^. #price) &&& (^. #size)) :: Order -> Order''    
    in (convert "asks" <$> obs ^. #asks) ++ (convert "bids" <$> obs ^. #bids)

instance ToRecord Order'' where
    toRecord (Order'' tag price size) = record 
      [
        toField tag, 
        toField price, 
        toField size
      ]

instance ToRecord Margin' where
    toRecord (Margin' p a m c l) = record 
      [
        toField p, 
        toField a, 
        toField m,
        toField c,
        toField l     
      ]

instance ToRecord Assets' where
    toRecord (Assets' m a r s) = record 
      [
        toField m, 
        toField a, 
        toField r,
        toField s     
      ]

orderBooksEncode :: OrderBooks -> BL.ByteString
orderBooksEncode = encode . fromOrderBooks

marginEncode :: Margin -> BL.ByteString
marginEncode = encode . (:[]) . fromMargin

assetsEncode :: [Assets] -> BL.ByteString
assetsEncode = encode . fmap fromAssets 