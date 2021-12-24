{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Gmo.ToRecord (extractRate, extractRates) where
import Data.Text
import qualified Data.ByteString.Lazy  as B 
import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Maybe
import qualified Data.Vector as V  
import Data.Aeson
import Data.Aeson.Lens
import Record
import Common
import Gmo.RestApi


extractTag :: Text -> IO (Maybe Value) -> IO (Maybe Value)
extractTag tag obj = do
    val <- obj
    let value = fromMaybe Null val :: Value
        res = (value ^? key "data" . key tag . _Value) in return res

extractFirstTag :: IO (Maybe Value) -> IO (Maybe Value)
extractFirstTag obj = do
    val <- obj
    let value = fromMaybe Null val :: Value
        res = (value ^? key "data" . nth 0 . _Value) in return res

extractTraversal :: IO (Maybe Value) -> IO (V.Vector Value)
extractTraversal obj = do
    value <- obj
    let val = fromMaybe Null value ^? key "data" . _Array
        xs = fromMaybe V.empty val
        in return xs

extractRate :: IO (Maybe Rate)
extractRate = do
  mval <- extractFirstTag $ getRates Nothing
  let val = encode $ fromMaybe Null mval :: B.ByteString
  pure $ decode val

extractRates :: IO (V.Vector Rate)
extractRates =do
  mval <- extractTraversal $ getRates Nothing
  let xs = decode . encode <$> mval :: (V.Vector (Maybe Rate))
  pure $ V.catMaybes xs
