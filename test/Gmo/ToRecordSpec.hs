{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Gmo.ToRecordSpec where

import Test.Hspec
import Data.Text
import qualified Data.ByteString.Lazy  as B 
import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Maybe
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

extractRate :: IO (Maybe Rate)
extractRate = do
  mval <- extractFirstTag $ getRates Nothing
  let val = encode $ fromMaybe Null mval :: B.ByteString
  pure $ decode val


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "test Gmo.ToRecord" $ do
      describe "test getRate" $ do
        it "fist Rate symbol is BTC" $ do
          extractRate `shouldReturn` Nothing