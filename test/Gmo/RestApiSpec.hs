{-# LANGUAGE OverloadedStrings #-} 

module Gmo.RestApiSpec where
import Test.Hspec
import Gmo.RestApi
import Data.Aeson.Lens
import           Data.Aeson                 (Value (..))

import Control.Lens
main :: IO ()
main = hspec spec

spec' :: Spec
spec' = do 
    describe "first test sample" $ do
        it "one add one equals two" $ do
            (1 + 1) `shouldBe` (2 :: Int)

extractStatus :: IO (Maybe Value) -> IO (Maybe String)
extractStatus obj = do
    value <- obj
    let res = (value ^. key "data" . key "status" :: Maybe String) in print value >> return res

spec :: Spec
spec = do 
    describe "test Gmo.RestApi" $ do
        describe "test getStatus" $ do
            it "status 0 is ok" $ do
                extractStatus checkStatus `shouldReturn` Just "OPEN"

