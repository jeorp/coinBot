{-# LANGUAGE OverloadedStrings #-} 
module Gmo.GetInfoSpec where


import Test.Hspec
import Gmo.GetInfo
import Data.Aeson.Lens
import Data.Aeson
import GetToken

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
    describe "first test sample" $ do
      it "one add one equals two" $ do
        (1 + 1) `shouldBe` (2 :: Int)

spec' :: Spec
spec' = do 
    describe "first test sample" $ do
      it "one add one equals two" $ do
        (1 + 1) `shouldBe` (2 :: Int)