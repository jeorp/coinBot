module Gmo.GetInfoSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
    describe "first test sample" $ do
        it "one add one equals two" $ do
            (1 + 1) `shouldBe` (2 :: Int)