{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Gmo.GetInfoSpec where


import Test.Hspec
import Data.Text
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BS
import Gmo.GetInfo
import Data.Aeson.Lens
import Data.Aeson
import Control.Lens
import Control.Arrow
import GetToken
import Common
makeLenses ''Config

main :: IO ()
main = hspec spec

textToByteString :: Text -> B.ByteString
textToByteString = BS.pack . unpack 

gmoToken :: IO GMOToken
gmoToken = do
  bb <- either (const ("", ""))  ((^. api_token) &&& (^. api_token_secret)) <$> extract
  pure $ both %~ textToByteString $ bb


spec :: Spec
spec = do 
    describe "Test Gmo.GetInfo" $ do
      describe "test gmoToken" $ do
        it "get gmo token" $ do
          pure "" `shouldReturn` "" -- gmoToken `shouldReturn` ("XXXXX", "YYYY")
      describe "test getPrivateInfo" $ do
        it "" $ do
          pure "" `shouldReturn` ""
      describe "test getMargin" $ do
        it "" $ do
          pure "" `shouldReturn` ""
      describe "test getAssets" $ do
        it "" $ do
          pure "" `shouldReturn` ""
      describe "test getOrders" $ do
        it "" $ do
          pure "" `shouldReturn` ""
      describe "test getActiveOrder" $ do
        it "" $ do
          pure "" `shouldReturn` ""