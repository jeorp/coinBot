{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module RecordSpec where


import Test.Hspec

import Data.Extensible
import Control.Lens hiding ((:>))

import qualified Data.ByteString as B 
import Data.Aeson

import Record

type Animal = Record
  [ "name" :> String
  , "collective" :> String
  , "cry" :> Maybe String
  ]

type Animal' = Record
  [ "name" :> String
  , "cry" :> Maybe String
  ]

type Point = Record
  [ "x" :> Float,
    "y" :> Float
  ]

p :: Point 
p = #x @= 1.2 <: #y @= 0.12 <: emptyRecord

dove :: Animal
dove = #name @= "dove"
  <: #collective @= "dule"
  <: #cry @= Just "coo"
  <: emptyRecord

dove' :: Animal'
dove' = #name @= "dove"
  <: #cry @= Just "coo"
  <: emptyRecord

swan :: Animal
swan = #name @= "swan"
  <: #collective @= "lamentation"
  <: #cry @= Nothing
  <: emptyRecord

exRate :: Rate
exRate = #ask @= "ask"
  <: #bid @= "bid"
  <: #high @= ""
  <: #last @= ""
  <: #low @= ""
  <: #symbol @= ""
  <: #timestamp @= ""
  <: #volume @= ""
  <: emptyRecord


collectiveOf :: (Associated s ("name" ':> String), Associated s ("collective" ':> String))
  => Record s -> String
collectiveOf a = unwords ["a", a ^. #collective, "of", a ^. #name ++ "s"]

main :: IO ()
main = hspec spec 

spec :: Spec
spec = do 
    describe "Test Record Sample" $ do
      
      describe "test swan name" $ do
        it "swan name is swan" $ do
            swan ^. #name `shouldBe` "swan"

      describe "test dove' name" $ do
        it "dove' name is dove" $ do
            dove' ^. #name `shouldBe` "dove" 

      describe "test collectiveOf" $ do
        it "apply swan" $ do
          collectiveOf swan `shouldBe` "a lamentation of swans"

      describe "test shrink" $ do
        it "shrink dove = dove'" $ do
            shrink dove  `shouldBe` dove'

      describe "test decode from json" $ do
        it "test exists json instance'" $ do
            decode "{\"name\": \"dove\", \"collective\": \"dule\", \"cry\": \"coo\"}"  `shouldBe` Just dove

      describe "test encode float" $ do
        it "decode eq p" $ do
            decode  "{\"x\": 1.2, \"y\": 0.12}" `shouldBe` Just p

      describe "check lens from import " $ do
        it "ask of exRate is ask" $ do
          exRate ^. #ask `shouldBe` "ask" 