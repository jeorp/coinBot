{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module RecordSpec where


import Test.Hspec

import Data.Extensible
import Control.Lens hiding ((:>))

mkField "name collective cry"

type Animal = Record
  [ "name" :> String
  , "collective" :> String
  , "cry" :> Maybe String
  ]

dove :: Animal
dove = name @= "dove"
  <: collective @= "dule"
  <: cry @= Just "coo"
  <: nil

swan :: Animal
swan = name @= "swan"
  <: collective @= "lamentation"
  <: cry @= Nothing
  <: nil

collectiveOf :: Animal -> String
collectiveOf a = unwords ["a", a ^. collective, "of", a ^. name ++ "s"]

main :: IO ()
main = hspec spec 

spec :: Spec
spec = do 
    describe "Test Record" $ do
      
      describe "test swan name" $ do
        it "swan name is swan" $ do
            swan ^. name `shouldBe` "swan" 

      describe "test collectiveOf" $ do
        it "apply swan" $ do
          collectiveOf swan `shouldBe` "a lamentation of swans"