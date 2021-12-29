{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE Rank2Types #-}

module Run where

import Control.Monad.Reader
import Control.Monad.State
import Data.Extensible
import Control.Lens hiding ((:>))

import Common
import Record
import GetToken
import Bot.Reply
import Gmo.ToRecord


main :: IO ()
main = pure ()