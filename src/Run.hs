{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE Rank2Types #-}

module Run (app) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Extensible
import Control.Lens hiding ((:>))

import Common
import Command
import Record
import GetToken
import Bot.Reply
import Gmo.ToRecord


app :: IO ()
app = pure ()