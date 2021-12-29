{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE Rank2Types #-}

module Run where

import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Common
import GetToken
import Bot.Reply
import Gmo.GetInfo
import Gmo.RestApi


main :: IO ()
main = pure ()