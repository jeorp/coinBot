{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Gmo.ToRecord where
import Data.Extensible
import Control.Lens hiding ((:>))

import qualified Data.ByteString as B 
import Data.Aeson
import Record
import Common
