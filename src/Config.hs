{-# LANGUAGE OverloadedStrings #-}

module Config where

import Foreign.C.Types
import Data.Text
import Linear

data Config = Config { title :: Text, size :: V2 CInt }

config :: Config
config = Config "Hello SDL" $ V2 640 480

