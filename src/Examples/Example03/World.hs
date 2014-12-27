-- World: data type to represent the world and its components

{-# LANGUAGE OverloadedStrings #-}

module World where

import qualified SDL

-- World: data type to represent the world and its components
data World = World { getWindow  :: SDL.Window
                   }
