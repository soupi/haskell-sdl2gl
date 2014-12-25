-- World: data type to represent the world and its components

{-# LANGUAGE OverloadedStrings #-}

module World where

import qualified Data.Word as Word

import qualified SDL

-- World: data type to represent the world and its components
data World = World { getWindow  :: SDL.Window
                   , getSurface :: SDL.Surface
                   , getIsup    :: Bool
                   , getNum     :: Word.Word8
                   }
