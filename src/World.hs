{-# LANGUAGE OverloadedStrings #-}

module World where

import qualified Data.Word as Word

import qualified SDL

data World = World { getWindow  :: SDL.Window
                   , getSurface :: SDL.Surface
                   , getIsup    :: Bool
                   , getNum     :: Word.Word8
                   }
