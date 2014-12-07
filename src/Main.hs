module Main where

import qualified MySDL

import Config

main :: IO ()
main = MySDL.withWindow config MySDL.withSurface
