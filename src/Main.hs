module Main where

import Control.Monad ((>=>))
import qualified MySDL
import qualified SDL

import Config

main :: IO ()
main = MySDL.withWindow config (MySDL.withSurface >=> gameloop)

gameloop :: (SDL.Window, SDL.Surface) -> IO ()
gameloop (window, surface) = do
  MySDL.updateWindow window
  gameloop (window, surface)
