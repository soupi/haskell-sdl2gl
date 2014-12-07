module MySDL where

import qualified Data.Word as Word

import qualified SDL
import Linear

import Config

withWindow :: Config -> (SDL.Window -> IO ()) -> IO ()
withWindow config f = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow (title config) SDL.defaultWindow { SDL.windowSize = size config }
  SDL.showWindow window

  f window

  SDL.destroyWindow window
  SDL.quit

withSurface :: SDL.Window -> IO (SDL.Window, SDL.Surface)
withSurface window = do
  screenSurface <- SDL.getWindowSurface window
  paintScreen (maxBound,maxBound,maxBound) screenSurface
  return (window, screenSurface)


paintScreen :: (Word.Word8, Word.Word8, Word.Word8) -> SDL.Surface -> IO ()
paintScreen (r,g,b) screenSurface = do
  screenSurfaceFormat <- SDL.surfaceFormat screenSurface
  white <- SDL.mapRGB screenSurfaceFormat (V3 r g b)
  SDL.fillRect screenSurface Nothing white

updateWindow :: SDL.Window -> IO ()
updateWindow = SDL.updateWindowSurface
