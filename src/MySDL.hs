module MySDL where

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

withSurface :: SDL.Window -> IO ()
withSurface window = do
  screenSurface <- SDL.getWindowSurface window
  screenSurfaceFormat <- SDL.surfaceFormat screenSurface
  white <- SDL.mapRGB screenSurfaceFormat (V3 maxBound maxBound maxBound)
  SDL.fillRect screenSurface Nothing white
  SDL.updateWindowSurface window
