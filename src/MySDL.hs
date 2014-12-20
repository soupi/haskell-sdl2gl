{-# LANGUAGE LambdaCase #-}

module MySDL where

import Control.Monad (when)
import Control.Applicative ((<$>))
import qualified Data.Word as Word

import qualified SDL
import Linear

import qualified Config as C


-- Config window
myWindowConfig :: C.Config -> SDL.WindowConfig
myWindowConfig config = SDL.defaultWindow { SDL.windowOpenGL = Just (C.glConf config), SDL.windowSize = C.size config }

-- create a window with config
createMyWindow :: C.Config -> IO SDL.Window
createMyWindow config = SDL.createWindow (C.title config) (myWindowConfig config)

-- will init SDL and create a Window and pass in as a parameter to function
withWindow :: C.Config -> (SDL.Window -> IO ()) -> IO ()
withWindow config go = do
  SDL.initialize [SDL.InitVideo]
  window <- createMyWindow config
  SDL.showWindow window

  go window

  SDL.destroyWindow window
  SDL.quit

-- create a Surface and pass in as a parameter to function
withSurface :: SDL.Window -> ((SDL.Window, SDL.Surface) -> IO ()) -> IO ()
withSurface window go = do
  screenSurface <- SDL.getWindowSurface window
  paintScreen (maxBound,maxBound,maxBound) screenSurface

  go (window, screenSurface)

  SDL.freeSurface screenSurface

-- paint background
paintScreen :: (Word.Word8, Word.Word8, Word.Word8) -> SDL.Surface -> IO ()
paintScreen (r,g,b) screenSurface = do
  screenSurfaceFormat <- SDL.surfaceFormat screenSurface
  white <- SDL.mapRGB screenSurfaceFormat (V3 r g b)
  SDL.fillRect screenSurface Nothing white


updateWindow :: SDL.Window -> IO ()
updateWindow = SDL.updateWindowSurface

collectEvents :: IO [SDL.Event]
collectEvents = SDL.pollEvent >>= \case
    Nothing -> return []
    Just e' -> (e' :) <$> collectEvents

-- checks if specific event happend
checkEvent :: SDL.EventPayload -> [SDL.Event] -> Bool
checkEvent event events = elem event $ map SDL.eventPayload events

-- will delay until ticks pass
regulateTicks :: Word.Word32 -> Word.Word32 -> Word.Word32 -> IO ()
regulateTicks ticks tick new_tick = when (ticks - (new_tick - tick) < ticks && (ticks - (new_tick - tick)) > 0) $ SDL.delay $ ticks - (new_tick - tick)
