{-# LANGUAGE LambdaCase #-}

module MySDL where

import Control.Monad (when)
import Control.Applicative ((<$>))
import qualified Data.Word as Word

import qualified SDL
import Linear

import Config

myOpenGLConfig :: SDL.OpenGLConfig
myOpenGLConfig = SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Normal 3 2 }

myWindowConfig :: Config -> SDL.WindowConfig
myWindowConfig config = SDL.defaultWindow { SDL.windowOpenGL = Just myOpenGLConfig, SDL.windowSize = size config }

createMyWindow :: Config -> IO SDL.Window
createMyWindow config = SDL.createWindow (title config) (myWindowConfig config)

withWindow :: Config -> (SDL.Window -> IO ()) -> IO ()
withWindow config go = do
  SDL.initialize [SDL.InitVideo]
  window <- createMyWindow config
  SDL.showWindow window

  go window

  SDL.destroyWindow window
  SDL.quit

withSurface :: SDL.Window -> ((SDL.Window, SDL.Surface) -> IO ()) -> IO ()
withSurface window go= do
  screenSurface <- SDL.getWindowSurface window
  paintScreen (maxBound,maxBound,maxBound) screenSurface

  go (window, screenSurface)

  SDL.freeSurface screenSurface

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

checkEvent :: SDL.EventPayload -> [SDL.Event] -> Bool
checkEvent event events = elem event $ map SDL.eventPayload events

regulateTicks :: Word.Word32 -> Word.Word32 -> Word.Word32 -> IO ()
regulateTicks ticks tick new_tick = when (ticks - (new_tick - tick) < ticks && (ticks - (new_tick - tick)) > 0) $ SDL.delay $ ticks - (new_tick - tick)
