module Main where

import Control.Monad ((>=>), when, unless)
import Debug.Trace (traceShow, trace)
import qualified Data.Word as Word
import qualified MySDL
import qualified SDL

import Config
import qualified World as W

main :: IO ()
main = MySDL.withWindow config $ flip MySDL.withSurface (gameloop logic . initWorld)

initWorld :: (SDL.Window, SDL.Surface) -> W.World
initWorld (window,surface) = W.World window surface False maxBound

gameloop :: (W.World -> [SDL.Event] -> IO W.World) -> W.World -> IO ()
gameloop update world = do
  tick <- SDL.ticks

  events <- MySDL.collectEvents
  new_world <- update world events
  MySDL.updateWindow (W.getWindow new_world)

  new_tick <- SDL.ticks
  MySDL.regulateTicks 17 tick new_tick

  unless (MySDL.checkEvent SDL.QuitEvent events) $ gameloop update new_world



logic :: W.World -> [SDL.Event] -> IO W.World
logic world _ = do
  let (new_isup, n) = newIsupNum (W.getIsup world) (W.getNum world)
  MySDL.paintScreen (n,(50 + n) `mod` maxBound,(100 + n) `mod` maxBound) (W.getSurface world)
  return $ world { W.getIsup = new_isup, W.getNum = n }

newIsupNum :: Bool -> Word.Word8 -> (Bool, Word.Word8)
newIsupNum isup num
  | isup && num <  maxBound = (True, num + 5)
  | isup && num >= maxBound = (False, num)
  | num > minBound          = (False, num -5)
  | otherwise               = (True, num)
