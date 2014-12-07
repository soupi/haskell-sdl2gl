module Main where

import Control.Monad ((>=>), when)
import Debug.Trace (traceShow, trace)
import qualified Data.Word as Word
import qualified MySDL
import qualified SDL

import Config
import qualified World as W

main :: IO ()
main = MySDL.withWindow config (MySDL.withSurface >=> gameloop logic . initWorld)

initWorld :: (SDL.Window, SDL.Surface) -> W.World
initWorld (window,surface) = W.World window surface False maxBound

gameloop :: (W.World -> IO W.World) -> W.World -> IO ()
gameloop f world = do
  tick <- SDL.ticks

  new_world <- f world
  MySDL.updateWindow (W.getWindow new_world)

  new_tick <- SDL.ticks
  when (17 - (new_tick - tick) < 17 && (17 - (new_tick - tick)) > 0) $ SDL.delay $ 17 - (new_tick - tick)

  gameloop f new_world



logic :: W.World -> IO W.World
logic world = do
  let (new_isup, n) = newIsupNum (W.getIsup world) (W.getNum world)
  MySDL.paintScreen (n,(50 + n) `mod` maxBound,(100 + n) `mod` maxBound) (W.getSurface world)
  return $ world { W.getIsup = new_isup, W.getNum = n }

newIsupNum :: Bool -> Word.Word8 -> (Bool, Word.Word8)
newIsupNum isup num
  | isup && num <  maxBound = (True, num + 5)
  | isup && num >= maxBound = (False, num)
  | num > minBound          = (False, num -5)
  | otherwise               = (True, num)
