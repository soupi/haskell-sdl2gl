-- Main: contains entry point, init world function, game loop and game logic

module Main where

-- import Debug.Trace (traceShow, trace)
import qualified Data.Word as Word
import qualified MySDL
import qualified SDL

import qualified Config as C
import qualified World as W


-- setup window, surface and  world and send them to gameloop along with a update logic function
main :: IO ()
main = MySDL.withWindow C.defaultConfig $ flip MySDL.withSurface (gameloop . initWorld)

-- init World
initWorld :: (SDL.Window, SDL.Surface) -> W.World
initWorld (window,surface) = W.World window surface False maxBound

-- game loop: takes an update function and the current world
-- manage ticks, events and loop
gameloop :: W.World -> IO ()
gameloop = MySDL.gameloop W.getWindow logic

-- update function of world, changes the color of the screen
logic :: W.World -> [SDL.Event] -> IO W.World
logic world _ = do
  let (new_isup, n) = newIsupNum (W.getIsup world) (W.getNum world)
  MySDL.paintScreen (n,(50 + n) `mod` maxBound,(100 + n) `mod` maxBound) (W.getSurface world)
  return $ world { W.getIsup = new_isup, W.getNum = n }


-- what will be the next color? will it be brighter or darker?
newIsupNum :: Bool -> Word.Word8 -> (Bool, Word.Word8)
newIsupNum isup num
  | isup && num <  maxBound = (True, num + 5)
  | isup && num >= maxBound = (False, num)
  | num > minBound          = (False, num -5)
  | otherwise               = (True, num)
