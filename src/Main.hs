module Main where

import Control.Monad ((>=>))
import qualified Data.Word as Word
import qualified MySDL
import qualified SDL

import Config

main :: IO ()
main = MySDL.withWindow config (MySDL.withSurface >=> gameloop)

gameloop :: (SDL.Window, SDL.Surface) -> IO ()
gameloop (window, surface) = loop window surface False maxBound

loop :: SDL.Window -> SDL.Surface -> Bool -> Word.Word8 -> IO a
loop window surface isup num = do
  let (new_isup, n) = newIsupNum isup num
  MySDL.paintScreen (n,(50 + n) `mod` maxBound,(100 + n) `mod` maxBound) surface
  MySDL.updateWindow window
  loop window surface new_isup n


newIsupNum :: Bool -> Word.Word8 -> (Bool, Word.Word8)
newIsupNum isup num
  | isup && num <  maxBound = (True, num + 1)
  | isup && num >= maxBound = (False, num)
  | num > minBound          = (False, num -1)
  | otherwise               = (True, num)
