-- Main: contains entry point, init world function, game loop and game logic

module Main where

import Debug.Trace (traceShow, trace)
import Control.Monad ((<=<))
import Data.Foldable (concat)
import System.Environment (getArgs)
import qualified Data.Word as W
import qualified SDL
import qualified Graphics.GL.Core32 as GL
import qualified Graphics.GL.Types as GL
import qualified Foreign as F (alloca, allocaArray, allocaArray0, pokeArray, peekArray, sizeOf, peek)
import qualified Foreign.Ptr as F (Ptr, castPtr, nullPtr)
import qualified Foreign.C.String as F (withCString, castCCharToChar)
import qualified Foreign.C.Types as F
import qualified Foreign.Marshal.Utils as F (with)
--import qualified Foreign.Marshal.Array as F (withArray0)

import qualified MySDL
import qualified Config as C
import qualified World as W

-- setup window, surface and  world and send them to gameloop along with a update logic function
main :: IO ()
main = do
  [posShaderFilePath, clrShaderFilePath] <- getArgs
  MySDL.withWindow C.defaultConfig $ flip MySDL.withSurface (gameloop <=< initWorld posShaderFilePath clrShaderFilePath)

-- init World
initWorld :: String -> String -> (SDL.Window, SDL.Surface) -> IO W.World
initWorld posShaderFilePath clrShaderFilePath (window,surface) = do
  vbo <- F.alloca useVBO
  compileShader posShaderFilePath GL.GL_VERTEX_SHADER
  compileShader clrShaderFilePath GL.GL_FRAGMENT_SHADER
  return $ W.World window surface

useVBO :: F.Ptr GL.GLuint -> IO W.Word32
useVBO vboPtr = do
  GL.glGenBuffers 1 vboPtr
  vbo <- F.peek vboPtr
  GL.glBindBuffer GL.GL_ARRAY_BUFFER vbo
  F.allocaArray 6 $ sendVerticesToGPU [0, 0.5, 0.5, -0.5, -0.5, -0.5]
  return vbo

sendVerticesToGPU :: [Float] -> F.Ptr F.CFloat -> IO ()
sendVerticesToGPU vertices vertices_array = do
  F.pokeArray vertices_array $ map F.CFloat vertices
  GL.glBufferData GL.GL_ARRAY_BUFFER (F.CPtrdiff (fromIntegral (F.sizeOf vertices_array))) (F.castPtr vertices_array) GL.GL_STATIC_DRAW

compileShader :: String -> GL.GLenum -> IO ()
compileShader shaderSourcePath shaderType = do
  shaderSource <- readFile shaderSourcePath
  shader <- GL.glCreateShader shaderType
  F.withCString shaderSource $ flip F.with (\shaderSourcePtr -> GL.glShaderSource shader 1 shaderSourcePtr F.nullPtr)
  GL.glCompileShader shader
  F.alloca (\status -> GL.glGetShaderiv shader GL.GL_COMPILE_STATUS status >> F.peek status >>= print . (==) GL.GL_TRUE)
  F.allocaArray0 511 (\arr -> GL.glGetShaderInfoLog shader 512 F.nullPtr arr >> F.peekArray 512 arr >>= putStrLn . fmap F.castCCharToChar)


-- game loop: takes an update function and the current world
-- manage ticks, events and loop
gameloop :: W.World -> IO ()
gameloop = MySDL.gameloop W.getWindow logic

-- update function of world, changes the color of the screen
logic :: W.World -> [SDL.Event] -> IO W.World
logic world _ = return world
