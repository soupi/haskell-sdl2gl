-- Main: contains entry point, init world function, game loop and game logic

module Main where

--import Debug.Trace (traceShow, trace)
import Control.Monad ((<=<))
import System.Environment (getArgs)
import qualified Data.Word as W
import qualified SDL
import qualified Graphics.GL.Core32 as GL
import qualified Graphics.GL.Types as GL
import qualified Foreign as F (alloca, allocaArray, allocaArray0, pokeArray, peekArray, peekArray0, sizeOf, peek)
import qualified Foreign.Ptr as F (Ptr, castPtr, nullPtr)
import qualified Foreign.C.String as F (withCString)--, castCCharToChar)
import qualified Foreign.C.Types as F
import qualified Foreign.Marshal.Utils as F (with)
--import qualified Foreign.Marshal.Array as F (withArray0)

import qualified MySDL
import qualified GLUtils
import qualified Config as C
import qualified World as W

-- setup window and world and send them to gameloop along with a update logic function
main :: IO ()
main = do
  [posShaderFilePath, clrShaderFilePath] <- getArgs
  MySDL.withWindow C.defaultConfig $ flip (withGL posShaderFilePath clrShaderFilePath) $ gameloop <=< initWorld


withGL :: String -> String -> a -> (a -> IO ()) -> IO ()
withGL posShaderFilePath clrShaderFilePath x go = do
  vao <- F.alloca $ \vao -> GL.glGenVertexArrays 1 vao >> F.peek vao
  GL.glBindVertexArray vao
  --
  vbo <- F.alloca useVBO
  posShader <- compileShader posShaderFilePath GL.GL_VERTEX_SHADER
  clrShader <- compileShader clrShaderFilePath GL.GL_FRAGMENT_SHADER
  shaderProgram <- createShaderProgram [(posShader, Nothing), (clrShader, Just (0, "outcolor"))]
  -- posShader
  posAttrib <- F.withCString "position" $ GL.glGetAttribLocation shaderProgram
  GL.glEnableVertexAttribArray (fromIntegral posAttrib)
  GL.glVertexAttribPointer (fromIntegral posAttrib) 2 GL.GL_FLOAT  GL.GL_FALSE 0 F.nullPtr
  --
  GL.glDrawArrays GL.GL_TRIANGLES 0 3

  go x

  GL.glDeleteProgram shaderProgram
  GL.glDeleteShader posShader
  GL.glDeleteShader clrShader
  F.with vbo $ GL.glDeleteBuffers 1
  F.with vao $ GL.glDeleteVertexArrays 1

-- init World
initWorld :: SDL.Window -> IO W.World
initWorld window = return $ W.World window


useVBO :: F.Ptr GL.GLuint -> IO W.Word32
useVBO vboPtr = do
  vbo <- GL.glGenBuffers 1 vboPtr >> F.peek vboPtr
  GL.glBindBuffer GL.GL_ARRAY_BUFFER vbo
  F.allocaArray 6 $ sendVerticesToGPU [0, 0.5, 0.5, -0.5, -0.5, -0.5]
  return vbo

sendVerticesToGPU :: [Float] -> F.Ptr F.CFloat -> IO ()
sendVerticesToGPU vertices vertices_array = do
  F.pokeArray vertices_array $ map F.CFloat vertices
  GL.glBufferData GL.GL_ARRAY_BUFFER (F.CPtrdiff (fromIntegral (F.sizeOf vertices_array))) (F.castPtr vertices_array) GL.GL_STATIC_DRAW

createShaderProgram :: [(GL.GLuint, Maybe (GL.GLuint, String))] -> IO GL.GLuint
createShaderProgram shaders = do
  shaderProgram <- GL.glCreateProgram
  mapM_ (GL.glAttachShader shaderProgram . fst) shaders
  mapM_ (maybe (return ()) (uncurry (bindFrag shaderProgram)) . snd) shaders
  GL.glLinkProgram shaderProgram
  GL.glUseProgram shaderProgram
  return shaderProgram


bindFrag :: GL.GLuint -> GL.GLuint -> String -> IO ()
bindFrag shaderProgram clr name = F.withCString name (GL.glBindFragDataLocation shaderProgram clr)

compileShader :: String -> GL.GLenum -> IO GL.GLuint
compileShader shaderSourcePath shaderType = do
  shaderSource <- readFile shaderSourcePath
  shader <- GL.glCreateShader shaderType
  F.withCString shaderSource $ flip F.with (\shaderSourcePtr -> GL.glShaderSource shader 1 shaderSourcePtr F.nullPtr)
  GL.glCompileShader shader
  F.alloca (\status -> GL.glGetShaderiv shader GL.GL_COMPILE_STATUS status >> F.peek status >>= print . (==) GL.GL_TRUE)
  return shader

-- game loop: takes an update function and the current world
-- manage ticks, events and loop
gameloop :: W.World -> IO ()
gameloop = MySDL.gameloop logic

-- update function of world
logic :: W.World -> [SDL.Event] -> IO W.World
logic world _ = do
  GL.glClearColor 0 0 1 1
  GL.glClear GL.GL_COLOR_BUFFER_BIT
  GL.glDrawArrays GL.GL_TRIANGLES 0 3
  SDL.glSwapWindow $ W.getWindow world
  return world
