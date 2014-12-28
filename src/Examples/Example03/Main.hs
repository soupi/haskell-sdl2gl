-- Main: contains entry point, init world function, game loop and game logic

{-# LANGUAGE LambdaCase #-}

module Main where

--import Debug.Trace (traceShow, trace)
import Control.Monad ((<=<))
import System.Environment (getArgs)
import qualified Data.Word as W
import qualified SDL
import qualified Graphics.GL.Core32 as GL
import qualified Graphics.GL.Types as GL
import qualified Foreign as F (alloca, peek)
import qualified Foreign.Ptr as F (Ptr, castPtr, nullPtr)
import qualified Foreign.C.String as F (withCString)
import qualified Foreign.C.Types as F
import qualified Foreign.Marshal.Utils as F (with)
import qualified Foreign.Marshal.Array as F (withArray)
import Data.Int (Int64)

import qualified MySDL
import qualified GLUtils
import qualified Config as C
import qualified World as W

-- setup window and world and send them to gameloop along with a update logic function
main :: IO ()
main =
  getArgs >>= \case
    [posShaderFilePath, clrShaderFilePath] ->
      MySDL.withWindow C.defaultConfig $ flip (withGL posShaderFilePath clrShaderFilePath) $ gameloop <=< initWorld
    _ -> putStrLn "usage: cabal run example03 <vertex shader> <fragment shader>"


withGL :: String -> String -> SDL.Window -> (SDL.Window -> IO ()) -> IO ()
withGL posShaderFilePath clrShaderFilePath win go = do
  -- Create Vertex Array Object
  vao <- F.alloca $ \vao -> GL.glGenVertexArrays 1 vao >> F.peek vao
  GL.glBindVertexArray vao
  -- Create a Vertex Buffer Object and copy the vertex data to it
  vbo <- F.alloca useVBO
  -- Create and compile the vertex shader
  Just posShader <- readFile posShaderFilePath >>= compileShader GL.GL_VERTEX_SHADER
  -- Create and compile the fragment shader
  Just clrShader <- readFile clrShaderFilePath >>= compileShader GL.GL_FRAGMENT_SHADER
  -- Link the vertex and fragment shader into a shader program
  shaderProgram <- createShaderProgram [(posShader, Nothing), (clrShader, Just (0, "outcolor"))]
  -- Specify the layout of the vertex data
  posAttrib <- F.withCString "position" $ GL.glGetAttribLocation shaderProgram
  GL.glEnableVertexAttribArray (fromIntegral posAttrib)
  GL.glVertexAttribPointer (fromIntegral posAttrib) 2 GL.GL_FLOAT  GL.GL_FALSE 0 F.nullPtr
  --

  go win

  --
  -- Cleanup
  GL.glDeleteProgram shaderProgram
  GL.glDeleteShader posShader
  GL.glDeleteShader clrShader
  F.with vbo $ GL.glDeleteBuffers 1
  F.with vao $ GL.glDeleteVertexArrays 1

-- init World
initWorld :: SDL.Window -> IO W.World
initWorld window = return $ W.World window


-- Create a Vertex Buffer Object and copy the vertex data to it
useVBO :: F.Ptr GL.GLuint -> IO W.Word32
useVBO vboPtr = do
  vbo <- GL.glGenBuffers 1 vboPtr >> F.peek vboPtr
  GL.glBindBuffer GL.GL_ARRAY_BUFFER vbo
  F.withArray (map F.CFloat [0, 0.5, 0.5, -0.5, -0.5, -0.5]) (sendVerticesToGPU 24)
  return vbo

-- copy vertex data to gpu
sendVerticesToGPU :: Int64 -> F.Ptr F.CFloat -> IO ()
sendVerticesToGPU size vertices_array =
  GL.glBufferData GL.GL_ARRAY_BUFFER (F.CPtrdiff size) (F.castPtr vertices_array) GL.GL_STATIC_DRAW

-- Create Shader program, attach shaders, link and use
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


-- compile shader
compileShader :: GL.GLenum -> String -> IO (Maybe GL.GLuint)
compileShader shaderType shaderSource = do
  shader <- GL.glCreateShader shaderType
  F.withCString shaderSource $ flip F.with (\shaderSourcePtr -> GL.glShaderSource shader 1 shaderSourcePtr F.nullPtr)
  GL.glCompileShader shader
  success <- F.alloca $ \status -> GL.glGetShaderiv shader GL.GL_COMPILE_STATUS status >> F.peek status
  print $ success == GL.GL_TRUE
  if success == GL.GL_TRUE then (return . Just) shader else return Nothing

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
