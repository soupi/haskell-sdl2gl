{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module GLUtils where

import Language.Haskell.TH.Ppr (bytesToString)
import qualified Graphics.GL.Core32 as GL
import qualified Graphics.GL.Types as GL
import qualified Foreign as F
import qualified Foreign.C.String as F
import qualified Foreign.C.Types as F()



printGLError :: IO ()
printGLError =  GL.glGetError >>= \case
  GL.GL_NO_ERROR -> putStrLn "GL_NO_ERROR"
  GL.GL_INVALID_ENUM -> putStrLn "GL_INVALID_ENUM"
  GL.GL_INVALID_VALUE -> putStrLn "GL_INVALID_VALUE"
  GL.GL_INVALID_OPERATION -> putStrLn "GL_INVALID_OPERATION"
  GL.GL_INVALID_FRAMEBUFFER_OPERATION -> putStrLn "GL_INVALID_FRAME_OPERATION"
  GL.GL_OUT_OF_MEMORY -> putStrLn "GL_OUT_OF_MEMORY"
  _ -> putStrLn "I don't know"


printGLVersion :: IO ()
printGLVersion = GL.glGetString GL.GL_VERSION >>= F.peekArray0 0 >>= putStrLn . bytesToString

printShaderInfoLog :: GL.GLuint -> IO ()
printShaderInfoLog shader = F.allocaArray0 511 (\arr -> GL.glGetShaderInfoLog shader 512 F.nullPtr arr >> F.peekArray 512 arr >>= putStrLn . fmap F.castCCharToChar)

