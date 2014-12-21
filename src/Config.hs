-- Config: Window and OpenGL configuration

{-# LANGUAGE OverloadedStrings #-}

module Config where

import Foreign.C.Types
import Data.Text
import Linear
import qualified SDL

-- window configuration
data Config = Config { title :: Text, glConf :: SDL.OpenGLConfig, size :: V2 CInt }

-- Config: OpenGL Core 3.2
myOpenGLConfig :: SDL.OpenGLConfig
myOpenGLConfig = SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Normal 3 2 }

-- window configuration
defaultConfig :: Config
defaultConfig = Config "Hello SDL" myOpenGLConfig $ V2 640 480

