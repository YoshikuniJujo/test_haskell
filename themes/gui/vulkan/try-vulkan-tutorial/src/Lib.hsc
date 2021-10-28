{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Enum

#include <GLFW/glfw3.h>

foreign import ccall "glfwInit" glfwInit :: IO ()

foreign import ccall "glfwTerminate" glfwTerminate :: IO ()

enum "GlfwWindowHint" ''CInt [''Show] [
	("GlfwStereo", #{const GLFW_STEREO}),
	("GlfwDoublebuffer", #{const GLFW_DOUBLEBUFFER}),
	("GlfwClientApi", #{const GLFW_CLIENT_API}),
	("GlfwContextCreationApi", #{const GLFW_CONTEXT_CREATION_API}),
	("GlfwOpenglForwardCompat", #{const GLFW_OPENGL_FORWARD_COMPAT}),
	("GlfwOpenglProfile", #{const GLFW_OPENGL_PROFILE}),
	("GlfwResizable", #{const GLFW_RESIZABLE}) ]

enum "GlfwWindowHintValue" ''CInt [''Show] [
	("GlfwNoApi", #{const GLFW_NO_API}),
	("GlfwFalse", #{const GLFW_FALSE}) ]

foreign import ccall "glfwWindowHint"
	glfwWindowHint :: GlfwWindowHint -> GlfwWindowHintValue -> IO ()

newtype GlfwWindow = GlfwWindow (Ptr GlfwWindow) deriving Show

glfwCreateWindowSimple :: CInt -> CInt -> String -> IO GlfwWindow
glfwCreateWindowSimple w h ttl = withCString ttl \cttl ->
	c_glfwCreateWindow w h cttl nullPtr nullPtr

foreign import ccall "glfwCreateWindow" c_glfwCreateWindow ::
	CInt -> CInt -> CString -> Ptr () -> Ptr () -> IO GlfwWindow

foreign import ccall "glfwDestroyWindow"
	glfwDestroyWindow :: GlfwWindow -> IO ()

glfwWindowShouldClose :: GlfwWindow -> IO Bool
glfwWindowShouldClose w = (<$> c_glfwWindowShouldClose w) \case
	0 -> False; _ -> True

foreign import ccall "glfwWindowShouldClose"
	c_glfwWindowShouldClose :: GlfwWindow -> IO CInt

foreign import ccall "glfwPollEvents" glfwPollEvents :: IO ()
