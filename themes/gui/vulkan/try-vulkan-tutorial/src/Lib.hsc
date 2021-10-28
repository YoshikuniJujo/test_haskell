{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Enum
import Data.Word

import qualified Data.Vector as V
import qualified Data.ByteString as BS

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

glfwGetRequiredInstanceExtensions :: IO (V.Vector BS.ByteString)
glfwGetRequiredInstanceExtensions = alloca \pn -> do
	ps <- c_glfwGetRequiredInstanceExtensions pn
	n <- peek pn
	byteStringVector (fromIntegral n) ps

foreign import ccall "glfwGetRequiredInstanceExtensions"
	c_glfwGetRequiredInstanceExtensions :: Ptr Word32 -> IO (Ptr CString)

byteStringVector :: Int -> Ptr CString -> IO (V.Vector BS.ByteString)
byteStringVector n p = V.mapM BS.packCString =<< makeVector n p

makeVector :: forall a . Storable a => Int -> Ptr a -> IO (V.Vector a)
makeVector n p = V.generateM n \i ->
	peek $ p `plusPtr` (i * sizeOf (undefined :: a))
