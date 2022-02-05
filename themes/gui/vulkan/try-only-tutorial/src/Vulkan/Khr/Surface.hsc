{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface where

import Foreign.Ptr

import Vulkan.Instance (Instance)

data SurfaceTag
type Surface = Ptr SurfaceTag

foreign import ccall "vkDestroySurfaceKHR" destroy ::
	Instance -> Surface -> Ptr () -> IO ()
