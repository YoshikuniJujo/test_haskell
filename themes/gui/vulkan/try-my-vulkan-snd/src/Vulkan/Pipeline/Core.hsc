{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Core where

import Foreign.Ptr

import qualified Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Vulkan.Device.Core as Device

data PTag
type P = Ptr PTag

foreign import ccall "vkDestroyPipeline" destroy ::
	Device.D -> P -> Ptr AllocationCallbacks.A -> IO ()
