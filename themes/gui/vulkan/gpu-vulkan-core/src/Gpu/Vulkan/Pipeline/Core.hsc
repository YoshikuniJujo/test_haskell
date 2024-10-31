{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Core (

	-- * DESTROY

	destroy, P

	) where

import Foreign.Ptr

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device

data PTag
type P = Ptr PTag

foreign import ccall "vkDestroyPipeline" destroy ::
	Device.D -> P -> Ptr AllocationCallbacks.A -> IO ()
