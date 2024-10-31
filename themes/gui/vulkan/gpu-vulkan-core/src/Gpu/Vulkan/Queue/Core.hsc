{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Queue.Core (

	-- SUBMIT AND WAIT IDLE

	Q, submit, waitIdle

	) where

import Foreign.Ptr
import Data.Word
import Data.Int

import Gpu.Vulkan.Core

import {-# SOURCE #-} qualified Gpu.Vulkan.Fence.Core as Fence

#include <vulkan/vulkan.h>

data QTag
type Q = Ptr QTag

foreign import ccall "vkQueueSubmit" submit ::
	Q -> #{type uint32_t} -> Ptr SubmitInfo -> Fence.F ->
	IO #{type VkResult}

foreign import ccall "vkQueueWaitIdle" waitIdle :: Q -> IO #{type VkResult}
