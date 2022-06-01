{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Queue.Core where

import Foreign.Ptr
import Data.Word
import Data.Int

import Vulkan.Core

import {-# SOURCE #-} qualified Vulkan.Fence.Core as Fence

#include <vulkan/vulkan.h>

data QueueTag
type Queue = Ptr QueueTag

foreign import ccall "vkQueueSubmit" queueSubmit ::
	Queue -> #{type uint32_t} -> Ptr SubmitInfo -> Fence.F ->
	IO #{type VkResult}

foreign import ccall "vkQueueWaitIdle" queueWaitIdle ::
	Queue -> IO #{type VkResult}
