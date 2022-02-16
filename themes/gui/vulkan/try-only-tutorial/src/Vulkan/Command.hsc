{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command where

import Foreign.Ptr
import Data.Word

import qualified Vulkan.CommandBuffer as CommandBuffer
import qualified Vulkan.RenderPass as RenderPass

#include <vulkan/vulkan.h>

foreign import ccall "vkCmdBeginRenderPass" beginRenderPass ::
	CommandBuffer.CommandBuffer -> Ptr RenderPass.BeginInfo ->
	#{type VkSubpassContents} -> IO ()
