{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command.Core where

import Foreign.Ptr
import Data.Word

import qualified Vulkan.RenderPass.Core as RenderPass
import qualified Vulkan.Pipeline.Graphics.Core as Pipeline
import qualified Vulkan.CommandBuffer.Core as CommandBuffer

#include <vulkan/vulkan.h>

foreign import ccall "vkCmdBeginRenderPass" beginRenderPass ::
	CommandBuffer.C -> Ptr RenderPass.BeginInfo ->
	#{type VkSubpassContents} -> IO ()

foreign import ccall "vkCmdBindPipeline" bindPipeline ::
	CommandBuffer.C -> #{type VkPipelineBindPoint} -> Pipeline.G -> IO ()

foreign import ccall "vkCmdDraw" draw ::
	CommandBuffer.C -> #{type uint32_t} -> #{type uint32_t} ->
	#{type uint32_t} -> #{type uint32_t} -> IO ()

foreign import ccall "vkCmdEndRenderPass" endRenderPass ::
	CommandBuffer.C -> IO ()
