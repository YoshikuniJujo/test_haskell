{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command.Core where

import Foreign.Ptr
import Data.Word

import qualified Vulkan.RenderPass.Core as RenderPass
import qualified Vulkan.Pipeline.Graphics.Core as Pipeline
import qualified Vulkan.CommandBuffer.Core as CommandBuffer
import qualified Vulkan.Buffer.Core as Buffer

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

foreign import ccall "vkCmdBindVertexBuffers" bindVertexBuffers ::
	CommandBuffer.C -> #{type uint32_t} -> #{type uint32_t} ->
	Ptr Buffer.B -> Ptr #{type VkDeviceSize} -> IO ()

foreign import ccall "vkCmdCopyBuffer" copyBuffer ::
	CommandBuffer.C -> Buffer.B -> Buffer.B -> #{type uint32_t} ->
	Ptr Buffer.Copy -> IO ()
