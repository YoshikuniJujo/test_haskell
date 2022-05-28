{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command.Core where

import Foreign.Ptr
import Data.Word
import Data.Int

import qualified Vulkan.RenderPass.Core as RenderPass
import qualified Vulkan.Pipeline.Graphics.Core as Pipeline
import qualified Vulkan.CommandBuffer.Core as CommandBuffer
import qualified Vulkan.Buffer.Core as Buffer
import qualified Vulkan.Pipeline.Layout.Core as Pipeline.Layout
import qualified Vulkan.Descriptor.Set.Core as DscSet
import qualified Vulkan.Memory.Core as Memory
import qualified Vulkan.Image.Core as Image

#include <vulkan/vulkan.h>

foreign import ccall "vkCmdBeginRenderPass" beginRenderPass ::
	CommandBuffer.C -> Ptr RenderPass.BeginInfo ->
	#{type VkSubpassContents} -> IO ()

foreign import ccall "vkCmdBindPipeline" bindPipeline ::
	CommandBuffer.C -> #{type VkPipelineBindPoint} -> Pipeline.G -> IO ()

foreign import ccall "vkCmdDraw" draw ::
	CommandBuffer.C -> #{type uint32_t} -> #{type uint32_t} ->
	#{type uint32_t} -> #{type uint32_t} -> IO ()

foreign import ccall "vkCmdDrawIndexed" drawIndexed ::
	CommandBuffer.C -> #{type uint32_t} -> #{type uint32_t} ->
	#{type uint32_t} -> #{type int32_t} -> #{type uint32_t} -> IO ()

foreign import ccall "vkCmdEndRenderPass" endRenderPass ::
	CommandBuffer.C -> IO ()

foreign import ccall "vkCmdBindVertexBuffers" bindVertexBuffers ::
	CommandBuffer.C -> #{type uint32_t} -> #{type uint32_t} ->
	Ptr Buffer.B -> Ptr #{type VkDeviceSize} -> IO ()

foreign import ccall "vkCmdCopyBuffer" copyBuffer ::
	CommandBuffer.C -> Buffer.B -> Buffer.B -> #{type uint32_t} ->
	Ptr Buffer.Copy -> IO ()

foreign import ccall "vkCmdBindIndexBuffer" bindIndexBuffer ::
	CommandBuffer.C -> Buffer.B -> #{type VkDeviceSize} ->
	#{type VkIndexType} -> IO ()

foreign import ccall "vkCmdBindDescriptorSets" bindDescriptorSets ::
	CommandBuffer.C -> #{type VkPipelineBindPoint} -> Pipeline.Layout.L ->
	#{type uint32_t} -> #{type uint32_t} -> Ptr DscSet.S ->
	#{type uint32_t} -> Ptr #{type uint32_t} -> IO ()

foreign import ccall "vkCmdPipelineBarrier" pipelineBarrier ::
	CommandBuffer.C ->
	#{type VkPipelineStageFlags} -> #{type VkPipelineStageFlags} ->
	#{type VkDependencyFlags} ->
	#{type uint32_t} -> Ptr Memory.Barrier ->
	#{type uint32_t} -> Ptr Buffer.MemoryBarrier ->
	#{type uint32_t} -> Ptr Image.MemoryBarrier -> IO ()

foreign import ccall "vkCmdCopyBufferToImage" copyBufferToImage ::
	CommandBuffer.C -> Buffer.B -> Image.I -> #{type VkImageLayout} ->
	#{type uint32_t} -> Ptr Buffer.ImageCopy -> IO ()

foreign import ccall "vkCmdBlitImage" blitImage ::
	CommandBuffer.C ->
	Image.I -> #{type VkImageLayout} -> Image.I -> #{type VkImageLayout} ->
	#{type uint32_t} -> Ptr Image.Blit -> #{type VkFilter} -> IO ()
