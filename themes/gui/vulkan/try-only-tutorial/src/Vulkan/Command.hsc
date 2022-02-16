{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command where

import Foreign.Ptr
import Data.Word

import Vulkan (CommandBuffer)

import qualified Vulkan.RenderPass as RenderPass
import qualified Vulkan.Pipeline as Pipeline

#include <vulkan/vulkan.h>

foreign import ccall "vkCmdBeginRenderPass" beginRenderPass ::
	CommandBuffer -> Ptr RenderPass.BeginInfo ->
	#{type VkSubpassContents} -> IO ()

foreign import ccall "vkCmdBindPipeline" bindPipeline ::
	CommandBuffer -> #{type VkPipelineBindPoint} -> Pipeline.Pipeline ->
	IO ()

foreign import ccall "vkCmdDraw" draw ::
	CommandBuffer -> #{type uint32_t} -> #{type uint32_t} ->
	#{type uint32_t} -> #{type uint32_t} -> IO ()

foreign import ccall "vkCmdEndRenderPass" endRenderPass ::
	CommandBuffer -> IO ()
