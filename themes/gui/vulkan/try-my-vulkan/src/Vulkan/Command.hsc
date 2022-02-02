{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.CommandBuffer (CommandBuffer(..))
import Vulkan.SubpassContents
import Vulkan.PipelineBindPoint

import qualified Vulkan.RenderPass as RenderPass
import qualified Vulkan.RenderPass.Internal as RenderPass.I

beginRenderPass :: Pointable n =>
	CommandBuffer vs ts -> RenderPass.BeginInfo n -> SubpassContents -> IO ()
beginRenderPass cb bi cs = ($ pure) $ runContT do
	RenderPass.I.BeginInfo_ fbi <- RenderPass.beginInfoToC bi
	pbi <- ContT $ withForeignPtr fbi
	lift $ c_vkCmdBeginRenderPass cb pbi cs

foreign import ccall "vkCmdBeginRenderPass" c_vkCmdBeginRenderPass ::
	CommandBuffer vs ts -> Ptr RenderPass.I.BeginInfo -> SubpassContents -> IO ()

bindPipeline ::
	CommandBuffer vs ts -> PipelineBindPoint -> Pipeline vs ts -> IO ()
bindPipeline = c_vkCmdBindPipeline

foreign import ccall "vkCmdBindPipeline" c_vkCmdBindPipeline ::
	CommandBuffer vs ts -> PipelineBindPoint -> Pipeline vs ts -> IO ()
