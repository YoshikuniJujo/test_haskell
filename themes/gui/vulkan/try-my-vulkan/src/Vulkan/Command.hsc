{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.CommandBuffer (CommandBuffer(..))
import Vulkan.SubpassContents
import Vulkan.PipelineBindPoint

import qualified Vulkan.RenderPass as RenderPass
import qualified Vulkan.RenderPass.Internal as RenderPass.I

#include <vulkan/vulkan.h>

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

draw :: CommandBuffer vs ts -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
draw cb (word32ToUint32T -> vc) (word32ToUint32T -> ic)
	(word32ToUint32T -> fv) (word32ToUint32T -> fi) = c_vkCmdDraw cb vc ic fv fi

foreign import ccall "vkCmdDraw" c_vkCmdDraw ::
	CommandBuffer vs ts -> #{type uint32_t} -> #{type uint32_t} ->
	#{type uint32_t} -> #{type uint32_t} -> IO ()

endRenderPass :: CommandBuffer vs ts -> IO ()
endRenderPass = c_vkCmdEndRenderPass

foreign import ccall "vkCmdEndRenderPass" c_vkCmdEndRenderPass ::
	CommandBuffer vs ts -> IO ()
