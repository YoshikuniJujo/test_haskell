{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command where

import Foreign.Pointable
import Control.Monad.Cont
import Data.Word
import Data.Int

import Vulkan

import qualified Vulkan.CommandBuffer as CommandBuffer
import qualified Vulkan.RenderPass as RenderPass
import qualified Vulkan.Subpass.Enum as Subpass
import qualified Vulkan.Pipeline.Graphics as Pipeline
import qualified Vulkan.Pipeline.Enum as Pipeline
import qualified Vulkan.Command.Core as C

beginRenderPass :: (Pointable n, ClearValueToCore ct) =>
	CommandBuffer.C vs -> RenderPass.BeginInfo n ct -> Subpass.Contents -> IO ()
beginRenderPass (CommandBuffer.C cb)
	rpbi (Subpass.Contents spcnt) = ($ pure) $ runContT do
	prpbi <- RenderPass.beginInfoToCore rpbi
	lift $ C.beginRenderPass cb prpbi spcnt

bindPipeline ::
	CommandBuffer.C vs -> Pipeline.BindPoint -> Pipeline.G vs ts -> IO ()
bindPipeline (CommandBuffer.C cb) (Pipeline.BindPoint pbp) (Pipeline.G ppl) =
	C.bindPipeline cb pbp ppl

draw :: CommandBuffer.C vs -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
draw (CommandBuffer.C cb) vc ic fv fi = C.draw cb vc ic fv fi

drawIndexed :: CommandBuffer.C vs ->
	Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
drawIndexed (CommandBuffer.C cb) idxc istc fidx vo fist =
	C.drawIndexed cb idxc istc fidx vo fist

endRenderPass :: CommandBuffer.C vs -> IO ()
endRenderPass (CommandBuffer.C cb) = C.endRenderPass cb
