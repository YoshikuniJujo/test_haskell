{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command where

import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
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
import qualified Vulkan.Pipeline.Layout as Pipeline.Layout
import qualified Vulkan.Descriptor.Set as Descriptor.Set

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

bindDescriptorSets ::
	CommandBuffer.C vs -> Pipeline.BindPoint -> Pipeline.Layout.L ->
	Word32 -> [Descriptor.Set.S] -> [Word32] -> IO ()
bindDescriptorSets
	(CommandBuffer.C cb) (Pipeline.BindPoint bp) (Pipeline.Layout.L lyt)
	fs (length &&& id -> (dsc, dss))
	(length &&& id -> (doc, dos)) = ($ pure) $ runContT do
	pdss <- ContT $ allocaArray dsc
	let	cdss = (\(Descriptor.Set.S s) -> s) <$> dss
	lift $ pokeArray pdss cdss
	pdos <- ContT $ allocaArray doc
	lift $ pokeArray pdos dos
	lift $ C.bindDescriptorSets
		cb bp lyt fs (fromIntegral dsc) pdss (fromIntegral doc) pdos
