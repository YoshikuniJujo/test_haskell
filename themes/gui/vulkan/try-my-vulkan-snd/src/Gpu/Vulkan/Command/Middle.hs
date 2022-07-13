{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Command.Middle where

import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.CommandBuffer.Middle as CommandBuffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer
import qualified Gpu.Vulkan.Buffer.Core as Buffer.C
import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Command.Core as C

import qualified Gpu.Vulkan.RenderPass.Middle as RenderPass
import qualified Gpu.Vulkan.Subpass.Enum as Subpass
import qualified Gpu.Vulkan.Pipeline.Graphics.Middle as Pipeline
import qualified Gpu.Vulkan.Pipeline.Compute.Middle as Pipeline.Compute
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.Pipeline.Layout.Middle as Pipeline.Layout

import qualified Gpu.Vulkan.DescriptorSet.Middle as Descriptor.Set

beginRenderPass :: (Pointable n, ClearValueToCore ct) =>
	CommandBuffer.C vs -> RenderPass.BeginInfo n ct -> Subpass.Contents -> IO ()
beginRenderPass (CommandBuffer.C cb)
	rpbi (Subpass.Contents spcnt) = ($ pure) $ runContT do
	prpbi <- RenderPass.beginInfoToCore rpbi
	lift $ C.beginRenderPass cb prpbi spcnt

endRenderPass :: CommandBuffer.C vs -> IO ()
endRenderPass (CommandBuffer.C cb) = C.endRenderPass cb

bindPipeline ::
	CommandBuffer.C vs -> Pipeline.BindPoint -> Pipeline.G vs ts -> IO ()
bindPipeline (CommandBuffer.C cb) (Pipeline.BindPoint pbp) ppl = do
	ppl' <- Pipeline.gToCore ppl
	C.bindPipeline cb pbp ppl'

bindPipelineCompute ::
	CommandBuffer.C vs -> Pipeline.BindPoint -> Pipeline.Compute.C -> IO ()
bindPipelineCompute (CommandBuffer.C cb) (Pipeline.BindPoint pbp) (Pipeline.Compute.C ppl) =
	C.bindPipeline cb pbp ppl

draw :: CommandBuffer.C vs -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
draw (CommandBuffer.C cb) vc ic fv fi = C.draw cb vc ic fv fi

bindVertexBuffers ::
	CommandBuffer.C vs -> Word32 -> [(Buffer.B, Device.Size)] -> IO ()
bindVertexBuffers (CommandBuffer.C c)
	fb ((length &&& unzip) -> (bc, (bs, os))) = ($ pure) $ runContT do
	pb <- ContT $ allocaArray bc
	lift . pokeArray pb $ (\(Buffer.B b) -> b) <$> bs
	po <- ContT $ allocaArray bc
	lift . pokeArray po $ (\(Device.Size sz) -> sz) <$> os
	lift $ C.bindVertexBuffers c fb (fromIntegral bc) pb po

copyBuffer ::
	CommandBuffer.C vs -> Buffer.B -> Buffer.B -> [Buffer.C.Copy] -> IO ()
copyBuffer (CommandBuffer.C c) (Buffer.B s) (Buffer.B d)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift do	pokeArray prs rs
		C.copyBuffer c s d (fromIntegral rc) prs

bindIndexBuffer ::
	CommandBuffer.C vs -> Buffer.B -> Device.Size -> IndexType -> IO ()
bindIndexBuffer
	(CommandBuffer.C cb) (Buffer.B ib) (Device.Size sz) (IndexType it) =
	C.bindIndexBuffer cb ib sz it

dispatch ::
	CommandBuffer.C vs -> Word32 -> Word32 -> Word32 -> IO ()
dispatch (CommandBuffer.C cb) = C.dispatch cb

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
