{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Command.Tmp (

	beginRenderPass, endRenderPass, bindPipeline, bindPipelineCompute,
	bindVertexBuffers, bindIndexBuffer, pushConstants, bindDescriptorSets,
	draw, drawIndexed, dispatch, copyBuffer,
	copyBufferToImage, copyImageToBuffer,
	pipelineBarrier, blitImage,

	) where

import Foreign.Storable
import Data.HeteroList hiding (length)
import Data.Word
import Data.Int

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.CommandBuffer.Type as CommandBuffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer
import qualified Gpu.Vulkan.Buffer.Core as Buffer.C
import qualified Gpu.Vulkan.Device.Middle as Device

import qualified Gpu.Vulkan.RenderPass.Middle as RenderPass
import qualified Gpu.Vulkan.Subpass.Enum as Subpass
import qualified Gpu.Vulkan.Pipeline.Graphics.Middle as Pipeline
import qualified Gpu.Vulkan.Pipeline.Compute.Middle as Pipeline.Compute
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.Pipeline.Layout.Middle as Pipeline.Layout

import qualified Gpu.Vulkan.DescriptorSet.Middle as Descriptor.Set

import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.Image.Middle as Image
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

import Gpu.Vulkan.Command.Middle qualified as M

beginRenderPass :: (Storable n, ClearValuesToCore ct) =>
	CommandBuffer.CC vs -> RenderPass.BeginInfo n ct -> Subpass.Contents -> IO ()
beginRenderPass (CommandBuffer.CC mcb) = M.beginRenderPass mcb

endRenderPass :: CommandBuffer.CC vs -> IO ()
endRenderPass (CommandBuffer.CC mcb) = M.endRenderPass mcb

bindPipeline ::
	CommandBuffer.CC vs -> Pipeline.BindPoint -> Pipeline.G vs ts -> IO ()
bindPipeline (CommandBuffer.CC mcb) = M.bindPipeline mcb

bindPipelineCompute ::
	CommandBuffer.CC vs -> Pipeline.BindPoint -> Pipeline.Compute.C -> IO ()
bindPipelineCompute (CommandBuffer.CC mcb) = M.bindPipelineCompute mcb

draw :: CommandBuffer.CC vs -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
draw (CommandBuffer.CC mcb) = M.draw mcb

drawIndexed :: CommandBuffer.CC vs ->
	Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
drawIndexed (CommandBuffer.CC mcb) = M.drawIndexed mcb

bindVertexBuffers ::
	CommandBuffer.CC vs -> Word32 -> [(Buffer.B, Device.Size)] -> IO ()
bindVertexBuffers (CommandBuffer.CC mcb) = M.bindVertexBuffers mcb

copyBuffer ::
	CommandBuffer.CC vs -> Buffer.B -> Buffer.B -> [Buffer.C.Copy] -> IO ()
copyBuffer (CommandBuffer.CC mcb) = M.copyBuffer mcb

bindIndexBuffer ::
	CommandBuffer.CC vs -> Buffer.B -> Device.Size -> IndexType -> IO ()
bindIndexBuffer (CommandBuffer.CC mcb) = M.bindIndexBuffer mcb

dispatch :: CommandBuffer.CC vs -> Word32 -> Word32 -> Word32 -> IO ()
dispatch (CommandBuffer.CC mcb) = M.dispatch mcb

bindDescriptorSets ::
	CommandBuffer.CC vs -> Pipeline.BindPoint -> Pipeline.Layout.L ->
	Word32 -> [Descriptor.Set.D] -> [Word32] -> IO ()
bindDescriptorSets (CommandBuffer.CC mcb) = M.bindDescriptorSets mcb

pushConstants :: forall vs ts .
	StoreHetero ts =>
	CommandBuffer.CC vs -> Pipeline.Layout.L ->
	ShaderStageFlags -> Word32 -> HeteroList ts -> IO ()
pushConstants (CommandBuffer.CC mcb) = M.pushConstants mcb

pipelineBarrier :: (
	StorableHeteroMap ns, StorableHeteroMap ns', StorableHeteroMap ns''
	) =>
	CommandBuffer.CC vs -> Pipeline.StageFlags -> Pipeline.StageFlags ->
	DependencyFlags ->
	HeteroVarList Memory.M.Barrier ns ->
	HeteroVarList Buffer.M.MemoryBarrier ns' ->
	HeteroVarList Image.MemoryBarrier ns'' -> IO ()
pipelineBarrier (CommandBuffer.CC mcb) = M.pipelineBarrier mcb

copyBufferToImage ::
	CommandBuffer.CC vs -> Buffer.M.B -> Image.I -> Image.Layout ->
	[Buffer.M.ImageCopy] -> IO ()
copyBufferToImage (CommandBuffer.CC mcb) = M.copyBufferToImage mcb

copyImageToBuffer ::
	CommandBuffer.CC vs -> Image.I -> Image.Layout -> Buffer.M.B ->
	[Buffer.M.ImageCopy] -> IO ()
copyImageToBuffer (CommandBuffer.CC mcb) = M.copyImageToBuffer mcb

blitImage :: CommandBuffer.CC v ->
	Image.I -> Image.Layout -> Image.I -> Image.Layout ->
	[Image.Blit] -> Filter -> IO ()
blitImage (CommandBuffer.CC mcb) = M.blitImage mcb
