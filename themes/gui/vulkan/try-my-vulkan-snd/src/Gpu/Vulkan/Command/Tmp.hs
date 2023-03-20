{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Command.Tmp (

	beginRenderPass, endRenderPass, bindPipeline, bindPipelineCompute,
	bindVertexBuffers, bindIndexBuffer,
	pushConstants',
	bindDescriptorSets,
	draw, drawIndexed, dispatch, copyBuffer,
	copyBufferToImage, copyImageToBuffer,
	pipelineBarrier, blitImage,

	) where

import Foreign.Storable
import Foreign.Storable.HeteroList
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*), pattern (:**))
import Data.Word
import Data.Int

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.CommandBuffer.Type as CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer.Middle as CommandBuffer.M
import qualified Gpu.Vulkan.Buffer.Middle as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.C
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

beginRenderPass :: (Storable n, ClearValueListToCore ct) =>
	CommandBuffer.M.C -> RenderPass.BeginInfo n ct -> Subpass.Contents -> IO ()
beginRenderPass mcb = M.beginRenderPass mcb

endRenderPass :: CommandBuffer.M.C -> IO ()
endRenderPass = M.endRenderPass

bindPipeline ::
	CommandBuffer.M.C -> Pipeline.BindPoint -> Pipeline.G -> IO ()
bindPipeline = M.bindPipeline

bindPipelineCompute ::
	CommandBuffer.M.C -> Pipeline.BindPoint -> Pipeline.Compute.C -> IO ()
bindPipelineCompute = M.bindPipelineCompute

draw :: CommandBuffer.M.C -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
draw = M.draw

drawIndexed :: CommandBuffer.M.C  ->
	Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
drawIndexed = M.drawIndexed

bindVertexBuffers ::
	CommandBuffer.M.C -> Word32 -> [(Buffer.B, Device.Size)] -> IO ()
bindVertexBuffers = M.bindVertexBuffers

copyBuffer ::
	CommandBuffer.M.C -> Buffer.B -> Buffer.B -> [Buffer.C.Copy] -> IO ()
copyBuffer = M.copyBuffer

bindIndexBuffer ::
	CommandBuffer.M.C -> Buffer.B -> Device.Size -> IndexType -> IO ()
bindIndexBuffer = M.bindIndexBuffer

dispatch :: CommandBuffer.M.C -> Word32 -> Word32 -> Word32 -> IO ()
dispatch = M.dispatch

bindDescriptorSets ::
	CommandBuffer.M.C -> Pipeline.BindPoint -> Pipeline.Layout.L ->
	Word32 -> [Descriptor.Set.D] -> [Word32] -> IO ()
bindDescriptorSets = M.bindDescriptorSets

pushConstants' :: forall vs ts . PokableList ts =>
	CommandBuffer.M.C -> Pipeline.Layout.L ->
	ShaderStageFlags -> Word32 -> HeteroParList.L ts -> IO ()
pushConstants' = M.pushConstants

pipelineBarrier :: (
	WithPokedHeteroToListCpsM ns, WithPokedHeteroToListCpsM ns',
	WithPokedHeteroToListCpsM ns'' ) =>
	CommandBuffer.M.C -> Pipeline.StageFlags -> Pipeline.StageFlags ->
	DependencyFlags ->
	HeteroParList.PL Memory.M.Barrier ns ->
	HeteroParList.PL Buffer.M.MemoryBarrier ns' ->
	HeteroParList.PL Image.MemoryBarrier ns'' -> IO ()
pipelineBarrier = M.pipelineBarrier

copyBufferToImage ::
	CommandBuffer.M.C -> Buffer.M.B -> Image.I -> Image.Layout ->
	[Buffer.M.ImageCopy] -> IO ()
copyBufferToImage = M.copyBufferToImage

copyImageToBuffer ::
	CommandBuffer.M.C -> Image.I -> Image.Layout -> Buffer.M.B ->
	[Buffer.M.ImageCopy] -> IO ()
copyImageToBuffer = M.copyImageToBuffer

blitImage :: CommandBuffer.M.C ->
	Image.I -> Image.Layout -> Image.I -> Image.Layout ->
	[Image.Blit] -> Filter -> IO ()
blitImage = M.blitImage
