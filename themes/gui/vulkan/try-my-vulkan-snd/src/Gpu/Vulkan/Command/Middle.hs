{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Command.Middle (
	beginRenderPassMiddle, endRenderPassMiddle,
	bindPipelineMiddle, bindPipelineComputeMiddle,
	bindVertexBuffersMiddle, bindIndexBufferMiddle,
	pushConstantsMiddle, bindDescriptorSetsMiddle,
	drawMiddle, drawIndexedMiddle, dispatchMiddle,
	copyBufferMiddle, copyBufferToImageMiddle, copyImageToBufferMiddle,
	pipelineBarrierMiddle, blitImageMiddle ) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.HeteroList hiding (length)
import Data.Word
import Data.Int
import Data.IORef

import Gpu.Vulkan.Middle.Internal
import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.CommandBuffer.Middle.Internal as CommandBuffer.M
import qualified Gpu.Vulkan.Buffer.Middle.Internal as Buffer
import qualified Gpu.Vulkan.Buffer.Core as Buffer.C
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Command.Core as C

import qualified Gpu.Vulkan.RenderPass.Middle.Internal as RenderPass
import qualified Gpu.Vulkan.Subpass.Enum as Subpass
import qualified Gpu.Vulkan.Pipeline.Graphics.Middle.Internal as Pipeline
import qualified Gpu.Vulkan.Pipeline.Compute.Middle.Internal as Pipeline.Compute
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.Pipeline.Layout.Middle.Internal as Pipeline.Layout

import qualified Gpu.Vulkan.DescriptorSet.Middle.Internal as Descriptor.Set

import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.Image.Middle.Internal as Image
import qualified Gpu.Vulkan.Buffer.Middle.Internal as Buffer.M
import qualified Gpu.Vulkan.Memory.Middle.Internal as Memory.M

beginRenderPassMiddle :: (Pointable n, ClearValuesToCore ct) =>
	CommandBuffer.M.C -> RenderPass.BeginInfo n ct -> Subpass.Contents -> IO ()
beginRenderPassMiddle (CommandBuffer.M.C _ cb)
	rpbi (Subpass.Contents spcnt) = ($ pure) $ runContT do
	prpbi <- RenderPass.beginInfoToCore rpbi
	lift $ C.beginRenderPass cb prpbi spcnt

endRenderPassMiddle :: CommandBuffer.M.C -> IO ()
endRenderPassMiddle (CommandBuffer.M.C _ cb) = C.endRenderPass cb

bindPipelineMiddle ::
	CommandBuffer.M.C -> Pipeline.BindPoint -> Pipeline.G vs ts -> IO ()
bindPipelineMiddle (CommandBuffer.M.C rppl cb) (Pipeline.BindPoint pbp) ppl = do
	ppl0 <- readIORef rppl
	ppl' <- Pipeline.gToCore ppl
	when (ppl' /= ppl0) do
		writeIORef rppl ppl'
		C.bindPipeline cb pbp ppl'

bindPipelineComputeMiddle ::
	CommandBuffer.M.C -> Pipeline.BindPoint -> Pipeline.Compute.C -> IO ()
bindPipelineComputeMiddle (CommandBuffer.M.C rppl cb) (Pipeline.BindPoint pbp) (Pipeline.Compute.C ppl) = do
	ppl0 <- readIORef rppl
	when (ppl /= ppl0) do
		writeIORef rppl ppl
		C.bindPipeline cb pbp ppl

bindVertexBuffersMiddle ::
	CommandBuffer.M.C -> Word32 -> [(Buffer.B, Device.Size)] -> IO ()
bindVertexBuffersMiddle (CommandBuffer.M.C _ c)
	fb ((length &&& unzip) -> (bc, (bs, os))) = ($ pure) $ runContT do
	pb <- ContT $ allocaArray bc
	lift . pokeArray pb $ (\(Buffer.B b) -> b) <$> bs
	po <- ContT $ allocaArray bc
	lift . pokeArray po $ (\(Device.Size sz) -> sz) <$> os
	lift $ C.bindVertexBuffers c fb (fromIntegral bc) pb po

bindIndexBufferMiddle ::
	CommandBuffer.M.C -> Buffer.B -> Device.Size -> IndexType -> IO ()
bindIndexBufferMiddle
	(CommandBuffer.M.C _ cb) (Buffer.B ib) (Device.Size sz) (IndexType it) =
	C.bindIndexBuffer cb ib sz it

pushConstantsMiddle :: forall ts .
	StoreHetero ts =>
	CommandBuffer.M.C -> Pipeline.Layout.L ->
	ShaderStageFlags -> Word32 -> HeteroList ts -> IO ()
pushConstantsMiddle (CommandBuffer.M.C _ cb) (Pipeline.Layout.L lyt)
	(ShaderStageFlagBits ss) ost xs = ($ pure) $ runContT do
	let	sz :: Integral n => n
		sz = fromIntegral $ storeHeteroSize @ts 0
	p <- ContT $ allocaBytes sz
	lift do	storeHetero p xs
		C.pushConstants cb lyt ss ost sz p

bindDescriptorSetsMiddle ::
	CommandBuffer.M.C -> Pipeline.BindPoint -> Pipeline.Layout.L ->
	Word32 -> [Descriptor.Set.D] -> [Word32] -> IO ()
bindDescriptorSetsMiddle
	(CommandBuffer.M.C _ cb) (Pipeline.BindPoint bp) (Pipeline.Layout.L lyt)
	fs (length &&& id -> (dsc, dss))
	(length &&& id -> (doc, dos)) = ($ pure) $ runContT do
	pdss <- ContT $ allocaArray dsc
	let	cdss = (\(Descriptor.Set.D s) -> s) <$> dss
	lift $ pokeArray pdss cdss
	pdos <- ContT $ allocaArray doc
	lift $ pokeArray pdos dos
	lift $ C.bindDescriptorSets
		cb bp lyt fs (fromIntegral dsc) pdss (fromIntegral doc) pdos

drawMiddle :: CommandBuffer.M.C -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
drawMiddle (CommandBuffer.M.C _ cb) vc ic fv fi = C.draw cb vc ic fv fi

drawIndexedMiddle :: CommandBuffer.M.C ->
	Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
drawIndexedMiddle (CommandBuffer.M.C _ cb) idxc istc fidx vo fist =
	C.drawIndexed cb idxc istc fidx vo fist

dispatchMiddle :: CommandBuffer.M.C -> Word32 -> Word32 -> Word32 -> IO ()
dispatchMiddle (CommandBuffer.M.C _ cb) = C.dispatch cb

copyBufferMiddle ::
	CommandBuffer.M.C -> Buffer.B -> Buffer.B -> [Buffer.C.Copy] -> IO ()
copyBufferMiddle (CommandBuffer.M.C _ c) (Buffer.B s) (Buffer.B d)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift do	pokeArray prs rs
		C.copyBuffer c s d (fromIntegral rc) prs

copyBufferToImageMiddle ::
	CommandBuffer.M.C -> Buffer.M.B -> Image.I -> Image.Layout ->
	[Buffer.M.ImageCopy] -> IO ()
copyBufferToImageMiddle (CommandBuffer.M.C _ cb)
	(Buffer.M.B sb) (Image.I rdi) (Image.Layout dil)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift . pokeArray prs $ Buffer.M.imageCopyToCore <$> rs
	lift do	(_, di) <- readIORef rdi
		C.copyBufferToImage cb sb di dil (fromIntegral rc) prs

copyImageToBufferMiddle ::
	CommandBuffer.M.C -> Image.I -> Image.Layout -> Buffer.M.B ->
	[Buffer.M.ImageCopy] -> IO ()
copyImageToBufferMiddle (CommandBuffer.M.C _ cb)
	(Image.I rsi) (Image.Layout sil) (Buffer.M.B db)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift . pokeArray prs $ Buffer.M.imageCopyToCore <$> rs
	lift do	(_, si) <- readIORef rsi
		C.copyImageToBuffer cb si sil db (fromIntegral rc) prs

pipelineBarrierMiddle :: (
	StorableHeteroMap ns, StorableHeteroMap ns', StorableHeteroMap ns''
	) =>
	CommandBuffer.M.C -> Pipeline.StageFlags -> Pipeline.StageFlags ->
	DependencyFlags ->
	HeteroVarList Memory.M.Barrier ns ->
	HeteroVarList Buffer.M.MemoryBarrier ns' ->
	HeteroVarList Image.MemoryBarrier ns'' -> IO ()
pipelineBarrierMiddle (CommandBuffer.M.C _ cb)
	(Pipeline.StageFlagBits ssm) (Pipeline.StageFlagBits dsm)
	(DependencyFlagBits dfs)
	mbs bbs ibs = ($ pure) $ runContT do
	cmbs <- storableHeteroMapM mbs Memory.M.barrierToCore
	let	mbc = length cmbs
	pmbs <- ContT $ allocaArray mbc
	lift $ pokeArray pmbs cmbs
	cbbs <- storableHeteroMapM bbs Buffer.M.memoryBarrierToCore
	let	bbc = length cbbs
	pbbs <- ContT $ allocaArray bbc
	lift $ pokeArray pbbs cbbs
	cibs <- storableHeteroMapM ibs Image.memoryBarrierToCore
	let	ibc = length cibs
	pibs <- ContT $ allocaArray ibc
	lift $ pokeArray pibs cibs
	lift $ C.pipelineBarrier cb ssm dsm dfs (fromIntegral mbc) pmbs
		(fromIntegral bbc) pbbs (fromIntegral ibc) pibs

blitImageMiddle :: CommandBuffer.M.C ->
	Image.I -> Image.Layout -> Image.I -> Image.Layout ->
	[Image.Blit] -> Filter -> IO ()
blitImageMiddle (CommandBuffer.M.C _ cb)
	(Image.I rsrc) (Image.Layout srcLyt) (Image.I rdst) (Image.Layout dstLyt)
	(length &&& id -> (bltc, blts)) (Filter ft) = ($ pure) $ runContT do
	pblts <- ContT $ allocaArray bltc
	lift . pokeArray pblts $ Image.blitToCore <$> blts
	lift do (_, src) <- readIORef rsrc
		(_, dst) <- readIORef rdst
		C.blitImage cb src srcLyt dst dstLyt (fromIntegral bltc) pblts ft
