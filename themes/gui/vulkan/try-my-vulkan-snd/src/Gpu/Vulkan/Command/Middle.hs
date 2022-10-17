{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Command.Middle (
	beginRenderPass, endRenderPass, bindPipeline, bindPipelineCompute,
	bindVertexBuffers, bindIndexBuffer, pushConstants, bindDescriptorSets,
	draw, drawIndexed, dispatch, copyBuffer, copyBufferToImage,
	pipelineBarrier, blitImage ) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.HeteroList hiding (length)
import Data.Word
import Data.Int
import Data.IORef

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.CommandBuffer.Middle.Internal as CommandBuffer.M
import qualified Gpu.Vulkan.CommandBuffer.Type as CommandBuffer
import qualified Gpu.Vulkan.Buffer.Middle.Internal as Buffer
import qualified Gpu.Vulkan.Buffer.Core as Buffer.C
import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Command.Core as C

import qualified Gpu.Vulkan.RenderPass.Middle as RenderPass
import qualified Gpu.Vulkan.Subpass.Enum as Subpass
import qualified Gpu.Vulkan.Pipeline.Graphics.Middle as Pipeline
import qualified Gpu.Vulkan.Pipeline.Compute.Middle as Pipeline.Compute
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.Pipeline.Layout.Middle as Pipeline.Layout

import qualified Gpu.Vulkan.DescriptorSet.Middle.Internal as Descriptor.Set

import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.Image.Middle as Image
import qualified Gpu.Vulkan.Buffer.Middle.Internal as Buffer.M
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

beginRenderPass :: (Pointable n, ClearValuesToCore ct) =>
	CommandBuffer.CC vs -> RenderPass.BeginInfo n ct -> Subpass.Contents -> IO ()
beginRenderPass (CommandBuffer.CC (CommandBuffer.M.C _ cb))
	rpbi (Subpass.Contents spcnt) = ($ pure) $ runContT do
	prpbi <- RenderPass.beginInfoToCore rpbi
	lift $ C.beginRenderPass cb prpbi spcnt

endRenderPass :: CommandBuffer.CC vs -> IO ()
endRenderPass (CommandBuffer.CC (CommandBuffer.M.C _ cb)) = C.endRenderPass cb

bindPipeline ::
	CommandBuffer.CC vs -> Pipeline.BindPoint -> Pipeline.G vs ts -> IO ()
bindPipeline (CommandBuffer.CC (CommandBuffer.M.C rppl cb)) (Pipeline.BindPoint pbp) ppl = do
	ppl0 <- readIORef rppl
	ppl' <- Pipeline.gToCore ppl
	when (ppl' /= ppl0) do
		writeIORef rppl ppl'
		C.bindPipeline cb pbp ppl'

bindPipelineCompute ::
	CommandBuffer.CC vs -> Pipeline.BindPoint -> Pipeline.Compute.C -> IO ()
bindPipelineCompute (CommandBuffer.CC (CommandBuffer.M.C rppl cb)) (Pipeline.BindPoint pbp) (Pipeline.Compute.C ppl) = do
	ppl0 <- readIORef rppl
	when (ppl /= ppl0) do
		writeIORef rppl ppl
		C.bindPipeline cb pbp ppl

draw :: CommandBuffer.CC vs -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
draw (CommandBuffer.CC (CommandBuffer.M.C _ cb)) vc ic fv fi = C.draw cb vc ic fv fi

drawIndexed :: CommandBuffer.CC vs ->
	Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
drawIndexed (CommandBuffer.CC (CommandBuffer.M.C _ cb)) idxc istc fidx vo fist =
	C.drawIndexed cb idxc istc fidx vo fist

bindVertexBuffers ::
	CommandBuffer.CC vs -> Word32 -> [(Buffer.B, Device.Size)] -> IO ()
bindVertexBuffers (CommandBuffer.CC (CommandBuffer.M.C _ c))
	fb ((length &&& unzip) -> (bc, (bs, os))) = ($ pure) $ runContT do
	pb <- ContT $ allocaArray bc
	lift . pokeArray pb $ (\(Buffer.B b) -> b) <$> bs
	po <- ContT $ allocaArray bc
	lift . pokeArray po $ (\(Device.Size sz) -> sz) <$> os
	lift $ C.bindVertexBuffers c fb (fromIntegral bc) pb po

copyBuffer ::
	CommandBuffer.CC vs -> Buffer.B -> Buffer.B -> [Buffer.C.Copy] -> IO ()
copyBuffer (CommandBuffer.CC (CommandBuffer.M.C _ c)) (Buffer.B s) (Buffer.B d)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift do	pokeArray prs rs
		C.copyBuffer c s d (fromIntegral rc) prs

bindIndexBuffer ::
	CommandBuffer.CC vs -> Buffer.B -> Device.Size -> IndexType -> IO ()
bindIndexBuffer
	(CommandBuffer.CC (CommandBuffer.M.C _ cb)) (Buffer.B ib) (Device.Size sz) (IndexType it) =
	C.bindIndexBuffer cb ib sz it

dispatch ::
	CommandBuffer.CC vs -> Word32 -> Word32 -> Word32 -> IO ()
dispatch (CommandBuffer.CC (CommandBuffer.M.C _ cb)) = C.dispatch cb

bindDescriptorSets ::
	CommandBuffer.CC vs -> Pipeline.BindPoint -> Pipeline.Layout.L ->
	Word32 -> [Descriptor.Set.D] -> [Word32] -> IO ()
bindDescriptorSets
	(CommandBuffer.CC (CommandBuffer.M.C _ cb)) (Pipeline.BindPoint bp) (Pipeline.Layout.L lyt)
	fs (length &&& id -> (dsc, dss))
	(length &&& id -> (doc, dos)) = ($ pure) $ runContT do
	pdss <- ContT $ allocaArray dsc
	let	cdss = (\(Descriptor.Set.D s) -> s) <$> dss
	lift $ pokeArray pdss cdss
	pdos <- ContT $ allocaArray doc
	lift $ pokeArray pdos dos
	lift $ C.bindDescriptorSets
		cb bp lyt fs (fromIntegral dsc) pdss (fromIntegral doc) pdos

pushConstants :: forall vs ts .
	StoreHetero ts =>
	CommandBuffer.CC vs -> Pipeline.Layout.L ->
	ShaderStageFlags -> Word32 -> HeteroList ts -> IO ()
pushConstants (CommandBuffer.CC (CommandBuffer.M.C _ cb)) (Pipeline.Layout.L lyt)
	(ShaderStageFlagBits ss) ost xs = ($ pure) $ runContT do
	let	sz :: Integral n => n
		sz = fromIntegral $ storeHeteroSize @ts 0
	p <- ContT $ allocaBytes sz
	lift do	storeHetero p xs
		C.pushConstants cb lyt ss ost sz p

pipelineBarrier :: (
	PointableHeteroMap ns, PointableHeteroMap ns', PointableHeteroMap ns''
	) =>
	CommandBuffer.CC vs -> Pipeline.StageFlags -> Pipeline.StageFlags ->
	DependencyFlags ->
	HeteroVarList Memory.M.Barrier ns ->
	HeteroVarList Buffer.M.MemoryBarrier ns' ->
	HeteroVarList Image.MemoryBarrier ns'' -> IO ()
pipelineBarrier (CommandBuffer.CC (CommandBuffer.M.C _ cb))
	(Pipeline.StageFlagBits ssm) (Pipeline.StageFlagBits dsm)
	(DependencyFlagBits dfs)
	mbs bbs ibs = ($ pure) $ runContT do
	cmbs <- pointableHeteroMapM mbs Memory.M.barrierToCore
	let	mbc = length cmbs
	pmbs <- ContT $ allocaArray mbc
	lift $ pokeArray pmbs cmbs
	cbbs <- pointableHeteroMapM bbs Buffer.M.memoryBarrierToCore
	let	bbc = length cbbs
	pbbs <- ContT $ allocaArray bbc
	lift $ pokeArray pbbs cbbs
	cibs <- pointableHeteroMapM ibs Image.memoryBarrierToCore
	let	ibc = length cibs
	pibs <- ContT $ allocaArray ibc
	lift $ pokeArray pibs cibs
	lift $ C.pipelineBarrier cb ssm dsm dfs (fromIntegral mbc) pmbs
		(fromIntegral bbc) pbbs (fromIntegral ibc) pibs

copyBufferToImage ::
	CommandBuffer.CC vs -> Buffer.M.B -> Image.I -> Image.Layout ->
	[Buffer.M.ImageCopy] -> IO ()
copyBufferToImage (CommandBuffer.CC (CommandBuffer.M.C _ cb))
	(Buffer.M.B sb) (Image.I rdi) (Image.Layout dil)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift . pokeArray prs $ Buffer.M.imageCopyToCore <$> rs
	lift do	(_, di) <- readIORef rdi
		C.copyBufferToImage cb sb di dil (fromIntegral rc) prs

blitImage :: CommandBuffer.CC v ->
	Image.I -> Image.Layout -> Image.I -> Image.Layout ->
	[Image.Blit] -> Filter -> IO ()
blitImage (CommandBuffer.CC (CommandBuffer.M.C _ cb))
	(Image.I rsrc) (Image.Layout srcLyt) (Image.I rdst) (Image.Layout dstLyt)
	(length &&& id -> (bltc, blts)) (Filter ft) = ($ pure) $ runContT do
	pblts <- ContT $ allocaArray bltc
	lift . pokeArray pblts $ Image.blitToCore <$> blts
	lift do (_, src) <- readIORef rsrc
		(_, dst) <- readIORef rdst
		C.blitImage cb src srcLyt dst dstLyt (fromIntegral bltc) pblts ft
