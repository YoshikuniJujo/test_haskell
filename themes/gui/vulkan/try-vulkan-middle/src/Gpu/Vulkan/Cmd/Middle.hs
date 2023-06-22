{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Cmd.Middle (

	-- * BEGIN AND END RENDER PASS

	beginRenderPass, endRenderPass,

	-- * DRAW AND DISPATCH

	-- ** Draw

	bindPipelineGraphics, bindVertexBuffers, bindIndexBuffer, draw, drawIndexed,

	-- ** Dispatch

	bindPipelineCompute, dispatch,

	-- * PUSH CONSTANTS AND BIND DESCRIPTOR SETS

	pushConstants, bindDescriptorSets,

	-- * COPY BUFFERS AND IMAGES

	copyBuffer, copyBufferToImage, copyImageToBuffer, blitImage,

	-- * MEMORY DEPENDENCY

	pipelineBarrier,

	-- * QUERY

	resetQueryPool, beginQuery, endQuery, writeTimestamp

	) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke (WithPoked)
import Foreign.Storable.HeteroList
import Control.Arrow
import Control.Monad.Cont
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.HeteroParList qualified as HeteroParList
import Data.Word
import Data.Int
import Data.IORef

import Gpu.Vulkan.Middle.Internal
import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.CommandBuffer.Middle.Internal as CommandBuffer.M
import qualified Gpu.Vulkan.Buffer.Middle.Internal as Buffer
import qualified Gpu.Vulkan.Buffer.Core as Buffer.C
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Cmd.Core as C

import qualified Gpu.Vulkan.RenderPass.Middle.Internal as RenderPass
import qualified Gpu.Vulkan.Subpass.Enum as Subpass
import qualified Gpu.Vulkan.Pipeline.Graphics.Middle.Internal as Pipeline
import qualified Gpu.Vulkan.Pipeline.Compute.Middle.Internal as Pipeline.Compute
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.PipelineLayout.Middle.Internal as Pipeline.Layout

import qualified Gpu.Vulkan.DescriptorSet.Middle.Internal as Descriptor.Set

import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.Image.Middle.Internal as Image
import qualified Gpu.Vulkan.Buffer.Middle.Internal as Buffer.M
import qualified Gpu.Vulkan.Memory.Middle.Internal as Memory.M

import Gpu.Vulkan.Query.Enum qualified as Query
import Gpu.Vulkan.QueryPool.Middle.Internal qualified as QueryPool

beginRenderPass :: (WithPoked (TMaybe.M mn), ClearValueListToCore cts) => CommandBuffer.M.C ->
	RenderPass.BeginInfo mn cts -> Subpass.Contents -> IO ()
beginRenderPass (CommandBuffer.M.C _ cb) rpbi (Subpass.Contents spcnt) =
	RenderPass.beginInfoToCore rpbi \prpbi ->
		C.beginRenderPass cb prpbi spcnt

endRenderPass :: CommandBuffer.M.C -> IO ()
endRenderPass (CommandBuffer.M.C _ cb) = C.endRenderPass cb

bindPipelineGraphics ::
	CommandBuffer.M.C -> Pipeline.BindPoint -> Pipeline.G -> IO ()
bindPipelineGraphics (CommandBuffer.M.C rppl cb) (Pipeline.BindPoint pbp) ppl = do
	ppl0 <- readIORef rppl
	ppl' <- Pipeline.gToCore ppl
	when (ppl' /= ppl0) do
		writeIORef rppl ppl'
		C.bindPipeline cb pbp ppl'

bindPipelineCompute ::
	CommandBuffer.M.C -> Pipeline.BindPoint -> Pipeline.Compute.C -> IO ()
bindPipelineCompute (CommandBuffer.M.C rppl cb) (Pipeline.BindPoint pbp) (Pipeline.Compute.C ppl) = do
	ppl0 <- readIORef rppl
	when (ppl /= ppl0) do
		writeIORef rppl ppl
		C.bindPipeline cb pbp ppl

bindVertexBuffers ::
	CommandBuffer.M.C -> Word32 -> [(Buffer.B, Device.Size)] -> IO ()
bindVertexBuffers (CommandBuffer.M.C _ c)
	fb ((length &&& unzip) -> (bc, (bs, os))) = ($ pure) $ runContT do
	pb <- ContT $ allocaArray bc
	lift . pokeArray pb $ (\(Buffer.B b) -> b) <$> bs
	po <- ContT $ allocaArray bc
	lift . pokeArray po $ (\(Device.Size sz) -> sz) <$> os
	lift $ C.bindVertexBuffers c fb (fromIntegral bc) pb po

bindIndexBuffer ::
	CommandBuffer.M.C -> Buffer.B -> Device.Size -> IndexType -> IO ()
bindIndexBuffer
	(CommandBuffer.M.C _ cb) (Buffer.B ib) (Device.Size sz) (IndexType it) =
	C.bindIndexBuffer cb ib sz it

pushConstants :: forall as . PokableList as =>
	CommandBuffer.M.C -> Pipeline.Layout.L ->
	ShaderStageFlags -> Word32 -> HeteroParList.L as -> IO ()
pushConstants (CommandBuffer.M.C _ cb) (Pipeline.Layout.L lyt)
	(ShaderStageFlagBits ss) ost xs = ($ pure) $ runContT do
	let	sz :: Integral n => n
		sz = fromIntegral $ wholeSize @as
	p <- ContT $ allocaBytes sz
	lift do	pokeList p xs
		C.pushConstants cb lyt ss ost sz p

bindDescriptorSets ::
	CommandBuffer.M.C -> Pipeline.BindPoint -> Pipeline.Layout.L ->
	Word32 -> [Descriptor.Set.D] -> [Word32] -> IO ()
bindDescriptorSets
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

draw :: CommandBuffer.M.C -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
draw (CommandBuffer.M.C _ cb) vc ic fv fi = C.draw cb vc ic fv fi

drawIndexed :: CommandBuffer.M.C ->
	Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
drawIndexed (CommandBuffer.M.C _ cb) idxc istc fidx vo fist =
	C.drawIndexed cb idxc istc fidx vo fist

dispatch :: CommandBuffer.M.C -> Word32 -> Word32 -> Word32 -> IO ()
dispatch (CommandBuffer.M.C _ cb) = C.dispatch cb

copyBuffer ::
	CommandBuffer.M.C -> Buffer.B -> Buffer.B -> [Buffer.C.Copy] -> IO ()
copyBuffer (CommandBuffer.M.C _ c) (Buffer.B s) (Buffer.B d)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift do	pokeArray prs rs
		C.copyBuffer c s d (fromIntegral rc) prs

copyBufferToImage ::
	CommandBuffer.M.C -> Buffer.M.B -> Image.I -> Image.Layout ->
	[Buffer.M.ImageCopy] -> IO ()
copyBufferToImage (CommandBuffer.M.C _ cb)
	(Buffer.M.B sb) (Image.I rdi) (Image.Layout dil)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift . pokeArray prs $ Buffer.M.imageCopyToCore <$> rs
	lift do	(_, di) <- readIORef rdi
		C.copyBufferToImage cb sb di dil (fromIntegral rc) prs

copyImageToBuffer ::
	CommandBuffer.M.C -> Image.I -> Image.Layout -> Buffer.M.B ->
	[Buffer.M.ImageCopy] -> IO ()
copyImageToBuffer (CommandBuffer.M.C _ cb)
	(Image.I rsi) (Image.Layout sil) (Buffer.M.B db)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift . pokeArray prs $ Buffer.M.imageCopyToCore <$> rs
	lift do	(_, si) <- readIORef rsi
		C.copyImageToBuffer cb si sil db (fromIntegral rc) prs

pipelineBarrier :: (
	HeteroParList.ToListWithCCpsM' WithPoked TMaybe.M ns,
	HeteroParList.ToListWithCCpsM' WithPoked TMaybe.M ns',
	HeteroParList.ToListWithCCpsM' WithPoked TMaybe.M ns'' ) =>
	CommandBuffer.M.C -> Pipeline.StageFlags -> Pipeline.StageFlags ->
	DependencyFlags ->
	HeteroParList.PL Memory.M.Barrier ns ->
	HeteroParList.PL Buffer.M.MemoryBarrier ns' ->
	HeteroParList.PL Image.MemoryBarrier ns'' -> IO ()
pipelineBarrier (CommandBuffer.M.C _ cb)
	(Pipeline.StageFlagBits ssm) (Pipeline.StageFlagBits dsm)
	(DependencyFlagBits dfs)
	mbs bbs ibs =
		HeteroParList.withListWithCCpsM' @_ @WithPoked @TMaybe.M mbs Memory.M.barrierToCore \cmbs ->
		let	mbc = length cmbs in
		allocaArray mbc \pmbs ->
		pokeArray pmbs cmbs >>
		HeteroParList.withListWithCCpsM' @_ @WithPoked @TMaybe.M bbs Buffer.M.memoryBarrierToCore' \cbbs ->
		let	bbc = length cbbs in
		allocaArray bbc \pbbs ->
		pokeArray pbbs cbbs >>
		HeteroParList.withListWithCCpsM' @_ @WithPoked @TMaybe.M ibs Image.memoryBarrierToCore \cibs ->
		let	ibc = length cibs in
		allocaArray ibc \pibs ->
		pokeArray pibs cibs >>

		C.pipelineBarrier cb ssm dsm dfs (fromIntegral mbc) pmbs
			(fromIntegral bbc) pbbs (fromIntegral ibc) pibs

blitImage :: CommandBuffer.M.C ->
	Image.I -> Image.Layout -> Image.I -> Image.Layout ->
	[Image.Blit] -> Filter -> IO ()
blitImage (CommandBuffer.M.C _ cb)
	(Image.I rsrc) (Image.Layout srcLyt) (Image.I rdst) (Image.Layout dstLyt)
	(length &&& id -> (bltc, blts)) (Filter ft) = ($ pure) $ runContT do
	pblts <- ContT $ allocaArray bltc
	lift . pokeArray pblts $ Image.blitToCore <$> blts
	lift do (_, src) <- readIORef rsrc
		(_, dst) <- readIORef rdst
		C.blitImage cb src srcLyt dst dstLyt (fromIntegral bltc) pblts ft

resetQueryPool :: CommandBuffer.M.C -> QueryPool.Q -> Word32 -> Word32 -> IO ()
resetQueryPool (CommandBuffer.M.C _ c) (QueryPool.Q q) fq qc =
	C.resetQueryPool c q fq qc

beginQuery :: CommandBuffer.M.C ->
	QueryPool.Q -> Word32 -> Query.ControlFlags -> IO ()
beginQuery (CommandBuffer.M.C _ c) (QueryPool.Q q) i (Query.ControlFlagBits flgs) =
	C.beginQuery c q i flgs

endQuery :: CommandBuffer.M.C -> QueryPool.Q -> Word32 -> IO ()
endQuery (CommandBuffer.M.C _ c) (QueryPool.Q q) = C.endQuery c q

writeTimestamp :: CommandBuffer.M.C -> Pipeline.StageFlagBits ->
	QueryPool.Q -> Word32 -> IO ()
writeTimestamp
	(CommandBuffer.M.C _ c) (Pipeline.StageFlagBits fls) (QueryPool.Q q) i =
	C.writeTimestamp c fls q i
