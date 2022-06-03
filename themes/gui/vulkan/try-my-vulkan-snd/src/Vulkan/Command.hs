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

import Vulkan.Middle
import Vulkan.Enum

import qualified Vulkan.CommandBuffer.Middle as CommandBuffer
import qualified Vulkan.RenderPass.Middle as RenderPass
import qualified Vulkan.Subpass.Enum as Subpass
import qualified Vulkan.Pipeline.Graphics as Pipeline
import qualified Vulkan.Pipeline.Enum as Pipeline
import qualified Vulkan.Command.Core as C
import qualified Vulkan.Pipeline.Layout as Pipeline.Layout
import qualified Vulkan.Descriptor.Set as Descriptor.Set
import qualified Vulkan.Memory.Middle as Memory.M
import qualified Vulkan.Buffer.Middle as Buffer.M
import qualified Vulkan.Image.Middle as Image
import qualified Vulkan.Image.Enum as Image

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

pipelineBarrier ::
	(Pointable n, Pointable n', Pointable n'') =>
	CommandBuffer.C vs -> Pipeline.StageFlags -> Pipeline.StageFlags ->
	DependencyFlags ->
	[Memory.M.Barrier n] -> [Buffer.M.MemoryBarrier n'] ->
	[Image.MemoryBarrier n''] -> IO ()
pipelineBarrier (CommandBuffer.C cb)
	(Pipeline.StageFlagBits ssm) (Pipeline.StageFlagBits dsm)
	(DependencyFlagBits dfs)
	(length &&& id -> (mbc, mbs))
	(length &&& id -> (bbc, bbs))
	(length &&& id -> (ibc, ibs)) = ($ pure) $ runContT do
	cmbs <- Memory.M.barrierToCore `mapM` mbs
	pmbs <- ContT $ allocaArray mbc
	lift $ pokeArray pmbs cmbs
	cbbs <- Buffer.M.memoryBarrierToCore `mapM` bbs
	pbbs <- ContT $ allocaArray bbc
	lift $ pokeArray pbbs cbbs
	cibs <- Image.memoryBarrierToCore `mapM` ibs
	pibs <- ContT $ allocaArray ibc
	lift $ pokeArray pibs cibs
	lift $ C.pipelineBarrier cb ssm dsm dfs (fromIntegral mbc) pmbs
		(fromIntegral bbc) pbbs (fromIntegral ibc) pibs

copyBufferToImage ::
	CommandBuffer.C vs -> Buffer.M.B -> Image.I -> Image.Layout ->
	[Buffer.M.ImageCopy] -> IO ()
copyBufferToImage (CommandBuffer.C cb)
	(Buffer.M.B sb) (Image.I di) (Image.Layout dil)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift . pokeArray prs $ Buffer.M.imageCopyToCore <$> rs
	lift $ C.copyBufferToImage cb sb di dil (fromIntegral rc) prs

blitImage :: CommandBuffer.C v ->
	Image.I -> Image.Layout -> Image.I -> Image.Layout ->
	[Image.Blit] -> Filter -> IO ()
blitImage (CommandBuffer.C cb)
	(Image.I src) (Image.Layout srcLyt) (Image.I dst) (Image.Layout dstLyt)
	(length &&& id -> (bltc, blts)) (Filter ft) = ($ pure) $ runContT do
	pblts <- ContT $ allocaArray bltc
	lift . pokeArray pblts $ Image.blitToCore <$> blts
	lift $ C.blitImage cb src srcLyt dst dstLyt (fromIntegral bltc) pblts ft
