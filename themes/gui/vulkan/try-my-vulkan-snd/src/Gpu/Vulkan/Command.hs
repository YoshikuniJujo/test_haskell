{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Command where

import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Control.Exception
import Data.Word
import Data.Int

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.CommandBuffer.Type as CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer.Middle as CommandBuffer.M
import qualified Gpu.Vulkan.Pipeline.Graphics.Type as Pipeline
import qualified Gpu.Vulkan.Pipeline.Compute as Pipeline.Compute
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.Command.Core as C
import qualified Gpu.Vulkan.Pipeline.Layout.Middle as Pipeline.Layout
import qualified Gpu.Vulkan.DescriptorSet.Middle as Descriptor.Set
import qualified Gpu.Vulkan.Memory.Middle as Memory.M
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Image.Middle as Image
import qualified Gpu.Vulkan.Image.Enum as Image

import qualified Gpu.Vulkan.RenderPass.Type as RenderPass
import qualified Gpu.Vulkan.Subpass.Enum as Subpass
import qualified Gpu.Vulkan.Command.Middle as M

beginRenderPass :: (Pointable n, ClearValueToCore ct) =>
	CommandBuffer.C sc vs -> RenderPass.BeginInfo n sr sf ct -> Subpass.Contents ->
	IO a -> IO a
beginRenderPass (CommandBuffer.C cb) bi cnt f = bracket_
	(M.beginRenderPass cb (RenderPass.beginInfoToMiddle bi) cnt)
	(M.endRenderPass cb) f

bindPipeline ::
	CommandBuffer.C sc vs -> Pipeline.BindPoint -> Pipeline.G sg vs ts -> IO ()
bindPipeline (CommandBuffer.C cb) bp (Pipeline.G g) = M.bindPipeline cb bp g

bindPipelineCompute ::
	CommandBuffer.C sc vs -> Pipeline.BindPoint -> Pipeline.Compute.C sg -> IO ()
bindPipelineCompute (CommandBuffer.C cb) bp (Pipeline.Compute.C g) = M.bindPipelineCompute cb bp g

draw :: CommandBuffer.C sc vs -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
draw (CommandBuffer.C cb) vc ic fv fi = M.draw cb vc ic fv fi

drawIndexed :: CommandBuffer.M.C vs ->
	Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
drawIndexed (CommandBuffer.M.C cb) idxc istc fidx vo fist =
	C.drawIndexed cb idxc istc fidx vo fist

bindDescriptorSets ::
	CommandBuffer.M.C vs -> Pipeline.BindPoint -> Pipeline.Layout.L ->
	Word32 -> [Descriptor.Set.S] -> [Word32] -> IO ()
bindDescriptorSets
	(CommandBuffer.M.C cb) (Pipeline.BindPoint bp) (Pipeline.Layout.L lyt)
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
	CommandBuffer.M.C vs -> Pipeline.StageFlags -> Pipeline.StageFlags ->
	DependencyFlags ->
	[Memory.M.Barrier n] -> [Buffer.M.MemoryBarrier n'] ->
	[Image.MemoryBarrier n''] -> IO ()
pipelineBarrier (CommandBuffer.M.C cb)
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
	CommandBuffer.M.C vs -> Buffer.M.B -> Image.I -> Image.Layout ->
	[Buffer.M.ImageCopy] -> IO ()
copyBufferToImage (CommandBuffer.M.C cb)
	(Buffer.M.B sb) (Image.I di) (Image.Layout dil)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift . pokeArray prs $ Buffer.M.imageCopyToCore <$> rs
	lift $ C.copyBufferToImage cb sb di dil (fromIntegral rc) prs

blitImage :: CommandBuffer.M.C v ->
	Image.I -> Image.Layout -> Image.I -> Image.Layout ->
	[Image.Blit] -> Filter -> IO ()
blitImage (CommandBuffer.M.C cb)
	(Image.I src) (Image.Layout srcLyt) (Image.I dst) (Image.Layout dstLyt)
	(length &&& id -> (bltc, blts)) (Filter ft) = ($ pure) $ runContT do
	pblts <- ContT $ allocaArray bltc
	lift . pokeArray pblts $ Image.blitToCore <$> blts
	lift $ C.blitImage cb src srcLyt dst dstLyt (fromIntegral bltc) pblts ft

dispatch :: CommandBuffer.C sc vs -> Word32 -> Word32 -> Word32 -> IO ()
dispatch (CommandBuffer.C cb) = M.dispatch cb
