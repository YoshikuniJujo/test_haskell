{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGe StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Command where

import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Control.Exception
import Data.Kind
import Data.Kind.Object
import Data.HeteroList hiding (length)
import Data.IORef
import Data.Word
import Data.Int
import TypeLevel.List hiding (length)

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Command.TypeLevel

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.CommandBuffer.Type as CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer.Middle as CommandBuffer.M
import qualified Gpu.Vulkan.Pipeline.Graphics.Type as Pipeline
import qualified Gpu.Vulkan.Pipeline.Compute as Pipeline.Compute
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.Command.Core as C
import qualified Gpu.Vulkan.Pipeline.Layout.Type as Pipeline.Layout
import qualified Gpu.Vulkan.DescriptorSet as DescriptorSet
import qualified Gpu.Vulkan.Memory.Middle as Memory.M
import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Image.Middle as Image
import qualified Gpu.Vulkan.Image.Enum as Image

import qualified Gpu.Vulkan.RenderPass.Type as RenderPass
import qualified Gpu.Vulkan.Subpass.Enum as Subpass
import qualified Gpu.Vulkan.Command.Middle as M

import Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList (MapSubType)

import qualified Gpu.Vulkan.PushConstant as PushConstant

beginRenderPass :: (Pointable n, ClearValuesToCore ct) =>
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

drawIndexedM :: CommandBuffer.M.C vs ->
	Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
drawIndexedM (CommandBuffer.M.C cb) idxc istc fidx vo fist =
	C.drawIndexed cb idxc istc fidx vo fist

drawIndexed :: CommandBuffer.C sc vs ->
	Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
drawIndexed (CommandBuffer.C cb) idxc istc fidx vo fist =
	drawIndexedM cb idxc istc fidx vo fist

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
	(Buffer.M.B sb) (Image.I rdi) (Image.Layout dil)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift . pokeArray prs $ Buffer.M.imageCopyToCore <$> rs
	lift do	di <- readIORef rdi
		C.copyBufferToImage cb sb di dil (fromIntegral rc) prs

blitImage :: CommandBuffer.M.C v ->
	Image.I -> Image.Layout -> Image.I -> Image.Layout ->
	[Image.Blit] -> Filter -> IO ()
blitImage (CommandBuffer.M.C cb)
	(Image.I rsrc) (Image.Layout srcLyt) (Image.I rdst) (Image.Layout dstLyt)
	(length &&& id -> (bltc, blts)) (Filter ft) = ($ pure) $ runContT do
	pblts <- ContT $ allocaArray bltc
	lift . pokeArray pblts $ Image.blitToCore <$> blts
	lift do src <- readIORef rsrc
		dst <- readIORef rdst
		C.blitImage cb src srcLyt dst dstLyt (fromIntegral bltc) pblts ft

dispatch :: CommandBuffer.C sc vs -> Word32 -> Word32 -> Word32 -> IO ()
dispatch (CommandBuffer.C cb) = M.dispatch cb

data DescriptorSet sd spslbts where
	DescriptorSet ::
		DescriptorSet.S sd sp slbts -> DescriptorSet sd '(sp, slbts)

deriving instance Show (DescriptorSet.S sd sp slbts) =>
	Show (DescriptorSet sd '(sp, slbts))

class HeteroVarListToList' (spslbtss :: [(Type, DescriptorSet.LayoutArg)]) where
	heteroVarListToList' :: (forall spslbts . t spslbts -> t') ->
		HeteroVarList t spslbtss -> [t']

instance HeteroVarListToList' '[] where heteroVarListToList' _ HVNil = []

instance HeteroVarListToList' spslbtss =>
	HeteroVarListToList' (spslbts ': spslbtss) where
	heteroVarListToList' f (x :...: xs) = f x : heteroVarListToList' f xs

bindDescriptorSets :: forall sc vs s sbtss sd spslbtss .
	(SetPos (MapSnd spslbtss) sbtss, HeteroVarListToList' spslbtss) =>
	CommandBuffer.C sc vs -> Pipeline.BindPoint ->
	Pipeline.Layout.LL s sbtss -> HeteroVarList (DescriptorSet sd) spslbtss ->
	[Word32] -> IO ()
bindDescriptorSets (CommandBuffer.C c) bp (Pipeline.Layout.LL l) dss dosts =
	M.bindDescriptorSets c bp l
		(firstSet' @spslbtss @sbtss)
		(heteroVarListToList'
			(\(DescriptorSet (DescriptorSet.S s)) -> s)
			dss)
		dosts

type family MapThird tpl where
	MapThird '[] = '[]
	MapThird ('(x, y, z) ': xs) = z ': MapThird xs

bindVertexBuffers :: forall sc vs smsbvs .
	InfixIndex (MapThird smsbvs) (MapSubType vs) =>
	CommandBuffer.C sc vs -> HeteroVarList (V3 Buffer.IndexedList) smsbvs ->
	IO ()
bindVertexBuffers (CommandBuffer.C cb) bils = M.bindVertexBuffers
	cb (fromIntegral fb) (Buffer.indexedListToMiddles bils)
	where fb = infixIndex @(MapThird smsbvs) @(MapSubType vs)

bindIndexBuffer :: forall sc vs sm sb v . IsIndexType v =>
	CommandBuffer.C sc vs -> Buffer.IndexedList sm sb v -> IO ()
bindIndexBuffer (CommandBuffer.C cb) ib =
	uncurry (M.bindIndexBuffer cb) (Buffer.indexedListToMiddle ib) (indexType @v)

class IsIndexType a where indexType :: IndexType

instance IsIndexType Word16 where indexType = IndexTypeUint16

copyBuffer :: forall (ass :: [[Object]]) sos sod sc vs sms sbs smd sbd .
	Buffer.MakeCopies ass sos sod =>
	CommandBuffer.C sc vs ->
	Buffer.Binded sms sbs sos -> Buffer.Binded smd sbd sod -> IO ()
copyBuffer (CommandBuffer.C cb) (Buffer.Binded lnss src) (Buffer.Binded lnsd dst) =
	M.copyBuffer cb (Buffer.M.B src) (Buffer.M.B dst) (Buffer.makeCopies @ass lnss lnsd)

pushConstants :: forall (ss :: [T.ShaderStageFlagBits]) sc vs s sbtss whole ts . (
	StoreHetero ts,
	PushConstant.ShaderStageFlagBitsToMiddle ss,
	PushConstant.OffsetSize whole ts ) =>
	CommandBuffer.C sc vs -> Pipeline.Layout.LLL s sbtss whole ->
	HeteroList ts -> IO ()
pushConstants (CommandBuffer.C cb) (Pipeline.Layout.LLL lyt) xs =
	M.pushConstants cb lyt (PushConstant.shaderStageFlagBitsToMiddle @ss)
		(PushConstant.offset @whole @ts 0) xs
