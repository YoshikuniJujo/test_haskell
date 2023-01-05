{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGe StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Command where

import GHC.TypeLits
import Foreign.Pointable
import Control.Exception
import Data.Kind
import Data.Kind.Object
import Data.HeteroList hiding (length)
import Data.Word
import Data.Int
import TypeLevel.List hiding (length)

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Command.TypeLevel

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.CommandBuffer.Type as CommandBuffer
import qualified Gpu.Vulkan.Pipeline.Graphics.Type as Pipeline
import qualified Gpu.Vulkan.Pipeline.Compute as Pipeline.Compute
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.Pipeline.Layout.Type as Pipeline.Layout
import qualified Gpu.Vulkan.DescriptorSet as DescriptorSet
import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Image as Image
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M

import qualified Gpu.Vulkan.RenderPass.Type as RenderPass
import qualified Gpu.Vulkan.Subpass.Enum as Subpass
import qualified Gpu.Vulkan.Command.Middle as M

import Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList (MapSubType)

import qualified Gpu.Vulkan.PushConstant as PushConstant
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

beginRenderPassNew :: (Pointable n, ClearValuesToCore ct) =>
	CommandBuffer.C sc vs -> RenderPass.BeginInfoNew n sr fmt sf ct ->
	Subpass.Contents -> IO a -> IO a
beginRenderPassNew (CommandBuffer.C cb) bi cnt f = bracket_
	(M.beginRenderPass cb (RenderPass.beginInfoToMiddleNew bi) cnt)
	(M.endRenderPass cb) f

beginRenderPass :: (Pointable n, ClearValuesToCore ct) =>
	CommandBuffer.C sc vs -> RenderPass.BeginInfo n sr sf ct ->
	Subpass.Contents -> IO a -> IO a
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

drawIndexed :: CommandBuffer.C sc vs ->
	Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
drawIndexed (CommandBuffer.C cb) idxc istc fidx vo fist =
	M.drawIndexed cb idxc istc fidx vo fist

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

bindDescriptorSetsNew :: forall sc vs s sbtss foo sd spslbtss .
	(SetPos (MapSnd spslbtss) sbtss, HeteroVarListToList' spslbtss) =>
	CommandBuffer.C sc vs -> Pipeline.BindPoint ->
	Pipeline.Layout.L s sbtss foo -> HeteroVarList (DescriptorSet sd) spslbtss ->
	[Word32] -> IO ()
bindDescriptorSetsNew (CommandBuffer.C c) bp (Pipeline.Layout.L l) dss dosts =
	M.bindDescriptorSets c bp l
		(firstSet' @spslbtss @sbtss)
		(heteroVarListToList'
			(\(DescriptorSet (DescriptorSet.S s)) -> s)
			dss)
		dosts

type family MapThird tpl where
	MapThird '[] = '[]
	MapThird ('(x, y, z) ': xs) = z ': MapThird xs

type family MapForth tpl where
	MapForth '[] = '[]
	MapForth ('(x :: j, y :: k, z :: l, w :: m) ': xs) = w ': MapForth xs

bindVertexBuffers :: forall sc vs smsbvs .
	InfixIndex (MapForth smsbvs) (MapSubType vs) =>
	CommandBuffer.C sc vs -> HeteroVarList (V4 Buffer.IndexedList) smsbvs ->
	IO ()
bindVertexBuffers (CommandBuffer.C cb) bils = M.bindVertexBuffers
	cb (fromIntegral fb) (Buffer.indexedListToMiddles bils)
	where fb = infixIndex @(MapForth smsbvs) @(MapSubType vs)

bindIndexBuffer :: forall sc vs sm sb nm v . IsIndexType v =>
	CommandBuffer.C sc vs -> Buffer.IndexedList sm sb nm v -> IO ()
bindIndexBuffer (CommandBuffer.C cb) ib =
	uncurry (M.bindIndexBuffer cb) (Buffer.indexedListToMiddle ib) (indexType @v)

class IsIndexType a where indexType :: IndexType

instance IsIndexType Word16 where indexType = IndexTypeUint16
instance IsIndexType Word32 where indexType = IndexTypeUint32

copyBuffer :: forall (ass :: [[Object]]) nms nmd sos sod sc vs sms sbs smd sbd .
	Buffer.MakeCopies ass sos sod =>
	CommandBuffer.C sc vs ->
	Buffer.Binded sms sbs nms sos -> Buffer.Binded smd sbd nmd sod -> IO ()
copyBuffer (CommandBuffer.C cb) (Buffer.Binded lnss src) (Buffer.Binded lnsd dst) =
	M.copyBuffer cb src dst (Buffer.makeCopies @ass lnss lnsd)

pushConstants :: forall (ss :: [T.ShaderStageFlagBits]) sc vs s sbtss whole ts . (
	StoreHetero ts,
	PushConstant.ShaderStageFlagBitsToMiddle ss,
	PushConstant.OffsetSize whole ts ) =>
	CommandBuffer.C sc vs -> Pipeline.Layout.L s sbtss whole ->
	HeteroList ts -> IO ()
pushConstants (CommandBuffer.C cb) (Pipeline.Layout.L lyt) xs =
	M.pushConstants cb lyt (PushConstant.shaderStageFlagBitsToMiddle @ss)
		(PushConstant.offset @whole @ts 0) xs

pipelineBarrier :: (
	StorableHeteroMap ns,
	StorableHeteroMap (Buffer.FirstOfFives nsmsbnmobjs),
	StorableHeteroMap (Image.FirstOfFives nsismnmfmts),
	Buffer.MemoryBarrierListToMiddle nsmsbnmobjs,
	Image.MemoryBarrierListToMiddle nsismnmfmts ) =>
	CommandBuffer.C sc vs -> Pipeline.StageFlags -> Pipeline.StageFlags ->
	DependencyFlags -> HeteroVarList Memory.M.Barrier ns ->
	HeteroVarList (V5 Buffer.MemoryBarrier) nsmsbnmobjs ->
	HeteroVarList (V5 Image.MemoryBarrier) nsismnmfmts -> IO ()
pipelineBarrier (CommandBuffer.C cb) ssm dsm dfs mbs bmbs imbs =
	M.pipelineBarrier cb ssm dsm dfs mbs
		(Buffer.memoryBarrierListToMiddle bmbs)
		(Image.memoryBarrierListToMiddle imbs)

copyBufferToImage :: (
	ImageCopyListToMiddle objs img inms ) =>
	CommandBuffer.C sc vs -> Buffer.Binded sm sb nm objs ->
	Image.BindedNew si sm' nm' (Buffer.ImageFormat img) -> Image.Layout ->
	HeteroVarList (Buffer.ImageCopy img) (inms :: [Symbol]) -> IO ()
copyBufferToImage (CommandBuffer.C cb)
	bf@(Buffer.Binded _ mbf) (Image.BindedNew mim) imlyt ics =
	M.copyBufferToImage cb mbf mim imlyt mics
	where mics = imageCopyListToMiddle bf ics

copyImageToBuffer :: (
	ImageCopyListToMiddle objs img inms ) =>
	CommandBuffer.C sc vs ->
	Image.BindedNew si sm' nm' (Buffer.ImageFormat img) -> Image.Layout ->
	Buffer.Binded sm sb nm objs ->
	HeteroVarList (Buffer.ImageCopy img) (inms :: [Symbol]) -> IO ()
copyImageToBuffer (CommandBuffer.C cb)
	(Image.BindedNew mim) imlyt bf@(Buffer.Binded _ mbf) ics =
	M.copyImageToBuffer cb mim imlyt mbf mics
	where mics = imageCopyListToMiddle bf ics

class ImageCopyListToMiddle objs (img :: Type) (inms :: [Symbol]) where
	imageCopyListToMiddle ::
		Buffer.Binded sm sb nm objs ->
		HeteroVarList (Buffer.ImageCopy img) inms ->
		[Buffer.M.ImageCopy]

instance ImageCopyListToMiddle objs img '[] where
	imageCopyListToMiddle _ HVNil = []

instance (
	Buffer.OffsetSize ('ObjImage img nm) objs,
	ImageCopyListToMiddle objs img nms ) =>
	ImageCopyListToMiddle objs img (nm ': nms) where
	imageCopyListToMiddle bf (ic :...: ics) =
		Buffer.imageCopyToMiddle @_ @nm bf (ic :: Buffer.ImageCopy img nm) :
		imageCopyListToMiddle bf ics

blitImage :: CommandBuffer.C sc vs ->
	Image.BindedNew ssi ssm snm sfmt -> Image.Layout ->
	Image.BindedNew dsi dsm dnm dfmt -> Image.Layout ->
	[Image.M.Blit] -> Filter -> IO ()
blitImage (CommandBuffer.C cb)
	(Image.BindedNew src) slyt (Image.BindedNew dst) dlyt blts fltr =
	M.blitImage cb src slyt dst dlyt blts fltr
