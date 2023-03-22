{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGe StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandNew where

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Foreign.Storable.HeteroList
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word
import Data.Int
import TypeLevel.List

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
import qualified Gpu.Vulkan.DescriptorSet.TypeLevel as DescriptorSet
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DescriptorSetLayout
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

import Data.IORef -- for debug
import Data.Kind.Object qualified as KObj

beginRenderPassNew :: (WithPoked n, ClearValueListToCore ct) =>
	CommandBuffer.Binded sc vs -> RenderPass.BeginInfoNew n sr fmt sf ct ->
	Subpass.Contents -> IO a -> IO a
beginRenderPassNew (CommandBuffer.Binded cb) bi cnt f = bracket_
	(M.beginRenderPass cb (RenderPass.beginInfoToMiddleNew bi) cnt)
	(M.endRenderPass cb) f

beginRenderPass :: (WithPoked n, ClearValueListToCore ct) =>
	CommandBuffer.Binded sc vs -> RenderPass.BeginInfo n sr sf ct ->
	Subpass.Contents -> IO a -> IO a
beginRenderPass (CommandBuffer.Binded cb) bi cnt f = bracket_
	(M.beginRenderPass cb (RenderPass.beginInfoToMiddle bi) cnt)
	(M.endRenderPass cb) f

bindPipeline :: CommandBuffer.Binded sc vs ->
	Pipeline.BindPoint -> Pipeline.G sg vs ts -> IO ()
bindPipeline (CommandBuffer.Binded cb) bp (Pipeline.G g) = M.bindPipeline cb bp g

bindPipelineNew :: CommandBuffer.C sc ->
	Pipeline.BindPoint -> Pipeline.GNew sg vs ts slbtss ->
	(forall sb . CommandBuffer.GBinded sb vs slbtss -> IO a) -> IO a
bindPipelineNew (CommandBuffer.C c) bp (Pipeline.GNew g) f =
	M.bindPipeline c bp g >> f (CommandBuffer.GBinded c)

bindPipelineCompute ::
	CommandBuffer.Binded sc vs -> Pipeline.BindPoint -> Pipeline.Compute.C sg -> IO ()
bindPipelineCompute (CommandBuffer.Binded cb) bp (Pipeline.Compute.C g) = M.bindPipelineCompute cb bp g

draw :: CommandBuffer.GBinded sc vs slbtss -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
draw (CommandBuffer.GBinded cb) vc ic fv fi = M.draw cb vc ic fv fi

drawIndexed :: CommandBuffer.GBinded sc vs slbtss ->
	Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
drawIndexed (CommandBuffer.GBinded cb) idxc istc fidx vo fist =
	M.drawIndexed cb idxc istc fidx vo fist

dispatch :: CommandBuffer.Binded sc vs -> Word32 -> Word32 -> Word32 -> IO ()
dispatch (CommandBuffer.Binded cb) = M.dispatch cb

class HeteroParListToList' (spslbtss :: [(Type, DescriptorSet.LayoutArg)]) where
	toList' :: (forall spslbts . t spslbts -> t') ->
		HeteroParList.PL t spslbtss -> [t']

instance HeteroParListToList' '[] where toList' _ HeteroParList.Nil = []

instance HeteroParListToList' spslbtss =>
	HeteroParListToList' (spslbts ': spslbtss) where
	toList' f (x :** xs) = f x : toList' f xs

bindDescriptorSets :: forall sc vs s sbtss foo sd spslbtss . (
	SetPos (MapSnd spslbtss) sbtss, HeteroParListToList' spslbtss ) =>
	CommandBuffer.GBinded sc vs '(s, sbtss, foo) -> Pipeline.BindPoint ->
	Pipeline.Layout.L s sbtss foo -> HeteroParList.PL (U2 (DescriptorSet.S sd)) spslbtss ->
	[Word32] -> IO ()
bindDescriptorSets (CommandBuffer.GBinded c) bp (Pipeline.Layout.L l) dss dosts =
	M.bindDescriptorSets c bp l
		(firstSet' @spslbtss @sbtss)
		(toList'
			(\(U2 (DescriptorSet.S _ s)) -> s)
			dss)
		dosts

bindDescriptorSetsNew :: forall sc vs s sbtss foo sd spslbtss . (
	DynamicOffsetList3ToList (DescriptorSet.LayoutArgListOnlyDynamics sbtss),
	Show (HeteroParList.PL3
		DynamicOffset (DescriptorSet.LayoutArgListOnlyDynamics sbtss)),
	GetOffsetList3 (DescriptorSet.LayoutArgListOnlyDynamics sbtss),
	GetDscSetListLengthSnds spslbtss ~ sbtss,
	Show (HeteroParList.PL3 DynamicIndex
		(DescriptorSet.LayoutArgListOnlyDynamics sbtss)),
	GetDscSetListLength spslbtss,
	Show (HeteroParList.PL3 KObj.ObjectLength
		(DescriptorSet.LayoutArgListOnlyDynamics
			(GetDscSetListLengthSnds spslbtss))),
	SetPos (MapSnd spslbtss) sbtss, HeteroParListToList' spslbtss ) =>
	CommandBuffer.Binded sc vs -> Pipeline.BindPoint ->
	Pipeline.Layout.L s sbtss foo -> HeteroParList.PL (U2 (DescriptorSet.S sd)) spslbtss ->
	HeteroParList.PL3 DynamicIndex (DescriptorSet.LayoutArgListOnlyDynamics sbtss) ->
	IO ()
bindDescriptorSetsNew (CommandBuffer.Binded c) bp (Pipeline.Layout.L l) dss idxs = do
	putStrLn "bindDescriptorSets:"
	lns <- getDscSetListLength dss
	print lns
	print idxs
	print $ getOffsetList3 lns idxs
	print . dynamicOffsetList3ToList $ getOffsetList3 lns idxs
	let	dosts = dynamicOffsetList3ToList $ getOffsetList3 lns idxs
	M.bindDescriptorSets c bp l
		(firstSet' @spslbtss @sbtss)
		(toList'
			(\(U2 (DescriptorSet.S _ s)) -> s)
			dss)
		dosts

newtype DynamicIndex (obj :: KObj.Object) = DynamicIndex Word32 deriving Show
newtype DynamicOffset (obj :: KObj.Object) = DynamicOffset Word32 deriving Show

getOffset :: forall obj . KObj.SizeAlignment obj =>
	KObj.ObjectLength obj -> DynamicIndex obj -> DynamicOffset obj
getOffset ln (DynamicIndex i) = DynamicOffset $ fromIntegral sz * i
	where
	sz = ((KObj.objectSize ln - 1) `div` algn + 1) * algn
	algn = KObj.objectAlignment @obj

class GetOffsetList os where
	getOffsetList ::
		HeteroParList.PL KObj.ObjectLength os ->
		HeteroParList.PL DynamicIndex os ->
		HeteroParList.PL DynamicOffset os

instance GetOffsetList '[] where
	getOffsetList HeteroParList.Nil HeteroParList.Nil = HeteroParList.Nil

instance (KObj.SizeAlignment o, GetOffsetList os) =>
	GetOffsetList (o ': os) where
	getOffsetList (ln :** lns) (i :** is) =
		getOffset ln i :** getOffsetList lns is

class GetOffsetList2 oss where
	getOffsetList2 ::
		HeteroParList.PL2 KObj.ObjectLength oss ->
		HeteroParList.PL2 DynamicIndex oss ->
		HeteroParList.PL2 DynamicOffset oss

instance GetOffsetList2 '[] where
	getOffsetList2 HeteroParList.Nil HeteroParList.Nil = HeteroParList.Nil

instance (GetOffsetList os, GetOffsetList2 oss) =>
	GetOffsetList2 (os ': oss) where
	getOffsetList2 (lns :** lnss) (is :** iss) =
		getOffsetList lns is :** getOffsetList2 lnss iss

class GetOffsetList3 (osss :: [[[KObj.Object]]]) where
	getOffsetList3 ::
		HeteroParList.PL3 KObj.ObjectLength osss ->
		HeteroParList.PL3 DynamicIndex osss ->
		HeteroParList.PL3 DynamicOffset osss

instance GetOffsetList3 '[] where
	getOffsetList3 HeteroParList.Nil HeteroParList.Nil = HeteroParList.Nil

instance (GetOffsetList2 oss, GetOffsetList3 osss) =>
	GetOffsetList3 (oss ': osss) where
	getOffsetList3 (lnss :** lnsss) (iss :** isss) =
		getOffsetList2 lnss iss :** getOffsetList3 lnsss isss

class DynamicOffsetList3ToList osss where
	dynamicOffsetList3ToList ::
		HeteroParList.PL3 DynamicOffset osss -> [Word32]

instance DynamicOffsetList3ToList '[] where
	dynamicOffsetList3ToList HeteroParList.Nil = []

instance DynamicOffsetList3ToList osss =>
	DynamicOffsetList3ToList ('[] ': osss) where
	dynamicOffsetList3ToList (HeteroParList.Nil :** dosss) =
		dynamicOffsetList3ToList dosss

instance DynamicOffsetList3ToList (oss ': osss) =>
	DynamicOffsetList3ToList (('[] ': oss) ': osss) where
	dynamicOffsetList3ToList ((HeteroParList.Nil :** doss) :** dosss) =
		dynamicOffsetList3ToList $ doss :** dosss

instance DynamicOffsetList3ToList ((os ': oss) ': osss) =>
	DynamicOffsetList3ToList (((obj ': os) ': oss) ': osss) where
	dynamicOffsetList3ToList (((DynamicOffset ofst :** dos) :** doss) :** dosss) =
		ofst : dynamicOffsetList3ToList ((dos :** doss) :** dosss)

printDscSetLengths ::
	Show (HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(DescriptorSet.LayoutArgOnlyDynamics slbts)) =>
	DescriptorSet.S sd sp slbts -> IO ()
printDscSetLengths (DescriptorSet.S lns _) = print =<< readIORef lns

getDscSetLengths :: DescriptorSet.S sd sp slbts ->
	IO (HeteroParList.PL2 KObj.ObjectLength
		(DescriptorSet.LayoutArgOnlyDynamics slbts))
getDscSetLengths (DescriptorSet.S lns _) = readIORef lns

class GetDscSetListLength spslbtss where
	type GetDscSetListLengthSnds spslbtss ::
		[(Type, [DescriptorSetLayout.BindingType])]
	getDscSetListLength ::
		HeteroParList.PL (U2 (DescriptorSet.S sd)) spslbtss ->
		IO (HeteroParList.PL3 KObj.ObjectLength
			(DescriptorSet.LayoutArgListOnlyDynamics
				(GetDscSetListLengthSnds spslbtss)))

instance GetDscSetListLength '[] where
	type GetDscSetListLengthSnds '[] = '[]
	getDscSetListLength HeteroParList.Nil = pure HeteroParList.Nil

instance GetDscSetListLength spslbtss =>
	GetDscSetListLength ('(sp, slbts) ': spslbtss) where
	type GetDscSetListLengthSnds ('(sp, slbts) ': spslbtss) =
		slbts ': GetDscSetListLengthSnds spslbtss
	getDscSetListLength (U2 ds :** dss) =
		(:**) <$> getDscSetLengths ds <*> getDscSetListLength dss

type family MapThird tpl where
	MapThird '[] = '[]
	MapThird ('(x, y, z) ': xs) = z ': MapThird xs

type family MapForth tpl where
	MapForth '[] = '[]
	MapForth ('(x :: j, y :: k, z :: l, w :: m) ': xs) = w ': MapForth xs

bindVertexBuffers :: forall sc vs slbtss smsbvs .
	InfixIndex (MapForth smsbvs) (MapSubType vs) =>
	CommandBuffer.GBinded sc vs slbtss -> HeteroParList.PL (U4 Buffer.IndexedList) smsbvs ->
	IO ()
bindVertexBuffers (CommandBuffer.GBinded cb) bils = M.bindVertexBuffers
	cb (fromIntegral fb) (Buffer.indexedListToMiddles bils)
	where fb = infixIndex @(MapForth smsbvs) @(MapSubType vs)

bindIndexBuffer :: forall sc vs slbtss sm sb nm v . IsIndexType v =>
	CommandBuffer.GBinded sc vs slbtss -> Buffer.IndexedList sm sb nm v -> IO ()
bindIndexBuffer (CommandBuffer.GBinded cb) ib =
	uncurry (M.bindIndexBuffer cb) (Buffer.indexedListToMiddle ib) (indexType @v)

class IsIndexType a where indexType :: IndexType

instance IsIndexType Word16 where indexType = IndexTypeUint16
instance IsIndexType Word32 where indexType = IndexTypeUint32

copyBuffer :: forall (ass :: [[VObj.Object]]) nms nmd sos sod sc vs sms sbs smd sbd .
	Buffer.MakeCopies ass sos sod =>
	CommandBuffer.Binded sc vs ->
	Buffer.Binded sms sbs nms sos -> Buffer.Binded smd sbd nmd sod -> IO ()
copyBuffer (CommandBuffer.Binded cb) (Buffer.Binded lnss src) (Buffer.Binded lnsd dst) =
	M.copyBuffer cb src dst (Buffer.makeCopies @ass lnss lnsd)

pushConstants' :: forall (ss :: [T.ShaderStageFlagBits]) sc vs s sbtss whole ts . (
	PokableList ts,
	PushConstant.ShaderStageFlagBitsToMiddle ss,
	PushConstant.OffsetSize whole ts ) =>
	CommandBuffer.Binded sc vs -> Pipeline.Layout.L s sbtss whole ->
	HeteroParList.L ts -> IO ()
pushConstants' (CommandBuffer.Binded cb) (Pipeline.Layout.L lyt) xs =
	M.pushConstants cb lyt (PushConstant.shaderStageFlagBitsToMiddle @ss)
		(PushConstant.offset @whole @ts 0) xs

pipelineBarrier :: (
	WithPokedHeteroToListCpsM ns,
	WithPokedHeteroToListCpsM (Buffer.FirstOfFives nsmsbnmobjs),
	WithPokedHeteroToListCpsM (Image.FirstOfFives nsismnmfmts),
	Buffer.MemoryBarrierListToMiddle nsmsbnmobjs,
	Image.MemoryBarrierListToMiddle nsismnmfmts ) =>
	CommandBuffer.Binded sc vs -> Pipeline.StageFlags -> Pipeline.StageFlags ->
	DependencyFlags -> HeteroParList.PL Memory.M.Barrier ns ->
	HeteroParList.PL (U5 Buffer.MemoryBarrier) nsmsbnmobjs ->
	HeteroParList.PL (U5 Image.MemoryBarrier) nsismnmfmts -> IO ()
pipelineBarrier (CommandBuffer.Binded cb) ssm dsm dfs mbs bmbs imbs =
	M.pipelineBarrier cb ssm dsm dfs mbs
		(Buffer.memoryBarrierListToMiddle bmbs)
		(Image.memoryBarrierListToMiddle imbs)

copyBufferToImage :: forall algn objs img inms sc vs sm sb nm si sm' nm' . (
	ImageCopyListToMiddle algn objs img inms ) =>
	CommandBuffer.Binded sc vs -> Buffer.Binded sm sb nm objs ->
	Image.BindedNew si sm' nm' (Buffer.ImageFormat img) -> Image.Layout ->
	HeteroParList.PL (Buffer.ImageCopy img) (inms :: [Symbol]) -> IO ()
copyBufferToImage (CommandBuffer.Binded cb)
	bf@(Buffer.Binded _ mbf) (Image.BindedNew mim) imlyt ics =
	M.copyBufferToImage cb mbf mim imlyt mics
	where mics = imageCopyListToMiddle @algn bf ics

copyImageToBuffer :: forall algn objs img inms sc vs sm sb nm si sm' nm' . (
	ImageCopyListToMiddle algn objs img inms ) =>
	CommandBuffer.Binded sc vs ->
	Image.BindedNew si sm' nm' (Buffer.ImageFormat img) -> Image.Layout ->
	Buffer.Binded sm sb nm objs ->
	HeteroParList.PL (Buffer.ImageCopy img) (inms :: [Symbol]) -> IO ()
copyImageToBuffer (CommandBuffer.Binded cb)
	(Image.BindedNew mim) imlyt bf@(Buffer.Binded _ mbf) ics =
	M.copyImageToBuffer cb mim imlyt mbf mics
	where mics = imageCopyListToMiddle @algn bf ics

class ImageCopyListToMiddle algn objs (img :: Type) (inms :: [Symbol]) where
	imageCopyListToMiddle ::
		Buffer.Binded sm sb nm objs ->
		HeteroParList.PL (Buffer.ImageCopy img) inms ->
		[Buffer.M.ImageCopy]

instance ImageCopyListToMiddle algn objs img '[] where
	imageCopyListToMiddle _ HeteroParList.Nil = []

instance (
	Buffer.OffsetSize (VObj.ObjImage algn img nm) objs,
	ImageCopyListToMiddle algn objs img nms ) =>
	ImageCopyListToMiddle algn objs img (nm ': nms) where
	imageCopyListToMiddle bf (ic :** ics) =
		Buffer.imageCopyToMiddle @algn @_ @nm bf (ic :: Buffer.ImageCopy img nm) :
		imageCopyListToMiddle @algn bf ics

blitImage :: CommandBuffer.Binded sc vs ->
	Image.BindedNew ssi ssm snm sfmt -> Image.Layout ->
	Image.BindedNew dsi dsm dnm dfmt -> Image.Layout ->
	[Image.M.Blit] -> Filter -> IO ()
blitImage (CommandBuffer.Binded cb)
	(Image.BindedNew src) slyt (Image.BindedNew dst) dlyt blts fltr =
	M.blitImage cb src slyt dst dlyt blts fltr
