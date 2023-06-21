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

module Gpu.Vulkan.Cmd (

-- * BEGIN RENDER PASS

beginRenderPass,

-- * DRAW AND DISPATCH

-- ** Draw

bindPipelineGraphics,
bindVertexBuffers, bindIndexBuffer, IsIndex,
draw, drawIndexed,
VertexCount, IndexCount, InstanceCount,
FirstVertex, FirstIndex, FirstInstance,
VertexOffset,

-- ** Dispatch

bindPipelineCompute, dispatch, GroupCountX, GroupCountY, GroupCountZ,

-- * PUSH CONSTANTS AND BIND DESCRIPTOR SETS

pushConstantsGraphics, pushConstantsCompute,
bindDescriptorSetsGraphics, bindDescriptorSetsCompute,
DynamicIndex(..), GetDynamicLength,

-- * COPY BUFFER AND IMAGES

copyBuffer,
copyBufferToImage,
copyImageToBuffer,
blitImage,

-- * MEMORY DEPENDENCY

pipelineBarrier,

-- * QUERY

resetQueryPool,
beginQuery,
writeTimestamp

) where

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Foreign.Storable.HeteroList
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word
import Data.Int

import Gpu.Vulkan
import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.CommandBuffer.Type as CommandBuffer
import qualified Gpu.Vulkan.Pipeline.Graphics.Type as Pipeline
import qualified Gpu.Vulkan.Pipeline.Compute as Pipeline.Compute
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.PipelineLayout.Type as PipelineLayout
import qualified Gpu.Vulkan.DescriptorSet as DescriptorSet
import qualified Gpu.Vulkan.DescriptorSet.TypeLevel.Write as DescriptorSet
import qualified Gpu.Vulkan.Buffer.Type as Buffer
import qualified Gpu.Vulkan.Buffer.Internal as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Image as Image
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M

import qualified Gpu.Vulkan.RenderPass.Type as RenderPass
import qualified Gpu.Vulkan.Subpass.Enum as Subpass
import qualified Gpu.Vulkan.Cmd.Middle as M

import qualified Gpu.Vulkan.PushConstant as PushConstant
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

import Data.IORef -- for debug
import Data.Kind.Object qualified as KObj

import Gpu.Vulkan.Query.Enum qualified as Query
import Gpu.Vulkan.QueryPool qualified as QueryPool

import Foreign.Storable

beginRenderPass :: (WithPoked (TMaybe.M mn), ClearValueListToCore cts) =>
	CommandBuffer.C sc -> RenderPass.BeginInfo mn sr sf cts ->
	Subpass.Contents -> IO a -> IO a
beginRenderPass (CommandBuffer.C cb) bi cnt f = bracket_
	(M.beginRenderPass cb (RenderPass.beginInfoToMiddle bi) cnt)
	(M.endRenderPass cb) f

bindPipelineGraphics :: CommandBuffer.C sc ->
	Pipeline.BindPoint -> Pipeline.G sg vibs vias slbtss ->
	(forall sgb . CommandBuffer.GBinded sgb vibs slbtss -> IO a) -> IO a
bindPipelineGraphics (CommandBuffer.C c) bp (Pipeline.G g) f =
	M.bindPipelineGraphics c bp g >> f (CommandBuffer.GBinded c)

bindPipelineCompute :: CommandBuffer.C scmdb -> Pipeline.BindPoint ->
	Pipeline.Compute.C scp slbtss ->
	(forall scbnd . CommandBuffer.CBinded scbnd slbtss -> IO a) -> IO a
bindPipelineCompute (CommandBuffer.C cb) bp (Pipeline.Compute.C g) f =
	M.bindPipelineCompute cb bp g >> f (CommandBuffer.CBinded cb)

draw :: CommandBuffer.GBinded sc vibs slbtss ->
	VertexCount -> InstanceCount -> FirstVertex -> FirstInstance -> IO ()
draw (CommandBuffer.GBinded cb) vc ic fv fi = M.draw cb vc ic fv fi

type VertexCount = Word32
type InstanceCount = Word32
type FirstVertex = Word32
type FirstInstance = Word32

drawIndexed :: CommandBuffer.GBinded sc vibs slbtss ->
	IndexCount -> InstanceCount ->
	FirstIndex -> VertexOffset -> FirstInstance -> IO ()
drawIndexed (CommandBuffer.GBinded cb) idxc istc fidx vo fist =
	M.drawIndexed cb idxc istc fidx vo fist

type IndexCount = Word32
type FirstIndex = Word32
type VertexOffset = Int32

dispatch :: CommandBuffer.CBinded sc slbtss ->
	GroupCountX -> GroupCountY -> GroupCountZ -> IO ()
dispatch (CommandBuffer.CBinded cb) = M.dispatch cb

type GroupCountX = Word32
type GroupCountY = Word32
type GroupCountZ = Word32

bindDescriptorSetsGraphics :: forall sgbnd vibs sl dsls pcs dss dsls' dyns . (
	TMapIndex.M1_2 dss ~ dsls',
	DescriptorSet.LayoutArgListOnlyDynamics dsls' ~ dyns,
	InfixIndex dsls' dsls,
	GetDynamicLength dss,
	HeteroParList.ZipListWithC3 KObj.SizeAlignment dyns ) =>
	CommandBuffer.GBinded sgbnd vibs '(sl, dsls, pcs) ->
	Pipeline.BindPoint -> PipelineLayout.L sl dsls pcs ->
	HeteroParList.PL (U2 DescriptorSet.D) dss ->
	HeteroParList.PL3 DynamicIndex
		(DescriptorSet.LayoutArgListOnlyDynamics dsls') -> IO ()
bindDescriptorSetsGraphics
	(CommandBuffer.GBinded c) bp (PipelineLayout.L l) dss idxs = do
	lns <- getDscSetListLength dss
	let	dosts = concat $ concat <$> getOffsetListNew lns idxs
	M.bindDescriptorSets c bp l
		(fromIntegral $ infixIndex @_ @dsls' @dsls)
		(HeteroParList.toList
			(\(U2 (DescriptorSet.D _ s)) -> s)
			dss)
		dosts

bindDescriptorSetsCompute :: forall sc s sbtss sbtss' foo spslbtss' sdsspslbtss dyns . (
	TMapIndex.M1_2 sdsspslbtss ~ spslbtss',
	DescriptorSet.LayoutArgListOnlyDynamics spslbtss' ~ dyns,
	spslbtss' ~ sbtss',
	GetDynamicLength sdsspslbtss,
	InfixIndex spslbtss' sbtss,
	HeteroParList.ZipListWithC3 KObj.SizeAlignment dyns ) =>
	CommandBuffer.CBinded sc '(s, sbtss, foo) ->
	PipelineLayout.L s sbtss foo -> HeteroParList.PL (U2 DescriptorSet.D) sdsspslbtss ->
	HeteroParList.PL3 DynamicIndex (DescriptorSet.LayoutArgListOnlyDynamics sbtss') ->
	IO ()
bindDescriptorSetsCompute
	(CommandBuffer.CBinded c) (PipelineLayout.L l) dss idxs = do
	lns <- getDscSetListLength dss
	let	dosts = concat $ concat <$> getOffsetListNew lns idxs
	M.bindDescriptorSets c Pipeline.BindPointCompute l
		(fromIntegral $ infixIndex @_ @spslbtss' @sbtss)
		(HeteroParList.toList (\(U2 (DescriptorSet.D _ s)) -> s)
			dss)
		dosts

newtype DynamicIndex (obj :: KObj.Object) = DynamicIndex Word32 deriving Show
newtype DynamicOffset (obj :: KObj.Object) = DynamicOffset Word32 deriving Show

getOffset' :: forall obj . KObj.SizeAlignment obj =>
	KObj.ObjectLength obj -> DynamicIndex obj -> Word32
getOffset' ln (DynamicIndex i) = fromIntegral sz * i
	where
	sz = ((KObj.objectSize ln - 1) `div` algn + 1) * algn
	algn = KObj.objectAlignment @obj

getOffsetListNew :: HeteroParList.ZipListWithC3 KObj.SizeAlignment osss =>
		HeteroParList.PL3 KObj.ObjectLength osss ->
		HeteroParList.PL3 DynamicIndex osss -> [[[Word32]]]
getOffsetListNew = HeteroParList.zipListWithC3 @KObj.SizeAlignment getOffset'

class GetDynamicLength sspslbtss where
	getDscSetListLength ::
		HeteroParList.PL (U2 DescriptorSet.D) sspslbtss ->
		IO (HeteroParList.PL3 KObj.ObjectLength
			(DescriptorSet.LayoutArgListOnlyDynamics (TMapIndex.M1_2 sspslbtss)))

instance GetDynamicLength '[] where
	getDscSetListLength HeteroParList.Nil = pure HeteroParList.Nil

instance GetDynamicLength spslbtss =>
	GetDynamicLength (slbts ': spslbtss) where
	getDscSetListLength (U2 ds :** dss) =
		(:**) <$> getDscSetLengthsNew ds <*> getDscSetListLength dss

getDscSetLengthsNew :: DescriptorSet.D s slbts ->
	IO (HeteroParList.PL2 KObj.ObjectLength
		(DescriptorSet.LayoutArgOnlyDynamics slbts))
getDscSetLengthsNew (DescriptorSet.D lns _) = readIORef lns

bindVertexBuffers :: forall sg vibs slbtss smsbnmts .
	InfixIndex (TMapIndex.M3_5 smsbnmts) (TMapIndex.M0_2 vibs) =>
	CommandBuffer.GBinded sg vibs slbtss ->
	HeteroParList.PL (U5 Buffer.IndexedForList) smsbnmts -> IO ()
bindVertexBuffers (CommandBuffer.GBinded cb) bils = M.bindVertexBuffers
	cb (fromIntegral fb) (Buffer.indexedListToMiddles bils)
	where fb = infixIndex @_ @(TMapIndex.M3_5 smsbnmts) @(TMapIndex.M0_2 vibs)

bindIndexBuffer :: forall sg vibs slbtss sm sb nm i onm . IsIndex i =>
	CommandBuffer.GBinded sg vibs slbtss ->
	Buffer.IndexedForList sm sb nm i onm -> IO ()
bindIndexBuffer (CommandBuffer.GBinded cb) ib =
	uncurry (M.bindIndexBuffer cb) (Buffer.indexedListToMiddle ib) (indexType @i)

class IsIndex a where indexType :: IndexType
instance IsIndex Word16 where indexType = IndexTypeUint16
instance IsIndex Word32 where indexType = IndexTypeUint32

copyBuffer :: forall (ass :: [[VObj.Object]]) nms nmd sos sod sc sms sbs smd sbd .
	Buffer.MakeCopies ass sos sod =>
	CommandBuffer.C sc ->
	Buffer.Binded sms sbs nms sos -> Buffer.Binded smd sbd nmd sod -> IO ()
copyBuffer (CommandBuffer.C cb) (Buffer.Binded lnss src) (Buffer.Binded lnsd dst) =
	M.copyBuffer cb src dst (Buffer.makeCopies @ass lnss lnsd)

pushConstantsGraphics :: forall sss sc vibs sl sbtss pcs ts . (
	PushConstant.ShaderStageFlagBitsToMiddle sss,
	PokableList ts, InfixOffsetSize ts pcs ) =>
	CommandBuffer.GBinded sc vibs '(sl, sbtss, pcs) ->
	PipelineLayout.L sl sbtss pcs -> HeteroParList.L ts -> IO ()
pushConstantsGraphics (CommandBuffer.GBinded cb) (PipelineLayout.L lyt) xs =
	M.pushConstants
		cb lyt (PushConstant.shaderStageFlagBitsToMiddle @sss) offt xs
		where (fromIntegral -> offt, _) = infixOffsetSize @ts @pcs

pushConstantsCompute :: forall sss sc sl sbtss pcs ts . (
	PushConstant.ShaderStageFlagBitsToMiddle sss,
	PokableList ts, InfixOffsetSize ts pcs ) =>
	CommandBuffer.CBinded sc '(sl, sbtss, pcs) ->
	PipelineLayout.L sl sbtss pcs -> HeteroParList.L ts -> IO ()
pushConstantsCompute (CommandBuffer.CBinded cb) (PipelineLayout.L lyt) xs =
	M.pushConstants
		cb lyt (PushConstant.shaderStageFlagBitsToMiddle @sss) offt xs
		where (fromIntegral -> offt, _) = infixOffsetSize @ts @pcs

pipelineBarrier :: (
	WithPokedHeteroToListCpsM' TMaybe.M ns,
	WithPokedHeteroToListCpsM' TMaybe.M (TMapIndex.M0_5 nsmsbnmobjs),
	WithPokedHeteroToListCpsM' TMaybe.M (Image.FirstOfFives nsismnmfmts),
	Buffer.MemoryBarrierListToMiddle nsmsbnmobjs,
	Image.MemoryBarrierListToMiddle nsismnmfmts ) =>
	CommandBuffer.C sc -> Pipeline.StageFlags -> Pipeline.StageFlags ->
	DependencyFlags -> HeteroParList.PL Memory.M.Barrier ns ->
	HeteroParList.PL (U5 Buffer.MemoryBarrier) nsmsbnmobjs ->
	HeteroParList.PL (U5 Image.MemoryBarrier) nsismnmfmts -> IO ()
pipelineBarrier (CommandBuffer.C cb) ssm dsm dfs mbs bmbs imbs =
	M.pipelineBarrier cb ssm dsm dfs mbs
		(Buffer.memoryBarrierListToMiddle bmbs)
		(Image.memoryBarrierListToMiddle imbs)

copyBufferToImage :: forall algn objs img inms sc sm sb nm si sm' nm' . (
	ImageCopyListToMiddle algn objs img inms ) =>
	CommandBuffer.C sc -> Buffer.Binded sm sb nm objs ->
	Image.BindedNew si sm' nm' (KObj.ImageFormat img) -> Image.Layout ->
	HeteroParList.PL (Buffer.ImageCopy img) (inms :: [Symbol]) -> IO ()
copyBufferToImage (CommandBuffer.C cb)
	bf@(Buffer.Binded _ mbf) (Image.BindedNew mim) imlyt ics =
	M.copyBufferToImage cb mbf mim imlyt mics
	where mics = imageCopyListToMiddle @algn bf ics

copyImageToBuffer :: forall algn objs img inms sc  sm sb nm si sm' nm' . (
	ImageCopyListToMiddle algn objs img inms ) =>
	CommandBuffer.C sc  ->
	Image.BindedNew si sm' nm' (KObj.ImageFormat img) -> Image.Layout ->
	Buffer.Binded sm sb nm objs ->
	HeteroParList.PL (Buffer.ImageCopy img) (inms :: [Symbol]) -> IO ()
copyImageToBuffer (CommandBuffer.C cb)
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
	Storable (KObj.IsImagePixel img), KnownNat algn,
	VObj.Offset (VObj.ObjImage algn img nm) objs,
	VObj.ObjectLengthOf (VObj.ObjImage algn img nm) objs,
	ImageCopyListToMiddle algn objs img nms ) =>
	ImageCopyListToMiddle algn objs img (nm ': nms) where
	imageCopyListToMiddle bf (ic :** ics) =
		Buffer.imageCopyToMiddle @algn @_ @nm bf (ic :: Buffer.ImageCopy img nm) :
		imageCopyListToMiddle @algn bf ics

blitImage :: CommandBuffer.C sc ->
	Image.BindedNew ssi ssm snm sfmt -> Image.Layout ->
	Image.BindedNew dsi dsm dnm dfmt -> Image.Layout ->
	[Image.M.Blit] -> Filter -> IO ()
blitImage (CommandBuffer.C cb)
	(Image.BindedNew src) slyt (Image.BindedNew dst) dlyt blts fltr =
	M.blitImage cb src slyt dst dlyt blts fltr

resetQueryPool ::
	CommandBuffer.C sc -> QueryPool.Q sq tp -> Word32 -> Word32 -> IO ()
resetQueryPool (CommandBuffer.C cb) (QueryPool.Q qp) fq qc =
	M.resetQueryPool cb qp fq qc

beginQuery :: CommandBuffer.C sc ->
	QueryPool.Q sq tp -> Word32 -> Query.ControlFlags -> IO a -> IO ()
beginQuery (CommandBuffer.C cb) (QueryPool.Q qp) i flgs act =
	M.beginQuery cb qp i flgs >> act >> M.endQuery cb qp i

writeTimestamp :: CommandBuffer.C sc -> Pipeline.StageFlagBits ->
	QueryPool.Q sq QueryPool.Timestamp -> Word32 -> IO ()
writeTimestamp (CommandBuffer.C cb) sflgs (QueryPool.Q qp) i =
	M.writeTimestamp cb sflgs qp i
