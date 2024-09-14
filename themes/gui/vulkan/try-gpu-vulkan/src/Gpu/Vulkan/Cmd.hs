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

copyBuffer, copyBufferToImage, copyImageToBuffer, blitImage,

-- * MEMORY DEPENDENCY

pipelineBarrier,

-- * QUERY

resetQueryPool,
beginQuery,
writeTimestamp,

-- * OTHERS

LayoutArgListOnlyDynamics

) where

import GHC.TypeNats
import Foreign.Storable.PeekPoke
import Foreign.Storable.HeteroList
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word
import Data.Int

import Gpu.Vulkan
import Gpu.Vulkan.TypeEnum qualified as T

import qualified Gpu.Vulkan.CommandBuffer as CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer.Type as CommandBuffer.T
import qualified Gpu.Vulkan.Pipeline.Graphics.Type as Pipeline
import qualified Gpu.Vulkan.Pipeline.Compute as Pipeline.Compute
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline
import qualified Gpu.Vulkan.PipelineLayout.Type as PipelineLayout
import qualified Gpu.Vulkan.DescriptorSet as DescriptorSet
import qualified Gpu.Vulkan.DescriptorSet.Type as DescriptorSet
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout
import qualified Gpu.Vulkan.Buffer.Type as Bffr
import qualified Gpu.Vulkan.Buffer as Bffr
import qualified Gpu.Vulkan.Buffer.Internal as Bffr.I
import qualified Gpu.Vulkan.Image as Image
import qualified Gpu.Vulkan.Image.Internal as Image.I
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M

import qualified Gpu.Vulkan.RenderPass.Internal as RenderPass
import qualified Gpu.Vulkan.Subpass.Enum as Subpass
import qualified Gpu.Vulkan.Cmd.Middle as M

import qualified Gpu.Vulkan.Memory as Memory

import Data.IORef -- for debug

import Gpu.Vulkan.Query.Enum qualified as Query
import Gpu.Vulkan.QueryPool qualified as QueryPool
import Gpu.Vulkan.QueryPool.Type qualified as QueryPool
import Gpu.Vulkan.Query qualified as Query
import Gpu.Vulkan.Object.Base qualified as KObj

beginRenderPass :: (WithPoked (TMaybe.M mn), ClearValueListToCore cts) =>
	CommandBuffer.C scb -> RenderPass.BeginInfo mn sr sf cts ->
	Subpass.Contents -> IO a -> IO a
beginRenderPass (CommandBuffer.T.C cb) bi cnt f = bracket_
	(M.beginRenderPass cb (RenderPass.beginInfoToMiddle bi) cnt)
	(M.endRenderPass cb) f

bindPipelineGraphics :: CommandBuffer.C scb ->
	Pipeline.BindPoint -> Pipeline.G sg vibs vias slbtss ->
	(forall sgb . CommandBuffer.GBinded sgb vibs slbtss -> IO a) -> IO a
bindPipelineGraphics (CommandBuffer.T.C c) bp (Pipeline.G g) f =
	M.bindPipelineGraphics c bp g >> f (CommandBuffer.T.GBinded c)

bindPipelineCompute :: CommandBuffer.C scmdb -> Pipeline.BindPoint ->
	Pipeline.Compute.C scp slbtss ->
	(forall scbnd . CommandBuffer.CBinded scbnd slbtss -> IO a) -> IO a
bindPipelineCompute (CommandBuffer.T.C cb) bp (Pipeline.Compute.C g) f =
	M.bindPipelineCompute cb bp g >> f (CommandBuffer.T.CBinded cb)

draw :: CommandBuffer.GBinded sc vibs slbtss ->
	VertexCount -> InstanceCount -> FirstVertex -> FirstInstance -> IO ()
draw (CommandBuffer.T.GBinded cb) vc ic fv fi = M.draw cb vc ic fv fi

type VertexCount = Word32
type InstanceCount = Word32
type FirstVertex = Word32
type FirstInstance = Word32

drawIndexed :: CommandBuffer.GBinded sc vibs slbtss ->
	IndexCount -> InstanceCount ->
	FirstIndex -> VertexOffset -> FirstInstance -> IO ()
drawIndexed (CommandBuffer.T.GBinded cb) idxc istc fidx vo fist =
	M.drawIndexed cb idxc istc fidx vo fist

type IndexCount = Word32
type FirstIndex = Word32
type VertexOffset = Int32

dispatch :: CommandBuffer.CBinded sc slbtss ->
	GroupCountX -> GroupCountY -> GroupCountZ -> IO ()
dispatch (CommandBuffer.T.CBinded cb) = M.dispatch cb

type GroupCountX = Word32
type GroupCountY = Word32
type GroupCountZ = Word32

bindDescriptorSetsGraphics :: forall sgbnd vibs sl dsls pcs dss dsls' dyns . (
	TMapIndex.M1_2 dss ~ dsls',
	LayoutArgListOnlyDynamics dsls' ~ dyns,
	InfixIndex dsls' dsls, GetDynamicLength dss,
	HeteroParList.ZipListWithC3 KObj.SizeAlignment dyns ) =>
	CommandBuffer.GBinded sgbnd vibs '(sl, dsls, pcs) ->
	Pipeline.BindPoint -> PipelineLayout.P sl dsls pcs ->
	HeteroParList.PL (U2 DescriptorSet.D) dss ->
	HeteroParList.PL3 DynamicIndex dyns -> IO ()
bindDescriptorSetsGraphics
	(CommandBuffer.T.GBinded c) bp (PipelineLayout.P l) dss idxs = do
	lns <- getDynamicLength dss
	let	dosts = concat $ concat <$> getOffsetListNew lns idxs
	M.bindDescriptorSets c bp l
		(fromIntegral $ infixIndex @_ @dsls' @dsls)
		(HeteroParList.toList
			(\(U2 (DescriptorSet.D _ s)) -> s)
			dss)
		dosts

bindDescriptorSetsCompute :: forall scbnd sl dsls pcs dss dsls' dyns . (
	TMapIndex.M1_2 dss ~ dsls',
	LayoutArgListOnlyDynamics dsls' ~ dyns,
	InfixIndex dsls' dsls, GetDynamicLength dss,
	HeteroParList.ZipListWithC3 KObj.SizeAlignment dyns ) =>
	CommandBuffer.CBinded scbnd '(sl, dsls, pcs) ->
	PipelineLayout.P sl dsls pcs ->
	HeteroParList.PL (U2 DescriptorSet.D) dss ->
	HeteroParList.PL3 DynamicIndex dyns -> IO ()
bindDescriptorSetsCompute
	(CommandBuffer.T.CBinded c) (PipelineLayout.P l) dss idxs = do
	lns <- getDynamicLength dss
	let	dosts = concat $ concat <$> getOffsetListNew lns idxs
	M.bindDescriptorSets c Pipeline.BindPointCompute l
		(fromIntegral $ infixIndex @_ @dsls' @dsls)
		(HeteroParList.toList (\(U2 (DescriptorSet.D _ s)) -> s)
			dss)
		dosts

newtype DynamicIndex (obj :: KObj.O) = DynamicIndex Word32 deriving Show
newtype DynamicOffset (obj :: KObj.O) = DynamicOffset Word32 deriving Show

getOffset' :: forall obj . KObj.SizeAlignment obj =>
	KObj.Length obj -> DynamicIndex obj -> Word32
getOffset' ln (DynamicIndex i) = fromIntegral sz * i
	where
	sz = ((KObj.size ln - 1) `div` algn + 1) * algn
	algn = KObj.alignment @obj

getOffsetListNew :: HeteroParList.ZipListWithC3 KObj.SizeAlignment osss =>
		HeteroParList.PL3 KObj.Length osss ->
		HeteroParList.PL3 DynamicIndex osss -> [[[Word32]]]
getOffsetListNew = HeteroParList.zipListWithC3 @KObj.SizeAlignment getOffset'

class GetDynamicLength sspslbtss where
	getDynamicLength ::
		HeteroParList.PL (U2 DescriptorSet.D) sspslbtss ->
		IO (HeteroParList.PL3 KObj.Length
			(LayoutArgListOnlyDynamics (TMapIndex.M1_2 sspslbtss)))

type family LayoutArgListOnlyDynamics las where
	LayoutArgListOnlyDynamics '[] = '[]
	LayoutArgListOnlyDynamics (la ': las) =
		Layout.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 la) ':
			LayoutArgListOnlyDynamics las

instance GetDynamicLength '[] where
	getDynamicLength HeteroParList.Nil = pure HeteroParList.Nil

instance GetDynamicLength spslbtss =>
	GetDynamicLength (slbts ': spslbtss) where
	getDynamicLength (U2 ds :** dss) =
		(:**) <$> getDscSetLengthsNew ds <*> getDynamicLength dss

getDscSetLengthsNew :: DescriptorSet.D s slbts ->
	IO (HeteroParList.PL2 KObj.Length
		(Layout.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts)))
getDscSetLengthsNew (DescriptorSet.D lns _) = readIORef lns

bindVertexBuffers :: forall sg vibs slbtss smsbnmts .
	InfixIndex (TMapIndex.M3_5 smsbnmts) (TMapIndex.M0_2 vibs) =>
	CommandBuffer.GBinded sg vibs slbtss ->
	HeteroParList.PL (U5 Bffr.IndexedForList) smsbnmts -> IO ()
bindVertexBuffers (CommandBuffer.T.GBinded cb) bils = M.bindVertexBuffers
	cb (fromIntegral fb) (Bffr.I.indexedListToMiddles bils)
	where fb = infixIndex @_ @(TMapIndex.M3_5 smsbnmts) @(TMapIndex.M0_2 vibs)

bindIndexBuffer :: forall sg vibs slbtss sm sb nm i onm . IsIndex i =>
	CommandBuffer.GBinded sg vibs slbtss ->
	Bffr.IndexedForList sm sb nm i onm -> IO ()
bindIndexBuffer (CommandBuffer.T.GBinded cb) ib =
	uncurry (M.bindIndexBuffer cb) (Bffr.I.indexedListToMiddle ib) (indexType @i)

class IsIndex a where indexType :: IndexType
instance IsIndex Word16 where indexType = IndexTypeUint16
instance IsIndex Word32 where indexType = IndexTypeUint32

copyBuffer :: forall cpobjss scb sms sbs nms objss smd sbd nmd objsd .
	Bffr.MakeCopies cpobjss objss objsd => CommandBuffer.C scb ->
	Bffr.Binded sms sbs nms objss -> Bffr.Binded smd sbd nmd objsd -> IO ()
copyBuffer (CommandBuffer.T.C cb) (Bffr.Binded lnss s) (Bffr.Binded lnsd d) =
	M.copyBuffer cb s d (Bffr.I.makeCopies @cpobjss lnss lnsd)

pushConstantsGraphics :: forall sss sc vibs sl sbtss pcs ts . (
	T.ShaderStageFlagBitsListToValue sss,
	PokableList ts, InfixOffsetSize ts pcs ) =>
	CommandBuffer.GBinded sc vibs '(sl, sbtss, pcs) ->
	PipelineLayout.P sl sbtss pcs -> HeteroParList.L ts -> IO ()
pushConstantsGraphics (CommandBuffer.T.GBinded cb) (PipelineLayout.P lyt) xs =
	M.pushConstants
		cb lyt (T.shaderStageFlagBitsListToValue @sss) offt xs
		where (fromIntegral -> offt, _) = infixOffsetSize @ts @pcs

pushConstantsCompute :: forall sss sc sl sbtss pcs ts . (
	T.ShaderStageFlagBitsListToValue sss,
	PokableList ts, InfixOffsetSize ts pcs ) =>
	CommandBuffer.CBinded sc '(sl, sbtss, pcs) ->
	PipelineLayout.P sl sbtss pcs -> HeteroParList.L ts -> IO ()
pushConstantsCompute (CommandBuffer.T.CBinded cb) (PipelineLayout.P lyt) xs =
	M.pushConstants
		cb lyt (T.shaderStageFlagBitsListToValue @sss) offt xs
		where (fromIntegral -> offt, _) = infixOffsetSize @ts @pcs

pipelineBarrier :: (
	HeteroParList.ToListWithCCpsM' WithPoked TMaybe.M mbargs,
	HeteroParList.ToListWithCCpsM' WithPoked TMaybe.M
		(TMapIndex.M0_5 bmbargss),
	HeteroParList.ToListWithCCpsM' WithPoked TMaybe.M
		(TMapIndex.M0_5 imbargss),
	Bffr.MemoryBarrierListToMiddle bmbargss,
	Image.MemoryBarrierListToMiddle imbargss ) =>
	CommandBuffer.C scb -> Pipeline.StageFlags -> Pipeline.StageFlags ->
	DependencyFlags -> HeteroParList.PL Memory.Barrier mbargs ->
	HeteroParList.PL (U5 Bffr.MemoryBarrier) bmbargss ->
	HeteroParList.PL (U5 Image.MemoryBarrier) imbargss -> IO ()
pipelineBarrier (CommandBuffer.T.C cb) ssm dsm dfs mbs bmbs imbs =
	M.pipelineBarrier cb ssm dsm dfs mbs
		(Bffr.I.memoryBarrierListToMiddle bmbs)
		(Image.I.memoryBarrierListToMiddle imbs)

copyBufferToImage ::
	forall (algn :: Nat) img inms scb smb sbb bnm objs smi si inm .
	(Bffr.ImageCopyListToMiddle algn objs img inms) =>
	CommandBuffer.C scb -> Bffr.Binded smb sbb bnm objs ->
	Image.Binded smi si inm (KObj.ImageFormat img) -> Image.Layout ->
	HeteroParList.PL (Bffr.ImageCopy img) inms -> IO ()
copyBufferToImage (CommandBuffer.T.C cb)
	bf@(Bffr.Binded _ mbf) (Image.Binded mim) imlyt ics =
	M.copyBufferToImage cb mbf mim imlyt mics
	where mics = Bffr.I.imageCopyListToMiddle @algn bf ics

copyImageToBuffer ::
	forall (algn :: Nat) img inms scb smi si inm smb sbb bnm objs .
	(Bffr.ImageCopyListToMiddle algn objs img inms) =>
	CommandBuffer.C scb  ->
	Image.Binded smi si inm (KObj.ImageFormat img) -> Image.Layout ->
	Bffr.Binded smb sbb bnm objs ->
	HeteroParList.PL (Bffr.ImageCopy img) inms -> IO ()
copyImageToBuffer (CommandBuffer.T.C cb)
	(Image.Binded mim) imlyt bf@(Bffr.Binded _ mbf) ics =
	M.copyImageToBuffer cb mim imlyt mbf mics
	where mics = Bffr.I.imageCopyListToMiddle @algn bf ics

blitImage :: CommandBuffer.C scb ->
	Image.Binded sms sis nms fmts -> Image.Layout ->
	Image.Binded smd sid nmd fmtd -> Image.Layout ->
	[Image.M.Blit] -> Filter -> IO ()
blitImage (CommandBuffer.T.C cb)
	(Image.Binded src) slyt (Image.Binded dst) dlyt blts fltr =
	M.blitImage cb src slyt dst dlyt blts fltr

resetQueryPool :: CommandBuffer.C sc ->
	QueryPool.Q sq tp -> Query.First -> Query.Count -> IO ()
resetQueryPool (CommandBuffer.T.C cb) (QueryPool.Q qp) fq qc =
	M.resetQueryPool cb qp fq qc

beginQuery :: CommandBuffer.C sc ->
	QueryPool.Q sq tp -> Query.Q -> Query.ControlFlags -> IO a -> IO ()
beginQuery (CommandBuffer.T.C cb) (QueryPool.Q qp) i flgs act =
	M.beginQuery cb qp i flgs >> act >> M.endQuery cb qp i

writeTimestamp :: CommandBuffer.C sc -> Pipeline.StageFlagBits ->
	QueryPool.Q sq QueryPool.Timestamp -> Query.Q -> IO ()
writeTimestamp (CommandBuffer.T.C cb) sflgs (QueryPool.Q qp) i =
	M.writeTimestamp cb sflgs qp i
