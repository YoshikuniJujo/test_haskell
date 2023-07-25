{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics (
	G, createGs, recreateGs, CreateInfo(..) ) where

import GHC.TypeNats
import Control.Exception
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word
import Data.Int

import Shaderc.EnumAuto

import Gpu.Vulkan.Pipeline.Enum
import Gpu.Vulkan.Pipeline.Graphics.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Pipeline.Graphics.Middle as M
import qualified Gpu.Vulkan.Pipeline.Graphics.Tmp as T
import qualified Gpu.Vulkan.PipelineCache.Type as Cache
import qualified Gpu.Vulkan.PipelineLayout.Type as Layout

import qualified Gpu.Vulkan.RenderPass.Type as RenderPass
import qualified Gpu.Vulkan.Pipeline.DynamicState as DynamicState
import qualified Gpu.Vulkan.Pipeline.ColorBlendState as ColorBlendState
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as DepthStencilState
import qualified Gpu.Vulkan.Pipeline.MultisampleState as MultisampleState
import qualified Gpu.Vulkan.Pipeline.RasterizationState as RasterizationState
import qualified Gpu.Vulkan.Pipeline.ViewportState as ViewportState
import qualified Gpu.Vulkan.Pipeline.TessellationState as TessellationState
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState as InputAssemblyState
import qualified Gpu.Vulkan.Pipeline.VertexInputState as VertexInputState
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Internal as ShaderStage

import Gpu.Vulkan.DescriptorSetLayout.Type qualified as DscStLyt

import Gpu.Vulkan.VertexInput qualified as VertexInput

import qualified Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList
	as BindingStrideList

data CreateInfo mn nnskndscdvss nvsts n3 n4 n5 n6 n7 n8 n9 n10 slsbtss sr sbvsts' =
	CreateInfo {
		createInfoNext :: TMaybe.M mn,
		createInfoFlags :: CreateFlags,
		createInfoStages ::
			HeteroParList.PL (U5 ShaderStage.CreateInfoNew) nnskndscdvss,
		createInfoVertexInputState ::
			Maybe (U3 VertexInputState.CreateInfo nvsts),
		createInfoInputAssemblyState ::
			Maybe (InputAssemblyState.CreateInfo n3),
		createInfoTessellationState ::
			Maybe (TessellationState.CreateInfo n4),
		createInfoViewportState :: Maybe (ViewportState.CreateInfo n5),
		createInfoRasterizationState ::
			Maybe (RasterizationState.CreateInfo n6),
		createInfoMultisampleState ::
			Maybe (MultisampleState.CreateInfo n7),
		createInfoDepthStencilState ::
			Maybe (DepthStencilState.CreateInfo n8),
		createInfoColorBlendState ::
			Maybe (ColorBlendState.CreateInfo n9),
		createInfoDynamicState :: Maybe (DynamicState.CreateInfo n10),
		createInfoLayout :: U3 Layout.L slsbtss,
		createInfoRenderPass :: RenderPass.R sr,
		createInfoSubpass :: Word32,
		createInfoBasePipelineHandle :: Maybe (U4 G sbvsts'),
		createInfoBasePipelineIndex :: Int32 }

type CreateInfoArgs14 = (
	Maybe Type,
	[(Maybe Type, Maybe Type, Shaderc.EnumAuto.ShaderKind, Maybe (Type, Type), [Type])],
	(Maybe Type, [(Type, VertexInput.Rate)], [(Nat, Type)]),
	Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type,
	(Type, [(Type, [DscStLyt.BindingType])], [Type]),
	Type,
	(Type, [(Type, VertexInput.Rate)], [(Nat, Type)],
		(Type, [(Type, [DscStLyt.BindingType])], [Type])) )

type GArgs3 = ([(Type, VertexInput.Rate)], [(Nat, Type)],
	(Type, [(Type, [DscStLyt.BindingType])], [Type]))

type family CreateInfoArgs14ToGArgs3 (cia :: CreateInfoArgs14) :: GArgs3 where
	CreateInfoArgs14ToGArgs3 '(
		n, nnskndscdvss, '(nv, vs, ts), n3, n4, n5, n6, n7, n8, n9, n10,
		slbtss, sr, sbvsts' ) = '(vs, ts, slbtss)

type family CreateInfoListArgs14ToGArgs3 (cias :: [CreateInfoArgs14]) ::
	[GArgs3] where
	CreateInfoListArgs14ToGArgs3 '[] = '[]
	CreateInfoListArgs14ToGArgs3 (cia ': cias) =
		CreateInfoArgs14ToGArgs3 cia ':
		CreateInfoListArgs14ToGArgs3 cias

createInfoToMiddle' :: (
	BindingStrideList.BindingStrideList
		VertexInput.Rate vs VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs ts,
	ShaderStage.CreateInfoListToMiddleNew nnskndscdvss ) =>
	Device.D sd ->
	CreateInfo n nnskndscdvss '(nv, vs, ts)
		n3 n4 n5 n6 n7 n8 n9 n10 sl sr '(sb, vs', ts', slbtss') ->
	IO (M.CreateInfo n (ShaderStage.MiddleVarsNew nnskndscdvss) nv n3 n4 n5 n6 n7 n8 n9 n10)
createInfoToMiddle' dv = (T.createInfoToMiddle <$>) . createInfoToMiddle dv

createInfoToMiddle :: (ShaderStage.CreateInfoListToMiddleNew nnskndscdvss) =>
	Device.D sd ->
	CreateInfo n nnskndscdvss nvsts
		n3 n4 n5 n6 n7 n8 n9 n10 sl sr '(sb, vs', ts', slbtss') ->
	IO (T.CreateInfo n (ShaderStage.MiddleVarsNew nnskndscdvss)
		nvsts n3 n4 n5 n6 n7 n8 n9 n10)
createInfoToMiddle dvc CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStages = stgs,
	createInfoVertexInputState = vis,
	createInfoInputAssemblyState = ias,
	createInfoTessellationState = ts,
	createInfoViewportState = vs,
	createInfoRasterizationState = rs,
	createInfoMultisampleState = ms,
	createInfoDepthStencilState = dss,
	createInfoColorBlendState = cbs,
	createInfoDynamicState = ds,
	createInfoLayout = U3 (Layout.L lyt),
	createInfoRenderPass = RenderPass.R rp,
	createInfoSubpass = sp,
	createInfoBasePipelineHandle = bph,
	createInfoBasePipelineIndex = bpi } = do
	stgs' <- ShaderStage.createInfoListToMiddleNew dvc stgs
	bph' <- maybe M.gNull (\(U4 (G g)) -> pure g) bph
	pure T.CreateInfo {
		T.createInfoNext = mnxt,
		T.createInfoFlags = flgs,
		T.createInfoStages = stgs',
		T.createInfoVertexInputState = vis,
		T.createInfoInputAssemblyState = ias,
		T.createInfoTessellationState = ts,
		T.createInfoViewportState = vs,
		T.createInfoRasterizationState = rs,
		T.createInfoMultisampleState = ms,
		T.createInfoDepthStencilState = dss,
		T.createInfoColorBlendState = cbs,
		T.createInfoDynamicState = ds,
		T.createInfoLayout = lyt,
		T.createInfoRenderPass = rp,
		T.createInfoSubpass = sp,
		T.createInfoBasePipelineHandle = bph',
		T.createInfoBasePipelineIndex = bpi }

class CreateInfoListToMiddle ss where
	type MiddleVars ss :: [
		(Maybe Type, [(Maybe Type, ShaderKind, [Type])],
		(Maybe Type, [(Type, VertexInput.Rate)], [(Nat, Type)]),
		Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type)]

	createInfoListToMiddle :: Device.D sd ->
		HeteroParList.PL (U14 CreateInfo) ss ->
		IO (HeteroParList.PL (U11 T.CreateInfo) (MiddleVars ss))

	destroyShaderStages :: Device.D sd ->
		HeteroParList.PL (U11 T.CreateInfo) (MiddleVars ss) ->
		HeteroParList.PL (U14 CreateInfo) ss -> IO ()

class CreateInfoListToMiddle' ss where
	type MiddleVars' ss :: [
		(Maybe Type, [(Maybe Type, ShaderKind, [Type])],
		(Maybe Type, [(Type, VertexInput.Rate)], [(Nat, Type)]),
		Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type)]

	createInfoListToMiddle' :: Device.D sd ->
		HeteroParList.PL (U14 CreateInfo) ss ->
		IO (HeteroParList.PL (U11 T.CreateInfo) (MiddleVars ss))

	destroyShaderStages' :: Device.D sd ->
		HeteroParList.PL (U11 T.CreateInfo) (MiddleVars ss) ->
		HeteroParList.PL (U14 CreateInfo) ss -> IO ()

instance CreateInfoListToMiddle '[] where
	type MiddleVars '[] = '[]
	createInfoListToMiddle _ HeteroParList.Nil = pure HeteroParList.Nil
	destroyShaderStages _ HeteroParList.Nil HeteroParList.Nil = pure ()

instance (
	ShaderStage.CreateInfoListToMiddleNew nnskndscdvss,
	CreateInfoListToMiddle ss ) =>
	CreateInfoListToMiddle ('(
		n, nnskndscdvss, nvsts, n3, n4, n5, n6, n7, n8, n9, n10,
		'(sl, sbtss, pcl), sr, '(sb, vs', ts', slbtss') ) ': ss)  where
	type MiddleVars ('(
		n, nnskndscdvss, nvsts, n3, n4, n5, n6, n7, n8, n9, n10,
		'(sl, sbtss, pcl), sr, '(sb, vs', ts', slbtss') ) ': ss) =
		'(n, ShaderStage.MiddleVarsNew nnskndscdvss, nvsts, n3, n4, n5, n6,
			n7, n8, n9, n10) ': MiddleVars ss
	createInfoListToMiddle dvc (U14 ci :** cis) = (:**)
		<$> (U11 <$> createInfoToMiddle dvc ci)
		<*> createInfoListToMiddle dvc cis
	destroyShaderStages dvc (U11 cim :** cims) (U14 ci :** cis) = do
		ShaderStage.destroyCreateInfoMiddleListNew dvc (T.createInfoStages cim) (createInfoStages ci)
		destroyShaderStages dvc cims cis

class U2g ss where
	v2g :: [M.G] -> HeteroParList.PL (U3 (G sg)) ss
	g2v :: HeteroParList.PL (U3 (G sg)) ss -> [M.G]

instance U2g '[] where v2g [] = HeteroParList.Nil; g2v HeteroParList.Nil = []

instance U2g ss => U2g ('(s, t, foo) ': ss) where
	v2g (g : gs) = U3 (G @_ @s @t g) :** v2g @ss gs
	g2v (U3 (G g) :** gs) = g : g2v gs

createGs :: (
	M.CreateInfoListToCore (T.CreateInfoListArgs (MiddleVars ss)),
	T.CreateInfoListToMiddle (MiddleVars ss),
	CreateInfoListToMiddle ss,
	U2g (CreateInfoListArgs14ToGArgs3 ss),
	AllocationCallbacks.ToMiddle mscc) =>
	Device.D sd -> Maybe (Cache.C sc) ->
	HeteroParList.PL (U14 CreateInfo) ss ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall sg .
		HeteroParList.PL (U3 (G sg)) (CreateInfoListArgs14ToGArgs3 ss) ->
		IO a) -> IO a
createGs d@(Device.D dvc) ((Cache.cToMiddle <$>) -> mc) cis
	macc@(AllocationCallbacks.toMiddle -> macd) f = bracket
	(createInfoListToMiddle d cis >>= \cis' -> M.createGs dvc mc
			(T.createInfoListToMiddle cis')
			(AllocationCallbacks.toMiddle macc)
		<* destroyShaderStages d cis' cis)
	(\gs -> M.destroyGs dvc gs macd) (f . v2g)

recreateGs :: (
	CreateInfoListToMiddle ss,
	M.CreateInfoListToCore (T.CreateInfoListArgs (MiddleVars ss)),
	T.CreateInfoListToMiddle (MiddleVars ss),
	U2g (CreateInfoListArgs14ToGArgs3 ss),
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> Maybe (Cache.C s) -> HeteroParList.PL (U14 CreateInfo) ss ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	HeteroParList.PL (U3 (G sg)) (CreateInfoListArgs14ToGArgs3 ss) -> IO ()
recreateGs d@(Device.D dvc) ((Cache.cToMiddle <$>) -> mc) cis macc gpls = do
	cis' <- createInfoListToMiddle d cis
	M.recreateGs dvc mc
		(T.createInfoListToMiddle cis')
		(AllocationCallbacks.toMiddle macc) $ g2v gpls
	destroyShaderStages d cis' cis
