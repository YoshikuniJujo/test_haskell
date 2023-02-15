{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics (
	G, createGs, recreateGs, CreateInfo(..) ) where

import GHC.TypeNats
import Foreign.Storable.PeekPoke
import Control.Exception
import Data.Kind
import Data.TypeLevel
import Data.HeteroList
import Data.Word
import Data.Int

import Shaderc.EnumAuto

import Gpu.Vulkan.Pipeline.Enum
import Gpu.Vulkan.Pipeline.Graphics.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Pipeline.Graphics.Middle as M
import qualified Gpu.Vulkan.Pipeline.Graphics.Tmp as T
import qualified Gpu.Vulkan.Pipeline.Cache.Type as Cache
import qualified Gpu.Vulkan.Pipeline.Layout.Type as Layout

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

data CreateInfo n nnskndscdvss nvsts n3 n4 n5 n6 n7 n8 n9 n10 slsbtss sr sbvsts' =
	CreateInfo {
		createInfoNext :: Maybe n,
		createInfoFlags :: CreateFlags,
		createInfoStages ::
			HeteroVarList (V6 ShaderStage.CreateInfoNew) nnskndscdvss,
		createInfoVertexInputState ::
			Maybe (V3 VertexInputState.CreateInfo nvsts),
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
		createInfoLayout :: V3 Layout.L slsbtss,
		createInfoRenderPass :: RenderPass.R sr,
		createInfoSubpass :: Word32,
		createInfoBasePipelineHandle :: Maybe (V3 G sbvsts'),
		createInfoBasePipelineIndex :: Int32 }

createInfoToMiddle :: (ShaderStage.CreateInfoListToMiddleNew nnskndscdvss) =>
	Device.D sd ->
	CreateInfo n nnskndscdvss nvsts
		n3 n4 n5 n6 n7 n8 n9 n10 sl sr '(sb, vs', ts') ->
	IO (T.CreateInfo n (ShaderStage.MiddleVarsNew nnskndscdvss)
		nvsts n3 n4 n5 n6 n7 n8 n9 n10 '(vs', ts'))
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
	createInfoLayout = V3 (Layout.L lyt),
	createInfoRenderPass = RenderPass.R rp,
	createInfoSubpass = sp,
	createInfoBasePipelineHandle = bph,
	createInfoBasePipelineIndex = bpi } = do
	stgs' <- ShaderStage.createInfoListToMiddleNew dvc stgs
	bph' <- maybe M.gNull (\(V3 (G g)) -> pure g) bph
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
		T.createInfoBasePipelineHandle = V2 bph',
		T.createInfoBasePipelineIndex = bpi }

class CreateInfoListToMiddle ss where
	type MiddleVars ss :: [
		(Type, [(Type, ShaderKind, [Type])], (Type, [Type], [(Nat, Type)]),
		Type, Type, Type, Type, Type, Type, Type, Type, ([Type], [(Nat, Type)]))]

	createInfoListToMiddle :: Device.D sd ->
		HeteroVarList (V14 CreateInfo) ss ->
		IO (HeteroVarList (V12 T.CreateInfo) (MiddleVars ss))

	destroyShaderStages :: Device.D sd ->
		HeteroVarList (V12 T.CreateInfo) (MiddleVars ss) ->
		HeteroVarList (V14 CreateInfo) ss -> IO ()

instance CreateInfoListToMiddle '[] where
	type MiddleVars '[] = '[]
	createInfoListToMiddle _ HVNil = pure HVNil
	destroyShaderStages _ HVNil HVNil = pure ()

instance (
	ShaderStage.CreateInfoListToMiddleNew nnskndscdvss,
	CreateInfoListToMiddle ss ) =>
	CreateInfoListToMiddle ('(
		n, nnskndscdvss, nvsts, n3, n4, n5, n6, n7, n8, n9, n10,
		'(sl, sbtss, pcl), sr, '(sb, vs', ts') ) ': ss)  where
	type MiddleVars ('(
		n, nnskndscdvss, nvsts, n3, n4, n5, n6, n7, n8, n9, n10,
		'(sl, sbtss, pcl), sr, '(sb, vs', ts') ) ': ss) =
		'(n, ShaderStage.MiddleVarsNew nnskndscdvss, nvsts, n3, n4, n5, n6,
			n7, n8, n9, n10, '(vs', ts')) ': MiddleVars ss
	createInfoListToMiddle dvc (V14 ci :...: cis) = (:...:)
		<$> (V12 <$> createInfoToMiddle dvc ci)
		<*> createInfoListToMiddle dvc cis
	destroyShaderStages dvc (V12 cim :...: cims) (V14 ci :...: cis) = do
		ShaderStage.destroyCreateInfoMiddleListNew dvc (T.createInfoStages cim) (createInfoStages ci)
		destroyShaderStages dvc cims cis

class V2g ss where
	v2g :: HeteroVarList (V2 M.G) ss -> HeteroVarList (V2 (G sg)) ss
	g2v :: HeteroVarList (V2 (G sg)) ss -> HeteroVarList (V2 M.G) ss

instance V2g '[] where v2g HVNil = HVNil; g2v HVNil = HVNil

instance V2g ss => V2g (s ': ss) where
	v2g (V2 g :...: gs) = V2 (G g) :...: v2g gs
	g2v (V2 (G g) :...: gs) = V2 g :...: g2v gs

createGs :: (
	M.CreateInfoListToCore (T.CreateInfoListArgs (MiddleVars ss)),
	T.CreateInfoListToMiddle (MiddleVars ss),
	Pokable c, Pokable d,
	CreateInfoListToMiddle ss,
	M.GListFromCore (T.GListVars (MiddleVars ss)),
	V2g (T.GListVars (MiddleVars ss)) ) =>
	Device.D sd -> Maybe (Cache.C sc) ->
	HeteroVarList (V14 CreateInfo) ss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall sg . HeteroVarList (V2 (G sg)) (T.GListVars (MiddleVars ss)) ->
		IO a) -> IO a
createGs d@(Device.D dvc) ((Cache.cToMiddle <$>) -> mc) cis macc macd f = bracket
	(createInfoListToMiddle d cis >>= \cis' ->
		T.createGs dvc mc cis' macc <* destroyShaderStages d cis' cis)
	(\gs -> M.destroyGs dvc gs macd) (f . v2g)

recreateGs :: (
	CreateInfoListToMiddle ss,
	M.CreateInfoListToCore (T.CreateInfoListArgs (MiddleVars ss)),
	T.CreateInfoListToMiddle (MiddleVars ss),
	Pokable c, Pokable d,
	M.GListFromCore (T.GListVars (MiddleVars ss)),
	V2g (T.GListVars (MiddleVars ss))) =>
	Device.D sd -> Maybe (Cache.C s) -> HeteroVarList (V14 CreateInfo) ss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	HeteroVarList (V2 (G sg)) (T.GListVars (MiddleVars ss)) -> IO ()
recreateGs d@(Device.D dvc) ((Cache.cToMiddle <$>) -> mc) cis macc macd gpls = do
	cis' <- createInfoListToMiddle d cis
	T.recreateGs dvc mc cis' macc macd $ g2v gpls
	destroyShaderStages d cis' cis
