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
	G, createGsNew, recreateGsNew, CreateInfoNew(..)
	) where

import GHC.TypeNats
import Foreign.Pointable
import Control.Exception
import Data.Kind
import Data.HeteroList
import Data.Word
import Data.Int

import Shaderc.EnumAuto

import Gpu.Vulkan.Pipeline.Enum
import Gpu.Vulkan.Pipeline.Graphics.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Pipeline.Graphics.Middle as M
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
import qualified Gpu.Vulkan.Pipeline.ShaderStage as ShaderStage

data CreateInfo n nnskndscdvss nvsts n3 n4 n5 n6 n7 n8 n9 n10 slsbtss sr sbvsts' =
	CreateInfo {
		createInfoNext :: Maybe n,
		createInfoFlags :: CreateFlags,
		createInfoStages ::
			HeteroVarList ShaderStage.CreateInfo' nnskndscdvss,
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

data CreateInfoNew n nnskndscdvss nvsts n3 n4 n5 n6 n7 n8 n9 n10 slsbtss sr sbvsts' =
	CreateInfoNew {
		createInfoNextNew :: Maybe n,
		createInfoFlagsNew :: CreateFlags,
		createInfoStagesNew ::
			HeteroVarList ShaderStage.CreateInfo' nnskndscdvss,
		createInfoVertexInputStateNew ::
			Maybe (V3 VertexInputState.CreateInfo nvsts),
		createInfoInputAssemblyStateNew ::
			Maybe (InputAssemblyState.CreateInfo n3),
		createInfoTessellationStateNew ::
			Maybe (TessellationState.CreateInfo n4),
		createInfoViewportStateNew :: Maybe (ViewportState.CreateInfo n5),
		createInfoRasterizationStateNew ::
			Maybe (RasterizationState.CreateInfo n6),
		createInfoMultisampleStateNew ::
			Maybe (MultisampleState.CreateInfo n7),
		createInfoDepthStencilStateNew ::
			Maybe (DepthStencilState.CreateInfo n8),
		createInfoColorBlendStateNew ::
			Maybe (ColorBlendState.CreateInfo n9),
		createInfoDynamicStateNew :: Maybe (DynamicState.CreateInfo n10),
		createInfoLayoutNew :: V3 Layout.L slsbtss,
		createInfoRenderPassNew :: RenderPass.R sr,
		createInfoSubpassNew :: Word32,
		createInfoBasePipelineHandleNew :: Maybe (V3 G sbvsts'),
		createInfoBasePipelineIndexNew :: Int32 }

createInfoFromNew ::
	CreateInfoNew n nnskndscdvss
		nvsts n3 n4 n5 n6 n7 n8 n9 n10 '(sl, sbtss, pcl) sr sbvsts' ->
	CreateInfo n nnskndscdvss
		nvsts n3 n4 n5 n6 n7 n8 n9 n10 '(sl, sbtss, pcl) sr sbvsts'
createInfoFromNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoStagesNew = stg,
	createInfoVertexInputStateNew = vist,
	createInfoInputAssemblyStateNew = iast,
	createInfoTessellationStateNew = tssst,
	createInfoViewportStateNew = vpst,
	createInfoRasterizationStateNew = rsst,
	createInfoMultisampleStateNew = msst,
	createInfoDepthStencilStateNew = stst,
	createInfoColorBlendStateNew = blst,
	createInfoDynamicStateNew = dsst,
	createInfoLayoutNew = lytm,
	createInfoRenderPassNew = rp,
	createInfoSubpassNew = sp,
	createInfoBasePipelineHandleNew = bpplh,
	createInfoBasePipelineIndexNew = bppli } = CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStages = stg,
	createInfoVertexInputState = vist,
	createInfoInputAssemblyState = iast,
	createInfoTessellationState = tssst,
	createInfoViewportState = vpst,
	createInfoRasterizationState = rsst,
	createInfoMultisampleState = msst,
	createInfoDepthStencilState = stst,
	createInfoColorBlendState = blst,
	createInfoDynamicState = dsst,
	createInfoLayout = lytm,
	createInfoRenderPass = rp,
	createInfoSubpass = sp,
	createInfoBasePipelineHandle = bpplh,
	createInfoBasePipelineIndex = bppli }

createInfoToMiddle :: (ShaderStage.CreateInfoListToMiddle' nnskndscdvss) =>
	Device.D sd ->
	CreateInfo n nnskndscdvss nvsts
		n3 n4 n5 n6 n7 n8 n9 n10 sl sr '(sb, vs', ts') ->
	IO (M.CreateInfo' n (ShaderStage.MiddleVars nnskndscdvss)
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
	stgs' <- ShaderStage.createInfoListToMiddle' dvc stgs
	bph' <- maybe M.gNull (\(V3 (G g)) -> pure g) bph
	pure M.CreateInfo' {
		M.createInfoNext' = mnxt,
		M.createInfoFlags' = flgs,
		M.createInfoStages' = stgs',
		M.createInfoVertexInputState' = vis,
		M.createInfoInputAssemblyState' = ias,
		M.createInfoTessellationState' = ts,
		M.createInfoViewportState' = vs,
		M.createInfoRasterizationState' = rs,
		M.createInfoMultisampleState' = ms,
		M.createInfoDepthStencilState' = dss,
		M.createInfoColorBlendState' = cbs,
		M.createInfoDynamicState' = ds,
		M.createInfoLayout' = lyt,
		M.createInfoRenderPass' = rp,
		M.createInfoSubpass' = sp,
		M.createInfoBasePipelineHandle' = V2 bph',
		M.createInfoBasePipelineIndex' = bpi }

class CreateInfoListToMiddleNew ss where
	type MiddleVarsNew ss :: [
		(Type, [(Type, ShaderKind, Type)], (Type, [Type], [(Nat, Type)]),
		Type, Type, Type, Type, Type, Type, Type, Type, ([Type], [(Nat, Type)]))]

	createInfoListToMiddleNew :: Device.D sd ->
		HeteroVarList (V14 CreateInfoNew) ss ->
		IO (HeteroVarList (V12 M.CreateInfo') (MiddleVarsNew ss))

	destroyShaderStagesNew :: Device.D sd ->
		HeteroVarList (V12 M.CreateInfo') (MiddleVarsNew ss) ->
		HeteroVarList (V14 CreateInfoNew) ss -> IO ()

instance CreateInfoListToMiddleNew '[] where
	type MiddleVarsNew '[] = '[]
	createInfoListToMiddleNew _ HVNil = pure HVNil
	destroyShaderStagesNew _ HVNil HVNil = pure ()

instance (
	ShaderStage.CreateInfoListToMiddle' nnskndscdvss,
	CreateInfoListToMiddleNew ss ) =>
	CreateInfoListToMiddleNew ('(
		n, nnskndscdvss, nvsts, n3, n4, n5, n6, n7, n8, n9, n10,
		'(sl, sbtss, pcl), sr, '(sb, vs', ts') ) ': ss)  where
	type MiddleVarsNew ('(
		n, nnskndscdvss, nvsts, n3, n4, n5, n6, n7, n8, n9, n10,
		'(sl, sbtss, pcl), sr, '(sb, vs', ts') ) ': ss) =
		'(n, ShaderStage.MiddleVars nnskndscdvss, nvsts, n3, n4, n5, n6,
			n7, n8, n9, n10, '(vs', ts')) ': MiddleVarsNew ss
	createInfoListToMiddleNew dvc (V14 ci :...: cis) = (:...:)
		<$> (V12 <$> createInfoToMiddle dvc (createInfoFromNew ci))
		<*> createInfoListToMiddleNew dvc cis
	destroyShaderStagesNew dvc (V12 cim :...: cims) (V14 ci :...: cis) = do
		ShaderStage.destroyCreateInfoMiddleList' dvc (M.createInfoStages' cim) (createInfoStagesNew ci)
		destroyShaderStagesNew dvc cims cis

class V2g ss where
	v2g :: HeteroVarList (V2 M.G) ss -> HeteroVarList (V2 (G sg)) ss
	g2v :: HeteroVarList (V2 (G sg)) ss -> HeteroVarList (V2 M.G) ss

instance V2g '[] where v2g HVNil = HVNil; g2v HVNil = HVNil

instance V2g ss => V2g (s ': ss) where
	v2g (V2 g :...: gs) = V2 (G g) :...: v2g gs
	g2v (V2 (G g) :...: gs) = V2 g :...: g2v gs

createGsNew :: (
	Pointable c, Pointable d,
	CreateInfoListToMiddleNew ss,
	M.CreateInfoListToCore' (MiddleVarsNew ss),
	M.GListFromCore (M.GListVars (MiddleVarsNew ss)),
	V2g (M.GListVars (MiddleVarsNew ss)) ) =>
	Device.D sd -> Maybe (Cache.C sc) ->
	HeteroVarList (V14 CreateInfoNew) ss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall sg . HeteroVarList (V2 (G sg)) (M.GListVars (MiddleVarsNew ss)) ->
		IO a) -> IO a
createGsNew d@(Device.D dvc) ((Cache.cToMiddle <$>) -> mc) cis macc macd f = bracket
	(createInfoListToMiddleNew d cis >>= \cis' ->
		M.createGs' dvc mc cis' macc <* destroyShaderStagesNew d cis' cis)
	(\gs -> M.destroyGs' dvc gs macd) (f . v2g)

recreateGsNew d@(Device.D dvc) ((Cache.cToMiddle <$>) -> mc) cis macc macd gpls = do
	cis' <- createInfoListToMiddleNew d cis
	M.recreateGs' dvc mc cis' macc macd $ g2v gpls
	destroyShaderStagesNew d cis' cis
