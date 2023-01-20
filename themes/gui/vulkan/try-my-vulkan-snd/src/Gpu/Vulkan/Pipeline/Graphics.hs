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
	G,

	CreateInfo(..),

	createGsOld, recreateGsOld, CreateInfoOld(..)
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

data CreateInfoOld n nnskndscdvss nvsts n3 n4 n5 n6 n7 n8 n9 n10 slsbtss sr sbvsts' =
	CreateInfoOld {
		createInfoNextOld :: Maybe n,
		createInfoFlagsOld :: CreateFlags,
		createInfoStagesOld ::
			HeteroVarList ShaderStage.CreateInfo' nnskndscdvss,
		createInfoVertexInputStateOld ::
			Maybe (V3 VertexInputState.CreateInfo nvsts),
		createInfoInputAssemblyStateOld ::
			Maybe (InputAssemblyState.CreateInfo n3),
		createInfoTessellationStateOld ::
			Maybe (TessellationState.CreateInfo n4),
		createInfoViewportStateOld :: Maybe (ViewportState.CreateInfo n5),
		createInfoRasterizationStateOld ::
			Maybe (RasterizationState.CreateInfo n6),
		createInfoMultisampleStateOld ::
			Maybe (MultisampleState.CreateInfo n7),
		createInfoDepthStencilStateOld ::
			Maybe (DepthStencilState.CreateInfo n8),
		createInfoColorBlendStateOld ::
			Maybe (ColorBlendState.CreateInfo n9),
		createInfoDynamicStateOld :: Maybe (DynamicState.CreateInfo n10),
		createInfoLayoutOld :: V3 Layout.L slsbtss,
		createInfoRenderPassOld :: RenderPass.R sr,
		createInfoSubpassOld :: Word32,
		createInfoBasePipelineHandleOld :: Maybe (V3 G sbvsts'),
		createInfoBasePipelineIndexOld :: Int32 }

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

createInfoToMiddleOld :: (ShaderStage.CreateInfoListToMiddle' nnskndscdvss) =>
	Device.D sd ->
	CreateInfoOld n nnskndscdvss nvsts
		n3 n4 n5 n6 n7 n8 n9 n10 sl sr '(sb, vs', ts') ->
	IO (T.CreateInfoOld n (ShaderStage.MiddleVars nnskndscdvss)
		nvsts n3 n4 n5 n6 n7 n8 n9 n10 '(vs', ts'))
createInfoToMiddleOld dvc CreateInfoOld {
	createInfoNextOld = mnxt,
	createInfoFlagsOld = flgs,
	createInfoStagesOld = stgs,
	createInfoVertexInputStateOld = vis,
	createInfoInputAssemblyStateOld = ias,
	createInfoTessellationStateOld = ts,
	createInfoViewportStateOld = vs,
	createInfoRasterizationStateOld = rs,
	createInfoMultisampleStateOld = ms,
	createInfoDepthStencilStateOld = dss,
	createInfoColorBlendStateOld = cbs,
	createInfoDynamicStateOld = ds,
	createInfoLayoutOld = V3 (Layout.L lyt),
	createInfoRenderPassOld = RenderPass.R rp,
	createInfoSubpassOld = sp,
	createInfoBasePipelineHandleOld = bph,
	createInfoBasePipelineIndexOld = bpi } = do
	stgs' <- ShaderStage.createInfoListToMiddle' dvc stgs
	bph' <- maybe M.gNull (\(V3 (G g)) -> pure g) bph
	pure T.CreateInfoOld {
		T.createInfoNextOld = mnxt,
		T.createInfoFlagsOld = flgs,
		T.createInfoStagesOld = stgs',
		T.createInfoVertexInputStateOld = vis,
		T.createInfoInputAssemblyStateOld = ias,
		T.createInfoTessellationStateOld = ts,
		T.createInfoViewportStateOld = vs,
		T.createInfoRasterizationStateOld = rs,
		T.createInfoMultisampleStateOld = ms,
		T.createInfoDepthStencilStateOld = dss,
		T.createInfoColorBlendStateOld = cbs,
		T.createInfoDynamicStateOld = ds,
		T.createInfoLayoutOld = lyt,
		T.createInfoRenderPassOld = rp,
		T.createInfoSubpassOld = sp,
		T.createInfoBasePipelineHandleOld = V2 bph',
		T.createInfoBasePipelineIndexOld = bpi }

{-
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
		-}

class CreateInfoListToMiddleOld ss where
	type MiddleVarsOld ss :: [
		(Type, [(Type, ShaderKind, Type)], (Type, [Type], [(Nat, Type)]),
		Type, Type, Type, Type, Type, Type, Type, Type, ([Type], [(Nat, Type)]))]

	createInfoListToMiddleOld :: Device.D sd ->
		HeteroVarList (V14 CreateInfoOld) ss ->
		IO (HeteroVarList (V12 T.CreateInfoOld) (MiddleVarsOld ss))

	destroyShaderStagesOld :: Device.D sd ->
		HeteroVarList (V12 T.CreateInfoOld) (MiddleVarsOld ss) ->
		HeteroVarList (V14 CreateInfoOld) ss -> IO ()

instance CreateInfoListToMiddleOld '[] where
	type MiddleVarsOld '[] = '[]
	createInfoListToMiddleOld _ HVNil = pure HVNil
	destroyShaderStagesOld _ HVNil HVNil = pure ()

instance (
	ShaderStage.CreateInfoListToMiddle' nnskndscdvss,
	CreateInfoListToMiddleOld ss ) =>
	CreateInfoListToMiddleOld ('(
		n, nnskndscdvss, nvsts, n3, n4, n5, n6, n7, n8, n9, n10,
		'(sl, sbtss, pcl), sr, '(sb, vs', ts') ) ': ss)  where
	type MiddleVarsOld ('(
		n, nnskndscdvss, nvsts, n3, n4, n5, n6, n7, n8, n9, n10,
		'(sl, sbtss, pcl), sr, '(sb, vs', ts') ) ': ss) =
		'(n, ShaderStage.MiddleVars nnskndscdvss, nvsts, n3, n4, n5, n6,
			n7, n8, n9, n10, '(vs', ts')) ': MiddleVarsOld ss
	createInfoListToMiddleOld dvc (V14 ci :...: cis) = (:...:)
		<$> (V12 <$> createInfoToMiddleOld dvc ci)
		<*> createInfoListToMiddleOld dvc cis
	destroyShaderStagesOld dvc (V12 cim :...: cims) (V14 ci :...: cis) = do
		ShaderStage.destroyCreateInfoMiddleList' dvc (T.createInfoStagesOld cim) (createInfoStagesOld ci)
		destroyShaderStagesOld dvc cims cis

class V2g ss where
	v2g :: HeteroVarList (V2 M.G) ss -> HeteroVarList (V2 (G sg)) ss
	g2v :: HeteroVarList (V2 (G sg)) ss -> HeteroVarList (V2 M.G) ss

instance V2g '[] where v2g HVNil = HVNil; g2v HVNil = HVNil

instance V2g ss => V2g (s ': ss) where
	v2g (V2 g :...: gs) = V2 (G g) :...: v2g gs
	g2v (V2 (G g) :...: gs) = V2 g :...: g2v gs

createGsOld :: (
	M.CreateInfoListToCore (T.CreateInfoListArgsOld (MiddleVarsOld ss)),
	T.CreateInfoListToMiddleOld (MiddleVarsOld ss),
	Pointable c, Pointable d,
	CreateInfoListToMiddleOld ss,
	M.GListFromCore (T.GListVarsOld (MiddleVarsOld ss)),
	V2g (T.GListVarsOld (MiddleVarsOld ss)) ) =>
	Device.D sd -> Maybe (Cache.C sc) ->
	HeteroVarList (V14 CreateInfoOld) ss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall sg . HeteroVarList (V2 (G sg)) (T.GListVarsOld (MiddleVarsOld ss)) ->
		IO a) -> IO a
createGsOld d@(Device.D dvc) ((Cache.cToMiddle <$>) -> mc) cis macc macd f = bracket
	(createInfoListToMiddleOld d cis >>= \cis' ->
		T.createGsOld dvc mc cis' macc <* destroyShaderStagesOld d cis' cis)
	(\gs -> M.destroyGs dvc gs macd) (f . v2g)

recreateGsOld d@(Device.D dvc) ((Cache.cToMiddle <$>) -> mc) cis macc macd gpls = do
	cis' <- createInfoListToMiddleOld d cis
	T.recreateGsOld dvc mc cis' macc macd $ g2v gpls
	destroyShaderStagesOld d cis' cis
