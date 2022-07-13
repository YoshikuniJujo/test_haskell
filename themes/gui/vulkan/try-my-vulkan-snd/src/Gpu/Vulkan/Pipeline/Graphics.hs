{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics (
	G, createGs, CreateInfoList(..), CreateInfo(..), createInfoToMiddle,
	CreateInfo'(..), createInfoToMiddle',
	GList, pattern GNil, pattern GCons ) where

import Foreign.Pointable
import Control.Exception
import Data.HeteroList
import Data.Word
import Data.Int

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

data CreateInfo n n1 n1' sknds a a' vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10 sl sr sb
	vs'' ts' = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStages :: ShaderStage.CreateInfoList n1 n1' sknds a a' vss,
	createInfoVertexInputState ::
		Maybe (VertexInputState.CreateInfo n2 vs' ts),
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
	createInfoLayout :: Layout.L sl,
	createInfoRenderPass :: RenderPass.R sr,
	createInfoSubpass :: Word32,
	createInfoBasePipelineHandle :: Maybe (G sb vs'' ts'),
	createInfoBasePipelineIndex :: Int32 }

data CreateInfo' n nnskndscdvss n2 vs ts n3 n4 n5 n6 n7 n8 n9 n10 sl sr sb
	vs' ts' = CreateInfo' {
	createInfoNext' :: Maybe n,
	createInfoFlags' :: CreateFlags,
	createInfoStages' :: HeteroVarList ShaderStage.CreateInfo' nnskndscdvss,
	createInfoVertexInputState' ::
		Maybe (VertexInputState.CreateInfo n2 vs ts),
	createInfoInputAssemblyState' ::
		Maybe (InputAssemblyState.CreateInfo n3),
	createInfoTessellationState' ::
		Maybe (TessellationState.CreateInfo n4),
	createInfoViewportState' :: Maybe (ViewportState.CreateInfo n5),
	createInfoRasterizationState' ::
		Maybe (RasterizationState.CreateInfo n6),
	createInfoMultisampleState' ::
		Maybe (MultisampleState.CreateInfo n7),
	createInfoDepthStencilState' ::
		Maybe (DepthStencilState.CreateInfo n8),
	createInfoColorBlendState' ::
		Maybe (ColorBlendState.CreateInfo n9),
	createInfoDynamicState' :: Maybe (DynamicState.CreateInfo n10),
	createInfoLayout' :: Layout.L sl,
	createInfoRenderPass' :: RenderPass.R sr,
	createInfoSubpass' :: Word32,
	createInfoBasePipelineHandle' :: Maybe (G sb vs' ts'),
	createInfoBasePipelineIndex' :: Int32 }

{-
deriving instance (
	Show n, Show n2, Show n3, Show n4, Show n5, Show n6, Show n7, Show n8,
	Show n9, Show n10,
	Show (ShaderStage.M.CreateInfoList n1 sknds vss)
	) =>
	Show (CreateInfo
		n n1 sknds vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10
		sl sr sb vs'' ts')
		-}

createInfoToMiddle :: (Pointable n1', Pointable a) =>
	Device.D sd ->
	CreateInfo
		n n1 n1' sknds a a' vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10
		sl sr sb vs'' ts' ->
	IO (M.CreateInfo
		n n1 sknds vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10 vs'' ts')
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
	createInfoLayout = Layout.L lyt,
	createInfoRenderPass = RenderPass.R rp,
	createInfoSubpass = sp,
	createInfoBasePipelineHandle = maybe M.GNull (\(G g) -> g) ->  bph,
	createInfoBasePipelineIndex = bpi } = do
	stgs' <- ShaderStage.createInfoListToMiddle dvc stgs
	pure M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoStages = stgs',
		M.createInfoVertexInputState = vis,
		M.createInfoInputAssemblyState = ias,
		M.createInfoTessellationState = ts,
		M.createInfoViewportState = vs,
		M.createInfoRasterizationState = rs,
		M.createInfoMultisampleState = ms,
		M.createInfoDepthStencilState = dss,
		M.createInfoColorBlendState = cbs,
		M.createInfoDynamicState = ds,
		M.createInfoLayout = lyt,
		M.createInfoRenderPass = rp,
		M.createInfoSubpass = sp,
		M.createInfoBasePipelineHandle = bph,
		M.createInfoBasePipelineIndex = bpi }

createInfoToMiddle' :: (ShaderStage.CreateInfoListToMiddle' nnskndscdvss) =>
	Device.D sd ->
	CreateInfo' n nnskndscdvss n2 vs ts
		n3 n4 n5 n6 n7 n8 n9 n10 sl sr sb vs' ts' ->
	IO (M.CreateInfo' n (ShaderStage.MiddleVars nnskndscdvss)
		n2 vs ts n3 n4 n5 n6 n7 n8 n9 n10 vs' ts')
createInfoToMiddle' dvc CreateInfo' {
	createInfoNext' = mnxt,
	createInfoFlags' = flgs,
	createInfoStages' = stgs,
	createInfoVertexInputState' = vis,
	createInfoInputAssemblyState' = ias,
	createInfoTessellationState' = ts,
	createInfoViewportState' = vs,
	createInfoRasterizationState' = rs,
	createInfoMultisampleState' = ms,
	createInfoDepthStencilState' = dss,
	createInfoColorBlendState' = cbs,
	createInfoDynamicState' = ds,
	createInfoLayout' = Layout.L lyt,
	createInfoRenderPass' = RenderPass.R rp,
	createInfoSubpass' = sp,
	createInfoBasePipelineHandle' = maybe M.GNull (\(G g) -> g) ->  bph,
	createInfoBasePipelineIndex' = bpi } = do
	stgs' <- ShaderStage.createInfoListToMiddle' dvc stgs
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
		M.createInfoBasePipelineHandle' = bph,
		M.createInfoBasePipelineIndex' = bpi }

data CreateInfoList
	ns n1s n1's skndss as a's vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s sls srs sbs vs''s ts's where
	CreateInfoNil :: CreateInfoList
		'[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[]
	CreateInfoCons ::
		CreateInfo n n1 n1' sknds a a' vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10 sl sr sb vs'' ts' ->
		CreateInfoList ns n1s n1's skndss as a's vsss
			n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s sls srs sbs vs''s ts's ->
		CreateInfoList (n ': ns)
			(n1 ': n1s) (n1' ': n1's) (sknds ': skndss) (a ': as) (a' ': a's) (vss ': vsss)
			(n2 ': n2s) (vs' ': vs's) (ts ': tss) (n3 ': n3s)
			(n4 ': n4s) (n5 ': n5s) (n6 ': n6s) (n7 ': n7s)
			(n8 ': n8s) (n9 ': n9s) (n10 ': n10s)
			(sl ': sls) (sr ': srs) (sb ': sbs)
			(vs'' ': vs''s) (ts' ': ts's)

class CreateInfoListToMiddle
	ns n1s n1's sknds as a's vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s
	n10s sls srs sbs vs''s ts's where
	createInfoListToMiddle :: Device.D sd ->
		CreateInfoList
			ns n1s n1's sknds as a's vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
			sls srs sbs vs''s ts's ->
		IO (M.CreateInfoList
			ns n1s sknds vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
			vs''s ts's)

instance CreateInfoListToMiddle
	'[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[]
	'[] '[] '[] '[] '[]
	where
	createInfoListToMiddle _ CreateInfoNil = pure M.CreateInfoNil

instance (
	Pointable n1', Pointable a,
	CreateInfoListToMiddle
		ns n1s n1's sknds as a's vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
		sls srs sbs vs''s ts's ) =>
	CreateInfoListToMiddle
		(n ': ns) (n1 ': n1s) (n1' ': n1's) (sknd ': sknds) (a ': as) (a' ': a's)
		(vss ': vsss) (n2 ': n2s) (vs' ': vs's) (ts ': tss) (n3 ': n3s) (n4 ': n4s)
		(n5 ': n5s) (n6 ': n6s) (n7 ': n7s) (n8 ': n8s) (n9 ': n9s) (n10 ': n10s)
		(sl ': sls) (sr ': srs) (sb ': sbs) (vs'' ': vs''s) (ts' ': ts's) where
	createInfoListToMiddle dvc (ci `CreateInfoCons` cis) = M.CreateInfoCons
		<$> createInfoToMiddle dvc ci
		<*> createInfoListToMiddle dvc cis

createGs :: (
	CreateInfoListToMiddle
		ns n1s n1's skndss as a's vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s
		n10s sls srs sbs vs''s ts's,
	DestroyShaderStages
		ns n1s n1's skndss as a's vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s
		n10s sls srs sbs vs''s ts's,
	M.CreateInfoListToCore
		ns n1s skndss vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
		vs''s ts's,
	M.PListFromCore vs's tss, Pointable n', Pointable n'' ) =>
	Device.D sd -> Maybe (Cache.C sc) ->
	CreateInfoList
		ns n1s n1's skndss as a's vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
		sls srs sbs vs''s ts's ->
	Maybe (AllocationCallbacks.A n') -> Maybe (AllocationCallbacks.A n'') ->
	(forall s . GList s vs's tss -> IO a) -> IO a
createGs d@(Device.D dvc) ((Cache.cToMiddle <$>) -> mc) cis macc macd f = bracket
	(createInfoListToMiddle d cis >>= \cis' -> M.create dvc mc cis' macc <* destroyShaderStages d cis' cis)
	(\gs -> M.destroyGs dvc gs macd) (f . GList)

class DestroyShaderStages
	ns n1s n1's skndss as a's vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
	sls srs sbs vs''s ts's where
	destroyShaderStages :: Device.D sd ->
		M.CreateInfoList
			ns n1s skndss vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
			vs''s ts's ->
		CreateInfoList
			ns n1s n1's skndss as a's vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
			sls srs sbs vs''s ts's -> IO ()

instance DestroyShaderStages '[] '[] '[] '[] '[] '[] '[]
	'[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] '[] where
	destroyShaderStages _ M.CreateInfoNil CreateInfoNil = pure ()

instance (
	Pointable a',
	DestroyShaderStages
		ns n1s n1's skndss as a's vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
		sls srs sbs vs''s ts's ) =>
	DestroyShaderStages
		(n ': ns) (n1 ': n1s) (n1' ': n1's) (sknds ': skndss) (a ': as) (a' ': a's)
		(vss ': vsss) (n2 ': n2s) (vs' ': vs's) (ts ': tss) (n3 ': n3s) (n4 ': n4s)
		(n5 ': n5s) (n6 ': n6s) (n7 ': n7s) (n8 ': n8s) (n9 ': n9s) (n10 ': n10s)
		(sl ': sls) (sr ': srs) (sb ': sbs) (vs'' ': vs''s) (ts' ': ts's) where
	destroyShaderStages dvc (mci `M.CreateInfoCons` mcis) (ci `CreateInfoCons` cis) = do
		ShaderStage.destroyCreateInfoMiddleList dvc (M.createInfoStages mci) (createInfoStages ci)
		destroyShaderStages dvc mcis cis
