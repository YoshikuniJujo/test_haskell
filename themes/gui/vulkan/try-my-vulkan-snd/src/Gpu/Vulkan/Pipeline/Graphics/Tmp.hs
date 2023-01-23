{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Tmp (
	createGs, recreateGs,
	CreateInfo(..), CreateInfoListToMiddle(..), GListVars ) where

import Gpu.Vulkan.Pipeline.Graphics.Middle qualified as M

import GHC.TypeNats
import Foreign.Storable.PeekPoke
import Foreign.Pointable
import Data.Kind
import Data.HeteroList
import Data.Word
import Data.Int

import Shaderc.EnumAuto

import Gpu.Vulkan.Pipeline.Enum

import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle.Internal as ShaderStage
import qualified Gpu.Vulkan.Pipeline.VertexInputState as VertexInputState
import Gpu.Vulkan.Pipeline.InputAssemblyState.Middle.Internal
	qualified as InputAssemblyState
import Gpu.Vulkan.Pipeline.TessellationState.Middle.Internal
	qualified as TessellationState
import qualified Gpu.Vulkan.Pipeline.ViewportState.Middle as ViewportState
import qualified Gpu.Vulkan.Pipeline.RasterizationState.Middle.Internal as RasterizationState
import qualified Gpu.Vulkan.Pipeline.MultisampleState.Middle.Internal as MultisampleState
import qualified Gpu.Vulkan.Pipeline.DepthStencilState.Middle.Internal
	as DepthStencilState
import qualified Gpu.Vulkan.Pipeline.ColorBlendState.Middle.Internal
	as ColorBlendState
import qualified Gpu.Vulkan.Pipeline.DynamicState.Middle.Internal
	as DynamicState
import qualified Gpu.Vulkan.Pipeline.Layout.Middle.Internal as Layout
import qualified Gpu.Vulkan.RenderPass.Middle as RenderPass
import qualified Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList
	as BindingStrideList
import qualified Gpu.Vulkan.VertexInput as VertexInput

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Pipeline.Cache.Middle.Internal as Cache

data CreateInfo n nskndvss nvsts n3 n4 n5 n6 n7 n8 n9 n10 vsts' = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStages :: HeteroVarList (V3 ShaderStage.CreateInfoNew) nskndvss,
	createInfoVertexInputState ::
		Maybe (V3 VertexInputState.CreateInfo nvsts),
	createInfoInputAssemblyState ::
		Maybe (InputAssemblyState.CreateInfo n3),
	createInfoTessellationState :: Maybe (TessellationState.CreateInfo n4),
	createInfoViewportState :: Maybe (ViewportState.CreateInfo n5),
	createInfoRasterizationState ::
		Maybe (RasterizationState.CreateInfo n6),
	createInfoMultisampleState :: Maybe (MultisampleState.CreateInfo n7),
	createInfoDepthStencilState :: Maybe (DepthStencilState.CreateInfo n8),
	createInfoColorBlendState :: Maybe (ColorBlendState.CreateInfo n9),
	createInfoDynamicState :: Maybe (DynamicState.CreateInfo n10),
	createInfoLayout :: Layout.L,
	createInfoRenderPass :: RenderPass.R,
	createInfoSubpass :: Word32,
	createInfoBasePipelineHandle :: V2 M.G vsts',
	createInfoBasePipelineIndex :: Int32 }

createInfoToMiddle :: (
	BindingStrideList.BindingStrideList vs VertexInput.Rate VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs ts ) =>
	CreateInfo n nskndvss '(nv, vs, ts) n3 n4 n5 n6 n7 n8 n9 n10 vsts' ->
	M.CreateInfoNew n nskndvss nv n3 n4 n5 n6 n7 n8 n9 n10 vsts'
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoStages = stg,
	createInfoVertexInputState = vis,
	createInfoInputAssemblyState = ias,
	createInfoTessellationState = ts,
	createInfoViewportState = vs,
	createInfoRasterizationState = rs,
	createInfoMultisampleState = ms,
	createInfoDepthStencilState = dss,
	createInfoColorBlendState = cbs,
	createInfoDynamicState = ds,
	createInfoLayout = lyt,
	createInfoRenderPass = rp,
	createInfoSubpass = sp,
	createInfoBasePipelineHandle = bph,
	createInfoBasePipelineIndex = bpi
	} = M.CreateInfoNew {
	M.createInfoNextNew = mnxt,
	M.createInfoFlagsNew = flgs,
	M.createInfoStagesNew = stg,
	M.createInfoVertexInputStateNew =
		VertexInputState.createInfoToMiddle . unV3 <$> vis,
	M.createInfoInputAssemblyStateNew = ias,
	M.createInfoTessellationStateNew = ts,
	M.createInfoViewportStateNew = vs,
	M.createInfoRasterizationStateNew = rs,
	M.createInfoMultisampleStateNew = ms,
	M.createInfoDepthStencilStateNew = dss,
	M.createInfoColorBlendStateNew = cbs,
	M.createInfoDynamicStateNew = ds,
	M.createInfoLayoutNew = lyt,
	M.createInfoRenderPassNew = rp,
	M.createInfoSubpassNew = sp,
	M.createInfoBasePipelineHandleNew = bph,
	M.createInfoBasePipelineIndexNew = bpi }

class CreateInfoListToMiddle sss where
	type CreateInfoListArgs sss ::
		[(*, [(*, ShaderKind, [*])], *, *, *, *, *, *, *, *, *, ([*], [(Nat, *)]))]
	createInfoListToMiddle ::
		HeteroVarList (V12 CreateInfo) sss ->
		HeteroVarList (V12 M.CreateInfoNew) (CreateInfoListArgs sss)

instance CreateInfoListToMiddle '[] where
	type CreateInfoListArgs '[] = '[]
	createInfoListToMiddle _ = HVNil

instance (
	BindingStrideList.BindingStrideList vs VertexInput.Rate VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs ts,
	CreateInfoListToMiddle ss ) =>
	CreateInfoListToMiddle ('(
		n, nskndvss, '(n2, vs, ts), n3, n4, n5, n6, n7, n8, n9, n10, vsts' ) ': ss) where
	type CreateInfoListArgs ('(
		n, nskndvss, '(n2, vs, ts), n3, n4, n5, n6, n7, n8, n9, n10, vsts' ) ': ss) = '(
		n, nskndvss, n2, n3, n4, n5, n6, n7, n8, n9, n10, vsts') : CreateInfoListArgs ss
	createInfoListToMiddle (V12 ci :...: cis) =
		V12 (createInfoToMiddle ci) :...: createInfoListToMiddle cis

createGs :: (
	Pokable n', M.GListFromCore (GListVars ss),
	M.CreateInfoListToCoreNew (CreateInfoListArgs ss),
	CreateInfoListToMiddle ss
	) =>
	Device.D -> Maybe Cache.C ->
	HeteroVarList (V12 CreateInfo) ss ->
	Maybe (AllocationCallbacks.A n') -> IO (HeteroVarList (V2 M.G) (GListVars ss))
createGs dvc mc cis mac = M.createGsNew dvc mc (createInfoListToMiddle cis) mac

recreateGs :: (
	M.CreateInfoListToCoreNew (CreateInfoListArgs ss),
	CreateInfoListToMiddle ss,
	Pokable c, Pokable d,
	M.GListFromCore (GListVars ss) ) => Device.D -> Maybe Cache.C ->
	HeteroVarList (V12 CreateInfo) ss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	HeteroVarList (V2 M.G) (GListVars ss) -> IO ()
recreateGs dvc mc cis macc macd gs =
	M.recreateGsNew dvc mc (createInfoListToMiddle cis) macc macd gs

type family GListVars (ss :: [(
		Type, [(Type, ShaderKind, [Type])],
		(Type, [Type], [(Nat, Type)]), Type, Type, Type, Type, Type, Type, Type,
		Type, ([Type], [(Nat, Type)]))]) :: [([Type], [(Nat, Type)])] where
	GListVars '[] = '[]
	GListVars ('(
		n, nskndvss, '(n2, vs, ts),
		n3, n4, n5, n6, n7, n8, n9, n10, vsts' ) ': ss) = '(vs, ts) ': GListVars ss
