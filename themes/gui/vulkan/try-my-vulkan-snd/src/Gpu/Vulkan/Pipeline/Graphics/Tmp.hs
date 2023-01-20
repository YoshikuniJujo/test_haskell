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
	createGsOld, recreateGsOld, CreateInfoOld(..), CreateInfoListToMiddleOld(..),
	GListVars
	) where

import Gpu.Vulkan.Pipeline.Graphics.Middle qualified as M

import GHC.TypeNats
import Foreign.Pointable
import Data.Kind
import Data.HeteroList hiding (length)
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

data CreateInfoOld n nskndvss nvsts n3 n4 n5 n6 n7 n8 n9 n10 vsts' = CreateInfoOld {
	createInfoNextOld :: Maybe n,
	createInfoFlagsOld :: CreateFlags,
	createInfoStagesOld :: HeteroVarList (V3 ShaderStage.CreateInfo) nskndvss,
	createInfoVertexInputStateOld ::
		Maybe (V3 VertexInputState.CreateInfo nvsts),
	createInfoInputAssemblyStateOld ::
		Maybe (InputAssemblyState.CreateInfo n3),
	createInfoTessellationStateOld :: Maybe (TessellationState.CreateInfo n4),
	createInfoViewportStateOld :: Maybe (ViewportState.CreateInfo n5),
	createInfoRasterizationStateOld ::
		Maybe (RasterizationState.CreateInfo n6),
	createInfoMultisampleStateOld :: Maybe (MultisampleState.CreateInfo n7),
	createInfoDepthStencilStateOld :: Maybe (DepthStencilState.CreateInfo n8),
	createInfoColorBlendStateOld :: Maybe (ColorBlendState.CreateInfo n9),
	createInfoDynamicStateOld :: Maybe (DynamicState.CreateInfo n10),
	createInfoLayoutOld :: Layout.L,
	createInfoRenderPassOld :: RenderPass.R,
	createInfoSubpassOld :: Word32,
	createInfoBasePipelineHandleOld :: V2 M.G vsts',
	createInfoBasePipelineIndexOld :: Int32 }

createInfoToMiddle :: (
	BindingStrideList.BindingStrideList vs VertexInput.Rate VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs ts ) =>
	CreateInfoOld n nskndvss '(nv, vs, ts) n3 n4 n5 n6 n7 n8 n9 n10 vsts' ->
	M.CreateInfo n nskndvss nv n3 n4 n5 n6 n7 n8 n9 n10 vsts'
createInfoToMiddle CreateInfoOld {
	createInfoNextOld = mnxt,
	createInfoFlagsOld = flgs,
	createInfoStagesOld = stg,
	createInfoVertexInputStateOld = vis,
	createInfoInputAssemblyStateOld = ias,
	createInfoTessellationStateOld = ts,
	createInfoViewportStateOld = vs,
	createInfoRasterizationStateOld = rs,
	createInfoMultisampleStateOld = ms,
	createInfoDepthStencilStateOld = dss,
	createInfoColorBlendStateOld = cbs,
	createInfoDynamicStateOld = ds,
	createInfoLayoutOld = lyt,
	createInfoRenderPassOld = rp,
	createInfoSubpassOld = sp,
	createInfoBasePipelineHandleOld = bph,
	createInfoBasePipelineIndexOld = bpi
	} = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoStages = stg,
	M.createInfoVertexInputState =
		VertexInputState.createInfoToMiddle . unV3 <$> vis,
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

class CreateInfoListToMiddleOld sss where
	type CreateInfoListArgsOld sss ::
		[(*, [(*, ShaderKind, *)], *, *, *, *, *, *, *, *, *, ([*], [(Nat, *)]))]
	createInfoListToMiddleOld ::
		HeteroVarList (V12 CreateInfoOld) sss ->
		HeteroVarList (V12 M.CreateInfo) (CreateInfoListArgsOld sss)

instance CreateInfoListToMiddleOld '[] where
	type CreateInfoListArgsOld '[] = '[]
	createInfoListToMiddleOld _ = HVNil

instance (
	BindingStrideList.BindingStrideList vs VertexInput.Rate VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs ts,
	CreateInfoListToMiddleOld ss ) =>
	CreateInfoListToMiddleOld ('(
		n, nskndvss, '(n2, vs, ts), n3, n4, n5, n6, n7, n8, n9, n10, vsts' ) ': ss) where
	type CreateInfoListArgsOld ('(
		n, nskndvss, '(n2, vs, ts), n3, n4, n5, n6, n7, n8, n9, n10, vsts' ) ': ss) = '(
		n, nskndvss, n2, n3, n4, n5, n6, n7, n8, n9, n10, vsts') : CreateInfoListArgsOld ss
	createInfoListToMiddleOld (V12 ci :...: cis) =
		V12 (createInfoToMiddle ci) :...: createInfoListToMiddleOld cis

createGsOld :: (
	Pointable n', M.GListFromCore (GListVars ss),
	M.CreateInfoListToCore (CreateInfoListArgsOld ss),
	CreateInfoListToMiddleOld ss
	) =>
	Device.D -> Maybe Cache.C ->
	HeteroVarList (V12 CreateInfoOld) ss ->
	Maybe (AllocationCallbacks.A n') -> IO (HeteroVarList (V2 M.G) (GListVars ss))
createGsOld dvc mc cis mac = M.createGs dvc mc (createInfoListToMiddleOld cis) mac

recreateGsOld :: (
	M.CreateInfoListToCore (CreateInfoListArgsOld ss),
	CreateInfoListToMiddleOld ss,
	Pointable c, Pointable d,
	M.GListFromCore (GListVars ss) ) => Device.D -> Maybe Cache.C ->
	HeteroVarList (V12 CreateInfoOld) ss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	HeteroVarList (V2 M.G) (GListVars ss) -> IO ()
recreateGsOld dvc mc cis macc macd gs =
	M.recreateGs dvc mc (createInfoListToMiddleOld cis) macc macd gs

type family GListVars (ss :: [(
		Type, [(Type, ShaderKind, Type)],
		(Type, [Type], [(Nat, Type)]), Type, Type, Type, Type, Type, Type, Type,
		Type, ([Type], [(Nat, Type)]))]) :: [([Type], [(Nat, Type)])] where
	GListVars '[] = '[]
	GListVars ('(
		n, nskndvss, '(n2, vs, ts),
		n3, n4, n5, n6, n7, n8, n9, n10, vsts' ) ': ss) = '(vs, ts) ': GListVars ss
