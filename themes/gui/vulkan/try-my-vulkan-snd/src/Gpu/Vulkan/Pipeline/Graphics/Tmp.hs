{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Tmp (

	CreateInfo(..), CreateInfoListToMiddle(..),

	createInfoToMiddle

	) where

import Gpu.Vulkan.Pipeline.Graphics.Middle qualified as M

import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word
import Data.Int

import Shaderc.EnumAuto

import Gpu.Vulkan.Pipeline.Enum

import qualified Gpu.Vulkan.Pipeline.ShaderStage.Middle as ShaderStage
import qualified Gpu.Vulkan.Pipeline.VertexInputState as VertexInputState
import Gpu.Vulkan.Pipeline.InputAssemblyState.Middle
	qualified as InputAssemblyState
import Gpu.Vulkan.Pipeline.TessellationState.Middle
	qualified as TessellationState
import qualified Gpu.Vulkan.Pipeline.ViewportState.Middle as ViewportState
import qualified Gpu.Vulkan.Pipeline.RasterizationState.Middle as RasterizationState
import qualified Gpu.Vulkan.Pipeline.MultisampleState.Middle as MultisampleState
import qualified Gpu.Vulkan.Pipeline.DepthStencilState.Middle
	as DepthStencilState
import qualified Gpu.Vulkan.Pipeline.ColorBlendState.Middle
	as ColorBlendState
import qualified Gpu.Vulkan.Pipeline.DynamicState.Middle
	as DynamicState
import qualified Gpu.Vulkan.PipelineLayout.Middle as Layout
import qualified Gpu.Vulkan.RenderPass.Middle as RenderPass
import qualified Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList
	as BindingStrideList
import qualified Gpu.Vulkan.VertexInput as VertexInput

data CreateInfo mn nskndvss nvsts n3 n4 n5 n6 n7 n8 n9 n10 = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoStages :: HeteroParList.PL (U3 ShaderStage.CreateInfo) nskndvss,
	createInfoVertexInputState ::
		Maybe (U3 VertexInputState.CreateInfo nvsts),
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
	createInfoBasePipelineHandle :: M.G,
	createInfoBasePipelineIndex :: Int32 }

createInfoToMiddle :: (
	BindingStrideList.BindingStrideList VertexInput.Rate vs VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs ts ) =>
	CreateInfo n nskndvss '(nv, vs, ts) n3 n4 n5 n6 n7 n8 n9 n10 ->
	M.CreateInfo n nskndvss nv n3 n4 n5 n6 n7 n8 n9 n10
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
	} = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoStages = stg,
	M.createInfoVertexInputState =
		VertexInputState.createInfoToMiddle . unU3 <$> vis,
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

class CreateInfoListToMiddle sss where
	type CreateInfoListArgs sss :: [(
		Maybe Type, [(Maybe Type, ShaderKind, [Type])],
		Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type, Maybe Type,
		Maybe Type, Maybe Type )]
	createInfoListToMiddle ::
		HeteroParList.PL (U11 CreateInfo) sss ->
		HeteroParList.PL (U11 M.CreateInfo) (CreateInfoListArgs sss)

instance CreateInfoListToMiddle '[] where
	type CreateInfoListArgs '[] = '[]
	createInfoListToMiddle _ = HeteroParList.Nil

instance (
	BindingStrideList.BindingStrideList VertexInput.Rate vs VertexInput.Rate,
	VertexInputState.CreateInfoAttributeDescription vs ts,
	CreateInfoListToMiddle ss ) =>
	CreateInfoListToMiddle ('(
		n, nskndvss, '(n2, vs, ts), n3, n4, n5, n6, n7, n8, n9, n10 ) ': ss) where
	type CreateInfoListArgs ('(
		n, nskndvss, '(n2, vs, ts), n3, n4, n5, n6, n7, n8, n9, n10 ) ': ss) = '(
		n, nskndvss, n2, n3, n4, n5, n6, n7, n8, n9, n10) : CreateInfoListArgs ss
	createInfoListToMiddle (U11 ci :** cis) =
		U11 (createInfoToMiddle ci) :** createInfoListToMiddle cis
