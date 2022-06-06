{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Graphics (
	createGs, M.CreateInfoList(..), CreateInfo(..),
	GList, pattern GNil, pattern GCons ) where

import Foreign.Pointable
import Control.Exception
import Data.Word
import Data.Int

import Vulkan.Pipeline.Enum
import Vulkan.Pipeline.Graphics.Type

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.Pipeline.Graphics.Middle as M
import qualified Vulkan.Pipeline.Cache.Type as Cache
import qualified Vulkan.Pipeline.Layout.Type as Layout

import qualified Vulkan.RenderPass.Type as RenderPass
import qualified Vulkan.Pipeline.DynamicState as DynamicState
import qualified Vulkan.Pipeline.ColorBlendState as ColorBlendState
import qualified Vulkan.Pipeline.DepthStencilState as DepthStencilState
import qualified Vulkan.Pipeline.MultisampleState as MultisampleState
import qualified Vulkan.Pipeline.RasterizationState as RasterizationState
import qualified Vulkan.Pipeline.ViewportState as ViewportState
import qualified Vulkan.Pipeline.TessellationState as TessellationState
import qualified Vulkan.Pipeline.InputAssemblyState as InputAssemblyState
import qualified Vulkan.Pipeline.VertexInputState as VertexInputState
import qualified Vulkan.Pipeline.ShaderStage as ShaderStage

data CreateInfo s n n1 sknds vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10 sl sr
	vs'' ts' = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStages :: ShaderStage.CreateInfoList n1 sknds vss,
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
	createInfoBasePipelineHandle :: G s vs'' ts',
	createInfoBasePipelineIndex :: Int32 }

deriving instance (
	Show n, Show n2, Show n3, Show n4, Show n5, Show n6, Show n7, Show n8,
	Show n9, Show n10,
	Show (ShaderStage.CreateInfoList n1 sknds vss)
	) =>
	Show (CreateInfo
		s n n1 sknds vss n2 vs' ts n3 n4 n5 n6 n7 n8 n9 n10
		sl sr vs'' ts')

createGs :: (
	M.CreateInfoListToCore
		ns n1s skndss vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
		vs''s ts's,
	M.PListFromCore vs's tss, Pointable n', Pointable n'' ) =>
	Device.D sd -> Maybe (Cache.C sc) ->
	M.CreateInfoList
		ns n1s skndss vsss n2s vs's tss n3s n4s n5s n6s n7s n8s n9s n10s
		vs''s ts's ->
	Maybe (AllocationCallbacks.A n') -> Maybe (AllocationCallbacks.A n'') ->
	(forall s . GList s vs's tss -> IO a) -> IO a
createGs (Device.D dvc) ((Cache.cToMiddle <$>) -> mc) ci macc macd f = bracket
	(M.create dvc mc ci macc) (\gs -> M.destroyGs dvc gs macd) (f . GList)
