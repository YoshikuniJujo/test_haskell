{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Data.Word
import Data.Int

import Vulkan.Pipeline.Enum

import qualified Vulkan.Pipeline.ShaderStage as ShaderStage
import qualified Vulkan.Pipeline.VertexInputState as VertexInputState
import qualified Vulkan.Pipeline.InputAssemblyState as InputAssemblyState
import qualified Vulkan.Pipeline.TessellationState as TessellationState
import qualified Vulkan.Pipeline.ViewportState as ViewportState
import qualified Vulkan.Pipeline.RasterizationState as RasterizationState
import qualified Vulkan.Pipeline.MultisampleState as MultisampleState
import qualified Vulkan.Pipeline.DepthStencilState as DepthStencilState
import qualified Vulkan.Pipeline.ColorBlendState as ColorBlendState
import qualified Vulkan.Pipeline.DynamicState as DynamicState
import qualified Vulkan.Pipeline.Layout as Layout
import qualified Vulkan.RenderPass as RenderPass
import qualified Vulkan.Pipeline.Core as C

data CreateInfo n n1 n2 n3 n4 vs ts n5 n6 n7 n8 n9 n10 n11 n12 = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStages :: [ShaderStage.CreateInfo n1 n2 n3],
	createInfoVertexInputState ::
		Maybe (VertexInputState.CreateInfo n4 vs ts),
	createInfoInputAssemblyState ::
		Maybe (InputAssemblyState.CreateInfo n5),
	createInfoTessellationState :: Maybe (TessellationState.CreateInfo n6),
	createInfoViewportState :: Maybe (ViewportState.CreateInfo n7),
	createInfoRasterizationState ::
		Maybe (RasterizationState.CreateInfo n8),
	createInfoMultisampleState :: Maybe (MultisampleState.CreateInfo n9),
	creaetInfoDepthStencilState :: Maybe (DepthStencilState.CreateInfo n10),
	createInfoColorBlendState :: Maybe (ColorBlendState.CreateInfo n11),
	createInfoDynamicState :: Maybe (DynamicState.CreateInfo n12),
	createInfoLayout :: Layout.L,
	createInfoRenderPass :: RenderPass.R,
	createInfoSubpass :: Word32,
	createInfoBasePipelineHandle :: P,
	createInfoBasePipelineIndex :: Int32 }
	deriving Show

newtype P = P C.P deriving Show
