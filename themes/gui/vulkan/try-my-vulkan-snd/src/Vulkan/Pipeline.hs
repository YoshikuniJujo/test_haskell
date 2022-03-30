{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Vulkan.Pipeline.Enum

import qualified Vulkan.Pipeline.ShaderStage as ShaderStage
import qualified Vulkan.Pipeline.VertexInputState as VertexInputState
import qualified Vulkan.Pipeline.InputAssemblyState as InputAssemblyState
import qualified Vulkan.Pipeline.TessellationState as TessellationState
import qualified Vulkan.Pipeline.ViewportState as ViewportState

data CreateInfo n n1 n2 n3 n4 vs ts n5 n6 n7 = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStages :: [ShaderStage.CreateInfo n1 n2 n3],
	createInfoVertexInputState ::
		Maybe (VertexInputState.CreateInfo n4 vs ts),
	createInfoInputAssemblyState ::
		Maybe (InputAssemblyState.CreateInfo n5),
	createInfoTessellationState :: Maybe (TessellationState.CreateInfo n6),
	createInfoViewportState :: Maybe (ViewportState.CreateInfo n7)
	}
	deriving Show
