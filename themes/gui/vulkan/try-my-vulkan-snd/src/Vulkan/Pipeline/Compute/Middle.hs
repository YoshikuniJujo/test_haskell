{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Compute.Middle where

import qualified Vulkan.Pipeline.Enum as Pipeline
-- import qualified Vulkan.Pipeline.Middle as Pipeline
import qualified Vulkan.Pipeline.Compute.Core as C
import qualified Vulkan.Pipeline.ShaderStage.Middle as ShaderStage
import qualified Vulkan.Pipeline.Layout.Middle as Pipeline.Layout

data CreateInfo n n1 sknd vs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: Pipeline.CreateFlags,
	createInfoStage :: ShaderStage.CreateInfo n1 sknd vs,
	createInfoLayout :: Pipeline.Layout.L
--	createInfoBasePipelineHandle :: Pipeline.P
	}
