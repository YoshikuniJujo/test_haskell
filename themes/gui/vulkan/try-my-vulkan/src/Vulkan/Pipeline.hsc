{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Vulkan.PipelineCreateFlagBits

import Vulkan.Pipeline.Internal as I
import Vulkan.Pipeline.ShaderStage as ShaderStage

data CreateInfo n n1 = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: PipelineCreateFlags,
	createInfoStages :: [ShaderStage.CreateInfo n1]
	}
	deriving Show
