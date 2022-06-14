{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Compute where

import Data.Int

import Shaderc.EnumAuto

import Vulkan.Pipeline.Enum

import qualified Vulkan.Pipeline.ShaderStage as ShaderStage
import qualified Vulkan.Pipeline.Layout as Layout
import qualified Vulkan.Pipeline.Compute.Middle as M

newtype C s = C M.C deriving Show

data CreateInfo n n1 n2 c d vs sl sbph = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage ::
		ShaderStage.CreateInfo n1 n2 'GlslComputeShader c d vs,
	createInfoLayout :: Layout.L sl,
	createInfoBasePipelineHandle :: C sbph,
	createInfoBasePipelineIndex :: Int32 }
