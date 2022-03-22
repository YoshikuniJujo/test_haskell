{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ShaderStage where

import qualified Data.ByteString as BS

import Vulkan.Pipeline.ShaderStage.Enum

import qualified Vulkan.Shader.Stage.Enum as ShaderStage
import qualified Vulkan.Shader.Module as ShaderModule

data CreateInfo n sknd = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoStage :: ShaderStage.FlagBits,
	createInfoModule :: ShaderModule.M,
	createInfoName :: BS.ByteString
--	createInfoSpecializationInfo :: 
	}
	deriving Show
