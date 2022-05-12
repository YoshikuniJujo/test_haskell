{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.DescriptorSet.Layout where

import Data.Word

import Vulkan.Enum

import qualified Vulkan.Shader.Stage.Enum as Shader.Stage
import qualified Vulkan.DescriptorSet.Layout.Core as C

data Binding = Binding {
	bindingBinding :: Word32,
	bindingDescriptorType :: DescriptorType,
	bindingDescriptorCount :: Word32,
	bindingStageFlags :: Shader.Stage.Flags
--	bindingImmutableSamplers ::
	}
	deriving Show

newtype L = L C.L deriving Show
