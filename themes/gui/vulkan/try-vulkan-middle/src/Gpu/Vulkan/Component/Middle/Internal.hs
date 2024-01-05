{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Component.Middle.Internal where

import Data.Default
import Gpu.Vulkan.Component.Enum

import qualified Gpu.Vulkan.Component.Core as C

data Mapping = Mapping {
	mappingR :: Swizzle, mappingG :: Swizzle,
	mappingB :: Swizzle, mappingA :: Swizzle }
	deriving Show

instance Default Mapping where
	def = Mapping {
		mappingR = def, mappingG = def, mappingB = def, mappingA = def }

mappingToCore :: Mapping -> C.Mapping
mappingToCore Mapping {
	mappingR = Swizzle r, mappingG = Swizzle g,
	mappingB = Swizzle b, mappingA = Swizzle a } = C.Mapping {
		C.mappingR = r, C.mappingG = g, C.mappingB = b, C.mappingA = a }
