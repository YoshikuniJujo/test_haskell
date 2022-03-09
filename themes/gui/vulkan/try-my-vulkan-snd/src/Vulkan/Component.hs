{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Component where

import Vulkan.Component.Enum

import qualified Vulkan.Component.Core as C

data Mapping = Mapping {
	mappingR :: Swizzle, mappingG :: Swizzle,
	mappingB :: Swizzle, mappingA :: Swizzle }
	deriving Show

mappingToCore :: Mapping -> C.Mapping
mappingToCore Mapping {
	mappingR = Swizzle r, mappingG = Swizzle g,
	mappingB = Swizzle b, mappingA = Swizzle a } = C.Mapping {
		C.mappingR = r, C.mappingG = g, C.mappingB = b, C.mappingA = a }
