{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Sampler where

import Vulkan.Enum
import Vulkan.Sampler.Enum

import qualified Vulkan.Sampler.Core as C

newtype S = S C.S deriving Show

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoMagFilter :: Filter,
	createInfoMinFilter :: Filter,
	createInfoMipmapMode :: MipmapMode
	}
	deriving Show
