{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ShaderModule.Middle (
	M, CreateInfo(..), CreateFlags, create, destroy
	) where

import Gpu.Vulkan.ShaderModule.Middle.Internal
