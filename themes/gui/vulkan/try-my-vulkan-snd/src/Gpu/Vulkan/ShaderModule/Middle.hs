{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ShaderModule.Middle (

	-- * Type

	M,

	-- * Create and Destroy

	create, destroy, CreateInfo(..), CreateFlags, CreateFlagBits ) where

import Gpu.Vulkan.ShaderModule.Middle.Internal
