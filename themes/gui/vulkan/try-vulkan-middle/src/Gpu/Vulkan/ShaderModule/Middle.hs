{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ShaderModule.Middle (

	-- * CREATE AND DESTROY

	create, destroy, M, CreateInfo(..), CreateFlags, CreateFlagBits ) where

import Gpu.Vulkan.ShaderModule.Middle.Internal
