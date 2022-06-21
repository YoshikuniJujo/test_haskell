{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ShaderModule (
	M(..), CreateInfo(..), CreateFlags, pattern CreateFlagsZero ) where

import Gpu.Vulkan.ShaderModule.Internal
