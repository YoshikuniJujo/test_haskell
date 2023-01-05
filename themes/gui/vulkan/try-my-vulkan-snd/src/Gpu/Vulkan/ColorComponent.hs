{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ColorComponent (
	Flags, FlagBits,
	pattern RBit, pattern GBit, pattern BBit, pattern ABit ) where

import Gpu.Vulkan.ColorComponent.Middle
