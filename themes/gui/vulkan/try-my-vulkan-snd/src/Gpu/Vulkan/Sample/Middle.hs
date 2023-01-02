{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs  #-}

module Gpu.Vulkan.Sample.Middle (
	CountAndMask(..),
	CountFlags, CountFlagBits,
	pattern CountFlagBitsMaxEnum,
	pattern Count64Bit, pattern Count32Bit, pattern Count16Bit,
	pattern Count8Bit, pattern Count4Bit, pattern Count2Bit,
	pattern Count1Bit ) where

import Gpu.Vulkan.Sample.Middle.Internal
import Gpu.Vulkan.Sample.Enum
