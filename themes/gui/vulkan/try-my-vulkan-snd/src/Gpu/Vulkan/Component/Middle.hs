{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Component.Middle (
	Mapping(..),
	Swizzle,
	pattern SwizzleIdentity, pattern SwizzleZero, pattern SwizzleOne,
	pattern SwizzleR, pattern SwizzleG, pattern SwizzleB, pattern SwizzleA,
	pattern SwizzleMaxEnum ) where

import Gpu.Vulkan.Component.Middle.Internal
import Gpu.Vulkan.Component.Enum
