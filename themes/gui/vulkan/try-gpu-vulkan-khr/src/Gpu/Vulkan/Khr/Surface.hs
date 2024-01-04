{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface (
	S, group, Group, unsafeDestroy, lookup,

	Capabilities(..),
	FormatOld, pattern FormatOld, formatOldFormat, formatOldColorSpace,
	Format(..),

	-- * ENUM

	module Gpu.Vulkan.Khr.Surface.Enum

	) where

import Prelude hiding (lookup)
import Gpu.Vulkan.Khr.Surface.Internal
import Gpu.Vulkan.Khr.Surface.Enum
