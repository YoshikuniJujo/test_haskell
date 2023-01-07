{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Middle (

	-- * Type

	D,

	-- * Allocate and Update

	allocateDs, AllocateInfo(..),
	updateDs, Write(..), WriteSources(..), Copy(..) ) where

import Gpu.Vulkan.DescriptorSet.Middle.Internal
