{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Middle (

	-- * Type

	D,

	-- * Allocate

	allocateDs, AllocateInfo(..),

	-- * Update

	updateDs, Write(..), WriteSources(..), Copy(..) ) where

import Gpu.Vulkan.DescriptorSet.Middle.Internal