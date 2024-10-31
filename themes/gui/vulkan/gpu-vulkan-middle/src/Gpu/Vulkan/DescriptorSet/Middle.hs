{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Middle (

	-- * ALLOCATE AND FREE

	allocateDs, freeDs, D, AllocateInfo(..),

	-- * UPDATE

	updateDs,
	WriteListToCore, Write(..), WriteSources(..), CopyListToCore, Copy(..)

	) where

import Gpu.Vulkan.DescriptorSet.Middle.Internal
