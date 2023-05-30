{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Middle (

	-- * Type

	D,

	-- * Allocate

	allocateDs, freeDs, AllocateInfo(..),

	-- * Update

	updateDs, Write(..), WriteSources(..), Copy(..),

	updateDsNew, WriteListToCore, CopyListToCore ) where

import Gpu.Vulkan.DescriptorSet.Middle.Internal
