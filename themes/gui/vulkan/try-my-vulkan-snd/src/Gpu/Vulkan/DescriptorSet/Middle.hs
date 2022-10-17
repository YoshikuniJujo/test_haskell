{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Middle (
	D, AllocateInfo(..), allocateSs,
	Write(..), WriteSources(..), Copy(..), updateDs
	) where

import Gpu.Vulkan.DescriptorSet.Middle.Internal
