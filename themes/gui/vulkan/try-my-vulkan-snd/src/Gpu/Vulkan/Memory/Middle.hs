{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Middle (
	M, AllocateInfo(..), allocate, reallocate, free,
	MapFlags(..), map, unmap,

	Requirements(..), Barrier(..),

	MType(..),
	TypeBits, TypeIndex, elemTypeIndex,
	) where

import Prelude hiding (map)

import Gpu.Vulkan.Memory.Middle.Internal
