{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Middle (

	-- * Type

	M,

	-- * Allocate and Free

	allocate, reallocate, free, AllocateInfo(..),


	-- * Map and Unmnap

	MapFlags(..), map, unmap,

	-- * Requirements and Barrier

	Requirements(..), Barrier(..),

	-- * Memory Type

	MType(..), TypeBits, TypeIndex, elemTypeIndex ) where

import Prelude hiding (map)

import Gpu.Vulkan.Memory.Middle.Internal
