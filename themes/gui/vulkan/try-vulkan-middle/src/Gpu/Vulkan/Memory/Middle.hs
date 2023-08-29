{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Middle (

	-- * ALLOCATE AND FREE

	allocate, reallocate, reallocate', free, M, AllocateInfo(..),

	-- ** Manage Destruction

	Manager, manage, allocate', free', lookup,

	-- * MAP AND UNMNAP

	map, unmap, MapFlags(..),

	-- * REQUIREMENTS AND BARRIER

	Requirements(..), Barrier(..),

	-- * MEMORY TYPE

	MType(..), TypeBits, TypeIndex, elemTypeIndex ) where

import Prelude hiding (map, lookup)
import Gpu.Vulkan.Memory.Middle.Internal
