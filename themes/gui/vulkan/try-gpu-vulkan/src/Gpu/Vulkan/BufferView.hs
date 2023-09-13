{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.BufferView (

	-- * CREATE

	create, B, CreateInfo(..), FormatOf,

	-- ** Buffer View Group
	Group, group, create', destroy, lookup

	) where

import Prelude hiding (lookup)
import Gpu.Vulkan.BufferView.Internal
