{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Cache.Middle (

	-- * Type

	C,

	-- * Create and Destroy

	create, destroy, CreateInfo(..), InitialData(..) ) where

import Gpu.Vulkan.Pipeline.Cache.Middle.Internal
