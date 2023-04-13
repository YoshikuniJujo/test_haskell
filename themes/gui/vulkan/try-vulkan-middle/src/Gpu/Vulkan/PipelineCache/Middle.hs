{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineCache.Middle (

	-- * Type

	C,

	-- * Create and Destroy

	create, destroy, getData, CreateInfo(..), Data(..) ) where

import Gpu.Vulkan.PipelineCache.Middle.Internal
