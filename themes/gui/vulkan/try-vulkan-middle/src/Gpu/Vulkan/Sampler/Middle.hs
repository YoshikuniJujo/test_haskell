{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sampler.Middle (

	-- * Type

	S,

	-- * Create and Destroy

	create, destroy, CreateInfo(..) ) where

import Gpu.Vulkan.Sampler.Middle.Internal
