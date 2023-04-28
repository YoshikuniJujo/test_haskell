{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineLayout.Middle (

	-- * Type

	L,

	-- * Create and Destroy

	create, destroy, CreateInfo(..), CreateFlags ) where

import Gpu.Vulkan.PipelineLayout.Middle.Internal
