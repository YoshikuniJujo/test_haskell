{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.Middle (

	-- * Type

	L,

	-- * Create and Destroy

	create, destroy, CreateInfo(..), Binding(..) ) where

import Gpu.Vulkan.DescriptorSetLayout.Middle.Internal
