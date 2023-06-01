{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.Middle (

	-- * CREATE AND DESTROY

	create, destroy, L, CreateInfo(..), Binding(..)

	) where

import Gpu.Vulkan.DescriptorSetLayout.Middle.Internal
