{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.Middle (

	-- * CREATE AND DESTROY

	create, destroy, D, CreateInfo(..), Binding(..)

	) where

import Gpu.Vulkan.DescriptorSetLayout.Middle.Internal
