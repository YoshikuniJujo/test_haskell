{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.Middle (
	L, CreateInfo(..), Binding(..), create, destroy ) where

import Gpu.Vulkan.DescriptorSetLayout.Middle.Internal
