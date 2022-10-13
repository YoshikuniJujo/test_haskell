{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorPool.Middle (
	P, CreateInfo(..), Size(..), create, destroy ) where

import Gpu.Vulkan.DescriptorPool.Middle.Internal
