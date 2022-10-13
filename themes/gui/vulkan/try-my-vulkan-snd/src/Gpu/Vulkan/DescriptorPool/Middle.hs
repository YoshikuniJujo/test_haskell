{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorPool.Middle (
	D, CreateInfo(..), Size(..), create, destroy ) where

import Gpu.Vulkan.DescriptorPool.Middle.Internal
