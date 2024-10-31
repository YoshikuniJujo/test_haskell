{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorPool.Middle (

	-- * CREATE AND DESTROY

	create, destroy, D, CreateInfo(..), Size(..)

	) where

import Gpu.Vulkan.DescriptorPool.Middle.Internal
