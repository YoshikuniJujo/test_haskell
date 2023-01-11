{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorPool.Middle (

	-- * Type

	D,

	-- * Create and Destroy

	create, destroy, CreateInfo(..),

	-- ** Size

	Size(..) ) where

import Gpu.Vulkan.DescriptorPool.Middle.Internal
