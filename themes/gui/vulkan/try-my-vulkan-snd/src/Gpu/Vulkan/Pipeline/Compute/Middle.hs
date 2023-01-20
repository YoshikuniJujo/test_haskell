{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Compute.Middle (

	-- * Type

	C,

	-- * Create and Destroy

	createCsNew, destroy, CreateInfoNew(..), CreateInfoListToCoreNew ) where

import Gpu.Vulkan.Pipeline.Compute.Middle.Internal
