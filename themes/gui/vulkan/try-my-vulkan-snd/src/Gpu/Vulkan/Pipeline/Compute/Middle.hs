{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Compute.Middle (

	-- * Type

	C,

	-- * Create and Destroy

	createCs, destroy, CreateInfo(..), CreateInfoListToCore,
	createCsNew, CreateInfoNew(..), CreateInfoListToCoreNew ) where

import Gpu.Vulkan.Pipeline.Compute.Middle.Internal
