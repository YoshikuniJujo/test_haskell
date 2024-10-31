{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Compute.Middle (

	-- * CREATE AND DESTROY

	createCs, destroy, C, CreateInfo(..), CreateInfoListToCore ) where

import Gpu.Vulkan.Pipeline.Compute.Middle.Internal
