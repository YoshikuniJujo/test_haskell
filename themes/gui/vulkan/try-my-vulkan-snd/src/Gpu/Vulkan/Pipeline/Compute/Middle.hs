{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Compute.Middle (
	C, CreateInfo(..), CreateInfoListToCore, createCs, destroy ) where

import Gpu.Vulkan.Pipeline.Compute.Middle.Internal
