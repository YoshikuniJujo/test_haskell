{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Cache.Middle (
	C, CreateInfo(..), InitialData(..), create, destroy ) where

import Gpu.Vulkan.Pipeline.Cache.Middle.Internal
