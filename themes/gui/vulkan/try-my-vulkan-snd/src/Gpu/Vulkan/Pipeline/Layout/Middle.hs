{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Layout.Middle (
	L, CreateInfo(..), CreateFlags, create, destroy
	) where

import Gpu.Vulkan.Pipeline.Layout.Middle.Internal
