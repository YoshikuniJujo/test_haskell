{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.Middle (
	CreateInfo(..), CreateFlags,

	createInfoToCore
	) where

import Gpu.Vulkan.Pipeline.VertexInputState.Middle.Internal
