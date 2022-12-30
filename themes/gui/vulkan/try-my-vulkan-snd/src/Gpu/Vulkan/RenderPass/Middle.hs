{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Middle (
	R, CreateInfo(..), BeginInfo(..), create, destroy
	) where

import Gpu.Vulkan.RenderPass.Middle.Internal
