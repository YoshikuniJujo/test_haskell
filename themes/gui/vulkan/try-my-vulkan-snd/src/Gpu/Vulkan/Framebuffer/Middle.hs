{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer.Middle (
	F, CreateInfo(..), create, recreate, destroy
	) where

import Gpu.Vulkan.Framebuffer.Middle.Internal
