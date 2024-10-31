{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer.Middle (

	-- * CREATE AND DESTROY

	create, recreate, destroy, F, CreateInfo(..)

	) where

import Gpu.Vulkan.Framebuffer.Middle.Internal
