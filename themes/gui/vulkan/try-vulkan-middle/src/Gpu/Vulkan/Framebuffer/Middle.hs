{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer.Middle (

	-- * Type

	F,

	-- * Create and Destroy

	create, recreate, destroy, CreateInfo(..) ) where

import Gpu.Vulkan.Framebuffer.Middle.Internal
