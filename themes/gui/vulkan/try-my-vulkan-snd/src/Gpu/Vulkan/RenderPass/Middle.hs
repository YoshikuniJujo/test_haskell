{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Middle (

	-- * Type

	R,

	-- * Create and Destroy

	create, destroy, CreateInfo(..),

	-- * BeginInfo

	BeginInfo(..) ) where

import Gpu.Vulkan.RenderPass.Middle.Internal
