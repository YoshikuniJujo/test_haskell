{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Layout.Middle (

	-- * Type

	L,

	-- * Create and Destroy

	create, destroy, CreateInfo(..), CreateFlags ) where

import Gpu.Vulkan.Pipeline.Layout.Middle.Internal
