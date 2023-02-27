{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Middle (

	-- * Type

	G, gNull,

	-- * Create and Destroy

	createGs, recreateGs, destroyGs,
	CreateInfo(..), CreateInfoListToCore ) where

import Gpu.Vulkan.Pipeline.Graphics.Middle.Internal
