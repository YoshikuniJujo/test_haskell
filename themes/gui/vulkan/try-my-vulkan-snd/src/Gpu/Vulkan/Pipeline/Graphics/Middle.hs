{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Middle (

	-- * Type

	G, gNull, GListFromCore,

	-- * Create and Destroy

	-- ** New

	createGsNew, recreateGsNew, destroyGs,
	CreateInfoNew(..), CreateInfoListToCoreNew,

	-- ** Old

	createGs, recreateGs,
	CreateInfo(..), CreateInfoListToCore ) where

import Gpu.Vulkan.Pipeline.Graphics.Middle.Internal
