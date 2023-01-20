{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Middle (

	-- * Type

	G, gNull, GListFromCore,

	-- * Create and Destroy

	createGsNew, recreateGsNew, destroyGs,
	CreateInfoNew(..), CreateInfoListToCoreNew ) where

import Gpu.Vulkan.Pipeline.Graphics.Middle.Internal
