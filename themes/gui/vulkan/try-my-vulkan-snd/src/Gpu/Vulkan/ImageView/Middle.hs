{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView.Middle (

	-- * Type

	I,

	-- * Create and Destroy

	create, recreate, destroy, CreateInfo(..) ) where

import Gpu.Vulkan.ImageView.Middle.Internal
