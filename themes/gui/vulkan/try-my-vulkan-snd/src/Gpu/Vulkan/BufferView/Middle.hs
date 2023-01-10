{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.BufferView.Middle (

	-- * Type

	B,

	-- * Create and Destroy

	create, destroy,
	CreateInfo(..), CreateFlags ) where

import Gpu.Vulkan.BufferView.Middle.Internal
