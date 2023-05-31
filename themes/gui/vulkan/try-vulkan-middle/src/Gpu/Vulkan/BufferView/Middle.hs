{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.BufferView.Middle (

	-- * CREATE AND DESTROY

	create, destroy, B, CreateInfo(..), CreateFlags

	) where

import Gpu.Vulkan.BufferView.Middle.Internal
