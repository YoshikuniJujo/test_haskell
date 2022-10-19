{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView.Middle (
	I, CreateInfo(..), create, recreate, destroy
	) where

import Gpu.Vulkan.ImageView.Middle.Internal
