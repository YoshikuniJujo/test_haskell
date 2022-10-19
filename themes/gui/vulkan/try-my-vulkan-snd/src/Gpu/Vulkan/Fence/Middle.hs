{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence.Middle (
	F, CreateInfo(..), create, destroy,

	waitForFs, resetFs ) where

import Gpu.Vulkan.Fence.Middle.Internal
