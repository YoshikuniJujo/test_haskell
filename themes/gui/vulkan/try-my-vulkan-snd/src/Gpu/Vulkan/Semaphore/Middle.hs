{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Semaphore.Middle (
	S, CreateInfo(..), CreateFlags, create, destroy
	) where

import Gpu.Vulkan.Semaphore.Middle.Internal
