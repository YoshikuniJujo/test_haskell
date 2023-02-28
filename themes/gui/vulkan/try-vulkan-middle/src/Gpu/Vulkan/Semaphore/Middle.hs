{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Semaphore.Middle (

	-- * Type

	S,

	-- * Create and Destroy

	create, destroy, CreateInfo(..), CreateFlags ) where

import Gpu.Vulkan.Semaphore.Middle.Internal
