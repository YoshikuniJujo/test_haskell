{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Exception.Middle (

	-- * THROW

	throwUnless, throwUnlessSuccess, throwUnlessSuccesses,

	-- * MULTI RESULT

	MultiResult(..)

	) where

import Gpu.Vulkan.Exception.Middle.Internal
