{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Exception.Middle (
	MultiResult(..), throwUnlessSuccess
	) where

import Gpu.Vulkan.Exception.Middle.Internal
