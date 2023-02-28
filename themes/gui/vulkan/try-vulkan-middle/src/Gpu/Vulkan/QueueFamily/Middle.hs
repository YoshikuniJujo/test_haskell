{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.QueueFamily.Middle (

	-- * Properties

	Properties(..), Index(..), pattern Ignored ) where

import Gpu.Vulkan.QueueFamily.EnumManual
import Gpu.Vulkan.QueueFamily.Middle.Internal
