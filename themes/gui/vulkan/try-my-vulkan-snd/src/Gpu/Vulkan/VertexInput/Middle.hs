{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.VertexInput.Middle (
	AttributeDescription(..), BindingDescription(..),
	Rate, pattern RateVertex, pattern RateInstance ) where

import Gpu.Vulkan.VertexInput.Middle.Internal
import Gpu.Vulkan.VertexInput.Enum
