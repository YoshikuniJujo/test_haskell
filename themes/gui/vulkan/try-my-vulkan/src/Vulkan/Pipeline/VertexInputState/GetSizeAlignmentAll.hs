{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.GetSizeAlignmentAll where

import Vulkan.Pipeline.VertexInputState.GetSizeAlignment

class FindSizeAlignmentAll a where
	findSizeAlignmentAll :: [(Size, Alignment)]
