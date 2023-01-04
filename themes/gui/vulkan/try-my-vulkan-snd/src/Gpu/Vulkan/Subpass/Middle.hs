{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Subpass.Middle (
	S(..), pattern SExternal,
	Description(..), Dependency(..) ) where

import Gpu.Vulkan.Subpass.Middle.Internal
