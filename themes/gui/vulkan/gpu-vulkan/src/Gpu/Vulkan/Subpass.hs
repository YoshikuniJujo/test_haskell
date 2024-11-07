{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Subpass (

	-- * SUBPASS DESCRIPTION

	Description(..),

	-- * SUBPASS DEPENDENCY

	Dependency(..), S, pattern SExternal,

	-- * ENUM

	module Gpu.Vulkan.Subpass.Enum

	) where

import Gpu.Vulkan.Subpass.Middle
import Gpu.Vulkan.Subpass.Enum
