{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Subpass (

	-- * SUBPASS DESCRIPTION

	Description(..),

	-- * SUBPASS DEPENDENCY

	Dependency(..), S(..), pattern SExternal

	) where

import Gpu.Vulkan.Subpass.Middle
