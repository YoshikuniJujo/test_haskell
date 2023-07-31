{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Subpass (

	-- * SUBPASS

	S(..), pattern SExternal,

	-- * SUBPASS DESCRIPTION

	Description(..),

	-- * SUBPASS DEPENDENCY

	Dependency(..)

	) where

import Gpu.Vulkan.Subpass.Middle
