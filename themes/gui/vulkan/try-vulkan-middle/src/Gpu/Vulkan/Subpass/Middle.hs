{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Subpass.Middle (

	-- * Description

	Description(..),

	-- * Dependency

	Dependency(..), S(..), pattern SExternal ) where

import Gpu.Vulkan.Subpass.Middle.Internal
