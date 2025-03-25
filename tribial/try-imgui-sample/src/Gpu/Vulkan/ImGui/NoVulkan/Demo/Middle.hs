{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.Demo.Middle where

import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Bool

import Gpu.Vulkan.ImGui.NoVulkan.Demo.Core qualified as C

showWindow :: Bool -> IO Bool
showWindow o = alloca \p ->
	poke p (bool 0 1 o) >> C.showWindow p >> (/= 0) <$> peek p
