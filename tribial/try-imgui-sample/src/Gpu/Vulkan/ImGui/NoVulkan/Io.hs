{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.Io (

	M.get, M.I(..),

	M.getConfigFlags, M.setConfigFlags, M.modifyConfigFlags,

	) where

import Gpu.Vulkan.ImGui.NoVulkan.Io.Middle qualified as M
