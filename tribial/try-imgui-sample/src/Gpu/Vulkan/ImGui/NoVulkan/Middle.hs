{-# LANGUAGE ImportQualifiedPost #-}

module Gpu.Vulkan.ImGui.NoVulkan.Middle (C.newFrame) where

import Control.Exception

import Gpu.Vulkan.ImGui.NoVulkan.Core qualified as C

-- begin :: String -> Bool -> IO a -> IO (a, Bool)
-- begin
