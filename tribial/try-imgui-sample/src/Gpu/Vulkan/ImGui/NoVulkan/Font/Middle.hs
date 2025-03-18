{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.Font.Middle where

import Gpu.Vulkan.ImGui.NoVulkan.Font.Core qualified as C

newtype F = F C.F deriving Show
