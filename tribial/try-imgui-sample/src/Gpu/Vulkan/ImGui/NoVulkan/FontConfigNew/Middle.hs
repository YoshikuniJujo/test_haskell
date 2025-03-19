{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.FontConfigNew.Middle where

import Gpu.Vulkan.ImGui.NoVulkan.FontConfigNew.Core qualified as C

newtype F = F C.F deriving Show
