{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Frame where

import Gpu.Vulkan.CommandPool.Type qualified as Vk.CmdPl

data FC scp = FC {
	fCCommandPool :: Vk.CmdPl.C scp
	}
