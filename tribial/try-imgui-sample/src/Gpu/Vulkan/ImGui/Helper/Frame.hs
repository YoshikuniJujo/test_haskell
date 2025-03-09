{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Frame where

import Gpu.Vulkan.CommandPool.Type qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer.Type qualified as Vk.CmdBffr

data FC scp scb = FC {
	fCCommandPool :: Vk.CmdPl.C scp,
	fCCommandBuffer :: Vk.CmdBffr.C scb
	}
