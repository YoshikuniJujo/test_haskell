{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper (imGuiImplVulkanHSelectSurfaceFormat) where

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd

import Gpu.Vulkan.Khr.Surface qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.Internal qualified as Vk.Sfc

import Gpu.Vulkan.ImGui.Helper.Middle qualified as M

imGuiImplVulkanHSelectSurfaceFormat ::
	Vk.Phd.P -> Vk.Sfc.S ssfc -> [Vk.Format] -> Vk.Sfc.ColorSpace ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Sfc.Format fmt -> IO a) -> IO a
imGuiImplVulkanHSelectSurfaceFormat pd (Vk.Sfc.S sfc) fmts cs a =
	M.imGuiImplVulkanHSelectSurfaceFormat pd sfc fmts cs \sfmt ->
	Vk.Sfc.formatFromMiddle sfmt a
