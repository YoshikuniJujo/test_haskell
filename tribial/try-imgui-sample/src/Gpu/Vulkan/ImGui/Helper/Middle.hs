{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Middle (imGuiImplVulkanHSelectSurfaceFormat) where

import Foreign.Marshal.Array
import Data.List qualified as L

import Gpu.Vulkan.Enum qualified as Vk
import Gpu.Vulkan.PhysicalDevice.Middle.Internal qualified as Vk.Phd
import Gpu.Vulkan.Khr.Surface.Enum qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.Middle.Internal qualified as Vk.Sfc

import Gpu.Vulkan.ImGui.Helper.Core qualified as C

imGuiImplVulkanHSelectSurfaceFormat ::
	Vk.Phd.P -> Vk.Sfc.S -> [Vk.Format] -> Vk.Sfc.ColorSpace ->
	(Vk.Sfc.Format -> IO a) -> IO a
imGuiImplVulkanHSelectSurfaceFormat
	(Vk.Phd.P pd) (Vk.Sfc.S sfc) fmts (Vk.Sfc.ColorSpace cs) a =
	allocaArray (length fmts) \pfmts -> do
	pokeArray pfmts $ (\(Vk.Format f) -> f) <$> fmts
	a . Vk.Sfc.formatFromCore =<< C.imGuiImplVulkanHSelectSurfaceFormat
		pd sfc pfmts (L.genericLength fmts) cs
