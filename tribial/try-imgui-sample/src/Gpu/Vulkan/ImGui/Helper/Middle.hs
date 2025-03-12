{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Middle (

	imGuiImplVulkanHSelectSurfaceFormat,
	imGuiImplVulkanHSelectPresentMode

	) where

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
	allocaArray fmtc \pfmts -> do
	pokeArray pfmts $ (\(Vk.Format f) -> f) <$> fmts
	a . Vk.Sfc.formatFromCore =<< C.imGuiImplVulkanHSelectSurfaceFormat
		pd sfc pfmts fmtc cs
	where fmtc :: Integral n => n; fmtc = L.genericLength fmts

imGuiImplVulkanHSelectPresentMode ::
	Vk.Phd.P -> Vk.Sfc.S -> [Vk.Sfc.PresentMode] ->
	(Vk.Sfc.PresentMode -> IO a) -> IO a
imGuiImplVulkanHSelectPresentMode (Vk.Phd.P pd) (Vk.Sfc.S sfc) pms a =
	allocaArray pmc \ppms -> do
	pokeArray ppms $ (\(Vk.Sfc.PresentMode pm) -> pm) <$> pms
	a . Vk.Sfc.PresentMode
		=<< C.imGuiImplVulkanHSelectPresentMode pd sfc ppms pmc
	where pmc :: Integral n => n; pmc = L.genericLength pms
