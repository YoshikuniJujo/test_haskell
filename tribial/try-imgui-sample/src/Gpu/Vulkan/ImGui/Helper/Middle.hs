{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Middle (

	selectSurfaceFormat,
	imGuiImplVulkanHSelectPresentMode,
	imGuiImplVulkanHCreateOrResizeWindow

	) where

import Foreign.Marshal.Array
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.List qualified as L
import Data.Word
import Data.Int

import Gpu.Vulkan.Enum qualified as Vk
import Gpu.Vulkan.AllocationCallbacks.Middle.Internal qualified as Vk.AllocCallbacks
import Gpu.Vulkan.Instance.Middle.Internal qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice.Middle.Internal qualified as Vk.Phd
import Gpu.Vulkan.Device.Middle.Internal qualified as Vk.Dvc
import Gpu.Vulkan.QueueFamily.Middle qualified as Vk.QFam

import Gpu.Vulkan.Khr.Surface.Enum qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.Middle.Internal qualified as Vk.Sfc

import Gpu.Vulkan.ImGui.Helper.Window.Middle qualified as Vk.ImGui.H.Win

import Gpu.Vulkan.ImGui.Helper.Core qualified as C

selectSurfaceFormat ::
	Vk.Phd.P -> Vk.Sfc.S -> [Vk.Format] -> Vk.Sfc.ColorSpace ->
	(Vk.Sfc.Format -> IO a) -> IO a
selectSurfaceFormat
	(Vk.Phd.P pd) (Vk.Sfc.S sfc) fmts (Vk.Sfc.ColorSpace cs) a =
	allocaArray fmtc \pfmts -> do
	pokeArray pfmts $ (\(Vk.Format f) -> f) <$> fmts
	a . Vk.Sfc.formatFromCore =<< C.selectSurfaceFormat
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

imGuiImplVulkanHCreateOrResizeWindow ::
	Vk.Ist.I -> Vk.Phd.P -> Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Vk.QFam.Index ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> Int32 -> Int32 -> Word32 -> IO ()
imGuiImplVulkanHCreateOrResizeWindow
	(Vk.Ist.I ist) (Vk.Phd.P phd) (Vk.Dvc.D dvc) wd (Vk.QFam.Index qfi) macs wdt hgt mic =
	Vk.AllocCallbacks.mToCore macs \pacs ->
	C.imGuiImplVulkanHCreateOrResizeWindow
		ist phd dvc wd qfi pacs wdt hgt mic
