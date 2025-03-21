{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper (

	selectSurfaceFormat,
	selectPresentMode,
	createOrResizeWindow

	) where

import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Word
import Data.Int

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.AllocationCallbacks.Internal qualified as Vk.AllocCallbacks
import Gpu.Vulkan.Instance.Internal qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Device.Internal qualified as Vk.Dvc
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam

import Gpu.Vulkan.Khr.Surface qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.Internal qualified as Vk.Sfc

import Gpu.Vulkan.ImGui.Helper.Window qualified as Vk.ImGui.H.Win

import Gpu.Vulkan.ImGui.Helper.Middle qualified as M

selectSurfaceFormat ::
	Vk.Phd.P -> Vk.Sfc.S ssfc -> [Vk.Format] -> Vk.Sfc.ColorSpace ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Sfc.Format fmt -> IO a) -> IO a
selectSurfaceFormat pd (Vk.Sfc.S sfc) fmts cs a =
	M.selectSurfaceFormat pd sfc fmts cs \sfmt ->
	Vk.Sfc.formatFromMiddle sfmt a

selectPresentMode ::
	Vk.Phd.P -> Vk.Sfc.S ssfc -> [Vk.Sfc.PresentMode] ->
	(Vk.Sfc.PresentMode -> IO a) -> IO a
selectPresentMode pd (Vk.Sfc.S sfc) =
	M.selectPresentMode pd sfc

createOrResizeWindow :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Ist.I si -> Vk.Phd.P -> Vk.Dvc.D sd -> Vk.ImGui.H.Win.W -> Vk.QFam.Index ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> Int32 -> Int32 -> Word32 -> IO ()
createOrResizeWindow
	(Vk.Ist.I ist) phd (Vk.Dvc.D dvc) wd qfi mac wdt hgt mic =
	M.createOrResizeWindow
		ist phd dvc wd qfi (Vk.AllocCallbacks.toMiddle mac) wdt hgt mic
