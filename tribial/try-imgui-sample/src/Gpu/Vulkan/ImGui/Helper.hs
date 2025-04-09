{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper (

	selectSurfaceFormat,
	selectPresentMode,
	createWindowSwapChain, createWindowCommandBuffers,

	destroyBeforeCreateSwapChain,
	createSwapChain, onlyCreateSwapChain,

	onlyCreateSwapChainNoWd, copySwapChainToWd, M.setSize

	) where

import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Word
import Data.Int

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.AllocationCallbacks.Internal qualified as Vk.AllocCallbacks
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Device.Internal qualified as Vk.Dvc
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam

import Gpu.Vulkan.Khr.Surface qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.Internal qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Swapchain.Type qualified as Vk.Swpch

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

createWindowSwapChain :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac ->
	Int32 -> Int32 -> Word32 -> Maybe (Vk.Swpch.S fmt ssc) -> IO ()
createWindowSwapChain (Vk.Dvc.D dvc) wd mac wdt hgt mic msc =
	let	mmsc = (\(Vk.Swpch.S sc) -> sc) <$> msc in
	M.createWindowSwapChain
		dvc wd (Vk.AllocCallbacks.toMiddle mac) wdt hgt mic mmsc

destroyBeforeCreateSwapChain :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> IO ()
destroyBeforeCreateSwapChain (Vk.Dvc.D dvc) wd mac =
	M.destroyBeforeCreateSwapChain dvc wd (Vk.AllocCallbacks.toMiddle mac)

createWindowCommandBuffers :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.ImGui.H.Win.W -> Vk.QFam.Index ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> IO ()
createWindowCommandBuffers phd (Vk.Dvc.D dvc) wd qfi mac =
	M.createWindowCommandBuffers
		phd dvc wd qfi (Vk.AllocCallbacks.toMiddle mac)

createSwapChain :: Vk.Dvc.D sd -> Vk.ImGui.H.Win.W -> Word32 -> IO ()
createSwapChain (Vk.Dvc.D dvc) wd mic = M.createSwapChain dvc wd mic

onlyCreateSwapChain :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> Int32 -> Int32 -> Word32 ->
	Vk.Swpch.S fmt ssc -> Vk.Sfc.Capabilities -> IO ()
onlyCreateSwapChain (Vk.Dvc.D dvc) wd mac wdt hgt mic (Vk.Swpch.S sc) cap =
	M.onlyCreateSwapChain dvc wd (Vk.AllocCallbacks.toMiddle mac) wdt hgt mic sc cap

onlyCreateSwapChainNoWd :: (
	Vk.AllocCallbacks.ToMiddle mac, Vk.T.FormatToValue fmt, Vk.T.FormatToValue fmt' ) =>
	Vk.Dvc.D sd ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> Word32 ->
	Vk.Swpch.S fmt ssc -> Vk.Sfc.Capabilities -> Vk.Sfc.S ssfc -> Vk.Sfc.Format fmt' ->
	Vk.Sfc.PresentMode -> Int32 -> Int32 -> IO (Vk.Swpch.S fmt ssc)
onlyCreateSwapChainNoWd (Vk.Dvc.D dvc)
	macs mic (Vk.Swpch.S sc) cap (Vk.Sfc.S sfc) (Vk.Sfc.formatToMiddle -> sfmt) pm wdt hgt =
	Vk.Swpch.S <$> M.onlyCreateSwapChainNoWd dvc
		(Vk.AllocCallbacks.toMiddle macs) mic sc cap sfc sfmt pm wdt hgt

copySwapChainToWd :: Vk.ImGui.H.Win.W -> Vk.Swpch.S fmt ssc -> IO ()
copySwapChainToWd wd (Vk.Swpch.S sc) = M.copySwapChainToWd wd sc
