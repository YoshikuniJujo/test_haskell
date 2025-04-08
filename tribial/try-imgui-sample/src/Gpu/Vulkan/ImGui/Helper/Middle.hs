{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Middle (

	selectSurfaceFormat,
	selectPresentMode,
	createWindowSwapChain, createWindowCommandBuffers,

	destroyBeforeCreateSwapChain,
	createSwapChain, onlyCreateSwapChain,

	onlyCreateSwapChainNoWd, copySwapChainToWd, setSize

	) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.List qualified as L
import Data.Word
import Data.Int

import Gpu.Vulkan.Middle qualified as Vk
import Gpu.Vulkan.Enum qualified as Vk
import Gpu.Vulkan.AllocationCallbacks.Middle.Internal qualified as Vk.AllocCallbacks
import Gpu.Vulkan.PhysicalDevice.Middle.Internal qualified as Vk.Phd
import Gpu.Vulkan.Device.Middle.Internal qualified as Vk.Dvc
import Gpu.Vulkan.QueueFamily.Middle qualified as Vk.QFam

import Gpu.Vulkan.Khr.Surface.Enum qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.Middle.Internal qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Swapchain.Middle.Internal qualified as Vk.Swpch

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

selectPresentMode ::
	Vk.Phd.P -> Vk.Sfc.S -> [Vk.Sfc.PresentMode] ->
	(Vk.Sfc.PresentMode -> IO a) -> IO a
selectPresentMode (Vk.Phd.P pd) (Vk.Sfc.S sfc) pms a =
	allocaArray pmc \ppms -> do
	pokeArray ppms $ (\(Vk.Sfc.PresentMode pm) -> pm) <$> pms
	a . Vk.Sfc.PresentMode
		=<< C.selectPresentMode pd sfc ppms pmc
	where pmc :: Integral n => n; pmc = L.genericLength pms

createWindowSwapChain ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> Int32 -> Int32 -> Word32 ->
	Vk.Swpch.S -> IO ()
createWindowSwapChain
	(Vk.Dvc.D dvc) wd macs wdt hgt mic sc =
	Vk.AllocCallbacks.mToCore macs \pacs ->
	C.createWindowSwapChain dvc wd pacs wdt hgt mic =<< Vk.Swpch.sToCore sc

destroyBeforeCreateSwapChain ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> TPMaybe.M Vk.AllocCallbacks.A mud -> IO ()
destroyBeforeCreateSwapChain (Vk.Dvc.D dvc) wd macs =
	Vk.AllocCallbacks.mToCore macs \pacs ->
	C.destroyBeforeCreateSwapChain dvc wd pacs

createWindowCommandBuffers ::
	Vk.Phd.P -> Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Vk.QFam.Index ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> IO ()
createWindowCommandBuffers
	(Vk.Phd.P phd) (Vk.Dvc.D dvc) wd (Vk.QFam.Index qfi) macs =
	Vk.AllocCallbacks.mToCore macs \pacs ->
	C.createWindowCommandBuffers phd dvc wd qfi pacs

createSwapChain :: Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Word32 -> IO ()
createSwapChain (Vk.Dvc.D dvc) wd mic = C.createSwapChain dvc wd mic

onlyCreateSwapChain ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> Int32 -> Int32 -> Word32 ->
	Vk.Swpch.S -> Vk.Sfc.Capabilities -> IO ()
onlyCreateSwapChain
	(Vk.Dvc.D dvc) wd macs wdt hgt mic sc cap =
	Vk.AllocCallbacks.mToCore macs \pacs -> alloca \pcap -> do
	poke pcap $ Vk.Sfc.capabilitiesToCore cap
	csc <- Vk.Swpch.sToCore sc
	C.onlyCreateSwapChain dvc wd pacs wdt hgt mic csc pcap

onlyCreateSwapChainNoWd ::
	Vk.Dvc.D -> TPMaybe.M Vk.AllocCallbacks.A mud -> Word32 ->
	Vk.Swpch.S -> Vk.Sfc.Capabilities -> Vk.Sfc.S -> Vk.Sfc.Format ->
	Vk.Sfc.PresentMode -> Int32 -> Int32 -> IO Vk.Swpch.S
onlyCreateSwapChainNoWd
	(Vk.Dvc.D dvc) macs mic sc cap (Vk.Sfc.S sfc) sfmt (Vk.Sfc.PresentMode pm) wdt hgt =
	alloca \psc' -> do
	Vk.AllocCallbacks.mToCore macs \pacs -> alloca \pcap -> alloca \psfmt -> do
		poke pcap $ Vk.Sfc.capabilitiesToCore cap
		csc <- Vk.Swpch.sToCore sc
		poke psfmt $ Vk.Sfc.formatToCore sfmt
		psc <- C.onlyCreateSwapChainNoWd dvc pacs mic csc pcap sfc psfmt pm wdt hgt
		poke psc' =<< peek psc
	Vk.Swpch.sFromCore (Vk.Extent2d (fromIntegral wdt) (fromIntegral hgt))
		=<< peek psc'

copySwapChainToWd :: Vk.ImGui.H.Win.W -> Vk.Swpch.S -> IO ()
copySwapChainToWd wd sc = alloca \psc -> do
	csc <- Vk.Swpch.sToCore sc
	poke psc csc
	C.copySwapChainToWd wd psc

setSize :: Vk.ImGui.H.Win.W -> Int32 -> Int32 -> Vk.Sfc.Capabilities -> IO ()
setSize wd w h cap = alloca \pcap -> do
	poke pcap $ Vk.Sfc.capabilitiesToCore cap
	C.setSize wd w h pcap
