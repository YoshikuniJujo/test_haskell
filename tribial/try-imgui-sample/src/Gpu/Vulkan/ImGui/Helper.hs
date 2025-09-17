{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper (

	selectSurfaceFormat,
	selectPresentMode,
	createWindowSwapChain,
	createWindowCommandBuffers,
	createWindowCommandBuffersCreateCommandPool,
	createWindowCommandBuffersFromCommandPool,
	createWindowCommandBuffersCopyCommandPool,
	createWindowCommandBuffersFromCommandPool2,
	createWindowCommandBuffersFrames,

	createWindowCommandBuffersFramesCommandBuffers2,
	createWindowCommandBuffersFramesCreateCommandBuffers,
	createWindowCommandBuffersFramesCopyCommandBuffers,

	createWindowCommandBuffersFramesFence2,
	createWindowCommandBuffersSemaphores,

	destroyBeforeCreateSwapChain,
	createSwapChain, onlyCreateSwapChain,

	onlyCreateSwapChainNoWd, copySwapChainToWd, M.setSize,

	createSwapChainModifyWd,

	createWindowRenderPass, createWindowImageViews, createWindowFramebuffer,

	createWindowRenderPassRaw, setWdRenderPass,

	createWindowImageViewsRaw, copyImageViewsToWd, copyImageViewsToWd',

	createWindowFramebufferRaw, copyFramebufferToWd

	) where

import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.HeteroParList qualified as HPList
import Data.Word
import Data.Int

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.AllocationCallbacks.Internal qualified as Vk.AllocCallbacks
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Device.Internal qualified as Vk.Dvc
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandPool.Type qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer.Type qualified as Vk.CmdBffr
import Gpu.Vulkan.Image.Internal qualified as Vk.Img
import Gpu.Vulkan.ImageView.Type qualified as Vk.ImgVw
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPss
import Gpu.Vulkan.RenderPass.Type qualified as Vk.RndrPss
import Gpu.Vulkan.Framebuffer qualified as Vk.Frmbffr
import Gpu.Vulkan.Framebuffer.Type qualified as Vk.Frmbffr

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
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> Word32 -> IO a -> IO a
createWindowCommandBuffers _phd dvc wd qfi mac ic f =
	createWindowCommandBuffersCreateCommandPool dvc qfi mac ic \cp ->
	createWindowCommandBuffersFromCommandPool dvc wd qfi mac cp f

createWindowCommandBuffersCreateCommandPool :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> Vk.QFam.Index ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> Word32 ->
	(forall scpls . HPList.PL Vk.CmdPl.C scpls -> IO a) -> IO a
createWindowCommandBuffersCreateCommandPool (Vk.Dvc.D dvc) qfi mac ic f = do
	cps <- M.createWindowCommandBuffersCreateCommandPool
		dvc qfi (Vk.AllocCallbacks.toMiddle mac) ic
	fromList Vk.CmdPl.C cps f
	

fromList :: (a -> t s) -> [a] -> (forall ss . HPList.PL t ss -> b) -> b
fromList _ [] f = f HPList.Nil
fromList k (x : xs) f = fromList k xs \ys -> f $ k x HPList.:** ys

createWindowCommandBuffersFromCommandPool :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W -> Vk.QFam.Index ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> HPList.PL Vk.CmdPl.C scpls -> IO a -> IO a
createWindowCommandBuffersFromCommandPool (Vk.Dvc.D dvc) wd qfi mac cps =
	M.createWindowCommandBuffersFromCommandPool
		dvc wd qfi (Vk.AllocCallbacks.toMiddle mac) (HPList.toList (\(Vk.CmdPl.C cp) -> cp) cps)

createWindowCommandBuffersCopyCommandPool ::
	Vk.ImGui.H.Win.W -> HPList.PL Vk.CmdPl.C scpls -> IO ()
createWindowCommandBuffersCopyCommandPool wd cps =
	M.createWindowCommandBuffersCopyCommandPool
		wd (HPList.toList (\(Vk.CmdPl.C cp) -> cp) cps)

createWindowCommandBuffersFromCommandPool2 :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W -> Vk.QFam.Index ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> IO a -> IO a
createWindowCommandBuffersFromCommandPool2 (Vk.Dvc.D dvc) wd qfi mac f =
	M.createWindowCommandBuffersFromCommandPool2
		dvc wd qfi (Vk.AllocCallbacks.toMiddle mac) f

createWindowCommandBuffersFrames ::
	Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W -> Vk.QFam.Index ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> IO a -> IO a
createWindowCommandBuffersFrames dvc wd qfi mac f =
	createWindowCommandBuffersFramesCommandBuffers2 dvc wd do
		createWindowCommandBuffersFramesFence2 dvc wd mac
		f

createWindowCommandBuffersFramesCommandBuffers2 ::
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W -> IO a -> IO a
createWindowCommandBuffersFramesCommandBuffers2 dvc wd f =
	createWindowCommandBuffersFramesCreateCommandBuffers dvc wd \cbs ->
	createWindowCommandBuffersFramesCopyCommandBuffers wd cbs f

createWindowCommandBuffersFramesCreateCommandBuffers ::
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W -> (forall scbs . HPList.PL Vk.CmdBffr.C scbs -> IO a) -> IO a
createWindowCommandBuffersFramesCreateCommandBuffers (Vk.Dvc.D dvc) wd f = do
	cbs <- M.createWindowCommandBuffersFramesCreateCommandBuffers dvc wd
	fromList Vk.CmdBffr.C cbs f

createWindowCommandBuffersFramesCopyCommandBuffers ::
	Vk.ImGui.H.Win.W -> HPList.PL Vk.CmdBffr.C scbs -> IO a -> IO a
createWindowCommandBuffersFramesCopyCommandBuffers wd cbs =
	M.createWindowCommandBuffersFramesCopyCommandBuffers wd
		(HPList.toList (\(Vk.CmdBffr.C cb) -> cb) cbs)

createWindowCommandBuffersFramesFence2 :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> IO ()
createWindowCommandBuffersFramesFence2 (Vk.Dvc.D dvc) wd mac =
	M.createWindowCommandBuffersFramesFence2
		dvc wd (Vk.AllocCallbacks.toMiddle mac)

createWindowCommandBuffersSemaphores :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> IO ()
createWindowCommandBuffersSemaphores (Vk.Dvc.D dvc) wd mac =
	M.createWindowCommandBuffersSemaphores
		dvc wd (Vk.AllocCallbacks.toMiddle mac)

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

createSwapChainModifyWd :: Vk.ImGui.H.Win.W -> [Vk.Img.Binded sm si nm fmt] -> IO a -> IO a
createSwapChainModifyWd wd is a = let mis = (\(Vk.Img.Binded i) -> i) <$> is in
	M.createSwapChainModifyWd wd mis a

createWindowRenderPass :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> IO ()
createWindowRenderPass (Vk.Dvc.D dvc) wd mac =
	M.createWindowRenderPass dvc wd (Vk.AllocCallbacks.toMiddle mac)

createWindowImageViews :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> IO ()
createWindowImageViews (Vk.Dvc.D dvc) wd mac =
	M.createWindowImageViews dvc wd (Vk.AllocCallbacks.toMiddle mac)

createWindowFramebuffer :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> Vk.ImGui.H.Win.W ->
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> IO ()
createWindowFramebuffer (Vk.Dvc.D dvc) wd mac =
	M.createWindowFramebuffer dvc wd (Vk.AllocCallbacks.toMiddle mac)

-- createWindowFramebufferRaw :: (Vk.AllocCallbacks.ToMiddle mac, HPList.FromList sfbs) =>
createWindowFramebufferRaw :: Vk.AllocCallbacks.ToMiddle mac =>
	Vk.Dvc.D sd -> TPMaybe.M (U2 Vk.AllocCallbacks.A) mac ->
	Bool -> Vk.RndrPss.R srp -> Int32 -> Int32 ->
--	HPList.PL (Vk.ImgVw.I nm fmt) sivs -> (HPList.PL Vk.Frmbffr.F sfbs -> IO a) -> IO a
	HPList.PL (Vk.ImgVw.I nm fmt) sivs -> IO [Vk.Frmbffr.F sfb]
-- createWindowFramebufferRaw (Vk.Dvc.D dvc) mac udr (Vk.RndrPss.R rp) wdt hgt ivs f = f =<<
--	HPList.fromList Vk.Frmbffr.F <$>
createWindowFramebufferRaw (Vk.Dvc.D dvc) mac udr (Vk.RndrPss.R rp) wdt hgt ivs =
	(Vk.Frmbffr.F <$>) <$>
		M.createWindowFramebufferRaw dvc (Vk.AllocCallbacks.toMiddle mac) udr
			rp wdt hgt (HPList.toList (\(Vk.ImgVw.I iv) -> iv) ivs)

copyFramebufferToWd ::
	Bool -> Vk.ImGui.H.Win.W -> [Vk.Frmbffr.F sfb] -> IO ()
copyFramebufferToWd udr wd fbs =
	M.copyFramebufferToWd udr wd ((\(Vk.Frmbffr.F fb) -> fb) <$> fbs)

copyFramebufferToWd' ::
	Bool -> Vk.ImGui.H.Win.W -> HPList.PL Vk.Frmbffr.F sfbs -> IO ()
copyFramebufferToWd' udr wd fbs =
	M.copyFramebufferToWd udr wd (HPList.toList (\(Vk.Frmbffr.F fb) -> fb) fbs)

createWindowRenderPassRaw :: forall (fmt :: Vk.T.Format) sd mac srp . (
	Vk.AllocCallbacks.ToMiddle mac, Vk.T.FormatToValue fmt
	) =>
	Vk.Dvc.D sd -> TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> Bool -> Bool ->
	IO (Vk.RndrPss.R srp)
createWindowRenderPassRaw (Vk.Dvc.D dvc) mac udr ce = Vk.RndrPss.R <$>
	M.createWindowRenderPassRaw
		dvc (Vk.AllocCallbacks.toMiddle mac) udr (Vk.T.formatToValue @fmt) ce

setWdRenderPass :: Vk.ImGui.H.Win.W -> Vk.RndrPss.R srp -> IO ()
setWdRenderPass wd (Vk.RndrPss.R rp) = M.setWdRenderPass wd rp

createWindowImageViewsRaw :: forall (fmt :: Vk.T.Format) sd sm si nm siv mac . (
	Vk.T.FormatToValue fmt, Vk.AllocCallbacks.ToMiddle mac ) =>
	Vk.Dvc.D sd -> [Vk.Img.Binded sm si nm fmt] ->
--	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> IO (HPList.PL (Vk.ImgVw.I nm fmt) sivs)
	TPMaybe.M (U2 Vk.AllocCallbacks.A) mac -> IO [Vk.ImgVw.I nm fmt siv]
createWindowImageViewsRaw (Vk.Dvc.D dvc) imgs mac = (Vk.ImgVw.I <$>) <$>
	M.createWindowImageViewsRaw dvc (Vk.T.formatToValue @fmt)
		((\(Vk.Img.Binded i) -> i) <$> imgs)
		(Vk.AllocCallbacks.toMiddle mac)

copyImageViewsToWd :: Vk.ImGui.H.Win.W -> [Vk.ImgVw.I nm fmt siv] -> IO ()
copyImageViewsToWd wd ivs = M.copyImageViewsToWd wd $ (\(Vk.ImgVw.I iv) -> iv) <$> ivs

copyImageViewsToWd' :: Vk.ImGui.H.Win.W -> HPList.PL (Vk.ImgVw.I nm fmt) sivs -> IO ()
copyImageViewsToWd' wd ivs = M.copyImageViewsToWd wd $ HPList.toList (\(Vk.ImgVw.I iv) -> iv) ivs
