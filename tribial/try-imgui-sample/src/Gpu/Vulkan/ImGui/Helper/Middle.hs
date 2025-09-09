{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Middle (

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
	createWindowCommandBuffersFramesFence2,
	createWindowCommandBuffersSemaphores,

	destroyBeforeCreateSwapChain,
	createSwapChain, onlyCreateSwapChain,

	onlyCreateSwapChainNoWd, copySwapChainToWd, setSize,

	createSwapChainModifyWd,

	createWindowRenderPass, createWindowImageViews, createWindowFramebuffer,

	createWindowRenderPassRaw, setWdRenderPass,

	createWindowImageViewsRaw, copyImageViewsToWd,

	createWindowFramebufferRaw, copyFramebufferToWd

	) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.List qualified as L
import Data.Bool
import Data.Word
import Data.Int
import Data.IORef

import Gpu.Vulkan.Middle qualified as Vk
import Gpu.Vulkan.Enum qualified as Vk
import Gpu.Vulkan.AllocationCallbacks.Middle.Internal qualified as Vk.AllocCallbacks
import Gpu.Vulkan.PhysicalDevice.Middle.Internal qualified as Vk.Phd
import Gpu.Vulkan.Device.Middle.Internal qualified as Vk.Dvc
import Gpu.Vulkan.QueueFamily.Middle qualified as Vk.QFam
import Gpu.Vulkan.CommandPool.Middle.Internal qualified as Vk.CmdPl

import Gpu.Vulkan.Image.Middle.Internal qualified as Vk.Img
import Gpu.Vulkan.ImageView.Middle.Internal qualified as Vk.ImgVw
import Gpu.Vulkan.RenderPass.Middle.Internal qualified as Vk.RndrPss
import Gpu.Vulkan.Framebuffer.Middle.Internal qualified as Vk.Frmbffr

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
	Maybe Vk.Swpch.S -> IO ()
createWindowSwapChain
	(Vk.Dvc.D dvc) wd macs wdt hgt mic msc =
	Vk.AllocCallbacks.mToCore macs \pacs ->
	C.createWindowSwapChain dvc wd pacs wdt hgt mic =<< maybe (pure nullPtr) Vk.Swpch.sToCore msc

destroyBeforeCreateSwapChain ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> TPMaybe.M Vk.AllocCallbacks.A mud -> IO ()
destroyBeforeCreateSwapChain (Vk.Dvc.D dvc) wd macs =
	Vk.AllocCallbacks.mToCore macs \pacs ->
	C.destroyBeforeCreateSwapChain dvc wd pacs

createWindowCommandBuffers ::
	Vk.Phd.P -> Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Vk.QFam.Index ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> Word32 -> IO ()
createWindowCommandBuffers _phd dvc wd qfi macs ic = do
	cps <- createWindowCommandBuffersCreateCommandPool dvc qfi macs ic
	createWindowCommandBuffersFromCommandPool dvc wd qfi macs cps

createWindowCommandBuffersCreateCommandPool ::
	Vk.Dvc.D -> Vk.QFam.Index ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> Word32 -> IO [Vk.CmdPl.C]
createWindowCommandBuffersCreateCommandPool
	(Vk.Dvc.D dvc) (Vk.QFam.Index qfi) macs ic = allocaArray (fromIntegral ic) \pcps -> do
	Vk.AllocCallbacks.mToCore macs \pacs -> do
		pcps' <- C.createWindowCommandBuffersCreateCommandPool dvc qfi pacs ic
		copyBytes pcps pcps' (fromIntegral ic * sizeOf (undefined :: Ptr ()))
	(Vk.CmdPl.C <$>) <$> peekArray (fromIntegral ic) pcps

createWindowCommandBuffersFromCommandPool ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Vk.QFam.Index ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> [Vk.CmdPl.C] -> IO ()
createWindowCommandBuffersFromCommandPool dvc wd qfi macs cps = do
	createWindowCommandBuffersCopyCommandPool wd cps
	createWindowCommandBuffersFromCommandPool2 dvc wd qfi macs

createWindowCommandBuffersCopyCommandPool ::
	Vk.ImGui.H.Win.W -> [Vk.CmdPl.C] -> IO ()
createWindowCommandBuffersCopyCommandPool wd cps =
	allocaArray (length cps) \pcps -> do
		pokeArray pcps $ (\(Vk.CmdPl.C cp) -> cp) <$> cps
		C.createWindowCommandBuffersCopyCommandPool wd pcps

createWindowCommandBuffersFromCommandPool2 ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Vk.QFam.Index ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> IO ()
createWindowCommandBuffersFromCommandPool2 dvc wd qfi macs = do
	createWindowCommandBuffersFrames dvc wd qfi macs
	createWindowCommandBuffersSemaphores dvc wd macs

createWindowCommandBuffersFrames ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> Vk.QFam.Index ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> IO ()
createWindowCommandBuffersFrames dvc wd qfi macs = do
	createWindowCommandBuffersFramesCommandBuffers2 dvc wd
	createWindowCommandBuffersFramesFence2 dvc wd macs

createWindowCommandBuffersFramesCommandBuffers2 ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> IO ()
createWindowCommandBuffersFramesCommandBuffers2 (Vk.Dvc.D dvc) wd =
	C.createWindowCommandBuffersFramesCommandBuffers2 dvc wd

createWindowCommandBuffersFramesFence2 ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> IO ()
createWindowCommandBuffersFramesFence2 (Vk.Dvc.D dvc) wd macs =
	Vk.AllocCallbacks.mToCore macs \pacs -> do
	C.createWindowCommandBuffersFramesFence2 dvc wd pacs

createWindowCommandBuffersSemaphores ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> IO ()
createWindowCommandBuffersSemaphores (Vk.Dvc.D dvc) wd macs =
	Vk.AllocCallbacks.mToCore macs \pacs -> do
	C.createWindowCommandBuffersSemaphores dvc wd pacs

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

createSwapChainModifyWd :: Vk.ImGui.H.Win.W -> [Vk.Img.I] -> IO a -> IO a
createSwapChainModifyWd wd is a = do
	cis <- ((snd <$>) . readIORef . (\(Vk.Img.I i) -> i)) `mapM` is
	allocaArray (length is) \pis -> do
		pokeArray pis cis
		C.createSwapChainModifyWd wd pis (fromIntegral $ length is)
		a

createWindowRenderPass ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> TPMaybe.M Vk.AllocCallbacks.A mud ->
	IO ()
createWindowRenderPass (Vk.Dvc.D dvc) wd macs =
	Vk.AllocCallbacks.mToCore macs \pacs ->
	C.createWindowRenderPass dvc wd pacs

createWindowImageViews ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> TPMaybe.M Vk.AllocCallbacks.A mud ->
	IO ()
createWindowImageViews (Vk.Dvc.D dvc) wd macs =
	Vk.AllocCallbacks.mToCore macs \pacs ->
	C.createWindowImageViews dvc wd pacs

createWindowFramebuffer ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W -> TPMaybe.M Vk.AllocCallbacks.A mud ->
	IO ()
createWindowFramebuffer (Vk.Dvc.D dvc) wd macs =
	Vk.AllocCallbacks.mToCore macs \pacs ->
	C.createWindowFramebuffer dvc wd pacs

createWindowFramebufferRaw ::
	Vk.Dvc.D -> TPMaybe.M Vk.AllocCallbacks.A mud ->
	Bool -> Vk.RndrPss.R -> Int32 -> Int32 -> [Vk.ImgVw.I] -> IO [Vk.Frmbffr.F]
createWindowFramebufferRaw
	(Vk.Dvc.D dvc) macs udr (Vk.RndrPss.R rp) wdt hgt ivs = (Vk.Frmbffr.F <$>)
		<$> allocaArray n \pfb -> do
		Vk.AllocCallbacks.mToCore macs \pacs -> alloca \prp -> allocaArray n \pivs -> do
			poke prp rp
			pokeArray pivs =<< Vk.ImgVw.iToCore `mapM` ivs
			pfb' <- C.createWindowFramebufferRaw dvc pacs (bool 0 1 udr) (fromIntegral n) prp wdt hgt pivs
			pokeArray pfb =<< peekArray n pfb'
		(newIORef `mapM`) =<< peekArray n pfb
	where n = length ivs

copyFramebufferToWd ::
	Bool -> Vk.ImGui.H.Win.W -> [Vk.Frmbffr.F] -> IO ()
copyFramebufferToWd udr wd fbs = allocaArray n \pfbs -> do
	pokeArray pfbs =<< (\(Vk.Frmbffr.F fb) -> readIORef fb) `mapM` fbs
	C.copyFramebufferToWd (bool 0 1 udr) wd (fromIntegral n) pfbs
	where
	n = length fbs

createWindowRenderPassRaw ::
	Vk.Dvc.D -> TPMaybe.M Vk.AllocCallbacks.A mud ->
	Bool -> Vk.Format -> Bool -> IO Vk.RndrPss.R
createWindowRenderPassRaw (Vk.Dvc.D dvc) macs udr (Vk.Format fmt) ce =
	Vk.RndrPss.R <$> alloca \prp -> do
		Vk.AllocCallbacks.mToCore macs \pacs -> do
			putStrLn "BEFORE C.createWindowRenderPassRaw"
			p <- C.createWindowRenderPassRaw dvc pacs
				(bool 0 1 udr) fmt (bool 0 1 ce)
			putStrLn "AFTER C.createWindowRenderPassRaw"
			poke prp =<< peek p
			putStrLn "AFTER poke prp =<< peek p"
		peek prp

setWdRenderPass :: Vk.ImGui.H.Win.W -> Vk.RndrPss.R -> IO ()
setWdRenderPass wd (Vk.RndrPss.R rp) = alloca \prp -> do
	poke prp rp
	C.setWdRenderPass wd prp

createWindowImageViewsRaw ::
	Vk.Dvc.D -> Vk.Format -> [Vk.Img.I] ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> IO [Vk.ImgVw.I]
createWindowImageViewsRaw (Vk.Dvc.D dvc) (Vk.Format fmt) imgs macs = (Vk.ImgVw.iFromCore `mapM`) =<<
	allocaArray n \pimgvws -> do
		Vk.AllocCallbacks.mToCore macs \pacs ->
			allocaArray n \pimgs -> do
			pokeArray pimgs =<< (\(Vk.Img.I ri) -> snd <$> readIORef ri) `mapM` imgs
			pokeArray pimgvws =<< peekArray n
				=<< C.createWindowImageViewsRaw
					dvc fmt (fromIntegral n) pimgs pacs
		peekArray n pimgvws
	where
	n = length imgs

copyImageViewsToWd :: Vk.ImGui.H.Win.W -> [Vk.ImgVw.I] -> IO ()
copyImageViewsToWd wd ivs = allocaArray n \pivs -> do
	pokeArray pivs =<< Vk.ImgVw.iToCore `mapM` ivs
	C.copyImageViewsToWd wd pivs
	where n = length ivs
