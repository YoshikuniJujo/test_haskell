{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Middle (

	selectSurfaceFormat,
	selectPresentMode,
	createWindowSwapChain,
	createWindowCommandBuffersCreateCommandPool,
	createWindowCommandBuffersCopyCommandPool,

-- * Here

--	createWindowCommandBuffersFramesCreateCommandBuffers,
	createWindowCommandBuffersFramesCopyCommandBuffers,

-- * After Here

	createWindowCommandBuffersFramesFence2,
	createWindowCommandBuffersFramesFence,
	createWindowCommandBuffersFramesFence2Copy,
	createWindowCommandBuffersSemaphores,
	createWindowCommandBuffersSemaphoresCreate,
	createWindowCommandBuffersSemaphoresCopy,

	onlyCreateSwapChain,

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
import Control.Monad
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
import Gpu.Vulkan.Semaphore.Middle.Internal qualified as Vk.Smp
import Gpu.Vulkan.Fence.Middle.Internal qualified as Vk.Fnc
import Gpu.Vulkan.QueueFamily.Middle qualified as Vk.QFam
import Gpu.Vulkan.CommandPool.Middle.Internal qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer.Middle.Internal qualified as Vk.CmdBffr
import Gpu.Vulkan.CommandBuffer.Core qualified as Vk.CmdBffr.C

import Gpu.Vulkan.Image.Middle.Internal qualified as Vk.Img
import Gpu.Vulkan.ImageView.Middle.Internal qualified as Vk.ImgVw
import Gpu.Vulkan.RenderPass.Middle.Internal qualified as Vk.RndrPss
import Gpu.Vulkan.Framebuffer.Middle.Internal qualified as Vk.Frmbffr

import Gpu.Vulkan.Khr.Surface.Enum qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.Middle.Internal qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Swapchain.Middle.Internal qualified as Vk.Swpch

import Gpu.Vulkan.ImGui.Helper.Window.Middle qualified as Vk.ImGui.H.Win
import Gpu.Vulkan.ImGui.Helper.Window.Core qualified as Vk.ImGui.H.Win.C

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

createWindowCommandBuffersCreateCommandPool ::
	Vk.Dvc.D -> Vk.QFam.Index ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> Word32 -> IO [Vk.CmdPl.C]
createWindowCommandBuffersCreateCommandPool
	(Vk.Dvc.D dvc) (Vk.QFam.Index qfi) macs ic = allocaArray (fromIntegral ic) \pcps -> do
	Vk.AllocCallbacks.mToCore macs \pacs -> do
		pcps' <- C.createWindowCommandBuffersCreateCommandPool dvc qfi pacs ic
		copyBytes pcps pcps' (fromIntegral ic * sizeOf (undefined :: Ptr ()))
	(Vk.CmdPl.C <$>) <$> peekArray (fromIntegral ic) pcps

createWindowCommandBuffersCopyCommandPool ::
	Vk.ImGui.H.Win.W -> [Vk.CmdPl.C] -> IO ()
createWindowCommandBuffersCopyCommandPool wd cps =
	allocaArray (length cps) \pcps -> do
		pokeArray pcps $ (\(Vk.CmdPl.C cp) -> cp) <$> cps
		C.createWindowCommandBuffersCopyCommandPool wd pcps

getCommandBuffersList ::
	Vk.ImGui.H.Win.W -> Ptr Vk.CmdBffr.C.C -> IO [Vk.CmdBffr.C.C]
getCommandBuffersList wd cbs = do
	n <- Vk.ImGui.H.Win.C.wCImageCount <$> Vk.ImGui.H.Win.C.toC wd
	peekArray (fromIntegral n) cbs

createWindowCommandBuffersFramesCopyCommandBuffers ::
	Vk.ImGui.H.Win.W -> [Vk.CmdBffr.C] -> IO a -> IO a
createWindowCommandBuffersFramesCopyCommandBuffers wd cbs f =
	let	ccbs = (\(Vk.CmdBffr.C _ cb) -> cb) <$> cbs in
	setCommandBufferList ccbs \pcbs -> do
	C.createWindowCommandBuffersFramesCopyCommandBuffers wd pcbs
	f

setCommandBufferList :: [Vk.CmdBffr.C.C] -> (Ptr Vk.CmdBffr.C.C -> IO a) -> IO a
setCommandBufferList cbs f =
	allocaArray (length cbs) \p -> pokeArray p cbs >> f p

createWindowCommandBuffersFramesFence2 ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> Int -> IO ()
createWindowCommandBuffersFramesFence2 dvc wd macs n = do
	fncs <- createWindowCommandBuffersFramesFence dvc macs $ fromIntegral n
	createWindowCommandBuffersFramesFence2Copy wd fncs n

createWindowCommandBuffersFramesFence ::
	Vk.Dvc.D ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> Word32 -> IO [Vk.Fnc.F]
createWindowCommandBuffersFramesFence (Vk.Dvc.D dvc) macs ic = (Vk.Fnc.F <$>) <$> allocaArray (fromIntegral ic) \pfs -> do
	Vk.AllocCallbacks.mToCore macs \pacs ->
		C.createWindowCommandBuffersFramesFence dvc pacs ic pfs
	peekArray (fromIntegral ic) pfs

createWindowCommandBuffersFramesFence2Copy ::
	Vk.ImGui.H.Win.W -> [Vk.Fnc.F] -> Int -> IO ()
createWindowCommandBuffersFramesFence2Copy wd fncs ic = allocaArray ic \pfs -> do
	pokeArray pfs $ (\(Vk.Fnc.F f) -> f) <$> fncs
	C.createWindowCommandBuffersFramesFence2Copy wd pfs

createWindowCommandBuffersSemaphores ::
	Vk.Dvc.D -> Vk.ImGui.H.Win.W ->
	TPMaybe.M Vk.AllocCallbacks.A mud -> Int -> IO ()
createWindowCommandBuffersSemaphores dvc wd macs n = do
	(iasmps, rcsmps) <-
		createWindowCommandBuffersSemaphoresCreate dvc macs n
	createWindowCommandBuffersSemaphoresCopy wd iasmps rcsmps

createWindowCommandBuffersSemaphoresCreate ::
	Vk.Dvc.D -> TPMaybe.M Vk.AllocCallbacks.A mud -> Int ->
	IO ([Vk.Smp.S], [Vk.Smp.S])
createWindowCommandBuffersSemaphoresCreate (Vk.Dvc.D dvc) macs n =
	allocaArray n \iasmps -> allocaArray n \rcsmps -> do
		Vk.AllocCallbacks.mToCore macs \pacs ->
			C.createWindowCommandBuffersSemaphoresCreate
				dvc pacs (fromIntegral n) iasmps rcsmps
		(,)	<$> ((Vk.Smp.S <$>) <$> peekArray n iasmps)
			<*> ((Vk.Smp.S <$>) <$> peekArray n rcsmps)

createWindowCommandBuffersSemaphoresCopy ::
	Vk.ImGui.H.Win.W -> [Vk.Smp.S] -> [Vk.Smp.S] -> IO ()
createWindowCommandBuffersSemaphoresCopy wd iasmps rcsmps =
	allocaArray n \piasmps -> allocaArray n \prcsmps -> do
	pokeArray piasmps $ (\(Vk.Smp.S s) -> s) <$> iasmps
	pokeArray prcsmps $ (\(Vk.Smp.S s) -> s) <$> rcsmps
	C.createWindowCommandBuffersSemaphoresCopy
		wd (fromIntegral n) piasmps prcsmps
	where n = length iasmps

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
