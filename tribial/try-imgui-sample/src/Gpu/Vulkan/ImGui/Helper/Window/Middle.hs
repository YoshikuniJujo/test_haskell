{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Window.Middle (
	WC(..), wCFromCxx, wCCopyToCxx ) where

import Foreign.Marshal.Array
import Control.Arrow
import Control.Monad
import Data.Default
import Data.List qualified as L
import Data.Word
import Data.Int
import Data.IORef
import Text.Show.ToolsYj

import Gpu.Vulkan.Middle.Internal qualified as Vk
import Gpu.Vulkan.Pipeline.Graphics.Middle.Internal qualified as Vk.Ppl.Gr
import Gpu.Vulkan.RenderPass.Middle.Internal qualified as  Vk.RndrPss

import Gpu.Vulkan.Khr.Swapchain.Middle.Internal qualified as Vk.Swpch
import Gpu.Vulkan.Khr.Surface.Enum qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.Middle.Internal qualified as Vk.Sfc

import Gpu.Vulkan.ImGui.Helper.Frame.Middle qualified as Frame
import Gpu.Vulkan.ImGui.Helper.FrameSemaphores.Middle qualified as FrameSemaphores

import Gpu.Vulkan.ImGui.Helper.Window.Core qualified as C

data WC ct = WC {
	wCWidth :: Int32, wCHeight :: Int32,
	wCSwapchain :: Vk.Swpch.S,
	wCSurface :: Vk.Sfc.S,
	wCSurfaceFormat :: Vk.Sfc.Format,
	wCPresentMode :: Vk.Sfc.PresentMode,
	wCRenderPass :: Vk.RndrPss.R,
	wCPipeline :: Vk.Ppl.Gr.G,
	wCUseDynamicRendering :: Bool,
	wCClearEnable :: Bool,
	wCClearValue :: Vk.ClearValue ct,
	wCFrameIndex :: Word32,
	wCImageCount :: Word32,
	wCSemaphoreCount :: Word32,
	wCSemaphoreIndex :: Word32,
	wCFrames :: [Frame.FC],
	wCFrameSemaphores :: [FrameSemaphores.FC] }

instance ShowIO (WC ct) where
	showIO w = do
		sppl <- showIO $ wCPipeline w
		sfs <- showIO `mapM` wCFrames w
		sfss <- showIO `mapM` wCFrameSemaphores w
		pure $	"WC { wCWidth = " ++ show (wCWidth w) ++
			", wCHeight = " ++ show (wCHeight w) ++
			", wCSwapchain = " ++ show (wCSwapchain w) ++
			", wCSurface = " ++ show (wCSurface w) ++
			", wCSurfaceFormat = " ++ show (wCSurfaceFormat w) ++
			", wCPresentMode = " ++ show (wCPresentMode w) ++
			", wCRenderPass = " ++ show (wCRenderPass w) ++
			", wCPipeline = " ++ sppl ++
			", wCUseDynamicRendering = " ++
				show (wCUseDynamicRendering w) ++
			", wCClearEnable = " ++ show (wCClearEnable w) ++
			", wCClearValue = " ++ show (wCClearValue w) ++
			", wCFrameIndex = " ++ show (wCFrameIndex w) ++
			", wCImageCount = " ++ show (wCImageCount w) ++
			", wCSemaphoreCount = " ++ show (wCSemaphoreCount w) ++
			", wCSemaphoreIndex = " ++ show (wCSemaphoreIndex w) ++
			", wCFrames = [" ++ L.intercalate ", " sfs ++ "]" ++
			", wCFrameSemaphores = [" ++ L.intercalate ", " sfss ++ "] }"

wCToCore :: Vk.ClearValueToCore ct => WC ct -> (C.WC -> IO a) -> IO a
wCToCore WC {
	wCWidth = w, wCHeight = h,
	wCSwapchain = Vk.Swpch.S rsc,
	wCSurface = Vk.Sfc.S sfc, wCSurfaceFormat = sfmt,
	wCPresentMode = Vk.Sfc.PresentMode pm,
	wCRenderPass = Vk.RndrPss.R rp,
	wCPipeline = Vk.Ppl.Gr.G rppl,
	wCUseDynamicRendering = udr,
	wCClearEnable = ce, wCClearValue = cv,
	wCFrameIndex = fi, wCImageCount = ic,
	wCSemaphoreCount = scc, wCSemaphoreIndex = si,
	wCFrames = length &&& id -> (fc, fs),
	wCFrameSemaphores = length &&& id -> (fsc, fss)
	} a = Vk.clearValueToCore cv \ccv -> do
	pfs <- mallocArray fc
	pfss <- mallocArray fsc
--	allocaArray fc \pfs -> allocaArray fsc \pfss -> do
	(_, sc) <- readIORef rsc
	ppl <- readIORef rppl
	pokeArray pfs =<< Frame.fcToCore `mapM` fs
	pokeArray pfss $ FrameSemaphores.fCToCore <$> fss
	a C.WC {
		C.wCWidth = w, C.wCHeight = h,
		C.wCSwapchain = sc,
		C.wCSurface = sfc, C.wCSurfaceFormat = Vk.Sfc.formatToCore sfmt,
		C.wCPresentMode = pm,
		C.wCRenderPass = rp,
		C.wCPipeline = ppl,
		C.wCUseDynamicRendering = if udr then 1 else 0,
		C.wCClearEnable = if ce then 1 else 0,
		C.wCClearValue = ccv,
		C.wCFrameIndex = fi, C.wCImageCount = ic,
		C.wCSemaphoreCount = scc, C.wCSemaphoreIndex = si,
		C.wCFramec = fromIntegral fc,
		C.wCPFrames = pfs,
		C.wCFrameSemaphorec = fromIntegral fsc,
		C.wCPFrameSemaphores = pfss }

wCFromCore :: Default (Vk.ClearValue ct) => C.WC -> IO (WC ct)
wCFromCore C.WC {
	C.wCWidth = w, C.wCHeight = h,
	C.wCSwapchain = sc,
	C.wCSurface = sfc, C.wCSurfaceFormat = sfmt,
	C.wCPresentMode = pm,
	C.wCRenderPass = rp,
	C.wCPipeline = ppl,
	C.wCUseDynamicRendering = udr,
	C.wCClearEnable = ce,
	C.wCClearValue = cv,
	C.wCFrameIndex = fi,
	C.wCImageCount = ic,
	C.wCSemaphoreCount = scc,
	C.wCSemaphoreIndex = si,
	C.wCFramec = fc,
	C.wCPFrames = pfs,
	C.wCFrameSemaphorec = fsc,
	C.wCPFrameSemaphores = pfss } = do
	rsc <- newIORef (Vk.Extent2d 0 0, sc)
	rppl <- newIORef ppl
	fs <- mapM Frame.fcFromCore =<< peekArray (fromIntegral fc) pfs
	fss <- (FrameSemaphores.fCFromCore <$>)
		<$> peekArray (fromIntegral fsc) pfss
	pure WC {
		wCWidth = w, wCHeight = h,
		wCSwapchain = Vk.Swpch.S rsc,
		wCSurface = Vk.Sfc.S sfc,
		wCSurfaceFormat = Vk.Sfc.formatFromCore sfmt,
		wCPresentMode = Vk.Sfc.PresentMode pm,
		wCRenderPass = Vk.RndrPss.R rp,
		wCPipeline = Vk.Ppl.Gr.G rppl,
		wCUseDynamicRendering = udr /= 0,
		wCClearEnable = ce /= 0,
		wCClearValue = def,
		wCFrameIndex = fi,
		wCImageCount = ic,
		wCSemaphoreCount = scc,
		wCSemaphoreIndex = si,
		wCFrames = fs,
		wCFrameSemaphores = fss }

wCCopyToCxx :: Vk.ClearValueToCore ct => WC ct -> C.W -> IO a -> IO a
wCCopyToCxx wc w a = wCToCore wc \cwc -> copyToCxx cwc w >> a

wCFromCxx :: Default (Vk.ClearValue ct) => C.W -> IO (WC ct)
wCFromCxx = wCFromCore <=< fromCxx'

fromCxx' :: C.W -> IO C.WC
fromCxx' = C.toC

copyToCxx :: C.WC -> C.W -> IO ()
copyToCxx = C.copyFromC
