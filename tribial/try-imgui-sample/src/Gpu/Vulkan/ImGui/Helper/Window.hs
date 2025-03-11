{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Window (
	WC(..), wCZero, wCZero', M.W(..),
	wCFromCxx, wCFromCxx', wCCopyToCxx, M.allocaW ) where

import Data.TypeLevel.Tuple.Uncurry
import Data.Default
import Data.List qualified as L
import Data.Word
import Data.Int
import Data.HeteroParList qualified as HPList
import Text.Show.ToolsYj

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Pipeline.Graphics.Type qualified as Vk.Ppl.Gr
import Gpu.Vulkan.RenderPass.Type qualified as Vk.RndrPss

import Gpu.Vulkan.Khr.Swapchain.Type qualified as Vk.Swpch
import Gpu.Vulkan.Khr.Surface qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.Middle.Internal qualified as Vk.Sfc.M
import Gpu.Vulkan.Khr.Surface.Internal qualified as Vk.Sfc

import Gpu.Vulkan.Middle.Internal qualified as Vk.M

import Gpu.Vulkan.ImGui.Helper.Frame qualified as Frame
import Gpu.Vulkan.ImGui.Helper.FrameSemaphores qualified as FrameSemaphores

import Gpu.Vulkan.ImGui.Helper.Window.Middle qualified as M

data WC scfmt ssc ssfc srp sppl vibs vias lyta ct
		bnm bfmt bvnm bvfmt fras frsmas = WC {
	wCWidth :: Int32, wCHeight :: Int32,
	wCSwapchain :: Vk.Swpch.S scfmt ssc,
	wCSurface :: Vk.Sfc.S ssfc,
	wCSurfaceFormat :: Vk.Sfc.Format scfmt,
	wCPresentMode :: Vk.Sfc.PresentMode,
	wCRenderPass :: Vk.RndrPss.R srp,
	wCPipeline :: Vk.Ppl.Gr.G sppl vibs vias lyta,
	wCUseDynamicRendering :: Bool,
	wCClearEnable :: Bool,
	wCClearValue :: Vk.ClearValue ct,
	wCFrameIndex :: Word32,
	wCImageCount :: Word32,
	wCSemaphoreCount :: Word32,
	wCSemaphoreIndex :: Word32,
	wCFrames :: HPList.PL (U7 (Frame.FC bnm bfmt bvnm bvfmt)) fras,
	wCFrameSemaphores :: HPList.PL (U2 FrameSemaphores.FC) frsmas }

instance Vk.T.FormatToValue scfmt => ShowIO
	(WC scfmt ssc ssfc srp sppl vibs vias lyta ct
		bnm bfmt bvnm bvfmt fras frsmas) where
	showIO wc = do
		sppl <- showIO $ wCPipeline wc
		sfrs <- sequence $ HPList.toList showIO (wCFrames wc)
		sfrsms <- sequence $ HPList.toList showIO (wCFrameSemaphores wc)
		pure $ "WC { " ++
			"wCWidth = " ++ show (wCWidth wc) ++ ", " ++
			"wCHeight = " ++ show (wCHeight wc) ++ ", " ++
			"wCSwapchain = " ++ show (wCSwapchain wc) ++ ", " ++
			"wCSurface = " ++ show (wCSurface wc) ++ ", " ++
			"wCSurfaceFormat = " ++
				show (wCSurfaceFormat wc) ++ ", " ++
			"wCPresentMode = " ++ show (wCPresentMode wc) ++ ", " ++
			"wCRenderPass = " ++ show (wCRenderPass wc) ++ ", " ++
			"wCPipeline = " ++ sppl ++ ", " ++
			"wCUseDynamicRendering = " ++
				show (wCUseDynamicRendering wc) ++ ", " ++
			"wCClearEnable = " ++ show (wCClearEnable wc) ++ ", " ++
			"wCClearValue = " ++ show (wCClearValue wc) ++ ", " ++
			"wCFrameIndex = " ++ show (wCFrameIndex wc) ++ ", " ++
			"wCImageCount = " ++ show (wCImageCount wc) ++ ", " ++
			"wCSemaphoreCount = " ++
				show (wCSemaphoreCount wc) ++ ", " ++
			"wCSemaphoreIndex = " ++
				show (wCSemaphoreIndex wc) ++ ", " ++
			"wCFrame = "  ++
				L.intercalate " :** " sfrs ++ "HPList.Nil, " ++
			"wCFrameSemaphores = " ++
				L.intercalate " :** " sfrsms ++ "HPList.Nil }"

wCToMiddle :: Vk.T.FormatToValue scfmt =>
	WC scfmt ssc ssfc srp sppl vibs vias lyta ct
		bnm bfmt bvnm bvfmt fras frsmas -> M.WC ct
wCToMiddle WC {
	wCWidth = wdt, wCHeight = hgt,
	wCSwapchain = Vk.Swpch.S sc,
	wCSurface = Vk.Sfc.S sfc, wCSurfaceFormat = fmt,
	wCPresentMode = pm, wCRenderPass = Vk.RndrPss.R rp,
	wCPipeline = Vk.Ppl.Gr.G ppl,
	wCUseDynamicRendering = udr,
	wCClearEnable = ce, wCClearValue = cv,
	wCFrameIndex = fi, wCImageCount = ic,
	wCSemaphoreCount = smc, wCSemaphoreIndex = smi,
	wCFrames = fs, wCFrameSemaphores = fss } = M.WC {
	M.wCWidth = wdt, M.wCHeight = hgt,
	M.wCSwapchain = sc,
	M.wCSurface = sfc , M.wCSurfaceFormat = Vk.Sfc.formatToMiddle fmt,
	M.wCPresentMode = pm, M.wCRenderPass = rp,
	M.wCPipeline = ppl,
	M.wCUseDynamicRendering = udr,
	M.wCClearEnable = ce, M.wCClearValue = cv,
	M.wCFrameIndex = fi, M.wCImageCount = ic,
	M.wCSemaphoreCount = smc, M.wCSemaphoreIndex = smi,
	M.wCFrames = HPList.toList (\(U7 f) -> Frame.fCToMiddle f) fs,
	M.wCFrameSemaphores =
		HPList.toList (\(U2 f) -> FrameSemaphores.fCToMiddle f) fss }

wCFromMiddle ::
	forall ssc ssfc srp
		sppl vibs vias lyta ct bnm bfmt bvnm bvfmt fras frsmas a . (
	Frame.FCListFromMiddle fras, FrameSemaphores.FCListFromMiddle frsmas ) =>
	M.WC ct -> (forall scfmt . WC scfmt ssc ssfc srp
		sppl vibs vias lyta ct bnm bfmt bvnm bvfmt fras frsmas -> a) -> a
wCFromMiddle M.WC {
	M.wCWidth = wdt, M.wCHeight = hgt,
	M.wCSwapchain = sc,
	M.wCSurface = sfc, M.wCSurfaceFormat = mfmt,
	M.wCPresentMode = pm,
	M.wCRenderPass = rp,
	M.wCPipeline = ppl,
	M.wCUseDynamicRendering = udr,
	M.wCClearEnable = ce, M.wCClearValue = cv,
	M.wCFrameIndex = fi, M.wCImageCount = ic,
	M.wCSemaphoreCount = smc, M.wCSemaphoreIndex = smi,
	M.wCFrames = fs, M.wCFrameSemaphores = fss
	} f = Vk.Sfc.formatFromMiddle mfmt \fmt -> f WC {
	wCWidth = wdt, wCHeight = hgt,
	wCSwapchain = Vk.Swpch.S sc,
	wCSurface = Vk.Sfc.S sfc, wCSurfaceFormat = fmt,
	wCPresentMode = pm,
	wCRenderPass = Vk.RndrPss.R rp,
	wCPipeline = Vk.Ppl.Gr.G ppl,
	wCUseDynamicRendering = udr,
	wCClearEnable = ce, wCClearValue = cv,
	wCFrameIndex = fi, wCImageCount = ic,
	wCSemaphoreCount = smc, wCSemaphoreIndex = smi,
	wCFrames = Frame.fCListFromMiddle fs,
	wCFrameSemaphores = FrameSemaphores.fCListFromMiddle fss }

wCFromCxx :: (
	Default (Vk.ClearValue ct),
	Frame.FCListFromMiddle fras,
	FrameSemaphores.FCListFromMiddle frsmas
	) =>
	M.W -> (forall scfmt . WC scfmt ssc ssfc srp
		sppl vibs vias lyta ct bnm bfmt bvnm bvfmt fras frsmas -> IO a) -> IO a
wCFromCxx cxx f = do
	m <- M.wCFromCxx cxx
	wCFromMiddle m f

wCCopyToCxx ::
	(Vk.M.ClearValueToCore ct, Vk.T.FormatToValue scfmt) =>
	WC scfmt ssc ssfc srp
		sppl vibs vias lyta ct bnm bfmt bvnm bvfmt fras frsmas -> M.W ->
	IO a -> IO a
wCCopyToCxx = M.wCCopyToCxx . wCToMiddle

wCZero :: Default (Vk.M.ClearValue ct) => (forall scfmt fras frsmas . Vk.T.FormatToValue scfmt =>
		WC scfmt ssc ssfc srp
			sppl vibs vias lyta ct bnm bfmt bvnm bvfmt fras frsmas -> IO a) -> IO a
wCZero f = (`wCFromMiddle'` f) =<< M.wCZero

wCZero' :: (
	Vk.T.FormatToValue scfmt,
	Default (Vk.M.ClearValue ct)
	) => (forall fras frsmas . Vk.T.FormatToValue scfmt =>
		WC scfmt ssc ssfc srp
			sppl vibs vias lyta ct bnm bfmt bvnm bvfmt fras frsmas -> IO a) -> IO a
wCZero' f = (`wCFromMiddle''` f) =<< M.wCZero

wCFromMiddle' ::
	forall ssc ssfc srp
		sppl vibs vias lyta ct bnm bfmt bvnm bvfmt a .
	M.WC ct -> (forall scfmt fras frsmas . Vk.T.FormatToValue scfmt =>
		WC scfmt ssc ssfc srp
			sppl vibs vias lyta ct bnm bfmt bvnm bvfmt fras frsmas -> a) -> a
wCFromMiddle' M.WC {
	M.wCWidth = wdt, M.wCHeight = hgt,
	M.wCSwapchain = sc,
	M.wCSurface = sfc, M.wCSurfaceFormat = mfmt,
	M.wCPresentMode = pm,
	M.wCRenderPass = rp,
	M.wCPipeline = ppl,
	M.wCUseDynamicRendering = udr,
	M.wCClearEnable = ce, M.wCClearValue = cv,
	M.wCFrameIndex = fi, M.wCImageCount = ic,
	M.wCSemaphoreCount = smc, M.wCSemaphoreIndex = smi,
	M.wCFrames = fs, M.wCFrameSemaphores = fss } f =
	Vk.Sfc.formatFromMiddle mfmt \fmt ->
	Frame.fCListFromMiddle' fs \fs' ->
	FrameSemaphores.fCListFromMiddle' fss \fss' ->
	f WC {
		wCWidth = wdt, wCHeight = hgt,
		wCSwapchain = Vk.Swpch.S sc,
		wCSurface = Vk.Sfc.S sfc, wCSurfaceFormat = fmt,
		wCPresentMode = pm,
		wCRenderPass = Vk.RndrPss.R rp,
		wCPipeline = Vk.Ppl.Gr.G ppl,
		wCUseDynamicRendering = udr,
		wCClearEnable = ce, wCClearValue = cv,
		wCFrameIndex = fi, wCImageCount = ic,
		wCSemaphoreCount = smc, wCSemaphoreIndex = smi,
		wCFrames = fs', wCFrameSemaphores = fss' }

wCFromMiddle'' ::
	forall scfmt ssc ssfc srp
		sppl vibs vias lyta ct bnm bfmt bvnm bvfmt a .
	Vk.T.FormatToValue scfmt =>
	M.WC ct -> (forall fras frsmas . Vk.T.FormatToValue scfmt =>
		WC scfmt ssc ssfc srp
			sppl vibs vias lyta ct bnm bfmt bvnm bvfmt fras frsmas -> a) -> a
wCFromMiddle'' M.WC {
	M.wCWidth = wdt, M.wCHeight = hgt,
	M.wCSwapchain = sc,
	M.wCSurface = sfc, M.wCSurfaceFormat = Vk.Sfc.M.Format _ cs,
	M.wCPresentMode = pm,
	M.wCRenderPass = rp,
	M.wCPipeline = ppl,
	M.wCUseDynamicRendering = udr,
	M.wCClearEnable = ce, M.wCClearValue = cv,
	M.wCFrameIndex = fi, M.wCImageCount = ic,
	M.wCSemaphoreCount = smc, M.wCSemaphoreIndex = smi,
	M.wCFrames = fs, M.wCFrameSemaphores = fss } f =
	Frame.fCListFromMiddle' fs \fs' ->
	FrameSemaphores.fCListFromMiddle' fss \fss' ->
	f WC {
		wCWidth = wdt, wCHeight = hgt,
		wCSwapchain = Vk.Swpch.S sc,
		wCSurface = Vk.Sfc.S sfc, wCSurfaceFormat = Vk.Sfc.Format cs,
		wCPresentMode = pm,
		wCRenderPass = Vk.RndrPss.R rp,
		wCPipeline = Vk.Ppl.Gr.G ppl,
		wCUseDynamicRendering = udr,
		wCClearEnable = ce, wCClearValue = cv,
		wCFrameIndex = fi, wCImageCount = ic,
		wCSemaphoreCount = smc, wCSemaphoreIndex = smi,
		wCFrames = fs', wCFrameSemaphores = fss' }

wCFromCxx' :: (Default (Vk.ClearValue ct)) =>
	M.W -> (forall scfmt fras frsmas . Vk.T.FormatToValue scfmt =>
		WC scfmt ssc ssfc srp
			sppl vibs vias lyta ct bnm bfmt bvnm bvfmt fras frsmas -> IO a) -> IO a
wCFromCxx' cxx f = do
	m <- M.wCFromCxx cxx
	wCFromMiddle' m f
