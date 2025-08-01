{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Monad
import Control.Concurrent
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Bits
import Data.Bits.ToolsYj
import Data.Default
import Data.Maybe
import Data.Maybe.ToolsYj
import Data.List qualified as L
import Data.List.ToolsYj
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.HeteroParList.Constrained qualified as HPListC
import Data.Bool
import Data.Bool.ToolsYj
import Data.Word
import Data.Int
import Data.Text.IO qualified as Txt
import Data.Color
import Text.Show.ToolsYj
import System.IO

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.AllocationCallbacks qualified as Vk.AllocCallbacks
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Instance.Internal qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device.Internal qualified as Vk.Dvc
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorPool.Type qualified as Vk.DscPl
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Component qualified as Vk.Cmp

import Gpu.Vulkan.Attachment qualified as Vk.Att
import Gpu.Vulkan.Sample qualified as Vk.Smpl
import Gpu.Vulkan.Subpass qualified as Vk.Sbpss
import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPss

import Gpu.Vulkan.Framebuffer qualified as Vk.Frmbffr

import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Swpch
import Gpu.Vulkan.Khr.Surface qualified as Vk.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Sfc.Win

import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.DbgUtls.Msngr

import Gpu.Vulkan.Instance.Middle.Internal qualified as Vk.Ist.M
import Gpu.Vulkan.PhysicalDevice.Middle.Internal qualified as Vk.Phd.M
import Gpu.Vulkan.Queue.Middle.Internal qualified as Vk.Q.M
import Gpu.Vulkan.QueueFamily.Middle qualified as Vk.QFam.M
import Gpu.Vulkan.Device.Middle.Internal qualified as Vk.Dvc.M
import Gpu.Vulkan.DescriptorPool.Middle.Internal qualified as Vk.DscPl.M

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win

import Gpu.Vulkan.ImGui qualified as Vk.ImGui
import Gpu.Vulkan.ImGui.NoVulkan qualified as ImGui
import Gpu.Vulkan.ImGui.NoVulkan.Io qualified as ImGui.Io
import Gpu.Vulkan.ImGui.NoVulkan.Style.Colors qualified as ImGui.Style.Colors
import Gpu.Vulkan.ImGui.NoVulkan.FontAtlas qualified as ImGui.FontAtlas
import Gpu.Vulkan.ImGui.NoVulkan.Glfw qualified as ImGui.Glfw
import Gpu.Vulkan.ImGui.NoVulkan.Demo qualified as ImGui.Demo
import Gpu.Vulkan.ImGui.Glfw qualified as Vk.ImGui.Glfw
import Gpu.Vulkan.ImGui.Helper qualified as Vk.ImGui.H
import Gpu.Vulkan.ImGui.Helper.Window qualified as Vk.ImGui.Win

import Gpu.Vulkan.Middle qualified as Vk.M
import Gpu.Vulkan.ImGui.Core qualified as Vk.ImGui.C

import Debug qualified
import OldLog qualified
import AppUseUnlimitedFrameRate qualified

import Gpu.Vulkan.ImGui.NoVulkan.FontAtlas.Middle qualified as ImGui.FontAtlas.M
import Gpu.Vulkan.ImGui.NoVulkan.FontAtlas.Core qualified as ImGui.FontAtlas.C

debug, oldLog :: Bool
debug = Debug.flag
oldLog = OldLog.flag

main :: IO ()
main = (GlfwG.setErrorCallback (Just glfwErrorCallback) >>) .
	GlfwG.init error $
	GlfwG.Win.hint
		(GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI) >>
	GlfwG.Win.create 1280 720
		"Dear ImGui GLFW+Vulkan example" Nothing Nothing \win -> do
	vs <- GlfwG.vulkanSupported
	when (not vs) $ error "GLFW: Vulkan Not Supported"
	when oldLog $ print AppUseUnlimitedFrameRate.flag
	createIst \ist -> Vk.Sfc.Win.create ist win nil \sfc ->
		pickPhd ist sfc >>= \(phd, qfm) ->
		createLgDvc phd qfm \dvc gq _ ->
		createDscPl dvc \dp ->
		Vk.Sfc.Phd.getSupport phd (grFam qfm) sfc >>= \res ->
		when (not res) (error "Error no WSI support on physicalDevice 0") >> do
		mainCxx win ist sfc phd (grFam qfm) dvc gq dp

glfwErrorCallback :: GlfwG.Error -> GlfwG.ErrorMessage -> IO ()
glfwErrorCallback err dsc =
	hPutStrLn stderr $ "GLFW Error " ++ show err ++ ": " ++ dsc

foreign import ccall "FrameRender" cxx_FrameRender ::
	Vk.ImGui.Win.W -> Vk.Dvc.D sd -> Vk.Q.Q ->
	ImGui.DrawData -> Ptr Word8 -> IO ()

foreign import ccall "FramePresent" cxx_FramePresent ::
	Vk.ImGui.Win.W -> Vk.Q.Q -> Ptr Word8 -> IO ()

foreign import ccall "simpleWindowBody" cxx_simpleWindowBody ::
	ImGui.Io.I -> Ptr Word8 -> Ptr Word8 -> Ptr Float -> IO ()

foreign import ccall "anotherWindow" cxx_anotherWindow ::
	Ptr Word8 -> IO ()

foreign import ccall "resizeSwapchain" cxx_resizeSwapchain ::
	Vk.Ist.I si -> Vk.Phd.P -> Vk.QFam.Index -> Vk.Dvc.D sd -> Vk.ImGui.Win.W ->
	Ptr Word8 -> Int32 -> Int32 -> IO ()

foreign import ccall "cleanup" cxx_cleanup ::
	Vk.Ist.I si -> Vk.Dvc.D sd -> Vk.ImGui.Win.W -> IO ()

foreign import ccall "new_ImGui_ImplVulkan_InitInfo"
	cxx_new_ImGui_ImplVulkan_InitInfo :: IO Vk.ImGui.InitInfoCxx

foreign import ccall "free_ImGui_ImplVulkan_InitInfo"
	cxx_free_ImGui_ImplVulkan_InitInfo ::
	Vk.ImGui.InitInfoCxx -> IO ()

foreign import ccall "initialize_ImGui_ImplVulkan_InitInfo"
	cxx_initialize_ImGui_ImplVulkan_InitInfo ::
	Vk.ImGui.InitInfoCxx -> Vk.Ist.I si -> Vk.Phd.P ->
	Vk.QFam.Index -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.DscPl.P sdp ->
	Vk.ImGui.Win.W -> IO ()

mainCxx ::
	GlfwG.Win.W sw -> Vk.Ist.I si -> Vk.Sfc.S ss -> Vk.Phd.P ->
	Vk.QFam.Index -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.DscPl.P sdp -> IO ()
mainCxx w ist sfc phd qfi dvc gq dp =
	let	rqSfcImgFmt = [
			Vk.FormatB8g8r8a8Unorm, Vk.FormatR8g8b8a8Unorm,
			Vk.FormatB8g8r8Unorm, Vk.FormatR8g8b8Unorm ] in
	Vk.ImGui.H.selectSurfaceFormat phd sfc rqSfcImgFmt Vk.Sfc.ColorSpaceSrgbNonlinear \(sfmt :: Vk.Sfc.Format fmt2) ->
	let	pms = bool
			[Vk.Sfc.PresentModeFifo]
			[	Vk.Sfc.PresentModeMailbox,
				Vk.Sfc.PresentModeImmediate,
				Vk.Sfc.PresentModeFifo ]
			AppUseUnlimitedFrameRate.flag in
	Vk.ImGui.H.selectPresentMode phd sfc pms \pm ->
	GlfwG.Win.getFramebufferSize w >>= \(fromIntegral -> wdt, fromIntegral -> hgt) ->
	Vk.Sfc.Phd.getCapabilities phd sfc >>= \cap ->

	Vk.ImGui.Win.wCZero' @_ @(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32) \z ->
	let	extnt = Vk.Sfc.capabilitiesCurrentExtent cap
		(wdt', hgt') = if Vk.extent2dWidth extnt == 0xffffffff
			then (wdt, hgt)
			else (fromIntegral $ Vk.extent2dWidth extnt, fromIntegral $ Vk.extent2dHeight extnt)
		z' = z {
			Vk.ImGui.Win.wCWidth = wdt',
			Vk.ImGui.Win.wCHeight = hgt',
			Vk.ImGui.Win.wCSurface = sfc,
			Vk.ImGui.Win.wCSurfaceFormat = sfmt,
			Vk.ImGui.Win.wCClearEnable = True,
			Vk.ImGui.Win.wCPresentMode = pm
			} in

	putStrLn "OOOOOPS" >>

	let	minImageCount = 2
		minImageCountNew =
			if minImageCount < Vk.Sfc.capabilitiesMinImageCount cap
			then Vk.Sfc.capabilitiesMinImageCount cap
			else if Vk.Sfc.capabilitiesMaxImageCount cap /= 0 &&
				minImageCount >
					Vk.Sfc.capabilitiesMaxImageCount cap
			then Vk.Sfc.capabilitiesMaxImageCount cap
			else minImageCount
		swpchInfo :: Vk.Swpch.CreateInfo _ _ fmt2 _
		swpchInfo = Vk.Swpch.CreateInfo {
			Vk.Swpch.createInfoNext = TMaybe.N,
			Vk.Swpch.createInfoFlags = zeroBits,
			Vk.Swpch.createInfoSurface = Vk.ImGui.Win.wCSurface z',
			Vk.Swpch.createInfoMinImageCount = minImageCountNew,
			Vk.Swpch.createInfoImageColorSpace =
				Vk.Sfc.formatColorSpace
					$ Vk.ImGui.Win.wCSurfaceFormat z',
			Vk.Swpch.createInfoImageExtent = Vk.Extent2d
				(fromIntegral $ Vk.ImGui.Win.wCWidth z')
				(fromIntegral $ Vk.ImGui.Win.wCHeight z'),
			Vk.Swpch.createInfoImageArrayLayers = 1,
			Vk.Swpch.createInfoImageUsage =
				Vk.Img.UsageColorAttachmentBit,
			Vk.Swpch.createInfoImageSharingMode =
				Vk.SharingModeExclusive,
			Vk.Swpch.createInfoQueueFamilyIndices = [],
			Vk.Swpch.createInfoPreTransform =
				Vk.Sfc.TransformIdentityBit,
			Vk.Swpch.createInfoCompositeAlpha =
				Vk.Sfc.CompositeAlphaOpaqueBit,
			Vk.Swpch.createInfoPresentMode =
				Vk.ImGui.Win.wCPresentMode z',
			Vk.Swpch.createInfoClipped = True,
			Vk.Swpch.createInfoOldSwapchain =
	--			TPMaybe.N } in
				TPMaybe.J . U2 $ Vk.ImGui.Win.wCSwapchain z' } in
	Vk.Swpch.create dvc swpchInfo nil \sc ->

	Vk.ImGui.checkVersion >>
	Vk.ImGui.createContextNoArg >>
	ImGui.Io.get >>= \io ->
	ImGui.Io.modifyConfigFlags io
		(.|. ImGui.ConfigFlagsNavEnableKeyboard) >>
	ImGui.Io.modifyConfigFlags io
		(.|. ImGui.ConfigFlagsNavEnableGamepad) >>
	ImGui.Style.Colors.darkNoArg >>
--	ImGui.Style.Colors.lightNoArg >>
--	ImGui.Style.Colors.classicNoArg >>
	Vk.ImGui.Glfw.init w True >>
	Vk.Swpch.getImages dvc sc >>= \scis ->

	let	attdsc :: Vk.Att.Description fmt2
		attdsc = Vk.Att.Description {
			Vk.Att.descriptionFlags = zeroBits,
			Vk.Att.descriptionSamples = Vk.Smpl.Count1Bit,
			Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
			Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
			Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOp = Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayout = Vk.Img.LayoutUndefined,
			Vk.Att.descriptionFinalLayout = Vk.Img.LayoutPresentSrcKhr }
		coloratt :: Vk.Att.Reference
		coloratt = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 0,
			Vk.Att.referenceLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
		sbpss :: Vk.Sbpss.Description
		sbpss = Vk.Sbpss.Description {
			Vk.Sbpss.descriptionFlags = zeroBits,
			Vk.Sbpss.descriptionPipelineBindPoint =
				Vk.Ppl.BindPointGraphics,
			Vk.Sbpss.descriptionInputAttachments = [],
			Vk.Sbpss.descriptionColorAndResolveAttachments =
				Left [coloratt],
			Vk.Sbpss.descriptionDepthStencilAttachment = Nothing,
			Vk.Sbpss.descriptionPreserveAttachments = [] }
		dpdcy :: Vk.Sbpss.Dependency
		dpdcy = Vk.Sbpss.Dependency {
			Vk.Sbpss.dependencySrcSubpass = Vk.Sbpss.SExternal,
			Vk.Sbpss.dependencyDstSubpass = 0,
			Vk.Sbpss.dependencySrcStageMask =
				Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.Sbpss.dependencyDstStageMask =
				Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.Sbpss.dependencySrcAccessMask = zeroBits,
			Vk.Sbpss.dependencyDstAccessMask =
				Vk.AccessColorAttachmentWriteBit,
			Vk.Sbpss.dependencyDependencyFlags = zeroBits }
		rndrpssInfo :: Vk.RndrPss.CreateInfo 'Nothing '[fmt2]
		rndrpssInfo = Vk.RndrPss.CreateInfo {
			Vk.RndrPss.createInfoNext = TMaybe.N,
			Vk.RndrPss.createInfoFlags = zeroBits,
			Vk.RndrPss.createInfoAttachments = HPList.Singleton attdsc,
			Vk.RndrPss.createInfoSubpasses = [sbpss],
			Vk.RndrPss.createInfoDependencies = [dpdcy] }
		imgvwInfo ::
			Vk.Img.Binded sm si nm fmt2 ->
			Vk.ImgVw.CreateInfo 'Nothing sm si nm fmt2 fmt2
		imgvwInfo i = Vk.ImgVw.CreateInfo {
			Vk.ImgVw.createInfoNext = TMaybe.N,
			Vk.ImgVw.createInfoFlags = zeroBits,
			Vk.ImgVw.createInfoImage = i,
			Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
			Vk.ImgVw.createInfoComponents = Vk.Cmp.Mapping {
				Vk.Cmp.mappingR = Vk.Cmp.SwizzleR,
				Vk.Cmp.mappingG = Vk.Cmp.SwizzleG,
				Vk.Cmp.mappingB = Vk.Cmp.SwizzleB,
				Vk.Cmp.mappingA = Vk.Cmp.SwizzleA },
			Vk.ImgVw.createInfoSubresourceRange =
				Vk.Img.SubresourceRange {
					Vk.Img.subresourceRangeAspectMask =
						Vk.Img.AspectColorBit,
					Vk.Img.subresourceRangeBaseMipLevel = 0,
					Vk.Img.subresourceRangeLevelCount = 1,
					Vk.Img.subresourceRangeBaseArrayLayer =
						0,
					Vk.Img.subresourceRangeLayerCount = 1 }
			}
		in

	Vk.RndrPss.create dvc rndrpssInfo nil \rp ->
	createImageViews dvc imgvwInfo scis \scvs ->
	Vk.Frmbffr.group dvc nil \fbg ->
	let	fbinfos = mkFramebufferInfoList rp scvs (fromIntegral wdt') (fromIntegral hgt') in
	createFramebufferList fbg [0 ..] fbinfos >>= \fbs ->

	Vk.ImGui.Win.allocaW \wdcxx ->
	Vk.ImGui.Win.wCCopyToCxx z' wdcxx $
	Vk.ImGui.H.copySwapChainToWd wdcxx sc >>
	pure () >>= \() ->
	Vk.ImGui.H.createSwapChainModifyWd wdcxx scis $
	Vk.ImGui.H.setWdRenderPass wdcxx rp >>
	Vk.ImGui.H.copyImageViewsToWd' wdcxx scvs >>
	Vk.ImGui.H.copyFramebufferToWd False wdcxx fbs >>

	Vk.ImGui.H.createWindowCommandBuffers phd dvc wdcxx qfi nil >>

	cxx_new_ImGui_ImplVulkan_InitInfo >>= \pInitInfo -> do
	cxx_initialize_ImGui_ImplVulkan_InitInfo
		pInitInfo ist phd qfi dvc gq dp wdcxx
--	initInfo <- Vk.ImGui.initInfoFromCxx @'Nothing pInitInfo
--	printIO initInfo
--	Vk.ImGui.copyInitInfoToCxx initInfo pInitInfo
	print =<< Vk.ImGui.C.cxx_imgui_impl_vulkan_init pInitInfo
	let	fa@(ImGui.FontAtlas.M.F pfa) = ImGui.Io.fonts io
		grsj = ImGui.FontAtlas.getGlyphRangesJapanese fa
	print $ length grsj
	alloca \pn -> do
		print =<< ImGui.FontAtlas.C.cxx_im_font_atlas_sources pfa pn
		print =<< peek pn
	mfont <- ImGui.FontAtlas.addFontFromFileTtf fa "/usr/share/fonts/mikachan-font-ttf/mikachan.ttf" 18.0 Nothing
		(Just $ ImGui.FontAtlas.getGlyphRangesJapanese fa)
	alloca \psdw -> alloca \psow -> allocaArray 4 \pcc -> alloca \pscr -> do
		poke psdw 1
		poke psow 0
		pokeArray pcc [0.45, 0.55, 0.60, 1.00]
		poke pscr 0
		untilClose w do
			GlfwG.pollEvents
			Vk.ImGui.Win.wCFromCxx' @(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32) wdcxx \wd -> do
				(fromIntegral -> fbwdt, fromIntegral -> fbhgt) <- GlfwG.Win.getFramebufferSize w
				scr <- peek pscr
				when (	fbwdt > 0 && fbhgt > 0 &&
					(scr /= 0 || Vk.ImGui.Win.wCWidth wd /= fbwdt || Vk.ImGui.Win.wCHeight wd /= fbhgt) ) do
					cxx_resizeSwapchain ist phd qfi dvc wdcxx pscr fbwdt fbhgt
				icnd <- GlfwG.Win.getIconified w
				if icnd then threadDelay 10000 else do
					Vk.ImGui.newFrame
					ImGui.Glfw.newFrame
					ImGui.newFrame
					sdw <- (/= 0) <$> peek psdw
					when sdw do
						sdw' <- ImGui.Demo.showWindow sdw
						poke psdw $ bool 0 1 sdw'
					_ <- ImGui.begin @() "Hello, Haskell world!" ImGui.windowFlagsZero
						\_ -> cxx_simpleWindowBody io psdw psow pcc
					sow <- (/= 0) <$> peek psow
					when sow $ cxx_anotherWindow psow
					ImGui.render
					dd <- ImGui.getDrawData
					let	(wdtds, hgtds) = ImGui.drawDataDisplaySize dd
						isMinimized = wdtds <= 0 || hgtds <= 0
					Vk.ImGui.Win.wCFromCxx'
						@(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32) wdcxx \wd' ->
						when (not isMinimized) do
							[r, g, b, a] <- peekArray 4 pcc
							let	wd'' = wd' {
									Vk.ImGui.Win.wCClearValue =
										Vk.ClearValueColor . fromJust
											$ rgbaDouble (r * a) (g * a) (b * a) a :: Vk.ClearValue (Vk.ClearTypeColor Vk.ClearColorTypeFloat32) }
							Vk.ImGui.Win.wCCopyToCxx wd'' wdcxx do
								cxx_FrameRender wdcxx dvc gq dd pscr
								cxx_FramePresent wdcxx gq pscr
	cxx_cleanup ist dvc wdcxx
	cxx_free_ImGui_ImplVulkan_InitInfo pInitInfo

data FramebufferInfos sr nm fmt sis where
	FramebufferInfosNil :: FramebufferInfos sr nm fmt '[]
	FramebufferInfos ::
		Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(nm, fmt, si)] ->
		FramebufferInfos sr nm fmt sis ->
		FramebufferInfos sr nm fmt ( si ': nmfmtsis)

mkFramebufferInfoList ::
	Vk.RndrPss.R sr -> HPList.PL (Vk.ImgVw.I nm fmt) sis -> Word32 -> Word32 ->
	FramebufferInfos sr nm fmt sis
mkFramebufferInfoList _ HPList.Nil _ _ = FramebufferInfosNil
mkFramebufferInfoList rp (iv :** ivs) wdt hgt = FramebufferInfos
	(mkFramebufferInfo rp iv wdt hgt) (mkFramebufferInfoList rp ivs wdt hgt)

mkFramebufferInfo ::
	Vk.RndrPss.R sr -> Vk.ImgVw.I nm fmt si -> Word32 -> Word32 ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(nm, fmt, si)]
mkFramebufferInfo rp iv wdt hgt = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = HPList.Singleton $ U3 iv,
	Vk.Frmbffr.createInfoWidth = wdt,
	Vk.Frmbffr.createInfoHeight = hgt,
	Vk.Frmbffr.createInfoLayers = 1 }

createFramebufferList :: (Ord k, Vk.AllocCallbacks.ToMiddle ma) =>
	Vk.Frmbffr.Group sd ma sf k -> [k] -> FramebufferInfos sr nm fmt sis ->
	IO [Vk.Frmbffr.F sf]
createFramebufferList _ _ FramebufferInfosNil = pure []
createFramebufferList grp (k : ks) (FramebufferInfos info infos) = (:) . (\(Right r) -> r)
	<$> Vk.Frmbffr.create' grp k info
	<*> createFramebufferList grp ks infos

untilClose :: GlfwG.Win.W sw -> IO a -> IO ()
untilClose w a = bool (a >> untilClose w a) (pure ())
	=<< GlfwG.Win.shouldClose w

createIst :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createIst f = do
	errorIf emsg . (debug &&) . elemNotAll vldLayers
		. (Vk.layerPropertiesLayerName <$>)
		=<< Vk.Ist.enumerateLayerProperties
	exts <- bool id (Vk.DbgUtls.extensionName :) debug
		. (Vk.Ist.ExtensionName <$>)
		<$> GlfwG.getRequiredInstanceExtensions
	bool	(Vk.Ist.create (info exts) nil f)
		(Vk.Ist.create (infoDbg exts) nil f) debug
	where
	emsg = "validation layers requested, but not available!"
	info exts = Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.N,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo = Just ainfo,
		Vk.Ist.createInfoEnabledLayerNames = [],
		Vk.Ist.createInfoEnabledExtensionNames = exts }
	infoDbg exts = Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.J dbgMsngrInfo,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo = Just ainfo,
		Vk.Ist.createInfoEnabledLayerNames = vldLayers,
		Vk.Ist.createInfoEnabledExtensionNames = exts }
	ainfo = Vk.ApplicationInfo {
		Vk.applicationInfoNext = TMaybe.N,
		Vk.applicationInfoApplicationName = "Hello Triangle",
		Vk.applicationInfoApplicationVersion =
			Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoEngineName = "No Engine",
		Vk.applicationInfoEngineVersion = Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoApiVersion = Vk.apiVersion_1_0 }

vldLayers :: [Vk.LayerName]
vldLayers = [Vk.layerKhronosValidation]

dbgMsngrInfo :: Vk.DbgUtls.Msngr.CreateInfo 'Nothing '[] ()
dbgMsngrInfo = Vk.DbgUtls.Msngr.CreateInfo {
	Vk.DbgUtls.Msngr.createInfoNext = TMaybe.N,
	Vk.DbgUtls.Msngr.createInfoFlags = zeroBits,
	Vk.DbgUtls.Msngr.createInfoMessageSeverity =
		Vk.DbgUtls.MessageSeverityVerboseBit .|.
		Vk.DbgUtls.MessageSeverityWarningBit .|.
		Vk.DbgUtls.MessageSeverityErrorBit,
	Vk.DbgUtls.Msngr.createInfoMessageType =
		Vk.DbgUtls.MessageTypeGeneralBit .|.
		Vk.DbgUtls.MessageTypeValidationBit .|.
		Vk.DbgUtls.MessageTypePerformanceBit,
	Vk.DbgUtls.Msngr.createInfoFnUserCallback = dbgCallback,
	Vk.DbgUtls.Msngr.createInfoUserData = Nothing }
	where dbgCallback _svr _tp cbdt _ud = False <$ Txt.putStrLn (
		"validation layer: " <>
		Vk.DbgUtls.Msngr.callbackDataMessage cbdt )

pickPhd :: Vk.Ist.I si -> Vk.Sfc.S ss -> IO (Vk.Phd.P, QFamIndices)
pickPhd ist sfc = Vk.Phd.enumerate ist >>= \case
	[] -> error "failed to find GPUs with Gpu.Vulkan support!"
	pds -> findMaybeM suit pds >>= \case
		Nothing -> error "failed to find a suitable GPU!"
		Just pdqfi -> pure pdqfi
	where
	suit pd = espt pd >>= bool (pure Nothing) do
		qfis <- findQFams pd sfc
		querySwpchSupport pd sfc \ss -> pure . bool qfis Nothing
			$	HPListC.null (snd $ formats ss) ||
				null (presentModes ss)
	espt pd = elemAll dvcExtensions
		. (Vk.Phd.extensionPropertiesExtensionName <$>)
		<$> Vk.Phd.enumerateExtensionProperties pd Nothing

dvcExtensions :: [Vk.Phd.ExtensionName]
dvcExtensions = [Vk.Swpch.extensionName]

data QFamIndices =
	QFamIndices { grFam :: Vk.QFam.Index, prFam :: Vk.QFam.Index }

findQFams :: Vk.Phd.P -> Vk.Sfc.S ss -> IO (Maybe QFamIndices)
findQFams pd sfc = do
	prps@((fst <$>) -> is) <- Vk.Phd.getQueueFamilyProperties pd
	mp <- listToMaybe
		<$> filterM (flip (Vk.Sfc.Phd.getSupport pd) sfc) is
	pure $ QFamIndices <$> (fst <$> L.find (grbit . snd) prps) <*> mp
	where grbit = checkBits Vk.Q.GraphicsBit . Vk.QFam.propertiesQueueFlags

data SwpchSupportDetails fmts = SwpchSupportDetails {
	capabilities :: Vk.Sfc.Capabilities,
	formats :: (
		[Vk.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
		HPListC.PL Vk.T.FormatToValue Vk.Sfc.Format fmts ),
	presentModes :: [Vk.Sfc.PresentMode] }

deriving instance
	Show (HPListC.PL Vk.T.FormatToValue Vk.Sfc.Format fmts) =>
	Show (SwpchSupportDetails fmts)

querySwpchSupport :: Vk.Phd.P -> Vk.Sfc.S ss -> (forall fmts .
	Show (HPListC.PL Vk.T.FormatToValue Vk.Sfc.Format fmts) =>
	SwpchSupportDetails fmts -> IO a) -> IO a
querySwpchSupport pd sfc f = Vk.Sfc.Phd.getFormats pd sfc \fmts ->
	f =<< SwpchSupportDetails
		<$> Vk.Sfc.Phd.getCapabilities pd sfc
		<*> ((, fmts) <$> Vk.Sfc.Phd.getFormatsFiltered pd sfc)
		<*> Vk.Sfc.Phd.getPresentModes pd sfc

createLgDvc :: Vk.Phd.P -> QFamIndices ->
	(forall sd . Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> IO a) -> IO a
createLgDvc pd qfis act = hetero qinfo uniqueQFams \qs ->
	Vk.Dvc.create pd (info qs) nil \dv -> join $ act dv
		<$> Vk.Dvc.getQueue dv (grFam qfis) 0
		<*> Vk.Dvc.getQueue dv (prFam qfis) 0
	where
	hetero :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] -> (forall ss .
		HPList.ToListWithCM' WithPoked TMaybe.M ss =>
		HPList.PL t ss -> b) -> b
	hetero _k [] f = f HPList.Nil
	hetero k (x : xs) f = hetero k xs \xs' -> f (k x :** xs')
	uniqueQFams = L.nub [grFam qfis, prFam qfis]
	info qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames = bool [] vldLayers debug,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures = Just def }
	qinfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }

createDscPl :: Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDscPl dv = Vk.DscPl.create dv info nil
	where info = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = 1,
		Vk.DscPl.createInfoPoolSizes = (: []) Vk.DscPl.Size {
			Vk.DscPl.sizeType = Vk.Dsc.TypeCombinedImageSampler,
			Vk.DscPl.sizeDescriptorCount = 1 } }

getMinImageCountFromPresentMode :: Vk.Sfc.PresentMode -> Int
getMinImageCountFromPresentMode = \case
	Vk.Sfc.PresentModeMailbox -> 3
	Vk.Sfc.PresentModeFifo -> 2
	Vk.Sfc.PresentModeFifoRelaxed -> 2
	Vk.Sfc.PresentModeImmediate -> 1
	_ -> error "bad"

createImageViews :: Vk.T.FormatToValue fmt2 =>
	Vk.Dvc.D sd ->
	(Vk.Img.Binded sm si nm fmt2 -> Vk.ImgVw.CreateInfo 'Nothing sm si nm fmt2 fmt2) ->
	[Vk.Img.Binded sm si nm fmt2] ->
	(forall sivs . HPList.PL (Vk.ImgVw.I nm fmt2) sivs -> IO a) -> IO a
createImageViews _ _ [] f = f HPList.Nil
createImageViews dvc info (i : is) f =
	Vk.ImgVw.create dvc (info i) nil \iv ->
	createImageViews dvc info is \ivs -> f $ iv :** ivs
