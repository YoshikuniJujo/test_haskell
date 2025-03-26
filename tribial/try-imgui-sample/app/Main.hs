{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications, RankNTypes #-}
{-# LANGUAGE DataKinds #-}
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
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
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
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Instance.Internal qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device.Internal qualified as Vk.Dvc
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorPool.Type qualified as Vk.DscPl

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
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win

import Graphics.UI.GLFW.C qualified as GlfwC

import Bindings.GLFW qualified as GlfwBase

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
		"Dear ImGui GLFW+Vulkan example" Nothing Nothing \win ->
	Vk.ImGui.Win.allocaW \w ->
	when oldLog (Vk.ImGui.Win.wCFromCxx'
		@(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32)
		w printIO ) >> do
	vs <- GlfwG.vulkanSupported
	when (not vs) $ error "GLFW: Vulkan Not Supported"
	when oldLog $ print AppUseUnlimitedFrameRate.flag
	createIst \ist -> Vk.Sfc.Win.create ist win nil \sfc ->
		pickPhd ist sfc >>= \(phd, qfm) ->
		createLgDvc phd qfm \dvc gq _ ->
		createDscPl dvc \dp ->
		Vk.Sfc.Phd.getSupport phd (grFam qfm) sfc >>= \res ->
		when (not res) (error "Error no WSI support on physicalDevice 0") >> do
		mainCxx win ist sfc phd (grFam qfm) dvc gq dp w
		when oldLog $ Vk.ImGui.Win.wCFromCxx'
			@(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32) w printIO

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
	Vk.QFam.Index -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.DscPl.P sdp -> Vk.ImGui.Win.W -> IO ()
mainCxx w@(GlfwG.Win.W win) ist sfc phd qfi dvc gq dp wdcxx =
	let	rqSfcImgFmt = [
			Vk.FormatB8g8r8a8Unorm, Vk.FormatR8g8b8a8Unorm,
			Vk.FormatB8g8r8Unorm, Vk.FormatR8g8b8Unorm ] in
	Vk.ImGui.H.selectSurfaceFormat phd sfc rqSfcImgFmt Vk.Sfc.ColorSpaceSrgbNonlinear \sfmt ->
	let	pms = bool
			[Vk.Sfc.PresentModeFifo]
			[	Vk.Sfc.PresentModeMailbox,
				Vk.Sfc.PresentModeImmediate,
				Vk.Sfc.PresentModeFifo ]
			AppUseUnlimitedFrameRate.flag in
	Vk.ImGui.H.selectPresentMode phd sfc pms \pm ->
	when oldLog (print pm) >>
	Vk.ImGui.Win.wCZero' @_ @(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32) \z ->
	let z' = z {
		Vk.ImGui.Win.wCSurface = sfc,
		Vk.ImGui.Win.wCSurfaceFormat = sfmt,
		Vk.ImGui.Win.wCClearEnable = True,
		Vk.ImGui.Win.wCPresentMode = pm
		} in
	Vk.ImGui.Win.wCCopyToCxx z' wdcxx $
	GlfwG.Win.getFramebufferSize w >>= \(fromIntegral -> wdt, fromIntegral -> hgt) ->
	Vk.ImGui.H.createOrResizeWindow ist phd dvc wdcxx qfi nil wdt hgt 2 >>
	Vk.ImGui.Win.wCFromCxx' @(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32) wdcxx \wd ->
	when oldLog (printIO wd) >>
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
	Vk.ImGui.Win.wCCopyToCxx wd wdcxx do
	pInitInfo <- cxx_new_ImGui_ImplVulkan_InitInfo
	cxx_initialize_ImGui_ImplVulkan_InitInfo
		pInitInfo ist phd qfi dvc gq dp wdcxx
	initInfo <- Vk.ImGui.initInfoFromCxx @'Nothing pInitInfo
	printIO initInfo
	Vk.ImGui.copyInitInfoToCxx initInfo pInitInfo
	print =<< Vk.ImGui.C.cxx_imgui_impl_vulkan_init pInitInfo
	let	fa@(ImGui.FontAtlas.M.F pfa) = ImGui.Io.fonts io
		grsj = ImGui.FontAtlas.getGlyphRangesJapanese fa
--	print grsj
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
					ImGui.begin "Hello, Haskell world!" () ImGui.windowFlagsZero
						\_ -> cxx_simpleWindowBody io psdw psow pcc
					sow <- (/= 0) <$> peek psow
					when sow $ cxx_anotherWindow psow
					ImGui.render
					dd <- ImGui.getDrawData
					let	(w, h) = ImGui.drawDataDisplaySize dd
						isMinimized = w <= 0 || h <= 0
					Vk.ImGui.Win.wCFromCxx'
						@(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32) wdcxx \wd ->
						when (not isMinimized) do
							[r, g, b, a] <- peekArray 4 pcc
							let	wd' = wd {
									Vk.ImGui.Win.wCClearValue =
										Vk.ClearValueColor . fromJust
											$ rgbaDouble (r * a) (g * a) (b * a) a :: Vk.ClearValue (Vk.ClearTypeColor Vk.ClearColorTypeFloat32) }
							Vk.ImGui.Win.wCCopyToCxx wd' wdcxx do
								cxx_FrameRender wdcxx dvc gq dd pscr
								cxx_FramePresent wdcxx gq pscr
	cxx_cleanup ist dvc wdcxx
	cxx_free_ImGui_ImplVulkan_InitInfo pInitInfo

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
