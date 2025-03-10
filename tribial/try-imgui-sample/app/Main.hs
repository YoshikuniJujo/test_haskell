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
import Foreign.Storable.PeekPoke
import Control.Monad
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
import Data.Text.IO qualified as Txt
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
import Gpu.Vulkan.Khr.Surface.Internal qualified as Vk.Sfc
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
import Gpu.Vulkan.Khr.Surface.Middle.Internal qualified as Vk.Sfc.M

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win

import Graphics.UI.GLFW.C qualified as GlfwC

import Bindings.GLFW qualified as GlfwBase

import Gpu.Vulkan.ImGui.Helper.Window.Middle qualified as Vk.ImGui.Win.M
import Gpu.Vulkan.ImGui.Helper.Window.Core qualified as Vk.ImGui.Win.C

import Gpu.Vulkan.Middle qualified as Vk.M

debug :: Bool
debug = True

main :: IO ()
main = (GlfwG.setErrorCallback (Just glfwErrorCallback) >>) .
	GlfwG.init error $
	GlfwG.Win.hint
		(GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI) >>
	GlfwG.Win.create 1280 720
		"Dear ImGui GLFW+Vulkan example" Nothing Nothing \win -> do
--	print =<< cxx_get_g_MainWindowData
--	printIO =<< Vk.ImGui.Win.M.wCFromCore @(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32) =<< Vk.ImGui.Win.M.fromCxx' =<< cxx_get_g_MainWindowData
--	print =<< cxx_get_g_MainWindowData
	printIO =<< Vk.ImGui.Win.M.wCFromCxx @(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32)
		=<< cxx_get_g_MainWindowData
	vs <- GlfwG.vulkanSupported
	when (not vs) $ error "GLFW: Vulkan Not Supported"
--	print =<< Vk.Ist.enumerateExtensionProperties Nothing
--	print Vk.DbgUtls.extensionName
	createIst \ist -> Vk.Sfc.Win.create ist win nil \sfc ->
		pickPhd ist sfc >>= \(phd, qfm) ->
		createLgDvc phd qfm \dvc gq _ ->
		createDscPl dvc \dp -> do
		mainCxx win ist sfc phd (grFam qfm) dvc gq dp
--		print =<< cxx_get_g_MainWindowData
		printIO =<< Vk.ImGui.Win.M.wCFromCxx @(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32)
			=<< cxx_get_g_MainWindowData

glfwErrorCallback :: GlfwG.Error -> GlfwG.ErrorMessage -> IO ()
glfwErrorCallback err dsc =
	hPutStrLn stderr $ "GLFW Error " ++ show err ++ ": " ++ dsc

foreign import ccall "main_cxx1" cxx_main_cxx1 ::
	Ptr GlfwBase.C'GLFWwindow -> Vk.Ist.I si -> Vk.Sfc.S ss -> Vk.Phd.P ->
	Vk.QFam.Index -> Vk.Dvc.D sd -> IO ()

foreign import ccall "main_cxx2" cxx_main_cxx2 ::
	Ptr GlfwBase.C'GLFWwindow -> Vk.Ist.I si -> Vk.Sfc.S ss -> Vk.Phd.P ->
	Vk.QFam.Index -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.DscPl.P sdp -> IO ()

foreign import ccall "get_g_MainWindowData" cxx_get_g_MainWindowData :: IO Vk.ImGui.Win.C.W

mainCxx ::
	GlfwG.Win.W sw -> Vk.Ist.I si -> Vk.Sfc.S ss -> Vk.Phd.P ->
	Vk.QFam.Index -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.DscPl.P sdp -> IO ()
mainCxx (GlfwG.Win.W win) ist sfc phd qfi dvc gq dp = do
	cxx_main_cxx1 (GlfwC.toC win) ist sfc phd qfi dvc
--	printIO =<< Vk.ImGui.Win.M.wCFromCore @(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32) =<< Vk.ImGui.Win.M.fromCxx' =<< cxx_get_g_MainWindowData
	wdcxx <- cxx_get_g_MainWindowData
	wd <- Vk.ImGui.Win.M.wCFromCxx @(Vk.M.ClearTypeColor Vk.M.ClearColorTypeFloat32) wdcxx
	printIO wd
	Vk.ImGui.Win.M.wCCopyToCxx wd wdcxx $
--	do
		cxx_main_cxx2 (GlfwC.toC win) ist sfc phd qfi dvc gq dp

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

-- imguiImplVulkanMinimumImageSamplerPoolSize = 1
