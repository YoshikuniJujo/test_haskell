{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Generics
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.Storable.HeteroList
import Foreign.Storable.SizeAlignment
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Proxy
import Data.Bool
import Data.Maybe
import Data.List
import Data.IORef
import Data.List.Length
import Data.Color

import qualified TypeLevel.List as TpLvlLst

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Graphics.UI.GLFW as Glfw hiding (createWindowSurface)
import qualified Glfw as Glfw
import qualified Cglm
import qualified Foreign.Storable.Generic

import ThEnv
import Shaderc
import Shaderc.EnumAuto
import Shaderc.TH

import Gpu.Vulkan.Misc
import Gpu.Vulkan.Data

import qualified Gpu.Vulkan as Vk
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.TypeEnum as Vk.T
import qualified Gpu.Vulkan.Exception as Vk
import qualified Gpu.Vulkan.Exception.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Ist
import qualified Gpu.Vulkan.Instance.Type as Vk.Ist
import qualified Gpu.Vulkan.Instance.Middle as Vk.Ist.M
import qualified Gpu.Vulkan.Khr as Vk.Khr
import qualified Gpu.Vulkan.Khr.Enum as Vk.Khr
import qualified Gpu.Vulkan.Ext.DebugUtils as Vk.Ext.DbgUtls
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger as Vk.Ext.DbgUtls.Msngr
import qualified Gpu.Vulkan.Ext.DebugUtils.Enum as Vk.Ext.DbgUtls
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.QueueFamily as Vk.QueueFamily

import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Device.Middle as Vk.Dvc.M
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Surface
import qualified Gpu.Vulkan.Khr.Surface.Middle as Vk.Khr.Surface.M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Surface.PhysicalDevice
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swapchain
import qualified Gpu.Vulkan.Khr.Swapchain.Type as Vk.Khr.Swapchain
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as Vk.Khr.Swapchain.M
import qualified Gpu.Vulkan.Image.Type as Vk.Image
import qualified Gpu.Vulkan.Image.Enum as Vk.Image
import qualified Gpu.Vulkan.Image.Middle as Vk.Image.M
import qualified Gpu.Vulkan.ImageView as Vk.ImgVw
import qualified Gpu.Vulkan.ImageView.Enum as Vk.ImgVw
import qualified Gpu.Vulkan.Component as Vk.Component
import qualified Gpu.Vulkan.ShaderModule as Vk.Shader.Module
import qualified Gpu.Vulkan.ShaderModule.Middle as Vk.Shader.Module.M
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShdrSt
import qualified Gpu.Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.InpAsmbSt
import qualified Gpu.Vulkan.Pipeline.ViewportState as Vk.Ppl.ViewportSt
import qualified Gpu.Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Gpu.Vulkan.Pipeline.MultisampleState as Vk.Ppl.MltSmplSt
import qualified Gpu.Vulkan.Sample as Vk.Sample
import qualified Gpu.Vulkan.Sample.Enum as Vk.Sample
import qualified Gpu.Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.ClrBlndAtt
import qualified Gpu.Vulkan.ColorComponent.Enum as Vk.ClrCmp
import qualified Gpu.Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlndSt
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Layout
import qualified Gpu.Vulkan.Attachment as Vk.Att
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Attachment.Enum as Vk.Att
import qualified Gpu.Vulkan.Subpass as Vk.Subpass
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Subpass.Enum as Vk.Subpass
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPass
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPass.M
import qualified Gpu.Vulkan.Pipeline.Graphics.Type as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Framebuffer as Vk.Frmbffr
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPool
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBffr
import qualified Gpu.Vulkan.CommandBuffer.Middle as Vk.CmdBffr.M
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.Fence.Enum as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Buffer.Enum as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Memory.Kind as Vk.Mem.K
import qualified Gpu.Vulkan.Memory.Middle as Vk.Mem.M
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mem
import qualified Gpu.Vulkan.Memory.AllocateInfo as Vk.Dvc.Mem.Buffer
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import Tools
import Gpu.Vulkan.Image.Enum qualified as Vk.Img

main :: IO ()
main = do
	g <- newFramebufferResized
	(`withWindow` g) \win -> createInstance \inst -> do
		if enableValidationLayers
			then setupDebugMessenger inst $ const $ run win inst g
			else run win inst g

type FramebufferResized = IORef Bool

globalFramebufferResized :: IORef Bool -> IORef Bool
globalFramebufferResized = id

newFramebufferResized :: IO FramebufferResized
newFramebufferResized = newIORef False

windowName :: String
windowName = "Triangle"

windowSize :: (Int, Int)
windowSize = (width, height) where width = 800; height = 600

enableValidationLayers :: Bool
enableValidationLayers = maybe True (const False) $(lookupCompileEnv "NDEBUG")

validationLayers :: [Txt.Text]
validationLayers = [Vk.Khr.validationLayerName]

maxFramesInFlight :: Integral n => n
maxFramesInFlight = 1

withWindow :: (Glfw.Window -> IO a) -> FramebufferResized -> IO a
withWindow f g = initWindow g >>= \w ->
	f w <* (Glfw.destroyWindow w >> Glfw.terminate)

initWindow :: FramebufferResized -> IO Glfw.Window
initWindow frszd = do
	Just w <- do
		True <- Glfw.init
		Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
		uncurry Glfw.createWindow windowSize windowName Nothing Nothing
	w <$ Glfw.setFramebufferSizeCallback
		w (Just $ \_ _ _ -> writeIORef frszd True)

createInstance :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createInstance f = do
	when enableValidationLayers $ bool
		(error "validation layers requested, but not available!")
		(pure ())
		=<< null . (validationLayers \\)
				. (Vk.layerPropertiesLayerName <$>)
			<$> Vk.Ist.M.enumerateLayerProperties
	extensions <- bool id (Vk.Ext.DbgUtls.extensionName :)
			enableValidationLayers
		<$> ((cstrToText `mapM`) =<< Glfw.getRequiredInstanceExtensions)
	print extensions
	let	appInfo = Vk.ApplicationInfo {
			Vk.applicationInfoNext = TMaybe.N,
			Vk.applicationInfoApplicationName = "Hello Triangle",
			Vk.applicationInfoApplicationVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoEngineName = "No Engine",
			Vk.applicationInfoEngineVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoApiVersion = Vk.apiVersion_1_0 }
		createInfo :: Vk.Ist.M.CreateInfo
			('Just (Vk.Ext.DbgUtls.Msngr.CreateInfo
				'Nothing '[] () () () ())) 'Nothing
		createInfo = Vk.Ist.M.CreateInfo {
			Vk.Ist.M.createInfoNext = TMaybe.J debugMessengerCreateInfo,
			Vk.Ist.M.createInfoFlags = def,
			Vk.Ist.M.createInfoApplicationInfo = Just appInfo,
			Vk.Ist.M.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Ist.M.createInfoEnabledExtensionNames = extensions }
	Vk.Ist.create createInfo nil' \i -> f i

instanceToMiddle :: Vk.Ist.I si -> Vk.Ist.M.I
instanceToMiddle (Vk.Ist.I inst) = inst

setupDebugMessenger ::
	Vk.Ist.I si ->
	(forall sm . Vk.Ext.DbgUtls.Msngr.M sm -> IO a) -> IO a
setupDebugMessenger ist f = Vk.Ext.DbgUtls.Msngr.create ist
	debugMessengerCreateInfo nil' \m -> f m

debugMessengerCreateInfo :: Vk.Ext.DbgUtls.Msngr.CreateInfo 'Nothing '[] () () () ()
debugMessengerCreateInfo = Vk.Ext.DbgUtls.Msngr.CreateInfo {
	Vk.Ext.DbgUtls.Msngr.createInfoNext = TMaybe.N,
	Vk.Ext.DbgUtls.Msngr.createInfoFlags = def,
	Vk.Ext.DbgUtls.Msngr.createInfoMessageSeverity =
		Vk.Ext.DbgUtls.MessageSeverityVerboseBit .|.
		Vk.Ext.DbgUtls.MessageSeverityWarningBit .|.
		Vk.Ext.DbgUtls.MessageSeverityErrorBit,
	Vk.Ext.DbgUtls.Msngr.createInfoMessageType =
		Vk.Ext.DbgUtls.MessageTypeGeneralBit .|.
		Vk.Ext.DbgUtls.MessageTypeValidationBit .|.
		Vk.Ext.DbgUtls.MessageTypePerformanceBit,
	Vk.Ext.DbgUtls.Msngr.createInfoFnUserCallback = debugCallback,
	Vk.Ext.DbgUtls.Msngr.createInfoUserData = Nothing }

debugCallback :: Vk.Ext.DbgUtls.Msngr.FnCallback '[] ()
debugCallback _msgSeverity _msgType cbdt _userData = False <$ Txt.putStrLn
	("validation layer: " <> Vk.Ext.DbgUtls.Msngr.callbackDataMessage cbdt)

run :: Glfw.Window -> Vk.Ist.I si -> FramebufferResized -> IO ()
run w inst g =
	createSurface w inst \sfc ->
	pickPhysicalDevice inst sfc >>= \(phdv, qfis) ->
	createLogicalDevice phdv qfis \dv gq pq ->
	createSwapChainNew w sfc phdv qfis dv
		\(sc :: Vk.Khr.Swapchain.SNew ss scifmt) ext ->
	Vk.Khr.Swapchain.getImagesNew dv sc >>= \imgs ->
	createImageViewsNew dv imgs \scivs ->
	createRenderPassNew @scifmt dv \rp ->
	createPipelineLayout' dv \ppllyt ->
	createGraphicsPipeline' dv ext rp ppllyt \gpl ->
	createFramebuffers dv ext rp scivs \fbs ->
	createCommandPool qfis dv \cp ->
	createVertexBuffer phdv dv gq cp \vb ->
	createCommandBuffer dv cp \cb ->
	createSyncObjects dv \sos ->
	mainLoop g w sfc phdv qfis dv gq pq sc ext scivs rp ppllyt gpl fbs vb cb sos

createSurface :: Glfw.Window -> Vk.Ist.I si ->
	(forall ss . Vk.Khr.Surface.S ss -> IO a) -> IO a
createSurface win ist f = Glfw.createWindowSurface ist win nil' \sfc -> f sfc

pickPhysicalDevice :: Vk.Ist.I si ->
	Vk.Khr.Surface.S ss -> IO (Vk.PhDvc.P, QueueFamilyIndices)
pickPhysicalDevice ist sfc = do
	dvcs <- Vk.PhDvc.enumerate ist
	when (null dvcs) $ error "failed to find GPUs with Gpu.Vulkan support!"
	findDevice (`isDeviceSuitable` sfc) dvcs >>= \case
		Just pdvc -> pure pdvc
		Nothing -> error "failed to find a suitable GPU!"

findDevice :: Monad m =>
	(Vk.PhDvc.P -> m (Maybe a)) -> [Vk.PhDvc.P] ->
	m (Maybe (Vk.PhDvc.P, a))
findDevice prd = \case
	[] -> pure Nothing
	p : ps -> prd p >>= \case
		Nothing -> findDevice prd ps; Just x -> pure $ Just (p, x)

isDeviceSuitable ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO (Maybe QueueFamilyIndices)
isDeviceSuitable phdvc sfc = do
	_deviceProperties <- Vk.PhDvc.getProperties phdvc
	_deviceFeatures <- Vk.PhDvc.getFeatures phdvc
	indices <- findQueueFamilies phdvc sfc
	extensionSupported <- checkDeviceExtensionSupport phdvc
	if extensionSupported
	then (<$> querySwapChainSupport phdvc sfc) \spp ->
		bool (completeQueueFamilies indices) Nothing
			$ null (formats spp) || null (presentModes spp)
	else pure Nothing

data QueueFamilyIndices = QueueFamilyIndices {
	graphicsFamily :: Vk.QueueFamily.Index,
	presentFamily :: Vk.QueueFamily.Index }

data QueueFamilyIndicesMaybe = QueueFamilyIndicesMaybe {
	graphicsFamilyMaybe :: Maybe Vk.QueueFamily.Index,
	presentFamilyMaybe :: Maybe Vk.QueueFamily.Index }

completeQueueFamilies :: QueueFamilyIndicesMaybe -> Maybe QueueFamilyIndices
completeQueueFamilies = \case
	QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = Just gf, presentFamilyMaybe = Just pf } ->
		Just QueueFamilyIndices {
			graphicsFamily = gf, presentFamily = pf }
	_ -> Nothing

findQueueFamilies ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO QueueFamilyIndicesMaybe
findQueueFamilies device sfc = do
	queueFamilies <- Vk.PhDvc.getQueueFamilyProperties device
	pfis <- filterM
		(\i -> Vk.Khr.Surface.PhysicalDevice.getSupport device i sfc)
		(fst <$> queueFamilies)
	pure QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = fst <$> find
			(checkBits Vk.Queue.GraphicsBit
				. Vk.QueueFamily.propertiesQueueFlags . snd)
			queueFamilies,
		presentFamilyMaybe = listToMaybe pfis }

checkDeviceExtensionSupport :: Vk.PhDvc.P -> IO Bool
checkDeviceExtensionSupport dvc =
	null . (deviceExtensions \\) . (Vk.extensionPropertiesExtensionName <$>)
		<$> Vk.PhDvc.enumerateExtensionProperties dvc Nothing

deviceExtensions :: [Txt.Text]
deviceExtensions = [Vk.Khr.Swapchain.M.extensionName]

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.Khr.Surface.M.Capabilities,
	formats :: [Vk.Khr.Surface.M.Format],
	presentModes :: [Vk.Khr.PresentMode] }
	deriving Show

querySwapChainSupport ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO SwapChainSupportDetails
querySwapChainSupport dvc sfc = SwapChainSupportDetails
	<$> Vk.Khr.Surface.PhysicalDevice.getCapabilities dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getFormats dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getPresentModes dvc sfc

createLogicalDevice :: Vk.PhDvc.P -> QueueFamilyIndices -> (forall sd .
		Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> IO a) -> IO a
createLogicalDevice phdvc qfis f = let
	uniqueQueueFamilies =
		nub [graphicsFamily qfis, presentFamily qfis]
	queueCreateInfos qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] } in
	mkHeteroParList queueCreateInfos uniqueQueueFamilies \qs -> do
	let	createInfo = Vk.Dvc.M.CreateInfo {
			Vk.Dvc.M.createInfoNext = TMaybe.N,
			Vk.Dvc.M.createInfoFlags = def,
			Vk.Dvc.M.createInfoQueueCreateInfos = qs,
			Vk.Dvc.M.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Dvc.M.createInfoEnabledExtensionNames =
				deviceExtensions,
			Vk.Dvc.M.createInfoEnabledFeatures = Just def }
	Vk.Dvc.create phdvc createInfo nil' \dvc -> do
		gq <- Vk.Dvc.getQueue dvc (graphicsFamily qfis) 0
		pq <- Vk.Dvc.getQueue dvc (presentFamily qfis) 0
		f dvc gq pq

mkHeteroParList :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] ->
	(forall ss . WithPokedHeteroToListM' TMaybe.M ss => HeteroParList.PL t ss -> b) -> b
mkHeteroParList _k [] f = f HeteroParList.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

createSwapChainNew :: Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd ->
	(forall ss scfmt . Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swapchain.SNew ss scfmt -> Vk.Extent2d -> IO a) ->
	IO a
createSwapChainNew win sfc phdvc qfis dvc f = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	fmt = Vk.Khr.Surface.M.formatFormat
			. chooseSwapSurfaceFormat $ formats spp
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		let	crInfo = mkSwapchainCreateInfoNew sfc qfis spp ext
		Vk.Khr.Swapchain.createNew @'Nothing @fmt dvc crInfo nil'
			\sc -> f sc ext

mkSwapchainCreateInfoNew :: Vk.Khr.Surface.S ss -> QueueFamilyIndices ->
	SwapChainSupportDetails -> Vk.Extent2d ->
	Vk.Khr.Swapchain.CreateInfoNew 'Nothing ss fmt
mkSwapchainCreateInfoNew sfc qfis0 spp ext =
	Vk.Khr.Swapchain.CreateInfoNew {
		Vk.Khr.Swapchain.createInfoNextNew = TMaybe.N,
		Vk.Khr.Swapchain.createInfoFlagsNew = def,
		Vk.Khr.Swapchain.createInfoSurfaceNew = sfc,
		Vk.Khr.Swapchain.createInfoMinImageCountNew = imgc,
		Vk.Khr.Swapchain.createInfoImageColorSpaceNew =
			Vk.Khr.Surface.M.formatColorSpace fmt,
		Vk.Khr.Swapchain.createInfoImageExtentNew = ext,
		Vk.Khr.Swapchain.createInfoImageArrayLayersNew = 1,
		Vk.Khr.Swapchain.createInfoImageUsageNew =
			Vk.Img.UsageColorAttachmentBit,
		Vk.Khr.Swapchain.createInfoImageSharingModeNew = ism,
		Vk.Khr.Swapchain.createInfoQueueFamilyIndicesNew = qfis,
		Vk.Khr.Swapchain.createInfoPreTransformNew =
			Vk.Khr.Surface.M.capabilitiesCurrentTransform caps,
		Vk.Khr.Swapchain.createInfoCompositeAlphaNew =
			Vk.Khr.CompositeAlphaOpaqueBit,
		Vk.Khr.Swapchain.createInfoPresentModeNew = presentMode,
		Vk.Khr.Swapchain.createInfoClippedNew = True,
		Vk.Khr.Swapchain.createInfoOldSwapchainNew = Nothing }
	where
	fmt = chooseSwapSurfaceFormat $ formats spp
	presentMode = chooseSwapPresentMode $ presentModes spp
	caps = capabilities spp
	maxImgc = fromMaybe maxBound . onlyIf (> 0)
		$ Vk.Khr.Surface.M.capabilitiesMaxImageCount caps
	imgc = clamp
		(Vk.Khr.Surface.M.capabilitiesMinImageCount caps + 1) 0 maxImgc
	(ism, qfis) = bool
		(Vk.SharingModeConcurrent,
			[graphicsFamily qfis0, presentFamily qfis0])
		(Vk.SharingModeExclusive, [])
		(graphicsFamily qfis0 == presentFamily qfis0)

recreateSwapChain :: forall ssfc sd ssc fmt . Vk.T.FormatToValue fmt =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Khr.Swapchain.SNew ssc fmt ->
	IO (Vk.Format, Vk.Extent2d)
recreateSwapChain win sfc phdvc qfis0 dvc sc = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	(crInfo, scifmt) = mkSwapchainCreateInfoRaw @fmt sfc qfis0 spp ext
	(scifmt, ext) <$ Vk.Khr.Swapchain.recreateNew dvc crInfo nil' sc

mkSwapchainCreateInfo :: Vk.Khr.Surface.S ss -> QueueFamilyIndices ->
	SwapChainSupportDetails -> Vk.Extent2d ->
	(forall fmt . Vk.T.FormatToValue fmt =>
		Vk.Khr.Swapchain.CreateInfoNew 'Nothing ss fmt -> Vk.Format -> a) -> a
mkSwapchainCreateInfo sfc qfis spp ext f = Vk.T.formatToType (Vk.Khr.Surface.M.formatFormat fmt) \(_ :: Proxy t) ->
	uncurry f $ mkSwapchainCreateInfoRaw @t sfc qfis spp ext
	where
	fmt = chooseSwapSurfaceFormat $ formats spp

mkSwapchainCreateInfoRaw :: forall fmt ss .
	Vk.Khr.Surface.S ss -> QueueFamilyIndices ->
	SwapChainSupportDetails -> Vk.Extent2d ->
	(Vk.Khr.Swapchain.CreateInfoNew 'Nothing ss fmt, Vk.Format)
mkSwapchainCreateInfoRaw sfc qfis0 spp ext = (
	Vk.Khr.Swapchain.CreateInfoNew {
		Vk.Khr.Swapchain.createInfoNextNew = TMaybe.N,
		Vk.Khr.Swapchain.createInfoFlagsNew = def,
		Vk.Khr.Swapchain.createInfoSurfaceNew = sfc,
		Vk.Khr.Swapchain.createInfoMinImageCountNew = imgc,
		Vk.Khr.Swapchain.createInfoImageColorSpaceNew =
			Vk.Khr.Surface.M.formatColorSpace fmt,
		Vk.Khr.Swapchain.createInfoImageExtentNew = ext,
		Vk.Khr.Swapchain.createInfoImageArrayLayersNew = 1,
		Vk.Khr.Swapchain.createInfoImageUsageNew =
			Vk.Image.UsageColorAttachmentBit,
		Vk.Khr.Swapchain.createInfoImageSharingModeNew = ism,
		Vk.Khr.Swapchain.createInfoQueueFamilyIndicesNew = qfis,
		Vk.Khr.Swapchain.createInfoPreTransformNew =
			Vk.Khr.Surface.M.capabilitiesCurrentTransform caps,
		Vk.Khr.Swapchain.createInfoCompositeAlphaNew =
			Vk.Khr.CompositeAlphaOpaqueBit,
		Vk.Khr.Swapchain.createInfoPresentModeNew = presentMode,
		Vk.Khr.Swapchain.createInfoClippedNew = True,
		Vk.Khr.Swapchain.createInfoOldSwapchainNew = Nothing }, scifmt )
	where
	fmt = chooseSwapSurfaceFormat $ formats spp
	scifmt = Vk.Khr.Surface.M.formatFormat fmt
	presentMode = chooseSwapPresentMode $ presentModes spp
	caps = capabilities spp
	maxImgc = fromMaybe maxBound . onlyIf (> 0)
		$ Vk.Khr.Surface.M.capabilitiesMaxImageCount caps
	imgc = clamp
		(Vk.Khr.Surface.M.capabilitiesMinImageCount caps + 1) 0 maxImgc
	(ism, qfis) = bool
		(Vk.SharingModeConcurrent,
			[graphicsFamily qfis0, presentFamily qfis0])
		(Vk.SharingModeExclusive, [])
		(graphicsFamily qfis0 == presentFamily qfis0)

chooseSwapSurfaceFormat  :: [Vk.Khr.Surface.M.Format] -> Vk.Khr.Surface.M.Format
chooseSwapSurfaceFormat = \case
	availableFormats@(af0 : _) -> fromMaybe af0
		$ find preferredSwapSurfaceFormat availableFormats
	_ -> error "no available swap surface formats"

preferredSwapSurfaceFormat :: Vk.Khr.Surface.M.Format -> Bool
preferredSwapSurfaceFormat f =
	Vk.Khr.Surface.M.formatFormat f == Vk.FormatB8g8r8a8Srgb &&
	Vk.Khr.Surface.M.formatColorSpace f == Vk.Khr.ColorSpaceSrgbNonlinear

chooseSwapPresentMode :: [Vk.Khr.PresentMode] -> Vk.Khr.PresentMode
chooseSwapPresentMode =
	fromMaybe Vk.Khr.PresentModeFifo . find (== Vk.Khr.PresentModeMailbox)

chooseSwapExtent :: Glfw.Window -> Vk.Khr.Surface.M.Capabilities -> IO Vk.Extent2d
chooseSwapExtent win caps
	| Vk.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> w, fromIntegral -> h) <-
			Glfw.getFramebufferSize win
		pure $ Vk.Extent2d
			(clamp w (Vk.extent2dWidth n) (Vk.extent2dHeight n))
			(clamp h (Vk.extent2dWidth x) (Vk.extent2dHeight x))
	where
	curExt = Vk.Khr.Surface.M.capabilitiesCurrentExtent caps
	n = Vk.Khr.Surface.M.capabilitiesMinImageExtent caps
	x = Vk.Khr.Surface.M.capabilitiesMaxImageExtent caps

createImageViewsNew :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Image.BindedNew ss ss nm fmt] ->
	(forall si . HeteroParList.PL (Vk.ImgVw.INew fmt nm) si -> IO a) -> IO a
createImageViewsNew _dvc [] f = f HeteroParList.Nil
createImageViewsNew dvc (sci : scis) f =
	createImageView dvc sci \sciv ->
	createImageViewsNew dvc scis \scivs -> f $ sciv :** scivs

recreateImageViewsNew :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Image.BindedNew ss ss nm scfmt] -> HeteroParList.PL (Vk.ImgVw.INew scfmt nm) sis -> IO ()
recreateImageViewsNew _dvc [] HeteroParList.Nil = pure ()
recreateImageViewsNew dvc (sci : scis) (iv :** ivs) =
	Vk.ImgVw.recreateNew dvc (mkImageViewCreateInfoNew sci) nil' iv >>
	recreateImageViewsNew dvc scis ivs
recreateImageViewsNew _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

createImageView :: forall ivfmt sd si sm nm ifmt a .
	Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Image.BindedNew si sm nm ifmt ->
	(forall siv . Vk.ImgVw.INew ivfmt nm siv -> IO a) -> IO a
createImageView dvc timg f =
	Vk.ImgVw.createNew dvc (mkImageViewCreateInfoNew timg) nil' f

mkImageViewCreateInfoNew ::
	Vk.Image.BindedNew si sm nm ifmt ->
	Vk.ImgVw.CreateInfoNew 'Nothing si sm nm ifmt ivfmt
mkImageViewCreateInfoNew sci = Vk.ImgVw.CreateInfoNew {
	Vk.ImgVw.createInfoNextNew = TMaybe.N,
	Vk.ImgVw.createInfoFlagsNew = Vk.ImgVw.CreateFlagsZero,
	Vk.ImgVw.createInfoImageNew = sci,
	Vk.ImgVw.createInfoViewTypeNew = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponentsNew = components,
	Vk.ImgVw.createInfoSubresourceRangeNew = subresourceRange }
	where
	components = Vk.Component.Mapping {
		Vk.Component.mappingR = def, Vk.Component.mappingG = def,
		Vk.Component.mappingB = def, Vk.Component.mappingA = def }
	subresourceRange = Vk.Image.M.SubresourceRange {
		Vk.Image.M.subresourceRangeAspectMask = Vk.Image.AspectColorBit,
		Vk.Image.M.subresourceRangeBaseMipLevel = 0,
		Vk.Image.M.subresourceRangeLevelCount = 1,
		Vk.Image.M.subresourceRangeBaseArrayLayer = 0,
		Vk.Image.M.subresourceRangeLayerCount = 1 }

createRenderPassNew ::
	forall (scifmt :: Vk.T.Format) sd a . Vk.T.FormatToValue scifmt =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPass.R sr -> IO a) -> IO a
createRenderPassNew dvc f = do
	let	colorAttachment :: Vk.Att.Description scifmt
		colorAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = zeroBits,
			Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
			Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
			Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
			Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOp =
				Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayout =
				Vk.Img.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Img.LayoutPresentSrcKhr }
		colorAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 0,
			Vk.Att.referenceLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
		subpass = Vk.Subpass.Description {
			Vk.Subpass.descriptionFlags = zeroBits,
			Vk.Subpass.descriptionPipelineBindPoint =
				Vk.Ppl.BindPointGraphics,
			Vk.Subpass.descriptionInputAttachments = [],
			Vk.Subpass.descriptionColorAndResolveAttachments =
				Left [colorAttachmentRef],
			Vk.Subpass.descriptionDepthStencilAttachment = Nothing,
			Vk.Subpass.descriptionPreserveAttachments = [] }
		dependency = Vk.Subpass.Dependency {
			Vk.Subpass.dependencySrcSubpass = Vk.Subpass.SExternal,
			Vk.Subpass.dependencyDstSubpass = Vk.Subpass.S 0,
			Vk.Subpass.dependencySrcStageMask =
				Vk.Ppl.StageColorAttachmentOutputBit .|.
				Vk.Ppl.StageEarlyFragmentTestsBit,
			Vk.Subpass.dependencySrcAccessMask = zeroBits,
			Vk.Subpass.dependencyDstStageMask =
				Vk.Ppl.StageColorAttachmentOutputBit .|.
				Vk.Ppl.StageEarlyFragmentTestsBit,
			Vk.Subpass.dependencyDstAccessMask =
				Vk.AccessColorAttachmentWriteBit .|.
				Vk.AccessDepthStencilAttachmentWriteBit,
			Vk.Subpass.dependencyDependencyFlags = zeroBits }
		renderPassInfo = Vk.RndrPass.M.CreateInfoNew {
			Vk.RndrPass.M.createInfoNextNew = TMaybe.N,
			Vk.RndrPass.M.createInfoFlagsNew = zeroBits,
			Vk.RndrPass.M.createInfoAttachmentsNew = colorAttachment :** HeteroParList.Nil,
			Vk.RndrPass.M.createInfoSubpassesNew = [subpass],
			Vk.RndrPass.M.createInfoDependenciesNew = [dependency] }
	Vk.RndrPass.createNew @'[scifmt] @'Nothing dvc renderPassInfo nil' \rp -> f rp

createPipelineLayout' ::
	Vk.Dvc.D sd -> (forall sl . Vk.Ppl.Layout.L sl '[] '[] -> IO b) -> IO b
createPipelineLayout' dvc f = do
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfoNew {
			Vk.Ppl.Layout.createInfoNextNew = TMaybe.N,
			Vk.Ppl.Layout.createInfoFlagsNew = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayoutsNew = HeteroParList.Nil }
	Vk.Ppl.Layout.createNew @_ @_ @'[] @'Nothing dvc pipelineLayoutInfo nil' f

createGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[] '[] ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]) -> IO a) -> IO a
createGraphicsPipeline' dvc sce rp ppllyt f =
	Vk.Ppl.Graphics.createGs dvc Nothing (U14 pplInfo :** HeteroParList.Nil)
			nil' \(U3 gpl :** HeteroParList.Nil) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo sce rp ppllyt

recreateGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]) -> IO ()
recreateGraphicsPipeline' dvc sce rp ppllyt gpls = Vk.Ppl.Graphics.recreateGs
	dvc Nothing (U14 pplInfo :** HeteroParList.Nil) nil' (U3 gpls :** HeteroParList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo sce rp ppllyt

mkGraphicsPipelineCreateInfo ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[] '[] ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing, '[ '(Vertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing '(sl, '[], '[]) sr '(sb, vs', ts', slbtss')
mkGraphicsPipelineCreateInfo sce rp ppllyt = Vk.Ppl.Graphics.CreateInfo {
	Vk.Ppl.Graphics.createInfoNext = TMaybe.N,
	Vk.Ppl.Graphics.createInfoFlags = Vk.Ppl.CreateFlagsZero,
	Vk.Ppl.Graphics.createInfoStages = shaderStages,
	Vk.Ppl.Graphics.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Graphics.createInfoInputAssemblyState = Just inputAssembly,
	Vk.Ppl.Graphics.createInfoViewportState = Just $ mkViewportState sce,
	Vk.Ppl.Graphics.createInfoRasterizationState = Just rasterizer,
	Vk.Ppl.Graphics.createInfoMultisampleState = Just multisampling,
	Vk.Ppl.Graphics.createInfoDepthStencilState = Nothing,
	Vk.Ppl.Graphics.createInfoColorBlendState = Just colorBlending,
	Vk.Ppl.Graphics.createInfoDynamicState = Nothing,
	Vk.Ppl.Graphics.createInfoLayout = U3 ppllyt,
	Vk.Ppl.Graphics.createInfoRenderPass = rp,
	Vk.Ppl.Graphics.createInfoSubpass = 0,
	Vk.Ppl.Graphics.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Graphics.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Graphics.createInfoTessellationState = Nothing }

shaderStages :: HeteroParList.PL (U5 Vk.Ppl.ShdrSt.CreateInfoNew) '[
	'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
	'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
shaderStages = U5 vertShaderStageInfo :** U5 fragShaderStageInfo :** HeteroParList.Nil
	where
	vertShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfoNew {
		Vk.Ppl.ShdrSt.createInfoNextNew = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlagsNew = def,
		Vk.Ppl.ShdrSt.createInfoStageNew = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModuleNew = vertShaderModule,
		Vk.Ppl.ShdrSt.createInfoNameNew = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfoNew = Nothing }
	fragShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfoNew {
		Vk.Ppl.ShdrSt.createInfoNextNew = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlagsNew = def,
		Vk.Ppl.ShdrSt.createInfoStageNew = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModuleNew = fragShaderModule,
		Vk.Ppl.ShdrSt.createInfoNameNew = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfoNew = Nothing }

inputAssembly :: Vk.Ppl.InpAsmbSt.CreateInfo 'Nothing
inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
	Vk.Ppl.InpAsmbSt.createInfoNext = TMaybe.N,
	Vk.Ppl.InpAsmbSt.createInfoFlags = zeroBits,
	Vk.Ppl.InpAsmbSt.createInfoTopology = Vk.PrimitiveTopologyTriangleList,
	Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable = False }

mkViewportState :: Vk.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo 'Nothing
mkViewportState sce = Vk.Ppl.ViewportSt.CreateInfo {
	Vk.Ppl.ViewportSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ViewportSt.createInfoFlags = zeroBits,
	Vk.Ppl.ViewportSt.createInfoViewports = [viewport],
	Vk.Ppl.ViewportSt.createInfoScissors = [scissor] }
	where
	viewport = Vk.Viewport {
		Vk.viewportX = 0, Vk.viewportY = 0,
		Vk.viewportWidth = fromIntegral $ Vk.extent2dWidth sce,
		Vk.viewportHeight = fromIntegral $ Vk.extent2dHeight sce,
		Vk.viewportMinDepth = 0, Vk.viewportMaxDepth = 1 }
	scissor = Vk.Rect2d {
		Vk.rect2dOffset = Vk.Offset2d 0 0, Vk.rect2dExtent = sce }

rasterizer :: Vk.Ppl.RstSt.CreateInfo 'Nothing
rasterizer = Vk.Ppl.RstSt.CreateInfo {
	Vk.Ppl.RstSt.createInfoNext = TMaybe.N,
	Vk.Ppl.RstSt.createInfoFlags = zeroBits,
	Vk.Ppl.RstSt.createInfoDepthClampEnable = False,
	Vk.Ppl.RstSt.createInfoRasterizerDiscardEnable = False,
	Vk.Ppl.RstSt.createInfoPolygonMode = Vk.PolygonModeFill,
	Vk.Ppl.RstSt.createInfoLineWidth = 1,
	Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeBackBit,
	Vk.Ppl.RstSt.createInfoFrontFace = Vk.FrontFaceClockwise,
	Vk.Ppl.RstSt.createInfoDepthBiasEnable = False,
	Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0 }

multisampling :: Vk.Ppl.MltSmplSt.CreateInfo 'Nothing
multisampling = Vk.Ppl.MltSmplSt.CreateInfo {
	Vk.Ppl.MltSmplSt.createInfoNext = TMaybe.N,
	Vk.Ppl.MltSmplSt.createInfoFlags = zeroBits,
	Vk.Ppl.MltSmplSt.createInfoSampleShadingEnable = False,
	Vk.Ppl.MltSmplSt.createInfoRasterizationSamplesAndMask =
		Vk.Sample.CountAndMask Vk.Sample.Count1Bit Nothing,
	Vk.Ppl.MltSmplSt.createInfoMinSampleShading = 1,
	Vk.Ppl.MltSmplSt.createInfoAlphaToCoverageEnable = False,
	Vk.Ppl.MltSmplSt.createInfoAlphaToOneEnable = False }

colorBlending :: Vk.Ppl.ClrBlndSt.CreateInfo 'Nothing
colorBlending = Vk.Ppl.ClrBlndSt.CreateInfo {
	Vk.Ppl.ClrBlndSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ClrBlndSt.createInfoFlags = zeroBits,
	Vk.Ppl.ClrBlndSt.createInfoLogicOpEnable = False,
	Vk.Ppl.ClrBlndSt.createInfoLogicOp = Vk.LogicOpCopy,
	Vk.Ppl.ClrBlndSt.createInfoAttachments = [colorBlendAttachment],
	Vk.Ppl.ClrBlndSt.createInfoBlendConstants =
		fromJust $ rgbaDouble 0 0 0 0 }

colorBlendAttachment :: Vk.Ppl.ClrBlndAtt.State
colorBlendAttachment = Vk.Ppl.ClrBlndAtt.State {
	Vk.Ppl.ClrBlndAtt.stateColorWriteMask =
		Vk.ClrCmp.RBit .|. Vk.ClrCmp.GBit .|.
		Vk.ClrCmp.BBit .|. Vk.ClrCmp.ABit,
	Vk.Ppl.ClrBlndAtt.stateBlendEnable = False,
	Vk.Ppl.ClrBlndAtt.stateSrcColorBlendFactor = Vk.BlendFactorOne,
	Vk.Ppl.ClrBlndAtt.stateDstColorBlendFactor = Vk.BlendFactorZero,
	Vk.Ppl.ClrBlndAtt.stateColorBlendOp = Vk.BlendOpAdd,
	Vk.Ppl.ClrBlndAtt.stateSrcAlphaBlendFactor = Vk.BlendFactorOne,
	Vk.Ppl.ClrBlndAtt.stateDstAlphaBlendFactor = Vk.BlendFactorZero,
	Vk.Ppl.ClrBlndAtt.stateAlphaBlendOp = Vk.BlendOpAdd }

createFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
	Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.INew fmt nm) sis ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HeteroParList.Nil f = f HeteroParList.Nil
createFramebuffers dvc sce rp (iv :** ivs) f =
	Vk.Frmbffr.createNew dvc (mkFramebufferCreateInfoNew sce rp iv) nil' \fb ->
	createFramebuffers dvc sce rp ivs \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.INew fmt nm) sis ->
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HeteroParList.Nil HeteroParList.Nil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dvc sce rp (sciv :** scivs) (fb :** fbs) =
		Vk.Frmbffr.recreateNew dvc
			(mkFramebufferCreateInfoNew sce rp sciv) nil' fb >>
		recreateFramebuffers dvc sce rp scivs fbs

mkFramebufferCreateInfoNew ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.ImgVw.INew fmt nm si ->
	Vk.Frmbffr.CreateInfoNew 'Nothing sr '[ '(fmt, nm, si)]
mkFramebufferCreateInfoNew sce rp attch = Vk.Frmbffr.CreateInfoNew {
	Vk.Frmbffr.createInfoNextNew = TMaybe.N,
	Vk.Frmbffr.createInfoFlagsNew = zeroBits,
	Vk.Frmbffr.createInfoRenderPassNew = rp,
	Vk.Frmbffr.createInfoAttachmentsNew = U3 attch :** HeteroParList.Nil,
	Vk.Frmbffr.createInfoWidthNew = w, Vk.Frmbffr.createInfoHeightNew = h,
	Vk.Frmbffr.createInfoLayersNew = 1 }
	where
	Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = sce

createCommandPool :: QueueFamilyIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPool.C sc -> IO a) -> IO a
createCommandPool qfis dvc f =
	Vk.CmdPool.create dvc poolInfo nil' \cp -> f cp
	where poolInfo = Vk.CmdPool.CreateInfo {
		Vk.CmdPool.createInfoNext = TMaybe.N,
		Vk.CmdPool.createInfoFlags =
			Vk.CmdPool.CreateResetCommandBufferBit,
		Vk.CmdPool.createInfoQueueFamilyIndex = graphicsFamily qfis }

createVertexBuffer :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] -> IO a ) -> IO a
createVertexBuffer phdvc dvc gq cp f =
	createBufferList phdvc dvc (length vertices)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList phdvc dvc (length vertices)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit ) \(b' :: Vk.Bffr.Binded sm sb "vertex-buffer" '[VObj.List 256 t ""]) bm' -> do
	Vk.Mem.write @"vertex-buffer" @(VObj.List 256 Vertex "") dvc bm' zeroBits vertices
	copyBuffer dvc gq cp b' b
	f b

createBufferList :: forall sd nm t a . Storable t =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Int -> Vk.Bffr.UsageFlags ->
	Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 t ""] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.K.Buffer nm '[VObj.List 256 t ""] ) ] ->
		IO a) ->
	IO a
createBufferList p dv ln usg props =
	createBuffer' p dv (VObj.ObjectLengthList ln) usg props

createBuffer' :: forall sd nm o a . VObj.SizeAlignment o =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> VObj.ObjectLength o ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mem.M sm
			'[ '(sb, 'Vk.Mem.K.Buffer nm '[o])] ->
		IO a) -> IO a
createBuffer' p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil' \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Mem.allocateBind dv (HeteroParList.Singleton . U2 $ Vk.Mem.Buffer b)
		(allcInfo mt) nil'
		$ f . \(HeteroParList.Singleton (U2 (Vk.Mem.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HeteroParList.Singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.M.TypeIndex -> Vk.Dvc.Mem.Buffer.AllocateInfo 'Nothing
	allcInfo mt = Vk.Dvc.Mem.Buffer.AllocateInfo {
		Vk.Dvc.Mem.Buffer.allocateInfoNext = TMaybe.N,
		Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = mt }

findMemoryType :: Vk.PhDvc.P -> Vk.Mem.M.TypeBits -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.M.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.PhDvc.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> find ((&&)
		<$> (`Vk.Mem.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mem.M.mTypePropertyFlags . snd) tps
		where tps = Vk.PhDvc.memoryPropertiesMemoryTypes props1

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Vertex ""] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CmdBffr.allocateNew
--		@() dvc allocInfo \(HeteroParList.Singleton (cb :: Vk.CmdBffr.Binded s '[])) -> do
		dvc allocInfo \((cb :: Vk.CmdBffr.C s) :*. HeteroParList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
				Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
		Vk.CmdBffr.beginNew @'Nothing @'Nothing cb beginInfo do
			Vk.Cmd.copyBuffer @'[ '[VObj.List 256 Vertex ""]] cb src dst
		Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) Nothing
		Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfoNew 'Nothing sc '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfoNew {
		Vk.CmdBffr.allocateInfoNextNew = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPoolNew = cp,
		Vk.CmdBffr.allocateInfoLevelNew = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.M.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }

createCommandBuffer ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPool.C scp ->
	(forall scb . Vk.CmdBffr.C scb -> IO a) ->
	IO a
createCommandBuffer dvc cp f =
	Vk.CmdBffr.allocateNew dvc allocInfo $ f . \(cb :*. HeteroParList.Nil) -> cb
	where
	allocInfo :: Vk.CmdBffr.AllocateInfoNew 'Nothing scp '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfoNew {
		Vk.CmdBffr.allocateInfoNextNew = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPoolNew = cp,
		Vk.CmdBffr.allocateInfoLevelNew = Vk.CmdBffr.LevelPrimary }

class VssList (vss :: [[(Type, Vk.VtxInp.Rate)]]) where
	vssListIndex ::
		HeteroParList.PL (Vk.CmdBffr.Binded scb) vss -> Int ->
		Vk.CmdBffr.Binded scb '[ '(Vertex, 'Vk.VtxInp.RateVertex)]

instance VssList '[] where
	vssListIndex HeteroParList.Nil _ = error "index too large"

type Vs = '[ '(Vertex, 'Vk.VtxInp.RateVertex)]

instance VssList vss =>
	VssList ('[ '(Vertex, 'Vk.VtxInp.RateVertex)] ': vss) where
	vssListIndex (cb :** _) 0 = cb
	vssListIndex (_ :** cbs) n = vssListIndex cbs (n - 1)

mkVss :: Int -> (forall (vss :: [[(Type, Vk.VtxInp.Rate)]]) .
	(TpLvlLst.Length [(Type, Vk.VtxInp.Rate)] vss, VssList vss) =>
	Proxy vss -> a) -> a
mkVss 0 f = f (Proxy @'[])
mkVss n f = mkVss (n - 1) \p -> f $ addTypeToProxy p

addTypeToProxy ::
	Proxy vss -> Proxy ('[ '(Vertex, 'Vk.VtxInp.RateVertex)] ': vss)
addTypeToProxy Proxy = Proxy

data SyncObjects (ssos :: (Type, Type, Type)) where
	SyncObjects :: {
		imageAvailableSemaphores :: Vk.Semaphore.S sias,
		renderFinishedSemaphores :: Vk.Semaphore.S srfs,
		inFlightFences :: Vk.Fence.F sfs } ->
		SyncObjects '(sias, srfs, sfs)

createSyncObjects ::
	Vk.Dvc.D sd -> (forall sias srfs siff . SyncObjects '(sias, srfs, siff) -> IO a ) -> IO a
createSyncObjects dvc f =
	Vk.Semaphore.create @'Nothing dvc def nil' \ias ->
	Vk.Semaphore.create @'Nothing dvc def nil' \rfs ->
	Vk.Fence.create @'Nothing dvc fncInfo nil' \iff ->
	f $ SyncObjects ias rfs iff
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sg sm sb nm sl .
	Vk.CmdBffr.C scb  ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]) ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] -> IO ()
recordCommandBuffer cb rp fb sce gpl vb =
	Vk.CmdBffr.beginNew @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb
		(HeteroParList.Singleton . U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @Vertex @"" vb) >>
	Vk.Cmd.draw cbb 3 1 0 0
	where
	rpInfo :: Vk.RndrPass.BeginInfo 'Nothing sr sf
		'[ 'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32]
	rpInfo = Vk.RndrPass.BeginInfo {
		Vk.RndrPass.beginInfoNext = TMaybe.N,
		Vk.RndrPass.beginInfoRenderPass = rp,
		Vk.RndrPass.beginInfoFramebuffer = fb,
		Vk.RndrPass.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = sce },
		Vk.RndrPass.beginInfoClearValues = HeteroParList.Singleton
			. Vk.ClearValueColor . fromJust $ rgbaDouble 0 0 0 1 }

mainLoop :: (RecreateFramebuffers ss sfs, Vk.T.FormatToValue fmt) =>
	FramebufferResized -> Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc fmt -> Vk.Extent2d ->
	HeteroParList.PL (Vk.ImgVw.INew fmt nm) ss ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[] '[] -> Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.CmdBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> IO ()
mainLoop g w sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs vb cb iasrfsifs = do
	($ ext0) $ fix \loop ext -> do
		Glfw.pollEvents
		runLoop w sfc phdvc qfis dvc gq pq sc g ext scivs rp ppllyt gpl fbs vb cb iasrfsifs loop
	Vk.Dvc.waitIdle dvc

runLoop :: (RecreateFramebuffers sis sfs, Vk.T.FormatToValue fmt) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc fmt -> FramebufferResized -> Vk.Extent2d ->
	HeteroParList.PL (Vk.ImgVw.INew fmt nm) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] '(sl, '[], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.CmdBffr.C scb ->
	SyncObjects '(sias, srfs, siff) ->
	(Vk.Extent2d -> IO ()) -> IO ()
runLoop win sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl fbs vb cb iasrfsifs loop = do
	catchAndRecreate win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop
		$ drawFrame dvc gq pq sc ext rp gpl fbs vb cb iasrfsifs
	cls <- Glfw.windowShouldClose win
	if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateSwapChainEtc
			win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs)

drawFrame :: forall sfs sd ssc fmt sr sg sm sb nm scb sias srfs siff sl .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> Vk.Khr.Swapchain.SNew ssc fmt ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Graphics.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] '(sl, '[], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.CmdBffr.C scb -> SyncObjects '(sias, srfs, siff) -> IO ()
drawFrame dvc gq pq sc ext rp gpl fbs vb cb (SyncObjects ias rfs iff) = do
	let	siff = HeteroParList.Singleton iff
	Vk.Fence.waitForFs dvc siff True maxBound
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc (Vk.Khr.Swapchain.sFromNew sc) uint64Max (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.resetNew cb def
	HeteroParList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext gpl vb
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers =
				HeteroParList.Singleton  cb,
			Vk.submitInfoSignalSemaphores = HeteroParList.Singleton rfs }
		presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphores = HeteroParList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = HeteroParList.Singleton
				$ Vk.Khr.SwapchainImageIndex (Vk.Khr.Swapchain.sFromNew sc) imgIdx }
	Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfo

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate :: (RecreateFramebuffers sis sfs, Vk.T.FormatToValue fmt) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swapchain.SNew ssc fmt ->
	HeteroParList.PL (Vk.ImgVw.INew fmt nm) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	(Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateSwapChainEtc
		win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs

recreateSwapChainEtc :: (
	RecreateFramebuffers sis sfs, Vk.T.FormatToValue fmt ) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swapchain.SNew ssc fmt -> HeteroParList.PL (Vk.ImgVw.INew fmt nm) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateSwapChainEtc win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	(scifmt, ext) <- recreateSwapChain win sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swapchain.getImagesNew dvc sc >>= \imgs ->
			recreateImageViewsNew dvc imgs scivs
		recreateGraphicsPipeline' dvc ext rp ppllyt gpl
		recreateFramebuffers dvc ext rp scivs fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize win = Glfw.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

data Vertex = Vertex { vertexPos :: Cglm.Vec2, vertexColor :: Cglm.Vec3 }
	deriving (Show, Generic)

instance Storable Vertex where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance SizeAlignmentList Vertex

instance SizeAlignmentListUntil Cglm.Vec2 Vertex
instance SizeAlignmentListUntil Cglm.Vec3 Vertex

instance Vk.Ppl.VertexInputSt.Formattable Cglm.Vec2 where
	formatOf = Vk.FormatR32g32Sfloat

instance Vk.Ppl.VertexInputSt.Formattable Cglm.Vec3 where
	formatOf = Vk.FormatR32g32b32Sfloat

instance Foreign.Storable.Generic.G Vertex where

vertices :: [Vertex]
vertices = [
	Vertex (Cglm.Vec2 $ 0.0 :. (- 0.5) :. NilL)
--		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL),
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 1.0 :. NilL),
	Vertex (Cglm.Vec2 $ 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL) ]

vertShaderModule :: Vk.Shader.Module.M 'Nothing 'GlslVertexShader 'Nothing
vertShaderModule = mkShaderModule glslVertexShaderMain

fragShaderModule :: Vk.Shader.Module.M 'Nothing 'GlslFragmentShader 'Nothing
fragShaderModule = mkShaderModule glslFragmentShaderMain

mkShaderModule :: Spv sknd -> Vk.Shader.Module.M 'Nothing sknd 'Nothing
mkShaderModule code = Vk.Shader.Module.M createInfo nil'
	where createInfo = Vk.Shader.Module.M.CreateInfo {
		Vk.Shader.Module.M.createInfoNext = TMaybe.N,
		Vk.Shader.Module.M.createInfoFlags = def,
		Vk.Shader.Module.M.createInfoCode = code }

[glslVertexShader|

#version 450

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec3 inColor;

layout(location = 0) out vec3 fragColor;

void
main()
{
	gl_Position = vec4(inPosition, 0.0, 1.0);
	fragColor = inColor;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec3 fragColor;

layout(location = 0) out vec4 outColor;

void
main()
{
	outColor = vec4(fragColor, 1.0);
}

|]
