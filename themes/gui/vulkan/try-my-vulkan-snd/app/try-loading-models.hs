{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Generics
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.Storable.HeteroList
import Foreign.Storable.SizeAlignment
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Data.Kind.Object
import Data.Default
import Data.Bits
import Data.Array hiding (indices)
import Data.TypeLevel.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*), pattern (:**))
import Data.Proxy
import Data.Bool
import Data.Maybe
import Data.List
import Data.IORef
import Data.List.Length
import Data.Word
import Data.Color
import Data.Time
import System.Environment
import Codec.Picture
import Codec.Picture.Tools

import qualified TypeLevel.List as TpLvlLst

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Storable as V
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
import qualified Gpu.Vulkan.TypeEnum as Vk.T
import qualified Gpu.Vulkan.Middle as Vk.M
import qualified Gpu.Vulkan.Core as Vk.C
import qualified Gpu.Vulkan.Enum as Vk
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
import qualified Gpu.Vulkan.QueueFamily.Middle as Vk.QueueFamily
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Device.Middle as Vk.Dvc.M
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Surface
import qualified Gpu.Vulkan.Khr.Surface.Middle as Vk.Khr.Surface.M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Surface.PhysicalDevice
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swapchain
import qualified Gpu.Vulkan.Khr.Swapchain.Type as Vk.Khr.Swapchain
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as Vk.Khr.Swapchain.M
import qualified Gpu.Vulkan.Image as Vk.Img
import qualified Gpu.Vulkan.Image.Enum as Vk.Img
import qualified Gpu.Vulkan.Image.Middle as Vk.Img.M
import qualified Gpu.Vulkan.ImageView as Vk.ImgVw
import qualified Gpu.Vulkan.ImageView.Enum as Vk.ImgVw
import qualified Gpu.Vulkan.Component as Vk.Component
import qualified Gpu.Vulkan.ShaderModule as Vk.Shader.Module
import qualified Gpu.Vulkan.ShaderModule.Middle as Vk.Shader.Module.M
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShdrSt
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.InpAsmbSt
import qualified Gpu.Vulkan.Pipeline.ViewportState as Vk.Ppl.ViewportSt
import qualified Gpu.Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Gpu.Vulkan.Pipeline.MultisampleState as Vk.Ppl.MltSmplSt
import qualified Gpu.Vulkan.Sample as Vk.Sample
import qualified Gpu.Vulkan.Sample.Enum as Vk.Sample
import qualified Gpu.Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.ClrBlndAtt
import qualified Gpu.Vulkan.ColorComponent.Enum as Vk.ClrCmp
import qualified Gpu.Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlndSt
import qualified Gpu.Vulkan.Pipeline.Layout as Vk.Ppl.Layout
import qualified Gpu.Vulkan.Attachment as Vk.Att
import qualified Gpu.Vulkan.Attachment.Enum as Vk.Att
import qualified Gpu.Vulkan.Subpass as Vk.Subpass
import qualified Gpu.Vulkan.Subpass.Enum as Vk.Subpass
import qualified Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
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
import qualified Gpu.Vulkan.Buffer.Enum as Vk.Bffr
import qualified Gpu.Vulkan.Memory.Middle as Vk.Mem.M
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mem
import qualified Gpu.Vulkan.Memory.AllocateInfo as Vk.Dvc.Mem.Buffer
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.Command as Vk.Cmd

import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet

import qualified Gpu.Vulkan.DescriptorSet.TypeLevel as Vk.DscSet.T
import qualified Gpu.Vulkan.Memory as Vk.Dvc.Mem.ImageBuffer
import qualified Gpu.Vulkan.Memory.Kind as Vk.Dvc.Mem.ImageBuffer.K

import qualified Gpu.Vulkan.Sampler as Vk.Smplr
import qualified Gpu.Vulkan.Sampler.Enum as Vk.Smplr
import qualified Gpu.Vulkan.Sampler.Middle as Vk.Smplr.M
import qualified Gpu.Vulkan.PhysicalDevice.Struct as Vk.PhDvc
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as Vk.Ppl.DptStnSt

import Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList(AddType)

import Tools
import Vertex
import Codec.Wavefront.Read
import Data.Vector.Storable.Indexing

main :: IO ()
main = do
	txfp : mdfp : _ <- getArgs
	g <- newFramebufferResized
	(`withWindow` g) \win -> createInstance \inst -> do
		if enableValidationLayers
			then setupDebugMessenger inst $ const $ run txfp mdfp win inst g
			else run txfp mdfp win inst g

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
maxFramesInFlight = 2

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
				. (Vk.M.layerPropertiesLayerName <$>)
			<$> Vk.Ist.M.enumerateLayerProperties
	extensions <- bool id (Vk.Ext.DbgUtls.extensionName :)
			enableValidationLayers
		<$> ((cstrToText `mapM`) =<< Glfw.getRequiredInstanceExtensions)
	print extensions
	let	appInfo = Vk.M.ApplicationInfo {
			Vk.M.applicationInfoNext = Nothing,
			Vk.M.applicationInfoApplicationName = "Hello Triangle",
			Vk.M.applicationInfoApplicationVersion =
				Vk.M.makeApiVersion 0 1 0 0,
			Vk.M.applicationInfoEngineName = "No Engine",
			Vk.M.applicationInfoEngineVersion =
				Vk.M.makeApiVersion 0 1 0 0,
			Vk.M.applicationInfoApiVersion = Vk.M.apiVersion_1_0 }
		createInfo :: Vk.Ist.M.CreateInfo
			(Vk.Ext.DbgUtls.Msngr.CreateInfo
				() () () () () ()) ()
		createInfo = Vk.Ist.M.CreateInfo {
			Vk.Ist.M.createInfoNext = Just debugMessengerCreateInfo,
			Vk.Ist.M.createInfoFlags = def,
			Vk.Ist.M.createInfoApplicationInfo = Just appInfo,
			Vk.Ist.M.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Ist.M.createInfoEnabledExtensionNames = extensions }
	Vk.Ist.create createInfo nil nil \i -> f i

instanceToMiddle :: Vk.Ist.I si -> Vk.Ist.M.I
instanceToMiddle (Vk.Ist.I inst) = inst

setupDebugMessenger ::
	Vk.Ist.I si ->
	(forall sm . Vk.Ext.DbgUtls.Msngr.M sm -> IO a) -> IO a
setupDebugMessenger ist f = Vk.Ext.DbgUtls.Msngr.create ist
	debugMessengerCreateInfo nil nil \m -> f m

debugMessengerCreateInfo :: Vk.Ext.DbgUtls.Msngr.CreateInfo () () () () () ()
debugMessengerCreateInfo = Vk.Ext.DbgUtls.Msngr.CreateInfo {
	Vk.Ext.DbgUtls.Msngr.createInfoNext = Nothing,
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

debugCallback :: Vk.Ext.DbgUtls.Msngr.FnCallback () () () () ()
debugCallback _msgSeverity _msgType cbdt _userData = False <$ Txt.putStrLn
	("validation layer: " <> Vk.Ext.DbgUtls.Msngr.callbackDataMessage cbdt)

run :: FilePath -> FilePath -> Glfw.Window -> Vk.Ist.I si -> FramebufferResized -> IO ()
run txfp mdfp w inst g =
	createSurface w inst \sfc ->
	pickPhysicalDevice inst sfc >>= \(phdv, qfis) ->
	createLogicalDevice phdv qfis \dv gq pq ->
	createSwapChain w sfc phdv qfis dv
		\(sc :: Vk.Khr.Swapchain.SNew ss scifmt) ext ->
	Vk.Khr.Swapchain.getImagesNew dv sc >>= \imgs ->
	createImageViews dv imgs \scivs ->
	findDepthFormat phdv >>= \dptfmt ->
	Vk.T.formatToType dptfmt \(_ :: Proxy dptfmt) ->
	createRenderPass @scifmt @dptfmt dv \rp ->
	createPipelineLayout' dv \dscslyt ppllyt ->
	createGraphicsPipeline' dv ext rp ppllyt \gpl ->
	createCommandPool qfis dv \cp ->
	createDepthResources phdv dv gq cp ext \dptImg dptImgMem dptImgVw ->
	createFramebuffers dv ext rp scivs dptImgVw \fbs ->
	createTextureImage phdv dv gq cp txfp \tximg ->
	createImageView @'Vk.T.FormatR8g8b8a8Srgb dv tximg Vk.Img.AspectColorBit
		\(tximgvw :: Vk.ImgVw.INew txfmt "texture" siv) ->
	createTextureSampler phdv dv \(txsmplr :: Vk.Smplr.S ssmp) ->
	loadModel mdfp >>= \(vtcs, idcs) ->
	createVertexBuffer phdv dv gq cp vtcs \vb ->
	createIndexBuffer phdv dv gq cp idcs \ib ->
	createDescriptorPool dv \dscp ->
	createUniformBuffers @ssmp @siv phdv dv dscslyt maxFramesInFlight \dscslyts ubs ums ->
	createDescriptorSets @_ @_ @ssmp @siv dv dscp ubs dscslyts tximgvw txsmplr >>= \dscss ->
	createCommandBuffers dv cp \cbs ->
	createSyncObjects dv \sos ->
	getCurrentTime >>= \tm ->
	mainLoop g w sfc phdv qfis dv gq pq sc ext scivs rp ppllyt gpl fbs cp (dptImg, dptImgMem, dptImgVw) idcs vb ib cbs sos ubs ums dscss tm

createSurface :: Glfw.Window -> Vk.Ist.I si ->
	(forall ss . Vk.Khr.Surface.S ss -> IO a) -> IO a
createSurface win ist f = Glfw.createWindowSurface ist win nil nil \sfc -> f sfc

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
	deviceFeatures <- Vk.PhDvc.getFeatures phdvc
	is <- findQueueFamilies phdvc sfc
	extensionSupported <- checkDeviceExtensionSupport phdvc
	if extensionSupported && Vk.PhDvc.featuresSamplerAnisotropy deviceFeatures
	then (<$> querySwapChainSupport phdvc sfc) \spp ->
		bool (completeQueueFamilies is) Nothing
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
	null . (deviceExtensions \\) . (Vk.M.extensionPropertiesExtensionName <$>)
		<$> Vk.PhDvc.enumerateExtensionProperties dvc Nothing

deviceExtensions :: [Txt.Text]
deviceExtensions = [Vk.Khr.Swapchain.M.extensionName]

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.Khr.Surface.M.Capabilities,
	formats :: [Vk.Khr.Surface.M.Format],
	presentModes :: [Vk.Khr.PresentMode] }

querySwapChainSupport ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO SwapChainSupportDetails
querySwapChainSupport dvc sfc = SwapChainSupportDetails
	<$> Vk.Khr.Surface.PhysicalDevice.getCapabilities dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getFormats dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getPresentModes dvc sfc

createLogicalDevice :: Vk.PhDvc.P -> QueueFamilyIndices -> (forall sd .
		Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> IO a) -> IO a
createLogicalDevice phdvc qfis f =
	mkHeteroParList @() queueCreateInfos uniqueQueueFamilies \qs ->
	let	createInfo = Vk.Dvc.M.CreateInfo {
			Vk.Dvc.M.createInfoNext = Nothing,
			Vk.Dvc.M.createInfoFlags = def,
			Vk.Dvc.M.createInfoQueueCreateInfos = qs,
			Vk.Dvc.M.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Dvc.M.createInfoEnabledExtensionNames =
				deviceExtensions,
			Vk.Dvc.M.createInfoEnabledFeatures = Just def {
				Vk.PhDvc.featuresSamplerAnisotropy = True } } in
	Vk.Dvc.create @() phdvc createInfo nil nil \dvc -> do
		gq <- Vk.Dvc.getQueue dvc (graphicsFamily qfis) 0
		pq <- Vk.Dvc.getQueue dvc (presentFamily qfis) 0
		f dvc gq pq
	where
	uniqueQueueFamilies = nub [graphicsFamily qfis, presentFamily qfis]
	queueCreateInfos qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = Nothing,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }

mkHeteroParList :: Storable' s => (a -> t s) -> [a] ->
	(forall ss . WithPokedHeteroToListM ss => HeteroParList.PL t ss -> b) -> b
mkHeteroParList _k [] f = f HeteroParList.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

createSwapChain :: Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd ->
	(forall ss scfmt . Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swapchain.SNew ss scfmt -> Vk.C.Extent2d -> IO a) ->
	IO a
createSwapChain win sfc phdvc qfis dvc f = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	fmt = Vk.Khr.Surface.M.formatFormat
			. chooseSwapSurfaceFormat $ formats spp
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		let	crInfo = mkSwapchainCreateInfo sfc qfis spp ext
		Vk.Khr.Swapchain.createNew @() @_ @_ @fmt dvc crInfo nil nil
			\sc -> f sc ext

recreateSwapChain :: Vk.T.FormatToValue scfmt =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Khr.Swapchain.SNew ssc scfmt ->
	IO Vk.C.Extent2d
recreateSwapChain win sfc phdvc qfis0 dvc sc = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	crInfo = mkSwapchainCreateInfo sfc qfis0 spp ext
	ext <$ Vk.Khr.Swapchain.recreateNew @() dvc crInfo nil nil sc

mkSwapchainCreateInfo :: Vk.Khr.Surface.S ss -> QueueFamilyIndices ->
	SwapChainSupportDetails -> Vk.C.Extent2d ->
	Vk.Khr.Swapchain.CreateInfoNew n ss fmt
mkSwapchainCreateInfo sfc qfis0 spp ext =
	Vk.Khr.Swapchain.CreateInfoNew {
		Vk.Khr.Swapchain.createInfoNextNew = Nothing,
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

chooseSwapExtent :: Glfw.Window -> Vk.Khr.Surface.M.Capabilities -> IO Vk.C.Extent2d
chooseSwapExtent win caps
	| Vk.C.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> w, fromIntegral -> h) <-
			Glfw.getFramebufferSize win
		pure $ Vk.C.Extent2d
			(clamp w (Vk.C.extent2dWidth n) (Vk.C.extent2dHeight n))
			(clamp h (Vk.C.extent2dWidth x) (Vk.C.extent2dHeight x))
	where
	curExt = Vk.Khr.Surface.M.capabilitiesCurrentExtent caps
	n = Vk.Khr.Surface.M.capabilitiesMinImageExtent caps
	x = Vk.Khr.Surface.M.capabilitiesMaxImageExtent caps

createImageViews :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Img.BindedNew ss ss nm fmt] ->
	(forall si . HeteroParList.PL (Vk.ImgVw.INew fmt nm) si -> IO a) -> IO a
createImageViews _dvc [] f = f HeteroParList.Nil
createImageViews dvc (sci : scis) f =
	createImageView dvc sci Vk.Img.AspectColorBit \sciv ->
	createImageViews dvc scis \scivs -> f $ sciv :** scivs

recreateImageViews :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Img.BindedNew ss ss nm scfmt] -> HeteroParList.PL (Vk.ImgVw.INew scfmt nm) sis -> IO ()
recreateImageViews _dvc [] HeteroParList.Nil = pure ()
recreateImageViews dvc (sci : scis) (iv :** ivs) =
	Vk.ImgVw.recreateNew dvc (mkImageViewCreateInfo sci Vk.Img.AspectColorBit) nil nil iv >>
	recreateImageViews dvc scis ivs
recreateImageViews _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

createImageView :: forall ivfmt sd si sm nm ifmt a .
	Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.BindedNew si sm nm ifmt ->
	Vk.Img.AspectFlags ->
	(forall siv . Vk.ImgVw.INew ivfmt nm siv -> IO a) -> IO a
createImageView dvc timg asps f =
	Vk.ImgVw.createNew dvc (mkImageViewCreateInfo timg asps) nil nil f

recreateImageView :: Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.BindedNew si sm nm ifmt ->
	Vk.Img.AspectFlags ->
	Vk.ImgVw.INew ivfmt nm s -> IO ()
recreateImageView dvc timg asps iv =
	Vk.ImgVw.recreateNew dvc (mkImageViewCreateInfo timg asps) nil nil iv

mkImageViewCreateInfo ::
	Vk.Img.BindedNew si sm nm ifmt -> Vk.Img.AspectFlags ->
	Vk.ImgVw.CreateInfoNew () si sm nm ifmt ivfmt
mkImageViewCreateInfo sci asps = Vk.ImgVw.CreateInfoNew {
	Vk.ImgVw.createInfoNextNew = Nothing,
	Vk.ImgVw.createInfoFlagsNew = zeroBits,
	Vk.ImgVw.createInfoImageNew = sci,
	Vk.ImgVw.createInfoViewTypeNew = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponentsNew = components,
	Vk.ImgVw.createInfoSubresourceRangeNew = subresourceRange }
	where
	components = Vk.Component.Mapping {
		Vk.Component.mappingR = def, Vk.Component.mappingG = def,
		Vk.Component.mappingB = def, Vk.Component.mappingA = def }
	subresourceRange = Vk.Img.M.SubresourceRange {
		Vk.Img.M.subresourceRangeAspectMask = asps,
		Vk.Img.M.subresourceRangeBaseMipLevel = 0,
		Vk.Img.M.subresourceRangeLevelCount = 1,
		Vk.Img.M.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.M.subresourceRangeLayerCount = 1 }

createRenderPass ::
	forall (scifmt :: Vk.T.Format) (dptfmt :: Vk.T.Format) sd a . (
	Vk.T.FormatToValue scifmt, Vk.T.FormatToValue dptfmt ) =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPass.R sr -> IO a) -> IO a
createRenderPass dvc f = do
	let	colorAttachment :: Vk.Att.DescriptionNew scifmt
		colorAttachment = Vk.Att.DescriptionNew {
			Vk.Att.descriptionFlagsNew = zeroBits,
			Vk.Att.descriptionSamplesNew = Vk.Sample.Count1Bit,
			Vk.Att.descriptionLoadOpNew = Vk.Att.LoadOpClear,
			Vk.Att.descriptionStoreOpNew = Vk.Att.StoreOpStore,
			Vk.Att.descriptionStencilLoadOpNew = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOpNew =
				Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayoutNew =
				Vk.Img.LayoutUndefined,
			Vk.Att.descriptionFinalLayoutNew =
				Vk.Img.LayoutPresentSrcKhr }
		colorAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 0,
			Vk.Att.referenceLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
		depthAttachment :: Vk.Att.DescriptionNew dptfmt
		depthAttachment = Vk.Att.DescriptionNew {
			Vk.Att.descriptionFlagsNew = zeroBits,
			Vk.Att.descriptionSamplesNew = Vk.Sample.Count1Bit,
			Vk.Att.descriptionLoadOpNew = Vk.Att.LoadOpClear,
			Vk.Att.descriptionStoreOpNew = Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionStencilLoadOpNew =
				Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOpNew =
				Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayoutNew =
				Vk.Img.LayoutUndefined,
			Vk.Att.descriptionFinalLayoutNew =
				Vk.Img.LayoutDepthStencilAttachmentOptimal }
		depthAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 1,
			Vk.Att.referenceLayout =
				Vk.Img.LayoutDepthStencilAttachmentOptimal }
		subpass = Vk.Subpass.Description {
			Vk.Subpass.descriptionFlags = zeroBits,
			Vk.Subpass.descriptionPipelineBindPoint =
				Vk.Ppl.BindPointGraphics,
			Vk.Subpass.descriptionInputAttachments = [],
			Vk.Subpass.descriptionColorAndResolveAttachments =
				Left [colorAttachmentRef],
			Vk.Subpass.descriptionDepthStencilAttachment =
				Just depthAttachmentRef,
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
			Vk.RndrPass.M.createInfoNextNew = Nothing,
			Vk.RndrPass.M.createInfoFlagsNew = zeroBits,
			Vk.RndrPass.M.createInfoAttachmentsNew =
				colorAttachment :** depthAttachment :** HeteroParList.Nil,
			Vk.RndrPass.M.createInfoSubpassesNew = [subpass],
			Vk.RndrPass.M.createInfoDependenciesNew = [dependency] }
	Vk.RndrPass.createNew @'[scifmt, dptfmt] @() dvc renderPassInfo nil nil \rp -> f rp

type AtomUbo s = '(s, '[
	'Vk.DscSetLyt.Buffer '[ 'Atom 256 UniformBufferObject 'Nothing],
	'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ])

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscSetLyt.L s '[
		'Vk.DscSetLyt.Buffer '[ 'Atom 256 UniformBufferObject 'Nothing],
		'Vk.DscSetLyt.Image
			'[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ] -> IO a) ->
	IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil nil
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo () '[
		'Vk.DscSetLyt.Buffer '[ 'Atom 256 UniformBufferObject 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = Nothing,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings =
			uboLayoutBinding :**
			samplerLayoutBinding :** HeteroParList.Nil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[ 'Atom 256 UniformBufferObject 'Nothing])
	uboLayoutBinding = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	samplerLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)])
	samplerLayoutBinding = Vk.DscSetLyt.BindingImage {
		Vk.DscSetLyt.bindingImageDescriptorType =
			Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscSetLyt.bindingImageStageFlags =
			Vk.ShaderStageFragmentBit }

createPipelineLayout' ::
	Vk.Dvc.D sd -> (forall sdsl sl .
		Vk.DscSetLyt.L sdsl '[
			'Vk.DscSetLyt.Buffer '[ 'Atom 256 UniformBufferObject 'Nothing],
			'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ] ->
		Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] -> IO b) -> IO b
createPipelineLayout' dvc f =
	createDescriptorSetLayout dvc \dsl ->
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfoNew {
			Vk.Ppl.Layout.createInfoNextNew = Nothing,
			Vk.Ppl.Layout.createInfoFlagsNew = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayoutsNew =
				HeteroParList.Singleton $ U2 dsl } in
	Vk.Ppl.Layout.createNew @_ @_ @'[] @() @() @() dvc pipelineLayoutInfo nil nil $ f dsl

createGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)] -> IO a) -> IO a
createGraphicsPipeline' dvc sce rp ppllyt f =
	Vk.Ppl.Graphics.createGs dvc Nothing (U14 pplInfo :** HeteroParList.Nil)
			nil nil \(U2 gpl :** HeteroParList.Nil) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt

recreateGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)] -> IO ()
recreateGraphicsPipeline' dvc sce rp ppllyt gpls = Vk.Ppl.Graphics.recreateGs
	dvc Nothing (U14 pplInfo :** HeteroParList.Nil) nil nil (U2 gpls :** HeteroParList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt

mkGraphicsPipelineCreateInfo' ::
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.CreateInfo () '[
			'((), (), 'GlslVertexShader, (), (), '[]),
			'((), (), 'GlslFragmentShader, (), (), '[]) ]
		'(	(), '[AddType Vertex 'Vk.VtxInp.RateVertex],
			'[ '(0, Pos), '(1, Color), '(2, TexCoord)] )
		() () () () () () () () '(sl, '[AtomUbo sdsl], '[]) sr '(sb, vs', ts')
mkGraphicsPipelineCreateInfo' sce rp ppllyt = Vk.Ppl.Graphics.CreateInfo {
	Vk.Ppl.Graphics.createInfoNext = Nothing,
	Vk.Ppl.Graphics.createInfoFlags = Vk.Ppl.CreateFlagsZero,
	Vk.Ppl.Graphics.createInfoStages = shaderStages,
	Vk.Ppl.Graphics.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Graphics.createInfoInputAssemblyState = Just inputAssembly,
	Vk.Ppl.Graphics.createInfoViewportState = Just $ mkViewportState sce,
	Vk.Ppl.Graphics.createInfoRasterizationState = Just rasterizer,
	Vk.Ppl.Graphics.createInfoMultisampleState = Just multisampling,
	Vk.Ppl.Graphics.createInfoDepthStencilState = Just depthStencil,
	Vk.Ppl.Graphics.createInfoColorBlendState = Just colorBlending,
	Vk.Ppl.Graphics.createInfoDynamicState = Nothing,
	Vk.Ppl.Graphics.createInfoLayout = U3 ppllyt,
	Vk.Ppl.Graphics.createInfoRenderPass = rp,
	Vk.Ppl.Graphics.createInfoSubpass = 0,
	Vk.Ppl.Graphics.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Graphics.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Graphics.createInfoTessellationState = Nothing }
	where depthStencil = Vk.Ppl.DptStnSt.CreateInfo {
		Vk.Ppl.DptStnSt.createInfoNext = Nothing,
		Vk.Ppl.DptStnSt.createInfoFlags = zeroBits,
		Vk.Ppl.DptStnSt.createInfoDepthTestEnable = True,
		Vk.Ppl.DptStnSt.createInfoDepthWriteEnable = True,
		Vk.Ppl.DptStnSt.createInfoDepthCompareOp = Vk.CompareOpLess,
		Vk.Ppl.DptStnSt.createInfoDepthBoundsTestEnable = False,
		Vk.Ppl.DptStnSt.createInfoStencilTestEnable = False,
		Vk.Ppl.DptStnSt.createInfoFront = def,
		Vk.Ppl.DptStnSt.createInfoBack = def,
		Vk.Ppl.DptStnSt.createInfoMinDepthBounds = 0,
		Vk.Ppl.DptStnSt.createInfoMaxDepthBounds = 1 }

shaderStages :: HeteroParList.PL (U6 Vk.Ppl.ShdrSt.CreateInfoNew) '[
	'((), (), 'GlslVertexShader, (), (), '[]),
	'((), (), 'GlslFragmentShader, (), (), '[]) ]
shaderStages = U6 vertShaderStageInfo :** U6 fragShaderStageInfo :** HeteroParList.Nil
	where
	vertShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfoNew {
		Vk.Ppl.ShdrSt.createInfoNextNew = Nothing,
		Vk.Ppl.ShdrSt.createInfoFlagsNew = def,
		Vk.Ppl.ShdrSt.createInfoStageNew = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModuleNew = vertShaderModule,
		Vk.Ppl.ShdrSt.createInfoNameNew = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfoNew = Nothing }
	fragShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfoNew {
		Vk.Ppl.ShdrSt.createInfoNextNew = Nothing,
		Vk.Ppl.ShdrSt.createInfoFlagsNew = def,
		Vk.Ppl.ShdrSt.createInfoStageNew = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModuleNew = fragShaderModule,
		Vk.Ppl.ShdrSt.createInfoNameNew = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfoNew = Nothing }

inputAssembly :: Vk.Ppl.InpAsmbSt.CreateInfo ()
inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
	Vk.Ppl.InpAsmbSt.createInfoNext = Nothing,
	Vk.Ppl.InpAsmbSt.createInfoFlags = zeroBits,
	Vk.Ppl.InpAsmbSt.createInfoTopology = Vk.PrimitiveTopologyTriangleList,
	Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable = False }

mkViewportState :: Vk.C.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo n
mkViewportState sce = Vk.Ppl.ViewportSt.CreateInfo {
	Vk.Ppl.ViewportSt.createInfoNext = Nothing,
	Vk.Ppl.ViewportSt.createInfoFlags = zeroBits,
	Vk.Ppl.ViewportSt.createInfoViewports = [viewport],
	Vk.Ppl.ViewportSt.createInfoScissors = [scissor] }
	where
	viewport = Vk.C.Viewport {
		Vk.C.viewportX = 0, Vk.C.viewportY = 0,
		Vk.C.viewportWidth = fromIntegral $ Vk.C.extent2dWidth sce,
		Vk.C.viewportHeight = fromIntegral $ Vk.C.extent2dHeight sce,
		Vk.C.viewportMinDepth = 0, Vk.C.viewportMaxDepth = 1 }
	scissor = Vk.C.Rect2d {
		Vk.C.rect2dOffset = Vk.C.Offset2d 0 0, Vk.C.rect2dExtent = sce }

rasterizer :: Vk.Ppl.RstSt.CreateInfo ()
rasterizer = Vk.Ppl.RstSt.CreateInfo {
	Vk.Ppl.RstSt.createInfoNext = Nothing,
	Vk.Ppl.RstSt.createInfoFlags = zeroBits,
	Vk.Ppl.RstSt.createInfoDepthClampEnable = False,
	Vk.Ppl.RstSt.createInfoRasterizerDiscardEnable = False,
	Vk.Ppl.RstSt.createInfoPolygonMode = Vk.PolygonModeFill,
	Vk.Ppl.RstSt.createInfoLineWidth = 1,
	Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeBackBit,
	Vk.Ppl.RstSt.createInfoFrontFace = Vk.FrontFaceCounterClockwise,
	Vk.Ppl.RstSt.createInfoDepthBiasEnable = False,
	Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0 }

multisampling :: Vk.Ppl.MltSmplSt.CreateInfo ()
multisampling = Vk.Ppl.MltSmplSt.CreateInfo {
	Vk.Ppl.MltSmplSt.createInfoNext = Nothing,
	Vk.Ppl.MltSmplSt.createInfoFlags = zeroBits,
	Vk.Ppl.MltSmplSt.createInfoSampleShadingEnable = False,
	Vk.Ppl.MltSmplSt.createInfoRasterizationSamplesAndMask =
		Vk.Sample.CountAndMask Vk.Sample.Count1Bit Nothing,
	Vk.Ppl.MltSmplSt.createInfoMinSampleShading = 1,
	Vk.Ppl.MltSmplSt.createInfoAlphaToCoverageEnable = False,
	Vk.Ppl.MltSmplSt.createInfoAlphaToOneEnable = False }

colorBlending :: Vk.Ppl.ClrBlndSt.CreateInfo ()
colorBlending = Vk.Ppl.ClrBlndSt.CreateInfo {
	Vk.Ppl.ClrBlndSt.createInfoNext = Nothing,
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

createFramebuffers :: Vk.Dvc.D sd -> Vk.C.Extent2d ->
	Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.INew fmt nm) sis ->
	Vk.ImgVw.INew dptfmt dptnm siv ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HeteroParList.Nil _ f = f HeteroParList.Nil
createFramebuffers dvc sce rp (iv :** ivs) dptiv f =
	Vk.Frmbffr.createNew dvc (mkFramebufferCreateInfo sce rp iv dptiv) nil nil \fb ->
	createFramebuffers dvc sce rp ivs dptiv \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.C.Extent2d ->
		Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.INew scfmt nm) sis ->
		Vk.ImgVw.INew dptfmt dptnm sdiv ->
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HeteroParList.Nil _ HeteroParList.Nil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dvc sce rp (sciv :** scivs) dptiv (fb :** fbs) =
		Vk.Frmbffr.recreateNew dvc
			(mkFramebufferCreateInfo sce rp sciv dptiv) nil nil fb >>
		recreateFramebuffers dvc sce rp scivs dptiv fbs

mkFramebufferCreateInfo ::
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.ImgVw.INew fmt nm si ->
	Vk.ImgVw.INew dptfmt dptnm sdiv ->
	Vk.Frmbffr.CreateInfoNew () sr
		'[ '(fmt, nm, si), '(dptfmt, dptnm, sdiv)]
mkFramebufferCreateInfo sce rp attch dpt = Vk.Frmbffr.CreateInfoNew {
	Vk.Frmbffr.createInfoNextNew = Nothing,
	Vk.Frmbffr.createInfoFlagsNew = zeroBits,
	Vk.Frmbffr.createInfoRenderPassNew = rp,
	Vk.Frmbffr.createInfoAttachmentsNew = U3 attch :** U3 dpt :** HeteroParList.Nil,
	Vk.Frmbffr.createInfoWidthNew = w, Vk.Frmbffr.createInfoHeightNew = h,
	Vk.Frmbffr.createInfoLayersNew = 1 }
	where
	Vk.C.Extent2d { Vk.C.extent2dWidth = w, Vk.C.extent2dHeight = h } = sce

createCommandPool :: QueueFamilyIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPool.C sc -> IO a) -> IO a
createCommandPool qfis dvc f =
	Vk.CmdPool.create @() dvc poolInfo nil nil \cp -> f cp
	where poolInfo = Vk.CmdPool.CreateInfo {
		Vk.CmdPool.createInfoNext = Nothing,
		Vk.CmdPool.createInfoFlags =
			Vk.CmdPool.CreateResetCommandBufferBit,
		Vk.CmdPool.createInfoQueueFamilyIndex = graphicsFamily qfis }

createDepthResources ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.C.Extent2d ->
	(forall si sm fmt siv . Vk.T.FormatToValue fmt =>
		Vk.Img.BindedNew si sm nm fmt ->
		Vk.Dvc.Mem.ImageBuffer.M sm
			'[ '(si, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt) ] ->
		Vk.ImgVw.INew fmt nm siv ->
		IO a) -> IO a
createDepthResources phdvc dvc gq cp ext f = do
	fmt <- findDepthFormat phdvc
	print fmt
	print ext
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		createImage @_ @fmt phdvc dvc
			(Vk.C.extent2dWidth ext) (Vk.C.extent2dHeight ext)
			Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
			Vk.Mem.PropertyDeviceLocalBit \dptImg dptImgMem ->
			createImageView @fmt
				dvc dptImg Vk.Img.AspectDepthBit \dptImgVw -> do
			transitionImageLayout dvc gq cp dptImg Vk.Img.LayoutUndefined
				Vk.Img.LayoutDepthStencilAttachmentOptimal
			f dptImg dptImgMem dptImgVw

recreateDepthResources :: Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.C.Extent2d ->
	Vk.Img.BindedNew sb sm nm fmt ->
	Vk.Dvc.Mem.ImageBuffer.M
		sm '[ '(sb, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt)] ->
	Vk.ImgVw.INew fmt nm sdiv -> IO ()
recreateDepthResources phdvc dvc gq cp ext dptImg dptImgMem dptImgVw = do
	print ext
	recreateImage phdvc dvc
		(Vk.C.extent2dWidth ext) (Vk.C.extent2dHeight ext)
		Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mem.PropertyDeviceLocalBit dptImg dptImgMem
	recreateImageView dvc dptImg Vk.Img.AspectDepthBit dptImgVw
	transitionImageLayout dvc gq cp dptImg Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal

type DepthResources sb sm nm fmt sdiv = (
	Vk.Img.BindedNew sb sm nm fmt,
	Vk.Dvc.Mem.ImageBuffer.M
		sm '[ '(sb, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt)],
	Vk.ImgVw.INew fmt nm sdiv )

findDepthFormat :: Vk.PhDvc.P -> IO Vk.Format
findDepthFormat phdvc = findSupportedFormat phdvc
	[Vk.FormatD32Sfloat, Vk.FormatD32SfloatS8Uint, Vk.FormatD24UnormS8Uint]
	Vk.Img.TilingOptimal
	Vk.FormatFeatureDepthStencilAttachmentBit

findSupportedFormat ::
	Vk.PhDvc.P -> [Vk.Format] -> Vk.Img.Tiling -> Vk.FormatFeatureFlags -> IO Vk.Format
findSupportedFormat phdvc fs tlng fffs = do
	props <- Vk.PhDvc.getFormatProperties phdvc `mapM` fs
	case tlng of
		Vk.Img.TilingLinear -> do
			putStrLn "LINEAR"
			pure . orError . find (checkFeatures fffs . snd) . zip fs
				$ Vk.formatPropertiesLinearTilingFeatures <$> props
		Vk.Img.TilingOptimal -> do
			putStrLn "OPTIMAL"
			pure . orError . find (checkFeatures fffs . snd) . zip fs
				$ Vk.formatPropertiesOptimalTilingFeatures <$> props
		_ -> error "no such image tiling"
	where orError = \case
		Just (x, _) -> x
		Nothing -> error "failed to find supported format!"

checkFeatures :: Vk.FormatFeatureFlags -> Vk.FormatFeatureFlags -> Bool
checkFeatures wntd ftrs = wntd .&. ftrs == wntd

createTextureImage ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> FilePath -> (
		forall si sm .
		Vk.Img.BindedNew si sm nm 'Vk.T.FormatR8g8b8a8Srgb -> IO a ) ->
	IO a
createTextureImage phdvc dvc gq cp txfp f = do
	img <- readRgba8 txfp
	print . V.length $ imageData img
	let	wdt = fromIntegral $ imageWidth img
		hgt = fromIntegral $ imageHeight img
	createImage @_ @'Vk.T.FormatR8g8b8a8Srgb phdvc dvc wdt hgt Vk.Img.TilingOptimal
		(Vk.Img.UsageTransferDstBit .|.  Vk.Img.UsageSampledBit)
		Vk.Mem.PropertyDeviceLocalBit \tximg _txmem -> do
		createBufferImage @MyImage @_ phdvc dvc
			(fromIntegral wdt, fromIntegral wdt, fromIntegral hgt, 1)
			Vk.Bffr.UsageTransferSrcBit
			(	Vk.Mem.PropertyHostVisibleBit .|.
				Vk.Mem.PropertyHostCoherentBit )
			\(sb :: Vk.Bffr.Binded
				sm sb "texture-buffer" '[ 'ObjImage a inm]) sbm -> do
			Vk.Dvc.Mem.ImageBuffer.write @"texture-buffer"
				@('ObjImage MyImage inm) dvc sbm zeroBits (MyImage img)
			print sb
			transitionImageLayout dvc gq cp tximg
				Vk.Img.LayoutUndefined
				Vk.Img.LayoutTransferDstOptimal
			copyBufferToImage dvc gq cp sb tximg wdt hgt
			transitionImageLayout dvc gq cp tximg
				Vk.Img.LayoutTransferDstOptimal
				Vk.Img.LayoutShaderReadOnlyOptimal
			f tximg

copyBufferToImage :: forall sd sc sm sb nm img inm si sm' nm' .
	Storable (IsImagePixel img) =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[ 'ObjImage img inm]  ->
	Vk.Img.BindedNew si sm' nm' (Vk.Bffr.ImageFormat img) ->
	Word32 -> Word32 -> IO ()
copyBufferToImage dvc gq cp bf img wdt hgt =
	beginSingleTimeCommands dvc gq cp \cb -> do
	let	region :: Vk.Bffr.ImageCopy img inm
		region = Vk.Bffr.ImageCopy {
			Vk.Bffr.imageCopyImageSubresource = isr,
			Vk.Bffr.imageCopyImageOffset = Vk.C.Offset3d 0 0 0,
			Vk.Bffr.imageCopyImageExtent = Vk.C.Extent3d wdt hgt 1 }
		isr = Vk.Img.M.SubresourceLayers {
			Vk.Img.M.subresourceLayersAspectMask =
				Vk.Img.AspectColorBit,
			Vk.Img.M.subresourceLayersMipLevel = 0,
			Vk.Img.M.subresourceLayersBaseArrayLayer = 0,
			Vk.Img.M.subresourceLayersLayerCount = 1 }
	Vk.Cmd.copyBufferToImage
		cb bf img Vk.Img.LayoutTransferDstOptimal (HeteroParList.Singleton region)

transitionImageLayout :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Img.BindedNew si sm nm fmt -> Vk.Img.Layout -> Vk.Img.Layout ->
	IO ()
transitionImageLayout dvc gq cp img olyt nlyt =
	beginSingleTimeCommands dvc gq cp \cb -> do
	let	barrier :: Vk.Img.MemoryBarrier () si sm nm fmt
		barrier = Vk.Img.MemoryBarrier {
			Vk.Img.memoryBarrierNext = Nothing,
			Vk.Img.memoryBarrierOldLayout = olyt,
			Vk.Img.memoryBarrierNewLayout = nlyt,
			Vk.Img.memoryBarrierSrcQueueFamilyIndex =
				Vk.QueueFamily.Ignored,
			Vk.Img.memoryBarrierDstQueueFamilyIndex =
				Vk.QueueFamily.Ignored,
			Vk.Img.memoryBarrierImage = img,
			Vk.Img.memoryBarrierSubresourceRange = srr,
			Vk.Img.memoryBarrierSrcAccessMask = sam,
			Vk.Img.memoryBarrierDstAccessMask = dam }
		srr = Vk.Img.SubresourceRange {
			Vk.Img.subresourceRangeAspectMask = asps,
			Vk.Img.subresourceRangeBaseMipLevel = 0,
			Vk.Img.subresourceRangeLevelCount = 1,
			Vk.Img.subresourceRangeBaseArrayLayer = 0,
			Vk.Img.subresourceRangeLayerCount = 1 }
	Vk.Cmd.pipelineBarrier cb
		sstg dstg zeroBits HeteroParList.Nil HeteroParList.Nil (HeteroParList.Singleton $ U5 barrier)
	where
	asps = case (nlyt, hasStencilComponentType @fmt) of
		(Vk.Img.LayoutDepthStencilAttachmentOptimal, hsst) ->
			Vk.Img.AspectDepthBit .|.
			bool zeroBits Vk.Img.AspectStencilBit hsst
		_ -> Vk.Img.AspectColorBit
	(sam, dam, sstg, dstg) = case (olyt, nlyt) of
		(Vk.Img.LayoutUndefined, Vk.Img.LayoutTransferDstOptimal) -> (
			zeroBits, Vk.AccessTransferWriteBit,
			Vk.Ppl.StageTopOfPipeBit, Vk.Ppl.StageTransferBit )
		(Vk.Img.LayoutTransferDstOptimal,
			Vk.Img.LayoutShaderReadOnlyOptimal) -> (
			Vk.AccessTransferWriteBit, Vk.AccessShaderReadBit,
			Vk.Ppl.StageTransferBit, Vk.Ppl.StageFragmentShaderBit )
		(Vk.Img.LayoutUndefined,
			Vk.Img.LayoutDepthStencilAttachmentOptimal) -> (
			zeroBits,
			Vk.AccessDepthStencilAttachmentReadBit .|.
				Vk.AccessDepthStencilAttachmentWriteBit,
			Vk.Ppl.StageTopOfPipeBit,
			Vk.Ppl.StageEarlyFragmentTestsBit )
		_ -> error "unsupported layout transition!"

hasStencilComponentType ::
	forall (fmt :: Vk.T.Format) . Vk.T.FormatToValue fmt => Bool
hasStencilComponentType = hasStencilComponent (Vk.T.formatToValue @fmt)

hasStencilComponent :: Vk.Format -> Bool
hasStencilComponent = \case
	Vk.FormatD32SfloatS8Uint -> True
	Vk.FormatD24UnormS8Uint -> True
	_ -> False

beginSingleTimeCommands :: forall sd sc a .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	(forall s . Vk.CmdBffr.C s '[] -> IO a) -> IO a
beginSingleTimeCommands dvc gq cp cmd = do
	Vk.CmdBffr.allocateNew
		@() dvc allocInfo \(HeteroParList.Singleton (cb :: Vk.CmdBffr.C s '[])) -> do
		let	submitInfo :: Vk.SubmitInfo () '[] '[ '(s, '[])] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = Nothing,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
				Vk.submitInfoCommandBuffers = HeteroParList.Singleton $ U2 cb,
				Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
		Vk.CmdBffr.begin @() @() cb beginInfo (cmd cb) <* do
			Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) Nothing
			Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfoNew () sc '[ '[]]
	allocInfo = Vk.CmdBffr.AllocateInfoNew {
		Vk.CmdBffr.allocateInfoNextNew = Nothing,
		Vk.CmdBffr.allocateInfoCommandPoolNew = cp,
		Vk.CmdBffr.allocateInfoLevelNew = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.M.BeginInfo {
		Vk.CmdBffr.beginInfoNext = Nothing,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }

createBufferImage :: Storable (IsImagePixel t) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> (Int, Int, Int, Int) ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sb sm nm '[ 'ObjImage t inm] ->
		Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
			sb,
			'Vk.Dvc.Mem.ImageBuffer.K.Buffer nm '[ 'ObjImage t inm])] ->
		IO a) -> IO a
createBufferImage p dv (r, w, h, d) usg props =
	createBuffer' p dv (ObjectLengthImage r w h d) usg props

createBufferAtom :: forall sd nm a b . Storable a => Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sb sm nm '[ 'Atom 256 a 'Nothing] ->
		Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
			sb,
			'Vk.Dvc.Mem.ImageBuffer.K.Buffer nm '[ 'Atom 256 a 'Nothing] )] ->
			IO b) -> IO b
createBufferAtom p dv usg props = createBuffer' p dv ObjectLengthAtom usg props

createBufferList :: forall sd nm t a . Storable t =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Int -> Vk.Bffr.UsageFlags ->
	Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sb sm nm '[ 'List 256 t ""] ->
		Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
			sb,
			'Vk.Dvc.Mem.ImageBuffer.K.Buffer nm '[ 'List 256 t ""] ) ] ->
		IO a) ->
	IO a
createBufferList p dv ln usg props =
	createBuffer' p dv (ObjectLengthList ln) usg props

createBuffer' :: forall sd nm o a . Data.Kind.Object.SizeAlignment o =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> ObjectLength o ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sb sm nm '[o] ->
		Vk.Dvc.Mem.ImageBuffer.M sm
			'[ '(sb, 'Vk.Dvc.Mem.ImageBuffer.K.Buffer nm '[o])] ->
		IO a) -> IO a
createBuffer' p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Dvc.Mem.ImageBuffer.allocateBind dv (HeteroParList.Singleton . U2 $ Vk.Dvc.Mem.ImageBuffer.Buffer b)
		(allcInfo mt) nil nil
		$ f . \(HeteroParList.Singleton (U2 (Vk.Dvc.Mem.ImageBuffer.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo () '[o]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = Nothing,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HeteroParList.Singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.M.TypeIndex -> Vk.Dvc.Mem.Buffer.AllocateInfo ()
	allcInfo mt = Vk.Dvc.Mem.Buffer.AllocateInfo {
		Vk.Dvc.Mem.Buffer.allocateInfoNext = Nothing,
		Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = mt }

createImage :: forall nm fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mem.PropertyFlagBits -> (forall si sm .
		Vk.Img.BindedNew si sm nm fmt ->
		Vk.Dvc.Mem.ImageBuffer.M sm
			'[ '(si, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt) ] ->
		IO a) -> IO a
createImage pd dvc wdt hgt tlng usg prps f = Vk.Img.createNew @() @() @() dvc
		(imageInfo wdt hgt tlng usg) Nothing Nothing \img -> do
	memInfo <- imageMemoryInfo pd dvc prps img
	imageAllocateBind dvc img memInfo f

recreateImage :: Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mem.PropertyFlags ->
	Vk.Img.BindedNew sb sm nm fmt ->
	Vk.Dvc.Mem.ImageBuffer.M
		sm '[ '(sb, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt)] -> IO ()
recreateImage pd dvc wdt hgt tlng usg prps img mem = do
	Vk.Img.recreateNew @() @() @() dvc
		(imageInfo wdt hgt tlng usg) Nothing Nothing img
	memInfo <- imageMemoryInfoBinded pd dvc prps img
	imageReallocateBind dvc img memInfo mem

imageInfo ::
	Word32 -> Word32 -> Vk.Img.Tiling -> Vk.Img.UsageFlags ->
	Vk.Img.CreateInfoNew n fmt
imageInfo wdt hgt tlng usg = Vk.Img.CreateInfoNew {
		Vk.Img.createInfoNextNew = Nothing,
		Vk.Img.createInfoImageTypeNew = Vk.Img.Type2d,
		Vk.Img.createInfoExtentNew = Vk.C.Extent3d {
			Vk.C.extent3dWidth = wdt,
			Vk.C.extent3dHeight = hgt,
			Vk.C.extent3dDepth = 1 },
		Vk.Img.createInfoMipLevelsNew = 1,
		Vk.Img.createInfoArrayLayersNew = 1,
		Vk.Img.createInfoTilingNew = tlng,
		Vk.Img.createInfoInitialLayoutNew = Vk.Img.LayoutUndefined,
		Vk.Img.createInfoUsageNew = usg,
		Vk.Img.createInfoSharingModeNew = Vk.SharingModeExclusive,
		Vk.Img.createInfoSamplesNew = Vk.Sample.Count1Bit,
		Vk.Img.createInfoFlagsNew = zeroBits,
		Vk.Img.createInfoQueueFamilyIndicesNew = [] }

imageAllocateBind :: Vk.Dvc.D sd -> Vk.Img.INew si nm fmt ->
	Vk.Dvc.Mem.Buffer.AllocateInfo () -> (forall sm .
		Vk.Img.BindedNew si sm nm fmt ->
		Vk.Dvc.Mem.ImageBuffer.M sm
			'[ '(si, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt) ] ->
		IO a) -> IO a
imageAllocateBind dvc img memInfo f =
	Vk.Dvc.Mem.ImageBuffer.allocateBind @() dvc
		(HeteroParList.Singleton . U2 $ Vk.Dvc.Mem.ImageBuffer.Image img) memInfo
		nil nil \(HeteroParList.Singleton (U2 (Vk.Dvc.Mem.ImageBuffer.ImageBinded bnd))) m -> do
		f bnd m

imageReallocateBind ::
	Vk.Dvc.D sd -> Vk.Img.BindedNew sb sm nm fmt ->
	Vk.Dvc.Mem.Buffer.AllocateInfo () ->
	Vk.Dvc.Mem.ImageBuffer.M
		sm '[ '(sb, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt)] -> IO ()
imageReallocateBind dvc img memInfo m =
	Vk.Dvc.Mem.ImageBuffer.reallocateBind @() dvc
		(HeteroParList.Singleton . U2 $ Vk.Dvc.Mem.ImageBuffer.ImageBinded img) memInfo
		nil nil m

imageMemoryInfo ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Mem.PropertyFlags ->
	Vk.Img.INew s nm fmt -> IO (Vk.Dvc.Mem.Buffer.AllocateInfo n)
imageMemoryInfo pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirementsNew dvc img
	mt <- findMemoryType pd (Vk.Mem.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Dvc.Mem.Buffer.AllocateInfo {
		Vk.Dvc.Mem.Buffer.allocateInfoNext = Nothing,
		Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = mt }

imageMemoryInfoBinded ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Mem.PropertyFlags ->
	Vk.Img.BindedNew sm si nm fmt -> IO (Vk.Dvc.Mem.Buffer.AllocateInfo n)
imageMemoryInfoBinded pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirementsBindedNew dvc img
	mt <- findMemoryType pd (Vk.Mem.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Dvc.Mem.Buffer.AllocateInfo {
		Vk.Dvc.Mem.Buffer.allocateInfoNext = Nothing,
		Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = mt }

newtype MyImage = MyImage (Image PixelRGBA8)

type instance Vk.Bffr.ImageFormat MyImage = 'Vk.T.FormatR8g8b8a8Srgb

newtype MyRgba8 = MyRgba8 { unMyRgba8 :: PixelRGBA8 }

instance Storable MyRgba8 where
	sizeOf _ = 4 * sizeOf @Pixel8 undefined
	alignment _ = alignment @Pixel8 undefined
	peek p = MyRgba8 . (\(r, g, b, a) -> PixelRGBA8 r g b a) . listToTuple4
		<$> peekArray 4 (castPtr p)
	poke p (MyRgba8 (PixelRGBA8 r g b a)) =
		pokeArray (castPtr p) [r, g, b, a]

listToTuple4 :: [a] -> (a, a, a, a)
listToTuple4 [r, g, b, a] = (r, g, b, a)
listToTuple4 _ = error "The length of the list is not 4"

instance IsImage MyImage where
	type IsImagePixel MyImage = MyRgba8
	isImageRow = isImageWidth
	isImageWidth (MyImage img) = imageWidth img
	isImageHeight (MyImage img) = imageHeight img
	isImageDepth _ = 1
	isImageBody (MyImage img) = (<$> [0 .. imageHeight img - 1]) \y ->
		(<$> [0 .. imageWidth img - 1]) \x -> MyRgba8 $ pixelAt img x y
	isImageMake w h _d pss = MyImage
		$ generateImage (\x y -> let MyRgba8 p = (pss' ! y) ! x in p) w h
		where pss' = listArray (0, h - 1) (listArray (0, w - 1) <$> pss)

createTextureSampler ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> (forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTextureSampler phdv dvc f = do
	prp <- Vk.PhDvc.getProperties phdv
	print . Vk.PhDvc.limitsMaxSamplerAnisotropy $ Vk.PhDvc.propertiesLimits prp
	let	samplerInfo = Vk.Smplr.M.CreateInfo {
			Vk.Smplr.M.createInfoNext = Nothing,
			Vk.Smplr.M.createInfoFlags = zeroBits,
			Vk.Smplr.M.createInfoMagFilter = Vk.FilterLinear,
			Vk.Smplr.M.createInfoMinFilter = Vk.FilterLinear,
			Vk.Smplr.M.createInfoMipmapMode =
				Vk.Smplr.MipmapModeLinear,
			Vk.Smplr.M.createInfoAddressModeU =
				Vk.Smplr.AddressModeRepeat,
			Vk.Smplr.M.createInfoAddressModeV =
				Vk.Smplr.AddressModeRepeat,
			Vk.Smplr.M.createInfoAddressModeW =
				Vk.Smplr.AddressModeRepeat,
			Vk.Smplr.M.createInfoMipLodBias = 0,
			Vk.Smplr.M.createInfoAnisotropyEnable = True,
			Vk.Smplr.M.createInfoMaxAnisotropy =
				Vk.PhDvc.limitsMaxSamplerAnisotropy
					$ Vk.PhDvc.propertiesLimits prp,
			Vk.Smplr.M.createInfoCompareEnable = False,
			Vk.Smplr.M.createInfoCompareOp = Vk.CompareOpAlways,
			Vk.Smplr.M.createInfoMinLod = 0,
			Vk.Smplr.M.createInfoMaxLod = 0,
			Vk.Smplr.M.createInfoBorderColor =
				Vk.BorderColorIntOpaqueBlack,
			Vk.Smplr.M.createInfoUnnormalizedCoordinates = False }
	Vk.Smplr.create @() dvc samplerInfo nil nil f

loadModel :: FilePath -> IO (V.Vector Vertex, V.Vector Word32)
loadModel fp = do
	(vtcs, idcs) <- verticesIndices fp
	let	(vtcs', idcs') = indexingVector vtcs
	putStrLn "LOAD MODEL"
	putStrLn $ "vtcs : " ++ show (V.length (vtcs :: V.Vector Vertex))
	putStrLn $ "vtcs': " ++ show (V.length (vtcs' :: V.Vector Vertex))
	putStrLn $ "idcs : " ++ show (V.length (idcs :: V.Vector WWord32))
	putStrLn $ "idcs': " ++ show (V.length (idcs':: V.Vector WWord32))
	pure (vtcs', V.map Foreign.Storable.Generic.unWrap idcs')

createVertexBuffer :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> V.Vector Vertex -> (forall sm sb .
		Vk.Bffr.Binded sm sb "vertex-buffer" '[ 'List 256 Vertex ""] -> IO a) -> IO a
createVertexBuffer phdvc dvc gq cp vtcs f =
	createBufferList phdvc dvc (V.length vtcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList phdvc dvc (V.length vtcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
		\(b' :: Vk.Bffr.Binded sb sm "vertex-buffer" '[ 'List 256 t ""])
			(bm' :: Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
				sb,
				'Vk.Dvc.Mem.ImageBuffer.K.Buffer
					"vertex-buffer" '[ 'List 256 Vertex ""] )]) -> do
	Vk.Dvc.Mem.ImageBuffer.write @"vertex-buffer" @('List 256 Vertex "") dvc bm' zeroBits vtcs
	copyBuffer dvc gq cp b' b
	f b

createIndexBuffer :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> V.Vector Word32 -> (forall sm sb .
		Vk.Bffr.Binded sm sb "index-buffer" '[ 'List 256 Word32 ""] -> IO a) -> IO a
createIndexBuffer phdvc dvc gq cp idcs f =
	createBufferList phdvc dvc (V.length idcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageIndexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList phdvc dvc (V.length idcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
		\(b' :: Vk.Bffr.Binded sb sm "index-buffer" '[ 'List 256 t ""])
			(bm' :: Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
				sb,
				'Vk.Dvc.Mem.ImageBuffer.K.Buffer "index-buffer"
					'[ 'List 256 Word32 ""] )]) -> do
	Vk.Dvc.Mem.ImageBuffer.write @"index-buffer" @('List 256 Word32 "") dvc bm' zeroBits idcs
	copyBuffer dvc gq cp b' b
	f b

createUniformBuffers :: forall ssmp siv sd sdsc a .
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.DscSetLyt.L sdsc '[
		'Vk.DscSetLyt.Buffer '[ 'Atom 256 UniformBufferObject 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)]] ->
	Int -> (forall slyts smsbs . (
		HeteroParList.FromList slyts,
		Update smsbs slyts ssmp siv,
		DescriptorSetIndex slyts sdsc ) =>
		HeteroParList.PL Vk.DscSet.Layout slyts ->
		HeteroParList.PL BindedUbo smsbs ->
		HeteroParList.PL MemoryUbo smsbs -> IO a) -> IO a
createUniformBuffers _ _ _ 0 f = f HeteroParList.Nil HeteroParList.Nil HeteroParList.Nil
createUniformBuffers ph dvc dscslyt n f =
	createUniformBuffer1 ph dvc \(b :: BindedUbo smsb) m ->
		createUniformBuffers @ssmp @siv ph dvc dscslyt (n - 1) \ls (bs :: HeteroParList.PL BindedUbo smsbs) ms -> f
			(Vk.DscSet.Layout dscslyt :** ls)
			(b :** bs :: HeteroParList.PL BindedUbo (smsb ': smsbs))
			(m :** ms)

type family MapFst abs where
	MapFst '[] = '[]
	MapFst ( '(a, b, c :: k) ': abs) = a ': MapFst abs

data BindedUbo smsb where
	BindedUbo :: Vk.Bffr.Binded sb sm "uniform-buffer" '[ 'Atom 256 UniformBufferObject 'Nothing] ->
		BindedUbo '(sm, sb)

data MemoryUbo smsb where
	MemoryUbo :: Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
		sb,
		'Vk.Dvc.Mem.ImageBuffer.K.Buffer "uniform-buffer"
			'[ 'Atom 256 UniformBufferObject 'Nothing] )] ->
		MemoryUbo '(sm, sb)

createUniformBuffer1 :: Vk.PhDvc.P -> Vk.Dvc.D sd -> (forall sm sb .
		BindedUbo '(sm, sb) -> MemoryUbo '(sm, sb) -> IO b) -> IO b
createUniformBuffer1 phdvc dvc f = createBufferAtom phdvc dvc
		Vk.Bffr.UsageUniformBufferBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit ) \b m ->
	f (BindedUbo b) (MemoryUbo m)

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDescriptorPool dvc = Vk.DscPool.create @() dvc poolInfo nil nil
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = Nothing,
		Vk.DscPool.createInfoFlags = zeroBits,
		Vk.DscPool.createInfoMaxSets = maxFramesInFlight,
		Vk.DscPool.createInfoPoolSizes = [poolSize0, poolSize1] }
	poolSize0 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = maxFramesInFlight }
	poolSize1 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscPool.sizeDescriptorCount = maxFramesInFlight }

createDescriptorSets :: (HeteroParList.FromList ss, Update smsbs ss ssmp siv) =>
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> HeteroParList.PL BindedUbo smsbs ->
	HeteroParList.PL Vk.DscSet.Layout ss ->
	Vk.ImgVw.INew 'Vk.T.FormatR8g8b8a8Srgb "texture" siv -> Vk.Smplr.S ssmp ->
	IO (HeteroParList.PL (Vk.DscSet.S sd sp) ss)
createDescriptorSets dvc dscp ubs dscslyts tximgvw txsmp = do
	dscss <- Vk.DscSet.allocateSs @() dvc allocInfo
	update dvc ubs dscss tximgvw txsmp
	pure dscss
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = Nothing,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = dscslyts }

descriptorWrite0 ::
	Vk.Bffr.Binded sm sb nm '[ 'Atom 256 UniformBufferObject 'Nothing] ->
	Vk.DscSet.S sd sp slbts ->
	Vk.DscSet.Write () sd sp slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sb, sm, nm,
		'[ 'Atom 256 UniformBufferObject 'Nothing], 'Atom 256 UniformBufferObject 'Nothing )])
descriptorWrite0 ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = Nothing,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HeteroParList.Singleton bufferInfo
	}
	where bufferInfo = Vk.Dsc.BufferInfoAtom ub

descriptorWrite1 ::
	Vk.DscSet.S sd sp slbts -> Vk.ImgVw.INew fmt nm si -> Vk.Smplr.S ss ->
	Vk.DscSet.Write () sd sp slbts
		('Vk.DscSet.WriteSourcesArgImage '[ '(ss, fmt, nm, si) ])
descriptorWrite1 dscs tiv tsmp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = Nothing,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeCombinedImageSampler,
	Vk.DscSet.writeSources = Vk.DscSet.ImageInfos . HeteroParList.Singleton
		$ U4 Vk.Dsc.ImageInfo {
			Vk.Dsc.imageInfoImageLayout =
				Vk.Img.LayoutShaderReadOnlyOptimal,
			Vk.Dsc.imageInfoImageView = tiv,
			Vk.Dsc.imageInfoSampler = tsmp } }

class Update smsbs slbtss ssmp siv where
	update ::
		Vk.Dvc.D sd ->
		HeteroParList.PL BindedUbo smsbs ->
		HeteroParList.PL (Vk.DscSet.S sd sp) slbtss ->
		Vk.ImgVw.INew 'Vk.T.FormatR8g8b8a8Srgb "texture" siv ->
		Vk.Smplr.S ssmp ->
		IO ()

instance Update '[] '[] ssmp siv where update _ HeteroParList.Nil HeteroParList.Nil _ _ = pure ()

instance (
	Vk.DscSet.T.BindingAndArrayElem (Vk.DscSet.T.BindingTypesFromLayoutArg dscs) '[ 'Atom 256 UniformBufferObject 'Nothing],
	Update ubs dscss ssmp siv,
	Vk.DscSet.WriteSourcesToMiddle dscs
		('Vk.DscSet.WriteSourcesArgImage
			'[ '(ssmp, 'Vk.T.FormatR8g8b8a8Srgb, "texture", siv)])
	) =>
	Update (ub ': ubs) (dscs ': dscss) ssmp siv where
	update dvc (BindedUbo ub :** ubs) (dscs :** dscss) tximgvw txsmp = do
		Vk.DscSet.updateDs @() @() dvc (
			Vk.DscSet.Write_ (descriptorWrite0 ub dscs) :**
			Vk.DscSet.Write_ (descriptorWrite1 dscs tximgvw txsmp) :**
			HeteroParList.Nil ) []
		update dvc ubs dscss tximgvw txsmp

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

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a . Storable a =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[ 'List 256 a ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ 'List 256 a ""] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CmdBffr.allocateNew
		@() dvc allocInfo \(HeteroParList.Singleton (cb :: Vk.CmdBffr.C s '[])) -> do
		let	submitInfo :: Vk.SubmitInfo () '[] '[ '(s, '[])] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = Nothing,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
				Vk.submitInfoCommandBuffers = HeteroParList.Singleton $ U2 cb,
				Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
		Vk.CmdBffr.begin @() @() cb beginInfo do
			Vk.Cmd.copyBuffer @'[ '[ 'List 256 a ""]] cb src dst
		Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) Nothing
		Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfoNew () sc '[ '[]]
	allocInfo = Vk.CmdBffr.AllocateInfoNew {
		Vk.CmdBffr.allocateInfoNextNew = Nothing,
		Vk.CmdBffr.allocateInfoCommandPoolNew = cp,
		Vk.CmdBffr.allocateInfoLevelNew = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.M.BeginInfo {
		Vk.CmdBffr.beginInfoNext = Nothing,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }

createCommandBuffers ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPool.C scp ->
	(forall scb vss . VssList vss =>
		HeteroParList.PL (Vk.CmdBffr.C scb) (vss :: [[Type]]) -> IO a) ->
	IO a
createCommandBuffers dvc cp f = mkVss maxFramesInFlight \(_p :: Proxy vss1) ->
	Vk.CmdBffr.allocateNew @() @vss1 dvc (allocInfo @vss1) (f @_ @vss1)
	where
	allocInfo :: forall vss . Vk.CmdBffr.AllocateInfoNew () scp vss
	allocInfo = Vk.CmdBffr.AllocateInfoNew {
		Vk.CmdBffr.allocateInfoNextNew = Nothing,
		Vk.CmdBffr.allocateInfoCommandPoolNew = cp,
		Vk.CmdBffr.allocateInfoLevelNew = Vk.CmdBffr.LevelPrimary }

class VssList (vss :: [[Type]]) where
	vssListIndex ::
		HeteroParList.PL (Vk.CmdBffr.C scb) vss -> Int ->
		Vk.CmdBffr.C scb '[AddType Vertex 'Vk.VtxInp.RateVertex]

instance VssList '[] where
	vssListIndex HeteroParList.Nil _ = error "index too large"

instance VssList vss =>
	VssList ('[AddType Vertex 'Vk.VtxInp.RateVertex] ': vss) where
	vssListIndex (cb :** _) 0 = cb
	vssListIndex (_ :** cbs) n = vssListIndex cbs (n - 1)

mkVss :: Int -> (forall (vss :: [[Type]]) .
	(TpLvlLst.Length [Type] vss, HeteroParList.FromList vss, VssList vss) =>
	Proxy vss -> a) -> a
mkVss 0 f = f (Proxy @'[])
mkVss n f = mkVss (n - 1) \p -> f $ addTypeToProxy p

addTypeToProxy ::
	Proxy vss -> Proxy ('[AddType Vertex 'Vk.VtxInp.RateVertex] ': vss)
addTypeToProxy Proxy = Proxy

data SyncObjects (ssos :: ([Type], [Type], [Type])) where
	SyncObjects :: {
		imageAvailableSemaphores :: HeteroParList.PL Vk.Semaphore.S siass,
		renderFinishedSemaphores :: HeteroParList.PL Vk.Semaphore.S srfss,
		inFlightFences :: HeteroParList.PL Vk.Fence.F sfss } ->
		SyncObjects '(siass, srfss, sfss)

createSyncObjects ::
	Vk.Dvc.D sd -> (forall ssos . SyncObjects ssos -> IO a ) -> IO a
createSyncObjects dvc f =
	HeteroParList.replicateM maxFramesInFlight
		(Vk.Semaphore.create @() dvc def nil nil) \iass ->
	HeteroParList.replicateM maxFramesInFlight
		(Vk.Semaphore.create @() dvc def nil nil) \rfss ->
	HeteroParList.replicateM maxFramesInFlight
		(Vk.Fence.create @() dvc fncInfo nil nil) \iffs ->
	f $ SyncObjects iass rfss iffs
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sl sg sm sb nm sm' sb' nm' sdsc sp sdsl .
	Vk.CmdBffr.C scb '[AddType Vertex 'Vk.VtxInp.RateVertex] ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.C.Extent2d ->
	Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)] ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[ 'List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ 'List 256 Word32 ""] ->
	Vk.DscSet.S sdsc sp (AtomUbo sdsl) ->
	IO ()
recordCommandBuffer cb rp fb sce ppllyt gpl idcs vb ib ubds =
	Vk.CmdBffr.begin @() @() cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline do
	Vk.Cmd.bindPipeline cb Vk.Ppl.BindPointGraphics gpl
	Vk.Cmd.bindVertexBuffers cb
		. HeteroParList.Singleton . U4 $ Vk.Bffr.IndexedList @_ @_ @_ @Vertex vb
	Vk.Cmd.bindIndexBuffer cb $ Vk.Bffr.IndexedList @_ @_ @_ @Word32 ib
	Vk.Cmd.bindDescriptorSetsNew cb Vk.Ppl.BindPointGraphics ppllyt
		(HeteroParList.Singleton $ Vk.Cmd.DescriptorSet ubds) []
	Vk.Cmd.drawIndexed cb (fromIntegral $ V.length idcs) 1 0 0 0
	where
	rpInfo :: Vk.RndrPass.BeginInfo () sr sf '[
		'Vk.M.ClearTypeColor 'Vk.M.ClearColorTypeFloat32,
		'Vk.M.ClearTypeDepthStencil ]
	rpInfo = Vk.RndrPass.BeginInfo {
		Vk.RndrPass.beginInfoNext = Nothing,
		Vk.RndrPass.beginInfoRenderPass = rp,
		Vk.RndrPass.beginInfoFramebuffer = fb,
		Vk.RndrPass.beginInfoRenderArea = Vk.C.Rect2d {
			Vk.C.rect2dOffset = Vk.C.Offset2d 0 0,
			Vk.C.rect2dExtent = sce },
		Vk.RndrPass.beginInfoClearValues =
			Vk.M.ClearValueColor (fromJust $ rgbaDouble 0 0 0 1) :**
			Vk.M.ClearValueDepthStencil (Vk.C.ClearDepthStencilValue 1 0) :**
			HeteroParList.Nil }

mainLoop :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers ss sfs, VssList vss,
	DescriptorSetIndex slyts sdsc ) =>
	FramebufferResized ->
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc scfmt -> Vk.C.Extent2d ->
	HeteroParList.PL (Vk.ImgVw.INew scfmt nm) ss ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsc] '[] -> Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)] ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPool.C sc ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[ 'List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ 'List 256 Word32 ""] ->
	HeteroParList.PL (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	HeteroParList.PL BindedUbo smsbs ->
	HeteroParList.PL MemoryUbo smsbs ->
	HeteroParList.PL (Vk.DscSet.S sd sp) slyts ->
	UTCTime ->
	IO ()
mainLoop g w sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs cp drsrcs idcs vb ib cbs iasrfsifs ubs ums dscss tm0 = do
	($ cycle [0 .. maxFramesInFlight - 1]) . ($ ext0) $ fix \loop ext (cf : cfs) -> do
		Glfw.pollEvents
		tm <- getCurrentTime
		runLoop w sfc phdvc qfis dvc gq pq
			sc g ext scivs rp ppllyt gpl fbs cp drsrcs idcs vb ib cbs iasrfsifs ubs ums dscss
			(realToFrac $ tm `diffUTCTime` tm0)
			cf (`loop` cfs)
	Vk.Dvc.waitIdle dvc

runLoop :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs, VssList vss,
	DescriptorSetIndex slyts sdsc) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc scfmt -> FramebufferResized -> Vk.C.Extent2d ->
	HeteroParList.PL (Vk.ImgVw.INew scfmt nm) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsc] '[] ->
	Vk.Ppl.Graphics.G sg '[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)] ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPool.C sc ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[ 'List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ 'List 256 Word32 ""] ->
	HeteroParList.PL (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	HeteroParList.PL BindedUbo smsbs ->
	HeteroParList.PL MemoryUbo smsbs ->
	HeteroParList.PL (Vk.DscSet.S sd sp) slyts ->
	Float ->
	Int ->
	(Vk.C.Extent2d -> IO ()) -> IO ()
runLoop win sfc phdvc qfis dvc gq pq sc frszd ext
	scivs rp ppllyt gpl fbs cp drsrcs idcs vb ib cbs iasrfsifs
	ubs ums dscss tm cf loop = do
	catchAndRecreate win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp drsrcs loop
		$ drawFrame dvc gq pq sc ext rp ppllyt gpl fbs idcs vb ib cbs iasrfsifs ubs ums dscss tm cf
	cls <- Glfw.windowShouldClose win
	if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateSwapChainEtc
			win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp drsrcs)

drawFrame :: forall sfs sd ssc scfmt sr sl sdsc sg sm sb nm sm' sb' nm' scb ssos vss smsbs sdsc' sp slyts .
	(VssList vss, DescriptorSetIndex slyts sdsc) =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> Vk.Khr.Swapchain.SNew ssc scfmt ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.L sl '[AtomUbo sdsc] '[] ->
	Vk.Ppl.Graphics.G sg '[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)] ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[ 'List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ 'List 256 Word32 ""] ->
	HeteroParList.PL (Vk.CmdBffr.C scb) vss -> SyncObjects ssos ->
	HeteroParList.PL BindedUbo smsbs ->
	HeteroParList.PL MemoryUbo smsbs ->
	HeteroParList.PL (Vk.DscSet.S sdsc' sp) slyts ->
	Float ->
	Int -> IO ()
drawFrame dvc gq pq sc ext rp ppllyt gpl fbs idcs vb ib cbs
	(SyncObjects iass rfss iffs) ubs ums dscss tm cf =
	HeteroParList.index iass cf \(ias :: Vk.Semaphore.S sias) ->
	HeteroParList.index rfss cf \(rfs :: Vk.Semaphore.S srfs) ->
	HeteroParList.index iffs cf \(id &&& HeteroParList.Singleton -> (iff, siff)) ->
	HeteroParList.index ubs cf \ub -> HeteroParList.index ums cf \um ->
	descriptorSetIndex dscss cf \dscs -> do
	Vk.Fence.waitForFs dvc siff True maxBound
	imgIdx <- Vk.Khr.acquireNextImageResultNew [Vk.Success, Vk.SuboptimalKhr]
		dvc sc uint64Max (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HeteroParList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext ppllyt gpl idcs vb ib dscs
	updateUniformBuffer dvc um ext tm
	let	submitInfo :: Vk.SubmitInfo () '[sias]
			'[ '(scb, '[AddType Vertex 'Vk.VtxInp.RateVertex])]
			'[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers = HeteroParList.Singleton $ U2 cb,
			Vk.submitInfoSignalSemaphores = HeteroParList.Singleton rfs }
		presentInfo = Vk.Khr.PresentInfoNew {
			Vk.Khr.presentInfoNextNew = Nothing,
			Vk.Khr.presentInfoWaitSemaphoresNew = HeteroParList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndicesNew = HeteroParList.Singleton
				$ Vk.Khr.SwapchainImageIndexNew sc imgIdx }
	Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresentNew @() pq presentInfo
	where	cb = cbs `vssListIndex` cf

class DescriptorSetIndex aus s where
	descriptorSetIndex :: HeteroParList.PL (Vk.DscSet.S sdsc sp) aus -> Int ->
		(Vk.DscSet.S sdsc sp (AtomUbo s) -> a) -> a

instance DescriptorSetIndex '[] s where
	descriptorSetIndex _ _ f = f $ error "index too large"

instance DescriptorSetIndex aus s =>
	DescriptorSetIndex (AtomUbo s ': aus) s where
	descriptorSetIndex (ds :** _) 0 f = f ds
	descriptorSetIndex (_ :** dss) n f = descriptorSetIndex dss (n - 1) f

updateUniformBuffer :: Vk.Dvc.D sd -> MemoryUbo sm -> Vk.C.Extent2d -> Float -> IO ()
updateUniformBuffer dvc (MemoryUbo um) sce tm =
	Vk.Dvc.Mem.ImageBuffer.write @"uniform-buffer" @('Atom 256 UniformBufferObject 'Nothing) dvc um zeroBits ubo
	where ubo = UniformBufferObject {
		uniformBufferObjectModel = Cglm.glmRotate
			Cglm.glmMat4Identity
			(tm * Cglm.glmRad 90)
			(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL),
		uniformBufferObjectView = Cglm.glmLookat
			(Cglm.Vec3 $ 2 :. 2 :. 2 :. NilL)
			(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
			(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL),
		uniformBufferObjectProj = Cglm.modifyMat4 1 1 negate
			$ Cglm.glmPerspective
				(Cglm.glmRad 45)
				(fromIntegral (Vk.C.extent2dWidth sce) /
					fromIntegral (Vk.C.extent2dHeight sce))
				0.1 10 }

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs ) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc scfmt ->
	HeteroParList.PL (Vk.ImgVw.INew scfmt nm) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsc] '[] ->
	Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)] ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPool.C sc ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	(Vk.C.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp drsrcs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateSwapChainEtc
		win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp drsrcs

recreateSwapChainEtc :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs ) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc scfmt ->
	HeteroParList.PL (Vk.ImgVw.INew scfmt nm) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsc] '[] ->
	Vk.Ppl.Graphics.G sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)] ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPool.C sc ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	IO Vk.C.Extent2d
recreateSwapChainEtc win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp (dptImg, dptImgMem, dptImgVw) = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwapChain win sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swapchain.getImagesNew dvc sc >>= \imgs ->
			recreateImageViews dvc imgs scivs
		recreateDepthResources phdvc dvc gq cp ext dptImg dptImgMem dptImgVw
		recreateGraphicsPipeline' dvc ext rp ppllyt gpl
		recreateFramebuffers dvc ext rp scivs dptImgVw fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize win = Glfw.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

data UniformBufferObject = UniformBufferObject {
	uniformBufferObjectModel :: Cglm.Mat4,
	uniformBufferObjectView :: Cglm.Mat4,
	uniformBufferObjectProj :: Cglm.Mat4 }
	deriving (Show, Generic)

instance Storable UniformBufferObject where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance SizeAlignmentList UniformBufferObject
instance Foreign.Storable.Generic.G UniformBufferObject

vertShaderModule :: Vk.Shader.Module.M n 'GlslVertexShader () ()
vertShaderModule = mkShaderModule glslVertexShaderMain

fragShaderModule :: Vk.Shader.Module.M n 'GlslFragmentShader () ()
fragShaderModule = mkShaderModule glslFragmentShaderMain

mkShaderModule :: Spv sknd -> Vk.Shader.Module.M n sknd () ()
mkShaderModule code = Vk.Shader.Module.M createInfo nil nil
	where createInfo = Vk.Shader.Module.M.CreateInfo {
		Vk.Shader.Module.M.createInfoNext = Nothing,
		Vk.Shader.Module.M.createInfoFlags = def,
		Vk.Shader.Module.M.createInfoCode = code }

[glslVertexShader|

#version 450

layout(binding = 0) uniform UniformBufferObject {
	mat4 model;
	mat4 view;
	mat4 proj;
	} ubo;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inColor;
layout(location = 2) in vec2 inTexCoord;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void
main()
{
	gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 1.0);
	fragColor = inColor;
	fragTexCoord = inTexCoord;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragTexCoord;

layout(location = 0) out vec4 outColor;

layout(binding = 1) uniform sampler2D texSampler;

void
main()
{
//	outColor = texture(texSampler, fragTexCoord * 2.0);
	outColor = vec4(fragColor * texture(texSampler, fragTexCoord).rgb, 1.0);
}

|]
