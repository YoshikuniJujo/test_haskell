{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Generics
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.Storable.HeteroList
import Foreign.Storable.SizeAlignment
import Foreign.Marshal.Array
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Data.Kind.Object qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Data.Foldable
import Data.Default
import Data.Bits
import Data.Array hiding (indices)
import Data.TypeLevel.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Proxy
import Data.Bool
import Data.Maybe
import Data.List
import Data.IORef
import Data.List.Length
import Data.Word
import Data.Int
import Data.Color
import Data.Time
import System.Environment
import Codec.Picture
import Codec.Picture.Tools

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
import qualified Gpu.Vulkan.Enum as Vk
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
import qualified Gpu.Vulkan.PhysicalDevice.Struct as Vk.PhDvc
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
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Layout
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
import qualified Gpu.Vulkan.Memory as Vk.Dvc.Mem.ImageBuffer
import qualified Gpu.Vulkan.Memory.Kind as Vk.Dvc.Mem.ImageBuffer.K
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.Command as Vk.Cmd

import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.DescriptorSet.TypeLevel.Write as Vk.DscSet

import qualified Gpu.Vulkan.Sampler as Vk.Smplr
import qualified Gpu.Vulkan.Sampler.Enum as Vk.Smplr
import qualified Gpu.Vulkan.Sampler.Middle as Vk.Smplr.M
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as Vk.Ppl.DptStnSt

import Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList(AddType)

import Tools
import Vertex
import Codec.Wavefront.Read
import Data.Vector.Storable.Indexing

main :: IO ()
main = do
	txfp : mdlfp : mnld : _ <- getArgs
	g <- newFramebufferResized
	(`withWindow` g) \win -> createInstance \inst -> do
		if enableValidationLayers
			then setupDebugMessenger inst
				$ const $ run txfp mdlfp (read mnld) win inst g
			else run txfp mdlfp (read mnld) win inst g

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

run :: FilePath -> FilePath -> Float -> Glfw.Window -> Vk.Ist.I si -> FramebufferResized -> IO ()
run txfp mdlfp mnld w inst g =
	createSurface w inst \sfc ->
	pickPhysicalDevice inst sfc >>= \(phdv, qfis, spcnt) ->
	createLogicalDevice phdv qfis \dv gq pq ->
	createSwapChain w sfc phdv qfis dv
		\(sc :: Vk.Khr.Swapchain.SNew ss scifmt)  scifmt ext ->
	Vk.Khr.Swapchain.getImagesNew dv sc >>= \imgs ->
	createImageViews dv imgs \scivs ->
	findDepthFormat phdv >>= \dptfmt ->
	Vk.T.formatToType dptfmt \(_ :: Proxy dptfmt) ->
	createColorResources @scifmt phdv dv ext spcnt \clrimg clrimgm clrimgvw ->
	createRenderPass @scifmt @dptfmt dv spcnt \rp ->
	createPipelineLayout' dv \dscslyt ppllyt ->
	createGraphicsPipeline' dv ext rp ppllyt spcnt \gpl ->
	createCommandPool qfis dv \cp ->
	createDepthResources phdv dv gq cp ext spcnt \dptImg dptImgMem dptImgVw ->
	createFramebuffers dv ext rp scivs dptImgVw clrimgvw \fbs ->
	createTextureImage phdv dv gq cp txfp \tximg mplvs ->
	createImageView @'Vk.T.FormatR8g8b8a8Srgb dv tximg Vk.Img.AspectColorBit mplvs \tximgvw ->
	createTextureSampler phdv dv mplvs mnld \txsmplr ->
	loadModel mdlfp >>= \(vtcs, idcs) ->
	createVertexBuffer phdv dv gq cp vtcs \vb ->
	createIndexBuffer phdv dv gq cp idcs \ib ->
	createUniformBuffer phdv dv \ub ubm ->
	createDescriptorPool dv \dscp ->
	createDescriptorSet dv dscp ub tximgvw txsmplr dscslyt >>= \ubds ->
	createCommandBuffer dv cp \cb ->
	createSyncObjects dv \sos ->
	getCurrentTime >>= \tm ->
	mainLoop g w sfc phdv qfis dv gq pq sc ext scivs rp ppllyt gpl fbs cp
		(clrimg, clrimgm, clrimgvw, spcnt)
		dptImg dptImgMem dptImgVw idcs vb ib ubm ubds cb sos tm

createSurface :: Glfw.Window -> Vk.Ist.I si ->
	(forall ss . Vk.Khr.Surface.S ss -> IO a) -> IO a
createSurface win ist f = Glfw.createWindowSurface ist win nil' \sfc -> f sfc

pickPhysicalDevice :: Vk.Ist.I si ->
	Vk.Khr.Surface.S ss -> IO (Vk.PhDvc.P, QueueFamilyIndices, Vk.Sample.CountFlags)
pickPhysicalDevice ist sfc = do
	putStrLn "PICK PHYSICAL DEVICE"
	dvcs <- Vk.PhDvc.enumerate ist
	when (null dvcs) $ error "failed to find GPUs with Gpu.Vulkan support!"
	findDevice (`isDeviceSuitable` sfc) dvcs >>= \case
		Just (p, q) -> do
			sc <- getMaxUsableSampleCount p
			print sc
			pure (p, q, sc)
		Nothing -> error "failed to find a suitable GPU!"

getMaxUsableSampleCount :: Vk.PhDvc.P -> IO Vk.Sample.CountFlags
getMaxUsableSampleCount phdvc = do
	cnts <- Vk.PhDvc.limitsFramebufferDepthSampleCounts . Vk.PhDvc.propertiesLimits
		<$> Vk.PhDvc.getProperties phdvc
	print cnts
	pure . fromMaybe Vk.Sample.Count1Bit $ findBit [
		Vk.Sample.Count64Bit, Vk.Sample.Count32Bit,
		Vk.Sample.Count16Bit, Vk.Sample.Count8Bit,
		Vk.Sample.Count4Bit, Vk.Sample.Count2Bit ] cnts

findBit :: Bits b => [b] -> b -> Maybe b
findBit bl bs = find ((/= zeroBits) . (.&. bs)) bl

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
	null . (deviceExtensions \\) . (Vk.extensionPropertiesExtensionName <$>)
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
	mkHeteroParList queueCreateInfos uniqueQueueFamilies \qs ->
	let	createInfo = Vk.Dvc.M.CreateInfo {
			Vk.Dvc.M.createInfoNext = TMaybe.N,
			Vk.Dvc.M.createInfoFlags = def,
			Vk.Dvc.M.createInfoQueueCreateInfos = qs,
			Vk.Dvc.M.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Dvc.M.createInfoEnabledExtensionNames =
				deviceExtensions,
			Vk.Dvc.M.createInfoEnabledFeatures = Just def {
				Vk.PhDvc.featuresSamplerAnisotropy = True,
				Vk.PhDvc.featuresSampleRateShading = True } } in
	Vk.Dvc.create phdvc createInfo nil' \dvc -> do
		gq <- Vk.Dvc.getQueue dvc (graphicsFamily qfis) 0
		pq <- Vk.Dvc.getQueue dvc (presentFamily qfis) 0
		f dvc gq pq
	where
	uniqueQueueFamilies = nub [graphicsFamily qfis, presentFamily qfis]
	queueCreateInfos qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }

mkHeteroParList :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] ->
	(forall ss . WithPokedHeteroToListM' TMaybe.M ss => HeteroParList.PL t ss -> b) -> b
mkHeteroParList _k [] f = f HeteroParList.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

createSwapChain :: Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> (forall ss fmt . Vk.T.FormatToValue fmt =>
		Vk.Khr.Swapchain.SNew ss fmt -> Vk.Format -> Vk.Extent2d -> IO a) ->
	IO a
createSwapChain win sfc phdvc qfis dvc f = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	fmt = Vk.Khr.Surface.M.formatFormat . chooseSwapSurfaceFormat $ formats spp
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		let	(crInfo, scifmt) = mkSwapchainCreateInfo sfc qfis spp ext
		Vk.Khr.Swapchain.createNew @'Nothing @fmt dvc crInfo nil' \sc -> f sc scifmt ext

recreateSwapChain :: forall ssfc sd ssc scfmt .
	Vk.T.FormatToValue scfmt =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Khr.Swapchain.SNew ssc scfmt ->
	IO Vk.Extent2d
recreateSwapChain win sfc phdvc qfis0 dvc sc = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	(crInfo, scifmt) = mkSwapchainCreateInfo sfc qfis0 spp ext
	ext <$ Vk.Khr.Swapchain.recreateNew @'Nothing @scfmt dvc crInfo nil' sc

mkSwapchainCreateInfo :: Vk.Khr.Surface.S ss -> QueueFamilyIndices ->
	SwapChainSupportDetails -> Vk.Extent2d ->
	(Vk.Khr.Swapchain.CreateInfoNew 'Nothing ss fmt, Vk.Format)
mkSwapchainCreateInfo sfc qfis0 spp ext = (
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

createImageViews :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Img.BindedNew ss ss nm fmt] ->
	(forall si . HeteroParList.PL (Vk.ImgVw.INew fmt nm) si -> IO a) -> IO a
createImageViews _dvc [] f = f HeteroParList.Nil
createImageViews dvc (sci : scis) f =
	createImageView dvc sci Vk.Img.AspectColorBit 1 \sciv ->
	createImageViews dvc scis \scivs -> f $ sciv :** scivs

recreateImageViews :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Img.BindedNew ss ss nm scfmt] -> HeteroParList.PL (Vk.ImgVw.INew scfmt nm) sis -> IO ()
recreateImageViews _dvc [] HeteroParList.Nil = pure ()
recreateImageViews dvc (sci : scis) (iv :** ivs) =
	Vk.ImgVw.recreateNew dvc (mkImageViewCreateInfo sci Vk.Img.AspectColorBit 1) nil' iv >>
	recreateImageViews dvc scis ivs
recreateImageViews _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

createImageView :: forall ivfmt sd si sm nm ifmt a .
	Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.BindedNew si sm nm ifmt ->
	Vk.Img.AspectFlags -> Word32 ->
	(forall siv . Vk.ImgVw.INew ivfmt nm siv -> IO a) -> IO a
createImageView dvc timg asps mplvs f =
	Vk.ImgVw.createNew dvc (mkImageViewCreateInfo timg asps mplvs) nil' f

recreateImageView :: Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.BindedNew si sm nm ifmt ->
	Vk.Img.AspectFlags ->
	Vk.ImgVw.INew ivfmt nm s -> Word32 -> IO ()
recreateImageView dvc timg asps iv mplvs =
	Vk.ImgVw.recreateNew dvc (mkImageViewCreateInfo timg asps mplvs) nil' iv

mkImageViewCreateInfo ::
	Vk.Img.BindedNew si sm nm ifmt -> Vk.Img.AspectFlags -> Word32 ->
	Vk.ImgVw.CreateInfoNew 'Nothing si sm nm ifmt ivfmt
mkImageViewCreateInfo sci asps mplvs = Vk.ImgVw.CreateInfoNew {
	Vk.ImgVw.createInfoNextNew = TMaybe.N,
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
		Vk.Img.M.subresourceRangeLevelCount = mplvs,
		Vk.Img.M.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.M.subresourceRangeLayerCount = 1 }

createRenderPass ::
	forall (scifmt :: Vk.T.Format) (dptfmt :: Vk.T.Format) sd a . (
	Vk.T.FormatToValue scifmt, Vk.T.FormatToValue dptfmt ) =>
	Vk.Dvc.D sd -> Vk.Sample.CountFlags ->
	(forall sr . Vk.RndrPass.R sr -> IO a) -> IO a
createRenderPass dvc mss f = do
	let	colorAttachment :: Vk.Att.Description scifmt
		colorAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = zeroBits,
			Vk.Att.descriptionSamples = mss,
			Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
			Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
			Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOp =
				Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayout =
				Vk.Img.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
--				Vk.Img.LayoutPresentSrcKhr }
		colorAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 0,
			Vk.Att.referenceLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
		depthAttachment :: Vk.Att.Description dptfmt
		depthAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = zeroBits,
			Vk.Att.descriptionSamples = mss,
			Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
			Vk.Att.descriptionStoreOp = Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionStencilLoadOp =
				Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOp =
				Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayout =
				Vk.Img.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Img.LayoutDepthStencilAttachmentOptimal }
		depthAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 1,
			Vk.Att.referenceLayout =
				Vk.Img.LayoutDepthStencilAttachmentOptimal }
		colorAttachmentResolve = Vk.Att.Description {
			Vk.Att.descriptionFlags = zeroBits,
			Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
			Vk.Att.descriptionLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
			Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOp =
				Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayout =
				Vk.Img.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Img.LayoutPresentSrcKhr }
		colorAttachmentResolveRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 2,
			Vk.Att.referenceLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
		subpass = Vk.Subpass.Description {
			Vk.Subpass.descriptionFlags = zeroBits,
			Vk.Subpass.descriptionPipelineBindPoint =
				Vk.Ppl.BindPointGraphics,
			Vk.Subpass.descriptionInputAttachments = [],
			Vk.Subpass.descriptionColorAndResolveAttachments =
				Right [(colorAttachmentRef, colorAttachmentResolveRef)],
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
			Vk.RndrPass.M.createInfoNextNew = TMaybe.N,
			Vk.RndrPass.M.createInfoFlagsNew = zeroBits,
			Vk.RndrPass.M.createInfoAttachmentsNew =
				colorAttachment :** depthAttachment :**
				colorAttachmentResolve :** HeteroParList.Nil,
			Vk.RndrPass.M.createInfoSubpassesNew = [subpass],
			Vk.RndrPass.M.createInfoDependenciesNew = [dependency] }
	Vk.RndrPass.createNew @'[scifmt, dptfmt, scifmt] @'Nothing dvc renderPassInfo nil' \rp -> f rp

type AtomUbo s = '(s, '[
	'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing],
	'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ])

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall s .
	Vk.DscSetLyt.L s '[
		'Vk.DscSetLyt.Buffer '[ VObj.Atom 256 UniformBufferObject 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ]
	-> IO a) -> IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil'
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
		'Vk.DscSetLyt.Buffer '[ VObj.Atom 256 UniformBufferObject 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings =
			uboLayoutBinding :**
			samplerLayoutBinding :**
			HeteroParList.Nil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[ VObj.Atom 256 UniformBufferObject 'Nothing])
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
			'Vk.DscSetLyt.Buffer '[ VObj.Atom 256 UniformBufferObject 'Nothing],
			'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ] ->
		Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] -> IO b) -> IO b
createPipelineLayout' dvc f =
	createDescriptorSetLayout dvc \dsl ->
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfoNew {
			Vk.Ppl.Layout.createInfoNextNew = TMaybe.N,
			Vk.Ppl.Layout.createInfoFlagsNew = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayoutsNew =
				HeteroParList.Singleton $ U2 dsl } in
	Vk.Ppl.Layout.createNew @_ @_ @'[] @'Nothing dvc pipelineLayoutInfo nil' $ f dsl

createGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	Vk.Sample.CountFlags ->
	(forall sg . Vk.Ppl.Graphics.GNew sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl], '[]) -> IO a) -> IO a
createGraphicsPipeline' dvc sce rp ppllyt mss f =
	Vk.Ppl.Graphics.createGs dvc Nothing (U14 pplInfo :** HeteroParList.Nil)
			nil' \(U3 gpl :** HeteroParList.Nil) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt mss

recreateGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	Vk.Sample.CountFlags ->
	Vk.Ppl.Graphics.GNew sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl], '[]) -> IO ()
recreateGraphicsPipeline' dvc sce rp ppllyt mss gpls = Vk.Ppl.Graphics.recreateGs
	dvc Nothing (U14 pplInfo :** HeteroParList.Nil) nil' (U3 gpls :** HeteroParList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt mss

mkGraphicsPipelineCreateInfo' ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	Vk.Sample.CountFlags ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing, '[AddType Vertex 'Vk.VtxInp.RateVertex],
			'[ '(0, Pos), '(1, Color), '(2, TexCoord)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'Nothing '(sl, '[AtomUbo sdsl], '[]) sr '(sb, vs', ts', slbtss')
mkGraphicsPipelineCreateInfo' sce rp ppllyt mss = Vk.Ppl.Graphics.CreateInfo {
	Vk.Ppl.Graphics.createInfoNext = TMaybe.N,
	Vk.Ppl.Graphics.createInfoFlags = Vk.Ppl.CreateFlagsZero,
	Vk.Ppl.Graphics.createInfoStages = shaderStages,
	Vk.Ppl.Graphics.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Graphics.createInfoInputAssemblyState = Just inputAssembly,
	Vk.Ppl.Graphics.createInfoViewportState = Just $ mkViewportState sce,
	Vk.Ppl.Graphics.createInfoRasterizationState = Just rasterizer,
	Vk.Ppl.Graphics.createInfoMultisampleState = Just $ multisampling mss,
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
		Vk.Ppl.DptStnSt.createInfoNext = TMaybe.N,
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
	Vk.Ppl.RstSt.createInfoFrontFace = Vk.FrontFaceCounterClockwise,
	Vk.Ppl.RstSt.createInfoDepthBiasEnable = False,
	Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0 }

multisampling :: Vk.Sample.CountFlags -> Vk.Ppl.MltSmplSt.CreateInfo 'Nothing
multisampling mss = Vk.Ppl.MltSmplSt.CreateInfo {
	Vk.Ppl.MltSmplSt.createInfoNext = TMaybe.N,
	Vk.Ppl.MltSmplSt.createInfoFlags = zeroBits,
	Vk.Ppl.MltSmplSt.createInfoSampleShadingEnable = True,
	Vk.Ppl.MltSmplSt.createInfoRasterizationSamplesAndMask =
		Vk.Sample.CountAndMask mss Nothing,
	Vk.Ppl.MltSmplSt.createInfoMinSampleShading = 0.2,
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
	Vk.ImgVw.INew dptfmt dptnm siv ->
	Vk.ImgVw.INew clrfmt clrnm clrsiv ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HeteroParList.Nil _ _ f = f HeteroParList.Nil
createFramebuffers dvc sce rp (iv :** ivs) divw clrvw f =
	Vk.Frmbffr.createNew dvc (mkFramebufferCreateInfo sce rp iv divw clrvw) nil' \fb ->
	createFramebuffers dvc sce rp ivs divw clrvw \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.INew scfmt nm) sis ->
		Vk.ImgVw.INew dptfmt dptnm sdiv ->
		Vk.ImgVw.INew clrfmt clrnm clrsdiv ->
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HeteroParList.Nil _ _ HeteroParList.Nil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dvc sce rp (sciv :** scivs) divw clrvw (fb :** fbs) =
		Vk.Frmbffr.recreateNew dvc
			(mkFramebufferCreateInfo sce rp sciv divw clrvw) nil' fb >>
		recreateFramebuffers dvc sce rp scivs divw clrvw fbs

mkFramebufferCreateInfo ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.ImgVw.INew fmt nm si ->
	Vk.ImgVw.INew dptfmt dptnm sdiv -> Vk.ImgVw.INew clrfmt clrnm clrsdiv ->
	Vk.Frmbffr.CreateInfoNew 'Nothing sr
		'[ '(clrfmt, clrnm, clrsdiv), '(dptfmt, dptnm, sdiv), '(fmt, nm, si)]
--	Vk.Frmbffr.CreateInfoNew () sr fmt nm '[si, sdiv]
mkFramebufferCreateInfo sce rp attch dpt clr = Vk.Frmbffr.CreateInfoNew {
	Vk.Frmbffr.createInfoNextNew = TMaybe.N,
	Vk.Frmbffr.createInfoFlagsNew = zeroBits,
	Vk.Frmbffr.createInfoRenderPassNew = rp,
	Vk.Frmbffr.createInfoAttachmentsNew = U3 clr :** U3 dpt :** U3 attch :** HeteroParList.Nil,
--	Vk.Frmbffr.createInfoAttachmentsNew = attch :** dpt :** HeteroParList.Nil,
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

type ColorResources fmt nm si sm siv = (
	Vk.Img.BindedNew si sm nm fmt,
	Vk.Dvc.Mem.ImageBuffer.M sm
		'[ '(si, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt)],
	Vk.ImgVw.INew fmt nm siv,
	Vk.Sample.CountFlags )

createColorResources :: forall fmt sd nm a . Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Extent2d -> Vk.Sample.CountFlags ->
	(forall si sm siv .
		Vk.Img.BindedNew si sm nm fmt ->
		Vk.Dvc.Mem.ImageBuffer.M sm
			'[ '(si, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt)] ->
		Vk.ImgVw.INew fmt nm siv ->
		IO a) -> IO a
createColorResources phdvc dvc ext msmpls f =
	createImage @_ @fmt phdvc dvc
		(Vk.extent2dWidth ext) (Vk.extent2dHeight ext) 1 msmpls
		Vk.Img.TilingOptimal
		(	Vk.Img.UsageTransientAttachmentBit .|.
			Vk.Img.UsageColorAttachmentBit )
		Vk.Mem.PropertyDeviceLocalBit \img imgmem ->
	createImageView @fmt dvc img Vk.Img.AspectColorBit 1 \imgvw ->
	f img imgmem imgvw

recreateColorResources :: forall fmt sd nm si sm siv . Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Extent2d -> Vk.Sample.CountFlags ->
	Vk.Img.BindedNew si sm nm fmt ->
	Vk.Dvc.Mem.ImageBuffer.M sm
		'[ '(si, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt)] ->
	Vk.ImgVw.INew fmt nm siv -> IO ()
recreateColorResources phdvc dvc ext msmpls img imgmem imgvw = do
	recreateImage phdvc dvc
		(Vk.extent2dWidth ext) (Vk.extent2dHeight ext) 1 msmpls
		Vk.Img.TilingOptimal
		(	Vk.Img.UsageTransientAttachmentBit .|.
			Vk.Img.UsageColorAttachmentBit )
		Vk.Mem.PropertyDeviceLocalBit img imgmem
	recreateImageView dvc img Vk.Img.AspectColorBit imgvw 1

createDepthResources ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Extent2d -> Vk.Sample.CountFlags ->
	(forall si sm fmt siv . Vk.T.FormatToValue fmt =>
		Vk.Img.BindedNew si sm nm fmt ->
		Vk.Dvc.Mem.ImageBuffer.M sm
			'[ '(si, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt) ] ->
		Vk.ImgVw.INew fmt nm siv ->
		IO a) -> IO a
createDepthResources phdvc dvc gq cp ext mss f = do
	fmt <- findDepthFormat phdvc
	print fmt
	print ext
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		createImage @_ @fmt phdvc dvc
			(Vk.extent2dWidth ext) (Vk.extent2dHeight ext) 1 mss
			Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
			Vk.Mem.PropertyDeviceLocalBit \dptImg dptImgMem ->
			createImageView @fmt
				dvc dptImg Vk.Img.AspectDepthBit 1 \dptImgVw -> do
			transitionImageLayout dvc gq cp dptImg Vk.Img.LayoutUndefined
				Vk.Img.LayoutDepthStencilAttachmentOptimal 1
			f dptImg dptImgMem dptImgVw

recreateDepthResources :: Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Extent2d ->
	Vk.Sample.CountFlags ->
	Vk.Img.BindedNew sb sm nm fmt ->
	Vk.Dvc.Mem.ImageBuffer.M
		sm '[ '(sb, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt)] ->
	Vk.ImgVw.INew fmt nm sdiv -> IO ()
recreateDepthResources phdvc dvc gq cp ext mss dptImg dptImgMem dptImgVw = do
	print ext
	recreateImage phdvc dvc
		(Vk.extent2dWidth ext) (Vk.extent2dHeight ext) 1 mss
		Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mem.PropertyDeviceLocalBit dptImg dptImgMem
	recreateImageView dvc dptImg Vk.Img.AspectDepthBit dptImgVw 1
	transitionImageLayout dvc gq cp dptImg Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal 1

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
		Vk.Img.BindedNew si sm nm 'Vk.T.FormatR8g8b8a8Srgb -> Word32 -> IO a ) ->
	IO a
createTextureImage phdvc dvc gq cp fp f = do
	img <- readRgba8 fp
	putStrLn "CREATE TEXTURE"
	print . V.length $ imageData img
	let	wdt_ = imageWidth img
		hgt_ = imageHeight img
		wdt = fromIntegral wdt_
		hgt = fromIntegral hgt_
		mipLevels :: Word32 = floor @Double . logBase 2
			$ max (fromIntegral wdt_) (fromIntegral hgt_)
	print (mipLevels :: Word32)
	createImage @_ @'Vk.T.FormatR8g8b8a8Srgb phdvc dvc wdt hgt mipLevels
		Vk.Sample.Count1Bit Vk.Img.TilingOptimal
		(	Vk.Img.UsageTransferSrcBit .|.
			Vk.Img.UsageTransferDstBit .|.
			Vk.Img.UsageSampledBit)
		Vk.Mem.PropertyDeviceLocalBit \tximg _txmem -> do
		createBufferImage @MyImage @_ phdvc dvc
			(fromIntegral wdt, fromIntegral wdt, fromIntegral hgt, 1)
			Vk.Bffr.UsageTransferSrcBit
			(	Vk.Mem.PropertyHostVisibleBit .|.
				Vk.Mem.PropertyHostCoherentBit )
			\(sb :: Vk.Bffr.Binded
				sm sb "texture-buffer" '[ VObj.ObjImage 1 a inm]) sbm -> do
			Vk.Dvc.Mem.ImageBuffer.write @"texture-buffer"
				@(VObj.ObjImage 1 MyImage inm) dvc sbm zeroBits (MyImage img)
			print sb
			transitionImageLayout dvc gq cp tximg
				Vk.Img.LayoutUndefined
				Vk.Img.LayoutTransferDstOptimal mipLevels
			copyBufferToImage dvc gq cp sb tximg wdt hgt
			generateMipmaps phdvc dvc gq cp tximg mipLevels (fromIntegral wdt_) (fromIntegral hgt_)
			f tximg mipLevels

checkImageFilterLinearBit ::
	forall fmt . Vk.T.FormatToValue fmt => Vk.PhDvc.P -> IO ()
checkImageFilterLinearBit phdvc = do
	let	fmt = Vk.T.formatToValue @fmt
	formatProperties <- Vk.PhDvc.getFormatProperties phdvc fmt
	let	bt = Vk.formatPropertiesOptimalTilingFeatures
				formatProperties .&.
			Vk.FormatFeatureSampledImageFilterLinearBit
	when (bt == zeroBits) $ error
		"texture image format does not support linear blitting!"

generateMipmaps :: forall sd scp si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C scp ->
	Vk.Img.BindedNew si sm nm fmt -> Word32 -> Int32 -> Int32 -> IO ()
generateMipmaps phdvc dvc gq cp img mlvs wdt hgt = do
	putStrLn "GENERATE MIPMAPS"
	checkImageFilterLinearBit @fmt phdvc
	beginSingleTimeCommands dvc gq cp \cb -> do
		for_ iwhs \(i, (w, h)) -> generateMipmap1 cb img i w h
		Vk.Cmd.pipelineBarrier cb
			Vk.Ppl.StageTransferBit Vk.Ppl.StageFragmentShaderBit zeroBits
			HeteroParList.Nil HeteroParList.Nil (HeteroParList.Singleton $ U5 barrier)
	where
	iwhs = [1 .. mlvs - 1] `zip` (halves wdt `zip` halves hgt)
	barrier :: Vk.Img.MemoryBarrier 'Nothing si sm nm fmt
	barrier = mipmapBarrier
		Vk.AccessTransferWriteBit Vk.AccessShaderReadBit
		Vk.Img.LayoutTransferDstOptimal
		Vk.Img.LayoutShaderReadOnlyOptimal img mlvs

mipmapBarrier :: Vk.AccessFlags -> Vk.AccessFlags ->
	Vk.Img.Layout -> Vk.Img.Layout -> Vk.Img.BindedNew si sm nm fmt ->
	Word32 -> Vk.Img.MemoryBarrier 'Nothing si sm nm fmt
mipmapBarrier sam dam olyt nlyt img i = Vk.Img.MemoryBarrier {
	Vk.Img.memoryBarrierNext = TMaybe.N,
	Vk.Img.memoryBarrierSrcAccessMask = sam,
	Vk.Img.memoryBarrierDstAccessMask = dam,
	Vk.Img.memoryBarrierOldLayout = olyt,
	Vk.Img.memoryBarrierNewLayout = nlyt,
	Vk.Img.memoryBarrierSrcQueueFamilyIndex = Vk.QueueFamily.Ignored,
	Vk.Img.memoryBarrierDstQueueFamilyIndex = Vk.QueueFamily.Ignored,
	Vk.Img.memoryBarrierImage = img,
	Vk.Img.memoryBarrierSubresourceRange = srr }
	where
	srr = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = i - 1,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 }

generateMipmap1 :: forall scb si sm nm fmt . Vk.CmdBffr.C scb ->
	Vk.Img.BindedNew si sm nm fmt -> Word32 -> Int32 -> Int32 -> IO ()
generateMipmap1 cb img i w h = do
	Vk.Cmd.pipelineBarrier cb
		Vk.Ppl.StageTransferBit Vk.Ppl.StageTransferBit zeroBits
		HeteroParList.Nil HeteroParList.Nil . HeteroParList.Singleton $ U5 barrier'
	Vk.Cmd.blitImageNew cb
		img Vk.Img.LayoutTransferSrcOptimal
		img Vk.Img.LayoutTransferDstOptimal
		[blit] Vk.FilterLinear
	Vk.Cmd.pipelineBarrier cb
		Vk.Ppl.StageTransferBit Vk.Ppl.StageFragmentShaderBit zeroBits
		HeteroParList.Nil HeteroParList.Nil . HeteroParList.Singleton $ U5 barrier''
	where
	barrier' :: Vk.Img.MemoryBarrier 'Nothing si sm nm fmt
	barrier' = mipmapBarrier Vk.AccessTransferWriteBit Vk.AccessTransferReadBit
		Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutTransferSrcOptimal
		img i
	barrier'' :: Vk.Img.MemoryBarrier 'Nothing si sm nm fmt
	barrier'' = mipmapBarrier Vk.AccessTransferReadBit Vk.AccessShaderReadBit
		Vk.Img.LayoutTransferSrcOptimal Vk.Img.LayoutShaderReadOnlyOptimal
		img i
	blit = Vk.Img.M.Blit {
		Vk.Img.M.blitSrcSubresource = bssr,
		Vk.Img.M.blitSrcOffsetFrom = Vk.Offset3d 0 0 0,
		Vk.Img.M.blitSrcOffsetTo = Vk.Offset3d w h 1,
		Vk.Img.M.blitDstSubresource = bdsr,
		Vk.Img.M.blitDstOffsetFrom = Vk.Offset3d 0 0 0,
		Vk.Img.M.blitDstOffsetTo = Vk.Offset3d (half w) (half h) 1 }
	bssr = Vk.Img.M.SubresourceLayers {
		Vk.Img.M.subresourceLayersAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.M.subresourceLayersMipLevel = i - 1,
		Vk.Img.M.subresourceLayersBaseArrayLayer = 0,
		Vk.Img.M.subresourceLayersLayerCount = 1 }
	bdsr = Vk.Img.M.SubresourceLayers {
		Vk.Img.M.subresourceLayersAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.M.subresourceLayersMipLevel = i,
		Vk.Img.M.subresourceLayersBaseArrayLayer = 0,
		Vk.Img.M.subresourceLayersLayerCount = 1 }

half :: Integral i => i -> i
half n = bool 1 (n `div` 2) (n > 1)

halves :: Integral i => i -> [i]
halves = iterate half

newtype MyImage = MyImage (Image PixelRGBA8)

-- type instance Vk.Bffr.ImageFormat MyImage = 'Vk.T.FormatR8g8b8a8Srgb

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

instance KObj.IsImage MyImage where
	type IsImagePixel MyImage = MyRgba8
	type ImageFormat MyImage = 'Vk.T.FormatR8g8b8a8Srgb
	isImageRow = KObj.isImageWidth
	isImageWidth (MyImage img) = imageWidth img
	isImageHeight (MyImage img) = imageHeight img
	isImageDepth _ = 1
	isImageBody (MyImage img) = (<$> [0 .. imageHeight img - 1]) \y ->
		(<$> [0 .. imageWidth img - 1]) \x -> MyRgba8 $ pixelAt img x y
	isImageMake w h _d pss = MyImage
		$ generateImage (\x y -> let MyRgba8 p = (pss' ! y) ! x in p) w h
		where pss' = listArray (0, h - 1) (listArray (0, w - 1) <$> pss)

createImage :: forall nm fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mem.PropertyFlagBits -> (forall si sm .
		Vk.Img.BindedNew si sm nm fmt ->
		Vk.Dvc.Mem.ImageBuffer.M sm
			'[ '(si, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt) ] ->
		IO a) -> IO a
createImage pd dvc wdt hgt mplvs nss tlng usg prps f = Vk.Img.createNew @'Nothing dvc
		(imageInfo wdt hgt mplvs nss tlng usg) nil' \img -> do
	memInfo <- imageMemoryInfo pd dvc prps img
	imageAllocateBind dvc img memInfo f

recreateImage :: Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mem.PropertyFlags ->
	Vk.Img.BindedNew sb sm nm fmt ->
	Vk.Dvc.Mem.ImageBuffer.M
		sm '[ '(sb, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt)] -> IO ()
recreateImage pd dvc wdt hgt mplvs nss tlng usg prps img mem = do
	Vk.Img.recreateNew @'Nothing dvc
		(imageInfo wdt hgt mplvs nss tlng usg) nil' img
	memInfo <- imageMemoryInfoBinded pd dvc prps img
	imageReallocateBind dvc img memInfo mem

imageInfo ::
	Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags -> Vk.Img.Tiling -> Vk.Img.UsageFlags ->
	Vk.Img.CreateInfoNew 'Nothing fmt
imageInfo wdt hgt mplvs numSamples tlng usg = Vk.Img.CreateInfoNew {
		Vk.Img.createInfoNextNew = TMaybe.N,
		Vk.Img.createInfoImageTypeNew = Vk.Img.Type2d,
		Vk.Img.createInfoExtentNew = Vk.Extent3d {
			Vk.extent3dWidth = wdt,
			Vk.extent3dHeight = hgt,
			Vk.extent3dDepth = 1 },
		Vk.Img.createInfoMipLevelsNew = mplvs,
		Vk.Img.createInfoArrayLayersNew = 1,
		Vk.Img.createInfoTilingNew = tlng,
		Vk.Img.createInfoInitialLayoutNew = Vk.Img.LayoutUndefined,
		Vk.Img.createInfoUsageNew = usg,
		Vk.Img.createInfoSharingModeNew = Vk.SharingModeExclusive,
		Vk.Img.createInfoSamplesNew = numSamples,
		Vk.Img.createInfoFlagsNew = zeroBits,
		Vk.Img.createInfoQueueFamilyIndicesNew = [] }

imageAllocateBind :: Vk.Dvc.D sd -> Vk.Img.INew si nm fmt ->
	Vk.Dvc.Mem.Buffer.AllocateInfo 'Nothing -> (forall sm .
		Vk.Img.BindedNew si sm nm fmt ->
		Vk.Dvc.Mem.ImageBuffer.M sm
			'[ '(si, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt) ] ->
		IO a) -> IO a
imageAllocateBind dvc img memInfo f =
	Vk.Dvc.Mem.ImageBuffer.allocateBind @'Nothing dvc
		(HeteroParList.Singleton . U2 $ Vk.Dvc.Mem.ImageBuffer.Image img) memInfo
		nil' \(HeteroParList.Singleton (U2 (Vk.Dvc.Mem.ImageBuffer.ImageBinded bnd))) m -> do
		f bnd m

imageReallocateBind ::
	Vk.Dvc.D sd -> Vk.Img.BindedNew sb sm nm fmt ->
	Vk.Dvc.Mem.Buffer.AllocateInfo 'Nothing ->
	Vk.Dvc.Mem.ImageBuffer.M
		sm '[ '(sb, 'Vk.Dvc.Mem.ImageBuffer.K.Image nm fmt)] -> IO ()
imageReallocateBind dvc img memInfo m =
	Vk.Dvc.Mem.ImageBuffer.reallocateBind @'Nothing dvc
		(HeteroParList.Singleton . U2 $ Vk.Dvc.Mem.ImageBuffer.ImageBinded img) memInfo
		nil' m

imageMemoryInfo ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Mem.PropertyFlags ->
	Vk.Img.INew s nm fmt -> IO (Vk.Dvc.Mem.Buffer.AllocateInfo 'Nothing)
imageMemoryInfo pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirementsNew dvc img
	mt <- findMemoryType pd (Vk.Mem.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Dvc.Mem.Buffer.AllocateInfo {
		Vk.Dvc.Mem.Buffer.allocateInfoNext = TMaybe.N,
		Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = mt }

imageMemoryInfoBinded ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Mem.PropertyFlags ->
	Vk.Img.BindedNew sm si nm fmt -> IO (Vk.Dvc.Mem.Buffer.AllocateInfo 'Nothing)
imageMemoryInfoBinded pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirementsBindedNew dvc img
	mt <- findMemoryType pd (Vk.Mem.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Dvc.Mem.Buffer.AllocateInfo {
		Vk.Dvc.Mem.Buffer.allocateInfoNext = TMaybe.N,
		Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = mt }

transitionImageLayout :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Img.BindedNew si sm nm fmt -> Vk.Img.Layout -> Vk.Img.Layout ->
	Word32 ->
	IO ()
transitionImageLayout dvc gq cp img olyt nlyt mplvs =
	beginSingleTimeCommands dvc gq cp \cb -> do
	let	barrier :: Vk.Img.MemoryBarrier 'Nothing si sm nm fmt
		barrier = Vk.Img.MemoryBarrier {
			Vk.Img.memoryBarrierNext = TMaybe.N,
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
			Vk.Img.subresourceRangeLevelCount = mplvs,
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

copyBufferToImage :: forall sd sc sm sb nm img inm si sm' nm' .
	Storable (KObj.IsImagePixel img) =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[ VObj.ObjImage 1 img inm]  ->
--	Vk.Img.BindedNew si sm' nm' (Vk.Bffr.ImageFormat img) ->
	Vk.Img.BindedNew si sm' nm' (KObj.ImageFormat img) ->
	Word32 -> Word32 -> IO ()
copyBufferToImage dvc gq cp bf img wdt hgt =
	beginSingleTimeCommands dvc gq cp \cb -> do
	let	region :: Vk.Bffr.ImageCopy img inm
		region = Vk.Bffr.ImageCopy {
			Vk.Bffr.imageCopyImageSubresource = isr,
			Vk.Bffr.imageCopyImageOffset = Vk.Offset3d 0 0 0,
			Vk.Bffr.imageCopyImageExtent = Vk.Extent3d wdt hgt 1 }
		isr = Vk.Img.M.SubresourceLayers {
			Vk.Img.M.subresourceLayersAspectMask =
				Vk.Img.AspectColorBit,
			Vk.Img.M.subresourceLayersMipLevel = 0,
			Vk.Img.M.subresourceLayersBaseArrayLayer = 0,
			Vk.Img.M.subresourceLayersLayerCount = 1 }
	Vk.Cmd.copyBufferToImageNew @1
		cb bf img Vk.Img.LayoutTransferDstOptimal (HeteroParList.Singleton region)

createTextureSampler :: Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Word32 -> Float -> (forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTextureSampler phdv dvc mplvs mnld f = do
	prp <- Vk.PhDvc.getProperties phdv
	print . Vk.PhDvc.limitsMaxSamplerAnisotropy $ Vk.PhDvc.propertiesLimits prp
	let	samplerInfo = Vk.Smplr.M.CreateInfo {
			Vk.Smplr.M.createInfoNext = TMaybe.N,
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
			Vk.Smplr.M.createInfoMinLod = mnld,
			Vk.Smplr.M.createInfoMaxLod = fromIntegral mplvs,
			Vk.Smplr.M.createInfoBorderColor =
				Vk.BorderColorIntOpaqueBlack,
			Vk.Smplr.M.createInfoUnnormalizedCoordinates = False }
	Vk.Smplr.create @'Nothing dvc samplerInfo nil' f

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
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> V.Vector Vertex ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ VObj.List 256 Vertex ""] -> IO a ) -> IO a
createVertexBuffer phdvc dvc gq cp vtcs f =
	createBufferList phdvc dvc (V.length vtcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList phdvc dvc (V.length vtcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
		\(b' :: Vk.Bffr.Binded sb sm "vertex-buffer" '[ VObj.List 256 t ""])
			(bm' :: Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
				sb,
				'Vk.Dvc.Mem.ImageBuffer.K.Buffer "vertex-buffer"
					'[ VObj.List 256 Vertex ""] ) ]) -> do
	Vk.Dvc.Mem.ImageBuffer.write
		@"vertex-buffer" @(VObj.List 256 Vertex "") dvc bm' zeroBits vtcs
	copyBuffer dvc gq cp b' b
	f b

createIndexBuffer :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> V.Vector Word32 -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ VObj.List 256 Word32 ""] -> IO a) -> IO a
createIndexBuffer phdvc dvc gq cp idcs f =
	createBufferList phdvc dvc (V.length idcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageIndexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList phdvc dvc (V.length idcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
		\(b' :: Vk.Bffr.Binded sb sm "index-buffer" '[ VObj.List 256 t ""])
			(bm' :: Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
				sb,
				'Vk.Dvc.Mem.ImageBuffer.K.Buffer "index-buffer"
					'[ VObj.List 256 Word32 ""] ) ]) -> do
	Vk.Dvc.Mem.ImageBuffer.write
		@"index-buffer" @(VObj.List 256 Word32 "") dvc bm' zeroBits idcs
	copyBuffer dvc gq cp b' b
	f b

createUniformBuffer :: Vk.PhDvc.P -> Vk.Dvc.D sd -> (forall sm sb .
		Vk.Bffr.Binded sb sm "uniform-buffer" '[ VObj.Atom 256 UniformBufferObject 'Nothing]  ->
		Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
			sb,
			'Vk.Dvc.Mem.ImageBuffer.K.Buffer "uniform-buffer"
				'[ VObj.Atom 256 UniformBufferObject 'Nothing]) ] ->
		IO b) -> IO b
createUniformBuffer phdvc dvc = createBufferAtom phdvc dvc
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mem.PropertyHostVisibleBit .|. Vk.Mem.PropertyHostCoherentBit)

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDescriptorPool dvc = Vk.DscPool.create dvc poolInfo nil'
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = TMaybe.N,
		Vk.DscPool.createInfoFlags = zeroBits,
		Vk.DscPool.createInfoMaxSets = 1,
		Vk.DscPool.createInfoPoolSizes = [poolSize0, poolSize1] }
	poolSize0 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = 1 }
	poolSize1 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscPool.sizeDescriptorCount = 1 }

createDescriptorSet ::
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> Vk.Bffr.Binded sm sb nm '[ VObj.Atom 256 UniformBufferObject 'Nothing] ->
	Vk.ImgVw.INew 'Vk.T.FormatR8g8b8a8Srgb "texture" siv  -> Vk.Smplr.S ss ->
	Vk.DscSetLyt.L sdsc '[
		'Vk.DscSetLyt.Buffer '[ VObj.Atom 256 UniformBufferObject 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ] ->
	IO (Vk.DscSet.S sd sp '(sdsc, '[
		'Vk.DscSetLyt.Buffer '[ VObj.Atom 256 UniformBufferObject 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ]))
createDescriptorSet dvc dscp ub tximgvw txsmp dscslyt = do
	HeteroParList.Singleton dscs <- Vk.DscSet.allocateSs dvc allocInfo
	Vk.DscSet.updateDsNew dvc (
		U5 (descriptorWrite0 ub dscs) :**
		U5 (descriptorWrite1 dscs tximgvw txsmp) :**
		HeteroParList.Nil )
		HeteroParList.Nil
	pure dscs
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts =
			HeteroParList.Singleton $ U2 dscslyt }

descriptorWrite0 ::
	Vk.Bffr.Binded sm sb nm '[ VObj.Atom 256 UniformBufferObject 'Nothing] ->
	Vk.DscSet.S sd sp '(sl, bts) ->
	Vk.DscSet.Write 'Nothing sd sp '(sl, bts) ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sb, sm, nm,
		'[ VObj.Atom 256 UniformBufferObject 'Nothing], VObj.Atom 256 UniformBufferObject 'Nothing )])
descriptorWrite0 ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $ HeteroParList.Singleton bufferInfo }
	where bufferInfo = Vk.Dsc.BufferInfoAtom ub

descriptorWrite1 ::
	Vk.DscSet.S sd sp '(sl, bts) -> Vk.ImgVw.INew fmt nm si -> Vk.Smplr.S ss ->
	Vk.DscSet.Write 'Nothing sd sp '(sl, bts)
		('Vk.DscSet.WriteSourcesArgImage '[ '(ss, fmt, nm, si) ])
descriptorWrite1 dscs tiv tsmp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeCombinedImageSampler,
	Vk.DscSet.writeSources = Vk.DscSet.ImageInfos . HeteroParList.Singleton
		$ U4 Vk.Dsc.ImageInfo {
			Vk.Dsc.imageInfoImageLayout =
				Vk.Img.LayoutShaderReadOnlyOptimal,
			Vk.Dsc.imageInfoImageView = tiv,
			Vk.Dsc.imageInfoSampler = tsmp }
	}

createBufferAtom :: forall sd nm a b . Storable a => Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sb sm nm '[ VObj.Atom 256 a 'Nothing] ->
		Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
			sb,
			'Vk.Dvc.Mem.ImageBuffer.K.Buffer nm '[ VObj.Atom 256 a 'Nothing] )] ->
			IO b) -> IO b
createBufferAtom p dv usg props = createBuffer p dv VObj.ObjectLengthAtom usg props

createBufferList :: forall sd nm t a . Storable t =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Int -> Vk.Bffr.UsageFlags ->
	Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sb sm nm '[ VObj.List 256 t ""] ->
		Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
			sb,
			'Vk.Dvc.Mem.ImageBuffer.K.Buffer nm '[ VObj.List 256 t ""] ) ] ->
		IO a) ->
	IO a
createBufferList p dv ln usg props =
	createBuffer p dv (VObj.ObjectLengthList ln) usg props

createBufferImage :: Storable (KObj.IsImagePixel t) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> (Int, Int, Int, Int) ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sb sm nm '[ VObj.ObjImage 1 t inm] ->
		Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
			sb,
			'Vk.Dvc.Mem.ImageBuffer.K.Buffer nm '[ VObj.ObjImage 1 t inm])] ->
		IO a) -> IO a
createBufferImage p dv (r, w, h, d) usg props =
	createBuffer p dv (VObj.ObjectLengthImage r w h d) usg props

createBuffer :: forall sd nm o a . VObj.SizeAlignment o =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> VObj.ObjectLength o ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sb sm nm '[o] ->
		Vk.Dvc.Mem.ImageBuffer.M sm
			'[ '(sb, 'Vk.Dvc.Mem.ImageBuffer.K.Buffer nm '[o])] ->
		IO a) -> IO a
createBuffer p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil' \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Dvc.Mem.ImageBuffer.allocateBind dv (HeteroParList.Singleton . U2 $ Vk.Dvc.Mem.ImageBuffer.Buffer b)
		(allcInfo mt) nil'
		$ f . \(HeteroParList.Singleton (U2 (Vk.Dvc.Mem.ImageBuffer.BufferBinded bnd))) -> bnd
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

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a . Storable' a =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[ VObj.List 256 a ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ VObj.List 256 a ""] -> IO ()
copyBuffer dvc gq cp src dst = beginSingleTimeCommands dvc gq cp \cb ->
	Vk.Cmd.copyBufferNew @'[ '[ VObj.List 256 a ""]] cb src dst

beginSingleTimeCommands :: forall sd sc a .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	(forall s . Vk.CmdBffr.C s -> IO a) -> IO a
beginSingleTimeCommands dvc gq cp cmd = do
	Vk.CmdBffr.allocateNew
		dvc allocInfo \((cb :: Vk.CmdBffr.C s) :*. HeteroParList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
				Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
		Vk.CmdBffr.beginNew @'Nothing @'Nothing cb beginInfo (cmd cb) <* do
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
createCommandBuffer dvc cp f = Vk.CmdBffr.allocateNew dvc allocInfo $ f . \(cb :*. HeteroParList.Nil) -> cb
	where
	allocInfo :: Vk.CmdBffr.AllocateInfoNew 'Nothing scp '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfoNew {
		Vk.CmdBffr.allocateInfoNextNew = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPoolNew = cp,
		Vk.CmdBffr.allocateInfoLevelNew = Vk.CmdBffr.LevelPrimary }

addTypeToProxy ::
	Proxy vss -> Proxy ('[AddType Vertex 'Vk.VtxInp.RateVertex] ': vss)
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

recordCommandBuffer :: forall scb sr scfmt sf sl sg sm sb nm sm' sb' nm' sdsc sp sdsl .
	Vk.CmdBffr.C scb ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.GNew sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[ VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ VObj.List 256 Word32 ""] ->
	Vk.DscSet.S sdsc sp (AtomUbo sdsl) ->
	IO ()
recordCommandBuffer cb rp fb sce ppllyt gpl idcs vb ib ubds =
	Vk.CmdBffr.beginNew @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass' cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineNew cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb
		(HeteroParList.Singleton . U4 $ Vk.Bffr.IndexedList @_ @_ @_ @Vertex vb) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedList @_ @_ @_ @Word32 ib) >>
	Vk.Cmd.bindDescriptorSetsNew cbb Vk.Ppl.BindPointGraphics ppllyt
		(HeteroParList.Singleton $ U2 ubds)
		(HeteroParList.Singleton
			(HeteroParList.Nil :** HeteroParList.Nil :** HeteroParList.Nil)) >>
	Vk.Cmd.drawIndexed cbb (fromIntegral $ V.length idcs) 1 0 0 0
	where
	rpInfo :: Vk.RndrPass.BeginInfo 'Nothing sr sf '[
		'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32,
		'Vk.ClearTypeDepthStencil ]
	rpInfo = Vk.RndrPass.BeginInfo {
		Vk.RndrPass.beginInfoNext = TMaybe.N,
		Vk.RndrPass.beginInfoRenderPass = rp,
		Vk.RndrPass.beginInfoFramebuffer = fb,
		Vk.RndrPass.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = sce },
		Vk.RndrPass.beginInfoClearValues =
			Vk.ClearValueColor (fromJust $ rgbaDouble 0 0 0 1) :**
			Vk.ClearValueDepthStencil (Vk.ClearDepthStencilValue 1 0) :**
			HeteroParList.Nil }

mainLoop :: (
	RecreateFramebuffers ss sfs,
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt ) =>
	FramebufferResized ->
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc scfmt -> Vk.Extent2d ->
	HeteroParList.PL (Vk.ImgVw.INew scfmt nm) ss ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.GNew sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPool.C sc ->
	ColorResources scfmt crnm crsi crsm crsiv ->
	Vk.Img.BindedNew sdi sdm "depth-buffer" dptfmt ->
	Vk.Dvc.Mem.ImageBuffer.M
		sdm '[ '(sdi, 'Vk.Dvc.Mem.ImageBuffer.K.Image "depth-buffer" dptfmt)] ->
	Vk.ImgVw.INew dptfmt "depth-buffer" sdiv ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[ VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ VObj.List 256 Word32 ""] ->
	Vk.Dvc.Mem.ImageBuffer.M sm2 '[ '(
		sb2,
		'Vk.Dvc.Mem.ImageBuffer.K.Buffer "uniform-buffer"
			'[ VObj.Atom 256 UniformBufferObject 'Nothing] )] ->
	Vk.DscSet.S sd sp (AtomUbo sdsl) ->
	Vk.CmdBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> UTCTime -> IO ()
mainLoop g w sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs cp clrrscs dptImg dptImgMem dptImgVw idcs vb ib ubm ubds cb iasrfsifs tm0 = do
	($ ext0) $ fix \loop ext -> do
		Glfw.pollEvents
		tm <- getCurrentTime
		runLoop w sfc phdvc qfis dvc gq pq
			sc g ext scivs
			rp ppllyt gpl clrrscs dptImg dptImgMem dptImgVw cp fbs idcs vb ib ubm ubds cb iasrfsifs
			(realToFrac $ tm `diffUTCTime` tm0) loop
	Vk.Dvc.waitIdle dvc

runLoop :: (
	RecreateFramebuffers sis sfs,
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc scfmt -> FramebufferResized -> Vk.Extent2d ->
	HeteroParList.PL (Vk.ImgVw.INew scfmt nm) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.GNew sg '[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	ColorResources scfmt crnm crsi crsm crsiv ->
	Vk.Img.BindedNew sdi sdm "depth-buffer" dptfmt ->
	Vk.Dvc.Mem.ImageBuffer.M
		sdm '[ '(sdi, 'Vk.Dvc.Mem.ImageBuffer.K.Image "depth-buffer" dptfmt)] ->
	Vk.ImgVw.INew dptfmt "depth-buffer" sdiv ->
	Vk.CmdPool.C sc ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[ VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ VObj.List 256 Word32 ""] ->
	Vk.Dvc.Mem.ImageBuffer.M sm2 '[ '(
		sb2,
		'Vk.Dvc.Mem.ImageBuffer.K.Buffer "uniform-buffer"
			'[ VObj.Atom 256 UniformBufferObject 'Nothing] )] ->
	Vk.DscSet.S sd sp (AtomUbo sdsl) ->
	Vk.CmdBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> Float ->
	(Vk.Extent2d -> IO ()) -> IO ()
runLoop win sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl clrrscs dptImg dptImgMem dptImgVw cp fbs idcs vb ib ubm ubds cb iasrfsifs tm loop = do
	catchAndRecreate win sfc phdvc qfis dvc gq sc scivs
		rp ppllyt gpl clrrscs dptImg dptImgMem dptImgVw cp fbs loop
		$ drawFrame dvc gq pq sc ext rp ppllyt gpl fbs idcs vb ib ubm ubds cb iasrfsifs tm
	cls <- Glfw.windowShouldClose win
	if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateSwapChainEtc
			win sfc phdvc qfis dvc gq sc scivs
			rp ppllyt gpl clrrscs dptImg dptImgMem dptImgVw cp fbs)

drawFrame :: forall sfs sd ssc scfmt sr sl sg sm sb nm sm' sb' nm' sm2 sb2 scb sias srfs siff sdsc sp sdsl .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> Vk.Khr.Swapchain.SNew ssc scfmt ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.GNew sg '[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[ VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ VObj.List 256 Word32 ""] ->
	Vk.Dvc.Mem.ImageBuffer.M sm2 '[ '(
		sb2,
		'Vk.Dvc.Mem.ImageBuffer.K.Buffer "uniform-buffer"
			'[ VObj.Atom 256 UniformBufferObject 'Nothing] )] ->
	Vk.DscSet.S sdsc sp (AtomUbo sdsl) ->
	Vk.CmdBffr.C scb -> SyncObjects '(sias, srfs, siff) -> Float -> IO ()
drawFrame dvc gq pq sc ext rp ppllyt gpl fbs idcs vb ib ubm ubds cb (SyncObjects ias rfs iff) tm = do
	let	siff = HeteroParList.Singleton iff
	Vk.Fence.waitForFs dvc siff True maxBound
	imgIdx <- Vk.Khr.acquireNextImageResultNew [Vk.Success, Vk.SuboptimalKhr]
		dvc sc uint64Max (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.resetNew cb def
	HeteroParList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext ppllyt gpl idcs vb ib ubds
	updateUniformBuffer dvc ubm ext tm
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HeteroParList.Singleton rfs }
		presentInfo' = Vk.Khr.PresentInfoNew {
			Vk.Khr.presentInfoNextNew = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphoresNew = HeteroParList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndicesNew = HeteroParList.Singleton
				$ Vk.Khr.SwapchainImageIndexNew sc imgIdx }
	Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresentNew @'Nothing pq presentInfo'

updateUniformBuffer :: Vk.Dvc.D sd ->
	Vk.Dvc.Mem.ImageBuffer.M sm '[ '(
		sb,
		'Vk.Dvc.Mem.ImageBuffer.K.Buffer "uniform-buffer"
			'[ VObj.Atom 256 UniformBufferObject 'Nothing] )] ->
	Vk.Extent2d -> Float -> IO ()
updateUniformBuffer dvc um sce tm = do
	Vk.Dvc.Mem.ImageBuffer.write @"uniform-buffer"
		@(VObj.Atom 256 UniformBufferObject 'Nothing) dvc um zeroBits ubo
	where ubo = UniformBufferObject {
		uniformBufferObjectModel = Cglm.rotate
			Cglm.mat4Identity
			(tm * Cglm.rad 90)
			(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL),
		uniformBufferObjectView = Cglm.lookat
			(Cglm.Vec3 $ 2 :. 2 :. 2 :. NilL)
			(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
			(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL),
		uniformBufferObjectProj = Cglm.modifyMat4 1 1 negate
			$ Cglm.perspective
				(Cglm.rad 45)
				(fromIntegral (Vk.extent2dWidth sce) /
					fromIntegral (Vk.extent2dHeight sce))
				0.1 10 }

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate :: (
	RecreateFramebuffers sis sfs,
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc scfmt ->
	HeteroParList.PL (Vk.ImgVw.INew scfmt nm) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.GNew sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	ColorResources scfmt crnm crsi crsm crsiv ->
	Vk.Img.BindedNew sdi sdm "depth-buffer" dptfmt ->
	Vk.Dvc.Mem.ImageBuffer.M
		sdm '[ '(sdi, 'Vk.Dvc.Mem.ImageBuffer.K.Image "depth-buffer" dptfmt)] ->
	Vk.ImgVw.INew dptfmt "depth-buffer" sdiv ->
	Vk.CmdPool.C sc ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	(Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl clrrscs dptImg dptImgMem dptImgVw cp fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateSwapChainEtc
		win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl clrrscs dptImg dptImgMem dptImgVw cp fbs

recreateSwapChainEtc :: (
	RecreateFramebuffers sis sfs,
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt ) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc scfmt -> HeteroParList.PL (Vk.ImgVw.INew scfmt nm) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.L sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.GNew sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	ColorResources scfmt crnm crsi crsm crsiv ->
	Vk.Img.BindedNew sdi sdm "depth-buffer" dptfmt ->
	Vk.Dvc.Mem.ImageBuffer.M
		sdm '[ '(sdi, 'Vk.Dvc.Mem.ImageBuffer.K.Image "depth-buffer" dptfmt)] ->
	Vk.ImgVw.INew dptfmt "depth-buffer" sdiv ->
	Vk.CmdPool.C sc ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	IO Vk.Extent2d
recreateSwapChainEtc win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl
	(clrimg, clrimgm, clrimgvw, mss) dptImg dptImgMem dptImgVw cp fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwapChain win sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swapchain.getImagesNew dvc sc >>= \imgs ->
			recreateImageViews dvc imgs scivs
		recreateColorResources phdvc dvc ext mss clrimg clrimgm clrimgvw
		recreateDepthResources phdvc dvc gq cp ext mss dptImg dptImgMem dptImgVw
		recreateGraphicsPipeline' dvc ext rp ppllyt mss gpl
		recreateFramebuffers dvc ext rp scivs dptImgVw clrimgvw fbs

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

layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) out vec4 outColor;

void
main()
{
//	outColor = texture(texSampler, fragTexCoord * 2.0);
	outColor = vec4(fragColor * texture(texSampler, fragTexCoord).rgb, 1.0);
}

|]
