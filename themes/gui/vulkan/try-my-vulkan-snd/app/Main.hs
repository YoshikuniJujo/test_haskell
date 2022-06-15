{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Pointable
import Control.Monad.Fix
import Control.Monad.Cont
import Data.Maybe
import Data.List
import Data.IORef
import Data.Bits
import Data.Bool
import Data.Word
import Data.Color

import ThEnv
import Vulkan.Base

import qualified Data.Set as Set
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Graphics.UI.GLFW as GlfwB

import qualified Glfw

import Shaderc
import Shaderc.EnumAuto
import Shaderc.TH

import qualified Vulkan.Middle as Vk
import qualified Vulkan.Enum as Vk
import qualified Vulkan.Instance.Type as Vk.Ist.T
import qualified Vulkan.Instance.Middle as Vk.Ist
import qualified Vulkan.Instance.Enum as Vk.Ist
import qualified Vulkan.Ext.DebugUtils.Messenger as Vk.Ext.DU.Msngr
import qualified Vulkan.Ext.DebugUtils.Message.Enum as Vk.Ext.DU.Msg

import qualified Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Vulkan.QueueFamily as Vk.QueueFamily
import qualified Vulkan.QueueFamily.EnumManual as Vk.QueueFamily
import qualified Vulkan.Device.Queue as Vk.Device.Queue
import qualified Vulkan.Device.Queue.Enum as Vk.Device.Queue
import qualified Vulkan.Device.Middle as Vk.Device

import qualified Vulkan.Khr as Vk.Khr
import qualified Vulkan.Khr.Enum as Vk.Khr
import qualified Vulkan.Khr.Surface as Vk.Khr.Sfc
import qualified Vulkan.Khr.Surface.PhysicalDevice as Vk.Khr.Sfc.PhysicalDevice
import qualified Vulkan.Image.Middle as Vk.Img
import qualified Vulkan.Image.Enum as Vk.Img

import qualified Vulkan.Core as Vk.C
import qualified Vulkan.Ext.DebugUtils as Vk.Ext.DU

import qualified Vulkan.Khr.Swapchain as Vk.Khr.Sc
import qualified Vulkan.Khr.Swapchain.Enum as Vk.Khr.Sc
import qualified Vulkan.ImageView.Middle as Vk.ImageView
import qualified Vulkan.ImageView.Enum as Vk.ImageView
import qualified Vulkan.Component as Vk.Component
import qualified Vulkan.Component.Enum as Vk.Component

import qualified Vulkan.ShaderModule.Middle as Vk.Shader.Module
import qualified Vulkan.Pipeline.ShaderStage.Middle as Vk.Ppl.ShaderStage
import qualified Vulkan.Pipeline.ShaderStage.Enum as Vk.Ppl.ShaderStage
import qualified Vulkan.Pipeline.VertexInputState as Vk.Ppl.VI
import qualified Vulkan.Pipeline.VertexInputState.Middle as Vk.Ppl.VI.M
import qualified Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.IA
import qualified Vulkan.Pipeline.ViewportState as Vk.Ppl.VP
import qualified Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Vulkan.Pipeline.MultisampleState as Vk.Ppl.MS
import qualified Vulkan.Sample as Vk.Sample
import qualified Vulkan.Sample.Enum as Vk.Sample
import qualified Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.CBA
import qualified Vulkan.Pipeline.ColorBlendState as Vk.Ppl.CB
import qualified Vulkan.Pipeline.Layout.Middle as Vk.Ppl.Lyt
import qualified Vulkan.Attachment as Vk.Att
import qualified Vulkan.Attachment.Enum as Vk.Att
import qualified Vulkan.Subpass as Vk.Subpass
import qualified Vulkan.Subpass.Enum as Vk.Subpass
import qualified Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Vulkan.RenderPass.Middle as Vk.RndrPss
import qualified Vulkan.RenderPass.Enum as Vk.RndrPss
import qualified Vulkan.Pipeline.Graphics.Middle as Vk.Ppl
import qualified Vulkan.Framebuffer.Middle as Vk.Fb
import qualified Vulkan.Framebuffer.Enum as Vk.Fb
import qualified Vulkan.CommandPool.Middle as Vk.CP
import qualified Vulkan.CommandPool.Enum as Vk.CP
import qualified Vulkan.CommandBuffer.Middle as Vk.CB
import qualified Vulkan.CommandBuffer.Enum as Vk.CB
import qualified Vulkan.Command as Vk.Cmd
import qualified Vulkan.Semaphore as Vk.Smp
import qualified Vulkan.Fence as Vk.Fnc
import qualified Vulkan.Fence.Enum as Vk.Fnc
import qualified Vulkan.Format.Enum as Vk.Format
import qualified Vulkan.Queue as Vk.Queue
import qualified Vulkan.Queue.Enum as Vk.Queue
import qualified Vulkan.Command.Middle as Vk.Cmd.M

import qualified Vulkan.ColorComponent.Enum as Vk.CC

main :: IO ()
main = run

enableValidationLayers :: Bool
enableValidationLayers =
	maybe True (const False) $(lookupCompileEnvExp "NDEBUG")

validationLayers :: [Txt.Text]
validationLayers = [Vk.Khr.validationLayerName]

data Global = Global {
	globalWindow :: GlfwB.Window,
	globalInstance :: IORef Vk.Ist.I,
	globalDebugMessenger :: IORef Vk.Ext.DU.Messenger,
	globalPhysicalDevice :: IORef Vk.PhysicalDevice.P,
	globalDevice :: IORef Vk.Device.D,
	globalGraphicsQueue :: IORef Vk.Queue.Q,
	globalPresentQueue :: IORef Vk.Queue.Q,
	globalSurface :: IORef Vk.Khr.Sfc.S,
	globalSwapChain :: IORef Vk.Khr.Sc.S,
	globalSwapChainImages :: IORef [Vk.Img.I],
	globalSwapChainImageFormat :: IORef Vk.Format.F,
	globalSwapChainExtent :: IORef Vk.C.Extent2d,
	globalSwapChainImageViews :: IORef [Vk.ImageView.I],
	globalPipelineLayout :: IORef Vk.Ppl.Lyt.L,
	globalRenderPass :: IORef Vk.RndrPss.R,
	globalGraphicsPipeline :: IORef (Vk.Ppl.G () '[]),
	globalSwapChainFramebuffers :: IORef [Vk.Fb.F],
	globalCommandPool :: IORef Vk.CP.C,
	globalCommandBuffers :: IORef [Vk.CB.C ()],
	globalImageAvailableSemaphore :: IORef Vk.Smp.S,
	globalRenderFinishedSemaphore :: IORef Vk.Smp.S,
	globalInFlightFence :: IORef Vk.Fnc.F
	}

newGlobal :: GlfwB.Window -> IO Global
newGlobal w = do
	ist <- newIORef $ Vk.Ist.I NullPtr
	dmsgr <- newIORef $ Vk.Ext.DU.Messenger NullPtr
	pdvc <- newIORef $ Vk.PhysicalDevice.P NullPtr
	dvc <- newIORef $ Vk.Device.D NullPtr
	gq <- newIORef $ Vk.Queue.Q NullPtr
	pq <- newIORef $ Vk.Queue.Q NullPtr
	sfc <- newIORef $ Vk.Khr.Sfc.S NullPtr
	sc <- newIORef $ Vk.Khr.Sc.S NullPtr
	scimgs <- newIORef []
	scimgfmt <- newIORef $ Vk.Format.Undefined
	scex <- newIORef $ Vk.C.Extent2d 0 0
	scivs <- newIORef []
	lyt <- newIORef $ Vk.Ppl.Lyt.L NullPtr
	rp <- newIORef $ Vk.RndrPss.R NullPtr
	gpl <- newIORef Vk.Ppl.GNull
	scfbs <- newIORef []
	cp <- newIORef $ Vk.CP.C NullPtr
	cbs <- newIORef []
	ias <- newIORef $ Vk.Smp.S NullPtr
	rfs <- newIORef $ Vk.Smp.S NullPtr
	iff <- newIORef $ Vk.Fnc.F NullPtr
	pure Global {
		globalWindow = w,
		globalInstance = ist,
		globalDebugMessenger = dmsgr,
		globalPhysicalDevice = pdvc,
		globalDevice = dvc,
		globalGraphicsQueue = gq,
		globalPresentQueue = pq,
		globalSurface = sfc,
		globalSwapChain = sc,
		globalSwapChainImages = scimgs,
		globalSwapChainImageFormat = scimgfmt,
		globalSwapChainExtent = scex,
		globalSwapChainImageViews = scivs,
		globalPipelineLayout = lyt,
		globalRenderPass = rp,
		globalGraphicsPipeline = gpl,
		globalSwapChainFramebuffers = scfbs,
		globalCommandPool = cp,
		globalCommandBuffers = cbs,
		globalImageAvailableSemaphore = ias,
		globalRenderFinishedSemaphore = rfs,
		globalInFlightFence = iff
		}

run :: IO ()
run = do
	w <- initWindow
	g <- newGlobal w
	initVulkan g
	mainLoop g
	cleanup g

width, height :: Int
width = 800; height = 600

initWindow :: IO GlfwB.Window
initWindow = do
	True <- GlfwB.init
	GlfwB.windowHint $ GlfwB.WindowHint'ClientAPI GlfwB.ClientAPI'NoAPI
	GlfwB.windowHint $ GlfwB.WindowHint'Resizable False
	Just w <- GlfwB.createWindow width height "Vulkan" Nothing Nothing
	pure w

initVulkan :: Global -> IO ()
initVulkan g = do
	createInstance g
	when enableValidationLayers $ setupDebugMessenger g
	createSurface g
	pickPhysicalDevice g
	createLogicalDevice g
	createSwapChain g
	createImageViews g
	createRenderPass g
	createGraphicsPipeline g
	createFramebuffers g
	createCommandPool g
	createCommandBuffers g
	createSyncObjects g

createInstance :: Global -> IO ()
createInstance Global { globalInstance = rist } = ($ pure) $ runContT do
	lift do	b <- checkValidationLayerSupport
		when (enableValidationLayers && not b)
			$ error "validation layers requested, but no available!"
		putStrLn "available extensions:"
		mapM_ (Txt.putStrLn . ("\t" <>)
				. Vk.extensionPropertiesExtensionName)
			=<< Vk.Ist.enumerateExtensionProperties Nothing
	extensions <- lift getRequiredExtensions
	let	appInfo = Vk.ApplicationInfo {
			Vk.applicationInfoNext = Nothing,
			Vk.applicationInfoApplicationName = "Hello Triangle",
			Vk.applicationInfoApplicationVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoEngineName = "No Engine",
			Vk.applicationInfoEngineVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoApiVersion = Vk.apiVersion_1_0 }
		createInfo = Vk.Ist.CreateInfo {
			Vk.Ist.createInfoNext = Just debugMessengerCreateInfo,
			Vk.Ist.createInfoFlags = Vk.Ist.CreateFlagsZero,
			Vk.Ist.createInfoApplicationInfo = Just appInfo,
			Vk.Ist.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Ist.createInfoEnabledExtensionNames = extensions }
	lift $ writeIORef rist
		=<< Vk.Ist.create @_ @() @() createInfo Nothing

checkValidationLayerSupport :: IO Bool
checkValidationLayerSupport =
	(\lns -> all (`elem` lns) validationLayers)
			. map Vk.layerPropertiesLayerName
		<$> Vk.Ist.enumerateLayerProperties

getRequiredExtensions :: IO [Txt.Text]
getRequiredExtensions =
	bool id (Vk.Ext.DU.extensionName :) enableValidationLayers
		<$> ((cstringToText `mapM`)
			=<< GlfwB.getRequiredInstanceExtensions)

setupDebugMessenger :: Global -> IO ()
setupDebugMessenger Global {
	globalInstance = rist,
	globalDebugMessenger = rdmsgr } = do
	ist <- readIORef rist
	msgr <- Vk.Ext.DU.Msngr.create ist debugMessengerCreateInfo nil
	writeIORef rdmsgr msgr

debugMessengerCreateInfo :: Vk.Ext.DU.Msngr.CreateInfo () () () () () ()
debugMessengerCreateInfo = Vk.Ext.DU.Msngr.CreateInfo {
	Vk.Ext.DU.Msngr.createInfoNext = Nothing,
	Vk.Ext.DU.Msngr.createInfoFlags = Vk.Ext.DU.Msngr.CreateFlagsZero,
	Vk.Ext.DU.Msngr.createInfoMessageSeverity =
		Vk.Ext.DU.Msg.SeverityVerboseBit .|.
		Vk.Ext.DU.Msg.SeverityWarningBit .|.
		Vk.Ext.DU.Msg.SeverityErrorBit,
	Vk.Ext.DU.Msngr.createInfoMessageType =
		Vk.Ext.DU.Msg.TypeGeneralBit .|.
		Vk.Ext.DU.Msg.TypeValidationBit .|.
		Vk.Ext.DU.Msg.TypePerformanceBit,
	Vk.Ext.DU.Msngr.createInfoFnUserCallback = debugCallback,
	Vk.Ext.DU.Msngr.createInfoUserData = Nothing }

debugCallback :: Vk.Ext.DU.Msngr.FnCallback () () () () ()
debugCallback _messageSeverity _messageType callbackData _userData = do
	let	message = Vk.Ext.DU.Msngr.callbackDataMessage callbackData
	Txt.putStrLn $ "validation layer: " <> message
	pure False

createSurface :: Global -> IO ()
createSurface Global {
	globalWindow = win, globalInstance = rist, globalSurface = rsfc } = do
	ist <- readIORef rist
	writeIORef rsfc =<< Glfw.createWindowSurface @() ist win Nothing

pickPhysicalDevice :: Global -> IO ()
pickPhysicalDevice g@Global {
	globalInstance = rist, globalPhysicalDevice = rpdvc } = do
	devices <- Vk.PhysicalDevice.enumerate . Vk.Ist.T.I =<< readIORef rist
	findM (isDeviceSuitable g) devices >>= \case
		Just pd -> writeIORef rpdvc pd
		Nothing -> error "no matched physical devices"

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (x : xs) = bool (findM p xs) (pure $ Just x) =<< p x

isDeviceSuitable :: Global -> Vk.PhysicalDevice.P -> IO Bool
isDeviceSuitable g dvc = do
	putStrLn "*** IS DEVICE SUITABLE ***"
	print =<< Vk.PhysicalDevice.getProperties dvc
	print =<< Vk.PhysicalDevice.getFeatures dvc

	(&&) <$> (isComplete <$> findQueueFamilies g dvc) <*> (
		checkDeviceExtensionSupport dvc >>= bool (pure False)
			((<$> querySwapChainSupport g dvc) \scs ->
				nn (swapChainSupportDetailsFormats scs) &&
				nn (swapChainSupportDetailsPresentModes scs)) )

nn :: [a] -> Bool
nn = not . null

deviceExtensions :: [Txt.Text]
deviceExtensions = [Vk.Khr.Sc.extensionName]

checkDeviceExtensionSupport :: Vk.PhysicalDevice.P -> IO Bool
checkDeviceExtensionSupport dvc = do
	availableExtensions <-
		Vk.PhysicalDevice.enumerateExtensionProperties dvc Nothing
	let	requiredExtensions = Set.fromList deviceExtensions
	pure . Set.null . foldr Set.delete requiredExtensions
		$ Vk.extensionPropertiesExtensionName <$> availableExtensions

data QueueFamilyIndices = QueueFamilyIndices {
	graphicsFamily :: Maybe Word32,
	presentFamily :: Maybe Word32 }
	deriving Show

isComplete :: QueueFamilyIndices -> Bool
isComplete QueueFamilyIndices {
	graphicsFamily = gf, presentFamily = pf } = isJust gf && isJust pf

findQueueFamilies :: Global -> Vk.PhysicalDevice.P -> IO QueueFamilyIndices
findQueueFamilies Global { globalSurface = rsfc } dvc = do
	putStrLn "*** FIND QUEUE FAMILIES BEGIN ***"
	props <- Vk.PhysicalDevice.getQueueFamilyProperties' dvc
	print props
	sfc <- readIORef rsfc
	pfi <- getPresentFamilyIndex sfc dvc (fromIntegral $ length props) 0
	putStrLn "*** FIND QUEUE FAMILIES END ***"
	props' <- Vk.PhysicalDevice.getQueueFamilyProperties' dvc
	pure QueueFamilyIndices {
		graphicsFamily = fromIntegral
			<$> findIndex checkGraphicsBit props',
		presentFamily = pfi }

getPresentFamilyIndex ::
	Vk.Khr.Sfc.S -> Vk.PhysicalDevice.P -> Word32 -> Word32 -> IO (Maybe Word32)
getPresentFamilyIndex sfc pd n i
	| i >= n = pure Nothing
	| otherwise = do
		presentSupport <- Vk.Khr.Sfc.PhysicalDevice.getSupport pd i sfc
		if presentSupport
			then pure $ Just i
			else getPresentFamilyIndex sfc pd n (i + 1)

checkGraphicsBit :: Vk.QueueFamily.Properties -> Bool
checkGraphicsBit prop = let
	Vk.Queue.FlagBits b = Vk.QueueFamily.propertiesQueueFlags prop in
	b .&. queueGraphicsBit /= 0

data SwapChainSupportDetails = SwapChainSupportDetails {
	swapChainSupportDetailsCapabilities :: Vk.Khr.Sfc.Capabilities,
	swapChainSupportDetailsFormats :: [Vk.Khr.Sfc.Format],
	swapChainSupportDetailsPresentModes :: [Vk.Khr.PresentMode] }
	deriving Show

querySwapChainSupport ::
	Global -> Vk.PhysicalDevice.P -> IO SwapChainSupportDetails
querySwapChainSupport Global { globalSurface = rsfc } dvc =
	readIORef rsfc >>= \sfc -> SwapChainSupportDetails
		<$> Vk.Khr.Sfc.PhysicalDevice.getCapabilities dvc sfc
		<*> Vk.Khr.Sfc.PhysicalDevice.getFormats dvc sfc
		<*> Vk.Khr.Sfc.PhysicalDevice.getPresentModes dvc sfc

createLogicalDevice :: Global -> IO ()
createLogicalDevice g@Global {
	globalPhysicalDevice = rphdvc,
	globalDevice = rdvc,
	globalGraphicsQueue = rgq,
	globalPresentQueue = rpq } = do
	phdvc <- readIORef rphdvc
	indices <- findQueueFamilies g phdvc
	let	uniqueQueueFamilies = Set.fromList [
			fromJust $ graphicsFamily indices,
			fromJust $ presentFamily indices ]
		queueCreateInfo qf = Vk.Device.Queue.CreateInfo {
			Vk.Device.Queue.createInfoNext = Nothing,
			Vk.Device.Queue.createInfoFlags =
				Vk.Device.Queue.CreateFlagsZero,
			Vk.Device.Queue.createInfoQueueFamilyIndex = qf,
			Vk.Device.Queue.createInfoQueuePriorities = [1.0] }
		deviceFeatures = Vk.PhysicalDevice.featuresZero
		createInfo = Vk.Device.CreateInfo {
			Vk.Device.createInfoNext = Nothing,
			Vk.Device.createInfoFlags = Vk.Device.CreateFlagsZero,
			Vk.Device.createInfoQueueCreateInfos =
				((: []) . queueCreateInfo . Vk.QueueFamily.Index) `foldMap`
					uniqueQueueFamilies,
			Vk.Device.createInfoEnabledLayerNames =
				if enableValidationLayers
					then validationLayers else [],
			Vk.Device.createInfoEnabledExtensionNames =
				deviceExtensions,
			Vk.Device.createInfoEnabledFeatures =
				Just deviceFeatures }
	dvc <- Vk.Device.create @() @() @() phdvc createInfo Nothing
	writeIORef rdvc dvc
	gq <- Vk.Device.getQueue dvc (fromJust $ graphicsFamily indices) 0
	pq <- Vk.Device.getQueue dvc (fromJust $ presentFamily indices) 0
	writeIORef rgq gq
	writeIORef rpq pq

createSwapChain :: Global -> IO ()
createSwapChain g@Global {
	globalPhysicalDevice = rphdvc,
	globalDevice = rdvc,
	globalSurface = rsfc,
	globalSwapChain = rsc,
	globalSwapChainImages = rscimgs,
	globalSwapChainImageFormat = rscimgfmt,
	globalSwapChainExtent = rscex } = do
	putStrLn "*** CREATE SWAP CHAIN ***"
	dvc <- readIORef rdvc
	sfc <- readIORef rsfc
	(indices, scs) <- readIORef rphdvc >>= \phdvc -> (,)
		<$> findQueueFamilies g phdvc
		<*> querySwapChainSupport g phdvc
	let	surfaceFormat = chooseSwapSurfaceFormat
			$ swapChainSupportDetailsFormats scs
		presentMode = chooseSwapPresentMode
			$ swapChainSupportDetailsPresentModes scs
		cap = swapChainSupportDetailsCapabilities scs
	extent <- chooseSwapExtent g cap
	let	minImageCount = Vk.Khr.Sfc.capabilitiesMinImageCount cap
		maxImageCount = Vk.Khr.Sfc.capabilitiesMaxImageCount cap
		imageCount = if maxImageCount > 0
			then min (minImageCount + 1) maxImageCount
			else minImageCount + 1
		(smode, fis) = if graphicsFamily indices /= presentFamily indices
			then (Vk.SharingModeConcurrent, [
				fromJust $ graphicsFamily indices,
				fromJust $ presentFamily indices ])
			else (Vk.SharingModeExclusive, [])
		createInfo = Vk.Khr.Sc.CreateInfo {
			Vk.Khr.Sc.createInfoNext = Nothing,
			Vk.Khr.Sc.createInfoFlags = Vk.Khr.Sc.CreateFlagsZero,
			Vk.Khr.Sc.createInfoSurface = sfc,
			Vk.Khr.Sc.createInfoMinImageCount = imageCount,
			Vk.Khr.Sc.createInfoImageFormat =
				Vk.Khr.Sfc.formatFormat surfaceFormat,
			Vk.Khr.Sc.createInfoImageColorSpace =
				Vk.Khr.Sfc.formatColorSpace surfaceFormat,
			Vk.Khr.Sc.createInfoImageExtent = extent,
			Vk.Khr.Sc.createInfoImageArrayLayers = 1,
			Vk.Khr.Sc.createInfoImageUsage =
				Vk.Img.UsageColorAttachmentBit,
			Vk.Khr.Sc.createInfoImageSharingMode = smode,
			Vk.Khr.Sc.createInfoQueueFamilyIndices = fis,
			Vk.Khr.Sc.createInfoPreTransform =
				Vk.Khr.Sfc.capabilitiesCurrentTransform
					$ swapChainSupportDetailsCapabilities scs,
			Vk.Khr.Sc.createInfoCompositeAlpha =
				Vk.Khr.CompositeAlphaOpaqueBit,
			Vk.Khr.Sc.createInfoPresentMode = presentMode,
			Vk.Khr.Sc.createInfoClipped = True,
			Vk.Khr.Sc.createInfoOldSwapchain = Nothing }
	sc <- Vk.Khr.Sc.create @() @() dvc createInfo Nothing
	writeIORef rsc sc
	writeIORef rscimgs =<< Vk.Khr.Sc.getImages dvc sc
	writeIORef rscimgfmt $ Vk.Khr.Sfc.formatFormat surfaceFormat
	writeIORef rscex extent

chooseSwapSurfaceFormat :: [Vk.Khr.Sfc.Format] -> Vk.Khr.Sfc.Format
chooseSwapSurfaceFormat availableFormats = head availableFormats `fromMaybe`
	(`find` availableFormats) \f ->
		Vk.Khr.Sfc.formatFormat f == Vk.Format.B8g8r8a8Srgb &&
		Vk.Khr.Sfc.formatColorSpace f == Vk.Khr.ColorSpaceSrgbNonlinear

chooseSwapPresentMode :: [Vk.Khr.PresentMode] -> Vk.Khr.PresentMode
chooseSwapPresentMode =
	fromMaybe Vk.Khr.PresentModeFifo . find (== Vk.Khr.PresentModeMailbox)

chooseSwapExtent :: Global -> Vk.Khr.Sfc.Capabilities -> IO Vk.C.Extent2d
chooseSwapExtent Global { globalWindow = win } capabilities =
	if Vk.C.extent2dWidth ce /= uint32Max then pure ce else do
		(fromIntegral -> w, fromIntegral -> h) <-
			GlfwB.getFramebufferSize win
		pure $ Vk.C.Extent2d
			(clamp w (Vk.C.extent2dWidth n) (Vk.C.extent2dHeight n))
			(clamp h (Vk.C.extent2dWidth x) (Vk.C.extent2dHeight x))
	where
	ce = Vk.Khr.Sfc.capabilitiesCurrentExtent capabilities
	n = Vk.Khr.Sfc.capabilitiesMinImageExtent capabilities
	x = Vk.Khr.Sfc.capabilitiesMaxImageExtent capabilities

clamp :: Ord a => a -> a -> a -> a
clamp x mn mx | x < mn = mn | x < mx = x | otherwise = mx

createImageViews :: Global -> IO ()
createImageViews g@Global {
	globalSwapChainImages = rscimgs, globalSwapChainImageViews = rscivs } =
	writeIORef rscivs =<< (createImageView1 g `mapM`) =<< readIORef rscimgs

createImageView1 :: Global -> Vk.Img.I -> IO Vk.ImageView.I
createImageView1 Global {
	globalDevice = rdvc, globalSwapChainImageFormat = rscimgfmt } img = do
	(dvc, fmt) <- (,) <$> readIORef rdvc <*> readIORef rscimgfmt
	let	createInfo = Vk.ImageView.CreateInfo {
			Vk.ImageView.createInfoNext = Nothing,
			Vk.ImageView.createInfoFlags =
				Vk.ImageView.CreateFlagsZero,
			Vk.ImageView.createInfoImage = img,
			Vk.ImageView.createInfoViewType = Vk.ImageView.Type2d,
			Vk.ImageView.createInfoFormat = fmt,
			Vk.ImageView.createInfoComponents = cmp,
			Vk.ImageView.createInfoSubresourceRange = srr }
		cmp = Vk.Component.Mapping {
			Vk.Component.mappingR = Vk.Component.SwizzleIdentity,
			Vk.Component.mappingG = Vk.Component.SwizzleIdentity,
			Vk.Component.mappingB = Vk.Component.SwizzleIdentity,
			Vk.Component.mappingA = Vk.Component.SwizzleIdentity }
		srr = Vk.Img.SubresourceRange {
			Vk.Img.subresourceRangeAspectMask =
				Vk.Img.AspectColorBit,
			Vk.Img.subresourceRangeBaseMipLevel = 0,
			Vk.Img.subresourceRangeLevelCount = 1,
			Vk.Img.subresourceRangeBaseArrayLayer = 0,
			Vk.Img.subresourceRangeLayerCount = 1 }
	Vk.ImageView.create @() @() dvc createInfo Nothing

createRenderPass :: Global -> IO ()
createRenderPass Global {
	globalDevice = rdvc,
	globalSwapChainImageFormat = rscimgfmt,
	globalRenderPass = rrp } = do
	scifscif <- readIORef rscimgfmt
	let	colorAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = Vk.Att.DescriptionFlagsZero,
			Vk.Att.descriptionFormat = scifscif,
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
			Vk.Att.referenceAttachment = Vk.Att.A 0,
			Vk.Att.referenceLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
		subpass = Vk.Subpass.Description {
			Vk.Subpass.descriptionFlags =
				Vk.Subpass.DescriptionFlagsZero,
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
				Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.Subpass.dependencySrcAccessMask = Vk.AccessFlagsZero,
			Vk.Subpass.dependencyDstStageMask =
				Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.Subpass.dependencyDstAccessMask =
				Vk.AccessColorAttachmentWriteBit,
			Vk.Subpass.dependencyDependencyFlags =
				Vk.DependencyFlagsZero }
		renderPassInfo = Vk.RndrPss.CreateInfo {
			Vk.RndrPss.createInfoNext = Nothing,
			Vk.RndrPss.createInfoFlags = Vk.RndrPss.CreateFlagsZero,
			Vk.RndrPss.createInfoAttachments = [colorAttachment],
			Vk.RndrPss.createInfoSubpasses = [subpass],
			Vk.RndrPss.createInfoDependencies = [dependency] }
	dvc <- readIORef rdvc
	rp <- Vk.RndrPss.create @() @() dvc renderPassInfo Nothing
	writeIORef rrp rp

createGraphicsPipeline :: Global -> IO ()
createGraphicsPipeline g@Global {
	globalDevice = rdvc,
	globalSwapChainExtent = rscex,
	globalPipelineLayout = rPplLyt,
	globalRenderPass = rrp,
	globalGraphicsPipeline = rgpl } = do
	dvc <- readIORef rdvc
	vertShaderModule <- createShaderModule g glslVertexShaderMain
	fragShaderModule <- createShaderModule g glslFragmentShaderMain
	sce <- readIORef rscex
	let	vertShaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
			Vk.Ppl.ShaderStage.createInfoNext = Nothing,
			Vk.Ppl.ShaderStage.createInfoFlags =
				Vk.Ppl.ShaderStage.CreateFlagsZero,
			Vk.Ppl.ShaderStage.createInfoStage =
				Vk.ShaderStageVertexBit,
			Vk.Ppl.ShaderStage.createInfoModule = vertShaderModule,
			Vk.Ppl.ShaderStage.createInfoName = "main",
			Vk.Ppl.ShaderStage.createInfoSpecializationInfo =
				Nothing }
		fragShaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
			Vk.Ppl.ShaderStage.createInfoNext = Nothing,
			Vk.Ppl.ShaderStage.createInfoFlags =
				Vk.Ppl.ShaderStage.CreateFlagsZero,
			Vk.Ppl.ShaderStage.createInfoStage =
				Vk.ShaderStageFragmentBit,
			Vk.Ppl.ShaderStage.createInfoModule = fragShaderModule,
			Vk.Ppl.ShaderStage.createInfoName = "main",
			Vk.Ppl.ShaderStage.createInfoSpecializationInfo =
				Nothing }
		vertexInputInfo :: Vk.Ppl.VI.CreateInfo () () '[]
		vertexInputInfo = Vk.Ppl.VI.CreateInfo {
			Vk.Ppl.VI.createInfoNext = Nothing,
			Vk.Ppl.VI.createInfoFlags =
				Vk.Ppl.VI.M.CreateFlagsZero }
		inputAssembly = Vk.Ppl.IA.CreateInfo {
			Vk.Ppl.IA.createInfoNext = Nothing,
			Vk.Ppl.IA.createInfoFlags = Vk.Ppl.IA.CreateFlagsZero,
			Vk.Ppl.IA.createInfoTopology =
				Vk.PrimitiveTopologyTriangleList,
			Vk.Ppl.IA.createInfoPrimitiveRestartEnable = False }
		viewport = Vk.C.Viewport {
			Vk.C.viewportX = 0, Vk.C.viewportY = 0,
			Vk.C.viewportWidth = fromIntegral $ Vk.C.extent2dWidth sce,
			Vk.C.viewportHeight = fromIntegral $ Vk.C.extent2dHeight sce,
			Vk.C.viewportMinDepth = 0, Vk.C.viewportMaxDepth = 1 }
		scissor = Vk.C.Rect2d {
			Vk.C.rect2dOffset = Vk.C.Offset2d {
				Vk.C.offset2dX = 0, Vk.C.offset2dY = 0 },
			Vk.C.rect2dExtent = sce }
		viewportState = Vk.Ppl.VP.CreateInfo {
			Vk.Ppl.VP.createInfoNext = Nothing,
			Vk.Ppl.VP.createInfoFlags = Vk.Ppl.VP.CreateFlagsZero,
			Vk.Ppl.VP.createInfoViewports = [viewport],
			Vk.Ppl.VP.createInfoScissors = [scissor] }
		rasterizer = Vk.Ppl.RstSt.CreateInfo {
			Vk.Ppl.RstSt.createInfoNext = Nothing,
			Vk.Ppl.RstSt.createInfoFlags =
				Vk.Ppl.RstSt.CreateFlagsZero,
			Vk.Ppl.RstSt.createInfoDepthClampEnable = False,
			Vk.Ppl.RstSt.createInfoRasterizerDiscardEnable = False,
			Vk.Ppl.RstSt.createInfoPolygonMode = Vk.PolygonModeFill,
			Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeBackBit,
			Vk.Ppl.RstSt.createInfoFrontFace =
				Vk.FrontFaceClockwise,
			Vk.Ppl.RstSt.createInfoDepthBiasEnable = False,
			Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
			Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
			Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0,
			Vk.Ppl.RstSt.createInfoLineWidth = 1 }
		multisampling = Vk.Ppl.MS.CreateInfo {
			Vk.Ppl.MS.createInfoNext = Nothing,
			Vk.Ppl.MS.createInfoFlags = Vk.Ppl.MS.CreateFlagsZero,
			Vk.Ppl.MS.createInfoRasterizationSamplesAndMask =
				Vk.Sample.CountAndMask
					Vk.Sample.Count1Bit Nothing,
			Vk.Ppl.MS.createInfoSampleShadingEnable = False,
			Vk.Ppl.MS.createInfoMinSampleShading = 1,
			Vk.Ppl.MS.createInfoAlphaToCoverageEnable = False,
			Vk.Ppl.MS.createInfoAlphaToOneEnable = False }
		colorBlendAttachment = Vk.Ppl.CBA.State {
			Vk.Ppl.CBA.stateBlendEnable = False,
			Vk.Ppl.CBA.stateSrcColorBlendFactor = Vk.BlendFactorOne,
			Vk.Ppl.CBA.stateDstColorBlendFactor =
				Vk.BlendFactorZero,
			Vk.Ppl.CBA.stateColorBlendOp = Vk.BlendOpAdd,
			Vk.Ppl.CBA.stateSrcAlphaBlendFactor = Vk.BlendFactorOne,
			Vk.Ppl.CBA.stateDstAlphaBlendFactor =
				Vk.BlendFactorZero,
			Vk.Ppl.CBA.stateAlphaBlendOp = Vk.BlendOpAdd,
			Vk.Ppl.CBA.stateColorWriteMask =
				Vk.CC.RBit .|. Vk.CC.GBit .|. Vk.CC.BBit .|.
				Vk.CC.ABit }
		colorBlending = Vk.Ppl.CB.CreateInfo {
			Vk.Ppl.CB.createInfoNext = Nothing,
			Vk.Ppl.CB.createInfoFlags = Vk.Ppl.CB.CreateFlagsZero,
			Vk.Ppl.CB.createInfoLogicOpEnable = False,
			Vk.Ppl.CB.createInfoLogicOp = Vk.LogicOpCopy,
			Vk.Ppl.CB.createInfoAttachments =
				[colorBlendAttachment],
			Vk.Ppl.CB.createInfoBlendConstants = fromJust $ rgbaDouble 0 0 0 0 }
		pipelineLayoutInfo = Vk.Ppl.Lyt.CreateInfo {
			Vk.Ppl.Lyt.createInfoNext = Nothing,
			Vk.Ppl.Lyt.createInfoFlags = Vk.Ppl.Lyt.CreateFlagsZero,
			Vk.Ppl.Lyt.createInfoSetLayouts = [],
			Vk.Ppl.Lyt.createInfoPushConstantRanges = [] }
	pipelineLayout <- Vk.Ppl.Lyt.create @() @() dvc pipelineLayoutInfo Nothing
	writeIORef rPplLyt pipelineLayout
	rp <- readIORef rrp
	let	pipelineInfo :: Vk.Ppl.CreateInfo
			() () '[ 'GlslVertexShader, 'GlslFragmentShader] '[(), ()] () () '[] () () () () () () () () () '[]
		pipelineInfo = Vk.Ppl.CreateInfo {
			Vk.Ppl.createInfoNext = Nothing,
			Vk.Ppl.createInfoFlags = Vk.Ppl.CreateFlagsZero,
			Vk.Ppl.createInfoStages =
				vertShaderStageInfo `Vk.Ppl.ShaderStage.CreateInfoCons`
				fragShaderStageInfo `Vk.Ppl.ShaderStage.CreateInfoCons`
				Vk.Ppl.ShaderStage.CreateInfoNil,
			Vk.Ppl.createInfoVertexInputState = Just vertexInputInfo,
			Vk.Ppl.createInfoInputAssemblyState = Just inputAssembly,
			Vk.Ppl.createInfoTessellationState = Nothing,
			Vk.Ppl.createInfoViewportState = Just viewportState,
			Vk.Ppl.createInfoRasterizationState = Just rasterizer,
			Vk.Ppl.createInfoMultisampleState = Just multisampling,
			Vk.Ppl.createInfoDepthStencilState = Nothing,
			Vk.Ppl.createInfoColorBlendState = Just colorBlending,
			Vk.Ppl.createInfoDynamicState = Nothing,	
			Vk.Ppl.createInfoLayout = pipelineLayout,
			Vk.Ppl.createInfoRenderPass = rp,
			Vk.Ppl.createInfoSubpass = 0,
			Vk.Ppl.createInfoBasePipelineHandle = Vk.Ppl.GNull,
			Vk.Ppl.createInfoBasePipelineIndex = - 1 }
	gpl `Vk.Ppl.PCons` Vk.Ppl.PNil <-
		Vk.Ppl.create @_ @_ @_ @_ @_ @_ @_ @_ @_ @_ @_ @_ @_ @_ @_ @_ @_ @()
			dvc Nothing
			(pipelineInfo `Vk.Ppl.CreateInfoCons` Vk.Ppl.CreateInfoNil)
			Nothing
	writeIORef rgpl gpl
	Vk.Shader.Module.destroy @() dvc fragShaderModule Nothing
	Vk.Shader.Module.destroy @() dvc vertShaderModule Nothing

createShaderModule :: Global -> Spv sknd -> IO (Vk.Shader.Module.M sknd)
createShaderModule Global { globalDevice = rdvc } cd = do
	dvc <- readIORef rdvc
	let	createInfo = Vk.Shader.Module.CreateInfo {
			Vk.Shader.Module.createInfoNext = Nothing,
			Vk.Shader.Module.createInfoFlags =
				Vk.Shader.Module.CreateFlagsZero,
			Vk.Shader.Module.createInfoCode = cd }
	Vk.Shader.Module.create @() @() dvc createInfo Nothing

readFromByteString :: BS.ByteString -> IO (Ptr Word32, Integer)
readFromByteString (BS.PS f o l) = do
	p' <- mallocBytes l
	withForeignPtr f \p -> copyBytes p' (p `plusPtr` o) l
	pure (p', fromIntegral l)

createFramebuffers :: Global -> IO ()
createFramebuffers g@Global {
	globalSwapChainImageViews = rscivs,
	globalSwapChainFramebuffers = rscfbs } =
	writeIORef rscfbs =<< (createFramebuffer1 g `mapM`) =<< readIORef rscivs

createFramebuffer1 :: Global -> Vk.ImageView.I -> IO Vk.Fb.F
createFramebuffer1 Global {
	globalDevice = rdvc,
	globalSwapChainExtent = rscex,
	globalRenderPass = rrp } attachment = do
	dvc <- readIORef rdvc
	rndrPss <- readIORef rrp
	sce <- readIORef rscex
	let	framebufferInfo = Vk.Fb.CreateInfo {
			Vk.Fb.createInfoNext = Nothing,
			Vk.Fb.createInfoFlags = Vk.Fb.CreateFlagsZero,
			Vk.Fb.createInfoRenderPass = rndrPss,
			Vk.Fb.createInfoAttachments = [attachment],
			Vk.Fb.createInfoWidth = Vk.C.extent2dWidth sce,
			Vk.Fb.createInfoHeight = Vk.C.extent2dHeight sce,
			Vk.Fb.createInfoLayers = 1 }
	Vk.Fb.create @() @() dvc framebufferInfo Nothing

createCommandPool :: Global -> IO ()
createCommandPool g@Global {
	globalPhysicalDevice = rpdvc,
	globalDevice = rdvc, globalCommandPool = rcp } = do
	queueFamilyIndices <- findQueueFamilies g =<< readIORef rpdvc
	let	poolInfo = Vk.CP.CreateInfo {
			Vk.CP.createInfoNext = Nothing,
			Vk.CP.createInfoFlags =
				Vk.CP.CreateResetCommandBufferBit,
			Vk.CP.createInfoQueueFamilyIndex =
				Vk.QueueFamily.Index . fromJust $ graphicsFamily queueFamilyIndices }
	dvc <- readIORef rdvc
	writeIORef rcp =<< Vk.CP.create @() @() dvc poolInfo Nothing

createCommandBuffers :: Global -> IO ()
createCommandBuffers Global {
	globalDevice = rdvc, globalSwapChainFramebuffers = rscfbs,
	globalCommandPool = rcp, globalCommandBuffers = rcbs } = do
	dvc <- readIORef rdvc
	scfbs <- readIORef rscfbs
	cp <- readIORef rcp
	cbs <- Vk.CB.allocate @() dvc Vk.CB.AllocateInfo {
		Vk.CB.allocateInfoNext = Nothing,
		Vk.CB.allocateInfoCommandPool = cp,
		Vk.CB.allocateInfoLevel = Vk.CB.LevelPrimary,
		Vk.CB.allocateInfoCommandBufferCount = genericLength scfbs }
	writeIORef rcbs cbs

recordCommandBuffer :: Global -> Vk.CB.C () -> Vk.Fb.F -> IO ()
recordCommandBuffer Global {
	globalSwapChainExtent = rscex, globalRenderPass = rrp,
	globalGraphicsPipeline = rppl } cb fb = do
	Vk.CB.begin @() @() cb Vk.CB.BeginInfo {
		Vk.CB.beginInfoNext = Nothing,
		Vk.CB.beginInfoFlags = Vk.CB.UsageFlagsZero,
		Vk.CB.beginInfoInheritanceInfo = Nothing }
	rp <- readIORef rrp
	sce <- readIORef rscex
	let	renderPassInfo = Vk.RndrPss.BeginInfo {
			Vk.RndrPss.beginInfoNext = Nothing,
			Vk.RndrPss.beginInfoRenderPass = rp,
			Vk.RndrPss.beginInfoFramebuffer = fb,
			Vk.RndrPss.beginInfoRenderArea = Vk.C.Rect2d {
				Vk.C.rect2dOffset = Vk.C.Offset2d 0 0,
				Vk.C.rect2dExtent = sce },
			Vk.RndrPss.beginInfoClearValues = [
				Vk.ClearValueColor
					. fromJust $ rgbaDouble 0 0 0 1 ] }
	Vk.Cmd.M.beginRenderPass @()
		@('Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32)
		cb renderPassInfo Vk.Subpass.ContentsInline
	Vk.Cmd.M.bindPipeline cb Vk.Ppl.BindPointGraphics =<< readIORef rppl
	Vk.Cmd.M.draw cb 3 1 0 0
	Vk.Cmd.M.endRenderPass cb
	Vk.CB.end cb

createSyncObjects :: Global -> IO ()
createSyncObjects Global {
	globalDevice = rdvc,
	globalImageAvailableSemaphore = rias,
	globalRenderFinishedSemaphore = rrfs,
	globalInFlightFence = riff } = do
	let	semaphoreInfo = Vk.Smp.CreateInfo {
			Vk.Smp.createInfoNext = Nothing,
			Vk.Smp.createInfoFlags = Vk.Smp.CreateFlagsZero }
		fenceInfo = Vk.Fnc.CreateInfo {
			Vk.Fnc.createInfoNext = Nothing,
			Vk.Fnc.createInfoFlags = Vk.Fnc.CreateSignaledBit }
	dvc <- readIORef rdvc
	writeIORef rias =<< Vk.Smp.create @() @() dvc semaphoreInfo Nothing
	writeIORef rrfs =<< Vk.Smp.create @() @() dvc semaphoreInfo Nothing
	writeIORef riff =<< Vk.Fnc.create @() @() dvc fenceInfo Nothing

mainLoop :: Global -> IO ()
mainLoop g@Global { globalWindow = win, globalDevice = rdvc } = do
	fix \loop -> bool (pure ()) loop =<< do
		GlfwB.pollEvents
		drawFrame g
		not <$> GlfwB.windowShouldClose win
	Vk.Device.waitIdle =<< readIORef rdvc

drawFrame :: Global -> IO ()
drawFrame g@Global {
	globalDevice = rdvc,
	globalSwapChainFramebuffers = rscfbs,
	globalGraphicsQueue = rgq,
	globalPresentQueue = rpq,
	globalSwapChain = rsc,
	globalCommandBuffers = rcbs,
	globalImageAvailableSemaphore = rias,
	globalRenderFinishedSemaphore = rrfs,
	globalInFlightFence = riff } = do
	dvc <- readIORef rdvc
	iff <- readIORef riff
	ias <- readIORef rias
	sc <- readIORef rsc
	scfbs <- readIORef rscfbs
	cbs <- readIORef rcbs
	rfs <- readIORef rrfs
	gq <- readIORef rgq
	pq <- readIORef rpq
	Vk.Fnc.waitForFs dvc [iff] True maxBound
	Vk.Fnc.resetFs dvc [iff]
	(fromIntegral -> imageIndex) <-
		Vk.Khr.acquireNextImage dvc sc uint64Max (Just ias) Nothing
	(`Vk.CB.reset` Vk.CB.ResetFlagsZero) `mapM_` cbs
	uncurry (recordCommandBuffer g) `mapM_` zip cbs scfbs
	let	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks =
				[(ias, Vk.Ppl.StageColorAttachmentOutputBit)],
			Vk.submitInfoCommandBuffers = [cbs !! imageIndex],
			Vk.submitInfoSignalSemaphores = [rfs] }
	Vk.Queue.submit' @() gq [submitInfo] $ Just iff
	let	presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = Nothing,
			Vk.Khr.presentInfoWaitSemaphores = [rfs],
			Vk.Khr.presentInfoSwapchainImageIndices =
				[(sc, fromIntegral imageIndex)] }
	Vk.Khr.queuePresent @() pq presentInfo
	Vk.Queue.waitIdle pq

cleanup :: Global -> IO ()
cleanup Global {
	globalWindow = win,
	globalInstance = rist,
	globalDebugMessenger = rdmsgr,
	globalDevice = rdvc,
	globalSurface = rsfc,
	globalSwapChain = rsc,
	globalSwapChainImageViews = rscivs,
	globalPipelineLayout = rPplLyt,
	globalRenderPass = rrp,
	globalGraphicsPipeline = rgpl,
	globalSwapChainFramebuffers = rscfbs,
	globalCommandPool = rcp,
	globalImageAvailableSemaphore = rias,
	globalRenderFinishedSemaphore = rrfs,
	globalInFlightFence = riff } = do
	dvc <- readIORef rdvc
	ias <- readIORef rias
	rfs <- readIORef rrfs
	iff <- readIORef riff
	Vk.Smp.destroy @() dvc ias Nothing
	Vk.Smp.destroy @() dvc rfs Nothing
	Vk.Fnc.destroy @() dvc iff Nothing
	cp <- readIORef rcp
	Vk.CP.destroy @() dvc cp Nothing
	fbs <- readIORef rscfbs
	(\fb -> Vk.Fb.destroy @() dvc fb Nothing) `mapM_` fbs
	gppl <- readIORef rgpl
	Vk.Ppl.destroy @() dvc gppl Nothing
	pl <- readIORef rPplLyt
	Vk.Ppl.Lyt.destroy @() dvc pl Nothing
	rp <- readIORef rrp
	Vk.RndrPss.destroy @() dvc rp Nothing
	((\iv -> Vk.ImageView.destroy @() dvc iv Nothing) `mapM_`)
		=<< readIORef rscivs
	(\sc -> Vk.Khr.Sc.destroy @() dvc sc Nothing) =<< readIORef rsc
	Vk.Device.destroy @() dvc Nothing
	ist <- readIORef rist
	(\sfc -> Vk.Khr.Sfc.destroy @() ist sfc Nothing) =<< readIORef rsfc
	when enableValidationLayers $ readIORef rdmsgr >>= \dm ->
		Vk.Ext.DU.Msngr.destroy ist dm nil
	Vk.Ist.destroy @() ist Nothing
	GlfwB.destroyWindow win
	GlfwB.terminate

[glslVertexShader|

#version 450

layout(location = 0) out vec3 fragColor;

vec2 positions[3] = vec2[](
	vec2(0.0, -0.5),
	vec2(0.5, 0.5),
	vec2(-0.5, 0.5) );

vec3 colors[3] = vec3[](
	vec3(1.0, 0.0, 0.0),
	vec3(0.0, 1.0, 0.0),
	vec3(0.0, 0.0, 1.0) );

void
main()
{
	gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
	fragColor = colors[gl_VertexIndex];
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
