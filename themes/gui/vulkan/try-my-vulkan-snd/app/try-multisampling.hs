{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Generics
import GHC.Tuple
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Exception
import Data.Foldable
import Data.MonoTraversable
import Data.Bits
import Data.Bool
import Data.Maybe
import Data.List
import Data.Vector.Storable.Indexing
import Data.Word
import Data.Int
import Data.IORef
import Data.List.Length
import Data.Time
import Data.Color
import Codec.Picture
import System.Environment

import Foreign.Storable.SizeAlignment

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Storable as V
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Graphics.UI.GLFW as GlfwB
import qualified Glfw
import qualified Cglm
import qualified Foreign.Storable.Generic

import ThEnv
import Shaderc
import Shaderc.EnumAuto
import Shaderc.TH

import Vulkan.Base

import qualified Vulkan.Middle as Vk
import qualified Vulkan.Core as Vk.C
import qualified Vulkan.Enum as Vk
import qualified Vulkan.Exception as Vk
import qualified Vulkan.Exception.Enum as Vk
import qualified Vulkan.Instance.Type as Vk.Instance.T
import qualified Vulkan.Instance.Middle as Vk.Instance
import qualified Vulkan.Instance.Enum as Vk.Instance
import qualified Vulkan.Khr as Vk.Khr
import qualified Vulkan.Khr.Enum as Vk.Khr
import qualified Vulkan.Ext.DebugUtils as Vk.Ext.DebugUtils
import qualified Vulkan.Ext.DebugUtils.Messenger as Vk.Ext.DebugUtils.Messenger
import qualified Vulkan.Ext.DebugUtils.Message.Enum as Vk.Ext.DebugUtils.Message
import qualified Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Vulkan.QueueFamily as Vk.QueueFamily
import qualified Vulkan.Device.Middle as Vk.Device
import qualified Vulkan.Device.Queue as Vk.Device.Queue
import qualified Vulkan.Device.Queue.Enum as Vk.Device.Queue
import qualified Vulkan.Khr.Surface as Vk.Khr.Surface
import qualified Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Surface.PhysicalDevice
import qualified Vulkan.Khr.Swapchain as Vk.Khr.Swapchain
import qualified Vulkan.Khr.Swapchain.Enum as Vk.Khr.Swapchain
import qualified Vulkan.Image.Middle as Vk.Image
import qualified Vulkan.Image.Enum as Vk.Image
import qualified Vulkan.ImageView.Middle as Vk.ImageView
import qualified Vulkan.ImageView.Enum as Vk.ImageView
import qualified Vulkan.Component as Vk.Component
import qualified Vulkan.Component.Enum as Vk.Component
import qualified Vulkan.Shader.Module.Middle as Vk.Shader.Module
import qualified Vulkan.Pipeline.ShaderStage.Middle as Vk.Ppl.ShaderStage
import qualified Vulkan.Pipeline.ShaderStage.Enum as Vk.Ppl.ShaderStage
import qualified Vulkan.Shader.Stage.Enum as Vk.Shader.Stage
import qualified Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt
import qualified Vulkan.Pipeline.VertexInputState.Middle as
	Vk.Ppl.VertexInputSt.M
import qualified Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.InpAsmbSt
import qualified Vulkan.Pipeline.ViewportState as Vk.Ppl.ViewportSt
import qualified Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Vulkan.Pipeline.MultisampleState as Vk.Ppl.MulSmplSt
import qualified Vulkan.Sample as Vk.Sample
import qualified Vulkan.Sample.Enum as Vk.Sample
import qualified Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.ClrBlndAtt
import qualified Vulkan.ColorComponent.Enum as Vk.ClrCmp
import qualified Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlndSt
import qualified Vulkan.Pipeline.Layout.Middle as Vk.Ppl.Layout
import qualified Vulkan.Attachment as Vk.Att
import qualified Vulkan.Attachment.Enum as Vk.Att
import qualified Vulkan.Subpass as Vk.Subpass
import qualified Vulkan.Subpass.Enum as Vk.Subpass
import qualified Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Vulkan.RenderPass.Middle as Vk.RenderPass
import qualified Vulkan.RenderPass.Enum as Vk.RenderPass
import qualified Vulkan.Pipeline.Graphics.Middle as Vk.Ppl.Graphics
import qualified Vulkan.Framebuffer.Middle as Vk.Framebuffer
import qualified Vulkan.Framebuffer.Enum as Vk.Framebuffer
import qualified Vulkan.CommandPool.Middle as Vk.CommandPool
import qualified Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified Vulkan.CommandBuffer.Middle as Vk.CommandBuffer
import qualified Vulkan.CommandBuffer.Enum as Vk.CommandBuffer
import qualified Vulkan.Command as Vk.Cmd
import qualified Vulkan.Semaphore as Vk.Semaphore
import qualified Vulkan.Fence as Vk.Fence
import qualified Vulkan.Fence.Enum as Vk.Fence
import qualified Vulkan.VertexInput as Vk.VertexInput
import qualified Vulkan.Buffer.List.Middle as Vk.Buffer.List
import qualified Vulkan.Buffer.Enum as Vk.Buffer
import qualified Vulkan.Memory.List.Middle as Vk.Memory.List
import qualified Vulkan.Memory.Middle as Vk.Memory.M
import qualified Vulkan.Memory.Enum as Vk.Memory
import qualified Vulkan.Command.List as Vk.Cmd.List
import qualified Vulkan.Descriptor.Set.Layout.Middle as Vk.DscSet.Lyt
import qualified Vulkan.Descriptor.Set.Layout.Enum as Vk.DscSet.Lyt
import qualified Vulkan.Buffer.Atom as Vk.Buffer.Atom
import qualified Vulkan.Memory.Atom as Vk.Memory.Atom
import qualified Vulkan.Descriptor.Pool.Middle as Vk.DscPool
import qualified Vulkan.Descriptor.Enum as Vk.Dsc
import qualified Vulkan.Descriptor.Pool.Enum as Vk.DscPool
import qualified Vulkan.Descriptor.Set as Vk.DscSet
import qualified Vulkan.Descriptor as Vk.Dsc
import qualified Vulkan.Memory.Image.Middle as Vk.Memory.Image
import qualified Vulkan.QueueFamily.EnumManual as Vk.QueueFamily
import qualified Vulkan.Buffer.Middle as Vk.Buffer.M
import qualified Vulkan.Sampler as Vk.Sampler
import qualified Vulkan.Sampler.Enum as Vk.Sampler
import qualified Vulkan.PhysicalDevice.Struct as Vk.PhysicalDevice
import qualified Vulkan.Format.Enum as Vk.Format
import qualified Vulkan.Format as Vk.Format
import qualified Vulkan.Pipeline.DepthStencilState as Vk.Ppl.DepthStencilSt
import qualified Vulkan.Queue as Vk.Queue
import qualified Vulkan.Queue.Enum as Vk.Queue
import qualified Vulkan.Memory as Vk.Memory
import qualified Vulkan.Command.Middle as Vk.Cmd.M

import Vulkan.Pipeline.VertexInputState.BindingStrideList(AddType)
import Vulkan.Buffer.List.Middle (BList(..))
import Vertex
import Codec.Wavefront.Read

main :: IO ()
main = getArgs >>= \case
	[tfp, mfp] -> runReaderT (run tfp mfp 0) =<< newGlobal
	[tfp, mfp, mnld] -> runReaderT (run tfp mfp $ read mnld) =<< newGlobal
	_ -> error "bad args"

width, height :: Int
width = 800; height = 600

enableValidationLayers :: Bool
enableValidationLayers =
	maybe True (const False) $(lookupCompileEnvExp "NDEBUG")

validationLayers :: [Txt.Text]
validationLayers = [Vk.Khr.validationLayerName]

maxFramesInFlight :: Int
maxFramesInFlight = 2

data Global = Global {
	globalWindow :: IORef (Maybe GlfwB.Window),
	globalInstance :: IORef Vk.Instance.I,
	globalDebugMessenger :: IORef Vk.Ext.DebugUtils.Messenger,
	globalPhysicalDevice :: IORef Vk.PhysicalDevice.P,
	globalDevice :: IORef Vk.Device.D,
	globalGraphicsQueue :: IORef Vk.Queue.Q,
	globalSurface :: IORef Vk.Khr.Surface.S,
	globalPresentQueue :: IORef Vk.Queue.Q,
	globalSwapChain :: IORef Vk.Khr.Swapchain.S,
	globalSwapChainImages :: IORef [Vk.Image.I],
	globalSwapChainImageFormat :: IORef (Maybe Vk.Format.F),
	globalSwapChainExtent :: IORef Vk.C.Extent2d,
	globalSwapChainImageViews :: IORef [Vk.ImageView.I],
	globalRenderPass :: IORef Vk.RenderPass.R,
	globalDescriptorSetLayout :: IORef Vk.DscSet.Lyt.L,
	globalPipelineLayout :: IORef Vk.Ppl.Layout.L,
	globalGraphicsPipeline :: IORef (Vk.Ppl.Graphics.G
		(Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
		'[Cglm.Vec3, Color, TexCoord]),
	globalSwapChainFramebuffers :: IORef [Vk.Framebuffer.F],
	globalCommandPool :: IORef Vk.CommandPool.C,
	globalCommandBuffers :: IORef [Vk.CommandBuffer.C (
		Solo (AddType Vertex 'Vk.VertexInput.RateVertex) )],
	globalImageAvailableSemaphores :: IORef [Vk.Semaphore.S],
	globalRenderFinishedSemaphores :: IORef [Vk.Semaphore.S],
	globalInFlightFences :: IORef [Vk.Fence.F],
	globalCurrentFrame :: IORef Int,
	globalFramebufferResized :: IORef Bool,
	globalVertexBuffer :: IORef (Vk.Buffer.List.B Vertex),
	globalVertexBufferMemory :: IORef (Vk.Device.MemoryList Vertex),
	globalIndexBuffer :: IORef (Vk.Buffer.List.B Word32),
	globalIndexBufferMemory :: IORef (Vk.Device.MemoryList Word32),
	globalUniformBuffers :: IORef [Vk.Buffer.Atom.B UniformBufferObject],
	globalUniformBuffersMemory ::
		IORef [Vk.Device.MemoryAtom UniformBufferObject],
	globalDescriptorPool :: IORef Vk.DscPool.P,
	globalDescriptorSets :: IORef [Vk.DscSet.S],
	globalMipLevels :: IORef Word32,
	globalTextureImage :: IORef Vk.Image.I,
	globalTextureImageMemory :: IORef Vk.Device.MemoryImage,
	globalTextureImageView :: IORef Vk.ImageView.I,
	globalTextureSampler :: IORef Vk.Sampler.S,
	globalDepthImage :: IORef Vk.Image.I,
	globalDepthImageMemory :: IORef Vk.Device.MemoryImage,
	globalDepthImageView :: IORef Vk.ImageView.I,
	globalTextureFilePath :: IORef FilePath,
	globalModelFilePath :: IORef FilePath,
	globalVertices :: IORef (V.Vector WVertex),
	globalIndices :: IORef (V.Vector WWord32),
	globalMinLod :: IORef Float,
	globalMsaaSamples :: IORef Vk.Sample.CountFlagBits,
	globalColorImage :: IORef Vk.Image.I,
	globalColorImageMemory :: IORef Vk.Device.MemoryImage,
	globalColorImageView :: IORef Vk.ImageView.I
	}

readGlobal :: (Global -> IORef a) -> ReaderT Global IO a
readGlobal ref = lift . readIORef =<< asks ref

writeGlobal :: (Global -> IORef a) -> a -> ReaderT Global IO ()
writeGlobal ref x = lift . (`writeIORef` x) =<< asks ref

newGlobal :: IO Global
newGlobal = do
	win <- newIORef Nothing
	ist <- newIORef $ Vk.Instance.I NullPtr
	dmsgr <- newIORef $ Vk.Ext.DebugUtils.Messenger NullPtr
	pdvc <- newIORef $ Vk.PhysicalDevice.P NullPtr
	dvc <- newIORef $ Vk.Device.D NullPtr
	gq <- newIORef $ Vk.Queue.Q NullPtr
	sfc <- newIORef $ Vk.Khr.Surface.S NullPtr
	pq <- newIORef $ Vk.Queue.Q NullPtr
	sc <- newIORef $ Vk.Khr.Swapchain.S NullPtr
	scis <- newIORef []
	scif <- newIORef Nothing
	sce <- newIORef $ Vk.C.Extent2d 0 0
	scivs <- newIORef []
	rp <- newIORef $ Vk.RenderPass.R NullPtr
	dscstlyt <- newIORef $ Vk.DscSet.Lyt.L NullPtr
	ppllyt <- newIORef $ Vk.Ppl.Layout.L NullPtr
	grppl <- newIORef Vk.Ppl.Graphics.GNull
	scfbs <- newIORef []
	cp <- newIORef $ Vk.CommandPool.C NullPtr
	cbs <- newIORef []
	iass <- newIORef []
	rfss <- newIORef []
	iffs <- newIORef []
	cf <- newIORef 0
	fbr <- newIORef False
	vb <- newIORef $ Vk.Buffer.List.B 0 NullPtr
	vbm <- newIORef $ Vk.Device.MemoryList 0 NullPtr
	ib <- newIORef $ Vk.Buffer.List.B 0 NullPtr
	ibm <- newIORef $ Vk.Device.MemoryList 0 NullPtr
	ubs <- newIORef []
	ubms <- newIORef []
	dp <- newIORef $ Vk.DscPool.P NullPtr
	dss <- newIORef []
	ml <- newIORef 0
	ti <- newIORef $ Vk.Image.I NullPtr
	tim <- newIORef $ Vk.Device.MemoryImage 0 NullPtr
	tiv <- newIORef $ Vk.ImageView.I NullPtr
	ts <- newIORef $ Vk.Sampler.S NullPtr
	di <- newIORef $ Vk.Image.I NullPtr
	dim <- newIORef $ Vk.Device.MemoryImage 0 NullPtr
	divw <- newIORef $ Vk.ImageView.I NullPtr
	tfp <- newIORef ""
	mfp <- newIORef ""
	vtcs <- newIORef V.empty
	idcs <- newIORef V.empty
	mnld <- newIORef 0
	msaaS <- newIORef Vk.Sample.CountFlagsZero
	ci <- newIORef $ Vk.Image.I NullPtr
	cim <- newIORef $ Vk.Device.MemoryImage 0 NullPtr
	civ <- newIORef $ Vk.ImageView.I NullPtr
	pure Global {
		globalWindow = win,
		globalInstance = ist,
		globalDebugMessenger = dmsgr,
		globalPhysicalDevice = pdvc,
		globalDevice = dvc,
		globalGraphicsQueue = gq,
		globalSurface = sfc,
		globalPresentQueue = pq,
		globalSwapChain = sc,
		globalSwapChainImages = scis,
		globalSwapChainImageFormat = scif,
		globalSwapChainExtent = sce,
		globalSwapChainImageViews = scivs,
		globalRenderPass = rp,
		globalDescriptorSetLayout = dscstlyt,
		globalPipelineLayout = ppllyt,
		globalGraphicsPipeline = grppl,
		globalSwapChainFramebuffers = scfbs,
		globalCommandPool = cp,
		globalCommandBuffers = cbs,
		globalImageAvailableSemaphores = iass,
		globalRenderFinishedSemaphores = rfss,
		globalInFlightFences = iffs,
		globalCurrentFrame = cf,
		globalFramebufferResized = fbr,
		globalVertexBuffer = vb,
		globalVertexBufferMemory = vbm,
		globalIndexBuffer = ib,
		globalIndexBufferMemory = ibm,
		globalUniformBuffers = ubs,
		globalUniformBuffersMemory = ubms,
		globalDescriptorPool = dp,
		globalDescriptorSets = dss,
		globalMipLevels = ml,
		globalTextureImage = ti,
		globalTextureImageMemory = tim,
		globalTextureImageView = tiv,
		globalTextureSampler = ts,
		globalDepthImage = di,
		globalDepthImageMemory = dim,
		globalDepthImageView = divw,
		globalTextureFilePath = tfp,
		globalModelFilePath = mfp,
		globalVertices = vtcs,
		globalIndices = idcs,
		globalMinLod = mnld,
		globalMsaaSamples = msaaS,
		globalColorImage = ci,
		globalColorImageMemory = cim,
		globalColorImageView = civ
		}

run :: FilePath -> FilePath -> Float -> ReaderT Global IO ()
run tfp mfp mnld = do
	writeGlobal globalTextureFilePath tfp
	writeGlobal globalModelFilePath mfp
	writeGlobal globalMinLod mnld
	initWindow
	initVulkan
	mainLoop
	cleanup

initWindow :: ReaderT Global IO ()
initWindow = do
	Just w <- lift do
		True <- GlfwB.init
		GlfwB.windowHint
			$ GlfwB.WindowHint'ClientAPI GlfwB.ClientAPI'NoAPI
		GlfwB.createWindow width height "Vulkan" Nothing Nothing
	writeGlobal globalWindow $ Just w
	g <- ask
	lift $ GlfwB.setFramebufferSizeCallback w
		(Just $ \_ _ _ -> writeIORef (globalFramebufferResized g) True)

initVulkan :: ReaderT Global IO ()
initVulkan = do
	createInstance
	when enableValidationLayers setupDebugMessenger
	createSurface
	pickPhysicalDevice
	createLogicalDevice
	createSwapChain
	createImageViews
	createRenderPass
	createDescriptorSetLayout
	createGraphicsPipeline
	createCommandPool
	createColorResources
	createDepthResources
	createFramebuffers
	createTextureImage
	createTextureImageView
	createTextureSampler
	loadModel
	createVertexBuffer
	createIndexBuffer
	createUniformBuffers
	createDescriptorPool
	createDescriptorSets
	createCommandBuffers
	createSyncObjects

createInstance :: ReaderT Global IO ()
createInstance = do
	lift . mapM_
		(Txt.putStrLn . ("\t" <>) . Vk.extensionPropertiesExtensionName)
		=<< lift (Vk.Instance.enumerateExtensionProperties Nothing)
	when enableValidationLayers $ bool
		(lift $ error "validation layers requested, but not available!")
		(pure ())
			=<< checkValidationLayerSupport
	let	appInfo = Vk.ApplicationInfo {
			Vk.applicationInfoNext = Nothing,
			Vk.applicationInfoApplicationName = "Hello Triangle",
			Vk.applicationInfoApplicationVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoEngineName = "No Engine",
			Vk.applicationInfoEngineVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoApiVersion = Vk.apiVersion_1_0 }
	extensions <- getRequiredExtensions
	let	createInfo = Vk.Instance.CreateInfo {
			Vk.Instance.createInfoNext =
				Just populateDebugMessengerCreateInfo,
			Vk.Instance.createInfoFlags =
				Vk.Instance.CreateFlagsZero,
			Vk.Instance.createInfoApplicationInfo = Just appInfo,
			Vk.Instance.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Instance.createInfoEnabledExtensionNames = extensions
			}
	writeGlobal globalInstance
		=<< lift (Vk.Instance.create @_ @() @() createInfo Nothing)

checkValidationLayerSupport :: ReaderT Global IO Bool
checkValidationLayerSupport = lift do
	availableLayers <- Vk.Instance.enumerateLayerProperties
	print validationLayers
	print availableLayers
	pure . null $ validationLayers \\
		(Vk.layerPropertiesLayerName <$> availableLayers)

getRequiredExtensions :: ReaderT Global IO [Txt.Text]
getRequiredExtensions = lift do
	glfwExtensions <-
		(cstringToText `mapM`) =<< GlfwB.getRequiredInstanceExtensions
	pure $ bool id (Vk.Ext.DebugUtils.extensionName :)
		enableValidationLayers glfwExtensions

setupDebugMessenger :: ReaderT Global IO ()
setupDebugMessenger = do
	ist <- readGlobal globalInstance
	writeGlobal globalDebugMessenger =<< lift (
		Vk.Ext.DebugUtils.Messenger.create ist
			populateDebugMessengerCreateInfo nil )

populateDebugMessengerCreateInfo ::
	Vk.Ext.DebugUtils.Messenger.CreateInfo () () () () () ()
populateDebugMessengerCreateInfo = Vk.Ext.DebugUtils.Messenger.CreateInfo {
	Vk.Ext.DebugUtils.Messenger.createInfoNext = Nothing,
	Vk.Ext.DebugUtils.Messenger.createInfoFlags =
		Vk.Ext.DebugUtils.Messenger.CreateFlagsZero,
	Vk.Ext.DebugUtils.Messenger.createInfoMessageSeverity =
		Vk.Ext.DebugUtils.Message.SeverityVerboseBit .|.
		Vk.Ext.DebugUtils.Message.SeverityWarningBit .|.
		Vk.Ext.DebugUtils.Message.SeverityErrorBit,
	Vk.Ext.DebugUtils.Messenger.createInfoMessageType =
		Vk.Ext.DebugUtils.Message.TypeGeneralBit .|.
		Vk.Ext.DebugUtils.Message.TypeValidationBit .|.
		Vk.Ext.DebugUtils.Message.TypePerformanceBit,
	Vk.Ext.DebugUtils.Messenger.createInfoFnUserCallback = debugCallback,
	Vk.Ext.DebugUtils.Messenger.createInfoUserData = Nothing }

debugCallback :: Vk.Ext.DebugUtils.Messenger.FnCallback () () () () ()
debugCallback _messageSeverity _messageType callbackData _userData = do
	let	message = Vk.Ext.DebugUtils.Messenger.callbackDataMessage
			callbackData
	Txt.putStrLn $ "validation layer: " <> message
	pure False

createSurface :: ReaderT Global IO ()
createSurface = do
	win <- fromJust <$> readGlobal globalWindow
	ist <- readGlobal globalInstance
	writeGlobal globalSurface
		=<< lift (Glfw.createWindowSurface ist win nil)

pickPhysicalDevice :: ReaderT Global IO ()
pickPhysicalDevice = do
	ist <- readGlobal globalInstance
	devices <- lift . Vk.PhysicalDevice.enumerate $ Vk.Instance.T.I ist
	when (null devices) $ error "failed to find GPUs with Vulkan support!"
	suitableDevices <- filterM isDeviceSuitable devices
	case suitableDevices of
		[] -> error "failed to find a suitable GPU!"
		(pdvc : _) -> do
			writeGlobal globalPhysicalDevice pdvc
			writeGlobal globalMsaaSamples =<< getMaxUsableSampleCount
			lift . print =<< readGlobal globalMsaaSamples

isDeviceSuitable :: Vk.PhysicalDevice.P -> ReaderT Global IO Bool
isDeviceSuitable pdvc = do
	_deviceProperties <- lift $ Vk.PhysicalDevice.getProperties pdvc
	_deviceFeatures <- lift $ Vk.PhysicalDevice.getFeatures pdvc
	is <- findQueueFamilies pdvc
	extensionSupported <- lift $ checkDeviceExtensionSupport pdvc
	swapChainSupport <- querySwapChainSupport pdvc
	let	swapChainAdequate =
			not (null $ formats swapChainSupport) &&
			not (null $ presentModes swapChainSupport)
	supportedFeatures <- lift $ Vk.PhysicalDevice.getFeatures pdvc
	pure $ isComplete is && extensionSupported && swapChainAdequate &&
		Vk.PhysicalDevice.featuresSamplerAnisotropy supportedFeatures

getMaxUsableSampleCount :: ReaderT Global IO Vk.Sample.CountFlagBits
getMaxUsableSampleCount = do
	phdvc <- readGlobal globalPhysicalDevice
	physicalDevicePropertiesLimits <- lift
		$ Vk.PhysicalDevice.propertiesLimits
		<$> Vk.PhysicalDevice.getProperties phdvc
	pure . maxBit $
		Vk.PhysicalDevice.limitsFramebufferColorSampleCounts
			physicalDevicePropertiesLimits .&.
		Vk.PhysicalDevice.limitsFramebufferDepthSampleCounts
			physicalDevicePropertiesLimits

maxBit :: FiniteBits n => n -> n
maxBit = mb $ rotateR (bit 0) 1
	where
	mb b n	| n == zeroBits = zeroBits
		| b .&. n /= zeroBits = b
		| otherwise = mb (shiftR b 1) n

deviceExtensions :: [Txt.Text]
deviceExtensions = [Vk.Khr.Swapchain.extensionName]

checkDeviceExtensionSupport :: Vk.PhysicalDevice.P -> IO Bool
checkDeviceExtensionSupport dvc = do
	availableExtensions <-
		Vk.PhysicalDevice.enumerateExtensionProperties dvc Nothing
	pure . null $ deviceExtensions \\
		(Vk.extensionPropertiesExtensionName <$> availableExtensions)

findQueueFamilies :: Vk.PhysicalDevice.P -> ReaderT Global IO QueueFamilyIndices
findQueueFamilies device = do
	queueFamilies <- lift
		$ Vk.PhysicalDevice.getQueueFamilyProperties device
	lift $ print queueFamilies
	sfc <- readGlobal globalSurface
	psi <- listToMaybe <$> lift (
		filterM (\i -> isPresentSupport device i sfc)
			[0 .. fromIntegral $ length queueFamilies - 1] )
	pure QueueFamilyIndices {
		graphicsFamily = fst <$> find
			((/= zeroBits)
				. (.&. Vk.Queue.GraphicsBit)
				. Vk.QueueFamily.propertiesQueueFlags
				. snd ) queueFamilies,
		presentFamily = Vk.QueueFamily.Index <$> psi }

isPresentSupport :: Vk.PhysicalDevice.P -> Word32 -> Vk.Khr.Surface.S -> IO Bool
isPresentSupport dvc i sfc = Vk.Khr.Surface.PhysicalDevice.getSupport dvc i sfc

data QueueFamilyIndices = QueueFamilyIndices {
	graphicsFamily :: Maybe Vk.QueueFamily.Index,
	presentFamily :: Maybe Vk.QueueFamily.Index }

isComplete :: QueueFamilyIndices -> Bool
isComplete QueueFamilyIndices {
	graphicsFamily = gf, presentFamily = pf } = isJust gf && isJust pf

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.Khr.Surface.Capabilities,
	formats :: [Vk.Khr.Surface.Format],
	presentModes :: [Vk.Khr.PresentMode] }

querySwapChainSupport ::
	Vk.PhysicalDevice.P -> ReaderT Global IO SwapChainSupportDetails
querySwapChainSupport dvc = readGlobal globalSurface >>= \sfc ->
	lift $ SwapChainSupportDetails
		<$> Vk.Khr.Surface.PhysicalDevice.getCapabilities dvc sfc
		<*> Vk.Khr.Surface.PhysicalDevice.getFormats dvc sfc
		<*> Vk.Khr.Surface.PhysicalDevice.getPresentModes dvc sfc

createLogicalDevice :: ReaderT Global IO ()
createLogicalDevice = do
	is <- findQueueFamilies =<< readGlobal globalPhysicalDevice
	let	uniqueQueueFamilies = nub [
			fromJust $ graphicsFamily is,
			fromJust $ presentFamily is ]
		queueCreateInfos qf = Vk.Device.Queue.CreateInfo {
			Vk.Device.Queue.createInfoNext = Nothing,
			Vk.Device.Queue.createInfoFlags =
				Vk.Device.Queue.CreateFlagsZero,
			Vk.Device.Queue.createInfoQueueFamilyIndex = qf,
			Vk.Device.Queue.createInfoQueuePriorities = [1] }
		deviceFeatures = Vk.PhysicalDevice.featuresZero {
			Vk.PhysicalDevice.featuresSamplerAnisotropy = True,
			Vk.PhysicalDevice.featuresSampleRateShading = True }
		createInfo = Vk.Device.CreateInfo {
			Vk.Device.createInfoNext = Nothing,
			Vk.Device.createInfoFlags = Vk.Device.CreateFlagsZero,
			Vk.Device.createInfoQueueCreateInfos =
				queueCreateInfos <$> uniqueQueueFamilies,
			Vk.Device.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Device.createInfoEnabledExtensionNames =
				deviceExtensions,
			Vk.Device.createInfoEnabledFeatures =
				Just deviceFeatures }
	pdvc <- readGlobal globalPhysicalDevice
	dvc <- lift (Vk.Device.create @() @() pdvc createInfo nil)
	writeGlobal globalDevice dvc
	writeGlobal globalGraphicsQueue =<< lift (
		Vk.Device.getQueue dvc (Vk.QueueFamily.unIndex . fromJust $ graphicsFamily is) 0 )
	writeGlobal globalPresentQueue =<< lift (
		Vk.Device.getQueue dvc (Vk.QueueFamily.unIndex . fromJust $ presentFamily is) 0 )

createSwapChain :: ReaderT Global IO ()
createSwapChain = do
	swapChainSupport <- querySwapChainSupport
		=<< readGlobal globalPhysicalDevice
	let	surfaceFormat =
			chooseSwapSurfaceFormat $ formats swapChainSupport
		presentMode =
			chooseSwapPresentMode $ presentModes swapChainSupport
	extent <- chooseSwapExtent $ capabilities swapChainSupport
	sfc <- readGlobal globalSurface
	is <- findQueueFamilies =<< readGlobal globalPhysicalDevice
	let	maxImageCount = fromMaybe maxBound . onlyIf (> 0)
			. Vk.Khr.Surface.capabilitiesMaxImageCount
			$ capabilities swapChainSupport
		imageCount = clamp
			(Vk.Khr.Surface.capabilitiesMinImageCount
				(capabilities swapChainSupport) + 1)
			0 maxImageCount
		(ism, qfis) = if graphicsFamily is /= presentFamily is
			then (Vk.SharingModeConcurrent, fromJust <$> [
				graphicsFamily is, presentFamily is ])
			else (Vk.SharingModeExclusive, [])
		createInfo = Vk.Khr.Swapchain.CreateInfo {
			Vk.Khr.Swapchain.createInfoNext = Nothing,
			Vk.Khr.Swapchain.createInfoFlags =
				Vk.Khr.Swapchain.CreateFlagsZero,
			Vk.Khr.Swapchain.createInfoSurface = sfc,
			Vk.Khr.Swapchain.createInfoMinImageCount = imageCount,
			Vk.Khr.Swapchain.createInfoImageFormat =
				Vk.Khr.Surface.formatFormat surfaceFormat,
			Vk.Khr.Swapchain.createInfoImageColorSpace =
				Vk.Khr.Surface.formatColorSpace surfaceFormat,
			Vk.Khr.Swapchain.createInfoImageExtent = extent,
			Vk.Khr.Swapchain.createInfoImageArrayLayers = 1,
			Vk.Khr.Swapchain.createInfoImageUsage =
				Vk.Image.UsageColorAttachmentBit,
			Vk.Khr.Swapchain.createInfoImageSharingMode = ism,
			Vk.Khr.Swapchain.createInfoQueueFamilyIndices = Vk.QueueFamily.unIndex <$> qfis,
			Vk.Khr.Swapchain.createInfoPreTransform =
				Vk.Khr.Surface.capabilitiesCurrentTransform
					$ capabilities swapChainSupport,
			Vk.Khr.Swapchain.createInfoCompositeAlpha =
				Vk.Khr.CompositeAlphaOpaqueBit,
			Vk.Khr.Swapchain.createInfoPresentMode = presentMode,
			Vk.Khr.Swapchain.createInfoClipped = True,
			Vk.Khr.Swapchain.createInfoOldSwapchain = Nothing }
	dvc <- readGlobal globalDevice
	sc <- lift $ Vk.Khr.Swapchain.create @() dvc createInfo nil
	writeGlobal globalSwapChain sc
	writeGlobal globalSwapChainImages
		=<< lift (Vk.Khr.Swapchain.getImages dvc sc)
	writeGlobal globalSwapChainImageFormat
		. Just $ Vk.Khr.Surface.formatFormat surfaceFormat
	writeGlobal globalSwapChainExtent extent
	lift do	putStrLn "*** CREATE SWAP CHAIN ***"
		print surfaceFormat
		print presentMode
		print extent

onlyIf :: (a -> Bool) -> a -> Maybe a
onlyIf p x | p x = Just x | otherwise = Nothing

chooseSwapSurfaceFormat  :: [Vk.Khr.Surface.Format] -> Vk.Khr.Surface.Format
chooseSwapSurfaceFormat = \case
	availableFormats@(af0 : _) -> fromMaybe af0
		$ find preferredSwapSurfaceFormat availableFormats
	_ -> error "no available swap surface formats"

preferredSwapSurfaceFormat :: Vk.Khr.Surface.Format -> Bool
preferredSwapSurfaceFormat f =
	Vk.Khr.Surface.formatFormat f == Vk.Format.B8g8r8a8Srgb &&
	Vk.Khr.Surface.formatColorSpace f == Vk.Khr.ColorSpaceSrgbNonlinear

chooseSwapPresentMode :: [Vk.Khr.PresentMode] -> Vk.Khr.PresentMode
chooseSwapPresentMode =
	fromMaybe Vk.Khr.PresentModeFifo . find (== Vk.Khr.PresentModeMailbox)

chooseSwapExtent :: Vk.Khr.Surface.Capabilities -> ReaderT Global IO Vk.C.Extent2d
chooseSwapExtent caps
	| Vk.C.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> w, fromIntegral -> h) <-
			lift . GlfwB.getFramebufferSize
				=<< fromJust <$> readGlobal globalWindow
		pure $ Vk.C.Extent2d
			(clamp w (Vk.C.extent2dWidth n) (Vk.C.extent2dHeight n))
			(clamp h (Vk.C.extent2dWidth x) (Vk.C.extent2dHeight x))
	where
	curExt = Vk.Khr.Surface.capabilitiesCurrentExtent caps
	n = Vk.Khr.Surface.capabilitiesMinImageExtent caps
	x = Vk.Khr.Surface.capabilitiesMaxImageExtent caps

clamp :: Ord a => a -> a -> a -> a
clamp x mn mx | x < mn = mn | x < mx = x | otherwise = mx

createImageViews :: ReaderT Global IO ()
createImageViews = do
	scif <- fromJust <$> readGlobal globalSwapChainImageFormat
	writeGlobal globalSwapChainImageViews
		=<< ((\img -> createImageView
			img scif Vk.Image.AspectColorBit 1) `mapM`)
		=<< readGlobal globalSwapChainImages

createImageView :: Vk.Image.I -> Vk.Format.F -> Vk.Image.AspectFlags ->
	Word32 -> ReaderT Global IO Vk.ImageView.I
createImageView image format aspectFlags mipLevels = do
	let	createInfo = Vk.ImageView.CreateInfo {
			Vk.ImageView.createInfoNext = Nothing,
			Vk.ImageView.createInfoFlags =
				Vk.ImageView.CreateFlagsZero,
			Vk.ImageView.createInfoImage = image,
			Vk.ImageView.createInfoViewType = Vk.ImageView.Type2d,
			Vk.ImageView.createInfoFormat = format,
			Vk.ImageView.createInfoComponents = components,
			Vk.ImageView.createInfoSubresourceRange =
				subresourceRange }
		components = Vk.Component.Mapping {
			Vk.Component.mappingR = Vk.Component.SwizzleIdentity,
			Vk.Component.mappingG = Vk.Component.SwizzleIdentity,
			Vk.Component.mappingB = Vk.Component.SwizzleIdentity,
			Vk.Component.mappingA = Vk.Component.SwizzleIdentity }
		subresourceRange = Vk.Image.SubresourceRange {
			Vk.Image.subresourceRangeAspectMask = aspectFlags,
			Vk.Image.subresourceRangeBaseMipLevel = 0,
			Vk.Image.subresourceRangeLevelCount = mipLevels,
			Vk.Image.subresourceRangeBaseArrayLayer = 0,
			Vk.Image.subresourceRangeLayerCount = 1 }
	dvc <- readGlobal globalDevice
	lift $ Vk.ImageView.create @() dvc createInfo nil

createRenderPass :: ReaderT Global IO ()
createRenderPass = do
	Just scif <- readGlobal globalSwapChainImageFormat
	msaaS <- readGlobal globalMsaaSamples
	df <- findDepthFormat
	let	colorAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = Vk.Att.DescriptionFlagsZero,
			Vk.Att.descriptionFormat = scif,
			Vk.Att.descriptionSamples = msaaS,
			Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
			Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
			Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOp =
				Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayout =
				Vk.Image.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Image.LayoutColorAttachmentOptimal }
		colorAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = Vk.Att.A 0,
			Vk.Att.referenceLayout =
				Vk.Image.LayoutColorAttachmentOptimal }
		depthAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = Vk.Att.DescriptionFlagsZero,
			Vk.Att.descriptionFormat = df,
			Vk.Att.descriptionSamples = msaaS,
			Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
			Vk.Att.descriptionStoreOp = Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOp = Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayout = Vk.Image.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Image.LayoutDepthStencilAttachmentOptimal }
		depthAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 1,
			Vk.Att.referenceLayout =
				Vk.Image.LayoutDepthStencilAttachmentOptimal }
		colorAttachmentResolve = Vk.Att.Description {
			Vk.Att.descriptionFlags = Vk.Att.DescriptionFlagsZero,
			Vk.Att.descriptionFormat = scif,
			Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
			Vk.Att.descriptionLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
			Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOp =
				Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayout =
				Vk.Image.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Image.LayoutPresentSrcKhr }
		colorAttachmentResolveRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 2,
			Vk.Att.referenceLayout =
				Vk.Image.LayoutColorAttachmentOptimal }
		subpass = Vk.Subpass.Description {
			Vk.Subpass.descriptionFlags =
				Vk.Subpass.DescriptionFlagsZero,
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
			Vk.Subpass.dependencySrcAccessMask = Vk.AccessFlagsZero,
			Vk.Subpass.dependencyDstStageMask =
				Vk.Ppl.StageColorAttachmentOutputBit .|.
				Vk.Ppl.StageEarlyFragmentTestsBit,
			Vk.Subpass.dependencyDstAccessMask =
				Vk.AccessColorAttachmentWriteBit .|.
				Vk.AccessDepthStencilAttachmentWriteBit,
			Vk.Subpass.dependencyDependencyFlags =
				Vk.DependencyFlagsZero }
		renderPassInfo = Vk.RenderPass.CreateInfo {
			Vk.RenderPass.createInfoNext = Nothing,
			Vk.RenderPass.createInfoFlags =
				Vk.RenderPass.CreateFlagsZero,
			Vk.RenderPass.createInfoAttachments = [
				colorAttachment, depthAttachment,
				colorAttachmentResolve ],
			Vk.RenderPass.createInfoSubpasses = [subpass],
			Vk.RenderPass.createInfoDependencies = [dependency] }
	dvc <- readGlobal globalDevice
	writeGlobal globalRenderPass
		=<< lift (Vk.RenderPass.create @() dvc renderPassInfo nil)

createDescriptorSetLayout :: ReaderT Global IO ()
createDescriptorSetLayout = do
	let	uboLayoutBinding = Vk.DscSet.Lyt.Binding {
			Vk.DscSet.Lyt.bindingBinding = 0,
			Vk.DscSet.Lyt.bindingDescriptorType =
				Vk.Dsc.TypeUniformBuffer,
			Vk.DscSet.Lyt.bindingDescriptorCountOrImmutableSamplers
				= Left 1,
			Vk.DscSet.Lyt.bindingStageFlags =
				Vk.Shader.Stage.VertexBit }
		samplerLayoutBinding = Vk.DscSet.Lyt.Binding {
			Vk.DscSet.Lyt.bindingBinding = 1,
			Vk.DscSet.Lyt.bindingDescriptorType =
				Vk.Dsc.TypeCombinedImageSampler,
			Vk.DscSet.Lyt.bindingDescriptorCountOrImmutableSamplers
				= Left 1,
			Vk.DscSet.Lyt.bindingStageFlags =
				Vk.Shader.Stage.FragmentBit }
		layoutInfo = Vk.DscSet.Lyt.CreateInfo {
			Vk.DscSet.Lyt.createInfoNext = Nothing,
			Vk.DscSet.Lyt.createInfoFlags =
				Vk.DscSet.Lyt.CreateFlagsZero,
			Vk.DscSet.Lyt.createInfoBindings =
				[uboLayoutBinding, samplerLayoutBinding] }
	dvc <- readGlobal globalDevice
	dsl <- lift $ Vk.DscSet.Lyt.create @() dvc layoutInfo nil
	writeGlobal globalDescriptorSetLayout dsl

createGraphicsPipeline :: ReaderT Global IO ()
createGraphicsPipeline = do
	vertShaderModule <- createShaderModule glslVertexShaderMain
	fragShaderModule <- createShaderModule glslFragmentShaderMain

	let	vertShaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
			Vk.Ppl.ShaderStage.createInfoNext = Nothing,
			Vk.Ppl.ShaderStage.createInfoFlags =
				Vk.Ppl.ShaderStage.CreateFlagsZero,
			Vk.Ppl.ShaderStage.createInfoStage =
				Vk.Shader.Stage.VertexBit,
			Vk.Ppl.ShaderStage.createInfoModule = vertShaderModule,
			Vk.Ppl.ShaderStage.createInfoName = "main",
			Vk.Ppl.ShaderStage.createInfoSpecializationInfo =
				Nothing }
		fragShaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
			Vk.Ppl.ShaderStage.createInfoNext = Nothing,
			Vk.Ppl.ShaderStage.createInfoFlags =
				Vk.Ppl.ShaderStage.CreateFlagsZero,
			Vk.Ppl.ShaderStage.createInfoStage =
				Vk.Shader.Stage.FragmentBit,
			Vk.Ppl.ShaderStage.createInfoModule = fragShaderModule,
			Vk.Ppl.ShaderStage.createInfoName = "main",
			Vk.Ppl.ShaderStage.createInfoSpecializationInfo =
				Nothing }
		shaderStages =
			vertShaderStageInfo `Vk.Ppl.ShaderStage.CreateInfoCons`
			fragShaderStageInfo `Vk.Ppl.ShaderStage.CreateInfoCons`
			Vk.Ppl.ShaderStage.CreateInfoNil
		vertexInputInfo :: Vk.Ppl.VertexInputSt.CreateInfo
			()
			(Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
			'[Cglm.Vec3, Color, TexCoord]
		vertexInputInfo = Vk.Ppl.VertexInputSt.CreateInfo {
			Vk.Ppl.VertexInputSt.createInfoNext = Nothing,
			Vk.Ppl.VertexInputSt.createInfoFlags =
				Vk.Ppl.VertexInputSt.M.CreateFlagsZero }
		inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
			Vk.Ppl.InpAsmbSt.createInfoNext = Nothing,
			Vk.Ppl.InpAsmbSt.createInfoFlags =
				Vk.Ppl.InpAsmbSt.CreateFlagsZero,
			Vk.Ppl.InpAsmbSt.createInfoTopology =
				Vk.PrimitiveTopologyTriangleList,
			Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable =
				False }
	sce <- readGlobal globalSwapChainExtent
	msaaS <- readGlobal globalMsaaSamples
	let	viewport = Vk.C.Viewport {
			Vk.C.viewportX = 0, Vk.C.viewportY = 0,
			Vk.C.viewportWidth =
				fromIntegral $ Vk.C.extent2dWidth sce,
			Vk.C.viewportHeight =
				fromIntegral $ Vk.C.extent2dHeight sce,
			Vk.C.viewportMinDepth = 0, Vk.C.viewportMaxDepth = 1 }
		scissor = Vk.C.Rect2d {
			Vk.C.rect2dOffset = Vk.C.Offset2d 0 0,
			Vk.C.rect2dExtent = sce }
		viewportState = Vk.Ppl.ViewportSt.CreateInfo {
			Vk.Ppl.ViewportSt.createInfoNext = Nothing,
			Vk.Ppl.ViewportSt.createInfoFlags =
				Vk.Ppl.ViewportSt.CreateFlagsZero,
			Vk.Ppl.ViewportSt.createInfoViewports = [viewport],
			Vk.Ppl.ViewportSt.createInfoScissors = [scissor] }
		rasterizer = Vk.Ppl.RstSt.CreateInfo {
			Vk.Ppl.RstSt.createInfoNext = Nothing,
			Vk.Ppl.RstSt.createInfoFlags =
				Vk.Ppl.RstSt.CreateFlagsZero,
			Vk.Ppl.RstSt.createInfoDepthClampEnable = False,
			Vk.Ppl.RstSt.createInfoRasterizerDiscardEnable = False,
			Vk.Ppl.RstSt.createInfoPolygonMode = Vk.PolygonModeFill,
			Vk.Ppl.RstSt.createInfoLineWidth = 1,
			Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeBackBit,
			Vk.Ppl.RstSt.createInfoFrontFace =
				Vk.FrontFaceCounterClockwise,
			Vk.Ppl.RstSt.createInfoDepthBiasEnable = False,
			Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
			Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
			Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0 }
		multisampling = Vk.Ppl.MulSmplSt.CreateInfo {
			Vk.Ppl.MulSmplSt.createInfoNext = Nothing,
			Vk.Ppl.MulSmplSt.createInfoFlags =
				Vk.Ppl.MulSmplSt.CreateFlagsZero,
			Vk.Ppl.MulSmplSt.createInfoSampleShadingEnable = True,
			Vk.Ppl.MulSmplSt.createInfoRasterizationSamplesAndMask =
				Vk.Sample.CountAndMask msaaS Nothing,
			Vk.Ppl.MulSmplSt.createInfoMinSampleShading = 0.2,
			Vk.Ppl.MulSmplSt.createInfoAlphaToCoverageEnable =
				False,
			Vk.Ppl.MulSmplSt.createInfoAlphaToOneEnable = False }
		colorBlendAttachment = Vk.Ppl.ClrBlndAtt.State {
			Vk.Ppl.ClrBlndAtt.stateColorWriteMask =
				Vk.ClrCmp.RBit .|. Vk.ClrCmp.GBit .|.
				Vk.ClrCmp.BBit .|. Vk.ClrCmp.ABit,
			Vk.Ppl.ClrBlndAtt.stateBlendEnable = False,
			Vk.Ppl.ClrBlndAtt.stateSrcColorBlendFactor =
				Vk.BlendFactorOne,
			Vk.Ppl.ClrBlndAtt.stateDstColorBlendFactor =
				Vk.BlendFactorZero,
			Vk.Ppl.ClrBlndAtt.stateColorBlendOp = Vk.BlendOpAdd,
			Vk.Ppl.ClrBlndAtt.stateSrcAlphaBlendFactor =
				Vk.BlendFactorOne,
			Vk.Ppl.ClrBlndAtt.stateDstAlphaBlendFactor =
				Vk.BlendFactorZero,
			Vk.Ppl.ClrBlndAtt.stateAlphaBlendOp = Vk.BlendOpAdd }
		colorBlending = Vk.Ppl.ClrBlndSt.CreateInfo {
			Vk.Ppl.ClrBlndSt.createInfoNext = Nothing,
			Vk.Ppl.ClrBlndSt.createInfoFlags =
				Vk.Ppl.ClrBlndSt.CreateFlagsZero,
			Vk.Ppl.ClrBlndSt.createInfoLogicOpEnable = False,
			Vk.Ppl.ClrBlndSt.createInfoLogicOp = Vk.LogicOpCopy,
			Vk.Ppl.ClrBlndSt.createInfoAttachments =
				[colorBlendAttachment],
			Vk.Ppl.ClrBlndSt.createInfoBlendConstants =
				fromJust $ rgbaDouble 0 0 0 0 }
	dsl <- readGlobal globalDescriptorSetLayout
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = Nothing,
			Vk.Ppl.Layout.createInfoFlags =
				Vk.Ppl.Layout.CreateFlagsZero,
			Vk.Ppl.Layout.createInfoSetLayouts = [dsl],
			Vk.Ppl.Layout.createInfoPushConstantRanges = [] }
	dvc <- readGlobal globalDevice
	writeGlobal globalPipelineLayout
		=<< lift (Vk.Ppl.Layout.create @() dvc pipelineLayoutInfo nil)

	ppllyt <- readGlobal globalPipelineLayout
	rp <- readGlobal globalRenderPass
	let	depthStencil = Vk.Ppl.DepthStencilSt.CreateInfo {
			Vk.Ppl.DepthStencilSt.createInfoNext = Nothing,
			Vk.Ppl.DepthStencilSt.createInfoFlags =
				Vk.Ppl.DepthStencilSt.CreateFlagsZero,
			Vk.Ppl.DepthStencilSt.createInfoDepthTestEnable = True,
			Vk.Ppl.DepthStencilSt.createInfoDepthWriteEnable = True,
			Vk.Ppl.DepthStencilSt.createInfoDepthCompareOp =
				Vk.CompareOpLess,
			Vk.Ppl.DepthStencilSt.createInfoDepthBoundsTestEnable =
				False,
			Vk.Ppl.DepthStencilSt.createInfoMinDepthBounds = 0,
			Vk.Ppl.DepthStencilSt.createInfoMaxDepthBounds = 1,
			Vk.Ppl.DepthStencilSt.createInfoStencilTestEnable = False,
			Vk.Ppl.DepthStencilSt.createInfoFront =
				Vk.stencilOpStateZero,
			Vk.Ppl.DepthStencilSt.createInfoBack =
				Vk.stencilOpStateZero }
		pipelineInfo :: Vk.Ppl.Graphics.CreateInfo
			() () '[ 'GlslVertexShader, 'GlslFragmentShader]
			'[(), ()] ()
			(Solo (AddType Vertex 'Vk.VertexInput.RateVertex))
			'[Cglm.Vec3, Color, TexCoord]
			() () () () () () () () () '[]
		pipelineInfo = Vk.Ppl.Graphics.CreateInfo {
			Vk.Ppl.Graphics.createInfoNext = Nothing,
			Vk.Ppl.Graphics.createInfoFlags =
				Vk.Ppl.CreateFlagsZero,
			Vk.Ppl.Graphics.createInfoStages = shaderStages,
			Vk.Ppl.Graphics.createInfoVertexInputState =
				Just vertexInputInfo,
			Vk.Ppl.Graphics.createInfoInputAssemblyState =
				Just inputAssembly,
			Vk.Ppl.Graphics.createInfoViewportState =
				Just viewportState,
			Vk.Ppl.Graphics.createInfoRasterizationState =
				Just rasterizer,
			Vk.Ppl.Graphics.createInfoMultisampleState =
				Just multisampling,
			Vk.Ppl.Graphics.createInfoDepthStencilState =
				Just depthStencil,
			Vk.Ppl.Graphics.createInfoColorBlendState =
				Just colorBlending,
			Vk.Ppl.Graphics.createInfoDynamicState = Nothing,
			Vk.Ppl.Graphics.createInfoLayout = ppllyt,
			Vk.Ppl.Graphics.createInfoRenderPass = rp,
			Vk.Ppl.Graphics.createInfoSubpass = 0,
			Vk.Ppl.Graphics.createInfoBasePipelineHandle =
				Vk.Ppl.Graphics.GNull,
			Vk.Ppl.Graphics.createInfoBasePipelineIndex = - 1,
			Vk.Ppl.Graphics.createInfoTessellationState = Nothing }

	gpl `Vk.Ppl.Graphics.PCons` Vk.Ppl.Graphics.PNil <- lift
		$ Vk.Ppl.Graphics.create
			dvc Nothing
			(pipelineInfo `Vk.Ppl.Graphics.CreateInfoCons`
				Vk.Ppl.Graphics.CreateInfoNil)
			nil
	writeGlobal globalGraphicsPipeline gpl

	lift do	Vk.Shader.Module.destroy dvc fragShaderModule nil
		Vk.Shader.Module.destroy dvc vertShaderModule nil

createShaderModule :: Spv sknd -> ReaderT Global IO (Vk.Shader.Module.M sknd)
createShaderModule cd = do
	let	createInfo = Vk.Shader.Module.CreateInfo {
			Vk.Shader.Module.createInfoNext = Nothing,
			Vk.Shader.Module.createInfoFlags =
				Vk.Shader.Module.CreateFlagsZero,
			Vk.Shader.Module.createInfoCode = cd }
	dvc <- readGlobal globalDevice
	lift $ Vk.Shader.Module.create @() dvc createInfo nil

createFramebuffers :: ReaderT Global IO ()
createFramebuffers = writeGlobal globalSwapChainFramebuffers
	=<< (createFramebuffer1 `mapM`) =<< readGlobal globalSwapChainImageViews

createFramebuffer1 :: Vk.ImageView.I -> ReaderT Global IO Vk.Framebuffer.F
createFramebuffer1 attachment = do
	rp <- readGlobal globalRenderPass
	divw <- readGlobal globalDepthImageView
	civw <- readGlobal globalColorImageView
	Vk.C.Extent2d {
		Vk.C.extent2dWidth = w,
		Vk.C.extent2dHeight = h } <- readGlobal globalSwapChainExtent
	let	framebufferInfo = Vk.Framebuffer.CreateInfo {
			Vk.Framebuffer.createInfoNext = Nothing,
			Vk.Framebuffer.createInfoFlags =
				Vk.Framebuffer.CreateFlagsZero,
			Vk.Framebuffer.createInfoRenderPass = rp,
			Vk.Framebuffer.createInfoAttachments =
				[civw, divw, attachment],
			Vk.Framebuffer.createInfoWidth = w,
			Vk.Framebuffer.createInfoHeight = h,
			Vk.Framebuffer.createInfoLayers = 1 }
	dvc <- readGlobal globalDevice
	lift $ Vk.Framebuffer.create @() dvc framebufferInfo nil

createCommandPool :: ReaderT Global IO ()
createCommandPool = do
	pdvc <- readGlobal globalPhysicalDevice
	queueFamilyIndices <- findQueueFamilies pdvc
	let	poolInfo = Vk.CommandPool.CreateInfo {
			Vk.CommandPool.createInfoNext = Nothing,
			Vk.CommandPool.createInfoFlags =
				Vk.CommandPool.CreateResetCommandBufferBit,
			Vk.CommandPool.createInfoQueueFamilyIndex =
				fromJust $ graphicsFamily queueFamilyIndices }
	dvc <- readGlobal globalDevice
	writeGlobal globalCommandPool
		=<< lift (Vk.CommandPool.create @() dvc poolInfo nil)

createColorResources :: ReaderT Global IO ()
createColorResources = do
	Just colorFormat <- readGlobal globalSwapChainImageFormat
	sce <- readGlobal globalSwapChainExtent
	msaaS <- readGlobal globalMsaaSamples
	(ci, cim) <- createImage
		(Vk.C.extent2dWidth sce) (Vk.C.extent2dHeight sce) 1
		msaaS colorFormat Vk.Image.TilingOptimal
			(Vk.Image.UsageTransientAttachmentBit .|. Vk.Image.UsageColorAttachmentBit)
			Vk.Memory.PropertyDeviceLocalBit
	civ <- createImageView ci colorFormat Vk.Image.AspectColorBit 1
	writeGlobal globalColorImage ci
	writeGlobal globalColorImageMemory cim
	writeGlobal globalColorImageView civ

createDepthResources :: ReaderT Global IO ()
createDepthResources = do
	lift $ putStrLn "*** CREATE DEPTH RESOURCES ***"
	depthFormat <- findDepthFormat
	lift $ print depthFormat
	sce <- readGlobal globalSwapChainExtent
	msaaS <- readGlobal globalMsaaSamples
	(di, dim) <- createImage
		(Vk.C.extent2dWidth sce) (Vk.C.extent2dHeight sce)
		1 msaaS depthFormat
		Vk.Image.TilingOptimal
		Vk.Image.UsageDepthStencilAttachmentBit
		Vk.Memory.PropertyDeviceLocalBit
	writeGlobal globalDepthImage di
	writeGlobal globalDepthImageMemory dim
	divw <- createImageView di depthFormat Vk.Image.AspectDepthBit 1
	writeGlobal globalDepthImageView divw
	transitionImageLayout di depthFormat Vk.Image.LayoutUndefined
		Vk.Image.LayoutDepthStencilAttachmentOptimal 1

findDepthFormat :: ReaderT Global IO Vk.Format.F
findDepthFormat = findSupportedFormat
	[Vk.Format.D32Sfloat,
		Vk.Format.D32SfloatS8Uint, Vk.Format.D24UnormS8Uint]
	Vk.Image.TilingOptimal Vk.Format.FeatureDepthStencilAttachmentBit

findSupportedFormat ::
	[Vk.Format.F] -> Vk.Image.Tiling -> Vk.Format.FeatureFlags ->
	ReaderT Global IO Vk.Format.F
findSupportedFormat candidates tiling features = do
	pdvc <- readGlobal globalPhysicalDevice
	fmts <- lift $ (`zip` candidates)
		<$> Vk.PhysicalDevice.getFormatProperties pdvc `mapM` candidates
	pure . maybe (error "failed to find supported format!") snd
		$ find (doesFeatureMatch tiling features . fst) fmts

doesFeatureMatch ::
	Vk.Image.Tiling -> Vk.Format.FeatureFlags -> Vk.Format.Properties ->
	Bool
doesFeatureMatch Vk.Image.TilingLinear features props =
	Vk.Format.propertiesLinearTilingFeatures props .&. features == features
doesFeatureMatch Vk.Image.TilingOptimal features props =
	Vk.Format.propertiesOptimalTilingFeatures props .&. features == features
doesFeatureMatch _ _ _ = False

hasStencilComponent :: Vk.Format.F -> Bool
hasStencilComponent = \case
	Vk.Format.D32SfloatS8Uint -> True; Vk.Format.D24UnormS8Uint -> True
	_ -> False

createTextureImage :: ReaderT Global IO ()
createTextureImage = do
	tfp <- readGlobal globalTextureFilePath
	img <- lift $ readRgba8 tfp
	let	texWidth = imageWidth img
		texHeight = imageHeight img
		ml = floor @Double
			(logBase 2 . fromIntegral $ max texWidth texHeight) + 1
	lift do	print texWidth
		print texHeight
		print . V.length $ imageData img
		putStrLn $ "mipLevels: " ++ show ml
	writeGlobal globalMipLevels ml
	let	imageSize = V.length $ imageData img
	(stagingBuffer, stagingBufferMemory) <- createBufferList @Word8 imageSize
		Vk.Buffer.UsageTransferSrcBit $
		Vk.Memory.PropertyHostVisibleBit .|.
		Vk.Memory.PropertyHostCoherentBit
	dvc <- readGlobal globalDevice
	lift $ Vk.Memory.List.writeList dvc stagingBufferMemory Vk.Memory.M.MapFlagsZero
		(V.toList $ imageData img)
	(ti, tim) <- createImage
		(fromIntegral texWidth) (fromIntegral texHeight)
		ml Vk.Sample.Count1Bit
		Vk.Format.R8g8b8a8Srgb Vk.Image.TilingOptimal
		(	Vk.Image.UsageTransferSrcBit .|.
			Vk.Image.UsageTransferDstBit .|.
			Vk.Image.UsageSampledBit )
		Vk.Memory.PropertyDeviceLocalBit
	writeGlobal globalTextureImage ti
	writeGlobal globalTextureImageMemory tim
	transitionImageLayout ti Vk.Format.R8g8b8a8Srgb Vk.Image.LayoutUndefined
		Vk.Image.LayoutTransferDstOptimal ml
	copyBufferToImage stagingBuffer ti
		(fromIntegral texWidth) (fromIntegral texHeight)
	generateMipmaps ti Vk.Format.R8g8b8a8Srgb
		(fromIntegral texWidth) (fromIntegral texHeight) ml
	lift do	Vk.Buffer.List.destroy dvc stagingBuffer nil
		Vk.Memory.List.free dvc stagingBufferMemory nil

generateMipmaps :: Vk.Image.I -> Vk.Format.F ->
	Int32 -> Int32 -> Word32 -> ReaderT Global IO ()
generateMipmaps img imageFormat tw th ml = do
	phdvc <- readGlobal globalPhysicalDevice
	formatProperties <-
		lift $ Vk.PhysicalDevice.getFormatProperties phdvc imageFormat
	lift do	putStrLn $ "FORMAT PROPERTIES: " ++ show formatProperties
		putStrLn $ "\t" ++ show (
			Vk.Format.propertiesOptimalTilingFeatures
				formatProperties .&.
			Vk.Format.FeatureSampledImageFilterLinearBit )
	when (Vk.Format.propertiesOptimalTilingFeatures formatProperties .&.
		Vk.Format.FeatureSampledImageFilterLinearBit == zeroBits) $
		error "texture image format does not support linear blitting!"

	commandBuffer <- beginSingleTimeCommands

	let	barrier = Vk.Image.MemoryBarrier {
			Vk.Image.memoryBarrierNext = Nothing,
			Vk.Image.memoryBarrierImage = img,
			Vk.Image.memoryBarrierSrcQueueFamilyIndex =
				Vk.QueueFamily.Ignored,
			Vk.Image.memoryBarrierDstQueueFamilyIndex =
				Vk.QueueFamily.Ignored,
			Vk.Image.memoryBarrierSubresourceRange =
				Vk.Image.SubresourceRange {
					Vk.Image.subresourceRangeAspectMask =
						Vk.Image.AspectColorBit,
					Vk.Image.subresourceRangeBaseArrayLayer
						= 0,
					Vk.Image.subresourceRangeLayerCount = 1,
					Vk.Image.subresourceRangeLevelCount = 1,
					Vk.Image.subresourceRangeBaseMipLevel =
						0 },
			Vk.Image.memoryBarrierOldLayout =
				Vk.Image.LayoutTransferDstOptimal,
			Vk.Image.memoryBarrierNewLayout =
				Vk.Image.LayoutTransferSrcOptimal,
			Vk.Image.memoryBarrierSrcAccessMask =
				Vk.AccessTransferWriteBit,
			Vk.Image.memoryBarrierDstAccessMask =
				Vk.AccessTransferReadBit }
		srr = Vk.Image.memoryBarrierSubresourceRange barrier
	for_ (zip [1 .. ml - 1] (zip
		(iterate halfOrOne tw)
		(iterate halfOrOne th))) $ \(i, (w, h)) ->
		generateMipmaps1 @() commandBuffer img barrier w h i

	let	barrier' = barrier {
			Vk.Image.memoryBarrierSubresourceRange = srr {
				Vk.Image.subresourceRangeBaseMipLevel =
					ml - 1 },
			Vk.Image.memoryBarrierOldLayout =
				Vk.Image.LayoutTransferDstOptimal,
			Vk.Image.memoryBarrierNewLayout =
				Vk.Image.LayoutShaderReadOnlyOptimal,
			Vk.Image.memoryBarrierSrcAccessMask =
				Vk.AccessTransferWriteBit,
			Vk.Image.memoryBarrierDstAccessMask =
				Vk.AccessShaderReadBit }
	lift $ Vk.Cmd.pipelineBarrier @() @() commandBuffer
		Vk.Ppl.StageTransferBit Vk.Ppl.StageFragmentShaderBit
		Vk.DependencyFlagsZero [] [] [barrier']

	endSingleTimeCommands commandBuffer

generateMipmaps1 :: Pointable n =>
	Vk.CommandBuffer.C v -> Vk.Image.I -> Vk.Image.MemoryBarrier n ->
	Int32 -> Int32 -> Word32 -> ReaderT Global IO ()
generateMipmaps1 commandBuffer img barrier_ texWidth texHeight i = do
	let	mipWidth = texWidth; mipHeight = texHeight
		srr_ = Vk.Image.memoryBarrierSubresourceRange barrier_
		barrier = barrier_ {
			Vk.Image.memoryBarrierSubresourceRange = srr_ {
					Vk.Image.subresourceRangeBaseMipLevel =
						i - 1 } }
	lift $ Vk.Cmd.pipelineBarrier @() @() commandBuffer
		Vk.Ppl.StageTransferBit Vk.Ppl.StageTransferBit
		Vk.DependencyFlagsZero [] [] [barrier]
	let	blit = Vk.Image.Blit {
			Vk.Image.blitSrcOffsetFrom = Vk.C.Offset3d 0 0 0,
			Vk.Image.blitSrcOffsetTo =
				Vk.C.Offset3d mipWidth mipHeight 1,
			Vk.Image.blitSrcSubresource =
				Vk.Image.SubresourceLayers {
					Vk.Image.subresourceLayersAspectMask =
						Vk.Image.AspectColorBit,
					Vk.Image.subresourceLayersMipLevel =
						i - 1,
					Vk.Image.subresourceLayersBaseArrayLayer
						= 0,
					Vk.Image.subresourceLayersLayerCount =
						1 },
			Vk.Image.blitDstOffsetFrom = Vk.C.Offset3d 0 0 0,
			Vk.Image.blitDstOffsetTo = Vk.C.Offset3d
				(halfOrOne mipWidth) (halfOrOne mipHeight) 1,
			Vk.Image.blitDstSubresource =
				Vk.Image.SubresourceLayers {
					Vk.Image.subresourceLayersAspectMask =
						Vk.Image.AspectColorBit,
					Vk.Image.subresourceLayersMipLevel = i,
					Vk.Image.subresourceLayersBaseArrayLayer
						= 0,
					Vk.Image.subresourceLayersLayerCount =
						1 } }
	lift $ Vk.Cmd.blitImage commandBuffer
		img Vk.Image.LayoutTransferSrcOptimal
		img Vk.Image.LayoutTransferDstOptimal
		[blit] Vk.FilterLinear
	let	barrier' = barrier {
			Vk.Image.memoryBarrierOldLayout =
				Vk.Image.LayoutTransferSrcOptimal,
			Vk.Image.memoryBarrierNewLayout =
				Vk.Image.LayoutShaderReadOnlyOptimal,
			Vk.Image.memoryBarrierSrcAccessMask =
				Vk.AccessTransferReadBit,
			Vk.Image.memoryBarrierDstAccessMask =
				Vk.AccessShaderReadBit }
	lift $ Vk.Cmd.pipelineBarrier @() @() commandBuffer
		Vk.Ppl.StageTransferBit Vk.Ppl.StageFragmentShaderBit
		Vk.DependencyFlagsZero [] [] [barrier']

halfOrOne :: Integral n => n -> n
halfOrOne n | n > 1 = n `div` 2 | otherwise = 1

createImage ::
	Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlagBits ->
	Vk.Format.F -> Vk.Image.Tiling ->
	Vk.Image.UsageFlags -> Vk.Memory.PropertyFlags ->
	ReaderT Global IO (Vk.Image.I, Vk.Device.MemoryImage)
createImage widt hght mipLevels numSamples format tiling usage properties = do
	dvc <- readGlobal globalDevice
	let	imageInfo = Vk.Image.CreateInfo {
			Vk.Image.createInfoNext = Nothing,
			Vk.Image.createInfoFlags = Vk.Image.CreateFlagsZero,
			Vk.Image.createInfoImageType = Vk.Image.Type2d,
			Vk.Image.createInfoExtent = Vk.C.Extent3d {
				Vk.C.extent3dWidth = widt, Vk.C.extent3dHeight = hght,
				Vk.C.extent3dDepth = 1 },
			Vk.Image.createInfoMipLevels = mipLevels,
			Vk.Image.createInfoArrayLayers = 1,
			Vk.Image.createInfoFormat = format,
			Vk.Image.createInfoTiling = tiling,
			Vk.Image.createInfoInitialLayout =
				Vk.Image.LayoutUndefined,
			Vk.Image.createInfoUsage = usage,
			Vk.Image.createInfoSharingMode =
				Vk.SharingModeExclusive,
			Vk.Image.createInfoSamples = numSamples,
			Vk.Image.createInfoQueueFamilyIndices = [] }
	ti <- lift $ Vk.Image.create @() dvc imageInfo nil
	memRequirements <- lift $ Vk.Image.getMemoryRequirements dvc ti
	lift $ print memRequirements
	mti <- findMemoryType
		(Vk.Memory.M.requirementsMemoryTypeBits memRequirements)
		properties
	let	allocInfo = Vk.Memory.Image.AllocateInfo {
			Vk.Memory.Image.allocateInfoNext = Nothing,
			Vk.Memory.Image.allocateInfoMemoryTypeIndex = Vk.Memory.TypeIndex mti }
	tim <- lift $ Vk.Memory.Image.allocate @() dvc ti allocInfo nil
	lift $ Vk.Image.bindMemory dvc ti tim
	pure (ti, tim)

readRgba8 :: FilePath -> IO (Image PixelRGBA8)
readRgba8 fp = either error convertRGBA8 <$> readImage fp

transitionImageLayout ::
	Vk.Image.I -> Vk.Format.F -> Vk.Image.Layout -> Vk.Image.Layout ->
	Word32 -> ReaderT Global IO ()
transitionImageLayout image format oldLayout newLayout mipLevels = do
	commandBuffer <- beginSingleTimeCommands

	let	(bSrc, bDst, srcSt, dstSt) = case (oldLayout, newLayout) of
			(Vk.Image.LayoutUndefined,
				Vk.Image.LayoutTransferDstOptimal) -> (
					Vk.AccessFlagsZero,
					Vk.AccessTransferWriteBit,
					Vk.Ppl.StageTopOfPipeBit,
					Vk.Ppl.StageTransferBit )
			(Vk.Image.LayoutTransferDstOptimal,
				Vk.Image.LayoutShaderReadOnlyOptimal) -> (
					Vk.AccessTransferWriteBit,
					Vk.AccessShaderReadBit,
					Vk.Ppl.StageTransferBit,
					Vk.Ppl.StageFragmentShaderBit )
			(Vk.Image.LayoutUndefined,
				Vk.Image.LayoutDepthStencilAttachmentOptimal) ->
				(	Vk.AccessFlagsZero,
					Vk.AccessDepthStencilAttachmentReadBit
					.|.
					Vk.AccessDepthStencilAttachmentWriteBit,
					Vk.Ppl.StageTopOfPipeBit,
					Vk.Ppl.StageEarlyFragmentTestsBit )

			_ -> error "unsupported layout transition!"

		aspectMask = case newLayout of
			Vk.Image.LayoutDepthStencilAttachmentOptimal ->
				Vk.Image.AspectDepthBit .|.
				bool	Vk.Image.AspectFlagsZero
					Vk.Image.AspectStencilBit
					(hasStencilComponent format)
			_ -> Vk.Image.AspectColorBit

		barrier = Vk.Image.MemoryBarrier {
			Vk.Image.memoryBarrierNext = Nothing,
			Vk.Image.memoryBarrierOldLayout = oldLayout,
			Vk.Image.memoryBarrierNewLayout = newLayout,
			Vk.Image.memoryBarrierSrcQueueFamilyIndex =
				Vk.QueueFamily.Ignored,
			Vk.Image.memoryBarrierDstQueueFamilyIndex =
				Vk.QueueFamily.Ignored,
			Vk.Image.memoryBarrierImage = image,
			Vk.Image.memoryBarrierSubresourceRange =
				Vk.Image.SubresourceRange {
					Vk.Image.subresourceRangeAspectMask =
						aspectMask,
					Vk.Image.subresourceRangeBaseMipLevel =
						0,
					Vk.Image.subresourceRangeLevelCount =
						mipLevels,
					Vk.Image.subresourceRangeBaseArrayLayer
						= 0,
					Vk.Image.subresourceRangeLayerCount = 1
					},
			Vk.Image.memoryBarrierSrcAccessMask = bSrc,
			Vk.Image.memoryBarrierDstAccessMask = bDst
			}

	lift $ Vk.Cmd.pipelineBarrier @() @() @() commandBuffer
		srcSt dstSt Vk.DependencyFlagsZero [] [] [barrier]

	endSingleTimeCommands commandBuffer

copyBufferToImage ::
	Vk.Buffer.List.B Word8 -> Vk.Image.I -> Word32 -> Word32 ->
	ReaderT Global IO ()
copyBufferToImage buffer image wdt hgt = do
	commandBuffer <- beginSingleTimeCommands

	let	region = Vk.Buffer.M.ImageCopy {
			Vk.Buffer.M.imageCopyBufferOffset = 0,
			Vk.Buffer.M.imageCopyBufferRowLength = 0,
			Vk.Buffer.M.imageCopyBufferImageHeight = 0,

			Vk.Buffer.M.imageCopyImageSubresource =
				Vk.Image.SubresourceLayers {
					Vk.Image.subresourceLayersAspectMask =
						Vk.Image.AspectColorBit,
					Vk.Image.subresourceLayersMipLevel = 0,
					Vk.Image.subresourceLayersBaseArrayLayer
						= 0,
					Vk.Image.subresourceLayersLayerCount = 1
					},
			Vk.Buffer.M.imageCopyImageOffset = Vk.C.Offset3d {
				Vk.C.offset3dX = 0,
				Vk.C.offset3dY = 0,
				Vk.C.offset3dZ = 0 },
			Vk.Buffer.M.imageCopyImageExtent = Vk.C.Extent3d {
				Vk.C.extent3dWidth = wdt,
				Vk.C.extent3dHeight = hgt,
				Vk.C.extent3dDepth = 1 } }
	lift $ Vk.Cmd.List.copyBufferToImage commandBuffer buffer image
		Vk.Image.LayoutTransferDstOptimal [region]

	endSingleTimeCommands commandBuffer

createTextureImageView :: ReaderT Global IO ()
createTextureImageView = do
	ti <- readGlobal globalTextureImage
	ml <- readGlobal globalMipLevels
	tiv <- createImageView
		ti Vk.Format.R8g8b8a8Srgb Vk.Image.AspectColorBit ml
	writeGlobal globalTextureImageView tiv

createTextureSampler :: ReaderT Global IO ()
createTextureSampler = do
	pdvc <- readGlobal globalPhysicalDevice
	ml <- readGlobal globalMipLevels
	mnld <- readGlobal globalMinLod
	properties <- lift $ Vk.PhysicalDevice.getProperties pdvc
	let	limits = Vk.PhysicalDevice.propertiesLimits properties
		maxAnisotropy =
			Vk.PhysicalDevice.limitsMaxSamplerAnisotropy limits
		samplerInfo = Vk.Sampler.CreateInfo {
			Vk.Sampler.createInfoNext = Nothing,
			Vk.Sampler.createInfoFlags = Vk.Sampler.CreateFlagsZero,
			Vk.Sampler.createInfoMagFilter = Vk.FilterLinear,
			Vk.Sampler.createInfoMinFilter = Vk.FilterLinear,
			Vk.Sampler.createInfoAddressModeU =
				Vk.Sampler.AddressModeRepeat,
			Vk.Sampler.createInfoAddressModeV =
				Vk.Sampler.AddressModeRepeat,
			Vk.Sampler.createInfoAddressModeW =
				Vk.Sampler.AddressModeRepeat,
			Vk.Sampler.createInfoAnisotropyEnable = True,
			Vk.Sampler.createInfoMaxAnisotropy = maxAnisotropy,
			Vk.Sampler.createInfoBorderColor =
				Vk.BorderColorIntOpaqueBlack,
			Vk.Sampler.createInfoUnnormalizedCoordinates = False,
			Vk.Sampler.createInfoCompareEnable = False,
			Vk.Sampler.createInfoCompareOp = Vk.CompareOpAlways,
			Vk.Sampler.createInfoMipmapMode =
				Vk.Sampler.MipmapModeLinear,
			Vk.Sampler.createInfoMipLodBias = 0,
			Vk.Sampler.createInfoMinLod = min mnld (fromIntegral ml),
			Vk.Sampler.createInfoMaxLod = fromIntegral ml }
	dvc <- readGlobal globalDevice
	ts <- lift $ Vk.Sampler.create @() dvc samplerInfo nil
	writeGlobal globalTextureSampler ts

loadModel :: ReaderT Global IO ()
loadModel = do
	(vtcs, idcs) <-
		lift . verticesIndices =<< readGlobal globalModelFilePath
	let	(vtcs', idcs') = indexingVector vtcs
	lift do	putStrLn $ "vtcs : " ++ show (V.length vtcs)
		putStrLn $ "vtcs': " ++ show (V.length vtcs')
		putStrLn $ "idcs : " ++ show (V.length idcs)
		putStrLn $ "idcs': " ++ show (V.length idcs')
	writeGlobal globalVertices vtcs'
	writeGlobal globalIndices idcs'

createVertexBuffer :: ReaderT Global IO ()
createVertexBuffer = do
	dvc <- readGlobal globalDevice
	vtcs <- readGlobal globalVertices
	(sb, sbm) <- createBufferList (olength vtcs)
		Vk.Buffer.UsageTransferSrcBit $
		Vk.Memory.PropertyHostVisibleBit .|.
		Vk.Memory.PropertyHostCoherentBit
	lift $ Vk.Memory.List.writeMonoW dvc sbm Vk.Memory.M.MapFlagsZero vtcs
	(vb, vbm) <- createBufferList (olength vtcs)
		(Vk.Buffer.UsageTransferDstBit .|.
			Vk.Buffer.UsageVertexBufferBit)
		Vk.Memory.PropertyDeviceLocalBit
	copyBuffer sb vb (olength vtcs)
	lift do	Vk.Buffer.List.destroy dvc sb nil
		Vk.Memory.List.free dvc sbm nil
	writeGlobal globalVertexBuffer vb
	writeGlobal globalVertexBufferMemory vbm

createIndexBuffer :: ReaderT Global IO ()
createIndexBuffer = do
	dvc <- readGlobal globalDevice
	idcs <- readGlobal globalIndices
	(sb, sbm) <- createBufferList (olength idcs)
		Vk.Buffer.UsageTransferSrcBit $
		Vk.Memory.PropertyHostVisibleBit .|.
		Vk.Memory.PropertyHostCoherentBit
	lift $ Vk.Memory.List.writeMonoW dvc sbm Vk.Memory.M.MapFlagsZero idcs
	(ib, ibm) <- createBufferList (olength idcs)
		(Vk.Buffer.UsageTransferDstBit .|.
			Vk.Buffer.UsageIndexBufferBit)
		Vk.Memory.PropertyDeviceLocalBit
	copyBuffer sb ib (olength idcs)
	lift do	Vk.Buffer.List.destroy dvc sb nil
		Vk.Memory.List.free dvc sbm nil
	writeGlobal globalIndexBuffer ib
	writeGlobal globalIndexBufferMemory ibm

createBufferList :: Storable (Foreign.Storable.Generic.Wrap v) =>
	Int -> Vk.Buffer.UsageFlags -> Vk.Memory.PropertyFlags ->
	ReaderT Global IO (Vk.Buffer.List.B v, Vk.Device.MemoryList v)
createBufferList ln usage properties = do
	let	bufferInfo = Vk.Buffer.List.CreateInfo {
			Vk.Buffer.List.createInfoNext = Nothing,
			Vk.Buffer.List.createInfoFlags =
				Vk.Buffer.CreateFlagsZero,
			Vk.Buffer.List.createInfoLength = ln,
			Vk.Buffer.List.createInfoUsage = usage,
			Vk.Buffer.List.createInfoSharingMode =
				Vk.SharingModeExclusive,
			Vk.Buffer.List.createInfoQueueFamilyIndices = [] }
	dvc <- readGlobal globalDevice
	b <- lift $ Vk.Buffer.List.create @() dvc bufferInfo nil
	memRequirements <- lift $ Vk.Buffer.List.getMemoryRequirements dvc b
	mti <- findMemoryType
		(Vk.Memory.M.requirementsMemoryTypeBits memRequirements)
		properties
	let	allocInfo = Vk.Memory.List.AllocateInfo {
			Vk.Memory.List.allocateInfoNext = Nothing,
			Vk.Memory.List.allocateInfoMemoryTypeIndex = Vk.Memory.TypeIndex mti }
	bm <- lift $ Vk.Memory.List.allocate @() dvc b allocInfo nil
	lift $ Vk.Buffer.List.bindMemory dvc b bm
	pure (b, bm)

createBufferAtom :: Storable (Foreign.Storable.Generic.Wrap v) =>
	Vk.Buffer.UsageFlags -> Vk.Memory.PropertyFlags ->
	ReaderT Global IO (Vk.Buffer.Atom.B v, Vk.Device.MemoryAtom v)
createBufferAtom usage properties = do
	dvc <- readGlobal globalDevice
	let	bufferInfo = Vk.Buffer.Atom.CreateInfo {
			Vk.Buffer.Atom.createInfoNext = Nothing,
			Vk.Buffer.Atom.createInfoFlags =
				Vk.Buffer.CreateFlagsZero,
			Vk.Buffer.Atom.createInfoUsage = usage,
			Vk.Buffer.Atom.createInfoSharingMode =
				Vk.SharingModeExclusive,
			Vk.Buffer.Atom.createInfoQueueFamilyIndices = [] }
	b <- lift $ Vk.Buffer.Atom.create @() dvc bufferInfo nil
	memRequirements <- lift $ Vk.Buffer.Atom.getMemoryRequirements dvc b
	mti <- findMemoryType
		(Vk.Memory.M.requirementsMemoryTypeBits memRequirements)
		properties
	let	allocInfo = Vk.Memory.Atom.AllocateInfo {
			Vk.Memory.Atom.allocateInfoNext = Nothing,
			Vk.Memory.Atom.allocateInfoMemoryTypeIndex = Vk.Memory.TypeIndex mti }
	bm <- lift $ Vk.Memory.Atom.allocate @() dvc b allocInfo nil
	lift $ Vk.Buffer.Atom.bindMemory dvc b bm
	pure (b, bm)

beginSingleTimeCommands :: ReaderT Global IO (Vk.CommandBuffer.C v)
beginSingleTimeCommands = do
	cp <- readGlobal globalCommandPool
	dvc <- readGlobal globalDevice
	let	allocInfo = Vk.CommandBuffer.AllocateInfo {
			Vk.CommandBuffer.allocateInfoNext = Nothing,
			Vk.CommandBuffer.allocateInfoLevel =
				Vk.CommandBuffer.LevelPrimary,
			Vk.CommandBuffer.allocateInfoCommandPool = cp,
			Vk.CommandBuffer.allocateInfoCommandBufferCount = 1 }
		beginInfo = Vk.CommandBuffer.BeginInfo {
			Vk.CommandBuffer.beginInfoNext = Nothing,
			Vk.CommandBuffer.beginInfoFlags =
				Vk.CommandBuffer.UsageOneTimeSubmitBit,
			Vk.CommandBuffer.beginInfoInheritanceInfo = Nothing }
	[commandBuffer] <- lift $ Vk.CommandBuffer.allocate @() dvc allocInfo
	lift $ Vk.CommandBuffer.begin @() @() commandBuffer beginInfo
	pure commandBuffer

endSingleTimeCommands :: Vk.CommandBuffer.C v -> ReaderT Global IO ()
endSingleTimeCommands commandBuffer = do
	dvc <- readGlobal globalDevice
	gq <- readGlobal globalGraphicsQueue
	cp <- readGlobal globalCommandPool
	let	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks = [],
			Vk.submitInfoCommandBuffers = [commandBuffer],
			Vk.submitInfoSignalSemaphores = [] }
	lift do	Vk.CommandBuffer.end commandBuffer
		Vk.Queue.submit' @() gq [submitInfo] Nothing
		Vk.Queue.waitIdle gq
		Vk.CommandBuffer.freeCs dvc cp [commandBuffer]

copyBuffer :: Storable (Foreign.Storable.Generic.Wrap v) =>
	Vk.Buffer.List.B v -> Vk.Buffer.List.B v -> Int -> ReaderT Global IO ()
copyBuffer srcBuffer dstBuffer ln = do
	let	copyRegion = Vk.Buffer.List.Copy {
			Vk.Buffer.List.copyLength = ln }
	commandBuffer <- beginSingleTimeCommands
	lift $ Vk.Cmd.List.copyBuffer commandBuffer srcBuffer dstBuffer copyRegion
	endSingleTimeCommands commandBuffer

findMemoryType :: Vk.Memory.M.TypeBits -> Vk.Memory.PropertyFlags ->
	ReaderT Global IO Word32
findMemoryType typeFilter properties = do
	phdvc <- readGlobal globalPhysicalDevice
	memProperties <- lift $ Vk.PhysicalDevice.getMemoryProperties phdvc
	let	r = find (suitable typeFilter properties memProperties)
			[0 .. length (
				Vk.PhysicalDevice.memoryPropertiesMemoryTypes
					memProperties) - 1]
	pure $ maybe
		(error "failed to find suitable memory type!") fromIntegral r

suitable :: Vk.Memory.M.TypeBits -> Vk.Memory.PropertyFlags ->
	Vk.PhysicalDevice.MemoryProperties -> Int -> Bool
suitable typeFilter properties memProperties i =
	(typeFilter .&. Vk.Memory.M.TypeBits 1 `shiftL` i /= zeroBits) &&
	(Vk.Memory.M.mTypePropertyFlags
		(snd $ Vk.PhysicalDevice.memoryPropertiesMemoryTypes memProperties !!
			i) .&. properties == properties)

size :: forall a . SizeAlignmentList a => a -> Size
size _ = fst (wholeSizeAlignment @a)

createUniformBuffers :: ReaderT Global IO ()
createUniformBuffers = do
	(ubs, ubms) <- unzip
		<$> maxFramesInFlight `replicateM` (createBufferAtom
			Vk.Buffer.UsageUniformBufferBit $
			Vk.Memory.PropertyHostVisibleBit .|.
			Vk.Memory.PropertyHostCoherentBit)

	writeGlobal globalUniformBuffers ubs
	writeGlobal globalUniformBuffersMemory ubms

createDescriptorPool :: ReaderT Global IO ()
createDescriptorPool = do
	let	poolSize0 = Vk.DscPool.Size {
			Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
			Vk.DscPool.sizeDescriptorCount =
				fromIntegral maxFramesInFlight }
		poolSize1 = Vk.DscPool.Size {
			Vk.DscPool.sizeType = Vk.Dsc.TypeCombinedImageSampler,
			Vk.DscPool.sizeDescriptorCount =
				fromIntegral maxFramesInFlight }
		poolInfo = Vk.DscPool.CreateInfo {
			Vk.DscPool.createInfoNext = Nothing,
			Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFlagsZero,
			Vk.DscPool.createInfoMaxSets =
				fromIntegral maxFramesInFlight,
			Vk.DscPool.createInfoPoolSizes = [poolSize0, poolSize1] }
	dvc <-readGlobal globalDevice
	writeGlobal globalDescriptorPool
		=<< lift (Vk.DscPool.create @() dvc poolInfo nil)

createDescriptorSets :: ReaderT Global IO ()
createDescriptorSets = do
	layouts <- replicate maxFramesInFlight
		<$> readGlobal globalDescriptorSetLayout
	dp <- readGlobal globalDescriptorPool
	let	allocInfo = Vk.DscSet.AllocateInfo {
			Vk.DscSet.allocateInfoNext = Nothing,
			Vk.DscSet.allocateInfoDescriptorPool = dp,
			Vk.DscSet.allocateInfoDescriptorSetCountOrSetLayouts =
				Right layouts }
	dvc <- readGlobal globalDevice
	dss <- lift $ Vk.DscSet.allocateSs @() dvc allocInfo
	writeGlobal globalDescriptorSets dss
	ubs <- readGlobal globalUniformBuffers
	tiv <- readGlobal globalTextureImageView
	ts <- readGlobal globalTextureSampler
	for_ [0 .. maxFramesInFlight - 1] \i -> do
		let	bufferInfo = Vk.Dsc.BufferInfo {
				Vk.Dsc.bufferInfoBuffer = ubs !! i,
				Vk.Dsc.bufferInfoOffset = 0 }
			imageInfo = Vk.Dsc.ImageInfo {
				Vk.Dsc.imageInfoImageLayout =
					Vk.Image.LayoutShaderReadOnlyOptimal,
				Vk.Dsc.imageInfoImageView = tiv,
				Vk.Dsc.imageInfoSampler = ts }
			descriptorWrite0 = Vk.DscSet.Write {
				Vk.DscSet.writeNext = Nothing,
				Vk.DscSet.writeDstSet = dss !! i,
				Vk.DscSet.writeDstBinding = 0,
				Vk.DscSet.writeDstArrayElement = 0,
				Vk.DscSet.writeDescriptorType =
					Vk.Dsc.TypeUniformBuffer,
				Vk.DscSet.writeImageBufferInfoTexelBufferViews =
					Right $ Vk.DscSet.BufferInfos
						[bufferInfo] }
			descriptorWrite1 = Vk.DscSet.Write {
				Vk.DscSet.writeNext = Nothing,
				Vk.DscSet.writeDstSet = dss !! i,
				Vk.DscSet.writeDstBinding = 1,
				Vk.DscSet.writeDstArrayElement = 0,
				Vk.DscSet.writeDescriptorType =
					Vk.Dsc.TypeCombinedImageSampler,
				Vk.DscSet.writeImageBufferInfoTexelBufferViews =
					Right $ Vk.DscSet.ImageInfos [imageInfo]
				}
		lift $ Vk.DscSet.updateSs @() @() dvc
			[descriptorWrite0, descriptorWrite1] []

createCommandBuffers :: ReaderT Global IO ()
createCommandBuffers = do
	cp <- readGlobal globalCommandPool
	let	allocInfo = Vk.CommandBuffer.AllocateInfo {
			Vk.CommandBuffer.allocateInfoNext = Nothing,
			Vk.CommandBuffer.allocateInfoCommandPool = cp,
			Vk.CommandBuffer.allocateInfoLevel =
				Vk.CommandBuffer.LevelPrimary,
			Vk.CommandBuffer.allocateInfoCommandBufferCount =
				fromIntegral maxFramesInFlight }
	dvc <- readGlobal globalDevice
	writeGlobal globalCommandBuffers
		=<< lift (Vk.CommandBuffer.allocate @() dvc allocInfo)

createSyncObjects :: ReaderT Global IO ()
createSyncObjects = do
	let	semaphoreInfo = Vk.Semaphore.CreateInfo {
			Vk.Semaphore.createInfoNext = Nothing,
			Vk.Semaphore.createInfoFlags =
				Vk.Semaphore.CreateFlagsZero }
		fenceInfo = Vk.Fence.CreateInfo {
			Vk.Fence.createInfoNext = Nothing,
			Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }
	dvc <- readGlobal globalDevice
	writeGlobal globalImageAvailableSemaphores
		=<< lift (replicateM maxFramesInFlight
			$ Vk.Semaphore.create @() dvc semaphoreInfo nil)
	writeGlobal globalRenderFinishedSemaphores
		=<< lift (replicateM maxFramesInFlight
			$ Vk.Semaphore.create @() dvc semaphoreInfo nil)
	writeGlobal globalInFlightFences
		=<< lift (replicateM maxFramesInFlight
			$ Vk.Fence.create @() dvc fenceInfo nil)

recordCommandBuffer :: Vk.CommandBuffer.C (Solo (AddType Vertex 'Vk.VertexInput.RateVertex)) -> Word32 -> ReaderT Global IO ()
recordCommandBuffer cb imageIndex = do
	idcs <- readGlobal globalIndices
	let	beginInfo = Vk.CommandBuffer.BeginInfo {
			Vk.CommandBuffer.beginInfoNext = Nothing,
			Vk.CommandBuffer.beginInfoFlags =
				Vk.CommandBuffer.UsageFlagsZero,
			Vk.CommandBuffer.beginInfoInheritanceInfo = Nothing }
	lift $ Vk.CommandBuffer.begin @() @() cb beginInfo
	rp <- readGlobal globalRenderPass
	scfbs <- readGlobal globalSwapChainFramebuffers
	sce <- readGlobal globalSwapChainExtent
	let	renderPassInfo = Vk.RenderPass.BeginInfo {
			Vk.RenderPass.beginInfoNext = Nothing,
			Vk.RenderPass.beginInfoRenderPass = rp,
			Vk.RenderPass.beginInfoFramebuffer =
				scfbs `genericIndex` imageIndex,
			Vk.RenderPass.beginInfoRenderArea = Vk.C.Rect2d {
				Vk.C.rect2dOffset = Vk.C.Offset2d 0 0,
				Vk.C.rect2dExtent = sce },
			Vk.RenderPass.beginInfoClearValues = [
				Vk.ClearValueColor
					. fromJust $ rgbaDouble 0 0 0 1,
				Vk.ClearValueDepthStencil
					$ Vk.C.ClearDepthStencilValue 1 0 ] }
	lift $ Vk.Cmd.M.beginRenderPass @()
		@('Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32)
		cb renderPassInfo Vk.Subpass.ContentsInline
	lift . Vk.Cmd.M.bindPipeline cb Vk.Ppl.BindPointGraphics
		=<< readGlobal globalGraphicsPipeline
	vb <- readGlobal globalVertexBuffer
	ib <- readGlobal globalIndexBuffer
	lift do	Vk.Cmd.List.bindVertexBuffers cb
			((vb, 0) :!: BNil :: BList '[Vertex])
		Vk.Cmd.List.bindIndexBuffer cb ib Vk.IndexTypeUint32

	dss <- readGlobal globalDescriptorSets
	ppll <- readGlobal globalPipelineLayout
	cf <- readGlobal globalCurrentFrame
	lift do	Vk.Cmd.bindDescriptorSets cb Vk.Ppl.BindPointGraphics ppll 0
			[dss !! cf] []
		Vk.Cmd.drawIndexed cb (fromIntegral $ olength idcs) 1 0 0 0
		Vk.Cmd.M.endRenderPass cb
		Vk.CommandBuffer.end cb

mainLoop :: ReaderT Global IO ()
mainLoop = do
	w <- fromJust <$> readGlobal globalWindow
	st <- lift getCurrentTime
	fix \loop -> bool (pure ()) loop =<< do
		lift GlfwB.pollEvents
		g <- ask
		lift $ catchAndRecreateSwapChain g $ drawFrame st `runReaderT` g
		not <$> lift (GlfwB.windowShouldClose w)
	lift . Vk.Device.waitIdle =<< readGlobal globalDevice

drawFrame :: UTCTime -> ReaderT Global IO ()
drawFrame st = do
	cf <- readGlobal globalCurrentFrame
	dvc <- readGlobal globalDevice
	iff <- (!! cf) <$> readGlobal globalInFlightFences
	lift $ Vk.Fence.waitForFs dvc [iff] True maxBound
	sc <- readGlobal globalSwapChain
	ias <- (!! cf) <$> readGlobal globalImageAvailableSemaphores
	imageIndex <- lift $ Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc uint64Max (Just ias) Nothing
	lift $ Vk.Fence.resetFs dvc [iff]
	cb <- (!! cf) <$> readGlobal globalCommandBuffers
	lift $ Vk.CommandBuffer.reset cb Vk.CommandBuffer.ResetFlagsZero
	recordCommandBuffer cb imageIndex
	rfs <- (!! cf) <$> readGlobal globalRenderFinishedSemaphores
	updateUniformBuffer st $ fromIntegral cf
	let	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks =
				[(ias, Vk.Ppl.StageColorAttachmentOutputBit)],
			Vk.submitInfoCommandBuffers = [cb],
			Vk.submitInfoSignalSemaphores = [rfs] }
	gq <- readGlobal globalGraphicsQueue
	lift . Vk.Queue.submit' @() gq [submitInfo] $ Just iff
	let	presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = Nothing,
			Vk.Khr.presentInfoWaitSemaphores = [rfs],
			Vk.Khr.presentInfoSwapchainImageIndices =
				[(sc, imageIndex)] }
	pq <- readGlobal globalPresentQueue
	g <- ask
	lift . catchAndRecreateSwapChain g . catchAndSerialize
		$ Vk.Khr.queuePresent @() pq presentInfo
	writeGlobal globalCurrentFrame $ (cf + 1) `mod` maxFramesInFlight

updateUniformBuffer :: UTCTime -> Word32 -> ReaderT Global IO ()
updateUniformBuffer startTime currentImage = do
	currentTime <- lift getCurrentTime
	sce <- readGlobal globalSwapChainExtent
	let	time :: Float = realToFrac $ currentTime `diffUTCTime` startTime
		ubo = UniformBufferObject {
			uniformBufferObjectModel = Cglm.glmRotate
				Cglm.glmMat4Identity
				(time * Cglm.glmRad 90)
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
	dvc <- readGlobal globalDevice
	ubm <- readGlobal globalUniformBuffersMemory
	lift $ Vk.Memory.Atom.write dvc
		(ubm !! fromIntegral currentImage) Vk.Memory.M.MapFlagsZero ubo

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreateSwapChain :: Global -> IO () -> IO ()
catchAndRecreateSwapChain g act = catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	(\_ -> do
		fbr <- readIORef $ globalFramebufferResized g
		when fbr do
			writeIORef (globalFramebufferResized g) False
			recreateSwapChain `runReaderT` g)

doWhile_ :: IO Bool -> IO ()
doWhile_ act = (`when` doWhile_ act) =<< act

recreateSwapChain :: ReaderT Global IO ()
recreateSwapChain = do
	w <- fromJust <$> readGlobal globalWindow
	lift do	(wdth, hght) <- GlfwB.getFramebufferSize w
		when (wdth == 0 || hght == 0) $ doWhile_ do
			GlfwB.waitEvents
			(wd, hg) <- GlfwB.getFramebufferSize w
			pure $ wd == 0 || hg == 0
	dvc <- readGlobal globalDevice
	lift $ Vk.Device.waitIdle dvc

	cleanupSwapChain

	createSwapChain
	createImageViews
	createRenderPass
	createGraphicsPipeline
	createColorResources
	createDepthResources
	createFramebuffers

cleanupSwapChain :: ReaderT Global IO ()
cleanupSwapChain = do
	dvc <- readGlobal globalDevice

	civ <- readGlobal globalColorImageView
	lift $ Vk.ImageView.destroy dvc civ nil

	ci <- readGlobal globalColorImage
	lift $ Vk.Image.destroy dvc ci nil

	cim <- readGlobal globalColorImageMemory
	lift $ Vk.Memory.Image.free dvc cim nil

	divw <- readGlobal globalDepthImageView
	lift $ Vk.ImageView.destroy dvc divw nil

	di <- readGlobal globalDepthImage
	lift $ Vk.Image.destroy dvc di nil

	dim <- readGlobal globalDepthImageMemory
	lift $ Vk.Memory.Image.free dvc dim nil

	scfbs <- readGlobal globalSwapChainFramebuffers
	lift $ flip (Vk.Framebuffer.destroy dvc) nil `mapM_` scfbs
	grppl <- readGlobal globalGraphicsPipeline
	lift $ Vk.Ppl.Graphics.destroy dvc grppl nil
	ppllyt <- readGlobal globalPipelineLayout
	lift $ Vk.Ppl.Layout.destroy dvc ppllyt nil
	rp <- readGlobal globalRenderPass
	lift $ Vk.RenderPass.destroy dvc rp nil
	scivs <- readGlobal globalSwapChainImageViews
	lift $ flip (Vk.ImageView.destroy dvc) nil `mapM_` scivs
	lift . (\sc -> Vk.Khr.Swapchain.destroy dvc sc nil)
		=<< readGlobal globalSwapChain

cleanup :: ReaderT Global IO ()
cleanup = do
	cleanupSwapChain
	dvc <- readGlobal globalDevice

	ts <- readGlobal globalTextureSampler
	lift $ Vk.Sampler.destroy dvc ts nil

	tiv <- readGlobal globalTextureImageView
	lift $ Vk.ImageView.destroy dvc tiv nil

	ti <- readGlobal globalTextureImage
	lift $ Vk.Image.destroy dvc ti nil

	tim <- readGlobal globalTextureImageMemory
	lift $ Vk.Memory.Image.free dvc tim nil

	ubs <- readGlobal globalUniformBuffers
	lift $ flip (Vk.Buffer.Atom.destroy dvc) nil `mapM_` ubs

	ubms <- readGlobal globalUniformBuffersMemory
	lift $ flip (Vk.Memory.Atom.free dvc) nil `mapM_` ubms

	dp <- readGlobal globalDescriptorPool
	lift $ Vk.DscPool.destroy dvc dp nil

	dsl <- readGlobal globalDescriptorSetLayout
	lift $ Vk.DscSet.Lyt.destroy dvc dsl nil

	vb <- readGlobal globalVertexBuffer
	vbm <- readGlobal globalVertexBufferMemory
	lift do	Vk.Buffer.List.destroy dvc vb nil
		Vk.Memory.List.free dvc vbm nil
	ib <- readGlobal globalIndexBuffer
	ibm <- readGlobal globalIndexBufferMemory
	lift do	Vk.Buffer.List.destroy dvc ib nil
		Vk.Memory.List.free dvc ibm nil

	lift . (flip (Vk.Semaphore.destroy dvc) nil `mapM_`)
		=<< readGlobal globalImageAvailableSemaphores
	lift . (flip (Vk.Semaphore.destroy dvc) nil `mapM_`)
		=<< readGlobal globalRenderFinishedSemaphores
	lift . (flip (Vk.Fence.destroy dvc) nil `mapM_`)
		=<< readGlobal globalInFlightFences
	lift . flip (Vk.CommandPool.destroy dvc) nil
		=<< readGlobal globalCommandPool
	lift $ Vk.Device.destroy dvc nil

	ist <- readGlobal globalInstance
	when enableValidationLayers
		. lift . flip (Vk.Ext.DebugUtils.Messenger.destroy ist) nil
		=<< readGlobal globalDebugMessenger
	lift . flip (Vk.Khr.Surface.destroy ist) nil
		=<< readGlobal globalSurface
	lift $ Vk.Instance.destroy ist nil

	lift . GlfwB.destroyWindow . fromJust =<< readGlobal globalWindow
	lift GlfwB.terminate

data UniformBufferObject = UniformBufferObject {
	uniformBufferObjectModel :: Cglm.Mat4,
	uniformBufferObjectView :: Cglm.Mat4,
	uniformBufferObjectProj :: Cglm.Mat4 }
	deriving (Show, Generic)

instance SizeAlignmentList UniformBufferObject
instance Foreign.Storable.Generic.G UniformBufferObject

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
//	outColor = vec4(fragColor, 0.0, 1.0);
//	outColor = texture(texSampler, fragTexCoord * 2.0);
	outColor = vec4(fragColor * texture(texSampler, fragTexCoord).rgb, 1.0);
}

|]
