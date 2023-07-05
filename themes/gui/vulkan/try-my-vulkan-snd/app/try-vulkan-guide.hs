{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Generics
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.Storable.SizeAlignment
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.Foldable
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List qualified as TLength
import Data.TypeLevel.List qualified as TpLvlLst
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Proxy
import Data.Bool
import Data.Maybe
import Data.List
import Data.IORef
import Data.List.Length
import Data.Word
import Data.Color
import System.Environment

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS
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
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Enum as Vk
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
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Image.Enum as Vk.Img
import qualified Gpu.Vulkan.Image as Vk.Img.M
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
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified "try-my-vulkan-snd" Gpu.Vulkan.CommandPool.Enum as Vk.CmdPl
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBffr
import qualified "try-my-vulkan-snd" Gpu.Vulkan.CommandBuffer.Enum as Vk.CmdBffr
import qualified Gpu.Vulkan.CommandBuffer.Middle as Vk.CmdBffr.M
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.Fence.Enum as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Buffer.Enum as Vk.Bffr
import qualified Gpu.Vulkan.Memory.Middle as Vk.Mem.M
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mem
import qualified Gpu.Vulkan.Memory.AllocateInfo as Vk.Dvc.Mem
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Memory.Kind as Vk.Mem.K
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.Cmd as Vk.Cmd
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as Vk.Ppl.DptStnSt
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.DscSetLyt
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Descriptor.Enum as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified "try-my-vulkan-snd" Gpu.Vulkan.DescriptorPool.Enum as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.DescriptorSet.UpdateDynamicLengths as Vk.DscSet.T
import qualified Gpu.Vulkan.DescriptorSet.BindingAndArrayElem as Vk.DscSet.T

import qualified Codec.Wavefront.ReadOld as W
import Tools

main :: IO ()
main = do
	[objfile] <- getArgs
	obj <- BS.readFile objfile
	frszd <- newFramebufferResized
	(`withWindow` frszd) \win -> createInstance \ist -> do
		if enableValidationLayers
			then setupDebugMessenger ist $ const $ run win ist frszd obj
			else run win ist frszd obj

type FramebufferResized = IORef Bool

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

frashRate :: Num n => n
frashRate = 2

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
	when enableValidationLayers $ bool (error msg) (pure ())
		=<< null . (validationLayers \\)
				. (Vk.layerPropertiesLayerName <$>)
			<$> Vk.Ist.M.enumerateLayerProperties
	exts <- bool id (Vk.Ext.DbgUtls.extensionName :) enableValidationLayers
		<$> ((cstrToText `mapM`) =<< Glfw.getRequiredInstanceExtensions)
	Vk.Ist.create (crInfo exts) nil' f
	where
	msg = "validation layers requested, but not available!"
	crInfo :: [Txt.Text] -> Vk.Ist.M.CreateInfo
		('Just (Vk.Ext.DbgUtls.Msngr.CreateInfo 'Nothing '[] () () () ())) 'Nothing
	crInfo exts = Vk.Ist.M.CreateInfo {
		Vk.Ist.M.createInfoNext = TMaybe.J debugMessengerCreateInfo,
		Vk.Ist.M.createInfoFlags = def,
		Vk.Ist.M.createInfoApplicationInfo = Just appInfo,
		Vk.Ist.M.createInfoEnabledLayerNames =
			bool [] validationLayers enableValidationLayers,
		Vk.Ist.M.createInfoEnabledExtensionNames = exts }
	appInfo = Vk.ApplicationInfo {
		Vk.applicationInfoNext = TMaybe.N,
		Vk.applicationInfoApplicationName =
			"Example Vulkan Application",
		Vk.applicationInfoApplicationVersion =
			Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoEngineName = "No Engine",
		Vk.applicationInfoEngineVersion = Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoApiVersion = Vk.apiVersion_1_0 }

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

print3 :: (Show a, Show b, Show c) => (a, b, c) -> IO ()
print3 (x, y, z) = print x >> print y >> print z

run :: Glfw.Window -> Vk.Ist.I si -> FramebufferResized -> BS.ByteString -> IO ()
run w ist g obj = let
	cnt = W.countV' obj
	(vs, ns, fs) = W.readV' (W.countVertex cnt) (W.countNormal cnt) (W.countFace cnt) obj in
	print cnt >>
--	print3 (takePosNormalFace 10 vnf) >>
	let	vns = V.map positionNormalToVertex $ W.facePosNormal vs ns fs in
--	print vns >>
	Glfw.createWindowSurface ist w nil' \sfc ->
	pickPhysicalDevice ist sfc >>= \(phdv, qfis) ->
	putStrLn "MIN ALIGN" >>
	(print . Vk.PhDvc.limitsMinUniformBufferOffsetAlignment . Vk.PhDvc.propertiesLimits =<< Vk.PhDvc.getProperties phdv) >>
	createDevice phdv qfis \dv gq pq ->
	createSwapchain w sfc phdv qfis dv
		\(sc :: Vk.Khr.Swapchain.SNew ss scifmt) ext ->
	Vk.Khr.Swapchain.getImagesNew dv sc >>= \imgs ->
	createImageViews dv imgs \scivs ->
	findDepthFormat phdv >>= \dptfmt ->
	Vk.T.formatToType dptfmt \(_ :: Proxy dptfmt) ->
	createDescriptorSetLayout dv \cmdslyt ->
	createRenderPass @scifmt @dptfmt dv \rp ->
	createPipelineLayout dv cmdslyt \ppllyt ->
	createGraphicsPipeline dv ext rp ppllyt 0 \gpl0 ->
	createGraphicsPipeline dv ext rp ppllyt 1 \gpl1 ->
	createCommandPool qfis dv \cp ->
	createDepthResources phdv dv gq cp ext \dptImg dptImgMem dptImgVw ->
	createFramebuffers dv ext rp scivs dptImgVw \fbs ->
	createCameraBuffers phdv dv cmdslyt maxFramesInFlight \cmlyts cmbs cmms ->
	createSceneBuffer phdv dv \scnb scnm ->
	createVertexBuffer phdv dv gq cp vns \vb ->
	createVertexBuffer phdv dv gq cp triangle \vbtri ->
	createCommandBuffers dv cp \cbs ->
	createSyncObjects dv \sos ->
	createDescriptorPool dv \cmdp ->
	createDescriptorSets dv cmdp cmbs cmlyts scnb \cmds ->
	mainLoop g w sfc phdv qfis dv gq pq sc ext scivs rp ppllyt
		gpl0 gpl1 cp (dptImg, dptImgMem, dptImgVw) fbs vb vbtri cbs sos cmbs cmms scnm cmds (fromIntegral $ V.length vns)

pickPhysicalDevice :: Vk.Ist.I si ->
	Vk.Khr.Surface.S ss -> IO (Vk.PhDvc.P, QueueFamilyIndices)
pickPhysicalDevice ist sfc = do
	dvcs <- Vk.PhDvc.enumerate ist
	when (null dvcs) $ error "failed to find GPUs with Gpu.Vulkan support!"
	findPhysicalDevice (`isPhysicalDeviceSuitable` sfc) dvcs >>= \case
		Just ph -> pure ph
		Nothing -> error "failed to find a suitable GPU!"

findPhysicalDevice :: Monad m =>
	(Vk.PhDvc.P -> m (Maybe a)) -> [Vk.PhDvc.P] -> m (Maybe (Vk.PhDvc.P, a))
findPhysicalDevice prd = \case
	[] -> pure Nothing
	p : ps -> prd p >>= \case
		Nothing -> findPhysicalDevice prd ps; Just x -> pure $ Just (p, x)

isPhysicalDeviceSuitable ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO (Maybe QueueFamilyIndices)
isPhysicalDeviceSuitable ph sfc = findQueueFamilies ph sfc >>= \is ->
	checkDeviceExtensionSupport ph >>= bool (pure Nothing)
		((<$> querySwapchainSupport ph sfc) \spp ->
			bool (completeQueueFamilies is) Nothing
				$ null (formats spp) || null (presentModes spp))

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

data SwapchainSupportDetails = SwapchainSupportDetails {
	capabilities :: Vk.Khr.Surface.M.Capabilities,
	formats :: [Vk.Khr.Surface.M.Format],
	presentModes :: [Vk.Khr.PresentMode] }

querySwapchainSupport ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO SwapchainSupportDetails
querySwapchainSupport dvc sfc = SwapchainSupportDetails
	<$> Vk.Khr.Surface.PhysicalDevice.getCapabilities dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getFormats dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getPresentModes dvc sfc

createDevice :: Vk.PhDvc.P -> QueueFamilyIndices ->
	(forall sd . Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> IO a) -> IO a
createDevice ph qfis f = mkHeteroParList qcrInfo qfs \qs ->
	Vk.Dvc.create ph (crInfo qs) nil' \dv -> do
		gq <- Vk.Dvc.getQueue dv (graphicsFamily qfis) 0
		pq <- Vk.Dvc.getQueue dv (presentFamily qfis) 0
		f dv gq pq
	where
	qfs = nub [graphicsFamily qfis, presentFamily qfis]
	crInfo qs = Vk.Dvc.M.CreateInfo {
		Vk.Dvc.M.createInfoNext = TMaybe.N,
		Vk.Dvc.M.createInfoFlags = def,
		Vk.Dvc.M.createInfoQueueCreateInfos = qs,
		Vk.Dvc.M.createInfoEnabledLayerNames =
			bool [] validationLayers enableValidationLayers,
		Vk.Dvc.M.createInfoEnabledExtensionNames = deviceExtensions,
		Vk.Dvc.M.createInfoEnabledFeatures = Just def }
	qcrInfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }

mkHeteroParList :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] ->
	(forall ss . HeteroParList.ToListWithCM' WithPoked TMaybe.M ss => HeteroParList.PL t ss -> b) -> b
mkHeteroParList _k [] f = f HeteroParList.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

createSwapchain :: Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> (forall ss scfmt .
		Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swapchain.SNew ss scfmt -> Vk.Extent2d ->
		IO a) -> IO a
createSwapchain win sfc ph qfis dv f = do
	spp <- querySwapchainSupport ph sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	fmt = Vk.Khr.Surface.M.formatFormat . chooseSwapSurfaceFormat $ formats spp
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		let	crInfo = mkSwapchainCreateInfo sfc qfis spp ext
		Vk.Khr.Swapchain.createNew @'Nothing @fmt
			dv crInfo nil' \sc -> f sc ext

recreateSwapchain :: Vk.T.FormatToValue scfmt =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Khr.Swapchain.SNew ssc scfmt ->
	IO Vk.Extent2d
recreateSwapchain win sfc ph qfis0 dv sc = do
	spp <- querySwapchainSupport ph sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	crInfo = mkSwapchainCreateInfo sfc qfis0 spp ext
	ext <$ Vk.Khr.Swapchain.recreateNew @'Nothing dv crInfo nil' sc

mkSwapchainCreateInfo :: Vk.Khr.Surface.S ss -> QueueFamilyIndices ->
	SwapchainSupportDetails -> Vk.Extent2d ->
	Vk.Khr.Swapchain.CreateInfoNew 'Nothing ss fmt
mkSwapchainCreateInfo sfc qfis0 spp ext =
	Vk.Khr.Swapchain.CreateInfoNew {
		Vk.Khr.Swapchain.createInfoNextNew = TMaybe.N,
		Vk.Khr.Swapchain.createInfoFlagsNew = zeroBits,
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
chooseSwapPresentMode = const Vk.Khr.PresentModeFifo
--	fromMaybe Vk.Khr.PresentModeFifo . find (== Vk.Khr.PresentModeMailbox)

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
	Vk.Dvc.D sd -> [Vk.Img.Binded ss ss nm fmt] ->
	(forall si . HeteroParList.PL (Vk.ImgVw.I nm fmt) si -> IO a) -> IO a
createImageViews _dvc [] f = f HeteroParList.Nil
createImageViews dvc (sci : scis) f =
	createImageView dvc sci Vk.Img.AspectColorBit \sciv ->
	createImageViews dvc scis \scivs -> f $ sciv :** scivs

recreateImageViews :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss nm scfmt] -> HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis -> IO ()
recreateImageViews _dvc [] HeteroParList.Nil = pure ()
recreateImageViews dvc (sci : scis) (iv :** ivs) =
	Vk.ImgVw.recreateNew dvc (mkImageViewCreateInfo sci Vk.Img.AspectColorBit) nil' iv >>
	recreateImageViews dvc scis ivs
recreateImageViews _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

createImageView :: forall ivfmt sd si sm nm ifmt a .
	Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	Vk.Img.AspectFlags ->
	(forall siv . Vk.ImgVw.I nm ivfmt siv -> IO a) -> IO a
createImageView dvc timg asps f =
	Vk.ImgVw.createNew dvc (mkImageViewCreateInfo timg asps) nil' f

recreateImageView :: Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	Vk.Img.AspectFlags ->
	Vk.ImgVw.I nm ivfmt s -> IO ()
recreateImageView dvc timg asps iv =
	Vk.ImgVw.recreateNew dvc (mkImageViewCreateInfo timg asps) nil' iv

mkImageViewCreateInfo ::
	Vk.Img.Binded sm si nm ifmt -> Vk.Img.AspectFlags ->
	Vk.ImgVw.CreateInfoNew 'Nothing sm si nm ifmt ivfmt
mkImageViewCreateInfo sci asps = Vk.ImgVw.CreateInfoNew {
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
		Vk.Img.M.subresourceRangeLevelCount = 1,
		Vk.Img.M.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.M.subresourceRangeLayerCount = 1 }

createRenderPass ::
	forall (scifmt :: Vk.T.Format) (dptfmt :: Vk.T.Format) sd a . (
	Vk.T.FormatToValue scifmt, Vk.T.FormatToValue dptfmt ) =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPass.R sr -> IO a) -> IO a
createRenderPass dvc f = do
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
		depthAttachment :: Vk.Att.Description dptfmt
		depthAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = zeroBits,
			Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
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
			Vk.RndrPass.M.createInfoNextNew = TMaybe.N,
			Vk.RndrPass.M.createInfoFlagsNew = zeroBits,
			Vk.RndrPass.M.createInfoAttachmentsNew =
				colorAttachment :** depthAttachment :** HeteroParList.Nil,
			Vk.RndrPass.M.createInfoSubpassesNew = [subpass],
			Vk.RndrPass.M.createInfoDependenciesNew = [dependency] }
	Vk.RndrPass.createNew @'[scifmt, dptfmt] @'Nothing dvc renderPassInfo nil' \rp -> f rp

createPipelineLayout :: forall sd s b .
	Vk.Dvc.D sd ->
	Vk.DscSetLyt.L s '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer '[
			VObj.Atom 256 GpuSceneData0 'Nothing ] ] -> (forall sl .
		Vk.Ppl.Layout.L sl
			'[ '(s, '[
				'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
				'Vk.DscSetLyt.Buffer '[
					VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
			'[WrapMeshPushConstants] ->
		IO b) -> IO b
createPipelineLayout dvc cmdslyt f = Vk.Ppl.Layout.createNew dvc crInfo nil' f
	where
	crInfo :: Vk.Ppl.Layout.CreateInfoNew 'Nothing
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing] ]) ]
		(
		'Vk.PushConstant.PushConstantLayout
			'[ WrapMeshPushConstants]
			'[ 'Vk.PushConstant.Range
				'[ 'Vk.T.ShaderStageVertexBit] '[WrapMeshPushConstants] ])
	crInfo = Vk.Ppl.Layout.CreateInfoNew {
		Vk.Ppl.Layout.createInfoNextNew = TMaybe.N,
		Vk.Ppl.Layout.createInfoFlagsNew = zeroBits,
		Vk.Ppl.Layout.createInfoSetLayoutsNew = HeteroParList.Singleton $ U2 cmdslyt }

createGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.L sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	Int ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) -> IO a) -> IO a
createGraphicsPipeline dvc sce rp ppllyt sdrn f =
	Vk.Ppl.Graphics.createGs dvc Nothing (HeteroParList.Singleton $ U14 pplInfo) nil'
		\(HeteroParList.Singleton (U3 gpl)) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo sce rp ppllyt sdrn

type Foo s = '[ '(s, '[
	'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
	'Vk.DscSetLyt.Buffer '[
		VObj.Atom 256 GpuSceneData0 'Nothing ] ])]

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.L sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] -> Int ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) -> IO ()
recreateGraphicsPipeline dvc sce rp ppllyt sdrn gpls = Vk.Ppl.Graphics.recreateGs
	dvc Nothing (U14 pplInfo :** HeteroParList.Nil) nil' (U3 gpls :** HeteroParList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo sce rp ppllyt sdrn

mkGraphicsPipelineCreateInfo ::
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.L sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] -> Int ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing, '[ '(Vertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Position), '(1, Normal), '(2, Color)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'(sl,	'[ '(s, '[
				'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
				'Vk.DscSetLyt.Buffer '[
					VObj.Atom 256 GpuSceneData0 'Nothing ] ])],
			'[WrapMeshPushConstants])
		sr '(sb, vs', ts', sbtss')
mkGraphicsPipelineCreateInfo sce rp ppllyt sdrn = Vk.Ppl.Graphics.CreateInfo {
	Vk.Ppl.Graphics.createInfoNext = TMaybe.N,
	Vk.Ppl.Graphics.createInfoFlags = Vk.Ppl.CreateFlagsZero,
	Vk.Ppl.Graphics.createInfoStages = uncurry shaderStages
		case sdrn `mod` 2 of
			0 -> shaderPair0
			1 -> shaderPair1
			_ -> error "never occur",
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

inputAssembly :: Vk.Ppl.InpAsmbSt.CreateInfo 'Nothing
inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
	Vk.Ppl.InpAsmbSt.createInfoNext = TMaybe.N,
	Vk.Ppl.InpAsmbSt.createInfoFlags = zeroBits,
	Vk.Ppl.InpAsmbSt.createInfoTopology = Vk.PrimitiveTopologyTriangleList,
	Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable = False }

mkViewportState :: Vk.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo 'Nothing
mkViewportState sce = def {
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
	Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeNone, -- Vk.CullModeBackBit,
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

createDepthResources ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPl.C sc ->
	Vk.Extent2d ->
	(forall si sm fmt siv . Vk.T.FormatToValue fmt =>
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.K.Image nm fmt) ] ->
		Vk.ImgVw.I nm fmt siv ->
		IO a) -> IO a
createDepthResources phdvc dvc gq cp ext f = do
	fmt <- findDepthFormat phdvc
	print fmt
	print ext
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		createImage @_ @fmt phdvc dvc
			(Vk.extent2dWidth ext) (Vk.extent2dHeight ext)
			Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
			Vk.Mem.PropertyDeviceLocalBit \dptImg dptImgMem ->
			createImageView @fmt
				dvc dptImg Vk.Img.AspectDepthBit \dptImgVw -> do
			transitionImageLayout dvc gq cp dptImg Vk.Img.LayoutUndefined
				Vk.Img.LayoutDepthStencilAttachmentOptimal
			f dptImg dptImgMem dptImgVw

recreateDepthResources :: Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.CmdPl.C sc ->
	Vk.Extent2d ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.K.Image nm fmt)] ->
	Vk.ImgVw.I nm fmt sdiv -> IO ()
recreateDepthResources phdvc dvc gq cp ext dptImg dptImgMem dptImgVw = do
	print ext
	recreateImage phdvc dvc
		(Vk.extent2dWidth ext) (Vk.extent2dHeight ext)
		Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mem.PropertyDeviceLocalBit dptImg dptImgMem
	recreateImageView dvc dptImg Vk.Img.AspectDepthBit dptImgVw
	transitionImageLayout dvc gq cp dptImg Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal

type DepthResources sb sm nm fmt sdiv = (
	Vk.Img.Binded sm sb nm fmt,
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.K.Image nm fmt)],
	Vk.ImgVw.I nm fmt sdiv )

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

createImage :: forall nm fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mem.PropertyFlagBits -> (forall si sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.K.Image nm fmt) ] ->
		IO a) -> IO a
createImage pd dvc wdt hgt tlng usg prps f = Vk.Img.createNew @'Nothing dvc
		(imageInfo wdt hgt tlng usg) nil' \img -> do
	memInfo <- imageMemoryInfo pd dvc prps img
	imageAllocateBind dvc img memInfo f

recreateImage :: Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mem.PropertyFlags ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.K.Image nm fmt)] -> IO ()
recreateImage pd dvc wdt hgt tlng usg prps img mem = do
	Vk.Img.recreateNew @'Nothing dvc
		(imageInfo wdt hgt tlng usg) nil' img
	memInfo <- imageMemoryInfoBinded pd dvc prps img
	imageReallocateBind dvc img memInfo mem

imageInfo ::
	Word32 -> Word32 -> Vk.Img.Tiling -> Vk.Img.UsageFlags ->
	Vk.Img.CreateInfoNew 'Nothing fmt
imageInfo wdt hgt tlng usg = Vk.Img.CreateInfoNew {
		Vk.Img.createInfoNextNew = TMaybe.N,
		Vk.Img.createInfoImageTypeNew = Vk.Img.Type2d,
		Vk.Img.createInfoExtentNew = Vk.Extent3d {
			Vk.extent3dWidth = wdt,
			Vk.extent3dHeight = hgt,
			Vk.extent3dDepth = 1 },
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
	Vk.Dvc.Mem.AllocateInfo 'Nothing -> (forall sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.K.Image nm fmt) ] ->
		IO a) -> IO a
imageAllocateBind dvc img memInfo f =
	Vk.Mem.allocateBind @'Nothing dvc
		(HeteroParList.Singleton . U2 $ Vk.Mem.Image img) memInfo
		nil' \(HeteroParList.Singleton (U2 (Vk.Mem.ImageBinded bnd))) m -> do
		f bnd m

imageReallocateBind ::
	Vk.Dvc.D sd -> Vk.Img.Binded sm sb nm fmt ->
	Vk.Dvc.Mem.AllocateInfo 'Nothing ->
	Vk.Mem.M sm '[ '(sb, 'Vk.Mem.K.Image nm fmt)] -> IO ()
imageReallocateBind dvc img memInfo m =
	Vk.Mem.reallocateBind @'Nothing dvc
		(HeteroParList.Singleton . U2 $ Vk.Mem.ImageBinded img) memInfo
		nil' m

imageMemoryInfo ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Mem.PropertyFlags ->
	Vk.Img.INew s nm fmt -> IO (Vk.Dvc.Mem.AllocateInfo 'Nothing)
imageMemoryInfo pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirementsNew dvc img
	mt <- findMemoryType pd (Vk.Mem.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Dvc.Mem.AllocateInfo {
		Vk.Dvc.Mem.allocateInfoNext = TMaybe.N,
		Vk.Dvc.Mem.allocateInfoMemoryTypeIndex = mt }

imageMemoryInfoBinded ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Mem.PropertyFlags ->
	Vk.Img.Binded sm si nm fmt -> IO (Vk.Dvc.Mem.AllocateInfo 'Nothing)
imageMemoryInfoBinded pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirementsBindedNew dvc img
	mt <- findMemoryType pd (Vk.Mem.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Dvc.Mem.AllocateInfo {
		Vk.Dvc.Mem.allocateInfoNext = TMaybe.N,
		Vk.Dvc.Mem.allocateInfoMemoryTypeIndex = mt }

transitionImageLayout :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPl.C sc ->
	Vk.Img.Binded sm si nm fmt -> Vk.Img.Layout -> Vk.Img.Layout ->
	IO ()
transitionImageLayout dvc gq cp img olyt nlyt =
	beginSingleTimeCommands dvc gq cp \cb -> do
	let	barrier :: Vk.Img.MemoryBarrier 'Nothing sm si nm fmt
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
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPl.C sc ->
	(forall s . Vk.CmdBffr.C s -> IO a) -> IO a
beginSingleTimeCommands dvc gq cp cmd = do
	Vk.CmdBffr.allocate
		dvc allocInfo \((cb :: Vk.CmdBffr.C s) :*. HeteroParList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
				Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
		Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo (cmd cb) <* do
			Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) Nothing
			Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo 'Nothing sc '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.M.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }

createFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
	Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.I nm fmt) sis ->
	Vk.ImgVw.I dptfmt dptnm siv ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HeteroParList.Nil _ f = f HeteroParList.Nil
createFramebuffers dvc sce rp (iv :** ivs) dptiv f =
	Vk.Frmbffr.createNew dvc (mkFramebufferCreateInfo sce rp iv dptiv) nil' \fb ->
	createFramebuffers dvc sce rp ivs dptiv \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
		Vk.ImgVw.I dptfmt dptnm sdiv ->
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HeteroParList.Nil _ HeteroParList.Nil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dvc sce rp (sciv :** scivs) dptiv (fb :** fbs) =
		Vk.Frmbffr.recreateNew dvc
			(mkFramebufferCreateInfo sce rp sciv dptiv) nil' fb >>
		recreateFramebuffers dvc sce rp scivs dptiv fbs

mkFramebufferCreateInfo ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.ImgVw.I nm fmt si ->
	Vk.ImgVw.I dptfmt dptnm sdiv ->
	Vk.Frmbffr.CreateInfoNew 'Nothing sr
		'[ '(nm, fmt, si), '(dptfmt, dptnm, sdiv)]
mkFramebufferCreateInfo sce rp attch dpt = Vk.Frmbffr.CreateInfoNew {
	Vk.Frmbffr.createInfoNextNew = TMaybe.N,
	Vk.Frmbffr.createInfoFlagsNew = zeroBits,
	Vk.Frmbffr.createInfoRenderPassNew = rp,
	Vk.Frmbffr.createInfoAttachmentsNew = U3 attch :** U3 dpt :** HeteroParList.Nil,
	Vk.Frmbffr.createInfoWidthNew = w, Vk.Frmbffr.createInfoHeightNew = h,
	Vk.Frmbffr.createInfoLayersNew = 1 }
	where
	Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = sce

createCommandPool :: QueueFamilyIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCommandPool qfis dv = Vk.CmdPl.create dv crInfo nil'
	where crInfo = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = graphicsFamily qfis }

createVertexBuffer :: forall sd sc vbnm a . Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPl.C sc -> V.Vector Vertex ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb vbnm '[VObj.List 256 Vertex ""] -> IO a ) -> IO a
createVertexBuffer phdvc dvc gq cp vtcs f =
	createBuffer phdvc dvc (HeteroParList.Singleton . VObj.ObjectLengthList $ V.length vtcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBuffer phdvc dvc (HeteroParList.Singleton . VObj.ObjectLengthList $ V.length vtcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
			\b' (bm' :: Vk.Mem.M sm '[
				'(sb, 'Vk.Mem.K.Buffer vbnm '[VObj.List 256 Vertex ""])
				]) -> do
	Vk.Mem.write @vbnm @(VObj.List 256 Vertex "") dvc bm' zeroBits vtcs
	copyBuffer dvc gq cp b' b
	f b

createCameraBuffers :: Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.DscSetLyt.L sdsc '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer '[
			VObj.Atom 256 GpuSceneData0 'Nothing ] ] ->
	Int ->
	(forall slyts sbsms . (
		Vk.DscSet.DListFromMiddle slyts,
		HeteroParList.FromList slyts, Update sbsms slyts,
		HeteroParList.HomoList '(sdsc, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ]) slyts ) =>
		HeteroParList.PL (U2 Vk.DscSetLyt.L) slyts ->
		HeteroParList.PL BindedGcd sbsms ->
		HeteroParList.PL MemoryGcd sbsms -> IO a) -> IO a
createCameraBuffers _ _ _ n f | n < 1 = f HeteroParList.Nil HeteroParList.Nil HeteroParList.Nil
createCameraBuffers phdvc dvc lyt n f = createCameraBuffer phdvc dvc \bnd mem ->
	createCameraBuffers phdvc dvc lyt (n - 1) \lyts bnds mems ->
	f (U2 lyt :** lyts) (BindedGcd bnd :** bnds) (MemoryGcd mem :** mems)

createCameraBuffer :: Vk.PhDvc.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 GpuCameraData 'Nothing] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.K.Buffer nm
				'[VObj.Atom 256 GpuCameraData 'Nothing]) ] ->
		IO a) -> IO a
createCameraBuffer phdvc dvc = createBuffer phdvc dvc (HeteroParList.Singleton VObj.ObjectLengthAtom)
	Vk.Bffr.UsageUniformBufferBit Vk.Mem.PropertyHostVisibleBit

createSceneBuffer :: Vk.PhDvc.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[
			VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
			VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.K.Buffer nm '[
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ] ) ] ->
		IO a) -> IO a
createSceneBuffer phdvc dvc = createBuffer2 phdvc dvc
	(VObj.ObjectLengthAtom :** VObj.ObjectLengthAtom :** HeteroParList.Nil)
	Vk.Bffr.UsageUniformBufferBit Vk.Mem.PropertyHostVisibleBit

createBuffer :: forall obj nm sd a . (
	VObj.SizeAlignmentList '[obj],
	VObj.SizeAlignment obj
--	Vk.Mem.Alignments '[
--		'(s, 'Vk.Mem.K.Buffer nm objs) ]
	) => -- VObj.SizeAlignment obj =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> HeteroParList.PL VObj.ObjectLength '[obj] ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[obj] ->
		Vk.Mem.M sm '[
			'(sb, 'Vk.Mem.K.Buffer nm '[obj])
			] -> IO a ) -> IO a
createBuffer p dv lns usg props f = Vk.Bffr.create dv bffrInfo nil'
		\b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Mem.allocateBind dv
		(HeteroParList.Singleton . U2 $ Vk.Mem.Buffer b) (allcInfo mt) nil'
		$ f . \(HeteroParList.Singleton (U2 (Vk.Mem.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[obj]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = lns,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.M.TypeIndex -> Vk.Dvc.Mem.AllocateInfo 'Nothing
	allcInfo mt = Vk.Dvc.Mem.AllocateInfo {
		Vk.Dvc.Mem.allocateInfoNext = TMaybe.N,
		Vk.Dvc.Mem.allocateInfoMemoryTypeIndex = mt }

createBuffer2 :: forall obj obj2 nm sd a . (
	VObj.SizeAlignmentList '[obj, obj2],
	VObj.SizeAlignment obj, VObj.SizeAlignment obj2
--	Vk.Mem.Alignments '[
--		'(s, 'Vk.Mem.K.Buffer nm objs) ]
	) => -- VObj.SizeAlignment obj =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> HeteroParList.PL VObj.ObjectLength '[obj, obj2] ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[obj, obj2] ->
		Vk.Mem.M sm '[
			'(sb, 'Vk.Mem.K.Buffer nm '[obj, obj2])
			] -> IO a ) -> IO a
createBuffer2 p dv lns usg props f = Vk.Bffr.create dv bffrInfo nil'
		\b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Mem.allocateBind dv
		(HeteroParList.Singleton . U2 $ Vk.Mem.Buffer b) (allcInfo mt) nil'
		$ f . \(HeteroParList.Singleton (U2 (Vk.Mem.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[obj, obj2]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = lns,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.M.TypeIndex -> Vk.Dvc.Mem.AllocateInfo 'Nothing
	allcInfo mt = Vk.Dvc.Mem.AllocateInfo {
		Vk.Dvc.Mem.allocateInfoNext = TMaybe.N,
		Vk.Dvc.Mem.allocateInfoMemoryTypeIndex = mt }

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

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscSetLyt.L s '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer
			'[VObj.Atom 256 GpuSceneData0 'Nothing]
		] -> IO a) ->
	IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil'
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer
			'[VObj.Atom 256 GpuSceneData0 'Nothing] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings =
			camBufferBinding :** sceneBind :** HeteroParList.Nil }
	camBufferBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing])
	camBufferBinding = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	sceneBind :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuSceneData0 'Nothing])
	sceneBind = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags =
			Vk.ShaderStageVertexBit .|.
			Vk.ShaderStageFragmentBit }

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDescriptorPool dvc = Vk.DscPool.create dvc poolInfo nil'
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = TMaybe.N,
		Vk.DscPool.createInfoFlags =
			Vk.DscPool.CreateFreeDescriptorSetBit,
		Vk.DscPool.createInfoMaxSets = 10,
		Vk.DscPool.createInfoPoolSizes = [poolSize0] }
	poolSize0 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

createDescriptorSets :: (
	Vk.DscSet.DListFromMiddle ss,
	HeteroParList.FromList ss, Update smsbs ss ) =>
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> HeteroParList.PL BindedGcd smsbs ->
	HeteroParList.PL (U2 Vk.DscSetLyt.L) ss ->
	Vk.Bffr.Binded sm sb "scene-buffer" '[
		VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
		VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ] ->
	(forall sds . HeteroParList.PL (Vk.DscSet.D sds) ss -> IO a) -> IO a
createDescriptorSets dvc dscp ubs dscslyts scnb f =
	Vk.DscSet.allocateDs dvc allocInfo \dscss -> do
	update dvc ubs dscss scnb 0
	f dscss
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = dscslyts }

class Update smsbs slbtss where
	update ::
		Vk.Dvc.D sd ->
		HeteroParList.PL BindedGcd smsbs ->
		HeteroParList.PL (Vk.DscSet.D sds) slbtss ->
		Vk.Bffr.Binded sm sb "scene-buffer" '[
			VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
			VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ] -> Int -> IO ()

instance Update '[] '[] where update _ HeteroParList.Nil HeteroParList.Nil _ _ = pure ()

instance (
	Vk.DscSet.T.BindingAndArrayElem (TIndex.I1_2 '(ds, cs)) '[VObj.Atom 256 GpuCameraData 'Nothing],
	Vk.DscSet.T.BindingAndArrayElem (TIndex.I1_2 '(ds, cs)) '[VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0")],
	Vk.DscSet.T.BindingAndArrayElem (TIndex.I1_2 '(ds, cs)) '[VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1")],
	Vk.DscSet.T.UpdateDynamicLength (TIndex.I1_2 '(ds, cs)) '[VObj.Atom 256 GpuCameraData 'Nothing],
	Vk.DscSet.T.UpdateDynamicLength (TIndex.I1_2 '(ds, cs)) '[VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0")],
	Vk.DscSet.T.UpdateDynamicLength (TIndex.I1_2 '(ds, cs)) '[VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1")],
	Update ubs dscss
	) =>
	Update (ub ': ubs) ('(ds, cs) ': dscss) where
	update dvc (BindedGcd ub :** ubs) (dscs :** dscss) scnb 0 = do
		Vk.DscSet.updateDs dvc (
			U4 (descriptorWrite0 @GpuCameraData @Nothing ub dscs Vk.Dsc.TypeUniformBuffer) :**
			U4 (descriptorWrite0 @GpuSceneData0 @('Just "scene-data-0") scnb dscs Vk.Dsc.TypeUniformBuffer) :**
			HeteroParList.Nil ) HeteroParList.Nil
		update dvc ubs dscss scnb 1
	update dvc (BindedGcd ub :** ubs) (dscs :** dscss) scnb 1 = do
		Vk.DscSet.updateDs dvc (
			U4 (descriptorWrite0 @GpuCameraData @Nothing ub dscs Vk.Dsc.TypeUniformBuffer) :**
			U4 (descriptorWrite0 @GpuSceneData0 @('Just "scene-data-1") scnb dscs Vk.Dsc.TypeUniformBuffer) :**
			HeteroParList.Nil )
			HeteroParList.Nil
		update dvc ubs dscss scnb 2
	update _ _ _ _ _ = error "bad"

descriptorWrite0 :: forall tp objnm objs sm sb nm slbts sds . (
	Show (HeteroParList.PL VObj.ObjectLength objs),
	VObj.Offset (VObj.Atom 256 tp objnm) objs ) =>
	Vk.Bffr.Binded sm sb nm objs ->
	Vk.DscSet.D sds slbts -> Vk.Dsc.Type ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, VObj.Atom 256 tp objnm )])
descriptorWrite0 ub dscs tp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = tp,
	Vk.DscSet.writeSources= Vk.DscSet.BufferInfos $
		HeteroParList.Singleton bufferInfo }
	where bufferInfo = U4 $ Vk.Dsc.BufferInfo ub

data BindedGcd smsb where
	BindedGcd ::
		Vk.Bffr.Binded sm sb "camera-buffer" '[VObj.Atom 256 GpuCameraData 'Nothing] ->
		BindedGcd '(sm, sb)

data MemoryGcd smsb where
	MemoryGcd ::
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.K.Buffer "camera-buffer"
				'[VObj.Atom 256 GpuCameraData 'Nothing] )] ->
		MemoryGcd '(sm, sb)

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Vertex ""] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CmdBffr.allocate
		dvc allocInfo \((cb :: Vk.CmdBffr.C s) :*. HeteroParList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
				Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
		Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo do
			Vk.Cmd.copyBuffer @'[ '[VObj.List 256 Vertex ""]] cb src dst
		Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) Nothing
		Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo 'Nothing sc '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.M.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }

createCommandBuffers ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb vss . (
		TLength.Length vss, HeteroParList.HomoList '() vss ) =>
		HeteroParList.LL (Vk.CmdBffr.C scb) (vss :: [()]) -> IO a) ->
	IO a
createCommandBuffers dvc cp f = mkVss maxFramesInFlight \(_p :: Proxy vss1) ->
	Vk.CmdBffr.allocate @_ @vss1 dvc (allcInfo @vss1) (f @_ @vss1)
	where
	allcInfo :: forall vss . Vk.CmdBffr.AllocateInfo 'Nothing scp vss
	allcInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }

mkVss :: Int -> (forall (vss :: [()]) . (
	TpLvlLst.Length vss, TLength.Length vss,
	HeteroParList.FromList vss, HeteroParList.HomoList '() vss ) =>
	Proxy vss -> a) -> a
mkVss 0 f = f (Proxy @'[])
mkVss n f = mkVss (n - 1) \p -> f $ addTypeToProxy p
	where
	addTypeToProxy :: Proxy vss -> Proxy ('() ': vss)
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
		(Vk.Semaphore.create @'Nothing dvc def nil') \iass ->
	HeteroParList.replicateM maxFramesInFlight
		(Vk.Semaphore.create @'Nothing dvc def nil') \rfss ->
	HeteroParList.replicateM maxFramesInFlight
		(Vk.Fence.create @'Nothing dvc fncInfo nil') \iffs ->
	f $ SyncObjects iass rfss iffs
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sg slyt sdlyt sm sb nm smtri sbtri nmtri sds .
	Vk.CmdBffr.C scb ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt, Foo sdlyt, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Layout.L slyt
		'[ '(sdlyt, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[VObj.List 256 Vertex ""] -> Int ->
	Vk.DscSet.D sds '(sdlyt, '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer '[
			VObj.Atom 256 GpuSceneData0 'Nothing ] ]) ->
	Word32 -> IO ()
recordCommandBuffer cb rp fb sce gpl lyt vb vbtri fn cmd vn =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb cbInfo $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	newIORef Nothing >>= \om ->
	drawObject om cb sce cmd RenderObject {
		renderObjectPipeline = gpl,
		renderObjectPipelineLayout = lyt,
		renderObjectMesh = vb,
		renderObjectMeshSize = vn,
		renderObjectTransformMatrix = model } >>
	newIORef Nothing >>= \omtri ->
	for_ [- 20 .. 20] \x -> for_ [- 20 .. 20] \y ->
		drawObject omtri cb sce cmd RenderObject {
			renderObjectPipeline = gpl,
			renderObjectPipelineLayout = lyt,
			renderObjectMesh = vbtri,
			renderObjectMeshSize = 3,
			renderObjectTransformMatrix =
				Cglm.mat4Mul (translation x y) scale }
	where
	model = Cglm.rotate
		Cglm.mat4Identity
		(fromIntegral fn * Cglm.rad 1)
		(Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL)
	translation x y = Cglm.translate
		Cglm.mat4Identity (Cglm.Vec3 $ x :. 0 :. y :. NilL)
	scale = Cglm.scale
		Cglm.mat4Identity (Cglm.Vec3 $ 0.2 :. 0.2 :. 0.2 :. NilL)
	cbInfo :: Vk.CmdBffr.BeginInfo 'Nothing 'Nothing
	cbInfo = def {
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit }
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
			Vk.ClearValueColor (fromJust $ rgbaDouble 0 0 blue 1) :**
			Vk.ClearValueDepthStencil (Vk.ClearDepthStencilValue 1 0) :**
			HeteroParList.Nil }
	blue = 0.5 + sin (fromIntegral fn / (180 * frashRate) * pi) / 2

data RenderObject sg sl sdlyt sm sb nm = RenderObject {
	renderObjectPipeline :: Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo sdlyt, '[WrapMeshPushConstants]),
	renderObjectPipelineLayout ::
		Vk.Ppl.Layout.L sl
			'[ '(sdlyt, '[
				'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
				'Vk.DscSetLyt.Buffer '[
					VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
			'[WrapMeshPushConstants],
	renderObjectMesh :: Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""],
	renderObjectMeshSize :: Word32,
	renderObjectTransformMatrix :: Cglm.Mat4 }

drawObject :: IORef (Maybe (Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""])) ->
	Vk.CmdBffr.C scb ->
	Vk.Extent2d ->
	Vk.DscSet.D sds '(sdlyt, '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer '[
			VObj.Atom 256 GpuSceneData0 'Nothing ] ]) ->
	RenderObject sg sl sdlyt sm sb nm -> IO ()
drawObject om cb sce cmd RenderObject {
	renderObjectPipeline = gpl,
	renderObjectPipelineLayout = lyt,
	renderObjectMesh = vb,
	renderObjectMeshSize = vn,
	renderObjectTransformMatrix = model } =
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics lyt
		(HeteroParList.Singleton $ U2 cmd)
		(HeteroParList.Singleton (
			HeteroParList.Nil :** HeteroParList.Nil :**
			HeteroParList.Nil )) >>
	readIORef om >>= \movb ->
	(case movb of
		Just ovb | vb == ovb -> pure ()
		_ -> do	Vk.Cmd.bindVertexBuffers cbb . HeteroParList.Singleton
				. U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @Vertex @"" vb
			writeIORef om $ Just vb) >>
	Vk.Cmd.pushConstantsGraphics @'[ 'Vk.T.ShaderStageVertexBit ] cbb lyt (HeteroParList.Id (Foreign.Storable.Generic.Wrap
		MeshPushConstants {
			meshPushConstantsData = Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
			meshPushConstantsRenderMatrix = model
			}) :** HeteroParList.Nil) >>
	Vk.Cmd.draw cbb vn 1 0 0

view :: Cglm.Mat4
view = Cglm.lookat
	(Cglm.Vec3 $ 0 :. 6 :. 10 :. NilL)
	(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
	(Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL)

projection :: Vk.Extent2d -> Cglm.Mat4
projection sce = Cglm.modifyMat4 1 1 negate $ Cglm.perspective
	(Cglm.rad 70) (fromIntegral (Vk.extent2dWidth sce) /
		fromIntegral (Vk.extent2dHeight sce)) 0.1 200

mainLoop :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers ss sfs,
	HeteroParList.HomoList '(s, '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer '[
			VObj.Atom 256 GpuSceneData0 'Nothing ] ]) slyts,
	HeteroParList.HomoList '() vss ) =>
	FramebufferResized ->
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc scfmt -> Vk.Extent2d ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) ss ->
	Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.L sl '[ '(s, '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer '[
			VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Graphics.G sg0
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Graphics.G sg1
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.CmdPl.C scp ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[VObj.List 256 Vertex ""] ->
	HeteroParList.LL (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	HeteroParList.PL BindedGcd sbsms ->
	HeteroParList.PL MemoryGcd sbsms ->
	Vk.Mem.M sscnm
		'[ '(sscnb, 'Vk.Mem.K.Buffer
			"scene-buffer" '[
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ])] ->
	HeteroParList.PL (Vk.DscSet.D sds) slyts ->
	Word32 -> IO ()
mainLoop g w sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs vb vbtri cbs iasrfsifs cmbs cmms scnm cmds vn = do
	($ 0) . ($ Glfw.KeyState'Released) . ($ 0) . ($ cycle [0 .. maxFramesInFlight - 1]) . ($ ext0) $ fix \loop ext (cf : cfs) fn spst0 sdrn -> do
		Glfw.pollEvents
		spst <- Glfw.getKey w Glfw.Key'Space
		let	prsd = case (spst0, spst) of
				(Glfw.KeyState'Released, Glfw.KeyState'Pressed) -> True
				_ -> False
			sdrn' = bool id (+ 1) prsd sdrn
		when prsd $ print sdrn'
		runLoop w sfc phdvc qfis dvc gq pq
			sc g ext scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs vb vbtri cbs iasrfsifs cf fn sdrn' cmbs cmms scnm cmds vn
			(\ex -> loop ex cfs ((fn + 1) `mod` (360 * frashRate)) spst sdrn')
	Vk.Dvc.waitIdle dvc

runLoop :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs,
	HeteroParList.HomoList
		'(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ]) slyts,
	HeteroParList.HomoList '() vss ) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc scfmt -> FramebufferResized -> Vk.Extent2d ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.L sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Graphics.G sg0 '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Graphics.G sg1 '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.CmdPl.C scp ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[VObj.List 256 Vertex ""] ->
	HeteroParList.LL (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	Int -> Int -> Int ->
	HeteroParList.PL BindedGcd sbsms ->
	HeteroParList.PL MemoryGcd sbsms ->
	Vk.Mem.M sscnm
		'[ '(sscnb, 'Vk.Mem.K.Buffer
			"scene-buffer" '[
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ])] ->
	HeteroParList.PL (Vk.DscSet.D sds) slyts ->
	Word32 ->
	(Vk.Extent2d -> IO ()) -> IO ()
runLoop win sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs vb vbtri cbs iasrfsifs cf fn sdrn cmbs cmms scnm cmds vn loop = do
	catchAndRecreate win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs loop
		$ drawFrame dvc gq pq sc ext rp gpl0 gpl1 ppllyt fbs vb vbtri cbs iasrfsifs cf fn sdrn cmbs cmms scnm cmds vn
	cls <- Glfw.windowShouldClose win
	if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateSwapchainEtc
			win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs)

drawFrame ::
	forall sfs sd ssc scfmt sr sg0 sg1 slyt s sm sb nm smtri sbtri nmtri
		scb ssos vss sbsms sscnm sscnb slyts sds . (
	HeteroParList.HomoList
		'(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ]) slyts,
	HeteroParList.HomoList '() vss ) =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> Vk.Khr.Swapchain.SNew ssc scfmt ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Graphics.G sg0 '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt, Foo s, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Graphics.G sg1 '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt, Foo s, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Layout.L slyt
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[VObj.List 256 Vertex ""] ->
	HeteroParList.LL (Vk.CmdBffr.C scb) vss -> SyncObjects ssos -> Int -> Int -> Int ->
	HeteroParList.PL BindedGcd sbsms ->
	HeteroParList.PL MemoryGcd sbsms ->
	Vk.Mem.M sscnm
		'[ '(sscnb, 'Vk.Mem.K.Buffer
			"scene-buffer" '[
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ])] ->
	HeteroParList.PL (Vk.DscSet.D sds) slyts ->
	Word32 -> IO ()
drawFrame dvc gq pq sc ext rp gpl0 gpl1 lyt fbs vb vbtri cbs (SyncObjects iass rfss iffs) cf fn sdrn cmbs cmms scnm cmds vn =
	HeteroParList.index iass cf \(ias :: Vk.Semaphore.S sias) ->
	HeteroParList.index rfss cf \(rfs :: Vk.Semaphore.S srfs) ->
	HeteroParList.index iffs cf \(id &&& HeteroParList.Singleton -> (iff, siff)) ->
	HeteroParList.index cmms cf \(MemoryGcd cmm) ->
	($ HeteroParList.homoListIndex cmds cf) \cmd -> do
	Vk.Mem.write @"camera-buffer" @(VObj.Atom 256 GpuCameraData 'Nothing) dvc cmm zeroBits (gpuCameraData ext)
	if cf == 0
		then Vk.Mem.write @"scene-buffer"
			@(VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"))
			dvc scnm zeroBits (GpuSceneData0 $ gpuSceneData fn)
		else Vk.Mem.write @"scene-buffer"
			@(VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1"))
			dvc scnm zeroBits (GpuSceneData0 $ gpuSceneData fn)
	Vk.Fence.waitForFs dvc siff True maxBound
	imgIdx <- Vk.Khr.acquireNextImageResultNew [Vk.Success, Vk.SuboptimalKhr]
		dvc sc uint64Max (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HeteroParList.index fbs imgIdx \fb -> case sdrn `mod` 2 of
		0 -> recordCommandBuffer cb rp fb ext gpl0 lyt vb vbtri fn cmd vn
		1 -> recordCommandBuffer cb rp fb ext gpl1 lyt vb vbtri fn cmd vn
		_ -> error "never occur"
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HeteroParList.Singleton rfs }
		presentInfoNew = Vk.Khr.PresentInfoNew {
			Vk.Khr.presentInfoNextNew = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphoresNew = HeteroParList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndicesNew = HeteroParList.Singleton
				$ Vk.Khr.SwapchainImageIndexNew sc imgIdx }
	Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresentNew @'Nothing pq presentInfoNew
	where	HeteroParList.Dummy cb = cbs `HeteroParList.homoListIndex` cf ::
			HeteroParList.Dummy (Vk.CmdBffr.C scb) '()

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
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.L sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Graphics.G sg0
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Graphics.G sg1
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.CmdPl.C scp ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	(Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateSwapchainEtc
		win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs

recreateSwapchainEtc :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs ) =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q ->
	Vk.Khr.Swapchain.SNew ssc scfmt ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.L sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Graphics.G sg0
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Graphics.G sg1
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.CmdPl.C scp ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HeteroParList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateSwapchainEtc win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl0 gpl1 cp (dimg, dim, divw) fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwapchain win sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swapchain.getImagesNew dvc sc >>= \imgs ->
			recreateImageViews dvc imgs scivs
		recreateDepthResources phdvc dvc gq cp ext dimg dim divw
		recreateGraphicsPipeline dvc ext rp ppllyt 0 gpl0
		recreateGraphicsPipeline dvc ext rp ppllyt 1 gpl1
		recreateFramebuffers dvc ext rp scivs divw fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize win = Glfw.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

positionNormalToVertex :: Foreign.Storable.Generic.Wrap W.PositionNormal -> Vertex
positionNormalToVertex
	(W.W (W.PositionNormal (W.W (W.Position x y z)) (W.W (W.Normal v w u)))) =
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ x :. y :. z :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ v :. w :. u :. NilL,
		vertexColor = Color . Cglm.Vec3 $ v :. w :. u :. NilL }

triangle :: V.Vector Vertex
triangle = V.fromList [
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ 1 :. 1 :. 0.5 :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vertexColor = Color . Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL },
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ (- 1) :. 1 :. 0.5 :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vertexColor = Color . Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL },
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ 0 :. (- 1) :. 0.5 :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vertexColor = Color . Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL } ]

data Vertex = Vertex {
	vertexPos :: Position,
	vertexNormal :: Normal,
	vertexColor :: Color }
	deriving (Show, Generic)

newtype Position = Position Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype Normal = Normal Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype Color = Color Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

instance Storable Vertex where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance SizeAlignmentList Vertex

instance SizeAlignmentListUntil Position Vertex
instance SizeAlignmentListUntil Normal Vertex
instance SizeAlignmentListUntil Color Vertex

{-
instance Vk.Ppl.VertexInputSt.Formattable Cglm.Vec2 where
	formatOf = Vk.FormatR32g32Sfloat

instance Vk.Ppl.VertexInputSt.Formattable Cglm.Vec3 where
	formatOf = Vk.FormatR32g32b32Sfloat
	-}

instance Foreign.Storable.Generic.G Vertex where

vertices, vertices' :: V.Vector Vertex
vertices = V.fromList [
	Vertex (Position . Cglm.Vec3 $ 1.0 :. 1.0 :. 0.0 :. NilL)
		(Normal . Cglm.Vec3 $ 0.0 :. 0.0 :. 0.0 :. NilL)
		(Color . Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL),
	Vertex (Position . Cglm.Vec3 $ (- 1.0) :. 1.0 :. 0.0 :. NilL)
		(Normal . Cglm.Vec3 $ 0.0 :. 0.0 :. 0.0 :. NilL)
		(Color . Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Position . Cglm.Vec3 $ 0.0 :. (- 1.0) :. 0.0 :. NilL)
		(Normal . Cglm.Vec3 $ 0.0 :. 0.0 :. 0.0 :. NilL)
		(Color . Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL) ]
vertices' = V.fromList [
	Vertex (Position . Cglm.Vec3 $ 1.0 :. 1.0 :. 0.0 :. NilL)
		(Normal . Cglm.Vec3 $ 0.0 :. 0.0 :. 0.0 :. NilL)
		(Color . Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Position . Cglm.Vec3 $ (- 1.0) :. 1.0 :. 0.0 :. NilL)
		(Normal . Cglm.Vec3 $ 0.0 :. 0.0 :. 0.0 :. NilL)
		(Color . Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Position . Cglm.Vec3 $ 0.0 :. (- 1.0) :. 0.0 :. NilL)
		(Normal . Cglm.Vec3 $ 0.0 :. 0.0 :. 0.0 :. NilL)
		(Color . Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL) ]

data MeshPushConstants = MeshPushConstants {
	meshPushConstantsData :: Cglm.Vec4,
	meshPushConstantsRenderMatrix :: Cglm.Mat4 } deriving (Show, Generic)

type WrapMeshPushConstants = Foreign.Storable.Generic.Wrap MeshPushConstants

instance SizeAlignmentList MeshPushConstants
instance Foreign.Storable.Generic.G MeshPushConstants

data GpuCameraData = GpuCameraData {
	gpuCameraDataView :: View,
	gpuCameraDataProj :: Proj,
	gpuCameraDAtaViewProj :: ViewProj }
	deriving (Show, Generic)

gpuCameraData sce = GpuCameraData (View view) (Proj $ projection sce)
--	(ViewProj . Cglm.mat4Mul view $ projection sce)
	(ViewProj $ Cglm.mat4Mul (projection sce) view)

instance Storable GpuCameraData where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance Foreign.Storable.Generic.G GpuCameraData
instance SizeAlignmentList GpuCameraData

newtype View = View Cglm.Mat4 deriving (Show, Storable)
newtype Proj = Proj Cglm.Mat4 deriving (Show, Storable)
newtype ViewProj = ViewProj Cglm.Mat4 deriving (Show, Storable)

newtype GpuSceneData0 = GpuSceneData0 GpuSceneData deriving (Show, Storable)
newtype GpuSceneData1 = GpuSceneData1 GpuSceneData deriving (Show, Storable)

data GpuSceneData = GpuSceneData {
	gpuSceneDataFogColor :: FogColor,
	gpuSceneDataFogDistances :: FogDistances,
	gpuSceneDataAmbientColor :: AmbientColor,
	gpuSceneDataSunlightDirection :: SunlightDirection,
	gpuSceneDataSunlightColor :: SunlightColor }
	deriving (Show, Generic)

gpuSceneData :: Int -> GpuSceneData
gpuSceneData fn = GpuSceneData {
	gpuSceneDataFogColor = FogColor . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	gpuSceneDataFogDistances =
		FogDistances . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	gpuSceneDataAmbientColor =
		AmbientColor . Cglm.Vec4 $ r :. 0 :. b :. 0 :. NilL,
	gpuSceneDataSunlightDirection =
		SunlightDirection . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	gpuSceneDataSunlightColor =
		SunlightColor . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL }
	where
	r = sin (fromIntegral fn / (180 * frashRate) * pi)
	b = cos (fromIntegral fn / (180 * frashRate) * pi)

instance Storable GpuSceneData where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance Foreign.Storable.Generic.G GpuSceneData
instance SizeAlignmentList GpuSceneData

newtype FogColor = FogColor Cglm.Vec4 deriving (Show, Storable)
newtype FogDistances = FogDistances Cglm.Vec4 deriving (Show, Storable)
newtype AmbientColor = AmbientColor Cglm.Vec4 deriving (Show, Storable)
newtype SunlightDirection =
	SunlightDirection Cglm.Vec4 deriving (Show, Storable)
newtype SunlightColor = SunlightColor Cglm.Vec4 deriving (Show, Storable)

shaderStages ::
	Spv 'GlslVertexShader -> Spv 'GlslFragmentShader ->
	HeteroParList.PL (U5 Vk.Ppl.ShdrSt.CreateInfoNew) '[
		'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
		'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
shaderStages vs fs = U5 vertShaderStageInfo :** U5 fragShaderStageInfo :** HeteroParList.Nil
	where
	vertShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfoNew {
		Vk.Ppl.ShdrSt.createInfoNextNew = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlagsNew = def,
		Vk.Ppl.ShdrSt.createInfoStageNew = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModuleNew = vertShaderModule1,
		Vk.Ppl.ShdrSt.createInfoNameNew = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfoNew = Nothing }
	fragShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfoNew {
		Vk.Ppl.ShdrSt.createInfoNextNew = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlagsNew = def,
		Vk.Ppl.ShdrSt.createInfoStageNew = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModuleNew = fragShaderModule1,
		Vk.Ppl.ShdrSt.createInfoNameNew = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfoNew = Nothing }
	vertShaderModule1 :: Vk.Shader.Module.M 'Nothing 'GlslVertexShader 'Nothing
	vertShaderModule1 = mkShaderModule vs
	fragShaderModule1 :: Vk.Shader.Module.M 'Nothing 'GlslFragmentShader 'Nothing
	fragShaderModule1 = mkShaderModule fs
	mkShaderModule :: Spv sknd -> Vk.Shader.Module.M 'Nothing sknd 'Nothing
	mkShaderModule cd = Vk.Shader.Module.M crInfo nil'
		where crInfo = Vk.Shader.Module.M.CreateInfo {
			Vk.Shader.Module.M.createInfoNext = TMaybe.N,
			Vk.Shader.Module.M.createInfoFlags = def,
			Vk.Shader.Module.M.createInfoCode = cd }

shaderPair0 :: (Spv 'GlslVertexShader, Spv 'GlslFragmentShader)
shaderPair0 = (glslVertexShaderMain0, glslFragmentShaderMain0)

glslVertexShaderMain0 :: Spv 'GlslVertexShader
glslVertexShaderMain0 = [glslVertexShader|

#version 450

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inColor;

void
main()
{
	gl_Position = vec4(inPosition, 1.0);
}

|]

glslFragmentShaderMain0 :: Spv 'GlslFragmentShader
glslFragmentShaderMain0 = [glslFragmentShader|

#version 450

layout(location = 0) out vec4 outColor;

void
main()
{
	outColor = vec4(1.0, 0.0, 0.0, 1.0);
}

|]

shaderPair1 :: (Spv 'GlslVertexShader, Spv 'GlslFragmentShader)
shaderPair1 = (glslVertexShaderMain1, glslFragmentShaderMain1)

glslVertexShaderMain1 :: Spv 'GlslVertexShader
glslVertexShaderMain1 = [glslVertexShader|

#version 450

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inColor;

layout(location = 0) out vec3 outColor;

layout (set = 0, binding = 0) uniform CameraBuffer {
	mat4 view;
	mat4 proj;
	mat4 viewproj;
} cameraData;

layout(push_constant) uniform constants
{
	vec4 data;
	mat4 render_matrix;
} PushConstants;

void
main()
{
	mat4 transformMatrix = (cameraData.viewproj * PushConstants.render_matrix);
//	mat4 transformMatrix = (cameraData.proj * cameraData.view * PushConstants.render_matrix);
	gl_Position = transformMatrix * vec4(inPosition, 1.0);
	outColor = inColor;
}

|]

glslFragmentShaderMain1 :: Spv 'GlslFragmentShader
glslFragmentShaderMain1 = [glslFragmentShader|

#version 450

layout(location = 0) in vec3 inColor;
layout(location = 0) out vec4 outColor;

layout (set = 0, binding = 1) uniform SceneData {
	vec4 fogColor;
	vec4 fogDistances;
	vec4 ambientColor;
	vec4 sunlightDirection;
	vec4 sunlightColor;
} sceneData;

void
main()
{
	outColor = vec4(inColor + sceneData.ambientColor.xyz, 1.0);
//	outColor = vec4(inColor, 1.0);
}

|]
