{-# LANGUAGE ImportQualifiedPost #-}
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
import GHC.TypeNats
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.Storable.HeteroList
import Foreign.Storable.SizeAlignment hiding (SizeAlignment)
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.Foldable
import Data.Default
import Data.Bits
import Data.TypeLevel.Uncurry
import Data.HeteroParList qualified as HL
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

import qualified TypeLevel.List as TpLvlLst

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
import qualified Gpu.Vulkan.Middle as Vk.M
import qualified Gpu.Vulkan.Middle as Vk.C
import qualified Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Exception as Vk
import qualified Gpu.Vulkan.Exception.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Ist
import qualified Gpu.Vulkan.Instance.Middle as Vk.Ist.M
import qualified Gpu.Vulkan.Khr as Vk.Khr
import qualified Gpu.Vulkan.Khr.Enum as Vk.Khr
import qualified Gpu.Vulkan.Ext.DebugUtils as Vk.Ext.DbgUtls
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger as Vk.Ext.DbgUtls.Msngr
import qualified Gpu.Vulkan.Ext.DebugUtils.Enum as Vk.Ext.DbgUtls
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.PhysicalDevice.Struct as Vk.Phd
import qualified Gpu.Vulkan.QueueFamily as Vk.QueueFamily
import qualified Gpu.Vulkan.QueueFamily.Middle as Vk.QueueFamily
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Device.Middle as Vk.Dvc.M
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Sfc
import qualified Gpu.Vulkan.Khr.Surface.Middle as Vk.Khr.Sfc.M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as Vk.Khr.Sfc.Phd
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swpch
import qualified Gpu.Vulkan.Khr.Swapchain.Type as Vk.Khr.Swpch
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as Vk.Khr.Swpch.M
import qualified Gpu.Vulkan.Image as Vk.Img
import qualified Gpu.Vulkan.Image.Enum as Vk.Img
import qualified Gpu.Vulkan.Image.Middle as Vk.Img.M
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
import qualified Gpu.Vulkan.Pipeline.Layout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Attachment as Vk.Att
import qualified Gpu.Vulkan.Attachment.Enum as Vk.Att
import qualified Gpu.Vulkan.Subpass as Vk.Subpass
import qualified Gpu.Vulkan.Subpass.Enum as Vk.Subpass
import qualified Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPass
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPass.M
import qualified Gpu.Vulkan.Pipeline.Graphics.Type as Vk.Ppl.Grph
import qualified Gpu.Vulkan.Pipeline.GraphicsNew as Vk.Ppl.Grph
import qualified Gpu.Vulkan.Framebuffer as Vk.Frmbffr
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBffr
import qualified Gpu.Vulkan.CommandBuffer.Type as Vk.CmdBffr
import qualified Gpu.Vulkan.CommandBuffer.Middle as Vk.CmdBffr.M
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.Fence.Enum as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Buffer.Enum as Vk.Bffr
import qualified Gpu.Vulkan.Memory.Middle as Vk.Mem.M
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mem
import qualified Gpu.Vulkan.Memory.AllocateInfo as Vk.Dvc.Mem
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Memory.Kind as Vk.Mem.K
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.Command as Vk.Cmd
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as Vk.Ppl.DptStnSt
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.DscSetLyt
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.DescriptorSet.TypeLevel as Vk.DscSet.T

import Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList(AddType)

import qualified Codec.Wavefront.ReadOld as W
import Tools
import TypeLevel.Nat

maxFramesInFlight :: Integral n => n
maxFramesInFlight = 2

type MaxFramesInFlight = 2

frashRate :: Num n => n
frashRate = 2

main :: IO ()
main = do
	[objfile] <- getArgs
	vns <- readVertices <$> BS.readFile objfile
	withWindow \w frszd -> createInstance \ist -> if enableValidationLayers
		then Vk.Ext.DbgUtls.Msngr.create ist debugMessengerInfo nil nil
			$ const $ run w ist frszd vns
		else run w ist frszd vns
	where
	readVertices s = V.map positionNormalToVertex . uncurry3 W.facePosNormal
		$ W.readV' (W.countVertex c) (W.countNormal c) (W.countFace c) s
		where c = W.countV' s

withWindow :: (Glfw.Window -> FramebufferResized -> IO a) -> IO a
withWindow f = newIORef False >>= \frszd -> initWindow frszd >>= \w ->
	f w frszd <* (Glfw.destroyWindow w >> Glfw.terminate)
	where
	initWindow frszd = do
		True <- Glfw.init
		Glfw.windowHint $ Glfw.WindowHint'ClientAPI Glfw.ClientAPI'NoAPI
		Just w <- Glfw.createWindow wdt hgt nm Nothing Nothing
		(w <$) . Glfw.setFramebufferSizeCallback w $ Just \_ _ _ ->
			writeIORef frszd True
	nm = "Spinning Monkey"; wdt = 800; hgt = 600

createInstance :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createInstance f = do
	when enableValidationLayers $ bool (error msg) (pure ()) =<< null
		. ([Vk.Khr.validationLayerName] \\)
		. (Vk.M.layerPropertiesLayerName <$>)
		<$> Vk.Ist.M.enumerateLayerProperties
	exts <- bool id (Vk.Ext.DbgUtls.extensionName :) enableValidationLayers
		<$> ((cstrToText `mapM`) =<< Glfw.getRequiredInstanceExtensions)
	Vk.Ist.create (crInfo exts) nil nil f
	where
	msg = "validation layers requested, but not available!"
	crInfo :: [Txt.Text] -> Vk.Ist.M.CreateInfo
		(Vk.Ext.DbgUtls.Msngr.CreateInfo () () () () () ()) ()
	crInfo exts = Vk.Ist.M.CreateInfo {
		Vk.Ist.M.createInfoNext = bool Nothing
			(Just debugMessengerInfo) enableValidationLayers,
		Vk.Ist.M.createInfoFlags = zeroBits,
		Vk.Ist.M.createInfoApplicationInfo = Just appInfo,
		Vk.Ist.M.createInfoEnabledLayerNames = bool
			[] [Vk.Khr.validationLayerName] enableValidationLayers,
		Vk.Ist.M.createInfoEnabledExtensionNames = exts }
	appInfo = Vk.M.ApplicationInfo {
		Vk.M.applicationInfoNext = Nothing,
		Vk.M.applicationInfoApplicationName =
			"Vulkan Guide with Dynamic Descriptor Sets",
		Vk.M.applicationInfoApplicationVersion =
			Vk.M.makeApiVersion 0 1 0 0,
		Vk.M.applicationInfoEngineName = "No Engine",
		Vk.M.applicationInfoEngineVersion = Vk.M.makeApiVersion 0 1 0 0,
		Vk.M.applicationInfoApiVersion = Vk.M.apiVersion_1_0 }

debugMessengerInfo :: Vk.Ext.DbgUtls.Msngr.CreateInfo () () () () () ()
debugMessengerInfo = Vk.Ext.DbgUtls.Msngr.CreateInfo {
	Vk.Ext.DbgUtls.Msngr.createInfoNext = Nothing,
	Vk.Ext.DbgUtls.Msngr.createInfoFlags = zeroBits,
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
	where debugCallback _msgSeverity _msgType callbackData _userData =
		False <$ Txt.putStrLn (
			"validation layer: " <>
			Vk.Ext.DbgUtls.Msngr.callbackDataMessage callbackData )

run :: Glfw.Window -> Vk.Ist.I s -> FramebufferResized -> V.Vector Vertex -> IO ()
run w ist g vns =
	Glfw.createWindowSurface ist w nil nil \sfc ->
	pickPhysicalDevice ist sfc >>= \(phd, qfs) ->
	putStrLn "MIN ALIGN" >>
	(print . Vk.Phd.limitsMinUniformBufferOffsetAlignment
		. Vk.Phd.propertiesLimits =<< Vk.Phd.getProperties phd) >>
	createDevice phd qfs \dv gq pq ->
	createSwapchain w sfc phd qfs dv \(sc :: Vk.Khr.Swpch.SNew ss fmt) ex ->
	Vk.Khr.Swpch.getImagesNew dv sc >>= \imgs ->
	createImageViews dv imgs \scivs ->
	findDepthFormat phd >>= \dptfmt ->
	Vk.T.formatToType dptfmt \(_ :: Proxy dptfmt) ->
	createRenderPass @fmt @dptfmt dv \rp ->

	createDescriptorSetLayout dv \dslyt ->
	createPipelineLayout dv dslyt \ppllyt ->
	createGraphicsPipeline dv ex rp ppllyt \gpl ->

	createCommandPool qfs dv \cp ->
	createDepthResources phd dv gq cp ex \dptImg dptImgMem dptImgVw ->
	createFramebuffers dv ex rp scivs dptImgVw \fbs ->

	createCameraBuffers phd dv dslyt maxFramesInFlight \cmlyts cmbs cmms ->
	createSceneBuffer phd dv \scnb scnm ->
	createDescriptorPool dv \cmdp ->
	createDescriptorSets dv cmdp cmbs cmlyts scnb >>= \cmds ->
	createVertexBuffer phd dv gq cp vns \vb ->
	createVertexBuffer phd dv gq cp triangle \vbtri ->
	createCommandBuffers dv cp \cbs ->
	createSyncObjects dv \sos ->
	mainLoop g w sfc phd qfs dv gq pq sc ex scivs rp ppllyt
		gpl cp (dptImg, dptImgMem, dptImgVw) fbs vb vbtri cbs sos cmms scnm cmds (fromIntegral $ V.length vns)

pickPhysicalDevice ::
	Vk.Ist.I si -> Vk.Khr.Sfc.S ss -> IO (Vk.Phd.P, QueueFamilyIndices)
pickPhysicalDevice ist sfc = Vk.Phd.enumerate ist >>= \dvs -> do
	when (null dvs) $ error "failed to find GPUs with Gpu.Vulkan support!"
	findPlusM (`isPhysicalDeviceSuitable` sfc) dvs >>= \case
		Just ph -> pure ph
		Nothing -> error "failed to find a suitable GPU!"

isPhysicalDeviceSuitable ::
	Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (Maybe QueueFamilyIndices)
isPhysicalDeviceSuitable ph sfc =
	(checkDeviceExtensionSupport ph >>=) . bool (pure Nothing) $
	findQueueFamilies ph sfc >>= \qfis ->
	((<$> getSwapchainSupport ph sfc) \spp ->
	bool (completeQueueFamilies qfis) Nothing
		$ null (formats spp) || null (presentModes spp))

checkDeviceExtensionSupport :: Vk.Phd.P -> IO Bool
checkDeviceExtensionSupport dv = null
	. (deviceExtensions \\) . (Vk.M.extensionPropertiesExtensionName <$>)
		<$> Vk.Phd.enumerateExtensionProperties dv Nothing

deviceExtensions :: [Txt.Text]
deviceExtensions = [Vk.Khr.Swpch.M.extensionName]

findQueueFamilies :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO QueueFamilyIndicesMaybe
findQueueFamilies dv sfc = Vk.Phd.getQueueFamilyProperties dv >>= \qfs ->
	filterM	(\i -> Vk.Khr.Sfc.Phd.getSupport dv i sfc)
		(fst <$> qfs) >>= \(listToMaybe -> pfi) ->
	pure QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = (`findBySnd` qfs)
			$ checkBits Vk.Queue.GraphicsBit
				. Vk.QueueFamily.propertiesQueueFlags,
		presentFamilyMaybe = pfi }

data QueueFamilyIndices = QueueFamilyIndices {
	graphicsFamily :: Vk.QueueFamily.Index,
	presentFamily :: Vk.QueueFamily.Index }

data QueueFamilyIndicesMaybe = QueueFamilyIndicesMaybe {
	graphicsFamilyMaybe :: Maybe Vk.QueueFamily.Index,
	presentFamilyMaybe :: Maybe Vk.QueueFamily.Index }

completeQueueFamilies :: QueueFamilyIndicesMaybe -> Maybe QueueFamilyIndices
completeQueueFamilies = \case
	QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = Just g, presentFamilyMaybe = Just p } ->
		Just QueueFamilyIndices {
			graphicsFamily = g, presentFamily = p }
	_ -> Nothing

createDevice :: Vk.Phd.P -> QueueFamilyIndices ->
	(forall sd . Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> IO a) -> IO a
createDevice ph qfis f = mkHeteroParList @() qcrInfo qfs \qcris ->
	Vk.Dvc.create @() ph (crInfo qcris) nil nil \dv -> do
		gq <- Vk.Dvc.getQueue dv (graphicsFamily qfis) 0
		pq <- Vk.Dvc.getQueue dv (presentFamily qfis) 0
		f dv gq pq
	where
	qfs = nub [graphicsFamily qfis, presentFamily qfis]
	qcrInfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = Nothing,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }
	crInfo qcris = Vk.Dvc.M.CreateInfo {
		Vk.Dvc.M.createInfoNext = Nothing,
		Vk.Dvc.M.createInfoFlags = zeroBits,
		Vk.Dvc.M.createInfoQueueCreateInfos = qcris,
		Vk.Dvc.M.createInfoEnabledLayerNames = bool
			[] [Vk.Khr.validationLayerName] enableValidationLayers,
		Vk.Dvc.M.createInfoEnabledExtensionNames = deviceExtensions,
		Vk.Dvc.M.createInfoEnabledFeatures = Just def }

mkHeteroParList :: WithPoked s => (a -> t s) -> [a] ->
	(forall ss . WithPokedHeteroToListM ss => HL.PL t ss -> b) ->
	b
mkHeteroParList _k [] f = f HL.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

enableValidationLayers :: Bool
enableValidationLayers = maybe True (const False) $(lookupCompileEnv "NDEBUG")

createSwapchain :: Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> (forall ss scfmt .
		Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swpch.SNew ss scfmt -> Vk.C.Extent2d -> IO a) -> IO a
createSwapchain w sfc ph qfs dv f = getSwapchainSupport ph sfc >>= \spp -> do
	ex <- chooseSwapExtent w $ capabilities spp
	let	fmt = Vk.Khr.Sfc.M.formatFormat
			. chooseSwapSurfaceFormat $ formats spp
	Vk.T.formatToType fmt \(_ :: Proxy fmt) ->
		Vk.Khr.Swpch.createNew @() @_ @_ @fmt dv
			(swapchainCreateInfo sfc qfs spp ex) nil nil (`f` ex)

recreateSwapchain :: Vk.T.FormatToValue scfmt =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Khr.Swpch.SNew ssc scfmt ->
	IO Vk.C.Extent2d
recreateSwapchain w sfc ph qfs dv sc = getSwapchainSupport ph sfc >>= \spp -> do
	ex <- chooseSwapExtent w $ capabilities spp
	ex <$ Vk.Khr.Swpch.recreateNew @() dv
		(swapchainCreateInfo sfc qfs spp ex) nil nil sc

getSwapchainSupport :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO SwapchainSupportDetails
getSwapchainSupport dv sfc = SwapchainSupportDetails
	<$> Vk.Khr.Sfc.Phd.getCapabilities dv sfc
	<*> Vk.Khr.Sfc.Phd.getFormats dv sfc
	<*> Vk.Khr.Sfc.Phd.getPresentModes dv sfc

chooseSwapExtent :: Glfw.Window -> Vk.Khr.Sfc.M.Capabilities -> IO Vk.C.Extent2d
chooseSwapExtent win caps
	| Vk.C.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> w, fromIntegral -> h) <-
			Glfw.getFramebufferSize win
		pure $ Vk.C.Extent2d
			(clamp w (Vk.C.extent2dWidth n) (Vk.C.extent2dWidth x))
			(clamp h (Vk.C.extent2dHeight n)
				(Vk.C.extent2dHeight x))
	where
	curExt = Vk.Khr.Sfc.M.capabilitiesCurrentExtent caps
	n = Vk.Khr.Sfc.M.capabilitiesMinImageExtent caps
	x = Vk.Khr.Sfc.M.capabilitiesMaxImageExtent caps

data SwapchainSupportDetails = SwapchainSupportDetails {
	capabilities :: Vk.Khr.Sfc.M.Capabilities,
	formats :: [Vk.Khr.Sfc.M.Format],
	presentModes :: [Vk.Khr.PresentMode] }

swapchainCreateInfo :: Vk.Khr.Sfc.S ss -> QueueFamilyIndices ->
	SwapchainSupportDetails -> Vk.C.Extent2d ->
	Vk.Khr.Swpch.CreateInfoNew n ss fmt
swapchainCreateInfo sfc qfs spp ext = Vk.Khr.Swpch.CreateInfoNew {
	Vk.Khr.Swpch.createInfoNextNew = Nothing,
	Vk.Khr.Swpch.createInfoFlagsNew = zeroBits,
	Vk.Khr.Swpch.createInfoSurfaceNew = sfc,
	Vk.Khr.Swpch.createInfoMinImageCountNew = imgc,
	Vk.Khr.Swpch.createInfoImageColorSpaceNew =
		Vk.Khr.Sfc.M.formatColorSpace fmt,
	Vk.Khr.Swpch.createInfoImageExtentNew = ext,
	Vk.Khr.Swpch.createInfoImageArrayLayersNew = 1,
	Vk.Khr.Swpch.createInfoImageUsageNew = Vk.Img.UsageColorAttachmentBit,
	Vk.Khr.Swpch.createInfoImageSharingModeNew = ism,
	Vk.Khr.Swpch.createInfoQueueFamilyIndicesNew = qfis,
	Vk.Khr.Swpch.createInfoPreTransformNew =
		Vk.Khr.Sfc.M.capabilitiesCurrentTransform caps,
	Vk.Khr.Swpch.createInfoCompositeAlphaNew =
		Vk.Khr.CompositeAlphaOpaqueBit,
	Vk.Khr.Swpch.createInfoPresentModeNew = Vk.Khr.PresentModeFifo,
	Vk.Khr.Swpch.createInfoClippedNew = True,
	Vk.Khr.Swpch.createInfoOldSwapchainNew = Nothing }
	where
	imgc = clamp (Vk.Khr.Sfc.M.capabilitiesMinImageCount caps + 1) 0
		. fromMaybe maxBound . onlyIf (> 0)
		$ Vk.Khr.Sfc.M.capabilitiesMaxImageCount caps
	fmt = chooseSwapSurfaceFormat $ formats spp
	caps = capabilities spp
	(ism, qfis) = bool
		(Vk.SharingModeConcurrent,
			[graphicsFamily qfs, presentFamily qfs])
		(Vk.SharingModeExclusive, [])
		(graphicsFamily qfs == presentFamily qfs)

chooseSwapSurfaceFormat  :: [Vk.Khr.Sfc.M.Format] -> Vk.Khr.Sfc.M.Format
chooseSwapSurfaceFormat = \case
	fs@(f : _) -> fromMaybe f $ find preferred fs
	_ -> error "no available swap surface formats"
	where
	preferred f = Vk.Khr.Sfc.M.formatFormat f == Vk.FormatB8g8r8a8Srgb &&
		Vk.Khr.Sfc.M.formatColorSpace f ==
			Vk.Khr.ColorSpaceSrgbNonlinear

createImageViews :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Img.BindedNew ss ss nm fmt] ->
	(forall s . HL.PL (Vk.ImgVw.INew fmt nm) s -> IO a) -> IO a
createImageViews _ [] f = f HL.Nil
createImageViews dv (sci : scis) f =
	createImageView dv sci Vk.Img.AspectColorBit \sciv ->
	createImageViews dv scis \scivs -> f $ sciv :** scivs

createImageView :: forall ivfmt sd si sm nm ifmt a . Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.BindedNew si sm nm ifmt ->
	Vk.Img.AspectFlags ->
	(forall siv . Vk.ImgVw.INew ivfmt nm siv -> IO a) -> IO a
createImageView dv img asps =
	Vk.ImgVw.createNew dv (imageViewCreateInfo img asps) nil nil

recreateImageViews :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Img.BindedNew ss ss nm fmt] ->
	HL.PL (Vk.ImgVw.INew fmt nm) sis -> IO ()
recreateImageViews _dv [] HL.Nil = pure ()
recreateImageViews dv (sci : scis) (iv :** ivs) =
	recreateImageView dv sci Vk.Img.AspectColorBit iv >>
	recreateImageViews dv scis ivs
recreateImageViews _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

recreateImageView :: Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.BindedNew si sm nm ifmt ->
	Vk.Img.AspectFlags -> Vk.ImgVw.INew ivfmt nm s -> IO ()
recreateImageView dv img asps iv =
	Vk.ImgVw.recreateNew dv (imageViewCreateInfo img asps) nil nil iv

imageViewCreateInfo ::
	Vk.Img.BindedNew si sm nm ifmt -> Vk.Img.AspectFlags ->
	Vk.ImgVw.CreateInfoNew () si sm nm ifmt ivfmt
imageViewCreateInfo img asps = Vk.ImgVw.CreateInfoNew {
	Vk.ImgVw.createInfoNextNew = Nothing,
	Vk.ImgVw.createInfoFlagsNew = zeroBits,
	Vk.ImgVw.createInfoImageNew = img,
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

findDepthFormat :: Vk.Phd.P -> IO Vk.Format
findDepthFormat phd = findSupportedFormat phd
	[Vk.FormatD32Sfloat, Vk.FormatD32SfloatS8Uint, Vk.FormatD24UnormS8Uint]
	Vk.Img.TilingOptimal Vk.FormatFeatureDepthStencilAttachmentBit

findSupportedFormat :: Vk.Phd.P ->
	[Vk.Format] -> Vk.Img.Tiling -> Vk.FormatFeatureFlags -> IO Vk.Format
findSupportedFormat phd fs tlng fffs = do
	props <- Vk.Phd.getFormatProperties phd `mapM` fs
	case tlng of
		Vk.Img.TilingLinear -> pure . orError
			. find (checkBits fffs . snd) . zip fs
			$ Vk.formatPropertiesLinearTilingFeatures <$> props
		Vk.Img.TilingOptimal -> pure . orError
			. find (checkBits fffs . snd) . zip fs
			$ Vk.formatPropertiesOptimalTilingFeatures <$> props
		_ -> error "no such image tiling"
	where orError = maybe (error "failed to find supported format!") fst

createRenderPass ::
	forall (scifmt :: Vk.T.Format) (dptfmt :: Vk.T.Format) sd a . (
	Vk.T.FormatToValue scifmt, Vk.T.FormatToValue dptfmt ) =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPass.R sr -> IO a) -> IO a
createRenderPass dv f = Vk.RndrPass.createNew @'[scifmt, dptfmt] @()
	dv renderPassInfo nil nil f where
	renderPassInfo = Vk.RndrPass.M.CreateInfoNew {
		Vk.RndrPass.M.createInfoNextNew = Nothing,
		Vk.RndrPass.M.createInfoFlagsNew = zeroBits,
		Vk.RndrPass.M.createInfoAttachmentsNew =
			colorAttachment :** depthAttachment :** HL.Nil,
		Vk.RndrPass.M.createInfoSubpassesNew = [subpass],
		Vk.RndrPass.M.createInfoDependenciesNew = [dependency] }
	colorAttachment :: Vk.Att.DescriptionNew scifmt
	colorAttachment = Vk.Att.DescriptionNew {
		Vk.Att.descriptionFlagsNew = zeroBits,
		Vk.Att.descriptionSamplesNew = Vk.Sample.Count1Bit,
		Vk.Att.descriptionLoadOpNew = Vk.Att.LoadOpClear,
		Vk.Att.descriptionStoreOpNew = Vk.Att.StoreOpStore,
		Vk.Att.descriptionStencilLoadOpNew = Vk.Att.LoadOpDontCare,
		Vk.Att.descriptionStencilStoreOpNew = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionInitialLayoutNew = Vk.Img.LayoutUndefined,
		Vk.Att.descriptionFinalLayoutNew = Vk.Img.LayoutPresentSrcKhr }
	depthAttachment :: Vk.Att.DescriptionNew dptfmt
	depthAttachment = Vk.Att.DescriptionNew {
		Vk.Att.descriptionFlagsNew = zeroBits,
		Vk.Att.descriptionSamplesNew = Vk.Sample.Count1Bit,
		Vk.Att.descriptionLoadOpNew = Vk.Att.LoadOpClear,
		Vk.Att.descriptionStoreOpNew = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionStencilLoadOpNew = Vk.Att.LoadOpDontCare,
		Vk.Att.descriptionStencilStoreOpNew = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionInitialLayoutNew = Vk.Img.LayoutUndefined,
		Vk.Att.descriptionFinalLayoutNew =
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
	colorAttachmentRef = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 0,
		Vk.Att.referenceLayout = Vk.Img.LayoutColorAttachmentOptimal }
	depthAttachmentRef = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 1,
		Vk.Att.referenceLayout =
				Vk.Img.LayoutDepthStencilAttachmentOptimal }
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

type CameraObj = VObj.Atom 256 GpuCameraData 'Nothing
type SceneObj = VObj.DynAtom 2 256 SceneData 'Nothing

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscSetLyt.L s '[
		'Vk.DscSetLyt.Buffer '[CameraObj],
		'Vk.DscSetLyt.Buffer '[SceneObj] ] -> IO a) -> IO a
createDescriptorSetLayout dv = Vk.DscSetLyt.create dv layoutInfo nil nil where
	layoutInfo :: Vk.DscSetLyt.CreateInfo () '[
		'Vk.DscSetLyt.Buffer '[CameraObj],
		'Vk.DscSetLyt.Buffer '[SceneObj] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = Nothing,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = camera :** scene :** HL.Nil }
	camera :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[CameraObj])
	camera = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	scene :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[SceneObj])
	scene = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBufferDynamic,
		Vk.DscSetLyt.bindingBufferStageFlags =
			Vk.ShaderStageVertexBit .|. Vk.ShaderStageFragmentBit }

createPipelineLayout :: forall sd sdl a . Vk.Dvc.D sd ->
	Vk.DscSetLyt.L sdl '[
		'Vk.DscSetLyt.Buffer '[CameraObj],
		'Vk.DscSetLyt.Buffer '[SceneObj] ] ->
	(forall sl . Vk.Ppl.Lyt.L sl
		'[ '(sdl, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])]
		'[WrapMeshPushConstants] -> IO a) -> IO a
createPipelineLayout dv dslyt f = Vk.Ppl.Lyt.createNew dv ci nil nil f where
	ci :: Vk.Ppl.Lyt.CreateInfoNew ()
		'[ '(sdl, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])]
		('Vk.PushConstant.PushConstantLayout
			'[ WrapMeshPushConstants]
			'[ 'Vk.PushConstant.Range
				'[ 'Vk.T.ShaderStageVertexBit]
				'[WrapMeshPushConstants] ])
	ci = Vk.Ppl.Lyt.CreateInfoNew {
		Vk.Ppl.Lyt.createInfoNextNew = Nothing,
		Vk.Ppl.Lyt.createInfoFlagsNew = zeroBits,
		Vk.Ppl.Lyt.createInfoSetLayoutsNew = HL.Singleton $ U2 dslyt }

createGraphicsPipeline :: Vk.Dvc.D sd -> Vk.C.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(sdl, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])]
		'[WrapMeshPushConstants] ->
	(forall sg . Vk.Ppl.Grph.GNew sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(sdl, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])], '[WrapMeshPushConstants]) -> IO a) -> IO a
createGraphicsPipeline dv sce rp lyt f = Vk.Ppl.Grph.createGs dv Nothing
	(HL.Singleton . U14 $ graphicsPipelineCreateInfo sce rp lyt) nil nil
	\(HL.Singleton (U3 gpl)) -> f gpl

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(sdl, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Grph.GNew sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(sdl, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])], '[WrapMeshPushConstants]) -> IO ()
recreateGraphicsPipeline dv sce rp lyt gpls = Vk.Ppl.Grph.recreateGs dv Nothing
	(U14 (graphicsPipelineCreateInfo sce rp lyt) :** HL.Nil) nil nil
	(U3 gpls :** HL.Nil)

graphicsPipelineCreateInfo ::
	Vk.C.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Lyt.L sl
		'[ '(sdl, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Grph.CreateInfo ()
		'[	'((), (), 'GlslVertexShader, (), (), '[]),
			'((), (), 'GlslFragmentShader, (), (), '[]) ]
		'(	(), '[ '(Vertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Position), '(1, Normal), '(2, Color)] )
		() () () () () () () ()
		'(sl,	'[ '(sdl, '[
				'Vk.DscSetLyt.Buffer '[CameraObj],
				'Vk.DscSetLyt.Buffer '[SceneObj] ])],
			'[WrapMeshPushConstants])
		sr '(sb, vs', ts', larg)
graphicsPipelineCreateInfo sce rp ppllyt = Vk.Ppl.Grph.CreateInfo {
	Vk.Ppl.Grph.createInfoNext = Nothing,
	Vk.Ppl.Grph.createInfoFlags = Vk.Ppl.CreateFlagsZero,
	Vk.Ppl.Grph.createInfoStages = uncurry shaderStages shaderPair1,
	Vk.Ppl.Grph.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Grph.createInfoInputAssemblyState = Just inputAssembly,
	Vk.Ppl.Grph.createInfoViewportState = Just $ mkViewportState sce,
	Vk.Ppl.Grph.createInfoRasterizationState = Just rasterizer,
	Vk.Ppl.Grph.createInfoMultisampleState = Just multisampling,
	Vk.Ppl.Grph.createInfoDepthStencilState = Just depthStencil,
	Vk.Ppl.Grph.createInfoColorBlendState = Just colorBlending,
	Vk.Ppl.Grph.createInfoDynamicState = Nothing,
	Vk.Ppl.Grph.createInfoLayout = U3 ppllyt,
	Vk.Ppl.Grph.createInfoRenderPass = rp,
	Vk.Ppl.Grph.createInfoSubpass = 0,
	Vk.Ppl.Grph.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Grph.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Grph.createInfoTessellationState = Nothing }
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

inputAssembly :: Vk.Ppl.InpAsmbSt.CreateInfo ()
inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
	Vk.Ppl.InpAsmbSt.createInfoNext = Nothing,
	Vk.Ppl.InpAsmbSt.createInfoFlags = zeroBits,
	Vk.Ppl.InpAsmbSt.createInfoTopology = Vk.PrimitiveTopologyTriangleList,
	Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable = False }

mkViewportState :: Vk.C.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo n
mkViewportState sce = def {
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
	Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeNone, -- Vk.CullModeBackBit,
	Vk.Ppl.RstSt.createInfoFrontFace = Vk.FrontFaceClockwise,
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

createDepthResources ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPl.C sc ->
	Vk.C.Extent2d ->
	(forall si sm fmt siv . Vk.T.FormatToValue fmt =>
		Vk.Img.BindedNew si sm nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.K.Image nm fmt) ] ->
		Vk.ImgVw.INew fmt nm siv ->
		IO a) -> IO a
createDepthResources phd dvc gq cp ext f = do
	fmt <- findDepthFormat phd
	print fmt
	print ext
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		createImage @_ @fmt phd dvc
			(Vk.C.extent2dWidth ext) (Vk.C.extent2dHeight ext)
			Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
			Vk.Mem.PropertyDeviceLocalBit \dptImg dptImgMem ->
			createImageView @fmt
				dvc dptImg Vk.Img.AspectDepthBit \dptImgVw -> do
			transitionImageLayout dvc gq cp dptImg Vk.Img.LayoutUndefined
				Vk.Img.LayoutDepthStencilAttachmentOptimal
			f dptImg dptImgMem dptImgVw

recreateDepthResources :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.CmdPl.C sc ->
	Vk.C.Extent2d ->
	Vk.Img.BindedNew sb sm nm fmt ->
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.K.Image nm fmt)] ->
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
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.K.Image nm fmt)],
	Vk.ImgVw.INew fmt nm sdiv )

createImage :: forall nm fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P ->
	Vk.Dvc.D sd -> Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mem.PropertyFlagBits -> (forall si sm .
		Vk.Img.BindedNew si sm nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.K.Image nm fmt) ] ->
		IO a) -> IO a
createImage pd dvc wdt hgt tlng usg prps f = Vk.Img.createNew @() @() @() dvc
		(imageInfo wdt hgt tlng usg) Nothing Nothing \img -> do
	memInfo <- imageMemoryInfo pd dvc prps img
	imageAllocateBind dvc img memInfo f

recreateImage :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mem.PropertyFlags ->
	Vk.Img.BindedNew sb sm nm fmt ->
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.K.Image nm fmt)] -> IO ()
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
	Vk.Dvc.Mem.AllocateInfo () -> (forall sm .
		Vk.Img.BindedNew si sm nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.K.Image nm fmt) ] ->
		IO a) -> IO a
imageAllocateBind dvc img memInfo f =
	Vk.Mem.allocateBind @() dvc
		(HL.Singleton . U2 $ Vk.Mem.Image img) memInfo
		nil nil \(HL.Singleton (U2 (Vk.Mem.ImageBinded bnd))) m -> do
		f bnd m

imageReallocateBind ::
	Vk.Dvc.D sd -> Vk.Img.BindedNew sb sm nm fmt ->
	Vk.Dvc.Mem.AllocateInfo () ->
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.K.Image nm fmt)] -> IO ()
imageReallocateBind dvc img memInfo m =
	Vk.Mem.reallocateBind @() dvc
		(HL.Singleton . U2 $ Vk.Mem.ImageBinded img) memInfo
		nil nil m

imageMemoryInfo ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Mem.PropertyFlags ->
	Vk.Img.INew s nm fmt -> IO (Vk.Dvc.Mem.AllocateInfo n)
imageMemoryInfo pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirementsNew dvc img
	mt <- findMemoryType pd (Vk.Mem.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Dvc.Mem.AllocateInfo {
		Vk.Dvc.Mem.allocateInfoNext = Nothing,
		Vk.Dvc.Mem.allocateInfoMemoryTypeIndex = mt }

imageMemoryInfoBinded ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Mem.PropertyFlags ->
	Vk.Img.BindedNew sm si nm fmt -> IO (Vk.Dvc.Mem.AllocateInfo n)
imageMemoryInfoBinded pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirementsBindedNew dvc img
	mt <- findMemoryType pd (Vk.Mem.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Dvc.Mem.AllocateInfo {
		Vk.Dvc.Mem.allocateInfoNext = Nothing,
		Vk.Dvc.Mem.allocateInfoMemoryTypeIndex = mt }

transitionImageLayout :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPl.C sc ->
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
		sstg dstg zeroBits HL.Nil HL.Nil (HL.Singleton $ U5 barrier)
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
	(forall s . Vk.CmdBffr.Binded s '[] -> IO a) -> IO a
beginSingleTimeCommands dvc gq cp cmd = do
	Vk.CmdBffr.allocate
		@() dvc allocInfo \(HL.Singleton (cb :: Vk.CmdBffr.Binded s '[])) -> do
		let	submitInfo :: Vk.SubmitInfo () '[] '[ '(s, '[])] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = Nothing,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Nil,
				Vk.submitInfoCommandBuffers = HL.Singleton $ U2 cb,
				Vk.submitInfoSignalSemaphores = HL.Nil }
		Vk.CmdBffr.begin @() @() cb beginInfo (cmd cb) <* do
			Vk.Queue.submit gq (HL.Singleton $ U4 submitInfo) Nothing
			Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo () sc '[ '[]]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = Nothing,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.M.BeginInfo {
		Vk.CmdBffr.beginInfoNext = Nothing,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }

createFramebuffers :: Vk.Dvc.D sd -> Vk.C.Extent2d ->
	Vk.RndrPass.R sr -> HL.PL (Vk.ImgVw.INew fmt nm) sis ->
	Vk.ImgVw.INew dptfmt dptnm siv ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HL.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HL.Nil _ f = f HL.Nil
createFramebuffers dvc sce rp (iv :** ivs) dptiv f =
	Vk.Frmbffr.createNew dvc (mkFramebufferCreateInfo sce rp iv dptiv) nil nil \fb ->
	createFramebuffers dvc sce rp ivs dptiv \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.C.Extent2d ->
		Vk.RndrPass.R sr -> HL.PL (Vk.ImgVw.INew scfmt nm) sis ->
		Vk.ImgVw.INew dptfmt dptnm sdiv ->
		HL.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HL.Nil _ HL.Nil = pure ()

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
	Vk.Frmbffr.createInfoAttachmentsNew = U3 attch :** U3 dpt :** HL.Nil,
	Vk.Frmbffr.createInfoWidthNew = w, Vk.Frmbffr.createInfoHeightNew = h,
	Vk.Frmbffr.createInfoLayersNew = 1 }
	where
	Vk.C.Extent2d { Vk.C.extent2dWidth = w, Vk.C.extent2dHeight = h } = sce

createCommandPool :: QueueFamilyIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCommandPool qfis dv = Vk.CmdPl.create @() dv crInfo nil nil
	where crInfo = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = Nothing,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = graphicsFamily qfis }

createVertexBuffer :: forall sd sc vbnm a . Vk.Phd.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPl.C sc -> V.Vector Vertex ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb vbnm '[VObj.List 256 Vertex ""] -> IO a ) -> IO a
createVertexBuffer phdvc dvc gq cp vtcs f =
	createBuffer phdvc dvc (HL.Singleton . VObj.ObjectLengthList $ V.length vtcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBuffer phdvc dvc (HL.Singleton . VObj.ObjectLengthList $ V.length vtcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
			\b' (bm' :: Vk.Mem.M sm '[
				'(sb, 'Vk.Mem.K.Buffer vbnm '[VObj.List 256 Vertex ""])
				]) -> do
	Vk.Mem.write @vbnm @(VObj.List 256 Vertex "") dvc bm' zeroBits vtcs
	copyBuffer dvc gq cp b' b
	f b

createCameraBuffers :: Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.DscSetLyt.L sdsc '[
		'Vk.DscSetLyt.Buffer '[CameraObj],
		'Vk.DscSetLyt.Buffer '[SceneObj] ] -> Int ->
	(forall slyts sbsms . (
		Vk.DscSet.SListFromMiddle slyts,
		HL.FromList slyts, Update sbsms slyts,
		HL.HomoList '(sdsc, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ]) slyts ) =>
		HL.PL Vk.DscSet.Layout slyts ->
		HL.PL BindedGcd sbsms ->
		HL.PL MemoryGcd sbsms -> IO a) -> IO a
createCameraBuffers _ _ _ n f | n < 1 = f HL.Nil HL.Nil HL.Nil
createCameraBuffers phdvc dvc lyt n f = createCameraBuffer phdvc dvc \bnd mem ->
	createCameraBuffers phdvc dvc lyt (n - 1) \lyts bnds mems ->
	f (U2 lyt :** lyts) (BindedGcd bnd :** bnds) (MemoryGcd mem :** mems)

createCameraBuffer :: Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sb sm nm '[CameraObj] ->
		Vk.Mem.M sm '[ '(
			sb, 'Vk.Mem.K.Buffer nm '[CameraObj]) ] ->
		IO a) -> IO a
createCameraBuffer phdvc dvc = createBuffer phdvc dvc (HL.Singleton VObj.ObjectLengthAtom)
	Vk.Bffr.UsageUniformBufferBit Vk.Mem.PropertyHostVisibleBit

createSceneBuffer :: Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sb sm nm '[
			SceneObj,
			SceneObj ] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.K.Buffer nm '[
				SceneObj,
				SceneObj ] ) ] ->
		IO a) -> IO a
createSceneBuffer phdvc dvc = createBuffer2 phdvc dvc
	(VObj.ObjectLengthDynAtom :** VObj.ObjectLengthDynAtom :** HL.Nil)
	Vk.Bffr.UsageUniformBufferBit Vk.Mem.PropertyHostVisibleBit

createBuffer :: forall obj nm sd a . (
	VObj.WholeSize '[obj],
	VObj.SizeAlignment obj
--	Vk.Mem.Alignments '[
--		'(s, 'Vk.Mem.K.Buffer nm objs) ]
	) => -- VObj.SizeAlignment obj =>
	Vk.Phd.P -> Vk.Dvc.D sd -> HL.PL VObj.ObjectLength '[obj] ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sb sm nm '[obj] ->
		Vk.Mem.M sm '[
			'(sb, 'Vk.Mem.K.Buffer nm '[obj])
			] -> IO a ) -> IO a
createBuffer p dv lns usg props f = Vk.Bffr.create dv bffrInfo nil nil
		\b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Mem.allocateBind dv
		(HL.Singleton . U2 $ Vk.Mem.Buffer b) (allcInfo mt) nil nil
		$ f . \(HL.Singleton (U2 (Vk.Mem.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo () '[obj]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = Nothing,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = lns,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.M.TypeIndex -> Vk.Dvc.Mem.AllocateInfo ()
	allcInfo mt = Vk.Dvc.Mem.AllocateInfo {
		Vk.Dvc.Mem.allocateInfoNext = Nothing,
		Vk.Dvc.Mem.allocateInfoMemoryTypeIndex = mt }

createBuffer2 :: forall obj obj2 nm sd a . (
	VObj.WholeSize '[obj, obj2],
	VObj.SizeAlignment obj, VObj.SizeAlignment obj2
--	Vk.Mem.Alignments '[
--		'(s, 'Vk.Mem.K.Buffer nm objs) ]
	) => -- VObj.SizeAlignment obj =>
	Vk.Phd.P -> Vk.Dvc.D sd -> HL.PL VObj.ObjectLength '[obj, obj2] ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sb sm nm '[obj, obj2] ->
		Vk.Mem.M sm '[
			'(sb, 'Vk.Mem.K.Buffer nm '[obj, obj2])
			] -> IO a ) -> IO a
createBuffer2 p dv lns usg props f = Vk.Bffr.create dv bffrInfo nil nil
		\b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Mem.allocateBind dv
		(HL.Singleton . U2 $ Vk.Mem.Buffer b) (allcInfo mt) nil nil
		$ f . \(HL.Singleton (U2 (Vk.Mem.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo () '[obj, obj2]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = Nothing,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = lns,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.M.TypeIndex -> Vk.Dvc.Mem.AllocateInfo ()
	allcInfo mt = Vk.Dvc.Mem.AllocateInfo {
		Vk.Dvc.Mem.allocateInfoNext = Nothing,
		Vk.Dvc.Mem.allocateInfoMemoryTypeIndex = mt }

findMemoryType :: Vk.Phd.P -> Vk.Mem.M.TypeBits -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.M.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.Phd.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> find ((&&)
		<$> (`Vk.Mem.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mem.M.mTypePropertyFlags . snd) tps
		where tps = Vk.Phd.memoryPropertiesMemoryTypes props1

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDescriptorPool dvc = Vk.DscPool.create @() dvc poolInfo nil nil
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = Nothing,
		Vk.DscPool.createInfoFlags = zeroBits,
		Vk.DscPool.createInfoMaxSets = 10,
		Vk.DscPool.createInfoPoolSizes = [poolSize0, poolSize1] }
	poolSize0 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }
	poolSize1 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBufferDynamic,
		Vk.DscPool.sizeDescriptorCount = 10 }

createDescriptorSets :: (
	Vk.DscSet.SListFromMiddle ss,
	HL.FromList ss, Update smsbs ss ) =>
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> HL.PL BindedGcd smsbs ->
	HL.PL Vk.DscSet.Layout ss ->
	Vk.Bffr.Binded sb sm "scene-buffer" '[
		SceneObj,
		SceneObj ] ->
	IO (HL.PL (Vk.DscSet.S sd sp) ss)
createDescriptorSets dvc dscp ubs dscslyts scnb = do
	dscss <- Vk.DscSet.allocateSs @() dvc allocInfo
	update dvc ubs dscss scnb 0
	pure dscss
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = Nothing,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = dscslyts }

class Update smsbs slbtss where
	update ::
		Vk.Dvc.D sd ->
		HL.PL BindedGcd smsbs ->
		HL.PL (Vk.DscSet.S sd sp) slbtss ->
		Vk.Bffr.Binded sb sm "scene-buffer" '[
			SceneObj,
			SceneObj ] -> Int -> IO ()

instance Update '[] '[] where update _ HL.Nil HL.Nil _ _ = pure ()

instance (
	Vk.DscSet.T.BindingAndArrayElem (Vk.DscSet.T.BindingTypesFromLayoutArg '(ds, cs)) '[CameraObj],
	Vk.DscSet.T.BindingAndArrayElem (Vk.DscSet.T.BindingTypesFromLayoutArg '(ds, cs)) '[SceneObj],
	Vk.DscSet.T.BindingAndArrayElem (Vk.DscSet.T.BindingTypesFromLayoutArg '(ds, cs)) '[SceneObj],
	Update ubs dscss
	) =>
	Update (ub ': ubs) ('(ds, cs) ': dscss) where
	update dvc (BindedGcd ub :** ubs) (dscs :** dscss) scnb 0 = do
		Vk.DscSet.updateDs @() @() dvc (
			U4 (descriptorWrite0 @GpuCameraData @Nothing ub dscs Vk.Dsc.TypeUniformBuffer) :**
			U4 (descriptorWrite1 @SceneData @'Nothing scnb dscs Vk.Dsc.TypeUniformBufferDynamic) :**
			HL.Nil ) []
		update dvc ubs dscss scnb 1
	update dvc (BindedGcd ub :** ubs) (dscs :** dscss) scnb 1 = do
		Vk.DscSet.updateDs @() @() dvc (
			U4 (descriptorWrite0 @GpuCameraData @Nothing ub dscs Vk.Dsc.TypeUniformBuffer) :**
			U4 (descriptorWrite1 @SceneData @'Nothing scnb dscs Vk.Dsc.TypeUniformBufferDynamic) :**
			HL.Nil ) []
		update dvc ubs dscss scnb 2
	update _ _ _ _ _ = error "bad"

descriptorWrite0 :: forall tp objnm objs sm sb nm sd sp slbts .
	Vk.Bffr.Binded sm sb nm objs ->
	Vk.DscSet.S sd sp slbts -> Vk.Dsc.Type ->
	Vk.DscSet.Write () sd sp slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sb, sm, nm,
		objs,VObj.Atom 256 tp objnm )])
descriptorWrite0 ub dscs tp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = Nothing,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = tp,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HL.Singleton bufferInfo }
	where bufferInfo = Vk.Dsc.BufferInfoAtom ub

descriptorWrite1 :: forall tp objnm objs sm sb nm sd sp slbts .
	Vk.Bffr.Binded sm sb nm objs ->
	Vk.DscSet.S sd sp slbts -> Vk.Dsc.Type ->
	Vk.DscSet.Write () sd sp slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sb, sm, nm,
		objs,VObj.DynAtom 2 256 tp objnm )])
descriptorWrite1 ub dscs tp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = Nothing,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = tp,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HL.Singleton bufferInfo }
	where bufferInfo = Vk.Dsc.BufferInfoDynAtom ub

data BindedGcd smsb where
	BindedGcd ::
		Vk.Bffr.Binded sb sm "camera-buffer" '[CameraObj] ->
		BindedGcd '(sm, sb)

data MemoryGcd smsb where
	MemoryGcd ::
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.K.Buffer "camera-buffer"
				'[CameraObj] )] ->
		MemoryGcd '(sm, sb)

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Vertex ""] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CmdBffr.allocate
		@() dvc allocInfo \(HL.Singleton (cb :: Vk.CmdBffr.Binded s '[])) -> do
		let	submitInfo :: Vk.SubmitInfo () '[] '[ '(s, '[])] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = Nothing,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Nil,
				Vk.submitInfoCommandBuffers = HL.Singleton $ U2 cb,
				Vk.submitInfoSignalSemaphores = HL.Nil }
		Vk.CmdBffr.begin @() @() cb beginInfo do
			Vk.Cmd.copyBuffer @'[ '[VObj.List 256 Vertex ""]] cb src dst
		Vk.Queue.submit gq (HL.Singleton $ U4 submitInfo) Nothing
		Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo () sc '[ '[]]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = Nothing,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.M.BeginInfo {
		Vk.CmdBffr.beginInfoNext = Nothing,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }

createCommandBuffers ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb vss . VssList vss =>
		HL.PL (Vk.CmdBffr.Binded scb) (vss :: [[(Type, Vk.VtxInp.Rate)]]) -> IO a) ->
	IO a
createCommandBuffers dvc cp f = -- mkVss maxFramesInFlight \(_p :: Proxy vss1) ->
	($ (Proxy :: Proxy (MkVss' MaxFramesInFlight))) \(_p :: Proxy vss1) ->
--	valNat' @vss1 maxFramesInFlight \(_p :: Proxy n) ->
--	valNat' maxFramesInFlight \(_p :: Proxy n) ->
		Vk.CmdBffr.allocateNew @() @MaxFramesInFlight dvc allcInfo
			(f @_ @vss1 . (grphToBindedList @(HL.Dummies MaxFramesInFlight) @vss1))

-- mkVss maxFramesInFlight \(_p :: Proxy vss1) ->
--	Vk.CmdBffr.allocate @() @vss1 dvc (allcInfo @vss1) (f @_ @vss1)
	where
	allcInfo :: forall n . Vk.CmdBffr.AllocateInfoNew () scp n
	allcInfo = Vk.CmdBffr.AllocateInfoNew {
		Vk.CmdBffr.allocateInfoNextNew = Nothing,
		Vk.CmdBffr.allocateInfoCommandPoolNew = cp,
		Vk.CmdBffr.allocateInfoLevelNew = Vk.CmdBffr.LevelPrimary }

valNat' :: forall vss a . Natural ->
--	(forall n . (KnownNat n, HL.FromList (HL.Dummies n), GrphToBindedList (HL.Dummies n) vss) => Proxy n -> a) ->
--	(forall n . (KnownNat n, GrphToBindedList (HL.Dummies n) vss) => Proxy n -> a) ->
	(forall n . KnownNat n => Proxy n -> a) ->
	a
valNat' nat f = (\(SomeNat n) -> f n) $ someNatVal nat

class VssList (vss :: [[(Type, Vk.VtxInp.Rate)]]) where
	vssListIndex ::
		HL.PL (Vk.CmdBffr.Binded scb) vss -> Int ->
		Vk.CmdBffr.Binded scb '[AddType Vertex 'Vk.VtxInp.RateVertex]

instance VssList '[] where
	vssListIndex HL.Nil _ = error "index too large"

instance VssList vss =>
	VssList ('[AddType Vertex 'Vk.VtxInp.RateVertex] ': vss) where
	vssListIndex (cb :** _) 0 = cb
	vssListIndex (_ :** cbs) n = vssListIndex cbs (n - 1)

mkVss :: Int -> (forall (vss :: [[(Type, Vk.VtxInp.Rate)]]) .
	(TpLvlLst.Length [(Type, Vk.VtxInp.Rate)] vss, HL.FromList vss, VssList vss) =>
	Proxy vss -> a) -> a
mkVss 0 f = f (Proxy @'[])
mkVss n f = mkVss (n - 1) \p -> f $ addTypeToProxy p

type family MkVss' n where
	MkVss' 0 = '[]
	MkVss' n = '[ '(Vertex, 'Vk.VtxInp.RateVertex)] ': MkVss' (n - 1)

class GrphToBindedList ds vss where
	grphToBindedList ::
		HL.LL (Vk.CmdBffr.C s) ds -> HL.PL (Vk.CmdBffr.Binded s) vss

instance GrphToBindedList '[] '[] where
	grphToBindedList HL.Nil = HL.Nil

instance GrphToBindedList ds vss =>
	GrphToBindedList ('() ': ds) (vs ': vss) where
	grphToBindedList (c :*. cs) = Vk.CmdBffr.toBinded c :** grphToBindedList cs

addTypeToProxy ::
	Proxy vss -> Proxy ('[AddType Vertex 'Vk.VtxInp.RateVertex] ': vss)
addTypeToProxy Proxy = Proxy

data SyncObjects (ssos :: ([Type], [Type], [Type])) where
	SyncObjects :: {
		imageAvailableSemaphores :: HL.PL Vk.Semaphore.S siass,
		renderFinishedSemaphores :: HL.PL Vk.Semaphore.S srfss,
		inFlightFences :: HL.PL Vk.Fence.F sfss } ->
		SyncObjects '(siass, srfss, sfss)

createSyncObjects ::
	Vk.Dvc.D sd -> (forall ssos . SyncObjects ssos -> IO a ) -> IO a
createSyncObjects dvc f =
	HL.replicateM maxFramesInFlight
		(Vk.Semaphore.create @() dvc def nil nil) \iass ->
	HL.replicateM maxFramesInFlight
		(Vk.Semaphore.create @() dvc def nil nil) \rfss ->
	HL.replicateM maxFramesInFlight
		(Vk.Fence.create @() dvc fncInfo nil nil) \iffs ->
	f $ SyncObjects iass rfss iffs
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sg slyt sdlyt sm sb nm smtri sbtri nmtri sd sp .
	Vk.CmdBffr.Binded scb '[AddType Vertex 'Vk.VtxInp.RateVertex] ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.C.Extent2d ->
	Vk.Ppl.Grph.GNew sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt,	'[ '(sdlyt, '[
				'Vk.DscSetLyt.Buffer '[CameraObj],
				'Vk.DscSetLyt.Buffer '[SceneObj] ])],
			'[WrapMeshPushConstants]) ->
	Vk.Ppl.Lyt.L slyt
		'[ '(sdlyt, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])]
		'[WrapMeshPushConstants] ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[VObj.List 256 Vertex ""] -> Int ->
	Vk.DscSet.S sd sp '(sdlyt, '[
		'Vk.DscSetLyt.Buffer '[CameraObj],
		'Vk.DscSetLyt.Buffer '[SceneObj] ]) ->
	Word32 -> Word32 -> IO ()
recordCommandBuffer cb rp fb sce gpl lyt vb vbtri fn cmd vn cf =
	Vk.CmdBffr.begin @() @() cb cbInfo $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline do
	om <- newIORef Nothing
	drawObject om cb cmd RenderObject {
		renderObjectPipeline = gpl,
		renderObjectPipelineLayout = lyt,
		renderObjectMesh = vb,
		renderObjectMeshSize = vn,
		renderObjectTransformMatrix = model } cf
	omtri <- newIORef Nothing
	for_ [- 20 .. 20] \x -> for_ [- 20 .. 20] \y ->
		drawObject omtri cb cmd RenderObject {
			renderObjectPipeline = gpl,
			renderObjectPipelineLayout = lyt,
			renderObjectMesh = vbtri,
			renderObjectMeshSize = 3,
			renderObjectTransformMatrix =
				Cglm.glmMat4Mul (translation x y) scale } cf
	where
	model = Cglm.glmRotate
		Cglm.glmMat4Identity
		(fromIntegral fn * Cglm.glmRad 1)
		(Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL)
	translation x y = Cglm.glmTranslate
		Cglm.glmMat4Identity (Cglm.Vec3 $ x :. 0 :. y :. NilL)
	scale = Cglm.glmScale
		Cglm.glmMat4Identity (Cglm.Vec3 $ 0.2 :. 0.2 :. 0.2 :. NilL)
	cbInfo :: Vk.CmdBffr.BeginInfo () ()
	cbInfo = def {
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit }
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
			Vk.M.ClearValueColor (fromJust $ rgbaDouble 0 0 blue 1) :**
			Vk.M.ClearValueDepthStencil (Vk.C.ClearDepthStencilValue 1 0) :**
			HL.Nil }
	blue = 0.5 + sin (fromIntegral fn / (180 * frashRate) * pi) / 2

data RenderObject sg sl sdlyt sm sb nm = RenderObject {
	renderObjectPipeline :: Vk.Ppl.Grph.GNew sg
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(sdlyt, '[
				'Vk.DscSetLyt.Buffer '[CameraObj],
				'Vk.DscSetLyt.Buffer '[SceneObj] ])],
			'[WrapMeshPushConstants]),
	renderObjectPipelineLayout ::
		Vk.Ppl.Lyt.L sl
			'[ '(sdlyt, '[
				'Vk.DscSetLyt.Buffer '[CameraObj],
				'Vk.DscSetLyt.Buffer '[SceneObj] ])]
			'[WrapMeshPushConstants],
	renderObjectMesh :: Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""],
	renderObjectMeshSize :: Word32,
	renderObjectTransformMatrix :: Cglm.Mat4 }

drawObject :: IORef (Maybe (Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""])) ->
	Vk.CmdBffr.Binded scb '[AddType Vertex 'Vk.VtxInp.RateVertex] ->
	Vk.DscSet.S sd sp '(sdlyt, '[
		'Vk.DscSetLyt.Buffer '[CameraObj],
		'Vk.DscSetLyt.Buffer '[SceneObj] ]) ->
	RenderObject sg sl sdlyt sm sb nm -> Word32 -> IO ()
drawObject om cb cmd RenderObject {
	renderObjectPipeline = gpl,
	renderObjectPipelineLayout = lyt,
	renderObjectMesh = vb,
	renderObjectMeshSize = vn,
	renderObjectTransformMatrix = model } cf = do
	Vk.Cmd.bindPipeline cb Vk.Ppl.BindPointGraphics (Vk.Ppl.Grph.gFromNew gpl)
	Vk.Cmd.bindDescriptorSetsNew cb Vk.Ppl.BindPointGraphics lyt
		(HL.Singleton $ U2 cmd) . HL.Singleton $
		(HL.Nil :** (Vk.Cmd.DynamicIndex cf :** HL.Nil) :** HL.Nil)
	movb <- readIORef om
	case movb of
		Just ovb | vb == ovb -> pure ()
		_ -> do	Vk.Cmd.bindVertexBuffers cb . HL.Singleton
				. U4 $ Vk.Bffr.IndexedList @_ @_ @_ @Vertex vb
			writeIORef om $ Just vb
	Vk.Cmd.pushConstants' @'[ 'Vk.T.ShaderStageVertexBit ] cb lyt $ HL.Id (Foreign.Storable.Generic.Wrap
		MeshPushConstants {
			meshPushConstantsData = Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
			meshPushConstantsRenderMatrix = model
			}) :** HL.Nil
	Vk.Cmd.draw cb vn 1 0 0

view :: Cglm.Mat4
view = Cglm.glmLookat
	(Cglm.Vec3 $ 0 :. 6 :. 10 :. NilL)
	(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
	(Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL)

projection :: Vk.C.Extent2d -> Cglm.Mat4
projection sce = Cglm.modifyMat4 1 1 negate $ Cglm.glmPerspective
	(Cglm.glmRad 70) (fromIntegral (Vk.C.extent2dWidth sce) /
		fromIntegral (Vk.C.extent2dHeight sce)) 0.1 200

mainLoop :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers ss sfs, VssList vss,
	HL.HomoList '(s, '[
		'Vk.DscSetLyt.Buffer '[CameraObj],
		'Vk.DscSetLyt.Buffer '[SceneObj] ]) slyts ) =>
	FramebufferResized ->
	Glfw.Window -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swpch.SNew ssc scfmt -> Vk.C.Extent2d ->
	HL.PL (Vk.ImgVw.INew scfmt nm) ss ->
	Vk.RndrPass.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Grph.GNew sg1
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(s, '[
				'Vk.DscSetLyt.Buffer '[CameraObj],
				'Vk.DscSetLyt.Buffer '[SceneObj] ])],
			'[WrapMeshPushConstants]) ->
	Vk.CmdPl.C scp ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HL.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[VObj.List 256 Vertex ""] ->
	HL.PL (Vk.CmdBffr.Binded scb) vss ->
	SyncObjects siassrfssfs ->
	HL.PL MemoryGcd sbsms ->
	Vk.Mem.M sscnm
		'[ '(sscnb, 'Vk.Mem.K.Buffer
			"scene-buffer" '[
				SceneObj,
				SceneObj ])] ->
	HL.PL (Vk.DscSet.S sd sp) slyts ->
	Word32 -> IO ()
mainLoop g w sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl1 cp drsrcs fbs vb vbtri cbs iasrfsifs cmms scnm cmds vn = do
	($ 0) . ($ cycle [0 .. maxFramesInFlight - 1]) . ($ ext0) $ fix \loop ext (cf : cfs) fn -> do
		Glfw.pollEvents
		runLoop w sfc phdvc qfis dvc gq pq
			sc g ext scivs rp ppllyt gpl1 cp drsrcs fbs vb vbtri cbs iasrfsifs cf fn cmms scnm cmds vn
			(\ex -> loop ex cfs ((fn + 1) `mod` (360 * frashRate)))
	Vk.Dvc.waitIdle dvc

runLoop :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs, VssList vss,
	HL.HomoList
		'(s, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ]) slyts
	) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swpch.SNew ssc scfmt -> FramebufferResized -> Vk.C.Extent2d ->
	HL.PL (Vk.ImgVw.INew scfmt nm) sis ->
	Vk.RndrPass.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Grph.GNew sg1 '[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(s, '[
				'Vk.DscSetLyt.Buffer '[CameraObj],
				'Vk.DscSetLyt.Buffer '[SceneObj] ])],
			'[WrapMeshPushConstants]) ->
	Vk.CmdPl.C scp ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HL.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[VObj.List 256 Vertex ""] ->
	HL.PL (Vk.CmdBffr.Binded scb) vss ->
	SyncObjects siassrfssfs ->
	Int -> Int ->
	HL.PL MemoryGcd sbsms ->
	Vk.Mem.M sscnm
		'[ '(sscnb, 'Vk.Mem.K.Buffer
			"scene-buffer" '[
				SceneObj,
				SceneObj ])] ->
	HL.PL (Vk.DscSet.S sd sp) slyts ->
	Word32 ->
	(Vk.C.Extent2d -> IO ()) -> IO ()
runLoop win sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt
	gpl1 cp drsrcs fbs vb vbtri cbs iasrfsifs cf fn cmms scnm cmds vn loop = do
	catchAndRecreate win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl1 cp drsrcs fbs loop
		$ drawFrame dvc gq pq sc ext rp gpl1 ppllyt fbs vb vbtri cbs iasrfsifs cf fn cmms scnm cmds vn
	cls <- Glfw.windowShouldClose win
	if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateSwapchainEtc
			win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl1 cp drsrcs fbs)

drawFrame ::
	forall sfs sd ssc scfmt sr sg1 slyt s sm sb nm smtri sbtri nmtri
		scb ssos vss sbsms sscnm sscnb sp slyts . (
	VssList vss,
	HL.HomoList
		'(s, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ]) slyts
	) =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> Vk.Khr.Swpch.SNew ssc scfmt ->
	Vk.C.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Grph.GNew sg1 '[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt,	'[ '(s, '[
				'Vk.DscSetLyt.Buffer '[CameraObj],
				'Vk.DscSetLyt.Buffer '[SceneObj] ])],
			'[WrapMeshPushConstants]) ->
	Vk.Ppl.Lyt.L slyt
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])]
		'[WrapMeshPushConstants] ->
	HL.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[VObj.List 256 Vertex ""] ->
	HL.PL (Vk.CmdBffr.Binded scb) vss -> SyncObjects ssos -> Int -> Int ->
	HL.PL MemoryGcd sbsms ->
	Vk.Mem.M sscnm
		'[ '(sscnb, 'Vk.Mem.K.Buffer
			"scene-buffer" '[
				SceneObj,
				SceneObj ])] ->
	HL.PL (Vk.DscSet.S sd sp) slyts ->
	Word32 -> IO ()
drawFrame dvc gq pq sc ext rp gpl1 lyt fbs vb vbtri cbs (SyncObjects iass rfss iffs) cf fn cmms scnm cmds vn =
	HL.index iass cf \(ias :: Vk.Semaphore.S sias) ->
	HL.index rfss cf \(rfs :: Vk.Semaphore.S srfs) ->
	HL.index iffs cf \(id &&& HL.Singleton -> (iff, siff)) ->
	HL.index cmms cf \(MemoryGcd cmm) ->
	($ HL.homoListIndex cmds cf) \cmd -> do
	Vk.Mem.write @"camera-buffer" @(CameraObj) dvc cmm zeroBits (gpuCameraData ext)
	if cf == 0
		then Vk.Mem.write @"scene-buffer"
			@(SceneObj)
			dvc scnm zeroBits [
				Just $ gpuSceneData fn, Nothing ]
--				SceneData $ gpuSceneData fn ]
--				SceneData gpuSceneDataZero ]
		else Vk.Mem.write @"scene-buffer"
			@(SceneObj)
			dvc scnm zeroBits [
				Nothing,
--				SceneData gpuSceneDataZero,
--				SceneData $ gpuSceneData fn,
				Just $ gpuSceneData fn]
	Vk.Fence.waitForFs dvc siff True maxBound
	imgIdx <- Vk.Khr.acquireNextImageResultNew [Vk.Success, Vk.SuboptimalKhr]
		dvc sc uint64Max (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HL.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext gpl1 lyt vb vbtri fn cmd vn $ fromIntegral cf
	let	submitInfo :: Vk.SubmitInfo () '[sias]
			'[ '(scb, '[AddType Vertex 'Vk.VtxInp.RateVertex])]
			'[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers = HL.Singleton $ U2 cb,
			Vk.submitInfoSignalSemaphores = HL.Singleton rfs }
		presentInfoNew = Vk.Khr.PresentInfoNew {
			Vk.Khr.presentInfoNextNew = Nothing,
			Vk.Khr.presentInfoWaitSemaphoresNew = HL.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndicesNew = HL.Singleton
				$ Vk.Khr.SwapchainImageIndexNew sc imgIdx }
	Vk.Queue.submit gq (HL.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresentNew @() pq presentInfoNew
	where	cb = cbs `vssListIndex` cf

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs ) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q ->
	Vk.Khr.Swpch.SNew ssc scfmt ->
	HL.PL (Vk.ImgVw.INew scfmt nm) sis ->
	Vk.RndrPass.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Grph.GNew sg1
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(s, '[
				'Vk.DscSetLyt.Buffer '[CameraObj],
				'Vk.DscSetLyt.Buffer '[SceneObj] ])],
			'[WrapMeshPushConstants]) ->
	Vk.CmdPl.C scp ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HL.PL Vk.Frmbffr.F sfs ->
	(Vk.C.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl1 cp drsrcs fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateSwapchainEtc
		win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl1 cp drsrcs fbs

recreateSwapchainEtc :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs ) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q ->
	Vk.Khr.Swpch.SNew ssc scfmt ->
	HL.PL (Vk.ImgVw.INew scfmt nm) sis ->
	Vk.RndrPass.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Grph.GNew sg1
		'[AddType Vertex 'Vk.VtxInp.RateVertex]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[CameraObj],
			'Vk.DscSetLyt.Buffer '[SceneObj] ])],
			'[WrapMeshPushConstants]) ->
	Vk.CmdPl.C scp ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HL.PL Vk.Frmbffr.F sfs -> IO Vk.C.Extent2d
recreateSwapchainEtc win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl1 cp (dimg, dim, divw) fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwapchain win sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swpch.getImagesNew dvc sc >>= \imgs ->
			recreateImageViews dvc imgs scivs
		recreateDepthResources phdvc dvc gq cp ext dimg dim divw
		recreateGraphicsPipeline dvc ext rp ppllyt gpl1
		recreateFramebuffers dvc ext rp scivs divw fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize win = Glfw.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

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
	vertexColor :: Color } deriving (Show, Generic)

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
instance Foreign.Storable.Generic.G Vertex

positionNormalToVertex :: Foreign.Storable.Generic.Wrap W.PositionNormal -> Vertex
positionNormalToVertex
	(W.W (W.PositionNormal (W.W (W.Position x y z)) (W.W (W.Normal v w u)))) =
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ x :. y :. z :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ v :. w :. u :. NilL,
		vertexColor = Color . Cglm.Vec3 $ v :. w :. u :. NilL }

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

gpuCameraData :: Vk.C.Extent2d -> GpuCameraData
gpuCameraData sce = GpuCameraData (View view) (Proj $ projection sce)
	(ViewProj $ Cglm.glmMat4Mul (projection sce) view)

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

-- newtype SceneData = SceneData SceneData deriving (Show, Storable)

data SceneData = SceneData {
	gpuSceneDataFogColor :: FogColor,
	gpuSceneDataFogDistances :: FogDistances,
	gpuSceneDataAmbientColor :: AmbientColor,
	gpuSceneDataSunlightDirection :: SunlightDirection,
	gpuSceneDataSunlightColor :: SunlightColor }
	deriving (Show, Generic)

gpuSceneDataZero :: SceneData
gpuSceneDataZero = SceneData {
	gpuSceneDataFogColor = FogColor . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	gpuSceneDataFogDistances =
		FogDistances . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	gpuSceneDataAmbientColor =
		AmbientColor . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	gpuSceneDataSunlightDirection =
		SunlightDirection . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	gpuSceneDataSunlightColor =
		SunlightColor . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL }

gpuSceneData :: Int -> SceneData
gpuSceneData fn = SceneData {
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

instance Storable SceneData where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance Foreign.Storable.Generic.G SceneData
instance SizeAlignmentList SceneData

newtype FogColor = FogColor Cglm.Vec4 deriving (Show, Storable)
newtype FogDistances = FogDistances Cglm.Vec4 deriving (Show, Storable)
newtype AmbientColor = AmbientColor Cglm.Vec4 deriving (Show, Storable)
newtype SunlightDirection =
	SunlightDirection Cglm.Vec4 deriving (Show, Storable)
newtype SunlightColor = SunlightColor Cglm.Vec4 deriving (Show, Storable)

type FramebufferResized = IORef Bool

shaderStages ::
	Spv 'GlslVertexShader -> Spv 'GlslFragmentShader ->
	HL.PL (U6 Vk.Ppl.ShdrSt.CreateInfoNew) '[
		'((), (), 'GlslVertexShader, (), (), '[]),
		'((), (), 'GlslFragmentShader, (), (), '[]) ]
shaderStages vs fs = U6 vertShaderStageInfo :** U6 fragShaderStageInfo :** HL.Nil
	where
	vertShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfoNew {
		Vk.Ppl.ShdrSt.createInfoNextNew = Nothing,
		Vk.Ppl.ShdrSt.createInfoFlagsNew = def,
		Vk.Ppl.ShdrSt.createInfoStageNew = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModuleNew = vertShaderModule1,
		Vk.Ppl.ShdrSt.createInfoNameNew = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfoNew = Nothing }
	fragShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfoNew {
		Vk.Ppl.ShdrSt.createInfoNextNew = Nothing,
		Vk.Ppl.ShdrSt.createInfoFlagsNew = def,
		Vk.Ppl.ShdrSt.createInfoStageNew = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModuleNew = fragShaderModule1,
		Vk.Ppl.ShdrSt.createInfoNameNew = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfoNew = Nothing }
	vertShaderModule1 :: Vk.Shader.Module.M n 'GlslVertexShader () ()
	vertShaderModule1 = mkShaderModule vs
	fragShaderModule1 :: Vk.Shader.Module.M n 'GlslFragmentShader () ()
	fragShaderModule1 = mkShaderModule fs
	mkShaderModule :: Spv sknd -> Vk.Shader.Module.M n sknd () ()
	mkShaderModule cd = Vk.Shader.Module.M crInfo nil nil
		where crInfo = Vk.Shader.Module.M.CreateInfo {
			Vk.Shader.Module.M.createInfoNext = Nothing,
			Vk.Shader.Module.M.createInfoFlags = def,
			Vk.Shader.Module.M.createInfoCode = cd }

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
