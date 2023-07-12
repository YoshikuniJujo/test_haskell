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
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified Gpu.Vulkan.Memory as Vk.Mem

import GHC.Generics
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.Storable.SizeAlignment
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object qualified as Obj
import Data.Foldable
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Bool qualified as TBool
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

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Graphics.UI.GLFW as Glfw hiding (createWindowSurface)
import qualified Glfw as Glfw
import qualified Cglm

import Foreign.Storable.Generic qualified as Str.G

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
import qualified Gpu.Vulkan.Instance.Middle as Vk.Ist.M
import qualified Gpu.Vulkan.Khr as Vk.Khr
import qualified Gpu.Vulkan.Khr.Enum as Vk.Khr
import qualified Gpu.Vulkan.Ext.DebugUtils as Vk.Ext.DbgUtls
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger as Vk.Ext.DbgUtls.Msngr
import qualified Gpu.Vulkan.Ext.DebugUtils.Enum as Vk.Ext.DbgUtls
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.PhysicalDevice.Struct as Vk.Phd
import qualified Gpu.Vulkan.QueueFamily as Vk.QFmly
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Device.Middle as Vk.Dvc.M
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Sfc
import qualified Gpu.Vulkan.Khr.Surface.Middle as Vk.Khr.Sfc.M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as Vk.Khr.Sfc.Phd
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swpch
import qualified Gpu.Vulkan.Image as Vk.Img
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Image.Enum as Vk.Img
import qualified Gpu.Vulkan.Image as Vk.Img.M
import qualified Gpu.Vulkan.ImageView as Vk.ImgVw
import qualified Gpu.Vulkan.ImageView.Enum as Vk.ImgVw
import qualified Gpu.Vulkan.Component as Vk.Component
import qualified Gpu.Vulkan.ShaderModule as Vk.Shader.Module
import qualified Gpu.Vulkan.ShaderModule.Middle as Vk.Shader.Module.M
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShdrSt
import qualified Gpu.Vulkan.Pipeline.VertexInputState as Vk.Ppl.VtxIptSt
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.InpAsmbSt
import qualified Gpu.Vulkan.Pipeline.ViewportState as Vk.Ppl.ViewportSt
import qualified Gpu.Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Gpu.Vulkan.Pipeline.MultisampleState as Vk.Ppl.MltSmplSt
import qualified Gpu.Vulkan.Sample as Vk.Sample
import qualified Gpu.Vulkan.Sample.Enum as Vk.Sample
import qualified Gpu.Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.ClrBlndAtt
import qualified Gpu.Vulkan.ColorComponent.Enum as Vk.ClrCmp
import qualified Gpu.Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlndSt
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Attachment as Vk.Att
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Attachment.Enum as Vk.Att
import qualified Gpu.Vulkan.Subpass as Vk.Subpass
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Subpass.Enum as Vk.Subpass
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPss
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPss.M
import qualified Gpu.Vulkan.Pipeline.Graphics.Type as Vk.Ppl.Grph
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Grph
import qualified Gpu.Vulkan.Framebuffer as Vk.Frmbffr
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified "try-my-vulkan-snd" Gpu.Vulkan.CommandPool.Enum as Vk.CmdPl
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr
import qualified "try-my-vulkan-snd" Gpu.Vulkan.CommandBuffer.Enum as Vk.CBffr
import qualified Gpu.Vulkan.CommandBuffer.Middle as Vk.CBffr.M
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fnc
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Fence.Enum as Vk.Fnc
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Buffer.Enum as Vk.Bffr
import qualified Gpu.Vulkan.Memory.Middle as Vk.Mm.M
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mm
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.Queue.Enum as Vk.Q
import qualified Gpu.Vulkan.Cmd as Vk.Cmd
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as Vk.Ppl.DptStnSt
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified "try-my-vulkan-snd" Gpu.Vulkan.Descriptor.Enum as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPl
import qualified "try-my-vulkan-snd" Gpu.Vulkan.DescriptorPool.Enum as Vk.DscPl
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.DescriptorSetLayout.UpdateDynamicLengths as Vk.DscSet.T
import qualified Gpu.Vulkan.DescriptorSet.BindingAndArrayElem.Buffer as Vk.DscSet.T

import qualified Codec.Wavefront.ReadOld as Wv
import Tools

maxFramesInFlight :: Integral n => n
maxFramesInFlight = 2

type MaxFramesInFlight = 2

frashRate :: Num n => n
frashRate = 2

main :: IO ()
main = do
	[objfile] <- getArgs
	vns <- vertices <$> BS.readFile objfile
	withWindow \w frszd -> createInstance \ist -> if enableValidationLayers
		then Vk.Ext.DbgUtls.Msngr.create ist debugMessengerInfo nil'
			$ const $ run w ist frszd vns
		else run w ist frszd vns
	where
	vertices s = V.map posNormalToVertex
		. uncurry3 Wv.facePosNormal $ Wv.readV' cv cn cf s
		where Wv.Count {
			Wv.countVertex = cv,
			Wv.countNormal = cn,
			Wv.countFace = cf } = Wv.countV' s

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
		. (Vk.layerPropertiesLayerName <$>)
		<$> Vk.Ist.M.enumerateLayerProperties
	exts <- bool id (Vk.Ext.DbgUtls.extensionName :) enableValidationLayers
		<$> ((cstrToText `mapM`) =<< Glfw.getRequiredInstanceExtensions)
	instInfo enableValidationLayers exts \ci ->
		Vk.Ist.create ci nil' f
	where
	msg = "validation layers requested, but not available!"

instInfo :: Bool -> [Txt.Text] -> (
	forall mn . WithPoked (TMaybe.M mn) => Vk.Ist.M.CreateInfo mn 'Nothing -> b
	) -> b
instInfo b exts f = istCreateInfoNext b \mn ->
	f Vk.Ist.M.CreateInfo {
		Vk.Ist.M.createInfoNext = mn,
		Vk.Ist.M.createInfoFlags = zeroBits,
		Vk.Ist.M.createInfoApplicationInfo = Just appInfo,
		Vk.Ist.M.createInfoEnabledLayerNames = bool
			[] [Vk.Khr.validationLayerName] enableValidationLayers,
		Vk.Ist.M.createInfoEnabledExtensionNames = exts }
	where
	appInfo = Vk.ApplicationInfo {
		Vk.applicationInfoNext = TMaybe.N,
		Vk.applicationInfoApplicationName =
			"Vulkan Guide with Dynamic Descriptor Sets",
		Vk.applicationInfoApplicationVersion =
			Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoEngineName = "No Engine",
		Vk.applicationInfoEngineVersion = Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoApiVersion = Vk.apiVersion_1_1 }

istCreateInfoNext :: Bool ->
	(forall mn . WithPoked (TMaybe.M mn) => TMaybe.M mn -> b) -> b
istCreateInfoNext = TBool.b @WithPoked TMaybe.N (TMaybe.J debugMessengerInfo)


debugMessengerInfo :: Vk.Ext.DbgUtls.Msngr.CreateInfo 'Nothing '[] ()
debugMessengerInfo = Vk.Ext.DbgUtls.Msngr.CreateInfo {
	Vk.Ext.DbgUtls.Msngr.createInfoNext = TMaybe.N,
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
run w ist rszd (id &&& fromIntegral . V.length -> (vns, vnsln)) =
	Glfw.createWindowSurface ist w nil' \sfc ->
	pickPhysicalDevice ist sfc >>= \(pd, qfs) ->
	putStrLn "MIN ALIGN" >>
	(print . Vk.Phd.limitsMinUniformBufferOffsetAlignment
		. Vk.Phd.propertiesLimits =<< Vk.Phd.getProperties pd) >>
	createDevice pd qfs \dv gq pq ->
	createSwapchain w sfc pd qfs dv \(sc :: Vk.Khr.Swpch.S fmt ss) ex ->
	Vk.Khr.Swpch.getImages dv sc >>= \imgs ->
	createImageViews dv imgs \scivs ->
	findDepthFormat pd >>= \dfmt ->
	Vk.T.formatToType dfmt \(_ :: Proxy dfmt) ->
	createRenderPass @fmt @dfmt dv \rp ->

	createDescriptorSetLayout dv \dslyt ->
	createPipelineLayout dv dslyt \lyt ->
	createGraphicsPipeline dv ex rp lyt \gpl ->

	createCommandPool dv qfs \cp ->
	createDepthResources @dfmt pd dv gq cp ex \drs@(_, _, dimgv) ->
	createFramebuffers dv ex rp scivs dimgv \fbs ->

	createCameraBuffers pd dv dslyt maxFramesInFlight \lyts cmbs cmms ->
	createSceneBuffer pd dv \scnb scnm ->
	createDescriptorPool dv \dp ->
	createDescriptorSets dv dp cmbs lyts scnb \dss ->
	createVertexBuffer pd dv gq cp vns \vb ->
	createVertexBuffer pd dv gq cp triangle \vbtri ->
	createCommandBuffers dv cp \cbs ->
	createSyncObjects dv \sos ->

	mainLoop w rszd sfc pd qfs dv gq pq sc ex scivs rp lyt gpl cp drs fbs
		cmms scnm dss vb vbtri cbs sos vnsln

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
	. (deviceExtensions \\) . (Vk.extensionPropertiesExtensionName <$>)
		<$> Vk.Phd.enumerateExtensionProperties dv Nothing

deviceExtensions :: [Txt.Text]
deviceExtensions = [Vk.Khr.Swpch.extensionName]

findQueueFamilies :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO QueueFamilyIndicesMaybe
findQueueFamilies dv sfc = Vk.Phd.getQueueFamilyProperties dv >>= \qfs ->
	filterM	(\i -> Vk.Khr.Sfc.Phd.getSupport dv i sfc)
		(fst <$> qfs) >>= \(listToMaybe -> pfi) ->
	pure QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = (`findBySnd` qfs)
			$ checkBits Vk.Q.GraphicsBit
				. Vk.QFmly.propertiesQueueFlags,
		presentFamilyMaybe = pfi }

data QueueFamilyIndices = QueueFamilyIndices {
	graphicsFamily :: Vk.QFmly.Index, presentFamily :: Vk.QFmly.Index }

data QueueFamilyIndicesMaybe = QueueFamilyIndicesMaybe {
	graphicsFamilyMaybe :: Maybe Vk.QFmly.Index,
	presentFamilyMaybe :: Maybe Vk.QFmly.Index }

completeQueueFamilies :: QueueFamilyIndicesMaybe -> Maybe QueueFamilyIndices
completeQueueFamilies = \case
	QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = Just g, presentFamilyMaybe = Just p } ->
		Just QueueFamilyIndices {
			graphicsFamily = g, presentFamily = p }
	_ -> Nothing

createDevice :: Vk.Phd.P -> QueueFamilyIndices ->
	(forall sd . Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> IO a) -> IO a
createDevice ph qfis f = mkHeteroParList qcrInfo qfs \qcris ->
	Vk.Dvc.create ph (crInfo qcris) nil' \dv -> do
		gq <- Vk.Dvc.getQueue dv (graphicsFamily qfis) 0
		pq <- Vk.Dvc.getQueue dv (presentFamily qfis) 0
		f dv gq pq
	where
	qfs = nub [graphicsFamily qfis, presentFamily qfis]
	qcrInfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }
	crInfo qcris = Vk.Dvc.M.CreateInfo {
		Vk.Dvc.M.createInfoNext = TMaybe.N,
		Vk.Dvc.M.createInfoFlags = zeroBits,
		Vk.Dvc.M.createInfoQueueCreateInfos = qcris,
		Vk.Dvc.M.createInfoEnabledLayerNames = bool
			[] [Vk.Khr.validationLayerName] enableValidationLayers,
		Vk.Dvc.M.createInfoEnabledExtensionNames = deviceExtensions,
		Vk.Dvc.M.createInfoEnabledFeatures = Just def }

mkHeteroParList :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] ->
	(forall ss . HL.ToListWithCM' WithPoked TMaybe.M ss => HL.PL t ss -> b) ->
	b
mkHeteroParList _k [] f = f HL.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

enableValidationLayers :: Bool
enableValidationLayers = maybe True (const False) $(lookupCompileEnv "NDEBUG")

createSwapchain :: Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> (forall ss scfmt .
		Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swpch.S scfmt ss -> Vk.Extent2d -> IO a) -> IO a
createSwapchain w sfc ph qfs dv f = getSwapchainSupport ph sfc >>= \spp -> do
	ex <- chooseSwapExtent w $ capabilities spp
	let	fmt = Vk.Khr.Sfc.M.formatFormat
			. chooseSwapSurfaceFormat $ formats spp
	Vk.T.formatToType fmt \(_ :: Proxy fmt) ->
		Vk.Khr.Swpch.create @'Nothing @fmt dv
			(swapchainCreateInfo sfc qfs spp ex) nil' (`f` ex)

recreateSwapchain :: Vk.T.FormatToValue scfmt =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Khr.Swpch.S scfmt ssc ->
	IO Vk.Extent2d
recreateSwapchain w sfc ph qfs dv sc = getSwapchainSupport ph sfc >>= \spp -> do
	ex <- chooseSwapExtent w $ capabilities spp
	ex <$ Vk.Khr.Swpch.recreate @'Nothing dv
		(swapchainCreateInfo sfc qfs spp ex) nil' sc

getSwapchainSupport :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO SwapchainSupportDetails
getSwapchainSupport dv sfc = SwapchainSupportDetails
	<$> Vk.Khr.Sfc.Phd.getCapabilities dv sfc
	<*> Vk.Khr.Sfc.Phd.getFormats dv sfc
	<*> Vk.Khr.Sfc.Phd.getPresentModes dv sfc

chooseSwapExtent :: Glfw.Window -> Vk.Khr.Sfc.M.Capabilities -> IO Vk.Extent2d
chooseSwapExtent win caps
	| Vk.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> w, fromIntegral -> h) <-
			Glfw.getFramebufferSize win
		pure $ Vk.Extent2d
			(clamp w (Vk.extent2dWidth n) (Vk.extent2dWidth x))
			(clamp h (Vk.extent2dHeight n)
				(Vk.extent2dHeight x))
	where
	curExt = Vk.Khr.Sfc.M.capabilitiesCurrentExtent caps
	n = Vk.Khr.Sfc.M.capabilitiesMinImageExtent caps
	x = Vk.Khr.Sfc.M.capabilitiesMaxImageExtent caps

data SwapchainSupportDetails = SwapchainSupportDetails {
	capabilities :: Vk.Khr.Sfc.M.Capabilities,
	formats :: [Vk.Khr.Sfc.M.Format],
	presentModes :: [Vk.Khr.PresentMode] }

swapchainCreateInfo :: Vk.Khr.Sfc.S ss -> QueueFamilyIndices ->
	SwapchainSupportDetails -> Vk.Extent2d ->
	Vk.Khr.Swpch.CreateInfo 'Nothing ss fmt
swapchainCreateInfo sfc qfs spp ext = Vk.Khr.Swpch.CreateInfo {
	Vk.Khr.Swpch.createInfoNext = TMaybe.N,
	Vk.Khr.Swpch.createInfoFlags = zeroBits,
	Vk.Khr.Swpch.createInfoSurface = sfc,
	Vk.Khr.Swpch.createInfoMinImageCount = imgc,
	Vk.Khr.Swpch.createInfoImageColorSpace =
		Vk.Khr.Sfc.M.formatColorSpace fmt,
	Vk.Khr.Swpch.createInfoImageExtent = ext,
	Vk.Khr.Swpch.createInfoImageArrayLayers = 1,
	Vk.Khr.Swpch.createInfoImageUsage = Vk.Img.UsageColorAttachmentBit,
	Vk.Khr.Swpch.createInfoImageSharingMode = ism,
	Vk.Khr.Swpch.createInfoQueueFamilyIndices = qfis,
	Vk.Khr.Swpch.createInfoPreTransform =
		Vk.Khr.Sfc.M.capabilitiesCurrentTransform caps,
	Vk.Khr.Swpch.createInfoCompositeAlpha =
		Vk.Khr.CompositeAlphaOpaqueBit,
	Vk.Khr.Swpch.createInfoPresentMode = Vk.Khr.PresentModeFifo,
	Vk.Khr.Swpch.createInfoClipped = True,
	Vk.Khr.Swpch.createInfoOldSwapchain = Nothing }
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
	Vk.Dvc.D sd -> [Vk.Img.Binded ss ss nm fmt] ->
	(forall s . HL.PL (Vk.ImgVw.I nm fmt) s -> IO a) -> IO a
createImageViews _ [] f = f HL.Nil
createImageViews dv (sci : scis) f =
	createImageView dv sci Vk.Img.AspectColorBit \sciv ->
	createImageViews dv scis \scivs -> f $ sciv :** scivs

createImageView :: forall ivfmt sd si sm nm ifmt a . Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	Vk.Img.AspectFlags ->
	(forall siv . Vk.ImgVw.I nm ivfmt siv -> IO a) -> IO a
createImageView dv img asps =
	Vk.ImgVw.create dv (imageViewCreateInfo img asps) nil'

recreateImageViews :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Img.Binded ss ss nm fmt] ->
	HL.PL (Vk.ImgVw.I nm fmt) sis -> IO ()
recreateImageViews _dv [] HL.Nil = pure ()
recreateImageViews dv (sci : scis) (iv :** ivs) =
	recreateImageView dv sci Vk.Img.AspectColorBit iv >>
	recreateImageViews dv scis ivs
recreateImageViews _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

recreateImageView :: Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	Vk.Img.AspectFlags -> Vk.ImgVw.I nm ivfmt s -> IO ()
recreateImageView dv img asps iv =
	Vk.ImgVw.recreate dv (imageViewCreateInfo img asps) nil' iv

imageViewCreateInfo ::
	Vk.Img.Binded sm si nm ifmt -> Vk.Img.AspectFlags ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt ivfmt
imageViewCreateInfo img asps = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = img,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = components,
	Vk.ImgVw.createInfoSubresourceRange = subresourceRange }
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
	forall (scifmt :: Vk.T.Format) (dfmt :: Vk.T.Format) sd a . (
	Vk.T.FormatToValue scifmt, Vk.T.FormatToValue dfmt ) =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPss.R sr -> IO a) -> IO a
createRenderPass dv f = Vk.RndrPss.createNew @'[scifmt, dfmt] @'Nothing
	dv renderPassInfo nil' f where
	renderPassInfo = Vk.RndrPss.M.CreateInfoNew {
		Vk.RndrPss.M.createInfoNextNew = TMaybe.N,
		Vk.RndrPss.M.createInfoFlagsNew = zeroBits,
		Vk.RndrPss.M.createInfoAttachmentsNew =
			colorAttachment :** depthAttachment :** HL.Nil,
		Vk.RndrPss.M.createInfoSubpassesNew = [subpass],
		Vk.RndrPss.M.createInfoDependenciesNew = [dependency] }
	colorAttachment :: Vk.Att.Description scifmt
	colorAttachment = Vk.Att.Description {
		Vk.Att.descriptionFlags = zeroBits,
		Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
		Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
		Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
		Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
		Vk.Att.descriptionStencilStoreOp = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Att.descriptionFinalLayout = Vk.Img.LayoutPresentSrcKhr }
	depthAttachment :: Vk.Att.Description dfmt
	depthAttachment = Vk.Att.Description {
		Vk.Att.descriptionFlags = zeroBits,
		Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
		Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
		Vk.Att.descriptionStoreOp = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
		Vk.Att.descriptionStencilStoreOp = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Att.descriptionFinalLayout =
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

createDescriptorSetLayout :: Vk.Dvc.D sd ->
	(forall (s :: Type) . Vk.DscSetLyt.L s Buffers -> IO a) -> IO a
createDescriptorSetLayout dv = Vk.DscSetLyt.create dv layoutInfo nil' where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing Buffers
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = camera :** scene :** HL.Nil }
	camera :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer '[CameraObj])
	camera = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	scene :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer '[SceneObj])
	scene = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBufferDynamic,
		Vk.DscSetLyt.bindingBufferStageFlags =
			Vk.ShaderStageVertexBit .|. Vk.ShaderStageFragmentBit }

type Buffers = '[
	'Vk.DscSetLyt.Buffer '[CameraObj], 'Vk.DscSetLyt.Buffer '[SceneObj] ]

type CameraObj = Obj.Atom 256 CameraData 'Nothing
type SceneObj = Obj.DynAtom 2 256 SceneData 'Nothing

createPipelineLayout :: forall sd sdl a . Vk.Dvc.D sd ->
	Vk.DscSetLyt.L sdl Buffers -> (forall sl .
		Vk.Ppl.Lyt.L sl '[ '(sdl, Buffers)] '[WMeshPushConstants] ->
		IO a) -> IO a
createPipelineLayout dv dslyt f = Vk.Ppl.Lyt.createNew dv ci nil' f where
	ci :: Vk.Ppl.Lyt.CreateInfoNew 'Nothing '[ '(sdl, Buffers)] (
		'Vk.PushConstant.PushConstantLayout
			'[ WMeshPushConstants]
			'[ 'Vk.PushConstant.Range '[ 'Vk.T.ShaderStageVertexBit]
				'[WMeshPushConstants]] )
	ci = Vk.Ppl.Lyt.CreateInfoNew {
		Vk.Ppl.Lyt.createInfoNextNew = TMaybe.N,
		Vk.Ppl.Lyt.createInfoFlagsNew = zeroBits,
		Vk.Ppl.Lyt.createInfoSetLayoutsNew = HL.Singleton $ U2 dslyt }

createGraphicsPipeline :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl '[ '(sdl, Buffers)] '[WMeshPushConstants] ->
	(forall sg . Vk.Ppl.Grph.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(sdl, Buffers)], '[WMeshPushConstants]) -> IO a) ->
	IO a
createGraphicsPipeline dv sce rp lyt f = Vk.Ppl.Grph.createGs dv Nothing
	(HL.Singleton . U14 $ graphicsPipelineCreateInfo sce rp lyt) nil'
	\(HL.Singleton (U3 gpl)) -> f gpl

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl '[ '(sdl, Buffers)] '[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(sdl, Buffers)], '[WMeshPushConstants]) -> IO ()
recreateGraphicsPipeline dv sce rp lyt gpls = Vk.Ppl.Grph.recreateGs dv Nothing
	(U14 (graphicsPipelineCreateInfo sce rp lyt) :** HL.Nil) nil'
	(U3 gpls :** HL.Nil)

graphicsPipelineCreateInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl '[ '(sdl, Buffers)] '[WMeshPushConstants] ->
	Vk.Ppl.Grph.CreateInfo 'Nothing
		'[	'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[])]
		'( 'Nothing, '[ '(Vertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Position), '(1, Normal), '(2, Color)])
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'(sl, '[ '(sdl, Buffers)], '[WMeshPushConstants]) sr
		'(sb, vs', ts', larg)
graphicsPipelineCreateInfo sce rp lyt = Vk.Ppl.Grph.CreateInfo {
	Vk.Ppl.Grph.createInfoNext = TMaybe.N,
	Vk.Ppl.Grph.createInfoFlags = zeroBits,
	Vk.Ppl.Grph.createInfoStages =
		shaderStages glslVertexShaderMain glslFragmentShaderMain,
	Vk.Ppl.Grph.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Grph.createInfoInputAssemblyState = Just inputAssembly,
	Vk.Ppl.Grph.createInfoViewportState = Just $ viewportState sce,
	Vk.Ppl.Grph.createInfoRasterizationState = Just rasterizer,
	Vk.Ppl.Grph.createInfoMultisampleState = Just multisampling,
	Vk.Ppl.Grph.createInfoDepthStencilState = Just depthStencil,
	Vk.Ppl.Grph.createInfoColorBlendState = Just colorBlending,
	Vk.Ppl.Grph.createInfoDynamicState = Nothing,
	Vk.Ppl.Grph.createInfoLayout = U3 lyt,
	Vk.Ppl.Grph.createInfoRenderPass = rp,
	Vk.Ppl.Grph.createInfoSubpass = 0,
	Vk.Ppl.Grph.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Grph.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Grph.createInfoTessellationState = Nothing }

inputAssembly :: Vk.Ppl.InpAsmbSt.CreateInfo 'Nothing
inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
	Vk.Ppl.InpAsmbSt.createInfoNext = TMaybe.N,
	Vk.Ppl.InpAsmbSt.createInfoFlags = zeroBits,
	Vk.Ppl.InpAsmbSt.createInfoTopology = Vk.PrimitiveTopologyTriangleList,
	Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable = False }

viewportState :: Vk.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo 'Nothing
viewportState sce = def {
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
	Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeNone,
	Vk.Ppl.RstSt.createInfoFrontFace = Vk.FrontFaceClockwise,
	Vk.Ppl.RstSt.createInfoDepthBiasEnable = False,
	Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0 }

depthStencil :: Vk.Ppl.DptStnSt.CreateInfo 'Nothing
depthStencil = Vk.Ppl.DptStnSt.CreateInfo {
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

createCommandPool :: Vk.Dvc.D sd ->
	QueueFamilyIndices -> (forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCommandPool dv qfs = Vk.CmdPl.create dv crInfo nil'
	where crInfo = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = graphicsFamily qfs }

type DepthResources sb sm nm fmt sdiv = (
	Vk.Img.Binded sm sb nm fmt,
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)],
	Vk.ImgVw.I nm fmt sdiv )

createDepthResources :: forall fmt sd sc nm a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Extent2d ->
	(forall si sm siv . DepthResources si sm nm fmt siv -> IO a) -> IO a
createDepthResources phd dv gq cp ex f =
	createImage @_ @fmt phd dv ex
		Vk.Img.TilingOptimal
		Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mm.PropertyDeviceLocalBit \dimg dimgm ->
	createImageView @fmt dv dimg Vk.Img.AspectDepthBit \dimgv ->
	transitionImageLayout dv gq cp dimg Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal >>
	f (dimg, dimgm, dimgv)

recreateDepthResources :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Extent2d -> DepthResources sb sm nm fmt sdiv -> IO ()
recreateDepthResources phd dv gq cp ex (dimg, dimgm, dimgv) = do
	recreateImage phd dv ex
		Vk.Img.TilingOptimal
		Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mm.PropertyDeviceLocalBit dimg dimgm
	recreateImageView dv dimg Vk.Img.AspectDepthBit dimgv
	transitionImageLayout dv gq cp dimg
		Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal

createImage :: forall nm fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Extent2d -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mm.PropertyFlagBits -> (forall si sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mm.M sm '[ '(si, 'Vk.Mm.ImageArg nm fmt) ] -> IO a) -> IO a
createImage pd dv ex tlng usg prs f =
	Vk.Img.create @'Nothing dv (imageInfo ex tlng usg) nil' \i ->
	imageMemoryInfo pd dv prs i >>= \ii -> imageAllocateBind dv i ii f

recreateImage :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Extent2d -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mm.PropertyFlags ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)] -> IO ()
recreateImage pd dv ex tlng usg prs i m = do
	Vk.Img.recreate @'Nothing dv (imageInfo ex tlng usg) nil' i
	imageMemoryInfoB pd dv prs i >>= \ii -> imageReallocateBind dv i ii m

imageInfo :: Vk.Extent2d ->
	Vk.Img.Tiling -> Vk.Img.UsageFlags -> Vk.Img.CreateInfo 'Nothing fmt
imageInfo ex tlng usg = Vk.Img.CreateInfo {
		Vk.Img.createInfoNext = TMaybe.N,
		Vk.Img.createInfoImageType = Vk.Img.Type2d,
		Vk.Img.createInfoExtent = Vk.Extent3d {
			Vk.extent3dWidth = Vk.extent2dWidth ex,
			Vk.extent3dHeight = Vk.extent2dHeight ex,
			Vk.extent3dDepth = 1 },
		Vk.Img.createInfoMipLevels = 1,
		Vk.Img.createInfoArrayLayers = 1,
		Vk.Img.createInfoTiling = tlng,
		Vk.Img.createInfoInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Img.createInfoUsage = usg,
		Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Img.createInfoSamples = Vk.Sample.Count1Bit,
		Vk.Img.createInfoFlags = zeroBits,
		Vk.Img.createInfoQueueFamilyIndices = [] }

imageMemoryInfo :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Mm.PropertyFlags ->
	Vk.Img.I s nm fmt -> IO (Vk.Mem.AllocateInfo 'Nothing)
imageMemoryInfo pd dv prs i = do
	rqs <- Vk.Img.getMemoryRequirements dv i
	mt <- findMemoryType pd (Vk.Mm.M.requirementsMemoryTypeBits rqs) prs
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

imageMemoryInfoB :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Mm.PropertyFlags ->
	Vk.Img.Binded sm si nm fmt -> IO (Vk.Mem.AllocateInfo 'Nothing)
imageMemoryInfoB pd dv prs i = do
	rqs <- Vk.Img.getMemoryRequirementsBinded dv i
	mt <- findMemoryType pd (Vk.Mm.M.requirementsMemoryTypeBits rqs) prs
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

findMemoryType :: Vk.Phd.P ->
	Vk.Mm.M.TypeBits -> Vk.Mm.PropertyFlags -> IO Vk.Mm.M.TypeIndex
findMemoryType pd ts prs0 =
	maybe (error msg) pure . suitable =<< Vk.Phd.getMemoryProperties pd
	where
	msg = "failed to find suitable memory type!"
	suitable prs1 = fst <$> find ((&&)
		<$> (`Vk.Mm.M.elemTypeIndex` ts) . fst
		<*> checkBits prs0 . Vk.Mm.M.mTypePropertyFlags . snd)
		(Vk.Phd.memoryPropertiesMemoryTypes prs1)

imageAllocateBind :: Vk.Dvc.D sd -> Vk.Img.I si nm fmt ->
	Vk.Mem.AllocateInfo 'Nothing -> (forall sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mm.M sm '[ '(si, 'Vk.Mm.ImageArg nm fmt) ] -> IO a) -> IO a
imageAllocateBind dv i mi f = Vk.Mm.allocateBind @'Nothing dv
	(HL.Singleton . U2 $ Vk.Mm.Image i) mi nil'
	\(HL.Singleton (U2 (Vk.Mm.ImageBinded b))) m -> f b m

imageReallocateBind :: Vk.Dvc.D sd -> Vk.Img.Binded sm sb nm fmt ->
	Vk.Mem.AllocateInfo 'Nothing ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)] -> IO ()
imageReallocateBind dv i mi m = Vk.Mm.reallocateBind @'Nothing dv
	(HL.Singleton . U2 $ Vk.Mm.ImageBinded i) mi nil' m

transitionImageLayout :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Img.Binded sm si nm fmt -> Vk.Img.Layout -> Vk.Img.Layout -> IO ()
transitionImageLayout dv gq cp i ol nl = beginSingleTimeCommands dv gq cp \cb ->
	Vk.Cmd.pipelineBarrier cb
		sstg dstg zeroBits HL.Nil HL.Nil (HL.Singleton $ U5 barrier)
	where
	barrier :: Vk.Img.MemoryBarrier 'Nothing sm si nm fmt
	barrier = Vk.Img.MemoryBarrier {
		Vk.Img.memoryBarrierNext = TMaybe.N,
		Vk.Img.memoryBarrierOldLayout = ol,
		Vk.Img.memoryBarrierNewLayout = nl,
		Vk.Img.memoryBarrierSrcQueueFamilyIndex = Vk.QFmly.Ignored,
		Vk.Img.memoryBarrierDstQueueFamilyIndex = Vk.QFmly.Ignored,
		Vk.Img.memoryBarrierImage = i,
		Vk.Img.memoryBarrierSubresourceRange = srr,
		Vk.Img.memoryBarrierSrcAccessMask = sam,
		Vk.Img.memoryBarrierDstAccessMask = dam }
	srr = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = asps,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 }
	asps = case nl of
		Vk.Img.LayoutDepthStencilAttachmentOptimal ->
			Vk.Img.AspectDepthBit .|.
			case Vk.T.formatToValue @fmt of
				Vk.FormatD32SfloatS8Uint ->
					Vk.Img.AspectStencilBit
				Vk.FormatD24UnormS8Uint ->
					Vk.Img.AspectStencilBit
				_ -> zeroBits
		_ -> Vk.Img.AspectColorBit
	(sam, dam, sstg, dstg) = case (ol, nl) of
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

beginSingleTimeCommands :: forall sd sc a .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	(forall s . Vk.CBffr.C s -> IO a) -> IO a
beginSingleTimeCommands dv gq cp cmds =
	Vk.CBffr.allocate dv allocInfo \(cb :*. HL.Nil) ->
	Vk.CBffr.begin @'Nothing @'Nothing cb beginInfo (cmds cb) <* do
	Vk.Q.submit gq (HL.Singleton . U4 $ submitInfo cb) Nothing
	Vk.Q.waitIdle gq
	where
	allocInfo :: Vk.CBffr.AllocateInfo 'Nothing sc  '[ '()]
	allocInfo = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cp,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }
	beginInfo = Vk.CBffr.M.BeginInfo {
		Vk.CBffr.beginInfoNext = TMaybe.N,
		Vk.CBffr.beginInfoFlags = Vk.CBffr.UsageOneTimeSubmitBit,
		Vk.CBffr.beginInfoInheritanceInfo = Nothing }
	submitInfo :: forall s . Vk.CBffr.C s -> Vk.SubmitInfo 'Nothing '[] '[s] '[]
	submitInfo cb = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Nil,
		Vk.submitInfoCommandBuffers = HL.Singleton cb,
		Vk.submitInfoSignalSemaphores = HL.Nil }

createFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
	Vk.RndrPss.R sr -> HL.PL (Vk.ImgVw.I nm fmt) sis ->
	Vk.ImgVw.I dfmt dnm siv ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HL.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HL.Nil _ f = f HL.Nil
createFramebuffers dv sce rp (iv :** ivs) dptiv f =
	Vk.Frmbffr.create dv (framebufferInfo sce rp iv dptiv) nil' \fb ->
	createFramebuffers dv sce rp ivs dptiv \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPss.R sr -> HL.PL (Vk.ImgVw.I nm scfmt) sis ->
		Vk.ImgVw.I dfmt dnm sdiv -> HL.PL Vk.Frmbffr.F sfs ->
		IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _ _ _ HL.Nil _ HL.Nil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dv sce rp (sciv :** scivs) dptiv (fb :** fbs) =
		Vk.Frmbffr.recreate dv
			(framebufferInfo sce rp sciv dptiv) nil' fb >>
		recreateFramebuffers dv sce rp scivs dptiv fbs

framebufferInfo ::
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I nm fmt si ->
	Vk.ImgVw.I dfmt dnm sdiv ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(nm, fmt, si), '(dfmt, dnm, sdiv)]
framebufferInfo Vk.Extent2d {
	Vk.extent2dWidth = w, Vk.extent2dHeight = h } rp attch dpt =
	Vk.Frmbffr.CreateInfo {
		Vk.Frmbffr.createInfoNext = TMaybe.N,
		Vk.Frmbffr.createInfoFlags = zeroBits,
		Vk.Frmbffr.createInfoRenderPass = rp,
		Vk.Frmbffr.createInfoAttachments =
			U3 attch :** U3 dpt :** HL.Nil,
		Vk.Frmbffr.createInfoWidth = w,
		Vk.Frmbffr.createInfoHeight = h,
		Vk.Frmbffr.createInfoLayers = 1 }

createCameraBuffers :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sdsc Buffers ->
	Int -> (forall slyts sbsms . (
		Vk.DscSet.DListFromMiddle slyts, HL.FromList slyts,
		Update sbsms slyts, HL.HomoList '(sdsc, Buffers) slyts ) =>
		HL.PL (U2 Vk.DscSetLyt.L) slyts ->
		HL.PL BindedCamera sbsms -> HL.PL MemoryCamera sbsms ->
		IO a) -> IO a
createCameraBuffers _ _ _ n f | n < 1 = f HL.Nil HL.Nil HL.Nil
createCameraBuffers pd dv lyt n f = createCameraBuffer pd dv \b m ->
	createCameraBuffers pd dv lyt (n - 1) \lyts bs ms ->
	f (U2 lyt :** lyts) (BindedCamera b :** bs) (MemoryCamera m :** ms)

data BindedCamera smsb where
	BindedCamera :: Vk.Bffr.Binded sm sb "camera-buffer" '[CameraObj] ->
		BindedCamera '(sm, sb)

data MemoryCamera smsb where
	MemoryCamera ::
		Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.BufferArg "camera-buffer" '[CameraObj]) ] ->
		MemoryCamera '(sm, sb)

createCameraBuffer :: Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb . Vk.Bffr.Binded sm sb nm '[CameraObj] ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[CameraObj]) ] ->
		IO a) -> IO a
createCameraBuffer pd dv = createBuffer pd dv
	(HL.Singleton Obj.ObjectLengthAtom)
	Vk.Bffr.UsageUniformBufferBit Vk.Mm.PropertyHostVisibleBit

createBuffer :: forall objs nm sd a . (
	Obj.SizeAlignmentList objs, forall s . SizeAlignmentAll s nm objs ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> HL.PL Obj.ObjectLength objs ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm objs ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm objs)] -> IO a) ->
	IO a
createBuffer p dv lns usg prs f =
	Vk.Bffr.create dv (bufferInfo lns usg) nil' \b ->
	Vk.Bffr.getMemoryRequirements dv b >>= \rs ->
	findMemoryType p (Vk.Mm.M.requirementsMemoryTypeBits rs) prs >>= \mt ->
	Vk.Mm.allocateBind dv
		(HL.Singleton . U2 $ Vk.Mm.Buffer b) (memoryInfo mt) nil'
		$ f . \(HL.Singleton (U2 (Vk.Mm.BufferBinded bnd))) -> bnd

class Vk.Mm.Alignments '[ '(s, 'Vk.Mm.BufferArg nm objs)] =>
	SizeAlignmentAll s nm (objs :: [Obj.Object])

instance Vk.Mm.Alignments '[ '(s, 'Vk.Mm.BufferArg nm '[obj])] =>
	SizeAlignmentAll s nm '[obj]

instance {-# OVERLAPPABLE #-} (
	Obj.SizeAlignment obj, SizeAlignmentAll s nm objs ) =>
	SizeAlignmentAll s nm (obj ': objs)

bufferInfo :: HL.PL Obj.ObjectLength objs ->
	Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo 'Nothing objs
bufferInfo lns usg = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = lns,
	Vk.Bffr.createInfoUsage = usg,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

memoryInfo :: Vk.Mm.M.TypeIndex -> Vk.Mem.AllocateInfo 'Nothing
memoryInfo mt = Vk.Mem.AllocateInfo {
	Vk.Mem.allocateInfoNext = TMaybe.N,
	Vk.Mem.allocateInfoMemoryTypeIndex = mt }

createSceneBuffer :: Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[SceneObj] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[SceneObj])] ->
	IO a) -> IO a
createSceneBuffer pd dv = createBuffer pd dv
	(HL.Singleton Obj.ObjectLengthDynAtom)
	Vk.Bffr.UsageUniformBufferBit Vk.Mm.PropertyHostVisibleBit

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDescriptorPool dv = Vk.DscPl.create dv poolInfo nil'
	where poolInfo = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = 10,
		Vk.DscPl.createInfoPoolSizes = [
			Vk.DscPl.Size {
				Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBuffer,
				Vk.DscPl.sizeDescriptorCount = 10 },
			Vk.DscPl.Size {
				Vk.DscPl.sizeType =
					Vk.Dsc.TypeUniformBufferDynamic,
				Vk.DscPl.sizeDescriptorCount = 10 } ] }

createDescriptorSets ::
	(Vk.DscSet.DListFromMiddle lyts, HL.FromList lyts, Update cmbs lyts) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	HL.PL BindedCamera cmbs -> HL.PL (U2 Vk.DscSetLyt.L) lyts ->
	Vk.Bffr.Binded ssb ssm "scene-buffer" '[SceneObj] ->
	(forall sds . HL.PL (Vk.DscSet.D sds) lyts -> IO a) -> IO a
createDescriptorSets dv dscp cmbs lyts scnb f =
	Vk.DscSet.allocateDs dv allocInfo \dscss -> do
	update dv dscss cmbs scnb
	f dscss
	where allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = lyts }

class Update csbs lyts where
	update :: Vk.Dvc.D sd -> HL.PL (Vk.DscSet.D sds) lyts ->
		HL.PL BindedCamera csbs ->
		Vk.Bffr.Binded ssb ssm "scene-buffer" '[SceneObj] -> IO ()

instance Update '[] '[] where update _ HL.Nil HL.Nil _ = pure ()

instance (
	Vk.DscSet.T.BindingAndArrayElemBuffer
		(TIndex.I1_2 '(slyt, bs))
		'[CameraObj] 0,
	Vk.DscSet.T.BindingAndArrayElemBuffer
		(TIndex.I1_2 '(slyt, bs))
		'[SceneObj] 0,
	Vk.DscSet.T.UpdateDynamicLength
		(TIndex.I1_2 '(slyt, bs))
		'[CameraObj],
	Vk.DscSet.T.UpdateDynamicLength
		(TIndex.I1_2 '(slyt, bs))
		'[SceneObj],
	Update csbs lyts ) => Update (csb ': csbs) ('(slyt, bs) ': lyts) where
	update dv (dscs :** dscss) (BindedCamera csb :** csbs) scnb = do
		Vk.DscSet.updateDs dv (
			U5 (descriptorWrite @CameraObj
				dscs csb Vk.Dsc.TypeUniformBuffer) :**
			U5 (descriptorWrite @SceneObj
				dscs scnb Vk.Dsc.TypeUniformBufferDynamic) :**
			HL.Nil )
			HL.Nil
		update dv dscss csbs scnb

descriptorWrite :: forall obj slbts sb sm nm objs sds . (
	Show (HL.PL Obj.ObjectLength objs), Obj.Offset obj objs ) =>
	Vk.DscSet.D sds slbts -> Vk.Bffr.Binded sm sb nm objs ->
	Vk.Dsc.Type -> Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgBuffer '[ '(sm, sb, nm, obj)]) 0
descriptorWrite dscs ub tp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = tp,
	Vk.DscSet.writeSources =
		Vk.DscSet.BufferInfos . HL.Singleton . U4 $ Vk.Dsc.BufferInfo ub }

createVertexBuffer :: forall sd sc nm a . Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C sc -> V.Vector Vertex -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""] -> IO a ) ->
	IO a
createVertexBuffer pd dv gq cp vs f =
	createBuffer pd dv lns
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit \b _ ->
	createBuffer pd dv lns
		Vk.Bffr.UsageTransferSrcBit
		(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)
		\b' (bm' :: Vk.Mm.M sm '[ '(s,
			'Vk.Mm.BufferArg nm '[Obj.List 256 Vertex ""])]) -> do
	Vk.Mm.write @nm @(Obj.List 256 Vertex "") dv bm' zeroBits vs
	beginSingleTimeCommands dv gq cp \cb ->
		Vk.Cmd.copyBuffer @'[ '[Obj.List 256 Vertex ""]] cb b' b
	f b
	where lns = HL.Singleton . Obj.ObjectLengthList $ V.length vs

createCommandBuffers ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb . HL.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> IO a) -> IO a
createCommandBuffers dv cp f = Vk.CBffr.allocate dv allcInfo f
	where
	allcInfo :: Vk.CBffr.AllocateInfo 'Nothing scp (HL.Dummies MaxFramesInFlight)
	allcInfo = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cp,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

createSyncObjects ::
	Vk.Dvc.D sd -> (forall ssos . SyncObjects ssos -> IO a ) -> IO a
createSyncObjects dv f =
	HL.replicateM maxFramesInFlight
		(Vk.Semaphore.create @'Nothing dv def nil') \iass ->
	HL.replicateM maxFramesInFlight
		(Vk.Semaphore.create @'Nothing dv def nil') \rfss ->
	HL.replicateM maxFramesInFlight
		(Vk.Fnc.create @'Nothing dv inf nil') \iffs ->
	f $ SyncObjects iass rfss iffs
	where inf = def { Vk.Fnc.createInfoFlags = Vk.Fnc.CreateSignaledBit }

data SyncObjects (ssos :: ([Type], [Type], [Type])) where
	SyncObjects :: {
		imageAvailableSemaphores :: HL.PL Vk.Semaphore.S siass,
		renderFinishedSemaphores :: HL.PL Vk.Semaphore.S srfss,
		inFlightFences :: HL.PL Vk.Fnc.F sffs } ->
		SyncObjects '(siass, srfss, siffs)

mainLoop :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs, HL.HomoList '(slyt, Buffers) lyts) =>
	Glfw.Window -> FramebufferResized -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HL.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl '[ '(slyt, Buffers)] '[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(slyt, Buffers)], '[WMeshPushConstants]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HL.PL Vk.Frmbffr.F sfs -> HL.PL MemoryCamera scms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg "scene-buffer" '[SceneObj])] ->
	HL.PL (Vk.DscSet.D sds) lyts ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 Vertex ""] ->
	HL.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> SyncObjects sos ->
	VertexNumber -> IO ()
mainLoop w rszd sfc pd qfis dv gq pq sc ex0 scivs rp lyt gpl cp drs fbs
	cmms scnm dss vb vbtri cbs sos vnsln =
	($ 0) . ($ cycleI [0 .. maxFramesInFlight - 1]) . ($ ex0) $ fix
		\loop ex (ffn :- ffns) fn -> Glfw.pollEvents >>
	step w rszd sfc pd qfis dv gq pq sc ex scivs rp lyt gpl cp drs fbs
		cmms scnm dss vb vbtri cbs sos vnsln ffn fn
		(\ex' -> loop ex' ffns ((fn + 1) `mod` (360 * frashRate))) >>
	Vk.Dvc.waitIdle dv

type VertexNumber = Word32

step :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs, HL.HomoList '(slyt, Buffers) lyts) =>
	Glfw.Window -> FramebufferResized -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HL.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl '[ '(slyt, Buffers)] '[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(slyt, Buffers)], '[WMeshPushConstants]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HL.PL Vk.Frmbffr.F sfs -> HL.PL MemoryCamera scms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg "scene-buffer" '[SceneObj])] ->
	HL.PL (Vk.DscSet.D sds) lyts ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 Vertex ""] ->
	HL.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> SyncObjects sos ->
	Word32 -> Int -> Int -> (Vk.Extent2d -> IO ()) -> IO ()
step w frszd sfc pd qfis dv gq pq sc ex scivs rp lyt gpl cp drs fbs
	cmms scnm dss vb vbtri cbs sos vnsln ffn fn loop = do
	catchAndRecreate w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs loop
		$ drawFrame dv gq pq sc ex rp lyt gpl fbs
			cmms scnm dss vb vbtri cbs sos vnsln ffn fn
	(Glfw.windowShouldClose w >>=) . flip bool (pure ()) $
		(checkFlag frszd >>=) . bool (loop ex) $
		loop =<< recreateAll
			w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs

catchAndRecreate :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QueueFamilyIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	HL.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl '[ '(s, Buffers)] '[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(s, Buffers)], '[WMeshPushConstants]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HL.PL Vk.Frmbffr.F sfs -> (Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just (); Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\() -> loop =<< recreateAll
		w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs

recreateAll :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QueueFamilyIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	HL.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl '[ '(slyt, Buffers)] '[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(slyt, Buffers)], '[WMeshPushConstants]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HL.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateAll w sfc pd qfs dv gq sc scivs rp lyt gpl cp drs@(_, _, divw) fbs =
	waitFramebufferSize w >> Vk.Dvc.waitIdle dv >>
	recreateSwapchain w sfc pd qfs dv sc >>= \ex ->
	ex <$ do
	Vk.Khr.Swpch.getImages dv sc >>= \i -> recreateImageViews dv i scivs
	recreateDepthResources pd dv gq cp ex drs
	recreateGraphicsPipeline dv ex rp lyt gpl
	recreateFramebuffers dv ex rp scivs divw fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize w = Glfw.getFramebufferSize w >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize w
	where zero = uncurry (||) . ((== 0) *** (== 0))

drawFrame ::
	forall sd ssc scfmt sr slyt sl sg sfs scmmbs ssm ssb lyts
	sm sb nm smtri sbtri nmtri scb ssos sds .
	(HL.HomoList '(sl, Buffers) lyts ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L slyt '[ '(sl, Buffers)] '[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt,	'[ '(sl, Buffers)], '[WMeshPushConstants]) ->
	HL.PL Vk.Frmbffr.F sfs -> HL.PL MemoryCamera scmmbs ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg "scene-buffer" '[SceneObj])] ->
	HL.PL (Vk.DscSet.D sds) lyts ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 Vertex ""] ->
	HL.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> SyncObjects ssos ->
	Word32 -> Int -> Int -> IO ()
drawFrame dv gq pq sc ex rp lyt gpl fbs cmms scnm dss vb vbtri cbs
	(SyncObjects iass rfss iffs) vnsln ffn fn =
	HL.index iass ffn \(ias :: Vk.Semaphore.S sias) ->
	HL.index rfss ffn \(rfs :: Vk.Semaphore.S srfs) ->
	HL.index iffs ffn \(id &&& HL.Singleton -> (iff, siff)) ->
	HL.index cmms ffn \(MemoryCamera cmm) -> do
	Vk.Mm.write @"camera-buffer" @CameraObj dv cmm zeroBits (cameraData ex)
	Vk.Mm.write @"scene-buffer" @(SceneObj) dv scnm zeroBits . (!! ffn)
		$ iterate (Nothing :) [Just $ sceneData fn]
	Vk.Fnc.waitForFs dv siff True Nothing
	iid <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dv sc uint64Max (Just ias) Nothing
	Vk.Fnc.resetFs dv siff
	Vk.CBffr.reset cb zeroBits
	HL.index fbs iid \fb -> recordCommandBuffer
		ex rp lyt gpl fb ds vb vbtri cb vnsln (fromIntegral ffn) fn
	Vk.Q.submit gq (HL.Singleton . U4 $ submitInfo ias rfs) $ Just iff
	catchAndSerialize . Vk.Khr.queuePresent @'Nothing pq $ presentInfo rfs iid
	where
	submitInfo :: Vk.Semaphore.S ssi -> Vk.Semaphore.S ssr ->
		Vk.SubmitInfo 'Nothing '[ssi] '[scb] '[ssr]
	submitInfo ias rfs = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Singleton
			$ Vk.SemaphorePipelineStageFlags ias
				Vk.Ppl.StageColorAttachmentOutputBit,
		Vk.submitInfoCommandBuffers = HL.Singleton cb,
		Vk.submitInfoSignalSemaphores = HL.Singleton rfs }
	presentInfo :: Vk.Semaphore.S ssr -> Word32 ->
		Vk.Khr.PresentInfo 'Nothing '[ssr] scfmt '[ssc]
	presentInfo rfs iid = Vk.Khr.PresentInfo {
		Vk.Khr.presentInfoNext = TMaybe.N,
		Vk.Khr.presentInfoWaitSemaphores = HL.Singleton rfs,
		Vk.Khr.presentInfoSwapchainImageIndices = HL.Singleton
			$ Vk.Khr.SwapchainImageIndex sc iid }
	HL.Dummy cb = HL.homoListIndex @'() cbs ffn
	ds = HL.homoListIndex @'(sl, Buffers) dss ffn
	catchAndSerialize = (`catch`
		\(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

recordCommandBuffer ::
	forall sr slyt sg sdlyt sf sm sb nm smtri sbtri nmtri scb sds .
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L slyt '[ '(sdlyt, Buffers)] '[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt,	'[ '(sdlyt, Buffers)], '[WMeshPushConstants]) ->
	Vk.Frmbffr.F sf -> Vk.DscSet.D sds '(sdlyt, Buffers) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 Vertex ""] ->
	Vk.CBffr.C scb -> Word32 -> Word32 -> Int -> IO ()
recordCommandBuffer sce rp lyt gpl fb ds vb vbt cb vn ffn (fromIntegral -> fn) =
	Vk.CBffr.begin @'Nothing @'Nothing cb binfo $
	Vk.Cmd.beginRenderPass cb rpinfo Vk.Subpass.ContentsInline do
	ovb <- newIORef Nothing
	drawObject ovb cb ds RenderObject {
		renderObjectPipeline = gpl,
		renderObjectPipelineLayout = lyt,
		renderObjectMesh = vb, renderObjectMeshSize = vn,
		renderObjectTransformMatrix = model } ffn
	ovbtri <- newIORef Nothing
	for_ [- 20 .. 20] \x -> for_ [- 20 .. 20] \y ->
		drawObject ovbtri cb ds RenderObject {
			renderObjectPipeline = gpl,
			renderObjectPipelineLayout = lyt,
			renderObjectMesh = vbt, renderObjectMeshSize = 3,
			renderObjectTransformMatrix =
				trans x y `Cglm.mat4Mul` scale } ffn
	where
	binfo :: Vk.CBffr.BeginInfo 'Nothing 'Nothing
	binfo = def { Vk.CBffr.beginInfoFlags = Vk.CBffr.UsageOneTimeSubmitBit }
	rpinfo :: Vk.RndrPss.BeginInfo 'Nothing sr sf '[
		'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32,
		'Vk.ClearTypeDepthStencil ]
	rpinfo = Vk.RndrPss.BeginInfo {
		Vk.RndrPss.beginInfoNext = TMaybe.N,
		Vk.RndrPss.beginInfoRenderPass = rp,
		Vk.RndrPss.beginInfoFramebuffer = fb,
		Vk.RndrPss.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = sce },
		Vk.RndrPss.beginInfoClearValues =
			Vk.ClearValueColor
				(fromJust $ rgbaDouble 0 0 blue 1) :**
			Vk.ClearValueDepthStencil
				(Vk.ClearDepthStencilValue 1 0) :** HL.Nil }
	blue = 0.5 + sin (fn / (180 * frashRate) * pi) / 2
	model = Cglm.rotate Cglm.mat4Identity
		(fn * Cglm.rad 1) (Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL)
	trans x y = Cglm.translate
		Cglm.mat4Identity (Cglm.Vec3 $ x :. 0 :. y :. NilL)
	scale = Cglm.scale
		Cglm.mat4Identity (Cglm.Vec3 $ 0.2 :. 0.2 :. 0.2 :. NilL)

drawObject ::
	IORef (Maybe (Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""])) ->
	Vk.CBffr.C scb -> Vk.DscSet.D sds '(sdlyt, Buffers) ->
	RenderObject sg sl sdlyt sm sb nm -> Word32 -> IO ()
drawObject ovb cb0 ds RenderObject {
	renderObjectPipeline = gpl,
	renderObjectPipelineLayout = lyt,
	renderObjectMesh = vb, renderObjectMeshSize = vn,
	renderObjectTransformMatrix = model } ffn =
	Vk.Cmd.bindPipelineGraphics cb0 Vk.Ppl.BindPointGraphics gpl \cb -> do
	Vk.Cmd.bindDescriptorSetsGraphics cb Vk.Ppl.BindPointGraphics lyt
		(HL.Singleton $ U2 ds) . HL.Singleton $
		(HL.Nil :** (Vk.Cmd.DynamicIndex ffn :** HL.Nil) :** HL.Nil)
	readIORef ovb >>= \case
		Just o | vb == o -> pure ()
		_ -> do	Vk.Cmd.bindVertexBuffers cb . HL.Singleton
				. U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @Vertex @"" vb
			writeIORef ovb $ Just vb
	Vk.Cmd.pushConstantsGraphics @'[ 'Vk.T.ShaderStageVertexBit] cb lyt
		$ HL.Id (Str.G.Wrap MeshPushConstants {
			meshPushConstantsData =
				Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
			meshPushConstantsRenderMatrix = model }) :** HL.Nil
	Vk.Cmd.draw cb vn 1 0 0

data RenderObject sg sl sdlyt sm sb nm = RenderObject {
	renderObjectPipeline :: Vk.Ppl.Grph.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(sdlyt, Buffers)], '[WMeshPushConstants]),
	renderObjectPipelineLayout ::
		Vk.Ppl.Lyt.L sl '[ '(sdlyt, Buffers)] '[WMeshPushConstants],
	renderObjectMesh :: Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""],
	renderObjectMeshSize :: Word32,
	renderObjectTransformMatrix :: Cglm.Mat4 }

-- VERTEX

data Vertex = Vertex {
	vertexPos :: Position, vertexNormal :: Normal, vertexColor :: Color }
	deriving (Show, Generic)

newtype Position = Position Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Normal = Normal Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Color = Color Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

instance SizeAlignmentList Vertex
instance SizeAlignmentListUntil Position Vertex
instance SizeAlignmentListUntil Normal Vertex
instance SizeAlignmentListUntil Color Vertex
instance Str.G.G Vertex

instance Storable Vertex where
	sizeOf = Str.G.gSizeOf; alignment = Str.G.gAlignment
	peek = Str.G.gPeek; poke = Str.G.gPoke

posNormalToVertex :: Str.G.Wrap Wv.PositionNormal -> Vertex
posNormalToVertex (Wv.W (Wv.PositionNormal
	(Wv.W (Wv.Position x y z)) (Wv.W (Wv.Normal v w u)))) =
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

-- CAMERA DATA

data CameraData = CameraData {
	cameraDataView :: View, cameraDataProj :: Proj,
	cameraDataViewProj :: ViewProj } deriving (Show, Generic)

newtype View = View Cglm.Mat4 deriving (Show, Storable)
newtype Proj = Proj Cglm.Mat4 deriving (Show, Storable)
newtype ViewProj = ViewProj Cglm.Mat4 deriving (Show, Storable)

instance Storable CameraData where
	sizeOf = Str.G.gSizeOf; alignment = Str.G.gAlignment
	peek = Str.G.gPeek; poke = Str.G.gPoke

instance Str.G.G CameraData
instance SizeAlignmentList CameraData

cameraData :: Vk.Extent2d -> CameraData
cameraData ex = CameraData (View view) (Proj $ projection ex)
	(ViewProj $ Cglm.mat4Mul (projection ex) view)

view :: Cglm.Mat4
view = Cglm.lookat
	(Cglm.Vec3 $ 0 :. 6 :. 10 :. NilL)
	(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
	(Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL)

projection :: Vk.Extent2d -> Cglm.Mat4
projection Vk.Extent2d {
	Vk.extent2dWidth = fromIntegral -> w,
	Vk.extent2dHeight = fromIntegral -> h } = Cglm.modifyMat4 1 1 negate
	$ Cglm.perspective (Cglm.rad 70) (w / h) 0.1 200

-- SCENE DATA

data SceneData = SceneData {
	sceneDataFogColor :: FogColor, sceneDataFogDists :: FogDists,
	sceneDataAmbColor :: AmbColor,
	sceneDataSunDir :: SunDir, sceneDataSunColor :: SunColor }
	deriving (Show, Generic)

newtype FogColor = FogColor Cglm.Vec4 deriving (Show, Storable)
newtype FogDists = FogDists Cglm.Vec4 deriving (Show, Storable)
newtype AmbColor = AmbColor Cglm.Vec4 deriving (Show, Storable)
newtype SunDir = SunDir Cglm.Vec4 deriving (Show, Storable)
newtype SunColor = SunColor Cglm.Vec4 deriving (Show, Storable)

instance Storable SceneData where
	sizeOf = Str.G.gSizeOf; alignment = Str.G.gAlignment
	peek = Str.G.gPeek; poke = Str.G.gPoke

instance Str.G.G SceneData
instance SizeAlignmentList SceneData

sceneData :: Int -> SceneData
sceneData fn = SceneData {
	sceneDataFogColor = FogColor . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	sceneDataFogDists = FogDists . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	sceneDataAmbColor = AmbColor . Cglm.Vec4 $ r :. 0 :. b :. 0 :. NilL,
	sceneDataSunDir = SunDir . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	sceneDataSunColor = SunColor . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL }
	where
	r = sin (fromIntegral fn / (180 * frashRate) * pi)
	b = cos (fromIntegral fn / (180 * frashRate) * pi)

-- MESH PUSH CONSTANTS

data MeshPushConstants = MeshPushConstants {
	meshPushConstantsData :: Cglm.Vec4,
	meshPushConstantsRenderMatrix :: Cglm.Mat4 } deriving (Show, Generic)

type WMeshPushConstants = Str.G.Wrap MeshPushConstants

instance SizeAlignmentList MeshPushConstants
instance Str.G.G MeshPushConstants

-- OTHER TYPES

type FramebufferResized = IORef Bool

-- SHADER

shaderStages ::
	Spv 'GlslVertexShader -> Spv 'GlslFragmentShader ->
	HL.PL (U5 Vk.Ppl.ShdrSt.CreateInfoNew) '[
		'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
		'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
shaderStages vs fs = U5 vertinfo :** U5 fraginfo :** HL.Nil where
	vertinfo = Vk.Ppl.ShdrSt.CreateInfoNew {
		Vk.Ppl.ShdrSt.createInfoNextNew = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlagsNew = def,
		Vk.Ppl.ShdrSt.createInfoStageNew = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModuleNew = mdl vs,
		Vk.Ppl.ShdrSt.createInfoNameNew = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfoNew = Nothing }
	fraginfo = Vk.Ppl.ShdrSt.CreateInfoNew {
		Vk.Ppl.ShdrSt.createInfoNextNew = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlagsNew = def,
		Vk.Ppl.ShdrSt.createInfoStageNew = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModuleNew = mdl fs,
		Vk.Ppl.ShdrSt.createInfoNameNew = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfoNew = Nothing }
	mdl :: Spv sknd -> Vk.Shader.Module.M 'Nothing sknd 'Nothing
	mdl cd = Vk.Shader.Module.M crInfo nil'
		where crInfo = Vk.Shader.Module.M.CreateInfo {
			Vk.Shader.Module.M.createInfoNext = TMaybe.N,
			Vk.Shader.Module.M.createInfoFlags = zeroBits,
			Vk.Shader.Module.M.createInfoCode = cd }

[glslVertexShader|

#version 450

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inColor;

layout(location = 0) out vec3 outColor;

layout (set = 0, binding = 0) uniform CameraBuffer {
	mat4 view; mat4 proj; mat4 viewproj; } cameraData;

layout(push_constant) uniform constants {
	vec4 data; mat4 render_matrix; } PushConstants;

void
main()
{
	mat4 transformMatrix =
		(cameraData.viewproj * PushConstants.render_matrix);
	gl_Position = transformMatrix * vec4(inPosition, 1.0);
	outColor = inColor;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec3 inColor;
layout(location = 0) out vec4 outColor;

layout (set = 0, binding = 1) uniform SceneData {
	vec4 fogColor; vec4 fogDists;
	vec4 ambientColor;
	vec4 sunlightDir; vec4 sunlightColor; } sceneData;

void
main()
{
	outColor = vec4(inColor + sceneData.ambientColor.xyz, 1.0);
}

|]
