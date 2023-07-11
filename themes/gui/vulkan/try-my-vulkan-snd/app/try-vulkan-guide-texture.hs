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
import Data.Kind.Object qualified as KObj
import Data.Foldable
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Bool qualified as TBool
import Data.TypeLevel.Maybe qualified as TMaybe
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
import qualified Gpu.Vulkan.PhysicalDevice.Middle as Vk.Phd.M
import qualified Gpu.Vulkan.PhysicalDevice.Struct as Vk.Phd
import qualified Gpu.Vulkan.QueueFamily as Vk.QFmly
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Device.Middle as Vk.Dvc.M
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Sfc
import qualified Gpu.Vulkan.Khr.Surface.Middle as Vk.Khr.Sfc.M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as Vk.Khr.Sfc.Phd
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swpch
import qualified Gpu.Vulkan.Khr.Swapchain.Type as Vk.Khr.Swpch
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as Vk.Khr.Swpch.M
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
import qualified Gpu.Vulkan.Memory.AllocateInfo as Vk.Dvc.Mem
import qualified Gpu.Vulkan.Memory as Vk.Dvc.Mem
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Memory.Kind as Vk.Mm.K
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

import Foreign.Ptr
import Foreign.Marshal.Array
import Data.Array
import Codec.Picture
import Codec.Picture.Tools

import Gpu.Vulkan.Sampler as Vk.Smplr
import Gpu.Vulkan.Sampler.Enum as Vk.Smplr
import Gpu.Vulkan.Sampler.Middle as Vk.Smplr.M

maxFramesInFlight :: Integral n => n
maxFramesInFlight = 2

type MaxFramesInFlight = 2

frashRate :: Num n => n
frashRate = 2

main :: IO ()
main = do
	[objfile] <- getArgs
	s <- BS.readFile objfile
	print $ Wv.countV' s
	let	vns = vertices s
--	print vns
	withWindow \w frszd -> createInstance \ist -> if enableValidationLayers
		then Vk.Ext.DbgUtls.Msngr.create ist debugMessengerInfo nil'
			$ const $ run w ist frszd vns
		else run w ist frszd vns
	where
	vertices s = V.map posTxtNormalToVertex
		. uncurry4 Wv.facePosTxtNormal $ Wv.readV'' cv ct cn cf s
		where Wv.Count {
			Wv.countVertex = cv,
			Wv.countTexture = ct,
			Wv.countNormal = cn,
			Wv.countFace = cf } = Wv.countV' s

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (x, y, z, w) = f x y z w

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
	createSwapchain w sfc pd qfs dv \(sc :: Vk.Khr.Swpch.SNew ss fmt) ex ->
	Vk.Khr.Swpch.getImagesNew dv sc >>= \imgs ->
	createImageViews dv imgs \scivs ->
	findDepthFormat pd >>= \dfmt ->
	Vk.T.formatToType dfmt \(_ :: Proxy dfmt) ->
	createRenderPass @fmt @dfmt dv \rp ->

	createDescriptorSetLayout dv \dslyt ->
	createDescriptorSetLayoutObjData dv \dslyto ->
	createSingleTextureSetLayout dv \dslyttx ->
	createPipelineLayout dv dslyt dslyto dslyttx \lyt ->
	createGraphicsPipeline dv ex rp lyt \gpl ->

	createCommandPool dv qfs \cp ->
	createDepthResources @dfmt pd dv gq cp ex \drs@(_, _, dimgv) ->
	createFramebuffers dv ex rp scivs dimgv \fbs ->

	createCameraObjDataBuffers pd dv dslyt dslyto maxFramesInFlight
		\lyts cmbs cmms
			(lytods :: HL.PL (U2 Vk.DscSetLyt.L) slytods)
			(odbs :: HL.PL BindedObjData sbsmods) odms ->

	createTextureImage pd dv gq cp "assets/lost_empire-RGBA.png" \timg ->
	createTextureImageView dv timg \timgvw ->

	createSceneBuffer pd dv \scnb scnm ->
--	createObjDataBuffers pd dv dslyto maxFramesInFlight \lytods odbs odms ->
	createDescriptorPool dv \dp ->

	allocateTextureDescriptorSets dv dp dslyttx \dscstx ->
	writeTexture1 dv dscstx timgvw \wtx ->
	Vk.DscSet.updateDs dv (HL.Singleton $ U5 wtx) HL.Nil >>

	createDescriptorSets @sbsmods @slytods dv dp cmbs lyts odbs lytods scnb \dss dssod ->

--	createUploadContext dv qfs \uctxt ->

--	print vns >>

	createVertexBuffer pd dv gq cp vns \vb ->
	createVertexBuffer pd dv gq cp triangle \vbtri ->
	createCommandBuffers dv cp \cbs ->
	createSyncObjects dv \sos ->

	mainLoop w rszd sfc pd qfs dv gq pq sc ex scivs rp lyt gpl cp drs fbs
		cmms scnm dss odms dssod dscstx vb vbtri cbs sos vnsln

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
deviceExtensions = [Vk.Khr.Swpch.M.extensionName]

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
		Vk.Dvc.M.createInfoNext = TMaybe.J drawParFeatures,
		Vk.Dvc.M.createInfoFlags = zeroBits,
		Vk.Dvc.M.createInfoQueueCreateInfos = qcris,
		Vk.Dvc.M.createInfoEnabledLayerNames = bool
			[] [Vk.Khr.validationLayerName] enableValidationLayers,
		Vk.Dvc.M.createInfoEnabledExtensionNames = deviceExtensions,
		Vk.Dvc.M.createInfoEnabledFeatures = Just def }
	drawParFeatures :: Vk.Phd.M.ShaderDrawParametersFeatures 'Nothing
	drawParFeatures = Vk.Phd.M.ShaderDrawParametersFeatures {
		Vk.Phd.M.shaderDrawParametersFeaturesNext = TMaybe.N,
		Vk.Phd.M.shaderDrawParametersFeaturesShaderDrawParameters = True }

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
		Vk.Khr.Swpch.SNew ss scfmt -> Vk.Extent2d -> IO a) -> IO a
createSwapchain w sfc ph qfs dv f = getSwapchainSupport ph sfc >>= \spp -> do
	ex <- chooseSwapExtent w $ capabilities spp
	let	fmt = Vk.Khr.Sfc.M.formatFormat
			. chooseSwapSurfaceFormat $ formats spp
	Vk.T.formatToType fmt \(_ :: Proxy fmt) ->
		Vk.Khr.Swpch.createNew @'Nothing @fmt dv
			(swapchainCreateInfo sfc qfs spp ex) nil' (`f` ex)

recreateSwapchain :: Vk.T.FormatToValue scfmt =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Khr.Swpch.SNew ssc scfmt ->
	IO Vk.Extent2d
recreateSwapchain w sfc ph qfs dv sc = getSwapchainSupport ph sfc >>= \spp -> do
	ex <- chooseSwapExtent w $ capabilities spp
	ex <$ Vk.Khr.Swpch.recreateNew @'Nothing dv
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
	Vk.Khr.Swpch.CreateInfoNew 'Nothing ss fmt
swapchainCreateInfo sfc qfs spp ext = Vk.Khr.Swpch.CreateInfoNew {
	Vk.Khr.Swpch.createInfoNextNew = TMaybe.N,
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
	Vk.ImgVw.createNew dv (imageViewCreateInfo img asps) nil'

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
	Vk.ImgVw.recreateNew dv (imageViewCreateInfo img asps) nil' iv

imageViewCreateInfo ::
	Vk.Img.Binded sm si nm ifmt -> Vk.Img.AspectFlags ->
	Vk.ImgVw.CreateInfoNew 'Nothing sm si nm ifmt ivfmt
imageViewCreateInfo img asps = Vk.ImgVw.CreateInfoNew {
	Vk.ImgVw.createInfoNextNew = TMaybe.N,
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

type ObjDataBuffers = '[ 'Vk.DscSetLyt.Buffer '[ObjDataList] ]

type CameraObj = Obj.Atom 256 CameraData 'Nothing
type SceneObj = Obj.DynAtom 2 256 SceneData 'Nothing

createDescriptorSetLayoutObjData :: Vk.Dvc.D sd ->
	(forall (s :: Type) . Vk.DscSetLyt.L s '[ 'Vk.DscSetLyt.Buffer '[ObjDataList]] -> IO a) -> IO a
createDescriptorSetLayoutObjData dv = Vk.DscSetLyt.create dv layoutInfo nil' where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[ 'Vk.DscSetLyt.Buffer '[ObjDataList]]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = objd :** HL.Nil }
	objd :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer '[ObjDataList])
	objd = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

type ObjDataList = Obj.List 256 ObjData ""

createSingleTextureSetLayout :: Vk.Dvc.D sd ->
	(forall s .
		Vk.DscSetLyt.L s '[
			'Vk.DscSetLyt.Image '[
				'("texture", 'Vk.T.FormatR8g8b8a8Srgb) ] ] ->
		IO a) -> IO a
createSingleTextureSetLayout dv = Vk.DscSetLyt.create dv layoutInfo nil' where
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = textureBind :** HL.Nil }
	textureBind = Vk.DscSetLyt.BindingImage {
		Vk.DscSetLyt.bindingImageDescriptorType =
			Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscSetLyt.bindingImageStageFlags =
			Vk.ShaderStageFragmentBit }

createPipelineLayout :: forall sd sdl sdlod sdltx foo a . Vk.Dvc.D sd ->
	Vk.DscSetLyt.L sdl Buffers ->
	Vk.DscSetLyt.L sdlod ObjDataBuffers ->
	Vk.DscSetLyt.L sdltx foo ->
	(forall sl . Vk.Ppl.Lyt.L sl
			'[ '(sdl, Buffers), '(sdlod, ObjDataBuffers), '(sdltx, foo) ]
			'[WMeshPushConstants] ->
		IO a) -> IO a
createPipelineLayout dv dslyt dslytod dslyttx f = Vk.Ppl.Lyt.createNew dv ci nil' f where
	ci :: Vk.Ppl.Lyt.CreateInfoNew 'Nothing '[ '(sdl, Buffers), '(sdlod, ObjDataBuffers), '(sdltx, foo) ] (
		'Vk.PushConstant.PushConstantLayout
			'[ WMeshPushConstants]
			'[ 'Vk.PushConstant.Range '[ 'Vk.T.ShaderStageVertexBit]
				'[WMeshPushConstants]] )
	ci = Vk.Ppl.Lyt.CreateInfoNew {
		Vk.Ppl.Lyt.createInfoNextNew = TMaybe.N,
		Vk.Ppl.Lyt.createInfoFlagsNew = zeroBits,
		Vk.Ppl.Lyt.createInfoSetLayoutsNew =
			U2 dslyt :** U2 dslytod :** U2 dslyttx :** HL.Nil }

type Foo = '[ 'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)]]

createGraphicsPipeline :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(sdl, Buffers), '(sdlod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConstants] ->
	(forall sg . Vk.Ppl.Grph.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl, '[ '(sdl, Buffers), '(sdlod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConstants]) -> IO a) ->
	IO a
createGraphicsPipeline dv sce rp lyt f = Vk.Ppl.Grph.createGs dv Nothing
	(HL.Singleton . U14 $ graphicsPipelineCreateInfo sce rp lyt) nil'
	\(HL.Singleton (U3 gpl)) -> f gpl

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(sdl, Buffers), '(sdlod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[ '(sdl, Buffers), '(sdlod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConstants]) -> IO ()
recreateGraphicsPipeline dv sce rp lyt gpls = Vk.Ppl.Grph.recreateGs dv Nothing
	(U14 (graphicsPipelineCreateInfo sce rp lyt) :** HL.Nil) nil'
	(U3 gpls :** HL.Nil)

graphicsPipelineCreateInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(sdl, Buffers), '(sdlod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.CreateInfo 'Nothing
		'[	'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[])]
		'( 'Nothing, '[ '(Vertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)])
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'(sl,	'[ '(sdl, Buffers), '(sdlod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConstants]) sr
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
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.K.Image nm fmt)],
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
		Vk.Mm.M sm '[ '(si, 'Vk.Mm.K.Image nm fmt) ] -> IO a) -> IO a
createImage pd dv ex tlng usg prs f =
	Vk.Img.createNew @'Nothing dv (imageInfo ex tlng usg) nil' \i ->
	imageMemoryInfo pd dv prs i >>= \ii -> imageAllocateBind dv i ii f

recreateImage :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Extent2d -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mm.PropertyFlags ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.K.Image nm fmt)] -> IO ()
recreateImage pd dv ex tlng usg prs i m = do
	Vk.Img.recreateNew @'Nothing dv (imageInfo ex tlng usg) nil' i
	imageMemoryInfoB pd dv prs i >>= \ii -> imageReallocateBind dv i ii m

imageInfo :: Vk.Extent2d ->
	Vk.Img.Tiling -> Vk.Img.UsageFlags -> Vk.Img.CreateInfoNew 'Nothing fmt
imageInfo ex tlng usg = Vk.Img.CreateInfoNew {
		Vk.Img.createInfoNextNew = TMaybe.N,
		Vk.Img.createInfoImageTypeNew = Vk.Img.Type2d,
		Vk.Img.createInfoExtentNew = Vk.Extent3d {
			Vk.extent3dWidth = Vk.extent2dWidth ex,
			Vk.extent3dHeight = Vk.extent2dHeight ex,
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

imageMemoryInfo :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Mm.PropertyFlags ->
	Vk.Img.INew s nm fmt -> IO (Vk.Dvc.Mem.AllocateInfo 'Nothing)
imageMemoryInfo pd dv prs i = do
	rqs <- Vk.Img.getMemoryRequirementsNew dv i
	mt <- findMemoryType pd (Vk.Mm.M.requirementsMemoryTypeBits rqs) prs
	pure Vk.Dvc.Mem.AllocateInfo {
		Vk.Dvc.Mem.allocateInfoNext = TMaybe.N,
		Vk.Dvc.Mem.allocateInfoMemoryTypeIndex = mt }

imageMemoryInfoB :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Mm.PropertyFlags ->
	Vk.Img.Binded sm si nm fmt -> IO (Vk.Dvc.Mem.AllocateInfo 'Nothing)
imageMemoryInfoB pd dv prs i = do
	rqs <- Vk.Img.getMemoryRequirementsBindedNew dv i
	mt <- findMemoryType pd (Vk.Mm.M.requirementsMemoryTypeBits rqs) prs
	pure Vk.Dvc.Mem.AllocateInfo {
		Vk.Dvc.Mem.allocateInfoNext = TMaybe.N,
		Vk.Dvc.Mem.allocateInfoMemoryTypeIndex = mt }

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

imageAllocateBind :: Vk.Dvc.D sd -> Vk.Img.INew si nm fmt ->
	Vk.Dvc.Mem.AllocateInfo 'Nothing -> (forall sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mm.M sm '[ '(si, 'Vk.Mm.K.Image nm fmt) ] -> IO a) -> IO a
imageAllocateBind dv i mi f = Vk.Mm.allocateBind @'Nothing dv
	(HL.Singleton . U2 $ Vk.Mm.Image i) mi nil'
	\(HL.Singleton (U2 (Vk.Mm.ImageBinded b))) m -> f b m

imageReallocateBind :: Vk.Dvc.D sd -> Vk.Img.Binded sm sb nm fmt ->
	Vk.Dvc.Mem.AllocateInfo 'Nothing ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.K.Image nm fmt)] -> IO ()
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
	Vk.Q.submit gq (HL.Singleton . U4 $ sminfo cb) Nothing
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
	sminfo :: forall s . Vk.CBffr.C s -> Vk.SubmitInfo 'Nothing '[] '[s] '[]
	sminfo cb = Vk.SubmitInfo {
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

{-
createCameraBuffers :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sdsc Buffers ->
	Int -> (forall slyts sbsms . (
		Vk.DscSet.SListFromMiddle slyts, HL.FromList slyts,
		Update sbsms slyts odbs slytods, HL.HomoList '(sdsc, Buffers) slyts ) =>
		HL.PL (U2 Vk.DscSetLyt.L) slyts ->
		HL.PL BindedCamera sbsms -> HL.PL MemoryCamera sbsms ->
		IO a) -> IO a
createCameraBuffers _ _ _ n f | n < 1 = f HL.Nil HL.Nil HL.Nil
createCameraBuffers pd dv lyt n f = createCameraBuffer pd dv \b m ->
	createCameraBuffers pd dv lyt (n - 1) \lyts bs ms ->
	f (U2 lyt :** lyts) (BindedCamera b :** bs) (MemoryCamera m :** ms)
	-}

createCameraObjDataBuffers :: Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.DscSetLyt.L sdsc Buffers ->
	Vk.DscSetLyt.L sodlyt ObjDataBuffers ->
	Int -> (forall slyts sbsms slytods sbsmods . (
		Vk.DscSet.DListFromMiddle slyts, HL.FromList slyts,
		Vk.DscSet.DListFromMiddle slytods,
		Update sbsms slyts sbsmods slytods,
		HL.HomoList '(sdsc, Buffers) slyts,
		HL.HomoList '(sodlyt, ObjDataBuffers) slytods
		) =>
		HL.PL (U2 Vk.DscSetLyt.L) slyts ->
		HL.PL BindedCamera sbsms -> HL.PL MemoryCamera sbsms ->
		HL.PL (U2 Vk.DscSetLyt.L) slytods ->
		HL.PL BindedObjData sbsmods -> HL.PL MemoryObjData sbsmods ->
		IO a) -> IO a
createCameraObjDataBuffers _ _ _ _ n f | n < 1 = f HL.Nil HL.Nil HL.Nil HL.Nil HL.Nil HL.Nil
createCameraObjDataBuffers pd dv lyt lytod n f =
	createCameraBuffer pd dv \b m ->
	createObjDataBuffer pd dv \bod mobjd ->
	createCameraObjDataBuffers pd dv lyt lytod (n - 1) \lyts bs ms lytods bods mods ->
	f (U2 lyt :** lyts) (BindedCamera b :** bs) (MemoryCamera m :** ms)
		(U2 lytod :** lytods) (BindedObjData bod :** bods) (MemoryObjData mobjd :** mods)

data BindedCamera smsb where
	BindedCamera :: Vk.Bffr.Binded sm sb "camera-buffer" '[CameraObj] ->
		BindedCamera '(sm, sb)

data BindedObjData smsb where
	BindedObjData ::
		Vk.Bffr.Binded sm sb "object-data-buffer" '[ObjDataList] ->
		BindedObjData '(sm, sb)

data MemoryCamera smsb where
	MemoryCamera ::
		Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.K.Buffer "camera-buffer" '[CameraObj]) ] ->
		MemoryCamera '(sm, sb)

data MemoryObjData smsb where
	MemoryObjData ::
		Vk.Mm.M sm '[ '(sb,
			'Vk.Mm.K.Buffer "object-data-buffer" '[ObjDataList])] ->
		MemoryObjData '(sm, sb)

createCameraBuffer :: Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb . Vk.Bffr.Binded sm sb nm '[CameraObj] ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.K.Buffer nm '[CameraObj]) ] ->
		IO a) -> IO a
createCameraBuffer pd dv = createBuffer pd dv
	(HL.Singleton Obj.ObjectLengthAtom)
	Vk.Bffr.UsageUniformBufferBit Vk.Mm.PropertyHostVisibleBit

createBuffer :: forall objs nm sd a . (
	Obj.SizeAlignmentList objs, forall s . SizeAlignmentAll s nm objs ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> HL.PL Obj.ObjectLength objs ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm objs ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.K.Buffer nm objs)] -> IO a) ->
	IO a
createBuffer pd dv lns usg prs f =
	Vk.Bffr.create dv (bufferInfo lns usg) nil' \b ->
	Vk.Bffr.getMemoryRequirements dv b >>= \rs ->
	findMemoryType pd (Vk.Mm.M.requirementsMemoryTypeBits rs) prs >>= \mt ->
	Vk.Mm.allocateBind dv
		(HL.Singleton . U2 $ Vk.Mm.Buffer b) (memoryInfo mt) nil'
		$ f . \(HL.Singleton (U2 (Vk.Mm.BufferBinded bnd))) -> bnd

class Vk.Mm.Alignments '[ '(s, 'Vk.Mm.K.Buffer nm objs)] =>
	SizeAlignmentAll s nm (objs :: [Obj.Object])

instance Vk.Mm.Alignments '[ '(s, 'Vk.Mm.K.Buffer nm '[obj])] =>
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

memoryInfo :: Vk.Mm.M.TypeIndex -> Vk.Dvc.Mem.AllocateInfo 'Nothing
memoryInfo mt = Vk.Dvc.Mem.AllocateInfo {
	Vk.Dvc.Mem.allocateInfoNext = TMaybe.N,
	Vk.Dvc.Mem.allocateInfoMemoryTypeIndex = mt }

createSceneBuffer :: Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[SceneObj] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.K.Buffer nm '[SceneObj])] ->
	IO a) -> IO a
createSceneBuffer pd dv = createBuffer pd dv
	(HL.Singleton Obj.ObjectLengthDynAtom)
	Vk.Bffr.UsageUniformBufferBit Vk.Mm.PropertyHostVisibleBit

maxObjects :: Int
maxObjects = 10000

createObjDataBuffers :: Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.DscSetLyt.L sdsc '[ 'Vk.DscSetLyt.Buffer '[ObjDataList] ] ->
	Int -> (forall slyts sbsms . (
		Vk.DscSet.DListFromMiddle slyts, HL.FromList slyts,
--		Update sbsms slyts,
		HL.HomoList
			'(sdsc, '[ 'Vk.DscSetLyt.Buffer '[ObjDataList]]) slyts ) =>
		HL.PL (U2 Vk.DscSetLyt.L) slyts ->
		HL.PL BindedObjData sbsms -> HL.PL MemoryObjData sbsms ->
		IO a) -> IO a
createObjDataBuffers _ _ _ n f | n < 1 = f HL.Nil HL.Nil HL.Nil
createObjDataBuffers pd dv lyt n f = createObjDataBuffer pd dv \b m ->
	createObjDataBuffers pd dv lyt (n - 1) \lyts bs ms ->
	f (U2 lyt :** lyts) (BindedObjData b :** bs) (MemoryObjData m :** ms)

createObjDataBuffer :: Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[ObjDataList] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.K.Buffer nm '[ObjDataList])] ->
	IO a) -> IO a
createObjDataBuffer pd dv = createBuffer pd dv
	(HL.Singleton $ Obj.ObjectLengthList maxObjects)
	Vk.Bffr.UsageStorageBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyDeviceLocalBit)

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
				Vk.DscPl.sizeDescriptorCount = 10 },
			Vk.DscPl.Size {
				Vk.DscPl.sizeType =
					Vk.Dsc.TypeStorageBuffer,
				Vk.DscPl.sizeDescriptorCount = 10 },
			Vk.DscPl.Size {
				Vk.DscPl.sizeType =
					Vk.Dsc.TypeCombinedImageSampler,
				Vk.DscPl.sizeDescriptorCount = 10 } ] }

createDescriptorSets ::
	forall odbs lytods lyts cmbs sd sp ssb ssm a . (
	Vk.DscSet.DListFromMiddle lyts,
	Vk.DscSet.DListFromMiddle lytods,
	HL.FromList lyts,
	Update cmbs lyts odbs lytods) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	HL.PL BindedCamera cmbs -> HL.PL (U2 Vk.DscSetLyt.L) lyts ->
	HL.PL BindedObjData odbs -> HL.PL (U2 Vk.DscSetLyt.L) lytods ->
	Vk.Bffr.Binded ssb ssm "scene-buffer" '[SceneObj] ->
	(forall sds sds' .
		HL.PL (Vk.DscSet.D sds) lyts ->
		HL.PL (Vk.DscSet.D sds') lytods -> IO a) -> IO a
createDescriptorSets dv dscp cmbs lyts odbs lytods scnb f =
	Vk.DscSet.allocateDs dv allocInfo \dscss ->
	Vk.DscSet.allocateDs dv allocInfoOd \dscsods -> do
	update @_ @_ @odbs @lytods dv dscss cmbs dscsods odbs scnb
	f dscss dscsods
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = lyts }
	allocInfoOd = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = lytods }

allocateTextureDescriptorSets :: forall slyt foo sd sp a .
	Default (HL.PL (HL.PL KObj.ObjectLength)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics foo)) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.DscSetLyt.L slyt foo ->
	(forall sds . Vk.DscSet.D sds '(slyt, foo) -> IO a) -> IO a
allocateTextureDescriptorSets dv dscpl lyt f =
	Vk.DscSet.allocateDs dv Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscpl,
		Vk.DscSet.allocateInfoSetLayouts = HL.Singleton $ U2 lyt
		} \(HL.Singleton dscs) -> f dscs

class Update cmbs lyts odbs lytods where
	update :: Vk.Dvc.D sd ->
		HL.PL (Vk.DscSet.D sds) lyts -> HL.PL BindedCamera cmbs ->
		HL.PL (Vk.DscSet.D sdso) lytods -> HL.PL BindedObjData odbs ->
		Vk.Bffr.Binded ssb ssm "scene-buffer" '[SceneObj] -> IO ()

instance Update '[] '[] '[] '[] where update _ HL.Nil HL.Nil HL.Nil HL.Nil _ = pure ()

instance (
	Vk.DscSet.T.BindingAndArrayElemBuffer
		(TIndex.I1_2 '(slyt, bs))
		'[CameraObj] 0,
	Vk.DscSet.T.BindingAndArrayElemBuffer
		(TIndex.I1_2 '(slyt, bs))
		'[SceneObj] 0,
	Vk.DscSet.T.BindingAndArrayElemBuffer bods '[ObjDataList] 0,
	Vk.DscSet.T.UpdateDynamicLength
		(TIndex.I1_2 '(slyt, bs))
		'[CameraObj],
	Vk.DscSet.T.UpdateDynamicLength
		(TIndex.I1_2 '(slyt, bs))
		'[SceneObj],
	Vk.DscSet.T.UpdateDynamicLength bods '[ObjDataList],
	Update cmbs lyts odbs lytods ) =>
	Update (cmb ': cmbs) ('(slyt, bs) ': lyts)
		(odb ': odbs) ('(slytod, bods) ': lytods) where
	update dv (dscs :** dscss) (BindedCamera cmb :** cmbs)
		(dscsod :** dscsods) (BindedObjData odb :** odbs) scnb = do
		Vk.DscSet.updateDs dv (
			U5 (descriptorWrite @CameraObj
				dscs cmb Vk.Dsc.TypeUniformBuffer) :**
			U5 (descriptorWrite @SceneObj
				dscs scnb Vk.Dsc.TypeUniformBufferDynamic) :**
			U5 (descriptorWrite @ObjDataList
				dscsod odb Vk.Dsc.TypeStorageBuffer) :**
			HL.Nil )
			HL.Nil
		update @_ @_ @odbs @lytods dv dscss cmbs dscsods odbs scnb

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

writeTexture1 ::
	Vk.Dvc.D sd -> Vk.DscSet.D sds lyt -> Vk.ImgVw.I "texture" ifmt siv -> (
		forall ss . 
		Vk.DscSet.Write 'Nothing sds lyt
			('Vk.DscSet.WriteSourcesArgImage '[ '(ss, "texture", ifmt, siv)]) 0 ->
		IO a) -> IO a
writeTexture1 dv dscs tiv f =
	createTextureSampler dv \smplr ->
	f $ writeDescriptorImage
		Vk.Dsc.TypeCombinedImageSampler dscs
		(textureImageBufferInfo tiv smplr)

writeDescriptorImage :: Vk.Dsc.Type ->
	Vk.DscSet.D sds slbts -> Vk.Dsc.ImageInfo ss nm fmt si ->
	Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgImage '[ '(ss, nm, fmt, si)]) 0
writeDescriptorImage tp dscs ii = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = tp,
	Vk.DscSet.writeSources =
		Vk.DscSet.ImageInfos . HL.Singleton $ U4 ii }

textureImageBufferInfo :: Vk.ImgVw.I nm fmt si ->
	Vk.Smplr.S ss -> Vk.Dsc.ImageInfo ss nm fmt si
textureImageBufferInfo tiv tsmp = Vk.Dsc.ImageInfo {
	Vk.Dsc.imageInfoImageLayout =
		Vk.Img.LayoutShaderReadOnlyOptimal,
	Vk.Dsc.imageInfoImageView = tiv,
	Vk.Dsc.imageInfoSampler = tsmp }

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
			'Vk.Mm.K.Buffer nm '[Obj.List 256 Vertex ""])]) -> do
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
	RecreateFramebuffers sis sfs,
	HL.HomoList '(slyt, Buffers) lyts,
	HL.HomoList '(slytod, ObjDataBuffers) lytods
	) =>
	Glfw.Window -> FramebufferResized -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.SNew ssc scfmt -> Vk.Extent2d ->
	HL.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(slyt, Buffers), '(slytod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[ '(slyt, Buffers), '(slytod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConstants]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HL.PL Vk.Frmbffr.F sfs ->
	HL.PL MemoryCamera scms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.K.Buffer "scene-buffer" '[SceneObj])] ->
	HL.PL (Vk.DscSet.D sds) lyts ->
	HL.PL MemoryObjData sods ->
	HL.PL (Vk.DscSet.D sds') lytods ->
	Vk.DscSet.D sds'' '(sfoo, Foo) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 Vertex ""] ->
	HL.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> SyncObjects sos ->
	VertexNumber -> IO ()
mainLoop w rszd sfc pd qfis dv gq pq sc ex0 scivs rp lyt gpl cp drs fbs
	cmms scnm dss odms dssod dstx vb vbtri cbs sos vnsln =
	($ 0) . ($ cycleI [0 .. maxFramesInFlight - 1]) . ($ ex0) $ fix
		\loop ex (ffn :- ffns) fn -> Glfw.pollEvents >>
	step w rszd sfc pd qfis dv gq pq sc ex scivs rp lyt gpl cp drs fbs
		cmms scnm dss odms dssod dstx vb vbtri cbs sos vnsln ffn fn
		(\ex' -> loop ex' ffns ((fn + 1) `mod` (3600 * frashRate))) >>
	Vk.Dvc.waitIdle dv

type VertexNumber = Word32

step :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs,
	HL.HomoList '(slyt, Buffers) lyts,
	HL.HomoList '(slytod, ObjDataBuffers) lytods
	) =>
	Glfw.Window -> FramebufferResized -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.SNew ssc scfmt -> Vk.Extent2d ->
	HL.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(slyt, Buffers), '(slytod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[ '(slyt, Buffers), '(slytod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConstants]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HL.PL Vk.Frmbffr.F sfs -> HL.PL MemoryCamera scms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.K.Buffer "scene-buffer" '[SceneObj])] ->
	HL.PL (Vk.DscSet.D sds) lyts ->
	HL.PL MemoryObjData sods ->
	HL.PL (Vk.DscSet.D sds') lytods ->
	Vk.DscSet.D sds'' '(sfoo, Foo) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 Vertex ""] ->
	HL.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> SyncObjects sos ->
	Word32 -> Int -> Int -> (Vk.Extent2d -> IO ()) -> IO ()
step w frszd sfc pd qfis dv gq pq sc ex scivs rp lyt gpl cp drs fbs
	cmms scnm dss odms dssod dstx vb vbtri cbs sos vnsln ffn fn loop = do
	catchAndRecreate w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs loop
		$ drawFrame dv gq pq sc ex rp lyt gpl fbs
			cmms scnm dss odms dssod dstx vb vbtri cbs sos vnsln ffn fn
	(Glfw.windowShouldClose w >>=) . flip bool (pure ()) $
		(checkFlag frszd >>=) . bool (loop ex) $
		loop =<< recreateAll
			w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs

catchAndRecreate :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QueueFamilyIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Khr.Swpch.SNew ssc scfmt ->
	HL.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(s, Buffers), '(sod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[ '(s, Buffers), '(sod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConstants]) ->
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
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Khr.Swpch.SNew ssc scfmt ->
	HL.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L sl
		'[ '(slyt, Buffers), '(slytod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[ '(slyt, Buffers), '(slytod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConstants]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HL.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateAll w sfc pd qfs dv gq sc scivs rp lyt gpl cp drs@(_, _, divw) fbs =
	waitFramebufferSize w >> Vk.Dvc.waitIdle dv >>
	recreateSwapchain w sfc pd qfs dv sc >>= \ex ->
	ex <$ do
	Vk.Khr.Swpch.getImagesNew dv sc >>= \i -> recreateImageViews dv i scivs
	recreateDepthResources pd dv gq cp ex drs
	recreateGraphicsPipeline dv ex rp lyt gpl
	recreateFramebuffers dv ex rp scivs divw fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize w = Glfw.getFramebufferSize w >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize w
	where zero = uncurry (||) . ((== 0) *** (== 0))

drawFrame ::
	forall sd ssc scfmt sr slyt sl slod sfoo sg sfs scmmbs ssm ssb lyts
	sm sb nm smtri sbtri nmtri scb ssos sods lytods sds sds' sds'' .  (
	HL.HomoList '(sl, Buffers) lyts,
	HL.HomoList '(slod, ObjDataBuffers) lytods
	) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.SNew ssc scfmt -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L slyt
		'[ '(sl, Buffers), '(slod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(slyt,	'[ '(sl, Buffers), '(slod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConstants]) ->
	HL.PL Vk.Frmbffr.F sfs -> HL.PL MemoryCamera scmmbs ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.K.Buffer "scene-buffer" '[SceneObj])] ->
	HL.PL (Vk.DscSet.D sds) lyts ->
	HL.PL MemoryObjData sods ->
	HL.PL (Vk.DscSet.D sds') lytods ->
	Vk.DscSet.D sds'' '(sfoo, Foo) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 Vertex ""] ->
	HL.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> SyncObjects ssos ->
	Word32 -> Int -> Int -> IO ()
drawFrame dv gq pq sc ex rp lyt gpl fbs cmms scnm dss odms dssod dstx vb vbtri cbs
	(SyncObjects iass rfss iffs) vnsln ffn fn =
	HL.index iass ffn \(ias :: Vk.Semaphore.S sias) ->
	HL.index rfss ffn \(rfs :: Vk.Semaphore.S srfs) ->
	HL.index iffs ffn \(id &&& HL.Singleton -> (iff, siff)) ->
	HL.index cmms ffn \(MemoryCamera cmm) ->
	HL.index odms ffn \(MemoryObjData odm) -> do
	Vk.Mm.write @"camera-buffer" @CameraObj dv cmm zeroBits (cameraData ex)
	Vk.Mm.write @"scene-buffer" @SceneObj dv scnm zeroBits . (!! ffn)
		$ iterate (Nothing :) [Just $ sceneData fn]
	Vk.Mm.write @"object-data-buffer" @ObjDataList dv odm zeroBits . map ObjData $
		model (fromIntegral fn) : [ objectMatrix x y | x <- [- 20 .. 20], y <- [- 20 .. 20] ]
	Vk.Fnc.waitForFs dv siff True Nothing
	iid <- Vk.Khr.acquireNextImageResultNew [Vk.Success, Vk.SuboptimalKhr]
		dv sc uint64Max (Just ias) Nothing
	Vk.Fnc.resetFs dv siff
	Vk.CBffr.reset cb zeroBits
	HL.index fbs iid \fb -> recordCommandBuffer
		ex rp lyt gpl fb ds dsod dstx vb vbtri cb vnsln (fromIntegral ffn) fn
	Vk.Q.submit gq (HL.Singleton . U4 $ sminfo ias rfs) $ Just iff
	catchAndSerialize . Vk.Khr.queuePresentNew @'Nothing pq $ presentInfo rfs iid
	where
	sminfo :: Vk.Semaphore.S ssi -> Vk.Semaphore.S ssr ->
		Vk.SubmitInfo 'Nothing '[ssi] '[scb] '[ssr]
	sminfo ias rfs = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Singleton
			$ Vk.SemaphorePipelineStageFlags ias
				Vk.Ppl.StageColorAttachmentOutputBit,
		Vk.submitInfoCommandBuffers = HL.Singleton cb,
		Vk.submitInfoSignalSemaphores = HL.Singleton rfs }
	presentInfo :: Vk.Semaphore.S ssr -> Word32 ->
		Vk.Khr.PresentInfoNew 'Nothing '[ssr] scfmt '[ssc]
	presentInfo rfs iid = Vk.Khr.PresentInfoNew {
		Vk.Khr.presentInfoNextNew = TMaybe.N,
		Vk.Khr.presentInfoWaitSemaphoresNew = HL.Singleton rfs,
		Vk.Khr.presentInfoSwapchainImageIndicesNew = HL.Singleton
			$ Vk.Khr.SwapchainImageIndexNew sc iid }
	HL.Dummy cb = HL.homoListIndex @'() cbs ffn
	ds = HL.homoListIndex @'(sl, Buffers) dss ffn
	dsod = HL.homoListIndex @'(slod, ObjDataBuffers) dssod ffn
	catchAndSerialize = (`catch`
		\(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

recordCommandBuffer ::
	forall sr slyt sg sdlyt sdlytod sfoo sf sm sb nm smtri sbtri nmtri scb sds sds' sds'' .
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.L slyt
		'[ '(sdlyt, Buffers), '(sdlytod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(slyt,	'[ '(sdlyt, Buffers), '(sdlytod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConstants]) ->
	Vk.Frmbffr.F sf ->
	Vk.DscSet.D sds '(sdlyt, Buffers) ->
	Vk.DscSet.D sds' '(sdlytod, ObjDataBuffers) ->
	Vk.DscSet.D sds'' '(sfoo, Foo) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 Vertex ""] ->
	Vk.CBffr.C scb -> Word32 -> Word32 -> Int -> IO ()
recordCommandBuffer sce rp lyt gpl fb ds dsod dstx vb vbt cb vn ffn (fromIntegral -> fn) =
	Vk.CBffr.begin @'Nothing @'Nothing cb binfo $
	Vk.Cmd.beginRenderPass cb rpinfo Vk.Subpass.ContentsInline do
	ovb <- newIORef Nothing
	drawObject ovb cb ds dsod dstx RenderObject {
		renderObjectPipeline = gpl,
		renderObjectPipelineLayout = lyt,
		renderObjectMesh = vb, renderObjectMeshSize = vn,
		renderObjectTransformMatrix = model fn } ffn 0
	ovbtri <- newIORef Nothing
	for_ [- 20 .. 20] \x -> for_ [- 20 .. 20] \y ->
		drawObject ovbtri cb ds dsod dstx RenderObject {
			renderObjectPipeline = gpl,
			renderObjectPipelineLayout = lyt,
			renderObjectMesh = vbt, renderObjectMeshSize = 3,
			renderObjectTransformMatrix =
				trans x y `Cglm.mat4Mul` scale } ffn
			(((round x + 20) * 41) + (round y + 20) + 1)
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
--				(fromJust $ rgbaDouble 0 0 blue 1) :**
				(fromJust $ rgbaDouble 0 0 0 1) :**
			Vk.ClearValueDepthStencil
				(Vk.ClearDepthStencilValue 1 0) :** HL.Nil }

model :: Float -> Cglm.Mat4
model fn = Cglm.rotate
	Cglm.mat4Identity (fn / 10 * Cglm.rad 1) (Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL)

objectMatrix :: Float -> Float -> Cglm.Mat4
objectMatrix x y = trans x y `Cglm.mat4Mul` scale

trans :: Float -> Float -> Cglm.Mat4
trans x y = Cglm.translate Cglm.mat4Identity (Cglm.Vec3 $ x :. 0 :. y :. NilL)

scale :: Cglm.Mat4
scale = Cglm.scale Cglm.mat4Identity (Cglm.Vec3 $ 0.2 :. 0.2 :. 0.2 :. NilL)

drawObject ::
	IORef (Maybe (Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""])) ->
	Vk.CBffr.C scb ->
	Vk.DscSet.D sds '(sdlyt, Buffers) ->
	Vk.DscSet.D sds' '(sdlytod, ObjDataBuffers) ->
	Vk.DscSet.D sds'' '(sfoo, Foo) ->
	RenderObject sg sl sdlyt sdlytod sfoo sm sb nm -> Word32 -> Word32 -> IO ()
drawObject ovb cb0 ds dsod dstx RenderObject {
	renderObjectPipeline = gpl,
	renderObjectPipelineLayout = lyt,
	renderObjectMesh = vb, renderObjectMeshSize = vn,
	renderObjectTransformMatrix = mdl } ffn i =
	Vk.Cmd.bindPipelineGraphics cb0 Vk.Ppl.BindPointGraphics gpl \cb -> do
	Vk.Cmd.bindDescriptorSetsGraphics cb Vk.Ppl.BindPointGraphics lyt
		(U2 ds :** U2 dsod :** U2 dstx :** HL.Nil) $
		(HL.Nil :** (Vk.Cmd.DynamicIndex ffn :** HL.Nil) :** HL.Nil) :**
		(HL.Nil :** HL.Nil) :** (HL.Nil :** HL.Nil) :** HL.Nil
	readIORef ovb >>= \case
		Just o | vb == o -> pure ()
		_ -> do	Vk.Cmd.bindVertexBuffers cb . HL.Singleton
				. U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @Vertex @"" vb
			writeIORef ovb $ Just vb
	Vk.Cmd.pushConstantsGraphics @'[ 'Vk.T.ShaderStageVertexBit] cb lyt
		$ HL.Id (Str.G.Wrap MeshPushConstants {
			meshPushConstantsData =
				Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
			meshPushConstantsRenderMatrix = mdl }) :** HL.Nil
	Vk.Cmd.draw cb vn 1 0 i

data RenderObject sg sl sdlyt sdlytod sfoo sm sb nm = RenderObject {
	renderObjectPipeline :: Vk.Ppl.Grph.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[ '(sdlyt, Buffers), '(sdlytod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConstants]),
	renderObjectPipelineLayout ::
		Vk.Ppl.Lyt.L sl
			'[ '(sdlyt, Buffers), '(sdlytod, ObjDataBuffers), '(sfoo, Foo)]
			'[WMeshPushConstants],
	renderObjectMesh :: Vk.Bffr.Binded sm sb nm '[Obj.List 256 Vertex ""],
	renderObjectMeshSize :: Word32,
	renderObjectTransformMatrix :: Cglm.Mat4 }

-- VERTEX

data Vertex = Vertex {
	vertexPos :: Position,
	vertexNormal :: Normal,
	vertexColor :: Color,
	vertexUv :: Uv } deriving (Show, Generic)

newtype Position = Position Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Normal = Normal Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Color = Color Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Uv = Uv Cglm.Vec2
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

instance SizeAlignmentList Vertex
instance SizeAlignmentListUntil Position Vertex
instance SizeAlignmentListUntil Normal Vertex
instance SizeAlignmentListUntil Color Vertex
instance SizeAlignmentListUntil Uv Vertex
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
		vertexColor = Color . Cglm.Vec3 $ v :. w :. u :. NilL,
		vertexUv = Uv . Cglm.Vec2 $ 0 :. 0 :. NilL }

posTxtNormalToVertex :: Str.G.Wrap Wv.PositionTxtNormal -> Vertex
posTxtNormalToVertex (Wv.W (Wv.PositionTxtNormal
	(Wv.W (Wv.Position x y z)) (Wv.W (Wv.TexCoord p q)) (Wv.W (Wv.Normal v w u)))) =
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ x :. y :. z :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ v :. w :. u :. NilL,
		vertexColor = Color . Cglm.Vec3 $ v :. w :. u :. NilL,
		vertexUv = Uv . Cglm.Vec2 $ p :. (1 - q) :. NilL }

triangle :: V.Vector Vertex
triangle = V.fromList [
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ 1 :. 1 :. 0.5 :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vertexColor = Color . Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL,
		vertexUv = Uv . Cglm.Vec2 $ 0 :. 0 :. NilL },
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ (- 1) :. 1 :. 0.5 :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vertexColor = Color . Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL,
		vertexUv = Uv . Cglm.Vec2 $ 0 :. 0 :. NilL },
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ 0 :. (- 1) :. 0.5 :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vertexColor = Color . Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL,
		vertexUv = Uv . Cglm.Vec2 $ 0 :. 0 :. NilL } ]

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
	(Cglm.Vec3 $ 0 :. 20 :. 20 :. NilL)
	(Cglm.Vec3 $ 0 :. 10 :. 0 :. NilL)
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

-- OBJECT DATA

newtype ObjData = ObjData {
	objectDataModelMatrix :: Cglm.Mat4 } deriving (Show, Generic)

instance Storable ObjData where
	sizeOf = Str.G.gSizeOf; alignment = Str.G.gAlignment
	peek = Str.G.gPeek; poke = Str.G.gPoke

instance Str.G.G ObjData
instance SizeAlignmentList ObjData
	
-- OTHER TYPES

type FramebufferResized = IORef Bool

-- IMMEDIATE SUBMIT

data UploadContext sf scp scb = UploadContext {
	uploadContextFence :: Vk.Fnc.F sf,
	uploadContextCommandPool :: Vk.CmdPl.C scp,
	uploadContextCommandBuffer :: Vk.CBffr.C scb }

vulkanEngineInitSyncStructures ::
	Vk.Dvc.D sd -> (forall sf . Vk.Fnc.F sf -> IO a) -> IO a
vulkanEngineInitSyncStructures dv f = do
	let	uploadFenceCreateInfo :: Vk.Fnc.CreateInfo 'Nothing
		uploadFenceCreateInfo = Vk.Fnc.CreateInfo {
			Vk.Fnc.createInfoNext = TMaybe.N,
			Vk.Fnc.createInfoFlags = zeroBits }
	Vk.Fnc.create dv uploadFenceCreateInfo nil' f

commandBufferBeginInfo :: Vk.CBffr.UsageFlags -> Vk.CBffr.BeginInfo 'Nothing 'Nothing
commandBufferBeginInfo flags = Vk.CBffr.BeginInfo {
	Vk.CBffr.beginInfoNext = TMaybe.N,
	Vk.CBffr.beginInfoFlags = flags,
	Vk.CBffr.beginInfoInheritanceInfo = Nothing }

uploadContextSubmitInfo :: Vk.CBffr.C scb -> Vk.SubmitInfo 'Nothing '[] '[scb] '[]
uploadContextSubmitInfo cmd = Vk.SubmitInfo {
	Vk.submitInfoNext = TMaybe.N,
	Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Nil,
	Vk.submitInfoCommandBuffers = HL.Singleton cmd,
	Vk.submitInfoSignalSemaphores = HL.Nil }

uploadContextCreateFence ::
	Vk.Dvc.D sd -> (forall sf . Vk.Fnc.F sf -> IO a) -> IO a
uploadContextCreateFence dv = Vk.Fnc.create @'Nothing dv def nil'

uploadContextCommandPoolCreateInfo :: QueueFamilyIndices -> Vk.CmdPl.CreateInfo 'Nothing
uploadContextCommandPoolCreateInfo qfis = Vk.CmdPl.CreateInfo {
	Vk.CmdPl.createInfoNext = TMaybe.N,
	Vk.CmdPl.createInfoFlags = zeroBits,
	Vk.CmdPl.createInfoQueueFamilyIndex = graphicsFamily qfis }

uploadContextCommandBufferAllocateInfo ::
	Vk.CmdPl.C scp -> Vk.CBffr.AllocateInfo 'Nothing scp '[ '()]
uploadContextCommandBufferAllocateInfo cp = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cp,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

createUploadContext ::
	Vk.Dvc.D sd -> QueueFamilyIndices ->
	(forall sf scp scb . UploadContext sf scp scb -> IO a) -> IO a
createUploadContext dv qfis f =
	uploadContextCreateFence dv \fnc ->
	Vk.CmdPl.create dv
		(uploadContextCommandPoolCreateInfo qfis) nil' \cp ->
	Vk.CBffr.allocate dv
		(uploadContextCommandBufferAllocateInfo cp) \(cb :*. HL.Nil) ->
	f UploadContext {
		uploadContextFence = fnc,
		uploadContextCommandPool = cp,
		uploadContextCommandBuffer = cb }

immediateSubmit :: Vk.Dvc.D sd -> Vk.Q.Q ->
	UploadContext sf scp scb -> (Vk.CBffr.C scb -> IO a) -> IO a
immediateSubmit dv gq uctxt f =
	let	cmd = uploadContextCommandBuffer uctxt
		cmdBeginInfo =
			commandBufferBeginInfo Vk.CBffr.UsageOneTimeSubmitBit in
	Vk.CBffr.begin cmd cmdBeginInfo (f cmd) >>= \rt ->
	let	submit = uploadContextSubmitInfo cmd
		fnc = uploadContextFence uctxt in
	Vk.Q.submit gq (HL.Singleton $ U4 submit) (Just fnc) >>
	Vk.Fnc.waitForFs dv (HL.Singleton fnc) True Nothing >>
	Vk.Fnc.resetFs dv (HL.Singleton fnc) >>
	Vk.CmdPl.reset dv (uploadContextCommandPool uctxt) zeroBits >>
	pure rt

-- TEXTURE

createTextureImage ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> FilePath -> (
		forall si sm .
		Vk.Img.Binded sm si nm 'Vk.T.FormatR8g8b8a8Srgb -> IO a ) ->
	IO a
createTextureImage pd dv gq cp fp f =
	readRgba8 fp >>= \img ->
	print (V.length $ imageData img) >>
	let	wdt, hgt :: Integral n => n
		wdt = fromIntegral $ imageWidth img
		hgt = fromIntegral $ imageHeight img in
	createImage' @_ @'Vk.T.FormatR8g8b8a8Srgb
		pd dv wdt hgt Vk.Img.TilingOptimal
		(Vk.Img.UsageTransferDstBit .|.  Vk.Img.UsageSampledBit)
		Vk.Mm.PropertyDeviceLocalBit \tximg _txmem -> do
	createBufferImage @MyImage @_ pd dv (wdt, wdt, hgt, 1)
		Vk.Bffr.UsageTransferSrcBit
		(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)
		\(sb :: Vk.Bffr.Binded
			sm sb "texture-buffer" '[ Obj.ObjImage 1 a inm]) sbm ->
		Vk.Dvc.Mem.write @"texture-buffer" @(Obj.ObjImage 1 MyImage inm)
			dv sbm zeroBits (MyImage img) >>
		print sb >>
		transitionImageLayout dv gq cp tximg Vk.Img.LayoutUndefined
			Vk.Img.LayoutTransferDstOptimal >>
		copyBufferToImage dv gq cp sb tximg wdt hgt >>
		transitionImageLayout dv gq cp tximg
			Vk.Img.LayoutTransferDstOptimal
			Vk.Img.LayoutShaderReadOnlyOptimal
	f tximg

createTextureImageView :: Vk.T.FormatToValue ifmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	(forall siv . Vk.ImgVw.I nm ifmt siv -> IO a) -> IO a
createTextureImageView dv timg f =
	Vk.ImgVw.createNew dv (textureImageViewCreateInfo timg) nil' f

textureImageViewCreateInfo ::
	Vk.Img.Binded sm si nm ifmt -> Vk.ImgVw.CreateInfoNew 'Nothing sm si nm ifmt ifmt
textureImageViewCreateInfo timg = Vk.ImgVw.CreateInfoNew {
	Vk.ImgVw.createInfoNextNew = TMaybe.N,
	Vk.ImgVw.createInfoFlagsNew = zeroBits,
	Vk.ImgVw.createInfoImageNew = timg,
	Vk.ImgVw.createInfoViewTypeNew = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponentsNew = components,
	Vk.ImgVw.createInfoSubresourceRangeNew = srr }
	where
	components = Vk.Component.Mapping {
		Vk.Component.mappingR = def, Vk.Component.mappingG = def,
		Vk.Component.mappingB = def, Vk.Component.mappingA = def }
	srr = Vk.Img.M.SubresourceRange {
		Vk.Img.M.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.M.subresourceRangeBaseMipLevel = 0,
		Vk.Img.M.subresourceRangeLevelCount = 1,
		Vk.Img.M.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.M.subresourceRangeLayerCount = 1 }

createTextureSampler ::
	Vk.Dvc.D sd -> (forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTextureSampler dv = Vk.Smplr.create dv textureSamplerCreateInfo nil'

textureSamplerCreateInfo :: Vk.Smplr.M.CreateInfo 'Nothing
textureSamplerCreateInfo = Vk.Smplr.M.CreateInfo {
	Vk.Smplr.M.createInfoNext = TMaybe.N,
	Vk.Smplr.M.createInfoFlags = zeroBits,
	Vk.Smplr.M.createInfoMagFilter = Vk.FilterNearest,
	Vk.Smplr.M.createInfoMinFilter = Vk.FilterNearest,
	Vk.Smplr.M.createInfoMipmapMode = Vk.Smplr.MipmapModeLinear,
	Vk.Smplr.M.createInfoAddressModeU = Vk.Smplr.AddressModeRepeat,
	Vk.Smplr.M.createInfoAddressModeV = Vk.Smplr.AddressModeRepeat,
	Vk.Smplr.M.createInfoAddressModeW = Vk.Smplr.AddressModeRepeat,
	Vk.Smplr.M.createInfoMipLodBias = 0,
	Vk.Smplr.M.createInfoAnisotropyEnable = False,
	Vk.Smplr.M.createInfoMaxAnisotropy = 0,
	Vk.Smplr.M.createInfoCompareEnable = False,
	Vk.Smplr.M.createInfoCompareOp = Vk.CompareOpAlways,
	Vk.Smplr.M.createInfoMinLod = 0,
	Vk.Smplr.M.createInfoMaxLod = 0,
	Vk.Smplr.M.createInfoBorderColor = Vk.BorderColorIntOpaqueBlack,
	Vk.Smplr.M.createInfoUnnormalizedCoordinates = False }

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

createImage' :: forall nm fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P ->
	Vk.Dvc.D sd -> Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mm.PropertyFlagBits -> (forall si sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Dvc.Mem.M sm
			'[ '(si, 'Vk.Mm.K.Image nm fmt) ] ->
		IO a) -> IO a
createImage' pd dvc wdt hgt tlng usg prps f =
	Vk.Img.createNew @'Nothing
		dvc (imageInfo ext tlng usg) nil' \img -> do
	reqs <- Vk.Img.getMemoryRequirementsNew dvc img
	print reqs
	mt <- findMemoryType pd (Vk.Mm.M.requirementsMemoryTypeBits reqs) prps
	print mt
	Vk.Dvc.Mem.allocateBind @'Nothing dvc
		(HL.Singleton . U2 $ Vk.Dvc.Mem.Image img) (memInfo mt)
		nil' \(HL.Singleton (U2 (Vk.Dvc.Mem.ImageBinded bnd))) m -> do
		f bnd m
	where
	ext = Vk.Extent2d {
		Vk.extent2dWidth = wdt, Vk.extent2dHeight = hgt }
	memInfo mt = Vk.Dvc.Mem.AllocateInfo {
		Vk.Dvc.Mem.allocateInfoNext = TMaybe.N,
		Vk.Dvc.Mem.allocateInfoMemoryTypeIndex = mt }

createBufferImage :: Storable (KObj.IsImagePixel t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (Int, Int, Int, Int) ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ Obj.ObjImage 1 t inm] ->
		Vk.Dvc.Mem.M sm '[ '(
			sb,
			'Vk.Mm.K.Buffer nm '[ Obj.ObjImage 1 t inm])] ->
		IO a) -> IO a
createBufferImage p dv (r, w, h, d) usg props =
	createBuffer p dv (HL.Singleton $ Obj.ObjectLengthImage r w h d) usg props

copyBufferToImage :: forall sd sc sm sb nm img inm si sm' nm' .
	Storable (KObj.IsImagePixel img) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[ Obj.ObjImage 1 img inm]  ->
--	Vk.Img.Binded sm' si nm' (Vk.Bffr.ImageFormat img) ->
	Vk.Img.Binded sm' si nm' (KObj.ImageFormat img) ->
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
	Vk.Cmd.copyBufferToImage @1
		cb bf img Vk.Img.LayoutTransferDstOptimal (HL.Singleton region)

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

#version 460

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inColor;
layout(location = 3) in vec2 vTexCoord;

layout(location = 0) out vec3 outColor;
layout(location = 1) out vec2 texCoord;

layout (set = 0, binding = 0) uniform CameraBuffer {
	mat4 view; mat4 proj; mat4 viewproj; } cameraData;

struct ObjectData { mat4 model; };

layout (std140, set = 1, binding = 0) readonly buffer ObjectBuffer {
	ObjectData objects[];
} objectBuffer;

layout(push_constant) uniform constants {
	vec4 data; mat4 render_matrix; } PushConstants;

void
main()
{
	mat4 modelMatrix = objectBuffer.objects[gl_BaseInstance].model;
	mat4 transformMatrix = cameraData.viewproj * modelMatrix;
	gl_Position = transformMatrix * vec4(inPosition, 1.0);
	outColor = inColor;
	texCoord = vTexCoord;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec3 inColor;
layout(location = 1) in vec2 texCoord;

layout(location = 0) out vec4 outFragColor;

layout (set = 0, binding = 1) uniform SceneData {
	vec4 fogColor; vec4 fogDists;
	vec4 ambientColor;
	vec4 sunlightDir; vec4 sunlightColor; } sceneData;

layout (set = 2, binding = 0) uniform sampler2D tex1;

void
main()
{
	vec3 color = texture(tex1, texCoord).xyz;
//	outFragColor = vec4(texCoord.x, texCoord.y, 0.5f, 1.0f);
	outFragColor = vec4(color, 1.0f);
}

|]
