{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.Generics
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent.STM
import Control.Exception
import Data.Kind
import Data.Foldable
import Data.Traversable
import Gpu.Vulkan.Object qualified as VObj
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Proxy
import Data.Bool
import Data.Maybe
import Data.List qualified as L
import Data.List.Length
import Data.Color

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as Txt

import qualified Gpu.Vulkan.Khr.Surface.Glfw.Window as Vk.Khr.Surface.Glfw.Win
import qualified Gpu.Vulkan.Cglm as Cglm
import qualified Foreign.Storable.Generic

import Graphics.UI.GlfwG as Glfw
import Graphics.UI.GlfwG.Window as Glfw.Win

import ThEnv
import qualified Language.SpirV as SpirV
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH


import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.TypeEnum as Vk.T
import qualified Gpu.Vulkan.Exception as Vk
import qualified Gpu.Vulkan.Instance.Internal as Vk.Ist
import qualified Gpu.Vulkan.Instance as Vk.Ist.M
import qualified Gpu.Vulkan.Khr as Vk.Khr
import qualified Gpu.Vulkan.Ext.DebugUtils as Vk.Ext.DbgUtls
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger as Vk.Ext.DbgUtls.Msngr
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.QueueFamily as Vk.QueueFamily

import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Device as Vk.Dvc.M
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Surface
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Surface.M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Surface.PhysicalDevice
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swapchain
import qualified Gpu.Vulkan.Image as Vk.Image
import qualified Gpu.Vulkan.Image as Vk.Image.M
import qualified Gpu.Vulkan.ImageView as Vk.ImgVw
import qualified Gpu.Vulkan.Component as Vk.Component
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderModule
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShdrSt
import qualified Gpu.Vulkan.Pipeline.InputAssemblyState as Vk.Ppl.InpAsmbSt
import qualified Gpu.Vulkan.Pipeline.ViewportState as Vk.Ppl.ViewportSt
import qualified Gpu.Vulkan.Pipeline.RasterizationState as Vk.Ppl.RstSt
import qualified Gpu.Vulkan.Pipeline.MultisampleState as Vk.Ppl.MltSmplSt
import qualified Gpu.Vulkan.Sample as Vk.Sample
import qualified Gpu.Vulkan.Pipeline.ColorBlendAttachment as Vk.Ppl.ClrBlndAtt
import qualified Gpu.Vulkan.ColorComponent as Vk.ClrCmp
import qualified Gpu.Vulkan.Pipeline.ColorBlendState as Vk.Ppl.ClrBlndSt
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Layout
import qualified Gpu.Vulkan.Attachment as Vk.Att
import qualified Gpu.Vulkan.Subpass as Vk.Subpass
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPass
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPass.M
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Framebuffer as Vk.Frmbffr
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPool
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBffr
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBffr.M
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Memory as Vk.Mem.M
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import Tools

import Gpu.Vulkan.AllocationCallbacks qualified as AllocationCallbacks

main :: IO ()
main = glfwInit $
	createInstance \inst ->
	bool (setupDebugMessenger inst) id enableValidationLayers $
	Vk.Dvc.group nil \dgrp ->

	fromDummy inst dgrp >>= \(phdv, qfis, scfmt, dv, gq, pq, n) ->

	print n >>
	getNum n \(_ :: Proxy n) ->

	Vk.T.formatToType scfmt \(_ :: Proxy scfmt) ->

	run @scfmt @n inst phdv qfis dv gq pq

fromDummy :: Vk.Ist.I si -> Vk.Dvc.Group 'Nothing sd () -> IO (
	Vk.PhDvc.P, QueueFamilyIndices, Vk.Format,
	Vk.Dvc.D sd, Vk.Queue.Q, Vk.Queue.Q, [()] )
fromDummy inst dgrp = withDummySurface inst \dwin dsfc -> do
	(phdv, qfis) <- pickPhysicalDevice' inst dsfc
	spp <- querySwapchainSupport phdv dsfc
	ext <- chooseSwapExtent dwin $ capabilities spp
	let	fmt = Vk.Khr.Surface.M.formatOldFormat
			. chooseSwapSurfaceFormat $ formats spp
	(dv, gq, pq) <- createLogicalDevice' phdv dgrp () qfis
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		n <- getSwapchainImageNum @fmt dv dsfc spp ext qfis
		pure (phdv, qfis, fmt, dv, gq, pq, n)

getSwapchainImageNum :: forall (fmt :: Vk.T.Format) sd ssfc .
	Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd ->
	Vk.Khr.Surface.S ssfc -> SwapChainSupportDetails -> Vk.Extent2d ->
	QueueFamilyIndices -> IO [()]
getSwapchainImageNum dv sfc spp ext qfis =
	withSwapchain @fmt dv sfc spp ext qfis \sc _ ->
		sameNum () <$> Vk.Khr.Swapchain.getImages dv sc

sameNum :: b -> [a] -> [b]
sameNum x = \case [] -> []; _ : ys -> x : sameNum x ys

withSwapchain :: forall scfmt ssfc sd a .
	Vk.T.FormatToValue scfmt =>
	Vk.Dvc.D sd ->
	Vk.Khr.Surface.S ssfc -> SwapChainSupportDetails -> Vk.Extent2d ->
	QueueFamilyIndices ->
	(forall ss . Vk.Khr.Swapchain.S scfmt ss -> Vk.Extent2d -> IO a) -> IO a
withSwapchain dvc sfc spp ext qfis f =
	let	crInfo = mkSwapchainCreateInfo sfc qfis spp ext in
	Vk.Khr.Swapchain.create @_ @scfmt dvc crInfo nil \sc -> f sc ext

getNum :: [a] -> (forall (n :: [()]) . (
	Mappable n ) =>
	Proxy n -> b) -> b
getNum [] f = f (Proxy :: Proxy '[])
getNum (_ : xs) f = getNum xs \(Proxy :: Proxy n) -> f (Proxy :: Proxy ('() ': n))

type FramebufferResized = TVar Bool

newFramebufferResized :: IO FramebufferResized
newFramebufferResized = atomically $ newTVar False

windowName :: String
windowName = "Triangle"

windowSize :: (Int, Int)
windowSize = (width, height) where width = 800; height = 600

enableValidationLayers :: Bool
enableValidationLayers = maybe True (const False) $(lookupCompileEnv "NDEBUG")

validationLayers :: [Vk.LayerName]
validationLayers = [Vk.layerKhronosValidation]

glfwInit :: IO a -> IO a
glfwInit = Glfw.init error

withDummySurface :: Vk.Ist.I si ->
	(forall sw ss . Glfw.Win.W sw -> Vk.Khr.Surface.S ss -> IO a) -> IO a
withDummySurface ist f = withDummyWindow \dwin -> createSurface dwin ist \sfc ->
	f dwin sfc

withDummyWindow :: (forall sw . Glfw.Win.W sw -> IO a) -> IO a
withDummyWindow f = Glfw.Win.group \wgrp -> do
	f =<< withDummyWindow' wgrp ()

withDummyWindow' :: Ord k => Glfw.Win.Group sw k -> k -> IO (Glfw.Win.W sw)
withDummyWindow' wgrp k = do
	Right w <- do
		Glfw.Win.hint $ Glfw.Win.WindowHint'ClientAPI Glfw.Win.ClientAPI'NoAPI
		Glfw.Win.hint $ Glfw.Win.WindowHint'Visible False
		uncurry (Glfw.Win.create' wgrp k)
			windowSize windowName Nothing Nothing
			<* Glfw.Win.hint (Glfw.Win.WindowHint'Visible True)
	pure w

withWindow' :: Ord k => Glfw.Win.Group sw k -> k -> FramebufferResized -> IO (Glfw.Win.W sw)
withWindow' wgrp k frszd = do
	Right w <- do
		Glfw.Win.hint $ Glfw.Win.WindowHint'ClientAPI Glfw.Win.ClientAPI'NoAPI
		uncurry (Glfw.Win.create' wgrp k)
			windowSize windowName Nothing Nothing
	Glfw.Win.setFramebufferSizeCallback
		w (Just $ \_ _ _ -> atomically $ writeTVar frszd True)
	pure w

createInstance :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createInstance f = do
	when enableValidationLayers $ bool
		(error "validation layers requested, but not available!")
		(pure ())
		=<< null . (validationLayers L.\\)
				. (Vk.layerPropertiesLayerName <$>)
			<$> Vk.Ist.M.enumerateLayerProperties
	extensions <- bool id (Vk.Ext.DbgUtls.extensionName :)
			enableValidationLayers . (Vk.Ist.ExtensionName <$>)
		<$> Glfw.getRequiredInstanceExtensions
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
				'Nothing '[] ())) 'Nothing
		createInfo = Vk.Ist.M.CreateInfo {
			Vk.Ist.M.createInfoNext = TMaybe.J debugMessengerCreateInfo,
			Vk.Ist.M.createInfoFlags = def,
			Vk.Ist.M.createInfoApplicationInfo = Just appInfo,
			Vk.Ist.M.createInfoEnabledLayerNames =
				bool [] validationLayers enableValidationLayers,
			Vk.Ist.M.createInfoEnabledExtensionNames = extensions }
	Vk.Ist.create createInfo nil \i -> f i

setupDebugMessenger ::
	Vk.Ist.I si ->
	IO a -> IO a
setupDebugMessenger ist f = Vk.Ext.DbgUtls.Msngr.create ist
	debugMessengerCreateInfo nil f

debugMessengerCreateInfo :: Vk.Ext.DbgUtls.Msngr.CreateInfo 'Nothing '[] ()
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

run :: forall (scfmt :: Vk.T.Format) (n :: [()]) si sd . (
	Vk.T.FormatToValue scfmt, 
	Mappable n ) =>
	Vk.Ist.I si -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> IO ()
run inst phdv qfis dv gq pq =
	createCommandPool qfis dv \cp ->
	createCommandBuffer dv cp \cb ->
	createPipelineLayout dv \ppllyt ->

	Vk.Bffr.group dv nil \bfgrp ->
	Vk.Mem.group dv nil \mmgrp ->
	(for [(0, vertices), (1, vertices2)] \(i, v) -> createVertexBuffer' @Int
		phdv dv gq cp bfgrp mmgrp i v) >>= \vbs ->

	winGroups @_ @_ @_ @scfmt inst dv \(wgs :: WinGroups
		sw si sd ssfc sr sg sl sias srfs siff scifmt ssc siv nm sf k) ->

	atomically (newTVar @Int 0) >>= \wnum ->
	let	crw = do
			wn <- atomically $ readTVar wnum <* modifyTVar wnum (+ 1)
			createWindowResourcesWinGroups @_ @n
				inst phdv dv qfis ppllyt wgs wn in

	mainLoop @n @siv @sf phdv qfis dv gq pq cb ppllyt (cycle vbs) crw

winGroups :: Vk.Ist.I si -> Vk.Dvc.D sd -> (
	forall sw ssfc sr sg sias srfs siff ssc siv sf .
	WinGroups sw si sd ssfc sr sg sl sias srfs siff scifmt ssc siv nm sf k ->
	IO a) -> IO a
winGroups inst dv f =
	Glfw.Win.group \wgrp ->
	Vk.Khr.Surface.group inst nil \sfcgrp ->
	Vk.RndrPass.group dv nil \rpgrp ->
	Vk.Ppl.Graphics.group dv nil \gpsgrp ->
	Vk.Semaphore.group dv nil \iasgrp ->
	Vk.Semaphore.group dv nil \rfsgrp ->
	Vk.Fence.group dv nil \iffgrp ->
	Vk.Khr.Swapchain.group dv nil \scgrp ->
	Vk.ImgVw.group dv nil
		\(ivgrp :: Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm scifmt) ->
	Vk.Frmbffr.group dv nil
		\(fbgrp :: Vk.Frmbffr.Group sd 'Nothing sf (k, Int)) ->
	f $ WinGroups
		wgrp sfcgrp rpgrp gpsgrp iasgrp rfsgrp iffgrp scgrp ivgrp fbgrp

data WinGroups sw si sd ssfc sr sg sl sias srfs siff scifmt ssc siv nm sf k =
	WinGroups
		(Glfw.Win.Group sw k) (Vk.Khr.Surface.Group si 'Nothing ssfc k)
		(Vk.RndrPass.Group sd 'Nothing sr k)
		(Vk.Ppl.Graphics.Group sd 'Nothing sg k '[ '(
			'[ '(Vertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)],
			'(sl, '[], '[]) )] )
		(Vk.Semaphore.Group sd 'Nothing sias k)
		(Vk.Semaphore.Group sd 'Nothing srfs k)
		(Vk.Fence.Group sd 'Nothing siff k)
		(Vk.Khr.Swapchain.Group sd 'Nothing scifmt ssc k)
		(Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm scifmt)
		(Vk.Frmbffr.Group sd 'Nothing sf (k, Int))

createWindowResourcesWinGroups ::
	forall (scifmt :: Vk.T.Format) (n :: [()])
		si siv sd sl sw nm ssfc sr sg sias srfs siff ssc sf k . (
	Ord k, Vk.T.FormatToValue scifmt,
	Mappable n ) =>
	Vk.Ist.I si -> Vk.PhDvc.P -> Vk.Dvc.D sd ->
	QueueFamilyIndices -> Vk.Ppl.Layout.P sl '[] '[] ->
	WinGroups sw si sd ssfc sr sg sl sias srfs siff scifmt ssc siv nm sf k ->
	k -> IO (WinParams
		sw sl nm ssfc sr sg sias srfs siff scifmt ssc
		(Replicate n siv) (Replicate n sf))
createWindowResourcesWinGroups inst phdv dv qfis ppllyt
	(WinGroups
		wgrp sfcgrp rpgrp gpsgrp iasgrp rfsgrp iffgrp scgrp ivgrp fbgrp)
	k = createWindowResources @_ @n
		inst phdv dv qfis ppllyt wgrp sfcgrp rpgrp gpsgrp iasgrp rfsgrp iffgrp
		scgrp ivgrp fbgrp k

createWindowResources ::
	forall (scifmt :: Vk.T.Format) (n :: [()])
		si siv sd sl sw nm ssfc sr sg sias srfs siff ssc sf k . (
	Ord k, Vk.T.FormatToValue scifmt,
	Mappable n
	) =>
	Vk.Ist.I si -> Vk.PhDvc.P -> Vk.Dvc.D sd ->
	QueueFamilyIndices -> Vk.Ppl.Layout.P sl '[] '[] ->
	Glfw.Win.Group sw k -> Vk.Khr.Surface.Group si 'Nothing ssfc k ->
	Vk.RndrPass.Group sd 'Nothing sr k ->
	Vk.Ppl.Graphics.Group sd 'Nothing sg k '[ '(
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)],
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)],
		'(sl, '[], '[]) )] ->
	Vk.Semaphore.Group sd 'Nothing sias k ->
	Vk.Semaphore.Group sd 'Nothing srfs k ->
	Vk.Fence.Group sd 'Nothing siff k ->
	Vk.Khr.Swapchain.Group sd 'Nothing scifmt ssc k ->
	Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm scifmt ->
	Vk.Frmbffr.Group sd 'Nothing sf (k, Int) ->
	k -> IO (WinParams
		sw sl nm ssfc sr sg sias srfs siff scifmt ssc
		(Replicate n siv) (Replicate n sf))
createWindowResources
	inst phdv dv qfis ppllyt wgrp sfcgrp rpgrp gpsgrp iasgrp rfsgrp iffgrp
	scgrp ivgrp fbgrp k =

	newFramebufferResized >>= \g ->
	withWindow' wgrp k g >>= \w ->
	createSurface' w inst sfcgrp k >>= \sfc ->
	prepareSwapchain @scifmt w sfc phdv >>= \(spp, ext) ->
	createRenderPass' @scifmt rpgrp k >>= \rp ->
	createGraphicsPipeline' gpsgrp k ext rp ppllyt >>= \gpl ->
	createSyncObjects' iasgrp rfsgrp iffgrp k >>= \sos ->
	createSwapchain scgrp k sfc spp ext qfis >>= \(sc, _) ->
	Vk.Khr.Swapchain.getImages dv sc >>= \imgs ->
	createImageViews''' @n ivgrp k imgs >>= \scivs ->
	createFramebuffers @n @k @sd @sf @sr @nm @_ @siv
		fbgrp k ext rp scivs >>= \fbs ->

	atomically (newTVar ext) >>= \vext ->
	pure $ WinParams w g sfc vext rp gpl sos sc scivs fbs

createSurface :: Glfw.Win.W sw -> Vk.Ist.I si ->
	(forall ss . Vk.Khr.Surface.S ss -> IO a) -> IO a
createSurface win ist f =
	Vk.Khr.Surface.group ist nil \sgrp ->
	f . fromRight =<< Vk.Khr.Surface.Glfw.Win.create' sgrp () win

fromRight :: Either String r -> r
fromRight = \case Right r -> r; Left emsg -> error $ "fromRight: not Right: " ++ emsg

createSurface' :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Glfw.Win.W sw -> Vk.Ist.I si -> Vk.Khr.Surface.Group si ma ss k -> k ->
	IO (Vk.Khr.Surface.S ss)
createSurface' win ist sgrp k =
	fromRight <$> Vk.Khr.Surface.Glfw.Win.create' sgrp k win

pickPhysicalDevice' :: Vk.Ist.I si ->
	Vk.Khr.Surface.S ssfc -> IO (Vk.PhDvc.P, QueueFamilyIndices)
pickPhysicalDevice' ist dsfc = do
	dvcs <- Vk.PhDvc.enumerate ist
	when (null dvcs) $ error "failed to find GPUs with Gpu.Vulkan support!"
	mpdvc <- findDevice (`isDeviceSuitable` dsfc) dvcs
	case mpdvc of
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
	then (<$> querySwapchainSupport phdvc sfc) \spp ->
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
		graphicsFamilyMaybe = fst <$> L.find
			(checkBits Vk.Queue.GraphicsBit
				. Vk.QueueFamily.propertiesQueueFlags . snd)
			queueFamilies,
		presentFamilyMaybe = listToMaybe pfis }

checkDeviceExtensionSupport :: Vk.PhDvc.P -> IO Bool
checkDeviceExtensionSupport dvc =
	null . (deviceExtensions L.\\) . (Vk.PhDvc.extensionPropertiesExtensionName <$>)
		<$> Vk.PhDvc.enumerateExtensionProperties dvc Nothing

deviceExtensions :: [Vk.PhDvc.ExtensionName]
deviceExtensions = [Vk.Khr.Swapchain.extensionName]

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.Khr.Surface.M.Capabilities,
	formats :: [Vk.Khr.Surface.M.FormatOld],
	presentModes :: [Vk.Khr.PresentMode] }
	deriving Show

querySwapchainSupport ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO SwapChainSupportDetails
querySwapchainSupport dvc sfc = SwapChainSupportDetails
	<$> Vk.Khr.Surface.PhysicalDevice.getCapabilities dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getFormatsOld dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getPresentModes dvc sfc

createLogicalDevice' :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Vk.PhDvc.P -> Vk.Dvc.Group ma sd k -> k -> QueueFamilyIndices ->
	IO (Vk.Dvc.D sd, Vk.Queue.Q, Vk.Queue.Q)
createLogicalDevice' phd dgrp k qfis =
	mkHeteroParList queueCreateInfos uniqueQueueFamilies \qs ->
	Vk.Dvc.create' phd dgrp k (createInfo qs) >>= \(fromRight -> dvc) -> do
		gq <- Vk.Dvc.getQueue dvc (graphicsFamily qfis) 0
		pq <- Vk.Dvc.getQueue dvc (presentFamily qfis) 0
		pure (dvc, gq, pq)
	where
	uniqueQueueFamilies =
		L.nub [graphicsFamily qfis, presentFamily qfis]
	queueCreateInfos qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }
	createInfo qs = Vk.Dvc.M.CreateInfo {
		Vk.Dvc.M.createInfoNext = TMaybe.N,
		Vk.Dvc.M.createInfoFlags = def,
		Vk.Dvc.M.createInfoQueueCreateInfos = qs,
		Vk.Dvc.M.createInfoEnabledLayerNames =
			bool [] validationLayers enableValidationLayers,
		Vk.Dvc.M.createInfoEnabledExtensionNames = deviceExtensions,
		Vk.Dvc.M.createInfoEnabledFeatures = Just def }

mkHeteroParList :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] ->
	(forall ss . HeteroParList.ToListWithCM' WithPoked TMaybe.M ss => HeteroParList.PL t ss -> b) -> b
mkHeteroParList _k [] f = f HeteroParList.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

prepareSwapchain :: forall (scfmt :: Vk.T.Format) sw ssfc .
	Vk.T.FormatToValue scfmt =>
	Glfw.Win.W sw -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	IO (SwapChainSupportDetails, Vk.Extent2d)
prepareSwapchain win sfc phdvc = do
	spp <- querySwapchainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	fmt0 = Vk.T.formatToValue @scfmt
		fmt = Vk.Khr.Surface.M.formatOldFormat
			. chooseSwapSurfaceFormat $ formats spp
	when (fmt0 /= fmt) $ error
		"app/try-multiple-windows: prepareSwapchain format not match"
	pure (spp, ext)

createSwapchain :: forall scfmt ssfc sd ma ss k .
	(Ord k, Vk.T.FormatToValue scfmt, AllocationCallbacks.ToMiddle ma) =>
	Vk.Khr.Swapchain.Group sd ma scfmt ss k -> k ->
	Vk.Khr.Surface.S ssfc -> SwapChainSupportDetails -> Vk.Extent2d ->
	QueueFamilyIndices ->
	IO (Vk.Khr.Swapchain.S scfmt ss, Vk.Extent2d)
createSwapchain scgrp k sfc spp ext qfis =
	let	crInfo = mkSwapchainCreateInfo sfc qfis spp ext in
	Vk.Khr.Swapchain.create' @scfmt scgrp k crInfo >>= \(fromRight -> sc) -> pure (sc, ext)

mkSwapchainCreateInfo :: Vk.Khr.Surface.S ss -> QueueFamilyIndices ->
	SwapChainSupportDetails -> Vk.Extent2d ->
	Vk.Khr.Swapchain.CreateInfo 'Nothing ss fmt
mkSwapchainCreateInfo sfc qfis0 spp ext =
	Vk.Khr.Swapchain.CreateInfo {
		Vk.Khr.Swapchain.createInfoNext = TMaybe.N,
		Vk.Khr.Swapchain.createInfoFlags = def,
		Vk.Khr.Swapchain.createInfoSurface = sfc,
		Vk.Khr.Swapchain.createInfoMinImageCount = imgc,
		Vk.Khr.Swapchain.createInfoImageColorSpace =
			Vk.Khr.Surface.M.formatOldColorSpace fmt,
		Vk.Khr.Swapchain.createInfoImageExtent = ext,
		Vk.Khr.Swapchain.createInfoImageArrayLayers = 1,
		Vk.Khr.Swapchain.createInfoImageUsage =
			Vk.Image.UsageColorAttachmentBit,
		Vk.Khr.Swapchain.createInfoImageSharingMode = ism,
		Vk.Khr.Swapchain.createInfoQueueFamilyIndices = qfis,
		Vk.Khr.Swapchain.createInfoPreTransform =
			Vk.Khr.Surface.M.capabilitiesCurrentTransform caps,
		Vk.Khr.Swapchain.createInfoCompositeAlpha =
			Vk.Khr.CompositeAlphaOpaqueBit,
		Vk.Khr.Swapchain.createInfoPresentMode = presentMode,
		Vk.Khr.Swapchain.createInfoClipped = True,
		Vk.Khr.Swapchain.createInfoOldSwapchain = Nothing }
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

recreateSwapchain :: forall ssfc sd sw ssc fmt . Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Glfw.Win.W sw -> Vk.Khr.Surface.S ssfc ->
	Vk.Khr.Swapchain.S fmt ssc ->
	IO (Vk.Format, Vk.Extent2d)
recreateSwapchain phdvc qfis0 dvc win sfc sc = do
	spp <- querySwapchainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	(crInfo, scifmt) = mkSwapchainCreateInfoRaw @fmt sfc qfis0 spp ext
	(scifmt, ext) <$ Vk.Khr.Swapchain.unsafeRecreate dvc crInfo nil sc

mkSwapchainCreateInfoRaw :: forall fmt ss .
	Vk.Khr.Surface.S ss -> QueueFamilyIndices ->
	SwapChainSupportDetails -> Vk.Extent2d ->
	(Vk.Khr.Swapchain.CreateInfo 'Nothing ss fmt, Vk.Format)
mkSwapchainCreateInfoRaw sfc qfis0 spp ext = (
	Vk.Khr.Swapchain.CreateInfo {
		Vk.Khr.Swapchain.createInfoNext = TMaybe.N,
		Vk.Khr.Swapchain.createInfoFlags = def,
		Vk.Khr.Swapchain.createInfoSurface = sfc,
		Vk.Khr.Swapchain.createInfoMinImageCount = imgc,
		Vk.Khr.Swapchain.createInfoImageColorSpace =
			Vk.Khr.Surface.M.formatOldColorSpace fmt,
		Vk.Khr.Swapchain.createInfoImageExtent = ext,
		Vk.Khr.Swapchain.createInfoImageArrayLayers = 1,
		Vk.Khr.Swapchain.createInfoImageUsage =
			Vk.Image.UsageColorAttachmentBit,
		Vk.Khr.Swapchain.createInfoImageSharingMode = ism,
		Vk.Khr.Swapchain.createInfoQueueFamilyIndices = qfis,
		Vk.Khr.Swapchain.createInfoPreTransform =
			Vk.Khr.Surface.M.capabilitiesCurrentTransform caps,
		Vk.Khr.Swapchain.createInfoCompositeAlpha =
			Vk.Khr.CompositeAlphaOpaqueBit,
		Vk.Khr.Swapchain.createInfoPresentMode = presentMode,
		Vk.Khr.Swapchain.createInfoClipped = True,
		Vk.Khr.Swapchain.createInfoOldSwapchain = Nothing }, scifmt )
	where
	fmt = chooseSwapSurfaceFormat $ formats spp
	scifmt = Vk.Khr.Surface.M.formatOldFormat fmt
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

chooseSwapSurfaceFormat  :: [Vk.Khr.Surface.M.FormatOld] -> Vk.Khr.Surface.M.FormatOld
chooseSwapSurfaceFormat = \case
	availableFormats@(af0 : _) -> fromMaybe af0
		$ L.find preferredSwapSurfaceFormat availableFormats
	_ -> error "no available swap surface formats"

preferredSwapSurfaceFormat :: Vk.Khr.Surface.M.FormatOld -> Bool
preferredSwapSurfaceFormat f =
	Vk.Khr.Surface.M.formatOldFormat f == Vk.FormatB8g8r8a8Srgb &&
	Vk.Khr.Surface.M.formatOldColorSpace f == Vk.Khr.ColorSpaceSrgbNonlinear

chooseSwapPresentMode :: [Vk.Khr.PresentMode] -> Vk.Khr.PresentMode
chooseSwapPresentMode =
	fromMaybe Vk.Khr.PresentModeFifo . L.find (== Vk.Khr.PresentModeMailbox)

chooseSwapExtent :: Glfw.Win.W sw -> Vk.Khr.Surface.M.Capabilities -> IO Vk.Extent2d
chooseSwapExtent w caps
	| Vk.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> wdt, fromIntegral -> h) <-
			Glfw.Win.getFramebufferSize w
		pure $ Vk.Extent2d
			(clamp wdt (Vk.extent2dWidth n) (Vk.extent2dHeight n))
			(clamp h (Vk.extent2dWidth x) (Vk.extent2dHeight x))
	where
	curExt = Vk.Khr.Surface.M.capabilitiesCurrentExtent caps
	n = Vk.Khr.Surface.M.capabilitiesMinImageExtent caps
	x = Vk.Khr.Surface.M.capabilitiesMaxImageExtent caps

recreateImageViews :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Image.Binded ss ss nm scfmt] -> HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis -> IO ()
recreateImageViews _dvc [] HeteroParList.Nil = pure ()
recreateImageViews dvc (sci : scis) (iv :** ivs) =
	Vk.ImgVw.unsafeRecreate dvc (mkImageViewCreateInfo sci) nil iv >>
	recreateImageViews dvc scis ivs
recreateImageViews _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

createImageViews''' :: forall n ivfmt sd si siv k sm nm ifmt . (
	Ord k, Vk.T.FormatToValue ivfmt, Mappable n ) =>
	Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm ivfmt -> k ->
	[Vk.Image.Binded sm si nm ifmt] ->
	IO (HeteroParList.PL (Vk.ImgVw.I nm ivfmt) (Replicate n siv))
createImageViews''' ivgrp k imgs = do
	ivs <- createImageViews' ivgrp k imgs
	pure $ homoListFromList @_ @n ivs

createImageViews' :: forall ivfmt sd si siv k sm nm ifmt .
	(Ord k, Vk.T.FormatToValue ivfmt) =>
	Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm ivfmt -> k ->
	[Vk.Image.Binded sm si nm ifmt] -> IO [Vk.ImgVw.I nm ivfmt siv]
createImageViews' ivgrp k imgs =
	mapM (\(i, img) -> createImageView' ivgrp (k, i) img) $ zip [0 ..] imgs

createImageView' :: forall ivfmt sd si siv k sm nm ifmt . Ord k =>
	Vk.T.FormatToValue ivfmt =>
	Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm ivfmt -> (k, Int) ->
	Vk.Image.Binded sm si nm ifmt -> IO (Vk.ImgVw.I nm ivfmt siv)
createImageView' ivgrp k timg =
	fromRight <$> Vk.ImgVw.create' ivgrp k (mkImageViewCreateInfo timg)

mkImageViewCreateInfo ::
	Vk.Image.Binded sm si nm ifmt ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt ivfmt
mkImageViewCreateInfo sci = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = Vk.ImgVw.CreateFlagsZero,
	Vk.ImgVw.createInfoImage = sci,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = components,
	Vk.ImgVw.createInfoSubresourceRange = subresourceRange }
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

createRenderPass' ::
	forall (scifmt :: Vk.T.Format) sd ma sr k . (
	Ord k, AllocationCallbacks.ToMiddle ma, Vk.T.FormatToValue scifmt ) =>
	Vk.RndrPass.Group sd ma sr k -> k ->  IO (Vk.RndrPass.R sr)
createRenderPass' rpgrp k =
	fromRight <$> Vk.RndrPass.create' @_ @_ @'[scifmt] rpgrp k renderPassInfo
	where
	renderPassInfo = Vk.RndrPass.M.CreateInfo {
		Vk.RndrPass.M.createInfoNext = TMaybe.N,
		Vk.RndrPass.M.createInfoFlags = zeroBits,
		Vk.RndrPass.M.createInfoAttachments =
			colorAttachment :** HeteroParList.Nil,
		Vk.RndrPass.M.createInfoSubpasses = [subpass],
		Vk.RndrPass.M.createInfoDependencies = [dependency] }
	colorAttachment :: Vk.Att.Description scifmt
	colorAttachment = Vk.Att.Description {
		Vk.Att.descriptionFlags = zeroBits,
		Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
		Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
		Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
		Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
		Vk.Att.descriptionStencilStoreOp = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionInitialLayout = Vk.Image.LayoutUndefined,
		Vk.Att.descriptionFinalLayout = Vk.Image.LayoutPresentSrcKhr }
	subpass = Vk.Subpass.Description {
		Vk.Subpass.descriptionFlags = zeroBits,
		Vk.Subpass.descriptionPipelineBindPoint =
			Vk.Ppl.BindPointGraphics,
		Vk.Subpass.descriptionInputAttachments = [],
		Vk.Subpass.descriptionColorAndResolveAttachments =
			Left [colorAttachmentRef],
		Vk.Subpass.descriptionDepthStencilAttachment = Nothing,
		Vk.Subpass.descriptionPreserveAttachments = [] }
	colorAttachmentRef = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 0,
		Vk.Att.referenceLayout = Vk.Image.LayoutColorAttachmentOptimal }
	dependency = Vk.Subpass.Dependency {
		Vk.Subpass.dependencySrcSubpass = Vk.Subpass.SExternal,
		Vk.Subpass.dependencyDstSubpass = 0,
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

createPipelineLayout ::
	Vk.Dvc.D sd -> (forall sl . Vk.Ppl.Layout.P sl '[] '[] -> IO b) -> IO b
createPipelineLayout dvc f = do
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = TMaybe.N,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts = HeteroParList.Nil }
	Vk.Ppl.Layout.create @'Nothing @_ @_ @'[] dvc pipelineLayoutInfo nil f

createGraphicsPipeline' :: (AllocationCallbacks.ToMiddle ma, Ord k) =>
	Vk.Ppl.Graphics.Group sd ma sg k '[ '(
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)],
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)],
		'(sl, '[], '[]) )] -> k ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[] '[] ->
	IO (Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]))
createGraphicsPipeline' gpsgrp k sce rp ppllyt =
	(\(U3 gpl :** HeteroParList.Nil) -> gpl) . fromRight <$>
	Vk.Ppl.Graphics.createGs' gpsgrp k Nothing
		(U14 pplInfo :** HeteroParList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo sce rp ppllyt

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]) -> IO ()
recreateGraphicsPipeline dvc sce rp ppllyt gpls = Vk.Ppl.Graphics.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HeteroParList.Nil) nil (U3 gpls :** HeteroParList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo sce rp ppllyt

mkGraphicsPipelineCreateInfo ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[] '[] ->
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

shaderStages :: HeteroParList.PL (U5 Vk.Ppl.ShdrSt.CreateInfo) '[
	'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
	'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
shaderStages = U5 vertShaderStageInfo :** U5 fragShaderStageInfo :** HeteroParList.Nil
	where
	vertShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModule = (
			shaderModuleCreateInfo glslVertexShaderMain, nil ),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	fragShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModule = (
			shaderModuleCreateInfo glslFragmentShaderMain, nil ),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }

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

createFramebuffers ::
	forall ts k sd sf sr nm fmt siv .
	(Mappable ts, Ord k) =>
	Vk.Frmbffr.Group sd 'Nothing sf (k, Int) -> k ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	HeteroParList.PL (Vk.ImgVw.I nm fmt) (Replicate ts siv) ->
	IO (HeteroParList.PL Vk.Frmbffr.F (Replicate ts sf))
createFramebuffers fbgrp k sce rp =
	mapHomoListMWithI @_ @ts @_ @_ @siv 0 \i sciv ->
	fromRight <$> Vk.Frmbffr.create'
		fbgrp (k, i) (mkFramebufferCreateInfo sce rp sciv)

recreateFramebuffers :: forall ts sd sr nm fmt siv sf .
	Mappable ts =>
	Vk.Dvc.D sd -> Vk.Extent2d ->
	Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.I nm fmt) (Replicate ts siv) ->
	HeteroParList.PL Vk.Frmbffr.F (Replicate ts sf) -> IO ()
recreateFramebuffers dvc sce rp =
	zipWithHomoListM_ @_ @ts @_ @_ @siv @_ @sf \sciv fb ->
	Vk.Frmbffr.unsafeRecreate dvc (mkFramebufferCreateInfo sce rp sciv) nil fb

class Mappable (ts :: [knd]) where
	type Replicate ts s :: [Type]
	homoListFromList :: [t s] -> HeteroParList.PL t (Replicate ts s)
	mapHomoListMWithI :: Monad m => Int -> (Int -> t a -> m (t' b)) ->
		HeteroParList.PL t (Replicate ts a) ->
		m (HeteroParList.PL t' (Replicate ts b))
	zipWithHomoListM_ :: Monad m => (t a -> t' b -> m c) ->
		HeteroParList.PL t (Replicate ts a) ->
		HeteroParList.PL t' (Replicate ts b) -> m ()

instance Mappable '[] where
	type Replicate '[] s = '[]
	homoListFromList [] = HeteroParList.Nil
	homoListFromList _ = error "bad"
	mapHomoListMWithI _ _ HeteroParList.Nil = pure HeteroParList.Nil
	zipWithHomoListM_ _ HeteroParList.Nil HeteroParList.Nil = pure ()

instance Mappable ts => Mappable (t ': ts) where
	type Replicate (t ': ts) s = s ': Replicate ts s
	homoListFromList (x : xs) = x :** (homoListFromList @_ @ts xs)
	homoListFromList _ = error "bad"
	mapHomoListMWithI :: forall t' a t'' b m .
		Monad m => Int -> (Int -> t' a -> m (t'' b)) ->
		HeteroParList.PL t' (Replicate (t ': ts) a) ->
		m (HeteroParList.PL t'' (Replicate (t ': ts) b))
	mapHomoListMWithI i f (x :** xs) = (:**) <$> f i x
		<*> mapHomoListMWithI @_ @ts @_ @_ @a @_ @b (i + 1) f xs
	zipWithHomoListM_ f (x :** xs) (y :** ys) =
		f x y >> zipWithHomoListM_ @_ @ts f xs ys

mkFramebufferCreateInfo ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.ImgVw.I nm fmt si ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(nm, fmt, si)]
mkFramebufferCreateInfo sce rp attch = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = U3 attch :** HeteroParList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where
	Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = sce

createCommandPool :: QueueFamilyIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPool.C sc -> IO a) -> IO a
createCommandPool qfis dvc f =
	Vk.CmdPool.create dvc poolInfo nil \cp -> f cp
	where poolInfo = Vk.CmdPool.CreateInfo {
		Vk.CmdPool.createInfoNext = TMaybe.N,
		Vk.CmdPool.createInfoFlags =
			Vk.CmdPool.CreateResetCommandBufferBit,
		Vk.CmdPool.createInfoQueueFamilyIndex = graphicsFamily qfis }

createVertexBuffer' :: Ord k => Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[VObj.List 256 Vertex ""] ->
	Vk.Mem.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 Vertex ""])] ->
	k -> [Vertex] -> IO (Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""])
createVertexBuffer' phdvc dvc gq cp bfgrp mmgrp k vs = do
	(b, _) <- createBufferList' phdvc dvc bfgrp mmgrp k (fromIntegral $ length vs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit
	createBufferList phdvc dvc (fromIntegral $ length vs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
		\(b' :: Vk.Bffr.Binded sm sb
			"vertex-buffer" '[VObj.List 256 t ""]) bm' -> do
		Vk.Mem.write @"vertex-buffer" @(VObj.List 256 Vertex "") dvc bm' zeroBits vs
		copyBuffer dvc gq cp b' b
	pure b

createBufferList :: forall sd nm t a . Storable t =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Dvc.M.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 t ""] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[VObj.List 256 t ""] ) ] ->
		IO a) -> IO a
createBufferList p dv ln usg props f =
	Vk.Bffr.group dv nil \bfgrp ->
	Vk.Mem.group dv nil \mmgrp ->
	uncurry f =<< createBufferList' p dv bfgrp mmgrp () ln usg props

createBufferList' :: forall sd sb sm k nm t .  (Ord k, Storable t) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[VObj.List 256 t ""] ->
	Vk.Mem.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 t ""])] ->
	k -> Vk.Dvc.M.Size ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 t ""],
		Vk.Mem.M sm
			'[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 t ""])] )
createBufferList' p dv bfgrp mmgrp k ln usg props =
	createBuffer p dv bfgrp mmgrp k (VObj.LengthList ln) usg props

createBuffer :: forall sd sb sm k nm o . (Ord k, VObj.SizeAlignment o) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[o] ->
	Vk.Mem.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mem.BufferArg nm '[o])] ->
	k -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[o],
		Vk.Mem.M sm '[ '(sb, 'Vk.Mem.BufferArg nm '[o])] )
createBuffer p dv bfgrp mmgrp k ln usg props =
	Vk.Bffr.create' bfgrp k bffrInfo >>= \(fromRight -> b) -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	(fromRight ->
		(HeteroParList.Singleton (U2 (Vk.Mem.BufferBinded bnd)), mm)) <-
		Vk.Mem.allocateBind' mmgrp k
			(HeteroParList.Singleton . U2 $ Vk.Mem.Buffer b)
			(allcInfo mt)
	pure (bnd, mm)
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HeteroParList.Singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.M.TypeIndex -> Vk.Mem.AllocateInfo 'Nothing
	allcInfo mt = Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

findMemoryType :: Vk.PhDvc.P -> Vk.Mem.M.TypeBits -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.M.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.PhDvc.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> L.find ((&&)
		<$> (`Vk.Mem.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mem.M.mTypePropertyFlags . snd) tps
		where tps = Vk.PhDvc.memoryPropertiesMemoryTypes props1

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
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

createCommandBuffer ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPool.C scp ->
	(forall scb . Vk.CmdBffr.C scb -> IO a) ->
	IO a
createCommandBuffer dvc cp f =
	Vk.CmdBffr.allocate dvc allocInfo $ f . \(cb :*. HeteroParList.Nil) -> cb
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo 'Nothing scp '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }

data SyncObjects (ssos :: (Type, Type, Type)) where
	SyncObjects :: {
		_imageAvailableSemaphores :: Vk.Semaphore.S sias,
		_renderFinishedSemaphores :: Vk.Semaphore.S srfs,
		_inFlightFences :: Vk.Fence.F sfs } ->
		SyncObjects '(sias, srfs, sfs)

createSyncObjects' :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Vk.Semaphore.Group sd ma sias k ->
	Vk.Semaphore.Group sd ma srfs k ->
	Vk.Fence.Group sd ma siff k -> k ->
	IO (SyncObjects '(sias, srfs, siff))
createSyncObjects' iasgrp rfsgrp iffgrp k =
	Vk.Semaphore.create' @_ @Nothing iasgrp k def >>= \(fromRight -> ias) ->
	Vk.Semaphore.create' @_ @Nothing rfsgrp k def >>= \(fromRight -> rfs) ->
	Vk.Fence.create' @_ @'Nothing iffgrp k finfo >>= \(fromRight -> iff) ->
	pure $ SyncObjects ias rfs iff
	where
	finfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sg sm sb nm sl .
	Vk.CmdBffr.C scb  ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf ->
	TVar Vk.Extent2d ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]) ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] -> IO ()
recordCommandBuffer cb rp fb vsce gpl vb =
	atomically (readTVar vsce) >>= \sce' ->
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb (rpInfo sce') Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb
		(HeteroParList.Singleton . U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @Vertex @"" vb) >>
	Vk.Cmd.draw cbb 3 1 0 0
	where
	rpInfo :: Vk.Extent2d -> Vk.RndrPass.BeginInfo 'Nothing sr sf
		'[ 'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32]
	rpInfo e = Vk.RndrPass.BeginInfo {
		Vk.RndrPass.beginInfoNext = TMaybe.N,
		Vk.RndrPass.beginInfoRenderPass = rp,
		Vk.RndrPass.beginInfoFramebuffer = fb,
		Vk.RndrPass.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = e },
		Vk.RndrPass.beginInfoClearValues = HeteroParList.Singleton
			. Vk.ClearValueColor . fromJust $ rgbaDouble 0 0 0 1 }

mainLoop :: forall n siv sf sd scb sl sm sb nm sw ssfc sr sg sias srfs siff fmt ssc . (
	Vk.T.FormatToValue fmt, Mappable n ) =>
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q -> Vk.CmdBffr.C scb ->
	Vk.Ppl.Layout.P sl '[] '[] ->
	[Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""]] ->
	IO (WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc
		(Replicate n siv) (Replicate n sf)) -> IO ()
mainLoop phdvc qfis dvc gq pq cb ppllyt vbs crw = do
	wpss0 <- replicateM 3 crw
	($ wpss0) $ fix \loop wpss -> do
		Glfw.pollEvents
		runLoop @n @siv @sf phdvc qfis dvc gq pq cb ppllyt vbs wpss (loop wpss)
	Vk.Dvc.waitIdle dvc

data WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc ss sfs = WinParams
	(Glfw.Win.W sw) FramebufferResized (Vk.Khr.Surface.S ssfc)
	(TVar Vk.Extent2d) (Vk.RndrPass.R sr)
	(Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]))
	(SyncObjects '(sias, srfs, siff))
	(Vk.Khr.Swapchain.S fmt ssc)
	(HeteroParList.PL (Vk.ImgVw.I nm fmt) ss)
	(HeteroParList.PL Vk.Frmbffr.F sfs)

type Recreates sw sl nm ssfc sr sg fmt ssc sis sfs = (
	Glfw.Win.W sw, Vk.Khr.Surface.S ssfc,
	TVar Vk.Extent2d,
	Vk.RndrPass.R sr,
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]),
	Vk.Khr.Swapchain.S fmt ssc,
	HeteroParList.PL (Vk.ImgVw.I nm fmt) sis,
	HeteroParList.PL Vk.Frmbffr.F sfs )

winParamsToRecreates ::
	WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc sscivs sfs ->
	Recreates sw sl nm ssfc sr sg fmt ssc sscivs sfs
winParamsToRecreates (WinParams w _frszd sfc ex rp gpl _sos sc scivs fb) =
	(w, sfc, ex, rp, gpl, sc, scivs, fb)

type Draws sl sr sg sias srfs siff fmt ssc sfs = (
	TVar Vk.Extent2d, Vk.RndrPass.R sr,
	Vk.Ppl.Graphics.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] '(sl, '[], '[]),
	SyncObjects '(sias, srfs, siff),
	Vk.Khr.Swapchain.S fmt ssc,
	HeteroParList.PL Vk.Frmbffr.F sfs )

winParamsToDraws ::
	WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc sscivs sfs ->
	Draws sl sr sg sias srfs siff fmt ssc sfs
winParamsToDraws (WinParams _w _frszd _sfc ex rp gpl sos sc _scivs fb) =
	(ex, rp, gpl, sos, sc, fb)

winParamsToWindow ::
	WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc sscivs sfs ->
	Glfw.Win.W sw
winParamsToWindow (WinParams w _ _ _ _ _ _ _ _ _) = w

checkFlagWinParams ::
	WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc sscivs sfs ->
	IO Bool
checkFlagWinParams (WinParams _ frszd _ _ _ _ _ _ _ _) = checkFlag frszd

runLoop :: forall n si sf sd scb sl sm sb nm ssfc sr sw sg sias srfs siff fmt ssc . (
	Vk.T.FormatToValue fmt, Mappable n ) =>
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q -> Vk.CmdBffr.C scb ->
	Vk.Ppl.Layout.P sl '[] '[] ->
	[Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""]] ->
	[WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc
		(Replicate n si) (Replicate n sf)] ->
	IO () -> IO ()
runLoop phdvc qfis dvc gq pq cb ppllyt vbs wpss loop = do
	for_ (zip vbs wpss) \(v, w) ->
		drawAndCatch @n @si @sf phdvc qfis dvc gq pq cb v ppllyt w loop
	cls <- or <$> (Glfw.Win.shouldClose . winParamsToWindow) `mapM` wpss
	if cls then (pure ()) else do
		for_ wpss \w -> do
			b <- checkFlagWinParams w
			case b of
				False -> pure ()
				True -> recreateAll' @n @si @sf
					phdvc qfis dvc ppllyt (winParamsToRecreates w)
		loop

drawAndCatch :: forall n (siv :: Type) (sf :: Type) sd sl sw ssfc sr sg sias srfs siff fmt ssc scb sm sb nm .
	(Vk.T.FormatToValue fmt,
	Mappable n) =>
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.CmdBffr.C scb ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Ppl.Layout.P sl '[] '[]  ->
	WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc
		(Replicate n siv) (Replicate n sf) ->
	IO () -> IO ()
drawAndCatch phdvc qfis dvc gq pq cb vb ppllyt wps loop = do
	catchAndRecreate' @n @siv @sf phdvc qfis dvc ppllyt (winParamsToRecreates wps) loop
		$ drawFrame dvc gq pq cb vb (winParamsToDraws wps)
	Vk.Dvc.waitIdle dvc

drawFrame :: forall sfs sd ssc fmt sr sg sm sb nm scb sias srfs siff sl .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> Vk.CmdBffr.C scb ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Draws sl sr sg sias srfs siff fmt ssc sfs -> IO ()
drawFrame dvc gq pq cb vb (vext, rp, gpl, (SyncObjects ias rfs iff), sc, fbs) = do
	let	siff = HeteroParList.Singleton iff
	Vk.Fence.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HeteroParList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb vext gpl vb
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers =
				HeteroParList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HeteroParList.Singleton rfs }
		presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphores = HeteroParList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = HeteroParList.Singleton
				$ Vk.Khr.SwapchainImageIndex sc imgIdx }
	Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfo

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate' :: forall n siv sf fmt sd sl sw nm ssfc sr sg ssc .
	(Mappable n,
	Vk.T.FormatToValue fmt) =>
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Ppl.Layout.P sl '[] '[] ->
	Recreates sw sl nm ssfc sr sg fmt ssc
		(Replicate n siv) (Replicate n sf) ->
	IO () -> IO () -> IO ()
catchAndRecreate' phd qfis dv plyt rcs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just (Left  "VK_ERROR_OUT_OF_DATE_KHR")
		Vk.SuboptimalKhr -> Just (Left "VK_SUBOPTIMAL_KHR")
		_ -> Nothing)
	act
	\mm -> do
		either putStrLn pure mm
		recreateAll' @n @siv @sf @_ @sd @sl @sw @nm @ssfc @sr @sg @ssc phd qfis dv plyt rcs >> loop

recreateAll' :: forall n siv sf fmt sd sl sw nm ssfc sr sg ssc . (
	Mappable n,
	Vk.T.FormatToValue fmt ) =>
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Ppl.Layout.P sl '[] '[] ->
	Recreates sw sl nm ssfc sr sg fmt ssc (Replicate n siv) (Replicate n sf) ->
	IO ()
recreateAll' phdvc qfis dvc ppllyt (w, sfc, vext, rp, gpl, sc, scivs, fbs) = do
	waitFramebufferSize w
	Vk.Dvc.waitIdle dvc

	(scifmt, ext) <- recreateSwapchain phdvc qfis dvc w sfc sc
	atomically $ writeTVar vext ext
	Vk.Khr.Swapchain.getImages dvc sc >>= \imgs ->
		recreateImageViews dvc imgs scivs
	recreateGraphicsPipeline dvc ext rp ppllyt gpl
	recreateFramebuffers @n @sd @sr @nm @fmt @siv @sf dvc ext rp scivs fbs

waitFramebufferSize :: Glfw.Win.W sw -> IO ()
waitFramebufferSize w = Glfw.Win.getFramebufferSize w >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.Win.getFramebufferSize w
	where zero = uncurry (||) . ((== 0) *** (== 0))

data Vertex = Vertex { vertexPos :: Cglm.Vec2, vertexColor :: Cglm.Vec3 }
	deriving (Show, Generic)

instance Storable Vertex where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke


instance Foreign.Storable.Generic.G Vertex where

vertices :: [Vertex]
vertices = [
	Vertex (Cglm.Vec2 $ 0.0 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 1.0 :. NilL),
	Vertex (Cglm.Vec2 $ 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL) ]

vertices2 :: [Vertex]
vertices2 = [
	Vertex (Cglm.Vec2 $ 0.0 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL) ]

shaderModuleCreateInfo :: SpirV.S sknd -> Vk.ShaderModule.CreateInfo 'Nothing sknd
shaderModuleCreateInfo code = Vk.ShaderModule.CreateInfo {
	Vk.ShaderModule.createInfoNext = TMaybe.N,
	Vk.ShaderModule.createInfoFlags = def,
	Vk.ShaderModule.createInfoCode = code }

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
