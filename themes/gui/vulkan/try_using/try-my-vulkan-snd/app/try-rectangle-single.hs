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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.Generics
import Foreign.Storable
import Foreign.Storable.PeekPoke
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

import Data.Function.ToolsYj
import Data.Tuple.ToolsYj
import Data.List.ToolsYj
import Data.Bool.ToolsYj

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
import Data.Time

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as Txt
import qualified Graphics.UI.GLFW as Glfw hiding (createWindowSurface)
import qualified Gpu.Vulkan.Khr.Surface.Glfw as Glfw
import qualified Gpu.Vulkan.Cglm as Cglm
import qualified Foreign.Storable.Generic

import ThEnv
import qualified Language.SpirV as SpirV
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH

import Data.TypeLevel.ParMaybe (nil)

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Exception as Vk
import qualified Gpu.Vulkan.Instance.Internal as Vk.Ist
import qualified Gpu.Vulkan.Instance as Vk.Ist.M
import qualified Gpu.Vulkan.Khr as Vk.Khr
import qualified Gpu.Vulkan.Ext.DebugUtils as Vk.DbgUtls
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger as Vk.DbgUtls.Msngr
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.QueueFamily as Vk.QueueFamily

import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Device as Vk.Dvc.M
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Sfc
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Sfc.M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Sfc.PhysicalDevice
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swpch
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
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Mem.M
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet

import Tools (clampOld, checkBits, checkFlag)

import Gpu.Vulkan.TypeEnum qualified as Vk.T

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win

import Data.Text.ToolsYj

import Debug

main :: IO ()
main = newIORef False >>= \fr -> withWindow fr \w ->
	createIst \ist -> bool id (dbgm ist) debug $ body fr w ist
	where dbgm i = Vk.DbgUtls.Msngr.create i dbgMsngrInfo nil

type FramebufferResized = IORef Bool

withWindow :: FramebufferResized -> (forall s . GlfwG.Win.W s -> IO a) -> IO a
withWindow fr a = GlfwG.init error $ GlfwG.Win.group \g -> a =<< initWindow fr g

initWindow :: FramebufferResized -> GlfwG.Win.Group s () -> IO (GlfwG.Win.W s)
initWindow fr g = do
	Right w <- do
		GlfwG.Win.hint noApi
		uncurryDup (GlfwG.Win.create' g ()) sizeName Nothing Nothing
	w <$ GlfwG.Win.setFramebufferSizeCallback
		w (Just . const3 $ writeIORef fr True)
	where
	noApi = GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI
	sizeName = ((800, 600), "Triangle")

createIst :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createIst f = do
	errorIf emsg . (debug &&) . elemNotAll vldLayers
		. (Vk.layerPropertiesLayerName <$>)
		=<< Vk.Ist.enumerateLayerProperties
	exts <- bool id (Vk.DbgUtls.extensionName :) debug
		. (Vk.Ist.ExtensionName <$>)
		<$> GlfwG.getRequiredInstanceExtensions
	bool	(Vk.Ist.create (info exts) nil f)
		(Vk.Ist.create (infoDbg exts) nil f) debug
	where
	emsg = "validation layers requested, but not available!"
	info exts = Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.N,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo = Just ainfo,
		Vk.Ist.createInfoEnabledLayerNames = [],
		Vk.Ist.createInfoEnabledExtensionNames = exts }
	infoDbg exts = Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.J dbgMsngrInfo,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo = Just ainfo,
		Vk.Ist.createInfoEnabledLayerNames = vldLayers,
		Vk.Ist.createInfoEnabledExtensionNames = exts }
	ainfo = Vk.ApplicationInfo {
		Vk.applicationInfoNext = TMaybe.N,
		Vk.applicationInfoApplicationName = "Hello Triangle",
		Vk.applicationInfoApplicationVersion =
			Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoEngineName = "No Engine",
		Vk.applicationInfoEngineVersion = Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoApiVersion = Vk.apiVersion_1_0 }

vldLayers :: [Vk.LayerName]
vldLayers = [Vk.layerKhronosValidation]

dbgMsngrInfo :: Vk.DbgUtls.Msngr.CreateInfo 'Nothing '[] ()
dbgMsngrInfo = Vk.DbgUtls.Msngr.CreateInfo {
	Vk.DbgUtls.Msngr.createInfoNext = TMaybe.N,
	Vk.DbgUtls.Msngr.createInfoFlags = zeroBits,
	Vk.DbgUtls.Msngr.createInfoMessageSeverity =
		Vk.DbgUtls.MessageSeverityVerboseBit .|.
		Vk.DbgUtls.MessageSeverityWarningBit .|.
		Vk.DbgUtls.MessageSeverityErrorBit,
	Vk.DbgUtls.Msngr.createInfoMessageType =
		Vk.DbgUtls.MessageTypeGeneralBit .|.
		Vk.DbgUtls.MessageTypeValidationBit .|.
		Vk.DbgUtls.MessageTypePerformanceBit,
	Vk.DbgUtls.Msngr.createInfoFnUserCallback = dbgCallback,
	Vk.DbgUtls.Msngr.createInfoUserData = Nothing }
	where dbgCallback _svr _tp cbdt _ud = False <$ Txt.putStrLn (
		"validation layer: " <>
		Vk.DbgUtls.Msngr.callbackDataMessage cbdt )

body :: FramebufferResized -> GlfwG.Win.W s -> Vk.Ist.I si -> IO ()
body fr w ist =
	Vk.Khr.Sfc.Glfw.Win.create ist w nil \sfc ->
	pickPhDvc ist sfc >>= \(pd, qfis) ->
	createLgDvc pd qfis \dv gq pq ->
	createSwpch w sfc pd qfis dv \(sc :: Vk.Khr.Swpch.S scifmt ss) ex ->
	Vk.Khr.Swpch.getImages dv sc >>= \scis -> createImgVws dv scis \scvs ->
	createRndrPss @scifmt dv \rp ->
	createPplLyt dv \dsl pl -> createGrPpl dv ex rp pl \gp ->
	createFrmbffrs dv ex rp scvs \fbs ->
	createCmdPl qfis dv \cp ->
	createVtxBffr pd dv gq cp \vb ->
	createIdxBffr pd dv gq cp \ib ->
	createUfmBffr pd dv \ub ubm ->
	createDscPl dv \dp ->
	createDscSt dv dp ub dsl \ds ->
	createCmdBffr dv cp \cb ->
	createSyncObjs dv \sos ->
	getCurrentTime >>= \tm ->
	mainLoop fr w sfc pd qfis dv gq pq sc ex scvs rp pl gp fbs vb ib ubm ds cb sos tm

pickPhDvc :: Vk.Ist.I si -> Vk.Khr.Sfc.S ss -> IO (Vk.PhDvc.P, QFamIndices)
pickPhDvc ist sfc = Vk.PhDvc.enumerate ist >>= \pds -> do
	when (null pds) $ error "failed to find GPUs with Gpu.Vulkan support!"
	findDevice suit pds >>= \case
		Just pdvc -> pure pdvc
		Nothing -> error "failed to find a suitable GPU!"
	where
	suit pd = espt pd >>= bool (pure Nothing) do
		qfis <- findQFams pd sfc
		(<$> querySwpchSupport pd sfc) \spp ->
			bool (completeQueueFamilies qfis) Nothing
				$ null (formats spp) || null (presentModes spp)
	espt pd = elemAll dvcExtensions
		. (Vk.PhDvc.extensionPropertiesExtensionName <$>)
			<$> Vk.PhDvc.enumerateExtensionProperties pd Nothing

dvcExtensions :: [Vk.PhDvc.ExtensionName]
dvcExtensions = [Vk.Khr.Swpch.extensionName]

findDevice :: Monad m =>
	(Vk.PhDvc.P -> m (Maybe a)) -> [Vk.PhDvc.P] ->
	m (Maybe (Vk.PhDvc.P, a))
findDevice prd = \case
	[] -> pure Nothing
	p : ps -> prd p >>= \case
		Nothing -> findDevice prd ps; Just x -> pure $ Just (p, x)

data QFamIndices = QFamIndices {
	graphicsFamily :: Vk.QueueFamily.Index,
	presentFamily :: Vk.QueueFamily.Index }

data QueueFamilyIndicesMaybe = QueueFamilyIndicesMaybe {
	graphicsFamilyMaybe :: Maybe Vk.QueueFamily.Index,
	presentFamilyMaybe :: Maybe Vk.QueueFamily.Index }

completeQueueFamilies :: QueueFamilyIndicesMaybe -> Maybe QFamIndices
completeQueueFamilies = \case
	QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = Just gf, presentFamilyMaybe = Just pf } ->
		Just QFamIndices {
			graphicsFamily = gf, presentFamily = pf }
	_ -> Nothing

findQFams ::
	Vk.PhDvc.P -> Vk.Khr.Sfc.S ss -> IO QueueFamilyIndicesMaybe
findQFams device sfc = do
	queueFamilies <- Vk.PhDvc.getQueueFamilyProperties device
	pfis <- filterM
		(\i -> Vk.Khr.Sfc.PhysicalDevice.getSupport device i sfc)
		(fst <$> queueFamilies)
	pure QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = fst <$> find
			(checkBits Vk.Queue.GraphicsBit
				. Vk.QueueFamily.propertiesQueueFlags . snd)
			queueFamilies,
		presentFamilyMaybe = listToMaybe pfis }

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.Khr.Sfc.M.Capabilities,
	formats :: [Vk.Khr.Sfc.M.FormatOld],
	presentModes :: [Vk.Khr.PresentMode] }

querySwpchSupport ::
	Vk.PhDvc.P -> Vk.Khr.Sfc.S ss -> IO SwapChainSupportDetails
querySwpchSupport dvc sfc = SwapChainSupportDetails
	<$> Vk.Khr.Sfc.PhysicalDevice.getCapabilities dvc sfc
	<*> Vk.Khr.Sfc.PhysicalDevice.getFormatsOld dvc sfc
	<*> Vk.Khr.Sfc.PhysicalDevice.getPresentModes dvc sfc

createLgDvc :: Vk.PhDvc.P -> QFamIndices -> (forall sd .
		Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> IO a) -> IO a
createLgDvc phdvc qfis f =
	let	uniqueQueueFamilies =
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
--					queueCreateInfos <$> uniqueQueueFamilies,
				Vk.Dvc.M.createInfoEnabledLayerNames =
					bool [] vldLayers debug,
				Vk.Dvc.M.createInfoEnabledExtensionNames =
					dvcExtensions,
				Vk.Dvc.M.createInfoEnabledFeatures = Just def }
		Vk.Dvc.create phdvc createInfo nil \dvc -> do
			gq <- Vk.Dvc.getQueue dvc (graphicsFamily qfis) 0
			pq <- Vk.Dvc.getQueue dvc (presentFamily qfis) 0
			f dvc gq pq

mkHeteroParList :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] ->
	(forall ss . HeteroParList.ToListWithCM' WithPoked TMaybe.M ss => HeteroParList.PL t ss -> b) -> b
mkHeteroParList _k [] f = f HeteroParList.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

createSwpch :: GlfwG.Win.W s -> Vk.Khr.Sfc.S ssfc -> Vk.PhDvc.P ->
	QFamIndices -> Vk.Dvc.D sd ->
	(forall ss scfmt . Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swpch.S scfmt ss -> Vk.Extent2d -> IO a) ->
	IO a
createSwpch (GlfwG.Win.W win) sfc phdvc qfis dvc f = do
	spp <- querySwpchSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	fmt = Vk.Khr.Sfc.M.formatOldFormat
			. chooseSwapSurfaceFormat $ formats spp
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		let	crInfo = mkSwapchainCreateInfoNew sfc qfis spp ext
		Vk.Khr.Swpch.create @'Nothing @fmt dvc crInfo nil
			\sc -> f sc ext

mkSwapchainCreateInfoNew :: Vk.Khr.Sfc.S ss -> QFamIndices ->
	SwapChainSupportDetails -> Vk.Extent2d ->
	Vk.Khr.Swpch.CreateInfo 'Nothing ss fmt
mkSwapchainCreateInfoNew sfc qfis0 spp ext =
	Vk.Khr.Swpch.CreateInfo {
		Vk.Khr.Swpch.createInfoNext = TMaybe.N,
		Vk.Khr.Swpch.createInfoFlags = def,
		Vk.Khr.Swpch.createInfoSurface = sfc,
		Vk.Khr.Swpch.createInfoMinImageCount = imgc,
		Vk.Khr.Swpch.createInfoImageColorSpace =
			Vk.Khr.Sfc.M.formatOldColorSpace fmt,
		Vk.Khr.Swpch.createInfoImageExtent = ext,
		Vk.Khr.Swpch.createInfoImageArrayLayers = 1,
		Vk.Khr.Swpch.createInfoImageUsage =
			Vk.Image.UsageColorAttachmentBit,
		Vk.Khr.Swpch.createInfoImageSharingMode = ism,
		Vk.Khr.Swpch.createInfoQueueFamilyIndices = qfis,
		Vk.Khr.Swpch.createInfoPreTransform =
			Vk.Khr.Sfc.M.capabilitiesCurrentTransform caps,
		Vk.Khr.Swpch.createInfoCompositeAlpha =
			Vk.Khr.CompositeAlphaOpaqueBit,
		Vk.Khr.Swpch.createInfoPresentMode = presentMode,
		Vk.Khr.Swpch.createInfoClipped = True,
		Vk.Khr.Swpch.createInfoOldSwapchain = Nothing }
	where
	fmt = chooseSwapSurfaceFormat $ formats spp
	presentMode = chooseSwapPresentMode $ presentModes spp
	caps = capabilities spp
	maxImgc = fromMaybe maxBound . onlyIf (> 0)
		$ Vk.Khr.Sfc.M.capabilitiesMaxImageCount caps
	imgc = clampOld
		(Vk.Khr.Sfc.M.capabilitiesMinImageCount caps + 1) 0 maxImgc
	(ism, qfis) = bool
		(Vk.SharingModeConcurrent,
			[graphicsFamily qfis0, presentFamily qfis0])
		(Vk.SharingModeExclusive, [])
		(graphicsFamily qfis0 == presentFamily qfis0)

recreateSwapChain :: Vk.T.FormatToValue scfmt =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.PhDvc.P ->
	QFamIndices -> Vk.Dvc.D sd -> Vk.Khr.Swpch.S scfmt ssc ->
	IO Vk.Extent2d
recreateSwapChain win sfc phdvc qfis0 dvc sc = do
	spp <- querySwpchSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	crInfo = mkSwapchainCreateInfoNew sfc qfis0 spp ext
	ext <$ Vk.Khr.Swpch.unsafeRecreate @'Nothing dvc crInfo nil sc

chooseSwapSurfaceFormat  :: [Vk.Khr.Sfc.M.FormatOld] -> Vk.Khr.Sfc.M.FormatOld
chooseSwapSurfaceFormat = \case
	availableFormats@(af0 : _) -> fromMaybe af0
		$ find preferredSwapSurfaceFormat availableFormats
	_ -> error "no available swap surface formats"

preferredSwapSurfaceFormat :: Vk.Khr.Sfc.M.FormatOld -> Bool
preferredSwapSurfaceFormat f =
	Vk.Khr.Sfc.M.formatOldFormat f == Vk.FormatB8g8r8a8Srgb &&
	Vk.Khr.Sfc.M.formatOldColorSpace f == Vk.Khr.ColorSpaceSrgbNonlinear

chooseSwapPresentMode :: [Vk.Khr.PresentMode] -> Vk.Khr.PresentMode
chooseSwapPresentMode =
	fromMaybe Vk.Khr.PresentModeFifo . find (== Vk.Khr.PresentModeMailbox)

chooseSwapExtent :: Glfw.Window -> Vk.Khr.Sfc.M.Capabilities -> IO Vk.Extent2d
chooseSwapExtent win caps
	| Vk.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> w, fromIntegral -> h) <-
			Glfw.getFramebufferSize win
		pure $ Vk.Extent2d
			(clampOld w (Vk.extent2dWidth n) (Vk.extent2dHeight n))
			(clampOld h (Vk.extent2dWidth x) (Vk.extent2dHeight x))
	where
	curExt = Vk.Khr.Sfc.M.capabilitiesCurrentExtent caps
	n = Vk.Khr.Sfc.M.capabilitiesMinImageExtent caps
	x = Vk.Khr.Sfc.M.capabilitiesMaxImageExtent caps

createImgVws :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Image.Binded ss ss nm fmt] ->
	(forall si . HeteroParList.PL (Vk.ImgVw.I nm fmt) si -> IO a) -> IO a
createImgVws _dvc [] f = f HeteroParList.Nil
createImgVws dvc (sci : scis) f =
	createImageView dvc sci \sciv ->
	createImgVws dvc scis \scivs -> f $ sciv :** scivs

recreateImageViewsNew :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Image.Binded ss ss nm scfmt] -> HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis -> IO ()
recreateImageViewsNew _dvc [] HeteroParList.Nil = pure ()
recreateImageViewsNew dvc (sci : scis) (iv :** ivs) =
	Vk.ImgVw.unsafeRecreate dvc (mkImageViewCreateInfoNew sci) nil iv >>
	recreateImageViewsNew dvc scis ivs
recreateImageViewsNew _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

createImageView :: forall ivfmt sd si sm nm ifmt a .
	Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Image.Binded sm si nm ifmt ->
	(forall siv . Vk.ImgVw.I nm ivfmt siv -> IO a) -> IO a
createImageView dvc timg f =
	Vk.ImgVw.create dvc (mkImageViewCreateInfoNew timg) nil f

mkImageViewCreateInfoNew ::
	Vk.Image.Binded sm si nm ifmt ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt ivfmt
mkImageViewCreateInfoNew sci = Vk.ImgVw.CreateInfo {
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

createRndrPss ::
	forall (scifmt :: Vk.T.Format) sd a . Vk.T.FormatToValue scifmt =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPass.R sr -> IO a) -> IO a
createRndrPss dvc f = do
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
				Vk.Image.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Image.LayoutPresentSrcKhr }
		colorAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 0,
			Vk.Att.referenceLayout =
				Vk.Image.LayoutColorAttachmentOptimal }
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
		renderPassInfo = Vk.RndrPass.M.CreateInfo {
			Vk.RndrPass.M.createInfoNext = TMaybe.N,
			Vk.RndrPass.M.createInfoFlags = zeroBits,
			Vk.RndrPass.M.createInfoAttachments = colorAttachment :** HeteroParList.Nil,
			Vk.RndrPass.M.createInfoSubpasses = [subpass],
			Vk.RndrPass.M.createInfoDependencies = [dependency] }
	Vk.RndrPass.create @'Nothing @'[scifmt] dvc renderPassInfo nil \rp -> f rp

type AtomUbo s = '(s, '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing]])

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall s .
	Vk.DscSetLyt.D s '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing]]
	-> IO a) -> IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing
		'[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = uboLayoutBinding :** HeteroParList.Nil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing])
	uboLayoutBinding = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

createPplLyt ::
	Vk.Dvc.D sd -> (forall sdsl sl .
		Vk.DscSetLyt.D sdsl
			'[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing]] ->
		Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] -> IO b) -> IO b
createPplLyt dvc f =
	createDescriptorSetLayout dvc \dsl ->
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = TMaybe.N,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts =
				HeteroParList.Singleton $ U2 dsl } in
	Vk.Ppl.Layout.create @'Nothing @_ @_ @'[] dvc pipelineLayoutInfo nil $ f dsl

createGrPpl :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) -> IO a) -> IO a
createGrPpl dvc sce rp ppllyt f =
	Vk.Ppl.Graphics.createGs dvc Nothing (U14 pplInfo :** HeteroParList.Nil)
			nil \(U3 gpl :** HeteroParList.Nil) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt

recreateGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) -> IO ()
recreateGraphicsPipeline' dvc sce rp ppllyt gpls = Vk.Ppl.Graphics.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HeteroParList.Nil) nil (U3 gpls :** HeteroParList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt

mkGraphicsPipelineCreateInfo' ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing, '[ '(Vertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing '(sl, '[AtomUbo sdsl], '[]) sr '(sb, vs', ts', foo)
mkGraphicsPipelineCreateInfo' sce rp ppllyt = Vk.Ppl.Graphics.CreateInfo {
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
	Vk.Ppl.RstSt.createInfoFrontFace = Vk.FrontFaceCounterClockwise,
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

createFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d ->
	Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.I nm fmt) sis ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HeteroParList.Nil f = f HeteroParList.Nil
createFrmbffrs dvc sce rp (iv :** ivs) f =
	Vk.Frmbffr.create dvc (mkFramebufferCreateInfoNew sce rp iv) nil \fb ->
	createFrmbffrs dvc sce rp ivs \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.I nm fmt) sis ->
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HeteroParList.Nil HeteroParList.Nil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dvc sce rp (sciv :** scivs) (fb :** fbs) =
		Vk.Frmbffr.unsafeRecreate dvc
			(mkFramebufferCreateInfoNew sce rp sciv) nil fb >>
		recreateFramebuffers dvc sce rp scivs fbs

mkFramebufferCreateInfoNew ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.ImgVw.I nm fmt si ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(nm, fmt, si)]
mkFramebufferCreateInfoNew sce rp attch = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = U3 attch :** HeteroParList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where
	Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = sce

createCmdPl :: QFamIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPool.C sc -> IO a) -> IO a
createCmdPl qfis dvc f =
	Vk.CmdPool.create dvc poolInfo nil \cp -> f cp
	where poolInfo = Vk.CmdPool.CreateInfo {
		Vk.CmdPool.createInfoNext = TMaybe.N,
		Vk.CmdPool.createInfoFlags =
			Vk.CmdPool.CreateResetCommandBufferBit,
		Vk.CmdPool.createInfoQueueFamilyIndex = graphicsFamily qfis }

createVtxBffr :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] -> IO a ) -> IO a
createVtxBffr phdvc dvc gq cp f =
	createBufferList' phdvc dvc (fromIntegral $ length vertices)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList' phdvc dvc (fromIntegral $ length vertices)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit ) \(b' :: Vk.Bffr.Binded sm sb "vertex-buffer" '[VObj.List 256 t ""]) bm' -> do
	Vk.Mem.write @"vertex-buffer" @(VObj.List 256 Vertex "") dvc bm' zeroBits vertices
	copyBuffer dvc gq cp b' b
	f b

createIdxBffr :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 Word16 ""] -> IO a) -> IO a
createIdxBffr phdvc dvc gq cp f =
	createBufferList' phdvc dvc (fromIntegral $ length indices)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageIndexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList' phdvc dvc (fromIntegral $ length indices)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit ) \(b' :: Vk.Bffr.Binded sm sb "vertex-buffer" '[VObj.List 256 t ""]) bm' -> do
	Vk.Mem.write @"vertex-buffer" @(VObj.List 256 Word16 "") dvc bm' zeroBits indices
	copyBuffer dvc gq cp b' b
	f b

createUfmBffr :: Vk.PhDvc.P -> Vk.Dvc.D sd -> (forall sm sb .
		Vk.Bffr.Binded sm sb "uniform-buffer" '[VObj.Atom 256 UniformBufferObject 'Nothing]  ->
		UniformBufferMemory sm sb ->
		IO b) -> IO b
createUfmBffr phdvc dvc = createBufferAtom' phdvc dvc
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mem.PropertyHostVisibleBit .|. Vk.Mem.PropertyHostCoherentBit)

type UniformBufferMemory sm sb = Vk.Mem.M sm '[ '(
	sb,
	'Vk.Mem.BufferArg "uniform-buffer" '[VObj.Atom 256 UniformBufferObject 'Nothing]
	)]

createDscPl ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDscPl dvc = Vk.DscPool.create dvc poolInfo nil
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = TMaybe.N,
		Vk.DscPool.createInfoFlags =
			Vk.DscPool.CreateFreeDescriptorSetBit,
		Vk.DscPool.createInfoMaxSets = 1,
		Vk.DscPool.createInfoPoolSizes = [poolSize] }
	poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = 1 }

createDscSt ::
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 UniformBufferObject 'Nothing] ->
	Vk.DscSetLyt.D sdsc '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing]] ->
	(forall sds .
		Vk.DscSet.D sds '(sdsc, '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 UniformBufferObject 'Nothing]]) -> IO a) -> IO a
createDscSt dvc dscp ub dscslyt f =
	Vk.DscSet.allocateDs dvc allocInfo \(HeteroParList.Singleton dscs) -> do
	Vk.DscSet.updateDs dvc
		(HeteroParList.Singleton . U5 $ descriptorWrite ub dscs) HeteroParList.Nil
	f dscs
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts =
			HeteroParList.Singleton $ U2 dscslyt }

descriptorWrite ::
	Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 UniformBufferObject 'Nothing] ->
	Vk.DscSet.D sds slbts ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, VObj.Atom 256 UniformBufferObject 'Nothing)]) 0
descriptorWrite ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HeteroParList.Singleton bufferInfo }
	where bufferInfo = U4 $ Vk.Dsc.BufferInfo ub

createBufferAtom' :: forall sd nm a b . Storable a => Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 a 'Nothing] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[VObj.Atom 256 a 'Nothing] )] ->
			IO b) -> IO b
createBufferAtom' p dv usg props = createBuffer' p dv VObj.LengthAtom usg props

createBufferList' :: forall sd nm t a . Storable t =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Dvc.M.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 t ""] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[VObj.List 256 t ""] ) ] ->
		IO a) ->
	IO a
createBufferList' p dv ln usg props =
	createBuffer' p dv (VObj.LengthList ln) usg props

createBuffer' :: forall sd nm o a . VObj.SizeAlignment o =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mem.M sm
			'[ '(sb, 'Vk.Mem.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer' p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Mem.allocateBind dv (HeteroParList.Singleton . U2 $ Vk.Mem.Buffer b)
		(allcInfo mt) nil
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
	suitable props1 = fst <$> find ((&&)
		<$> (`Vk.Mem.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mem.M.mTypePropertyFlags . snd) tps
		where tps = Vk.PhDvc.memoryPropertiesMemoryTypes props1

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a . Storable' a =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 a ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 a ""] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CmdBffr.allocate
		dvc allocInfo \(cb :*. HeteroParList.Nil) -> do
		let	submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks =
					HeteroParList.Nil,
				Vk.submitInfoCommandBuffers =
					HeteroParList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
		Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo do
			Vk.Cmd.copyBuffer @'[ '[VObj.List 256 a ""]] cb src dst
		Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) Nothing
		Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo 'Nothing sc '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }

createCmdBffr ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPool.C scp ->
	(forall scb . Vk.CmdBffr.C scb -> IO a) ->
	IO a
createCmdBffr dvc cp f = Vk.CmdBffr.allocate dvc allocInfo $ f . \(cb :*. HeteroParList.Nil) -> cb
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo 'Nothing scp '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }

addTypeToProxy ::
	Proxy vss -> Proxy ('[ '(Vertex, 'Vk.VtxInp.RateVertex)] ': vss)
addTypeToProxy Proxy = Proxy

data SyncObjects (ssos :: (Type, Type, Type)) where
	SyncObjects :: {
		imageAvailableSemaphores :: Vk.Semaphore.S sias,
		renderFinishedSemaphores :: Vk.Semaphore.S srfs,
		inFlightFences :: Vk.Fence.F sfs } ->
		SyncObjects '(sias, srfs, sfs)

createSyncObjs ::
	Vk.Dvc.D sd -> (forall sias srfs siff . SyncObjects '(sias, srfs, siff) -> IO a ) -> IO a
createSyncObjs dvc f =
	Vk.Semaphore.create @'Nothing dvc def nil \ias ->
	Vk.Semaphore.create @'Nothing dvc def nil \rfs ->
	Vk.Fence.create @'Nothing dvc fncInfo nil \iff ->
	f $ SyncObjects ias rfs iff
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sl sg sm sb nm sm' sb' nm' sdsl sds .
	Vk.CmdBffr.C scb ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] '(sl, '[AtomUbo sdsl], '[]) ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	IO ()
recordCommandBuffer cb rp fb sce ppllyt gpl vb ib ubds =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb
		(HeteroParList.Singleton . U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @Vertex @"" vb) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word16 @"" ib) >>
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics ppllyt
		(HeteroParList.Singleton $ U2 ubds)
		(HeteroParList.Singleton (
			HeteroParList.Nil :**
			HeteroParList.Nil )) >>
	Vk.Cmd.drawIndexed cbb (fromIntegral $ length indices) 1 0 0 0
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

mainLoop :: (RecreateFramebuffers ss sfs, Vk.T.FormatToValue scfmt) =>
	FramebufferResized ->
	GlfwG.Win.W s -> Vk.Khr.Sfc.S ssfc ->
	Vk.PhDvc.P -> QFamIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d -> HeteroParList.PL (Vk.ImgVw.I nm scfmt) ss ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] -> Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	Vk.CmdBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> UTCTime -> IO ()
mainLoop g (GlfwG.Win.W w) sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm0 = do
	($ ext0) $ fix \loop ext -> do
		Glfw.pollEvents
		tm <- getCurrentTime
		runLoop w sfc phdvc qfis dvc gq pq
			sc g ext scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs
			(realToFrac $ tm `diffUTCTime` tm0) loop
	Vk.Dvc.waitIdle dvc

runLoop :: (RecreateFramebuffers sis sfs, Vk.T.FormatToValue scfmt) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.PhDvc.P ->
	QFamIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> FramebufferResized -> Vk.Extent2d ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	Vk.CmdBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> Float ->
	(Vk.Extent2d -> IO ()) -> IO ()
runLoop win sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm loop = do
	catchAndRecreate win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop
		$ drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm
	cls <- Glfw.windowShouldClose win
	if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateSwapChainEtc
			win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs)

drawFrame :: forall sfs sd ssc sr sl sg sm sb nm sm' sb' nm' sm2 sb2 scb sias srfs siff sdsl scfmt sds .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	Vk.CmdBffr.C scb -> SyncObjects '(sias, srfs, siff) -> Float -> IO ()
drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib ubm ubds cb (SyncObjects ias rfs iff) tm = do
	let	siff = HeteroParList.Singleton iff
	Vk.Fence.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HeteroParList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext ppllyt gpl vb ib ubds
	updateUniformBuffer dvc ubm ext tm
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HeteroParList.Singleton rfs }
		presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphores = HeteroParList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = HeteroParList.Singleton
				$ Vk.Khr.SwapchainImageIndex sc imgIdx }
	Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfo

updateUniformBuffer :: Vk.Dvc.D sd ->
	UniformBufferMemory sm2 sb2 -> Vk.Extent2d -> Float -> IO ()
updateUniformBuffer dvc um sce tm = do
	Vk.Mem.write @"uniform-buffer" @(VObj.Atom 256 UniformBufferObject 'Nothing)
		dvc um zeroBits ubo
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

catchAndRecreate :: (RecreateFramebuffers sis sfs, Vk.T.FormatToValue scfmt) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc ->
	Vk.PhDvc.P -> QFamIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
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
	RecreateFramebuffers sis sfs, Vk.T.FormatToValue scfmt ) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc ->
	Vk.PhDvc.P -> QFamIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUbo sdsl], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateSwapChainEtc win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwapChain win sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swpch.getImages dvc sc >>= \imgs ->
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


instance Foreign.Storable.Generic.G Vertex where

vertices :: [Vertex]
vertices = [
	Vertex (Cglm.Vec2 $ (- 0.5) :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ 0.5 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Cglm.Vec2 $ (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 1.0 :. NilL) ]

indices :: [Word16]
indices = [0, 1, 2, 2, 3, 0]

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

instance Foreign.Storable.Generic.G UniformBufferObject

shaderModuleCreateInfo :: SpirV.S sknd -> Vk.ShaderModule.CreateInfo 'Nothing sknd
shaderModuleCreateInfo code = Vk.ShaderModule.CreateInfo {
	Vk.ShaderModule.createInfoNext = TMaybe.N,
	Vk.ShaderModule.createInfoFlags = def,
	Vk.ShaderModule.createInfoCode = code }

[glslVertexShader|

#version 450

layout(binding = 0) uniform UniformBufferObject {
	mat4 model;
	mat4 view;
	mat4 proj;
} ubo;

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec3 inColor;

layout(location = 0) out vec3 fragColor;

void
main()
{
	gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 0.0, 1.0);
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
