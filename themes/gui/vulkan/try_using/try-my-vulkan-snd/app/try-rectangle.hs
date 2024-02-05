{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.Generics
import GHC.TypeNats
import GHC.TypeLits (Symbol)
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
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List qualified as TLength
import Data.TypeLevel.List qualified as TpLvlLst
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.Proxy
import Data.Bool
import Data.Maybe
import Data.Maybe.ToolsYj
import Data.List
import Data.IORef
import Data.List.Length
import Data.Word
import Data.Color
import Data.Time

import Data.List.NonEmpty qualified as NE
import Data.Text.IO qualified as Txt
import Graphics.UI.GLFW qualified as Glfw hiding (createWindowSurface)
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Cglm qualified as Cglm
import qualified Foreign.Storable.Generic

import Language.SpirV qualified as SpirV
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH

import Data.TypeLevel.ParMaybe (nil)

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Exception qualified as Vk
import Gpu.Vulkan.Instance qualified as Vk.Ist
import Gpu.Vulkan.Khr qualified as Vk.Khr
import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.DbgUtls.Msngr
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.QueueFamily qualified as Vk.QueueFamily
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam

import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc.M
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as 
	Vk.Khr.Sfc.PhysicalDevice
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch
import Gpu.Vulkan.Image qualified as Vk.Image
import Gpu.Vulkan.Image qualified as Vk.Image.M
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Component qualified as Vk.Component
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderModule
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.InputAssemblyState qualified as Vk.Ppl.InpAsmbSt
import Gpu.Vulkan.Pipeline.ViewportState qualified as Vk.Ppl.ViewportSt
import Gpu.Vulkan.Pipeline.RasterizationState qualified as Vk.Ppl.RstSt
import Gpu.Vulkan.Pipeline.MultisampleState qualified as Vk.Ppl.MltSmplSt
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.Pipeline.ColorBlendAttachment qualified as Vk.Ppl.ClrBlndAtt
import Gpu.Vulkan.ColorComponent qualified as Vk.ClrCmp
import Gpu.Vulkan.Pipeline.ColorBlendState qualified as Vk.Ppl.ClrBlndSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.Ppl.Layout
import Gpu.Vulkan.Attachment qualified as Vk.Att
import Gpu.Vulkan.Subpass qualified as Vk.Subpass
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPass
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPass.M
import Gpu.Vulkan.Pipeline.Graphics qualified as Vk.Ppl.Graphics
import Gpu.Vulkan.Framebuffer qualified as Vk.Frmbffr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPool
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.Semaphore qualified as Vk.Semaphore
import Gpu.Vulkan.Fence qualified as Vk.Fence
import Gpu.Vulkan.VertexInput qualified as Vk.VtxInp
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Queue qualified as Vk.Queue
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.Cmd qualified as Vk.Cmd

import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscSetLyt
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPool
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSet


import Tools (clampOld, checkBits, checkFlag)

import Gpu.Vulkan.TypeEnum qualified as Vk.T

import Data.Text.ToolsYj

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win

import Data.Tuple.ToolsYj
import Data.List.ToolsYj
import Data.Bool.ToolsYj
import Data.Function.ToolsYj


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

body :: FramebufferResized -> GlfwG.Win.W sw -> Vk.Ist.I si -> IO ()
body fr w ist =
	Vk.Khr.Sfc.Glfw.Win.create ist w nil \sfc ->
	pickPhd ist sfc >>= \(pd, qfis) ->
	createLgDvc pd qfis \dv gq pq ->
	createSwpch w sfc pd qfis dv \(sc :: Vk.Khr.Swpch.S scifmt ss) ex ->
	Vk.Khr.Swpch.getImages dv sc >>= \scis -> createImgVws dv scis \scvs ->
	createRndrPss @scifmt dv \rp ->
	unfrmBffrOstAlgn pd \(_ :: Proxy alu) ->
	createPplLyt @alu dv \dsl pl -> createGrPpl dv ex rp pl \gp ->
	createFrmbffrs dv ex rp scvs \fbs ->
	createCmdPl qfis dv \cp ->
	createVtxBffr pd dv gq cp vertices \vb ->
	createIdxBffr pd dv gq cp indices \ib ->
	createMvpBffrsOld pd dv dsl maxFramesInFlight \dsls mbs mbms ->
	createDscPl dv \dp -> createDscSts dv dp mbs dsls \dss ->
	createCmdBffrs dv cp \cbs ->
	createSyncObjs dv \sos ->
	getCurrentTime >>=
	mainloop fr w sfc pd qfis dv gq pq
		sc ex scvs rp pl gp fbs vb ib mbms dss cbs sos

maxFramesInFlight :: Integral n => n
maxFramesInFlight = 2

pickPhd :: Vk.Ist.I si -> Vk.Khr.Sfc.S ss -> IO (Vk.Phd.P, QFamIndices)
pickPhd ist sfc = Vk.Phd.enumerate ist >>= \case
	[] -> error "failed to find GPUs with Gpu.Vulkan support!"
	pds -> findMaybeM suit pds >>= \case
		Nothing -> error "failed to find a suitable GPU!"
		Just pdqfi -> pure pdqfi
	where
	suit pd = espt pd >>= bool (pure Nothing) do
		qfis <- findQFams pd sfc
		querySwpchSupport pd sfc \ss -> pure . bool qfis Nothing
			$	HPListC.null (snd $ formats ss) ||
				null (presentModes ss)
	espt pd = elemAll dvcExtensions
		. (Vk.Phd.extensionPropertiesExtensionName <$>)
		<$> Vk.Phd.enumerateExtensionProperties pd Nothing

dvcExtensions :: [Vk.Phd.ExtensionName]
dvcExtensions = [Vk.Khr.Swpch.extensionName]

data QFamIndices =
	QFamIndices { grFam :: Vk.QFam.Index, prFam :: Vk.QFam.Index }

findQFams :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (Maybe QFamIndices)
findQFams pd sfc = do
	prps@((fst <$>) -> is) <- Vk.Phd.getQueueFamilyProperties pd
	mp <- listToMaybe
		<$> filterM (flip (Vk.Khr.Sfc.Phd.getSupport pd) sfc) is
	pure $ QFamIndices <$> (fst <$> find (grbit . snd) prps) <*> mp
	where grbit = checkBits Vk.Q.GraphicsBit . Vk.QFam.propertiesQueueFlags

createLgDvc :: Vk.Phd.P -> QFamIndices ->
	(forall sd . Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> IO a) -> IO a
createLgDvc pd qfis act = hetero qinfo uniqueQFams \qs ->
	Vk.Dvc.create pd (info qs) nil \dv -> join $ act dv
		<$> Vk.Dvc.getQueue dv (grFam qfis) 0
		<*> Vk.Dvc.getQueue dv (prFam qfis) 0
	where
	hetero :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] -> (forall ss .
		HPList.ToListWithCM' WithPoked TMaybe.M ss =>
		HPList.PL t ss -> b) -> b
	hetero _k [] f = f HPList.Nil
	hetero k (x : xs) f = hetero k xs \xs' -> f (k x :** xs')
	uniqueQFams = nub [grFam qfis, prFam qfis]
	info qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames = bool [] vldLayers debug,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures = Just def }
	qinfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }

createSwpch :: GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> (forall ss scfmt .
		Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swpch.S scfmt ss -> Vk.Extent2d -> IO a) -> IO a
createSwpch (GlfwG.Win.W win) sfc phdvc qfis dvc f = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilitiesOld spp
	let	fmt = Vk.Khr.Sfc.M.formatOldFormat
			. chooseSwapSurfaceFormat $ formatsOld spp
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		let	crInfo = mkSwapchainCreateInfoNew sfc qfis spp ext
		Vk.Khr.Swpch.create @'Nothing @fmt dvc crInfo nil
			\sc -> f sc ext

data SwpchSupportDetails fmts = SwpchSupportDetails {
	capabilities :: Vk.Khr.Sfc.Capabilities,
	formats :: (
		[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
		HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ),
	presentModes :: [Vk.Khr.PresentMode] }

deriving instance
	Show (HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts) =>
	Show (SwpchSupportDetails fmts)

querySwpchSupport :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> (forall fmts .
	Show (HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts) =>
	SwpchSupportDetails fmts -> IO a) -> IO a
querySwpchSupport pd sfc f = Vk.Khr.Sfc.Phd.getFormats pd sfc \fmts ->
	f =<< SwpchSupportDetails
		<$> Vk.Khr.Sfc.Phd.getCapabilities pd sfc
		<*> ((, fmts) <$> Vk.Khr.Sfc.Phd.getFormatsFiltered pd sfc)
		<*> Vk.Khr.Sfc.Phd.getPresentModes pd sfc

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilitiesOld :: Vk.Khr.Sfc.M.Capabilities,
	formatsOld :: [Vk.Khr.Sfc.M.FormatOld],
	presentModesOld :: [Vk.Khr.PresentMode] }

querySwapChainSupport ::
	Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO SwapChainSupportDetails
querySwapChainSupport dvc sfc = SwapChainSupportDetails
	<$> Vk.Khr.Sfc.PhysicalDevice.getCapabilities dvc sfc
	<*> Vk.Khr.Sfc.PhysicalDevice.getFormatsOld dvc sfc
	<*> Vk.Khr.Sfc.PhysicalDevice.getPresentModes dvc sfc

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
	fmt = chooseSwapSurfaceFormat $ formatsOld spp
	presentMode = chooseSwapPresentMode $ presentModesOld spp
	caps = capabilitiesOld spp
	maxImgc = fromMaybe maxBound . onlyIf (> 0)
		$ Vk.Khr.Sfc.M.capabilitiesMaxImageCount caps
	imgc = clampOld
		(Vk.Khr.Sfc.M.capabilitiesMinImageCount caps + 1) 0 maxImgc
	(ism, qfis) = bool
		(Vk.SharingModeConcurrent,
			[grFam qfis0, prFam qfis0])
		(Vk.SharingModeExclusive, [])
		(grFam qfis0 == prFam qfis0)

recreateSwapChain :: Vk.T.FormatToValue fmt => Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> Vk.Khr.Swpch.S fmt ssc ->
	IO (Vk.Format, Vk.Extent2d)
recreateSwapChain win sfc phdvc qfis0 dvc sc = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilitiesOld spp
	let	crInfo = mkSwapchainCreateInfoNew sfc qfis0 spp ext
		fmt = chooseSwapSurfaceFormat $ formatsOld spp
		scifmt = Vk.Khr.Sfc.M.formatOldFormat fmt
	(scifmt, ext) <$ Vk.Khr.Swpch.unsafeRecreate @'Nothing dvc crInfo nil sc

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
	(forall si . HPList.PL (Vk.ImgVw.I nm fmt) si -> IO a) -> IO a
createImgVws _dvc [] f = f HPList.Nil
createImgVws dvc (sci : scis) f =
	createImageView dvc sci \sciv ->
	createImgVws dvc scis \scivs -> f $ sciv :** scivs

recreateImageViewsNew :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Image.Binded ss ss nm scfmt] -> HPList.PL (Vk.ImgVw.I nm scfmt) sis -> IO ()
recreateImageViewsNew _dvc [] HPList.Nil = pure ()
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
			Vk.RndrPass.M.createInfoAttachments = colorAttachment :** HPList.Nil,
			Vk.RndrPass.M.createInfoSubpasses = [subpass],
			Vk.RndrPass.M.createInfoDependencies = [dependency] }
	Vk.RndrPass.create @'Nothing @'[scifmt] dvc renderPassInfo nil \rp -> f rp

unfrmBffrOstAlgn ::
	Vk.Phd.P -> (forall a . KnownNat a => Proxy a -> IO b) -> IO b
unfrmBffrOstAlgn pd f = (\(SomeNat p) -> f p) . someNatVal . fromIntegral
	. Vk.Phd.limitsMinUniformBufferOffsetAlignment . Vk.Phd.propertiesLimits
	=<< Vk.Phd.getProperties pd

type AtomUboNew s al = '(s, '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom al UniformBufferObject 'Nothing]])

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscSetLyt.D s '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom al UniformBufferObject 'Nothing]]
	-> IO a) -> IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing
		'[ 'Vk.DscSetLyt.Buffer '[VObj.Atom al UniformBufferObject 'Nothing] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = uboLayoutBinding :** HPList.Nil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[VObj.Atom al UniformBufferObject 'Nothing])
	uboLayoutBinding = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

createPplLyt :: forall al sd b .
	Vk.Dvc.D sd -> (forall sdsl sl .
		Vk.DscSetLyt.D sdsl
			'[ 'Vk.DscSetLyt.Buffer '[VObj.Atom al UniformBufferObject 'Nothing]] ->
		Vk.Ppl.Layout.P sl '[AtomUboNew sdsl al] '[] -> IO b) -> IO b
createPplLyt dvc f =
	createDescriptorSetLayout dvc \dsl ->
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = TMaybe.N,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts =
				HPList.Singleton $ U2 dsl } in
	Vk.Ppl.Layout.create @'Nothing @_ @_ @'[] dvc pipelineLayoutInfo nil $ f dsl

createGrPpl :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUboNew sdsl al] '[] ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUboNew sdsl al], '[]) -> IO a) -> IO a
createGrPpl dvc sce rp ppllyt f =
	Vk.Ppl.Graphics.createGs dvc Nothing (U14 pplInfo :** HPList.Nil)
			nil \(U3 gpl :** HPList.Nil) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt

recreateGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUboNew sdsl al] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUboNew sdsl al], '[]) -> IO ()
recreateGraphicsPipeline' dvc sce rp ppllyt gpls = Vk.Ppl.Graphics.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HPList.Nil) nil (U3 gpls :** HPList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt

mkGraphicsPipelineCreateInfo' ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUboNew sdsl al] '[] ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing, '[ '(Vertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'Nothing '(sl, '[AtomUboNew sdsl al], '[]) sr '(sb, vs', ts', sbtss')
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

shaderStages :: HPList.PL (U5 Vk.Ppl.ShdrSt.CreateInfo) '[
	'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
	'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
shaderStages = U5 vertShaderStageInfo :** U5 fragShaderStageInfo :** HPList.Nil
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
	Vk.RndrPass.R sr -> HPList.PL (Vk.ImgVw.I nm fmt) sis ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HPList.Nil f = f HPList.Nil
createFrmbffrs dvc sce rp (iv :** ivs) f =
	Vk.Frmbffr.create dvc (mkFramebufferCreateInfoNew sce rp iv) nil \fb ->
	createFrmbffrs dvc sce rp ivs \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPass.R sr -> HPList.PL (Vk.ImgVw.I nm fmt) sis ->
		HPList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HPList.Nil HPList.Nil = pure ()

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
	Vk.Frmbffr.createInfoAttachments = U3 attch :** HPList.Nil,
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
		Vk.CmdPool.createInfoQueueFamilyIndex = grFam qfis }

createVtxBffr :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	[Vertex] -> (forall sm sb al . KnownNat al => Vk.Bffr.Binded sm sb bnm
		'[VObj.List al Vertex lnm] -> IO a) -> IO a
createVtxBffr = createBffrMem Vk.Bffr.UsageVertexBufferBit

createIdxBffr :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	[Word16] -> (forall sm sb al . KnownNat al => Vk.Bffr.Binded sm sb nm
		'[VObj.List al Word16 lnm] -> IO a) -> IO a
createIdxBffr = createBffrMem Vk.Bffr.UsageIndexBufferBit

createBffrMem :: forall sd sc nm t lnm a . Storable' t =>
	Vk.Bffr.UsageFlags -> Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> [t] ->
	(forall sm sb al . KnownNat al => Vk.Bffr.Binded sm sb nm
		'[VObj.List al t lnm] -> IO a) -> IO a
createBffrMem us pd dv gq cp xs@(fromIntegral . length -> ln) f =
	bffrLstAlgn @t dv ln (Vk.Bffr.UsageTransferDstBit .|. us)
		\(_ :: Proxy al) ->
	createBffrLst pd dv ln (Vk.Bffr.UsageTransferDstBit .|. us)
		Vk.Mm.PropertyDeviceLocalBit \b _ -> do
		createBffrLst pd dv ln
			Vk.Bffr.UsageTransferSrcBit (
			Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) \
			(b' :: Vk.Bffr.Binded sm sb bnm' '[VObj.List al t lnm'])
			bm' -> do
			Vk.Mm.write
				@bnm' @(VObj.List al t lnm') dv bm' zeroBits xs
			copyBffr dv gq cp b' b
		f b

bffrLstAlgn :: forall t sd a (lnm :: Symbol) . Storable t =>
	Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags -> (forall al .
		KnownNat al => Proxy al -> IO a) -> IO a
bffrLstAlgn dv sz = bffrAlgn @(VObj.List 256 t lnm) dv (VObj.LengthList sz)

bffrAlgn :: forall o sd a . VObj.SizeAlignment o =>
	Vk.Dvc.D sd -> VObj.Length o -> Vk.Bffr.UsageFlags ->
	(forall al . KnownNat al => Proxy al -> IO a) -> IO a
bffrAlgn dv ln us f = Vk.Bffr.create dv (bffrInfo ln us) nil \b ->
	(\(SomeNat p) -> f p) . someNatVal . fromIntegral =<<
	Vk.Mm.requirementsAlignment <$> Vk.Bffr.getMemoryRequirements dv b

class CreateMvpBffrs (mff :: [()]) where
	createMvpBffrs ::
		KnownNat al =>
			Vk.Phd.P -> Vk.Dvc.D sd ->
		Vk.DscSetLyt.D sdsc '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom al UniformBufferObject 'Nothing]] ->
		(forall slyts smsbs . (
			Vk.DscSet.DListFromMiddle slyts,
			HPList.FromList slyts,
			UpdateNew al smsbs slyts,
			HPList.HomoList (AtomUboNew sdsc al) slyts) =>
			HPList.PL (U2 Vk.DscSetLyt.D) slyts ->
			HPList.PL (BindedUboNew al) smsbs ->
			HPList.PL (MemoryUboNew al) smsbs -> IO a) -> IO a

instance CreateMvpBffrs '[] where
	createMvpBffrs _ _ _ f = f HPList.Nil HPList.Nil HPList.Nil

instance CreateMvpBffrs mff => CreateMvpBffrs ('() ': mff) where
	createMvpBffrs ph dvc dscslyt f =
		createUniformBuffer1New ph dvc \(b :: BindedUboNew al smsb) m ->
			createMvpBffrs @mff ph dvc dscslyt \ls (bs :: HPList.PL (BindedUboNew al) smsbs) ms -> f
				(U2 dscslyt :** ls)
				(b :** bs :: HPList.PL (BindedUboNew al) (smsb ': smsbs))
				(m :** ms)

createMvpBffrsOld ::
	KnownNat al =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.DscSetLyt.D sdsc '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom al UniformBufferObject 'Nothing]] ->
	Int -> (forall slyts smsbs . (
		Vk.DscSet.DListFromMiddle slyts,
		HPList.FromList slyts,
		UpdateNew al smsbs slyts,
		HPList.HomoList (AtomUboNew sdsc al) slyts) =>
		HPList.PL (U2 Vk.DscSetLyt.D) slyts ->
		HPList.PL (BindedUboNew al) smsbs ->
		HPList.PL (MemoryUboNew al) smsbs -> IO a) -> IO a
createMvpBffrsOld _ _ _ 0 f = f HPList.Nil HPList.Nil HPList.Nil
createMvpBffrsOld ph dvc dscslyt n f =
	createUniformBuffer1New ph dvc \(b :: BindedUboNew al smsb) m ->
		createMvpBffrsOld ph dvc dscslyt (n - 1) \ls (bs :: HPList.PL (BindedUboNew al) smsbs) ms -> f
			(U2 dscslyt :** ls)
			(b :** bs :: HPList.PL (BindedUboNew al) (smsb ': smsbs))
			(m :** ms)

type family MapFst abs where
	MapFst '[] = '[]
	MapFst ( '(a, b, c :: k) ': abs) = a ': MapFst abs

data BindedUbo smsb where
	BindedUbo :: Vk.Bffr.Binded sm sb "uniform-buffer" '[VObj.Atom 256 UniformBufferObject 'Nothing] ->
		BindedUbo '(sm, sb)

data BindedUboNew al smsb where
	BindedUboNew :: Vk.Bffr.Binded sm sb "uniform-buffer" '[VObj.Atom al UniformBufferObject 'Nothing] ->
		BindedUboNew al '(sm, sb)

data MemoryUboNew al smsb where
	MemoryUboNew :: UniformBufferMemoryNew sm sb "uniform-buffer" al -> MemoryUboNew al '(sm, sb)

createUniformBuffer1New :: KnownNat al => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
		BindedUboNew al '(sm, sb) -> MemoryUboNew al '(sm, sb) -> IO b) -> IO b
createUniformBuffer1New phdvc dvc f = createBffrAtm
		Vk.Bffr.UsageUniformBufferBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) phdvc dvc
	\b m -> f (BindedUboNew b) (MemoryUboNew m)

type UniformBufferMemoryNew sm sb nm al = Vk.Mm.M sm '[ '(
	sb,
	'Vk.Mm.BufferArg nm '[VObj.Atom al UniformBufferObject 'Nothing]
	)]

createDscPl :: Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDscPl dvc = Vk.DscPool.create dvc poolInfo nil
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = TMaybe.N,
		Vk.DscPool.createInfoFlags =
			Vk.DscPool.CreateFreeDescriptorSetBit,
		Vk.DscPool.createInfoMaxSets = maxFramesInFlight,
		Vk.DscPool.createInfoPoolSizes = [poolSize] }
	poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = maxFramesInFlight }

createDscSts :: (
	Vk.DscSet.DListFromMiddle ss,
	HPList.FromList ss, UpdateNew al smsbs ss ) =>
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> HPList.PL (BindedUboNew al) smsbs ->
	HPList.PL (U2 Vk.DscSetLyt.D) ss ->
	(forall sds . HPList.PL (Vk.DscSet.D sds) ss -> IO a) -> IO a
createDscSts dvc dscp ubs dscslyts f =
	Vk.DscSet.allocateDs dvc allocInfo \dscss -> do
	updateNew dvc ubs dscss
	f dscss
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = dscslyts }

descriptorWrite :: KnownNat al =>
	Vk.Bffr.Binded sm sb nm '[VObj.Atom al UniformBufferObject 'Nothing] ->
	Vk.DscSet.D sds slbts ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, VObj.Atom al UniformBufferObject 'Nothing )]) 0
descriptorWrite ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HPList.Singleton bufferInfo
	}
	where bufferInfo = U4 $ Vk.Dsc.BufferInfo ub

class Update smsbs slbtss where
	update :: Vk.Dvc.D sd -> HPList.PL BindedUbo smsbs ->
		HPList.PL (Vk.DscSet.D sds) slbtss -> IO ()

instance Update '[] '[] where update _ HPList.Nil HPList.Nil = pure ()

instance Update '[t] '[] where update _ (HPList.Singleton _) HPList.Nil = pure ()

instance (
	Vk.DscSet.BindingAndArrayElemBuffer
		(TIndex.I1_2 '(ds, cs))
		'[VObj.Atom 256 UniformBufferObject 'Nothing] 0,
	Vk.DscSet.UpdateDynamicLength
		(TIndex.I1_2 '(ds, cs))
		'[VObj.Atom 256 UniformBufferObject 'Nothing],
	Update ubs dscss ) =>
	Update (ub ': ubs) ('(ds, cs) ': dscss ) where
	update dvc (BindedUbo ub :** ubs) (dscs :** dscss) = do
		Vk.DscSet.updateDs dvc
			(HPList.Singleton . U5 $ descriptorWrite ub dscs) HPList.Nil
		update dvc ubs dscss

class UpdateNew al smsbs slbtss where
	updateNew :: Vk.Dvc.D sd -> HPList.PL (BindedUboNew al) smsbs ->
		HPList.PL (Vk.DscSet.D sds) slbtss -> IO ()

instance UpdateNew al '[] '[] where updateNew _ HPList.Nil HPList.Nil = pure ()

instance UpdateNew al '[t] '[] where updateNew _ (HPList.Singleton _) HPList.Nil = pure ()

instance (
	Vk.DscSet.BindingAndArrayElemBuffer
		(TIndex.I1_2 '(ds, cs))
		'[VObj.Atom al UniformBufferObject 'Nothing] 0,
	Vk.DscSet.UpdateDynamicLength
		(TIndex.I1_2 '(ds, cs))
		'[VObj.Atom al UniformBufferObject 'Nothing],
	KnownNat al,
	UpdateNew al ubs dscss ) =>
	UpdateNew al (ub ': ubs) ('(ds, cs) ': dscss ) where
	updateNew dvc (BindedUboNew ub :** ubs) (dscs :** dscss) = do
		Vk.DscSet.updateDs dvc
			(HPList.Singleton . U5 $ descriptorWrite ub dscs) HPList.Nil
		updateNew dvc ubs dscss

createBffrAtm :: forall al sd nm a b . (KnownNat al, Storable a) =>
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.Atom al a 'Nothing] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[VObj.Atom al a 'Nothing] )] ->
		IO b) -> IO b
createBffrAtm us prs p dv = createBffr p dv VObj.LengthAtom us prs

createBffrLst :: forall al sd bnm lnm t a . (KnownNat al, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[VObj.List al t lnm] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm '[VObj.List al t lnm])] ->
		IO a) -> IO a
createBffrLst p dv ln = createBffr p dv $ VObj.LengthList ln

createBffr :: forall sd bnm o a . VObj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[o] -> Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm '[o])] -> IO a) -> IO a
createBffr p dv ln us prs f = Vk.Bffr.create dv (bffrInfo ln us) nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits reqs) prs
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(ainfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bd))) -> bd
	where ainfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

bffrInfo ::
	VObj.Length o -> Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo 'Nothing '[o]
bffrInfo ln us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N, Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton ln,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

findMmType :: Vk.Phd.P ->
	Vk.Mm.TypeBits -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmType pd flt prs =
	fromMaybe (error msg) . suit <$> Vk.Phd.getMemoryProperties pd
	where
	msg = "failed to find suitable memory type!"
	suit prs1 = fst <$> find ((&&)
		<$> (`Vk.Mm.elemTypeIndex` flt) . fst
		<*> checkBits prs . Vk.Mm.mTypePropertyFlags . snd)
			(Vk.Phd.memoryPropertiesMemoryTypes prs1)

copyBffr :: forall sd sc sm sb sm' sb' bnm bnm' al t lnm .
	(KnownNat al, Sizable t, Storable t) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb bnm '[VObj.List al t lnm] ->
	Vk.Bffr.Binded sm' sb' bnm' '[VObj.List al t lnm] -> IO ()
copyBffr dv gq cp s d = createCmdBffr dv cp \cb -> do
	Vk.CmdBffr.begin @'Nothing @'Nothing cb binfo $
		Vk.Cmd.copyBuffer @'[ '[VObj.List al t lnm]] cb s d
	Vk.Q.submit gq (HPList.Singleton . U4 $ sinfo cb) Nothing
	Vk.Q.waitIdle gq
	where
	binfo = Vk.CmdBffr.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }
	sinfo cb = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

createCmdBffr :: forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb . Vk.CmdBffr.C scb -> IO a) -> IO a
createCmdBffr dv cp f =
	Vk.CmdBffr.allocate dv info $ f . \(cb :*. HPList.Nil) -> cb
	where
	info :: Vk.CmdBffr.AllocateInfo 'Nothing scp '[ '()]
	info = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }

createCmdBffrs :: forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPool.C scp ->
	(forall scb vss . (TLength.Length vss, HPList.HomoList '() vss) =>
		HPList.LL (Vk.CmdBffr.C scb) (vss :: [()]) -> IO a) ->
	IO a
createCmdBffrs dvc cp f = mkVss maxFramesInFlight \(_p :: Proxy vss1) ->
	Vk.CmdBffr.allocate @_ @vss1 dvc (allocInfo @vss1) (f @_ @vss1)
	where
	allocInfo :: forall vss . Vk.CmdBffr.AllocateInfo 'Nothing scp vss
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }

mkVss :: Int -> (forall (vss :: [()]) . TLength.Length vss => (
	TpLvlLst.Length vss, HPList.FromList vss,
	HPList.HomoList '() vss ) =>
	Proxy vss -> a) -> a
mkVss 0 f = f (Proxy @'[])
mkVss n f = mkVss (n - 1) \p -> f $ addTypeToProxy p
	where
	addTypeToProxy :: Proxy vss -> Proxy ('() ': vss)
	addTypeToProxy Proxy = Proxy

data SyncObjects (ssos :: ([Type], [Type], [Type])) where
	SyncObjects :: {
		_imageAvailableSemaphores :: HPList.PL Vk.Semaphore.S siass,
		_renderFinishedSemaphores :: HPList.PL Vk.Semaphore.S srfss,
		_inFlightFences :: HPList.PL Vk.Fence.F sfss } ->
		SyncObjects '(siass, srfss, sfss)

createSyncObjs ::
	Vk.Dvc.D sd -> (forall ssos . SyncObjects ssos -> IO a ) -> IO a
createSyncObjs dvc f =
	HPList.replicateM maxFramesInFlight
		(Vk.Semaphore.create @'Nothing dvc def nil) \iass ->
	HPList.replicateM maxFramesInFlight
		(Vk.Semaphore.create @'Nothing dvc def nil) \rfss ->
	HPList.replicateM maxFramesInFlight
		(Vk.Fence.create @'Nothing dvc fncInfo nil) \iffs ->
	f $ SyncObjects iass rfss iffs
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sl sdsc sf sg sm sb nm sm' sb' nm' sds al alv ali .
	(KnownNat alv, KnownNat ali) =>
	Vk.CmdBffr.C scb ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Layout.P sl '[AtomUboNew sdsc al] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUboNew sdsc al], '[]) ->
	Vk.Bffr.Binded sm sb nm '[VObj.List alv Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List ali Word16 ""] ->
--	Vk.DscSet.S sdsc' sp s ->
	Vk.DscSet.D sds (AtomUboNew sdsc al) ->
	IO ()
recordCommandBuffer cb rp fb sce ppllyt gpl vb ib dscs =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb
		(HPList.Singleton . U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @Vertex @"" vb) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word16 @"" ib) >>
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics ppllyt
		(HPList.Singleton $ U2 dscs)
		(HPList.Singleton (
			HPList.Nil :**
			HPList.Nil )) >>
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
		Vk.RndrPass.beginInfoClearValues = HPList.Singleton
			. Vk.ClearValueColor . fromJust $ rgbaDouble 0 0 0 1 }

mainloop :: (
	RecreateFramebuffers ss sfs,
	HPList.HomoList (AtomUboNew sdsc al) slyts,
	Vk.T.FormatToValue fmt, HPList.HomoList '() vss,
	KnownNat al, KnownNat alv, KnownNat ali
	) =>
	FramebufferResized ->
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swpch.S fmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm fmt) ss ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUboNew sdsc al] '[] -> Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUboNew sdsc al], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List alv Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List ali Word16 ""] ->
	HPList.PL (MemoryUboNew al) smsbs ->
	HPList.PL (Vk.DscSet.D sds) slyts ->
	HPList.LL (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	UTCTime ->
	IO ()
mainloop g (GlfwG.Win.W w) sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs vb ib ums dscss cbs iasrfsifs tm0 = do
	($ cycle [0 .. maxFramesInFlight - 1]) . ($ ext0) $ fix \loop ext (cf : cfs) -> do
		Glfw.pollEvents
		tm <- getCurrentTime
		runLoopNew w sfc phdvc qfis dvc gq pq
			sc g ext scivs rp ppllyt gpl fbs vb ib cbs iasrfsifs ums dscss
			(realToFrac $ tm `diffUTCTime` tm0)
			cf (`loop` cfs)
	Vk.Dvc.waitIdle dvc

runLoopNew :: (
	RecreateFramebuffers sis sfs,
	Vk.T.FormatToValue fmt, HPList.HomoList '() vss,
	HPList.HomoList (AtomUboNew sdsc al) slyts,
	KnownNat al, KnownNat alv, KnownNat ali ) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swpch.S fmt ssc -> FramebufferResized -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm fmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUboNew sdsc al] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUboNew sdsc al], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List alv Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List ali Word16 ""] ->
	HPList.LL (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	HPList.PL (MemoryUboNew al) smsbs ->
	HPList.PL (Vk.DscSet.D sds) slyts ->
	Float ->
	Int ->
	(Vk.Extent2d -> IO ()) -> IO ()
runLoopNew win sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl fbs vb ib cbs iasrfsifs ums dscss tm cf loop = do
	catchAndRecreate win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop
		$ drawFrameNew dvc gq pq sc ext rp ppllyt gpl fbs vb ib cbs iasrfsifs ums dscss tm cf
	cls <- Glfw.windowShouldClose win
	if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateSwapChainEtc
			win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs)

drawFrameNew :: forall sfs sd ssc fmt sr sl sdsc sg sm sb nm sm' sb' nm' scb ssos vss smsbs slyts sds al alv ali . (
	HPList.HomoList '() vss,
	HPList.HomoList (AtomUboNew sdsc al) slyts,
	KnownNat al,
	KnownNat alv,
	KnownNat ali
	) =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> Vk.Khr.Swpch.S fmt ssc ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.P sl '[AtomUboNew sdsc al] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUboNew sdsc al], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List alv Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List ali Word16 ""] ->
	HPList.LL (Vk.CmdBffr.C scb) vss -> SyncObjects ssos ->
	HPList.PL (MemoryUboNew al) smsbs ->
	HPList.PL (Vk.DscSet.D sds) slyts ->
	Float ->
	Int -> IO ()
drawFrameNew dvc gq pq sc ext rp ppllyt gpl fbs vb ib cbs (SyncObjects iass rfss iffs) ums dscss tm cf =
	HPList.index iass cf \(ias :: Vk.Semaphore.S sias) ->
	HPList.index rfss cf \(rfs :: Vk.Semaphore.S srfs) ->
	HPList.index iffs cf \(id &&& HPList.Singleton -> (iff, siff)) ->
--	HPList.index ubs cf \_ub -> HPList.index ums cf \um ->
	HPList.index ums cf \um ->
	($ HPList.homoListIndex dscss cf) \(dscs :: Vk.DscSet.D sds (AtomUboNew sdsc al)) -> do
	Vk.Fence.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HPList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext ppllyt gpl vb ib dscs
	updateUniformBufferNew dvc um ext tm
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers = HPList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
		presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphores = HPList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = HPList.Singleton
				$ Vk.Khr.SwapchainImageIndex sc imgIdx }
	Vk.Queue.submit gq (HPList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfo
	where	HPList.Dummy cb = cbs `HPList.homoListIndex` cf ::
			HPList.Dummy (Vk.CmdBffr.C scb) '()

updateUniformBufferNew :: forall sd al sbsm . KnownNat al =>
	Vk.Dvc.D sd -> MemoryUboNew al sbsm -> Vk.Extent2d -> Float -> IO ()
updateUniformBufferNew dvc (MemoryUboNew um) sce tm =
	Vk.Mm.write @"uniform-buffer" @(VObj.Atom al UniformBufferObject 'Nothing) dvc um zeroBits ubo
	where ubo = UniformBufferObject {
		uniformBufferObjectModel = Cglm.rotate
			Cglm.mat4Identity
			(tm * Cglm.rad 90)
			(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL),
		uniformBufferObjectView = Cglm.lookat -- Cglm.mat4Identity,
			(Cglm.Vec3 $ 2 :. 2 :. 2 :. NilL)
			(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
			(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL),
		uniformBufferObjectProj = Cglm.modifyMat4 1 1 negate -- Cglm.mat4Identity }
			$ Cglm.perspective
				(Cglm.rad 45)
				(fromIntegral (Vk.extent2dWidth sce) /
					fromIntegral (Vk.extent2dHeight sce))
				0.1 10 }

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate :: (
	RecreateFramebuffers sis sfs, Vk.T.FormatToValue fmt ) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S fmt ssc ->
	HPList.PL (Vk.ImgVw.I nm fmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUboNew sdsc al] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUboNew sdsc al], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
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
	RecreateFramebuffers sis sfs, Vk.T.FormatToValue fmt) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S fmt ssc ->
	HPList.PL (Vk.ImgVw.I nm fmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUboNew sdsc al] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[AtomUboNew sdsc al], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateSwapChainEtc win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	(scifmt, ext) <- recreateSwapChain win sfc phdvc qfis dvc sc
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
