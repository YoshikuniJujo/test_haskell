{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main(main) where

import Foreign.Storable.PeekPoke
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Ord.ToolsYj
import Data.Bits
import Data.Function.ToolsYj
import Data.Default
import Data.Tuple.ToolsYj
import Data.Maybe
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.List.ToolsYj
import Data.HeteroParList (pattern (:**), pattern (:*.))
import Data.HeteroParList qualified as HPList
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.Bool
import Data.Bool.ToolsYj
import Data.Word
import Data.Text.IO qualified as Txt
import Data.Color
import Data.IORef
import Data.IORef.ToolsYj

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Exception qualified as Vk
import Gpu.Vulkan.Instance qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.QueueFamily qualified as Vk.QFm
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.Fence qualified as Vk.Fence
import Gpu.Vulkan.Semaphore qualified as Vk.Semaphore

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl

import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch

import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.DbgUtls.Msngr

import Debug

main :: IO ()
main = newIORef False >>= \fr -> withWindow fr \w -> createIst \ist ->
	Vk.Khr.Sfc.Glfw.Win.create ist w nil \sfc ->
	Vk.Phd.enumerate ist >>= \[pd] -> printPhdPrps pd sfc >>= \qfi ->
	createLgDvc pd qfi \dv q -> createSwpch w sfc pd qfi dv \sc ex ->
	Vk.Khr.Swpch.getImages dv sc >>= \scis0 -> -- createImgVws dv scis \scvs ->
	HPList.replicateM frameOverlap (createCmdPl qfi dv) \cps ->
	createCmdBffrs dv cps \cbs@(cb1 :** cb2 :** HPList.Nil) ->
	createSyncObjs @'[ '(), '()] dv \soss ->
	(($ scis0) . ($ 0) $ fix \go fn scis -> do
		catchAndRecreate w sfc pd qfi dv sc (go $ fn + 1)
			$ draw dv sc scis q cbs soss (fn `mod` 2) fn
		GlfwG.pollEvents
		sz <- checkFlag fr
		GlfwG.Win.shouldClose w >>= \case
			False -> if sz
				then do
					when sz . void $ recreateAll w sfc pd qfi dv sc
					go (fn + 1) =<< Vk.Khr.Swpch.getImages dv sc
				else go (fn + 1) scis
			True -> pure ()
		) >>
	Vk.Dvc.waitIdle dv
	where
	v1213Features pd = do
		Vk.Phd.Features2 (TMaybe.J v1213fs) _fs <- Vk.Phd.getFeatures2
			@('Just (Vk.Phd.Vulkan12Features ('Just (Vk.Phd.Vulkan13Features 'Nothing)))) pd
		pure v1213fs

frameOverlap :: Int
frameOverlap = 2

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
	sizeName = ((800, 600), "Vulkan Guide Initializing")

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
		Vk.applicationInfoApplicationName = "Example Vulkan Application",
		Vk.applicationInfoApplicationVersion =
			Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoEngineName = "No Engine",
		Vk.applicationInfoEngineVersion = Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoApiVersion = Vk.apiVersion_1_3 }

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

printPhdPrps :: Vk.Phd.P -> Vk.Khr.Sfc.S s -> IO Vk.QFm.Index
printPhdPrps pd sfc = do
	prps <- Vk.Phd.getProperties pd
	fs <- Vk.Phd.getFeatures2 @(
		'Just (Vk.Phd.Vulkan12Features (
		'Just (Vk.Phd.Vulkan13Features 'Nothing) )) ) pd
	qfs <- Vk.Phd.getQueueFamilyProperties pd
	let	TMaybe.J v12fs = Vk.Phd.features2Next fs
		TMaybe.J v13fs = Vk.Phd.vulkan12FeaturesNext v12fs
		qi0 = fst $ head qfs
	putStrLn $ "API Version          : " ++
		show (Vk.fromApiVersion $ Vk.Phd.propertiesApiVersion prps)
	putStrLn $ "Dynamic Rendering    : " ++
		show (Vk.Phd.vulkan13FeaturesDynamicRendering v13fs)
	putStrLn $ "Synchronization 2    : " ++
		show (Vk.Phd.vulkan13FeaturesSynchronization2 v13fs)
	putStrLn $ "Bufefr Device Address: " ++
		show (Vk.Phd.vulkan12FeaturesBufferDeviceAddress v12fs)
	putStrLn $ "Descriptor Indexing  : " ++
		show (Vk.Phd.vulkan12FeaturesDescriptorIndexing v12fs)
	qs <- Vk.Khr.Sfc.Phd.getSupport pd qi0 sfc
	putStrLn $ "Support the surface  : " ++ show qs
	pure qi0

createLgDvc :: Vk.Phd.P -> Vk.QFm.Index ->
	(forall sd . Vk.Dvc.D sd -> Vk.Q.Q -> IO a) -> IO a
createLgDvc pd qfi act = hetero qinfo [qfi] \qs ->
	Vk.Dvc.create pd (info qs) nil \dv ->
	act dv =<< Vk.Dvc.getQueue dv qfi 0
	where
	hetero :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] -> (forall ss .
		HPList.ToListWithCM' WithPoked TMaybe.M ss =>
		HPList.PL t ss -> b) -> b
	hetero _k [] f = f HPList.Nil
	hetero k (x : xs) f = hetero k xs \xs' -> f (k x :** xs')
	info qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.J features,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames = bool [] vldLayers debug,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures = Nothing }
	qinfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }

dvcExtensions :: [Vk.Phd.ExtensionName]
dvcExtensions = [Vk.Khr.Swpch.extensionName]

features :: Vk.Phd.Features2 (
	'Just (Vk.Phd.Vulkan12Features (
	'Just (Vk.Phd.Vulkan13Features 'Nothing) )) )
features = Vk.Phd.Features2 {
	Vk.Phd.features2Next = TMaybe.J features12,
	Vk.Phd.features2Features = def }

features12 :: Vk.Phd.Vulkan12Features ('Just (Vk.Phd.Vulkan13Features 'Nothing))
features12 = (Vk.Phd.vulkan12FeaturesZero $ TMaybe.J features13) {
	Vk.Phd.vulkan12FeaturesBufferDeviceAddress = True,
	Vk.Phd.vulkan12FeaturesDescriptorIndexing = True }

features13 :: Vk.Phd.Vulkan13Features 'Nothing
features13 = (Vk.Phd.vulkan13FeaturesZero TMaybe.N) {
	Vk.Phd.vulkan13FeaturesDynamicRendering = True,
	Vk.Phd.vulkan13FeaturesSynchronization2 = True }

createSwpch :: GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	Vk.QFm.Index -> Vk.Dvc.D sd -> (forall ss scfmt .
		Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swpch.S scfmt ss -> Vk.Extent2d -> IO a) -> IO a
createSwpch win sfc pd qfi dv f = querySwpchSupport pd sfc \ss -> do
	ex <- swapExtent win $ capabilities ss
	let	cps = capabilities ss
		pm = findDefault Vk.Khr.Sfc.PresentModeFifo
			(== Vk.Khr.Sfc.PresentModeMailbox) $ presentModes ss
	chooseSwpSfcFmt (formats ss)
		\(Vk.Khr.Sfc.Format sc :: Vk.Khr.Sfc.Format fmt) ->
		Vk.Khr.Swpch.create @_ @fmt dv
			(swpchInfo sfc qfi cps sc pm ex) nil (`f` ex)

data SwpchSupportDetails fmts = SwpchSupportDetails {
	capabilities :: Vk.Khr.Sfc.Capabilities,
	formats :: (
		[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
		HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ),
	presentModes :: [Vk.Khr.Sfc.PresentMode] }

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

chooseSwpSfcFmt :: (
	[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
	HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ) ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Khr.Sfc.Format fmt -> a) -> a
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`L.find` fmts)
	$ (== Vk.Khr.Sfc.ColorSpaceSrgbNonlinear) . Vk.Khr.Sfc.formatColorSpace
chooseSwpSfcFmt (_, HPListC.Nil) _ = error "no available swap surface formats"

recreateSwpch :: forall sw ssfc sd fmt ssc . Vk.T.FormatToValue fmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	Vk.QFm.Index -> Vk.Dvc.D sd -> Vk.Khr.Swpch.S fmt ssc -> IO Vk.Extent2d
recreateSwpch win sfc phdvc qfi dvc sc = do
	ss <- querySwpchSupportFmt @fmt phdvc sfc
	ex <- swapExtent win $ capabilitiesFmt ss
	let	cps = capabilitiesFmt ss
		Vk.Khr.Sfc.Format cs = fromMaybe
			(error "no available swap surface formats")
			. listToMaybe $ formatsFmt ss
		pm = findDefault Vk.Khr.Sfc.PresentModeFifo
			(== Vk.Khr.Sfc.PresentModeMailbox) $ presentModesFmt ss
	ex <$ Vk.Khr.Swpch.unsafeRecreate dvc
		(swpchInfo @fmt sfc qfi cps cs pm ex) nil sc

data SwpchSupportDetailsFmt fmt = SwpchSupportDetailsFmt {
	capabilitiesFmt :: Vk.Khr.Sfc.Capabilities,
	formatsFmt :: [Vk.Khr.Sfc.Format fmt],
	presentModesFmt :: [Vk.Khr.Sfc.PresentMode] } deriving Show

querySwpchSupportFmt :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (SwpchSupportDetailsFmt fmt)
querySwpchSupportFmt dvc sfc = SwpchSupportDetailsFmt
	<$> Vk.Khr.Sfc.Phd.getCapabilities dvc sfc
	<*> Vk.Khr.Sfc.Phd.getFormatsFiltered dvc sfc
	<*> Vk.Khr.Sfc.Phd.getPresentModes dvc sfc

swapExtent :: GlfwG.Win.W sw -> Vk.Khr.Sfc.Capabilities -> IO Vk.Extent2d
swapExtent win cps
	| Vk.extent2dWidth cur /= maxBound = pure cur
	| otherwise = (<$> GlfwG.Win.getFramebufferSize win)
		\(fromIntegral -> w, fromIntegral -> h) ->
		Vk.Extent2d
			(clamp (Vk.extent2dWidth n) (Vk.extent2dWidth x) w)
			(clamp (Vk.extent2dHeight n) (Vk.extent2dHeight x) h)
	where
	cur = Vk.Khr.Sfc.capabilitiesCurrentExtent cps
	n = Vk.Khr.Sfc.capabilitiesMinImageExtent cps
	x = Vk.Khr.Sfc.capabilitiesMaxImageExtent cps

swpchInfo :: forall fmt ss .
	Vk.Khr.Sfc.S ss -> Vk.QFm.Index -> Vk.Khr.Sfc.Capabilities ->
	Vk.Khr.Sfc.ColorSpace -> Vk.Khr.Sfc.PresentMode -> Vk.Extent2d ->
	Vk.Khr.Swpch.CreateInfo 'Nothing ss fmt
swpchInfo sfc qfis0 cps cs pm ex = Vk.Khr.Swpch.CreateInfo {
	Vk.Khr.Swpch.createInfoNext = TMaybe.N,
	Vk.Khr.Swpch.createInfoFlags = zeroBits,
	Vk.Khr.Swpch.createInfoSurface = sfc,
	Vk.Khr.Swpch.createInfoMinImageCount = imgc,
	Vk.Khr.Swpch.createInfoImageColorSpace = cs,
	Vk.Khr.Swpch.createInfoImageExtent = ex,
	Vk.Khr.Swpch.createInfoImageArrayLayers = 1,
	Vk.Khr.Swpch.createInfoImageUsage = Vk.Img.UsageTransferDstBit,
	Vk.Khr.Swpch.createInfoImageSharingMode = ism,
	Vk.Khr.Swpch.createInfoQueueFamilyIndices = qfis,
	Vk.Khr.Swpch.createInfoPreTransform =
		Vk.Khr.Sfc.capabilitiesCurrentTransform cps,
	Vk.Khr.Swpch.createInfoCompositeAlpha =
		Vk.Khr.Sfc.CompositeAlphaOpaqueBit,
	Vk.Khr.Swpch.createInfoPresentMode = pm,
	Vk.Khr.Swpch.createInfoClipped = True,
	Vk.Khr.Swpch.createInfoOldSwapchain = Nothing }
	where
	imgc = clamp 0 imgcx (Vk.Khr.Sfc.capabilitiesMinImageCount cps + 1)
	imgcx = fromMaybe maxBound
		. onlyIf (> 0) $ Vk.Khr.Sfc.capabilitiesMaxImageCount cps
	(ism, qfis) = (Vk.SharingModeExclusive, [])

createImgVws :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Img.Binded ss ss inm fmt] ->
	(forall si . HPList.PL (Vk.ImgVw.I inm fmt) si -> IO a) -> IO a
createImgVws _dv [] f = f HPList.Nil
createImgVws dv (i : is) f =
	Vk.ImgVw.create dv (imgVwInfo i) nil \v ->
	createImgVws dv is \vs -> f $ v :** vs

recreateImgVws :: Vk.T.FormatToValue fmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss inm fmt] ->
	HPList.PL (Vk.ImgVw.I inm fmt) sis -> IO ()
recreateImgVws _dv [] HPList.Nil = pure ()
recreateImgVws dv (i : is) (v :** vs) =
	Vk.ImgVw.unsafeRecreate dv (imgVwInfo i) nil v >>
	recreateImgVws dv is vs
recreateImgVws _ _ _ =
	error "number of Vk.Img.I and Vk.ImageView.I should be same"

imgVwInfo :: Vk.Img.Binded sm si nm ifmt ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt vfmt
imgVwInfo i = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = i,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = def,
	Vk.ImgVw.createInfoSubresourceRange = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 } }

{-
recreateAll :: (RecreateFrmbffrs svs sfs, Vk.T.FormatToValue fmt) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QFamIndices ->
	Vk.Dvc.D sd -> Vk.Khr.Swpch.S fmt ssc ->
	HPList.PL (Vk.ImgVw.I nm fmt) svs ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[] '[] -> Vk.Ppl.Gr.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] '(sl, '[], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateAll w sfc pd qfis dv sc vs rp pl gp fbs = do
	waitFramebufferSize w >> Vk.Dvc.waitIdle dv
	ex <- recreateSwpch w sfc pd qfis dv sc
	ex <$ do
		Vk.Khr.Swpch.getImages dv sc >>= \is -> recreateImgVws dv is vs
		recreateGrPpl dv ex rp pl gp
		recreateFrmbffrs dv ex rp vs fbs
-}

createCmdPl :: Vk.QFm.Index -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfi dv = Vk.CmdPl.create dv info nil
	where info = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

createCmdBffrs :: Vk.Dvc.D sd -> HPList.PL Vk.CmdPl.C scps ->
	(forall scbs . HPList.PL Vk.CmdBffr.C scbs -> IO a) -> IO a
createCmdBffrs _ HPList.Nil f = f HPList.Nil
createCmdBffrs dv (cp :** cps) f =
	createCmdBffr dv cp \cb -> createCmdBffrs dv cps \cbs -> f $ cb :** cbs

createCmdBffr :: Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb . Vk.CmdBffr.C scb -> IO a) -> IO a
createCmdBffr dv cp f = Vk.CmdBffr.allocateCs @_ @'[ '()] dv info \(cb :*. HPList.Nil) -> f cb
	where info = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }

data SyncObjs (ssos :: ([Type], [Type], [Type])) where
	SyncObjs :: {
		_swapchainSemaphore :: HPList.PL Vk.Semaphore.S sscss,
		_renderSemaphore :: HPList.PL Vk.Semaphore.S srss,
		_renderFence :: HPList.PL Vk.Fence.F srfs } ->
		SyncObjs '(sscss, srss, srfs)

createSyncObjs :: forall n sd a . HPList.RepM n =>
	Vk.Dvc.D sd -> (forall ssos . SyncObjs ssos -> IO a) -> IO a
createSyncObjs dv f =
	HPList.repM @n (Vk.Semaphore.create @'Nothing dv def nil) \scss ->
	HPList.repM @n (Vk.Semaphore.create @'Nothing dv def nil) \rss ->
	HPList.repM @n (Vk.Fence.create @'Nothing dv finfo nil) \rfs ->
	f $ SyncObjs scss rss rfs
	where
	finfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

catchAndRecreate :: Vk.T.FormatToValue fmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> Vk.QFm.Index ->
	Vk.Dvc.D sd -> Vk.Khr.Swpch.S fmt ssc ->
	([Vk.Img.Binded ssc ssc nm fmt] -> IO ()) -> IO () -> IO ()
catchAndRecreate w sfc pd qfis dv sc go act = catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just (); _ -> Nothing) act
	\_ -> do
		_ <- recreateAll w sfc pd qfis dv sc
		go =<< Vk.Khr.Swpch.getImages dv sc

recreateAll :: Vk.T.FormatToValue fmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> Vk.QFm.Index ->
	Vk.Dvc.D sd -> Vk.Khr.Swpch.S fmt ssc ->
	IO Vk.Extent2d
recreateAll w sfc pd qfis dv sc = do
	waitFramebufferSize w >> Vk.Dvc.waitIdle dv
	recreateSwpch w sfc pd qfis dv sc

waitFramebufferSize :: GlfwG.Win.W sw -> IO ()
waitFramebufferSize w = GlfwG.Win.getFramebufferSize w >>= \sz ->
	when (zero sz) $ fix \go -> (`when` go) . zero =<<
		GlfwG.waitEvents *> GlfwG.Win.getFramebufferSize w
	where zero = uncurry (||) . ((== 0) *** (== 0))

draw :: forall sd scfmt ssc ss inm scbs ssos .
	Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S scfmt ssc -> [Vk.Img.Binded ss ss inm scfmt] -> Vk.Q.Q ->
	HPList.PL Vk.CmdBffr.C scbs ->
	SyncObjs ssos -> Int -> Int -> IO ()
draw dv sc scis q cbs (SyncObjs scss rss  rfs) cf fn =
	HPList.index cbs cf \cb ->
	HPList.index scss cf \scs -> HPList.index rss cf \rs ->
	HPList.index rfs cf \rf -> let rf' = HPList.Singleton rf in
	Vk.Fence.waitForFs dv rf' True (Just 1) >> Vk.Fence.resetFs dv rf' >>
	Vk.Khr.acquireNextImageResult
		[Vk.Success, Vk.SuboptimalKhr]
		dv sc maxBound (Just scs) Nothing >>= \ii ->
--	print ii >>
	Vk.CmdBffr.reset cb def >>
	Vk.CmdBffr.begin @'Nothing @'Nothing cb binfo do
		transitionImage cb (scis !! fromIntegral ii) Vk.Img.LayoutUndefined Vk.Img.LayoutGeneral
--		let	flash = sin (fromIntegral fn / 120) / 2 + 0.5
		let	flash = sin (fromIntegral fn / 720) / 2 + 0.5
--		print flash
		let	clearValue = Vk.ClearValueColor . fromJust $ rgbaDouble 0 0 flash 1
			clearRange = imageSubresourceRange Vk.Img.AspectColorBit
		Vk.Cmd.clearColorImage @Vk.ClearColorTypeFloat32 cb (scis !! fromIntegral ii) Vk.Img.LayoutGeneral clearValue [clearRange]
		transitionImage cb (scis !! fromIntegral ii) Vk.Img.LayoutGeneral Vk.Img.LayoutPresentSrcKhr
	>>
--	Vk.Q.submit q (HPList.Singleton . U4 $ sinfo cb scs) Nothing -- $ Just iff
	Vk.Q.submit2 q (HPList.Singleton . U4 $ submit cb scs rs) (Just rf) -- Nothing -- $ Just iff
--	pure ()
	>>
	catchAndSerialize (Vk.Khr.Swpch.queuePresent @'Nothing q $ pinfo ii rs)
	where
	binfo = Vk.CmdBffr.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }
--	siff = HPList.Singleton iff
	sinfo :: forall s sscs . Vk.CmdBffr.C s -> Vk.Semaphore.S sscs -> Vk.SubmitInfo 'Nothing '[sscs] '[s] '[]
	sinfo cb scs = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks =
			HPList.Singleton $ Vk.SemaphorePipelineStageFlags
				scs Vk.Ppl.StageColorAttachmentOutputBit,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil } -- HPList.Singleton rfs }
	pinfo :: forall s . Word32 -> Vk.Semaphore.S s -> Vk.Khr.Swpch.PresentInfo 'Nothing '[s] scfmt '[ssc]
	pinfo ii rs = Vk.Khr.Swpch.PresentInfo {
		Vk.Khr.Swpch.presentInfoNext = TMaybe.N,
		Vk.Khr.Swpch.presentInfoWaitSemaphores = HPList.Singleton rs, -- HPList.Nil, -- HPList.Singleton rfs,
		Vk.Khr.Swpch.presentInfoSwapchainImageIndices =
			HPList.Singleton $ Vk.Khr.Swpch.SwapchainImageIndex sc ii }
	submit cb scs rs = submitInfo (cmdinfo cb) (signalinfo rs) (waitinfo scs)
	cmdinfo cb = commandBufferSubmitInfo cb
	waitinfo scs = semaphoreSubmitInfo Vk.Ppl.Stage2ColorAttachmentOutputBit scs
	signalinfo rs = semaphoreSubmitInfo Vk.Ppl.Stage2AllGraphicsBit rs

submitInfo :: Vk.CmdBffr.SubmitInfo mncb scb ->
	Vk.Semaphore.SubmitInfo mnss ss -> Vk.Semaphore.SubmitInfo mnws ws ->
	Vk.SubmitInfo2 'Nothing '[ '(mnws, ws)] '[ '(mncb, scb)] '[ '(mnss, ss)]
submitInfo cb ss ws = Vk.SubmitInfo2 {
	Vk.submitInfo2Next = TMaybe.N,
	Vk.submitInfo2Flags = zeroBits,
	Vk.submitInfo2WaitSemaphoreInfos = HPList.Singleton $ U2 ws,
	Vk.submitInfo2CommandBufferInfos = HPList.Singleton $ U2 cb,
	Vk.submitInfo2SignalSemaphoreInfos = HPList.Singleton $ U2 ss }

semaphoreSubmitInfo ::
	Vk.Ppl.StageFlags2 -> Vk.Semaphore.S ss ->
	Vk.Semaphore.SubmitInfo 'Nothing ss
semaphoreSubmitInfo sm smp = Vk.Semaphore.SubmitInfo {
	Vk.Semaphore.submitInfoNext = TMaybe.N,
	Vk.Semaphore.submitInfoSemaphore = smp,
	Vk.Semaphore.submitInfoValue = 1,
	Vk.Semaphore.submitInfoStageMask = sm,
	Vk.Semaphore.submitInfoDeviceIndex = 0 }

commandBufferSubmitInfo ::
	Vk.CmdBffr.C scb -> Vk.CmdBffr.SubmitInfo 'Nothing scb
commandBufferSubmitInfo cb = Vk.CmdBffr.SubmitInfo {
	Vk.CmdBffr.submitInfoNext = TMaybe.N,
	Vk.CmdBffr.submitInfoCommandBuffer = cb,
	Vk.CmdBffr.submitInfoDeviceMask = 0 }

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

transitionImage :: Vk.CmdBffr.C scb -> Vk.Img.Binded ss ss inm fmt -> Vk.Img.Layout -> Vk.Img.Layout -> IO ()
transitionImage cb img cl nl = Vk.Cmd.pipelineBarrier2 cb depInfo
	where
	depInfo = Vk.DependencyInfo {
		Vk.dependencyInfoNext = TMaybe.N,
		Vk.dependencyInfoDependencyFlags = zeroBits,
		Vk.dependencyInfoMemoryBarriers = HPList.Nil,
		Vk.dependencyInfoBufferMemoryBarriers = HPList.Nil,
		Vk.dependencyInfoImageMemoryBarriers = HPList.Singleton $ U5 imgBarrier }
	imgBarrier = Vk.Img.MemoryBarrier2 {
		Vk.Img.memoryBarrier2Next = TMaybe.N,
		Vk.Img.memoryBarrier2SrcStageMask = Vk.Ppl.Stage2AllCommandsBit,
		Vk.Img.memoryBarrier2SrcAccessMask = Vk.Access2MemoryWriteBit,
		Vk.Img.memoryBarrier2DstStageMask = Vk.Ppl.Stage2AllCommandsBit,
		Vk.Img.memoryBarrier2DstAccessMask =
			Vk.Access2MemoryWriteBit .|. Vk.Access2MemoryReadBit,
		Vk.Img.memoryBarrier2OldLayout = cl,
		Vk.Img.memoryBarrier2NewLayout = nl,
		Vk.Img.memoryBarrier2SrcQueueFamilyIndex = Vk.QFm.Ignored,
		Vk.Img.memoryBarrier2DstQueueFamilyIndex = Vk.QFm.Ignored,
		Vk.Img.memoryBarrier2Image = img,
		Vk.Img.memoryBarrier2SubresourceRange =
			imageSubresourceRange case nl of
				Vk.Img.LayoutDepthAttachmentOptimal ->
					Vk.Img.AspectDepthBit
				_ -> Vk.Img.AspectColorBit }
				

imageSubresourceRange :: Vk.Img.AspectFlags -> Vk.Img.SubresourceRange
imageSubresourceRange am = Vk.Img.SubresourceRange {
	Vk.Img.subresourceRangeAspectMask = am,
	Vk.Img.subresourceRangeBaseMipLevel = 0,
	Vk.Img.subresourceRangeLevelCount = 1,
	Vk.Img.subresourceRangeBaseArrayLayer = 0,
	Vk.Img.subresourceRangeLayerCount = 1 }
