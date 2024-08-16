{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.Generics
import GHC.TypeNats
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent.STM
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.Proxy
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)

import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.Bool
import Data.Maybe
import Data.Maybe.ToolsYj
import Data.List qualified as L
import Data.List.Length
import Data.Word
import Data.Color
import Data.Time

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as Txt
import qualified Graphics.UI.GLFW as Glfw hiding (createWindowSurface)
import qualified Gpu.Vulkan.Cglm as Cglm
import qualified Foreign.Storable.Generic

import qualified Language.SpirV as SpirV
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH


import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Exception as Vk
import qualified Gpu.Vulkan.Instance.Internal as Vk.Ist
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr
import qualified Gpu.Vulkan.Ext.DebugUtils as Vk.DbgUtls
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger as Vk.DbgUtls.Msngr
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam

import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Sfc
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as Vk.Khr.Sfc.Phd
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swpch
import qualified Gpu.Vulkan.Image as Vk.Img
import qualified Gpu.Vulkan.ImageView as Vk.ImgVw
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
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPss
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Framebuffer as Vk.Frmbffr
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPool
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBffr
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Mm.M
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet

import Tools hiding (onlyIf)

import Gpu.Vulkan.TypeEnum qualified as Vk.T

import Debug

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win

import Data.Ord.ToolsYj
import Data.Function.ToolsYj
import Data.Tuple.ToolsYj
import Data.List.ToolsYj
import Data.Bool.ToolsYj

import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win

main :: IO ()
main = atomically (newTVar False) >>= \fr -> withWindow fr \w ->
	createIst \ist -> bool id (dbgm ist) debug $ body fr w ist
	where dbgm i = Vk.DbgUtls.Msngr.create i dbgMsngrInfo nil

type FramebufferResized = TVar Bool

withWindow :: FramebufferResized -> (forall sw . GlfwG.Win.W sw -> IO a) -> IO a
withWindow fr a = GlfwG.init error $ GlfwG.Win.group \g -> a =<< initWindow fr g

initWindow :: FramebufferResized -> GlfwG.Win.Group s () -> IO (GlfwG.Win.W s)
initWindow fr g = do
	Right w <- do
		GlfwG.Win.hint noApi
		uncurryDup (GlfwG.Win.create' g ()) sizeName Nothing Nothing
	w <$ GlfwG.Win.setFramebufferSizeCallback
		w (Just . const3 . atomically $ writeTVar fr True)
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
	createLgDvc pd qfis \d gq pq ->
	createSwpch w sfc pd qfis d \(sc :: Vk.Khr.Swpch.S scifmt ss) ex ->
	Vk.Khr.Swpch.getImages d sc >>= \scis -> createImgVws d scis \scvs ->
	createRndrPss @scifmt d \rp ->
	unfrmBffrOstAlgn pd \(_ :: Proxy alu) ->
	createPplLyt @alu d \dsl pl -> createGrPpl d ex rp pl \gp ->
	createFrmbffrs d ex rp scvs \fbs ->
	createCmdPl qfis d \cp ->
	createVertexBuffer pd d gq cp \vb ->
	createIndexBuffer pd d gq cp \ib ->
	createUniformBuffer pd d \ub mbm ->
	createDescriptorPool d \dscp ->
	createDescriptorSet d dscp ub dsl \ds ->
	createCommandBuffer d cp \cb ->
	createSyncObjects d \sos ->
	getCurrentTime >>=
	mainloop fr w sfc pd qfis d gq pq
		sc ex scvs rp pl gp fbs vb ib mbm ds cb sos

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
	pure $ QFamIndices <$> (fst <$> L.find (grbit . snd) prps) <*> mp
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
	uniqueQFams = L.nub [grFam qfis, prFam qfis]
	qinfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }
	info qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames = bool [] vldLayers debug,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures = Just def }

createSwpch :: GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> (forall ss scfmt .
		Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swpch.S scfmt ss -> Vk.Extent2d -> IO a) -> IO a
createSwpch w sfc pd qfis dv f = querySwpchSupport pd sfc \ss -> do
	ex <- swapExtent w $ capabilities ss
	let	cps = capabilities ss
		pm = findDefault Vk.Khr.PresentModeFifo
			(== Vk.Khr.PresentModeMailbox) $ presentModes ss
	chooseSwpSfcFmt (formats ss)
		\(Vk.Khr.Sfc.Format sc :: Vk.Khr.Sfc.Format fmt) ->
		Vk.Khr.Swpch.create @_ @fmt dv
			(swpchInfo sfc qfis cps sc pm ex) nil (`f` ex)

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

chooseSwpSfcFmt :: (
	[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
	HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ) ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Khr.Sfc.Format fmt -> a) -> a
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`L.find` fmts)
	$ (== Vk.Khr.ColorSpaceSrgbNonlinear) . Vk.Khr.Sfc.formatColorSpace
chooseSwpSfcFmt (_, HPListC.Nil) _ = error "no available swap surface formats"

recreateSwpch :: forall sw ssfc sd fmt ssc . Vk.T.FormatToValue fmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> Vk.Khr.Swpch.S fmt ssc -> IO Vk.Extent2d
recreateSwpch win sfc phdvc qfis0 dvc sc = do
	ss <- querySwpchSupportFmt @fmt phdvc sfc
	ex <- swapExtent win $ capabilitiesFmt ss
	let	cps = capabilitiesFmt ss
		Vk.Khr.Sfc.Format cs = fromMaybe
			(error "no available swap surface formats")
			. listToMaybe $ formatsFmt ss
		pm = findDefault Vk.Khr.PresentModeFifo
			(== Vk.Khr.PresentModeMailbox) $ presentModesFmt ss
	ex <$ Vk.Khr.Swpch.unsafeRecreate dvc
		(swpchInfo @fmt sfc qfis0 cps cs pm ex) nil sc

data SwpchSupportDetailsFmt fmt = SwpchSupportDetailsFmt {
	capabilitiesFmt :: Vk.Khr.Sfc.Capabilities,
	formatsFmt :: [Vk.Khr.Sfc.Format fmt],
	presentModesFmt :: [Vk.Khr.PresentMode] } deriving Show

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
	Vk.Khr.Sfc.S ss -> QFamIndices -> Vk.Khr.Sfc.Capabilities ->
	Vk.Khr.ColorSpace -> Vk.Khr.PresentMode -> Vk.Extent2d ->
	Vk.Khr.Swpch.CreateInfo 'Nothing ss fmt
swpchInfo sfc qfis0 cps cs pm ex = Vk.Khr.Swpch.CreateInfo {
	Vk.Khr.Swpch.createInfoNext = TMaybe.N,
	Vk.Khr.Swpch.createInfoFlags = zeroBits,
	Vk.Khr.Swpch.createInfoSurface = sfc,
	Vk.Khr.Swpch.createInfoMinImageCount = imgc,
	Vk.Khr.Swpch.createInfoImageColorSpace = cs,
	Vk.Khr.Swpch.createInfoImageExtent = ex,
	Vk.Khr.Swpch.createInfoImageArrayLayers = 1,
	Vk.Khr.Swpch.createInfoImageUsage = Vk.Img.UsageColorAttachmentBit,
	Vk.Khr.Swpch.createInfoImageSharingMode = ism,
	Vk.Khr.Swpch.createInfoQueueFamilyIndices = qfis,
	Vk.Khr.Swpch.createInfoPreTransform =
		Vk.Khr.Sfc.capabilitiesCurrentTransform cps,
	Vk.Khr.Swpch.createInfoCompositeAlpha = Vk.Khr.CompositeAlphaOpaqueBit,
	Vk.Khr.Swpch.createInfoPresentMode = pm,
	Vk.Khr.Swpch.createInfoClipped = True,
	Vk.Khr.Swpch.createInfoOldSwapchain = Nothing }
	where
	imgc = clamp 0 imgcx (Vk.Khr.Sfc.capabilitiesMinImageCount cps + 1)
	imgcx = fromMaybe maxBound
		. onlyIf (> 0) $ Vk.Khr.Sfc.capabilitiesMaxImageCount cps
	(ism, qfis) = bool
		(Vk.SharingModeConcurrent, [grFam qfis0, prFam qfis0])
		(Vk.SharingModeExclusive, []) (grFam qfis0 == prFam qfis0)

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
	error "number of Vk.Img.I and Vk.ImgView.I should be same"

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

createRndrPss :: forall fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPss.R sr -> IO a) -> IO a
createRndrPss dv = Vk.RndrPss.create @'Nothing @'[fmt] dv info nil
	where
	info = Vk.RndrPss.CreateInfo {
		Vk.RndrPss.createInfoNext = TMaybe.N,
		Vk.RndrPss.createInfoFlags = zeroBits,
		Vk.RndrPss.createInfoAttachments = HPList.Singleton ca,
		Vk.RndrPss.createInfoSubpasses = [sbpss],
		Vk.RndrPss.createInfoDependencies = [dpnd] }
	ca = Vk.Att.Description {
		Vk.Att.descriptionFlags = zeroBits,
		Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
		Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
		Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
		Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
		Vk.Att.descriptionStencilStoreOp = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Att.descriptionFinalLayout = Vk.Img.LayoutPresentSrcKhr }
	sbpss = Vk.Subpass.Description {
		Vk.Subpass.descriptionFlags = zeroBits,
		Vk.Subpass.descriptionPipelineBindPoint =
			Vk.Ppl.BindPointGraphics,
		Vk.Subpass.descriptionInputAttachments = [],
		Vk.Subpass.descriptionColorAndResolveAttachments = Left [car],
		Vk.Subpass.descriptionDepthStencilAttachment = Nothing,
		Vk.Subpass.descriptionPreserveAttachments = [] }
	car = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 0,
		Vk.Att.referenceLayout = Vk.Img.LayoutColorAttachmentOptimal }
	dpnd = Vk.Subpass.Dependency {
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

unfrmBffrOstAlgn ::
	Vk.Phd.P -> (forall a . KnownNat a => Proxy a -> IO b) -> IO b
unfrmBffrOstAlgn pd f = (\(SomeNat p) -> f p) . someNatVal . fromIntegral
	. Vk.Phd.limitsMinUniformBufferOffsetAlignment . Vk.Phd.propertiesLimits
	=<< Vk.Phd.getProperties pd

createPplLyt :: forall alu sd a . Vk.Dvc.D sd -> (forall sl sdsl .
	Vk.DscSetLyt.D sdsl
		'[ 'Vk.DscSetLyt.Buffer '[VObj.Atom alu UniformBufferObject 'Nothing]] ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] -> IO a) -> IO a
createPplLyt dvc f =
	createDescriptorSetLayout dvc \dsl ->
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = TMaybe.N,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts =
				HPList.Singleton $ U2 dsl } in
	Vk.Ppl.Layout.create @'Nothing @_ @_ @'[] dvc pipelineLayoutInfo nil $ f dsl

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall s .
	Vk.DscSetLyt.D s '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom alu UniformBufferObject 'Nothing]]
	-> IO a) -> IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing
		'[ 'Vk.DscSetLyt.Buffer '[VObj.Atom alu UniformBufferObject 'Nothing] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = uboLayoutBinding :** HPList.Nil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[VObj.Atom alu UniformBufferObject 'Nothing])
	uboLayoutBinding = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

type AtomUbo s alu = '(s, '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom alu UniformBufferObject 'Nothing]])

createGrPpl :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec3), '(1, Cglm.Vec4)]
		'(sl, '[AtomUbo sdsl alu], '[]) -> IO a) -> IO a
createGrPpl dvc sce rp ppllyt f =
	Vk.Ppl.Graphics.createGs dvc Nothing (U14 pplInfo :** HPList.Nil)
			nil \(U3 gpl :** HPList.Nil) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt

recreateGraphicsPipeline' :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec3), '(1, Cglm.Vec4)]
		'(sl, '[AtomUbo sdsl alu], '[]) -> IO ()
recreateGraphicsPipeline' dvc sce rp ppllyt gpls = Vk.Ppl.Graphics.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HPList.Nil) nil (U3 gpls :** HPList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp ppllyt

mkGraphicsPipelineCreateInfo' ::
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing, '[ '(Vertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Cglm.Vec3), '(1, Cglm.Vec4)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing '(sl, '[AtomUbo sdsl alu], '[]) sr '(sb, vs', ts', foo)
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
	Vk.RndrPss.R sr -> HPList.PL (Vk.ImgVw.I nm fmt) sis ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HPList.Nil f = f HPList.Nil
createFrmbffrs dvc sce rp (iv :** ivs) f =
	Vk.Frmbffr.create dvc (mkFramebufferCreateInfoNew sce rp iv) nil \fb ->
	createFrmbffrs dvc sce rp ivs \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPss.R sr -> HPList.PL (Vk.ImgVw.I nm fmt) sis ->
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
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I nm fmt si ->
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

createVertexBuffer :: Vk.Phd.P ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPool.C sc -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] -> IO a ) -> IO a
createVertexBuffer phdvc dvc gq cp f =
	createBufferList' phdvc dvc (fromIntegral $ length vertices)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit \b _ ->
	createBufferList' phdvc dvc (fromIntegral $ length vertices)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) \(b' :: Vk.Bffr.Binded sm sb "vertex-buffer" '[VObj.List 256 t ""]) bm' -> do
	Vk.Mm.write @"vertex-buffer" @(VObj.List 256 Vertex "") @0 dvc bm' zeroBits vertices
	copyBuffer dvc gq cp b' b
	f b

createIndexBuffer :: Vk.Phd.P ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPool.C sc -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 Word16 ""] -> IO a) -> IO a
createIndexBuffer phdvc dvc gq cp f =
	createBufferList' phdvc dvc (fromIntegral $ length indices)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageIndexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit \b _ ->
	createBufferList' phdvc dvc (fromIntegral $ length indices)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) \(b' :: Vk.Bffr.Binded sm sb "vertex-buffer" '[VObj.List 256 t ""]) bm' -> do
	Vk.Mm.write @"vertex-buffer" @(VObj.List 256 Word16 "") @0 dvc bm' zeroBits indices
	copyBuffer dvc gq cp b' b
	f b

createUniformBuffer :: KnownNat alu => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
		Vk.Bffr.Binded sm sb "uniform-buffer" '[VObj.Atom alu UniformBufferObject 'Nothing]  ->
		UniformBufferMemory alu sm sb ->
		IO b) -> IO b
createUniformBuffer phdvc dvc = createBufferAtom' phdvc dvc
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

type UniformBufferMemory alu sm sb = Vk.Mm.M sm '[ '(
	sb,
	'Vk.Mm.BufferArg "uniform-buffer" '[VObj.Atom alu UniformBufferObject 'Nothing]
	)]

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDescriptorPool dvc = Vk.DscPool.create dvc poolInfo nil
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

createDescriptorSet :: KnownNat alu =>
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> Vk.Bffr.Binded sm sb nm '[VObj.Atom alu UniformBufferObject 'Nothing] ->
	Vk.DscSetLyt.D sdsc '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom alu UniformBufferObject 'Nothing]] ->
	(forall sds .
		Vk.DscSet.D sds '(sdsc, '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom alu UniformBufferObject 'Nothing]]) -> IO a) -> IO a
createDescriptorSet dvc dscp ub dscslyt f =
	Vk.DscSet.allocateDs dvc allocInfo \(HPList.Singleton dscs) -> do
	Vk.DscSet.updateDs dvc
		(HPList.Singleton . U5 $ descriptorWrite ub dscs) HPList.Nil
	f dscs
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts =
			HPList.Singleton $ U2 dscslyt }

descriptorWrite :: KnownNat alu =>
	Vk.Bffr.Binded sm sb nm '[VObj.Atom alu UniformBufferObject 'Nothing] ->
	Vk.DscSet.D sds slbts ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, VObj.Atom alu UniformBufferObject 'Nothing, 0)]) 0
descriptorWrite ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HPList.Singleton bufferInfo }
	where bufferInfo = U5 $ Vk.Dsc.BufferInfo ub

type ModelViewProjMemory sm sb mnm alm =
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg mnm '[AtomModelViewProj alm])]

type BufferModelViewProj alm = 'Vk.DscSetLyt.Buffer '[AtomModelViewProj alm]

type AtomModelViewProj alm = VObj.Atom alm WModelViewProj 'Nothing

createBufferAtom' :: forall sd nm a b alu . (Storable a, KnownNat alu) => Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.Atom alu a 'Nothing] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[VObj.Atom alu a 'Nothing] )] ->
			IO b) -> IO b
createBufferAtom' p dv usg props = createBuffer' p dv VObj.LengthAtom usg props

createBufferList' :: forall sd nm t a . Storable t =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 t ""] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[VObj.List 256 t ""] ) ] ->
		IO a) ->
	IO a
createBufferList' p dv ln usg props =
	createBuffer' p dv (VObj.LengthList ln) usg props

createBuffer' :: forall sd nm o a . VObj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer' p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mm.M.requirementsMemoryTypeBits reqs) props
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(allcInfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HPList.Singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mm.M.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
	allcInfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

findMemoryType :: Vk.Phd.P -> Vk.Mm.M.TypeBits -> Vk.Mm.PropertyFlags ->
	IO Vk.Mm.M.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.Phd.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> L.find ((&&)
		<$> (`Vk.Mm.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mm.M.mTypePropertyFlags . snd) tps
		where tps = Vk.Phd.memoryPropertiesMemoryTypes props1

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a alu . (Storable' a, KnownNat alu) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[VObj.List alu a ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List alu a ""] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CmdBffr.allocate
		dvc allocInfo \(cb :*. HPList.Nil) -> do
		let	submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks =
					HPList.Nil,
				Vk.submitInfoCommandBuffers =
					HPList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HPList.Nil }
		Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo do
			Vk.Cmd.copyBuffer @'[ '( '[VObj.List alu a ""], 0, 0)] cb src dst
		Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) Nothing
		Vk.Q.waitIdle gq
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

createCommandBuffer ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPool.C scp ->
	(forall scb . Vk.CmdBffr.C scb -> IO a) ->
	IO a
createCommandBuffer dvc cp f = Vk.CmdBffr.allocate dvc allocInfo $ f . \(cb :*. HPList.Nil) -> cb
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

createSyncObjects ::
	Vk.Dvc.D sd -> (forall sias srfs siff . SyncObjects '(sias, srfs, siff) -> IO a ) -> IO a
createSyncObjects dvc f =
	Vk.Semaphore.create @'Nothing dvc def nil \ias ->
	Vk.Semaphore.create @'Nothing dvc def nil \rfs ->
	Vk.Fence.create @'Nothing dvc fncInfo nil \iff ->
	f $ SyncObjects ias rfs iff
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sl sg sm sb nm sm' sb' nm' sdsl sds alu .
	Vk.CmdBffr.C scb ->
	Vk.RndrPss.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec3), '(1, Cglm.Vec4)] '(sl, '[AtomUbo sdsl alu], '[]) ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	Vk.DscSet.D sds (AtomUbo sdsl alu) ->
	IO ()
recordCommandBuffer cb rp fb sce ppllyt gpl vb ib ubds =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb
		(HPList.Singleton . U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @Vertex @"" vb) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word16 @"" ib) >>
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics ppllyt
		(HPList.Singleton $ U2 ubds)
		(HPList.Singleton (
			HPList.Nil :**
			HPList.Nil )) >>
	Vk.Cmd.drawIndexed cbb (fromIntegral $ length indices) 1 0 0 0
	where
	rpInfo :: Vk.RndrPss.BeginInfo 'Nothing sr sf
		'[ 'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32]
	rpInfo = Vk.RndrPss.BeginInfo {
		Vk.RndrPss.beginInfoNext = TMaybe.N,
		Vk.RndrPss.beginInfoRenderPass = rp,
		Vk.RndrPss.beginInfoFramebuffer = fb,
		Vk.RndrPss.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = sce },
		Vk.RndrPss.beginInfoClearValues = HPList.Singleton
			. Vk.ClearValueColor . fromJust $ rgbaDouble 0 0 0 1 }

mainloop :: (RecreateFramebuffers ss sfs, Vk.T.FormatToValue scfmt, KnownNat alu) =>
	FramebufferResized ->
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d -> HPList.PL (Vk.ImgVw.I nm scfmt) ss ->
	Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] -> Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec3), '(1, Cglm.Vec4)]
		'(sl, '[AtomUbo sdsl alu], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory alu sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl alu) ->
	Vk.CmdBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> UTCTime -> IO ()
mainloop g (GlfwG.Win.W w) sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm0 = do
	($ ext0) $ fix \loop ext -> do
		Glfw.pollEvents
		tm <- getCurrentTime
		runLoop w sfc phdvc qfis dvc gq pq
			sc g ext scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs
			(realToFrac $ tm `diffUTCTime` tm0) loop
	Vk.Dvc.waitIdle dvc

runLoop :: (RecreateFramebuffers sis sfs, Vk.T.FormatToValue scfmt, KnownNat alu) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> FramebufferResized -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec3), '(1, Cglm.Vec4)]
		'(sl, '[AtomUbo sdsl alu], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory alu sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl alu) ->
	Vk.CmdBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> Float ->
	(Vk.Extent2d -> IO ()) -> IO ()
runLoop win sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm loop = do
	catchAndRecreate win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop
		$ drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm
	cls <- Glfw.windowShouldClose win
	if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateAll
			win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs)

drawFrame :: forall sfs sd ssc sr sl sg sm sb nm sm' sb' nm' sm2 sb2 scb sias srfs siff sdsl scfmt sds alu .
	KnownNat alu =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec3), '(1, Cglm.Vec4)]
		'(sl, '[AtomUbo sdsl alu], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory alu sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl alu) ->
	Vk.CmdBffr.C scb -> SyncObjects '(sias, srfs, siff) -> Float -> IO ()
drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib ubm ubds cb (SyncObjects ias rfs iff) tm = do
	let	siff = HPList.Singleton iff
	Vk.Fence.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HPList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext ppllyt gpl vb ib ubds
	updateUniformBuffer dvc ubm ext tm
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
	Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfo

updateUniformBuffer :: forall sd alu sm2 sb2 . KnownNat alu => Vk.Dvc.D sd ->
	UniformBufferMemory alu sm2 sb2 -> Vk.Extent2d -> Float -> IO ()
updateUniformBuffer dvc um sce tm = do
	Vk.Mm.write @"uniform-buffer" @(VObj.Atom alu UniformBufferObject 'Nothing) @0
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
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec3), '(1, Cglm.Vec4)]
		'(sl, '[AtomUbo sdsl alu], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	(Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateAll
		win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs

recreateAll :: (
	RecreateFramebuffers sis sfs, Vk.T.FormatToValue scfmt ) =>
	Glfw.Window -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec3), '(1, Cglm.Vec4)]
		'(sl, '[AtomUbo sdsl alu], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateAll win sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwpch (GlfwG.Win.W win) sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swpch.getImages dvc sc >>= \imgs ->
			recreateImgVws dvc imgs scivs
		recreateGraphicsPipeline' dvc ext rp ppllyt gpl
		recreateFramebuffers dvc ext rp scivs fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize win = Glfw.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

data Vertex = Vertex { vertexPos :: Cglm.Vec3, vertexColor :: Cglm.Vec4 }
	deriving (Show, Generic)

instance Storable Vertex where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance Foreign.Storable.Generic.G Vertex where

vertices :: [Vertex]
vertices = [
	Vertex (Cglm.Vec3 $ (- 0.5) :. (- 0.5) :. 0 :. NilL)
		(Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Cglm.Vec3 $ 0.5 :. (- 0.5) :. 0 :. NilL)
		(Cglm.Vec4 $ 0.0 :. 1.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Cglm.Vec3 $ 0.5 :. 0.5 :. 0 :. NilL)
		(Cglm.Vec4 $ 0.0 :. 0.0 :. 1.0 :. 1.0 :. NilL),
	Vertex (Cglm.Vec3 $ (- 0.5) :. 0.5 :. 0 :. NilL)
		(Cglm.Vec4 $ 1.0 :. 1.0 :. 1.0 :. 1.0 :. NilL) ]

indices :: [Word16]
indices = [0, 1, 2, 2, 3, 0]

-- type WModelViewProj = Foreign.Storable.Generic.W ModelViewProj
type WModelViewProj = Foreign.Storable.Generic.W UniformBufferObject

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

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec4 inColor;

layout(location = 0) out vec4 fragColor;

void
main()
{
	gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 1.0);
	fragColor = inColor;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec4 fragColor;

layout(location = 0) out vec4 outColor;

void
main()
{
	outColor = fragColor;
}

|]
