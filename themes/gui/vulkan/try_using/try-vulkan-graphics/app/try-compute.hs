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
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.Generics
import GHC.TypeNats
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.Generic qualified as GStorable
import Foreign.Storable.PeekPoke
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Data.Proxy
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.List qualified as TList
import Data.Foldable
import Data.MonoTraversable (Element, olength)
import Data.Sequences (IsSequence)
import Data.Default
import Data.Ord.ToolsYj
import Data.Bits
import Data.Bits.ToolsYj
import Data.Function.ToolsYj
import Data.Tuple.ToolsYj
import Data.Bool
import Data.Bool.ToolsYj
import Data.Maybe
import Data.Maybe.ToolsYj
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.List.Infinite qualified as Inf
import Data.List.Infinite (pattern (:~))
import Data.List.Length
import Data.List.ToolsYj
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.Array hiding (indices)
import Data.Word
import Data.Text.IO qualified as Txt
import Data.Time
import Data.Color
import Data.IORef
import Data.IORef.ToolsYj
import Codec.Picture

import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH
import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object qualified as Vk.Obj
import Gpu.Vulkan.Object.NoAlignment qualified as Vk.ObjNA
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.Exception qualified as Vk
import Gpu.Vulkan.Instance qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Framebuffer qualified as Vk.Frmbffr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.Semaphore qualified as Vk.Semaphore
import Gpu.Vulkan.Fence qualified as Vk.Fence

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Pipeline.Graphics qualified as Vk.Ppl.Graphics
import Gpu.Vulkan.Pipeline.Compute qualified as Vk.Ppl.Cmpt
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.InputAssemblyState qualified as Vk.Ppl.InpAsmbSt
import Gpu.Vulkan.Pipeline.ViewportState qualified as Vk.Ppl.ViewportSt
import Gpu.Vulkan.Pipeline.RasterizationState qualified as Vk.Ppl.RstSt
import Gpu.Vulkan.Pipeline.MultisampleState qualified as Vk.Ppl.MltSmplSt
import Gpu.Vulkan.Pipeline.ColorBlendAttachment qualified as Vk.Ppl.ClrBlndAtt
import Gpu.Vulkan.Pipeline.ColorBlendState qualified as Vk.Ppl.ClrBlndSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.PplLyt
import Gpu.Vulkan.PushConstant qualified as Vk.PshCnst
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderModule
import Gpu.Vulkan.VertexInput qualified as Vk.VtxInp
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.ColorComponent qualified as Vk.ClrCmp
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPss
import Gpu.Vulkan.Attachment qualified as Vk.Att
import Gpu.Vulkan.Subpass qualified as Vk.Subpass
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt

import Gpu.Vulkan.Cglm qualified as Glm
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch
import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.DbgUtls.Msngr

import Debug

import System.Random
import Gpu.Vulkan.Pipeline.VertexInputState qualified as Vk.Ppl.VertexInputSt

particleCount :: Integral n => n
-- particleCount = 8192
particleCount = 50

main :: IO ()
main = newIORef False >>= \fr -> withWindow fr \w ->
	createIst \ist -> bool id (dbgm ist) debug $ body fr w ist
	where dbgm i = Vk.DbgUtls.Msngr.create i dbgMsngrInfo nil

type FramebufferResized = IORef Bool

withWindow :: FramebufferResized -> (forall sw . GlfwG.Win.W sw -> IO a) -> IO a
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
	pickPhd ist sfc >>= \(pd, qfis, spcnt) ->
	createLgDvc pd qfis \d gq cq pq ->
	createCmdPl qfis d \cp ->
	createSwpch w sfc pd qfis d \(sc :: Vk.Khr.Swpch.S scifmt ss) ex ->
	Vk.Khr.Swpch.getImages d sc >>= \scis -> createImgVws d scis \scvs ->
	createClrRsrcs @scifmt pd d ex spcnt \crs@(_, _, cv, _) ->
	createRndrPss @scifmt d spcnt \rp ->
	unfrmBffrOstAlgn pd \(_ :: Proxy alu) ->
	createPplLyt @alu d \dsl pl -> createGrPpl d ex rp pl spcnt \gp ->
	createFrmbffrs d ex rp scvs cv \fbs ->
	let vtcs = vertices' $ mkStdGen 8 in
	createVtxBffr pd d gq cp vtcs \vbs ->
	tnum maxFramesInFlight \(_ :: Proxy mff) ->
	createMvpBffrs' maxFramesInFlight pd d dsl \dsls mbs mbms ->
	createDscPl d \dp -> createDscSts d dp mbs dsls \dss ->
	Vk.CBffr.allocate @_ @mff d (cmdBffrInfo cp) \cbs ->
	createSyncObjs @mff d \sos ->

	createBffrAtm @1 @_ @_ @Float
		Vk.Bffr.UsageUniformBufferBit
		(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)
		pd d \bdt mdt ->
	createCmpPpl d \cdsl cpl cmppl ->

	getCurrentTime >>=
	mainloop fr w sfc pd qfis d gq pq
		sc ex scvs rp pl gp fbs crs vbs mbms dss cbs sos
	where
	tnum :: Int -> (forall (n :: [()]) . (
		TList.Length n, HPList.FromList n,
		HPList.HomoList '() n, HPList.RepM n ) => Proxy n -> a) -> a
	tnum 0 f = f (Proxy @'[])
	tnum n f = tnum (n - 1) \p -> f $ plus1 p
		where plus1 :: Proxy n -> Proxy ('() ': n); plus1 Proxy = Proxy

maxFramesInFlight :: Integral n => n
maxFramesInFlight = 2

pickPhd :: Vk.Ist.I si -> Vk.Khr.Sfc.S ss ->
	IO (Vk.Phd.P, QFamIndices, Vk.Sample.CountFlags)
pickPhd ist sfc = Vk.Phd.enumerate ist >>= \case
	[] -> error "failed to find GPUs with Gpu.Vulkan support!"
	pds -> findMaybeM suit pds >>= \case
		Nothing -> error "failed to find a suitable GPU!"
		Just (pd, qfi) -> (pd, qfi,) <$> mxSmplCnt pd
	where
	suit pd = espt pd >>= bool (pure Nothing) do
		qfis <- findQFams pd sfc
		querySwpchSupport pd sfc \ss -> pure . bool qfis Nothing
			$	HPListC.null (snd $ formats ss) ||
				null (presentModes ss)
	espt pd = elemAll dvcExtensions
		. (Vk.Phd.extensionPropertiesExtensionName <$>)
		<$> Vk.Phd.enumerateExtensionProperties pd Nothing
	mxSmplCnt pd = do
		cnts <- Vk.Phd.limitsFramebufferColorSampleCounts
			. Vk.Phd.propertiesLimits <$> Vk.Phd.getProperties pd
		pure . fromMaybe Vk.Sample.Count1Bit $ findBit [
			Vk.Sample.Count64Bit, Vk.Sample.Count32Bit,
			Vk.Sample.Count16Bit, Vk.Sample.Count8Bit,
			Vk.Sample.Count4Bit, Vk.Sample.Count2Bit ] cnts
	findBit bl bs = find ((/= zeroBits) . (.&. bs)) bl

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
	where grbit = checkBits (Vk.Q.GraphicsBit .|. Vk.Q.ComputeBit)
		. Vk.QFam.propertiesQueueFlags

createLgDvc :: Vk.Phd.P -> QFamIndices ->
	(forall sd . Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Q.Q -> IO a) -> IO a
createLgDvc pd qfis act = hetero qinfo uniqueQFams \qs ->
	Vk.Dvc.create pd (info qs) nil \dv -> join $ act dv
		<$> Vk.Dvc.getQueue dv (grFam qfis) 0
		<*> Vk.Dvc.getQueue dv (grFam qfis) 0
		<*> Vk.Dvc.getQueue dv (prFam qfis) 0
	where
	hetero :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] -> (forall ss .
		HPList.ToListWithCM' WithPoked TMaybe.M ss =>
		HPList.PL t ss -> b) -> b
	hetero _k [] f = f HPList.Nil
	hetero k (x : xs) f = hetero k xs \xs' -> f (k x :** xs')
	qinfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }
	uniqueQFams = L.nub [grFam qfis, prFam qfis]
	info qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames = bool [] vldLayers debug,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures = Just def {
			Vk.Phd.featuresSamplerAnisotropy = True,
			Vk.Phd.featuresSampleRateShading = True } }

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

querySwpchSupport :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> (forall fmts .
	Show (HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts) =>
	SwpchSupportDetails fmts -> IO a) -> IO a
querySwpchSupport pd sfc f = Vk.Khr.Sfc.Phd.getFormats pd sfc \fmts ->
	f =<< SwpchSupportDetails
		<$> Vk.Khr.Sfc.Phd.getCapabilities pd sfc
		<*> ((, fmts) <$> Vk.Khr.Sfc.Phd.getFormatsFiltered pd sfc)
		<*> Vk.Khr.Sfc.Phd.getPresentModes pd sfc

data SwpchSupportDetails fmts = SwpchSupportDetails {
	capabilities :: Vk.Khr.Sfc.Capabilities,
	formats :: (
		[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
		HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ),
	presentModes :: [Vk.Khr.PresentMode] }

deriving instance
	Show (HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts) =>
	Show (SwpchSupportDetails fmts)

chooseSwpSfcFmt :: (
	[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
	HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ) ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Khr.Sfc.Format fmt -> a) -> a
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`find` fmts)
	$ (== Vk.Khr.ColorSpaceSrgbNonlinear) . Vk.Khr.Sfc.formatColorSpace
chooseSwpSfcFmt (_, HPListC.Nil) _ = error "no available swap surface formats"

recreateSwpch :: forall sw ssfc sd fmt ssc . Vk.T.FormatToValue fmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> Vk.Khr.Swpch.S fmt ssc -> IO Vk.Extent2d
recreateSwpch win sfc pd qfis0 dvc sc = do
	ss <- querySwpchSupportFmt @fmt pd sfc
	ex <- swapExtent win $ capabilitiesFmt ss
	let	cps = capabilitiesFmt ss
		Vk.Khr.Sfc.Format cs = fromMaybe
			(error "no available swap surface formats")
			. listToMaybe $ formatsFmt ss
		pm = findDefault Vk.Khr.PresentModeFifo
			(== Vk.Khr.PresentModeMailbox) $ presentModesFmt ss
	ex <$ Vk.Khr.Swpch.unsafeRecreate dvc
		(swpchInfo @fmt sfc qfis0 cps cs pm ex) nil sc

querySwpchSupportFmt :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (SwpchSupportDetailsFmt fmt)
querySwpchSupportFmt dvc sfc = SwpchSupportDetailsFmt
	<$> Vk.Khr.Sfc.Phd.getCapabilities dvc sfc
	<*> Vk.Khr.Sfc.Phd.getFormatsFiltered dvc sfc
	<*> Vk.Khr.Sfc.Phd.getPresentModes dvc sfc

data SwpchSupportDetailsFmt fmt = SwpchSupportDetailsFmt {
	capabilitiesFmt :: Vk.Khr.Sfc.Capabilities,
	formatsFmt :: [Vk.Khr.Sfc.Format fmt],
	presentModesFmt :: [Vk.Khr.PresentMode] } deriving Show

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

createClrRsrcs :: forall fmt sd nm a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Extent2d -> Vk.Sample.CountFlags ->
	(forall si sm siv . ClrRsrcs fmt nm si sm siv -> IO a) -> IO a
createClrRsrcs pd dv (Vk.Extent2d w h) spcnt f =
	prepareImg pd dv Vk.Img.TilingOptimal
		(	Vk.Img.UsageTransientAttachmentBit .|.
			Vk.Img.UsageColorAttachmentBit )
		Vk.Mm.PropertyDeviceLocalBit w h 1 spcnt \ci cm ->
	Vk.ImgVw.create dv (imgVwInfo ci Vk.Img.AspectColorBit 1) nil \cv ->
	f (ci, cm, cv, spcnt)

recreateClrRsrcs :: forall fmt sd nm si sm siv . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Extent2d -> ClrRsrcs fmt nm si sm siv ->
	IO ()
recreateClrRsrcs pd dv (Vk.Extent2d w h) (ci, cm, cv, spcnt) = do
	reprepareImg pd dv Vk.Img.TilingOptimal
		(	Vk.Img.UsageTransientAttachmentBit .|.
			Vk.Img.UsageColorAttachmentBit )
		Vk.Mm.PropertyDeviceLocalBit w h 1 spcnt ci cm
	Vk.ImgVw.unsafeRecreate dv (imgVwInfo ci Vk.Img.AspectColorBit 1) nil cv

type ClrRsrcs fmt nm si sm siv = (
	Vk.Img.Binded sm si nm fmt,
	Vk.Mm.M sm '[ '(si, 'Vk.Mm.ImageArg nm fmt)],
	Vk.ImgVw.I nm fmt siv, Vk.Sample.CountFlags )

createImgVws :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Img.Binded ss ss inm fmt] ->
	(forall si . HPList.PL (Vk.ImgVw.I inm fmt) si -> IO a) -> IO a
createImgVws _dv [] f = f HPList.Nil
createImgVws dv (i : is) f =
	Vk.ImgVw.create dv (imgVwInfo i Vk.Img.AspectColorBit 1) nil \v ->
	createImgVws dv is \vs -> f $ v :** vs

recreateImgVws :: Vk.T.FormatToValue fmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss inm fmt] ->
	HPList.PL (Vk.ImgVw.I inm fmt) sis -> IO ()
recreateImgVws _dv [] HPList.Nil = pure ()
recreateImgVws dv (i : is) (v :** vs) =
	Vk.ImgVw.unsafeRecreate dv (imgVwInfo i Vk.Img.AspectColorBit 1) nil v >>
	recreateImgVws dv is vs
recreateImgVws _ _ _ =
	error "number of Vk.Image.I and Vk.ImageView.I should be same"

imgVwInfo :: Vk.Img.Binded sm si nm ifmt -> Vk.Img.AspectFlags -> Word32 ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt vfmt
imgVwInfo i a ml = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = i,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = def,
	Vk.ImgVw.createInfoSubresourceRange = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = a,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = ml,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 } }

createRndrPss :: forall fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Sample.CountFlags ->
	(forall sr . Vk.RndrPss.R sr -> IO a) -> IO a
createRndrPss dv spcnt = Vk.RndrPss.create @_ @'[fmt, fmt] dv info nil
	where
	info = Vk.RndrPss.CreateInfo {
		Vk.RndrPss.createInfoNext = TMaybe.N,
		Vk.RndrPss.createInfoFlags = zeroBits,
		Vk.RndrPss.createInfoAttachments =
			ca :** rslv :** HPList.Nil,
		Vk.RndrPss.createInfoSubpasses = [sbpss],
		Vk.RndrPss.createInfoDependencies = [dpnd] }
	ca = Vk.Att.Description {
		Vk.Att.descriptionFlags = zeroBits,
		Vk.Att.descriptionSamples = spcnt,
		Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
		Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
		Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
		Vk.Att.descriptionStencilStoreOp = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Att.descriptionFinalLayout =
			Vk.Img.LayoutColorAttachmentOptimal }
	rslv = Vk.Att.Description {
		Vk.Att.descriptionFlags = zeroBits,
		Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
		Vk.Att.descriptionLoadOp = Vk.Att.LoadOpDontCare,
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
		Vk.Subpass.descriptionColorAndResolveAttachments =
			Right [(car, rslvr)],
		Vk.Subpass.descriptionDepthStencilAttachment = Nothing,
		Vk.Subpass.descriptionPreserveAttachments = [] }
	car = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 0,
		Vk.Att.referenceLayout = Vk.Img.LayoutColorAttachmentOptimal }
	rslvr = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 1,
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
			Vk.AccessColorAttachmentWriteBit,
		Vk.Subpass.dependencyDependencyFlags = zeroBits }

unfrmBffrOstAlgn ::
	Vk.Phd.P -> (forall a . KnownNat a => Proxy a -> IO b) -> IO b
unfrmBffrOstAlgn pd f = (\(SomeNat p) -> f p) . someNatVal . fromIntegral
	. Vk.Phd.limitsMinUniformBufferOffsetAlignment . Vk.Phd.propertiesLimits
	=<< Vk.Phd.getProperties pd

createCmpPpl :: Vk.Dvc.D sd -> (forall sds scmpp spl .
	Vk.DscStLyt.D sds [
		'Vk.DscStLyt.Buffer '[Vk.Obj.AtomNew 1 Float nmdt],
		Vk.DscStLyt.Buffer '[Vk.Obj.List 1 WVertex nmh],
		Vk.DscStLyt.Buffer '[Vk.Obj.List 1 WVertex nmh]  ] ->
	Vk.PplLyt.P spl '[ '(sds, '[
		'Vk.DscStLyt.Buffer '[Vk.Obj.AtomNew 1 Float nmdt],
		'Vk.DscStLyt.Buffer '[Vk.Obj.List 1 WVertex nmh],
		'Vk.DscStLyt.Buffer '[Vk.Obj.List 1 WVertex nmh] ])] '[] ->
	Vk.Ppl.Cmpt.C scmpp
		'(	spl,
			'[ '( sds, '[	'Vk.DscStLyt.Buffer '[Vk.Obj.AtomNew 1 Float nmdt],
					Vk.DscStLyt.Buffer '[Vk.Obj.List 1 WVertex nmh],
					Vk.DscStLyt.Buffer '[Vk.Obj.List 1 WVertex nmh] ])],
			'[]) ->
	IO a) -> IO a
createCmpPpl dv f =
	createCmpPplLyt dv \dsl pl ->
	Vk.Ppl.Cmpt.createCs dv Nothing (HPList.Singleton . U4 $ cmpPplInfo pl) nil
		\(HPList.Singleton cmpp) ->
		f dsl pl cmpp

cmpPplInfo :: Vk.PplLyt.P sl sbtss '[] -> Vk.Ppl.Cmpt.CreateInfo 'Nothing
	'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[])
	'(sl, sbtss, '[]) sbph
cmpPplInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U5 cmpShaderStages,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

createCmpPplLyt :: Vk.Dvc.D sd -> (forall sds spl .
	Vk.DscStLyt.D sds [
		'Vk.DscStLyt.Buffer '[Vk.Obj.AtomNew 1 Float nmdt],
		Vk.DscStLyt.Buffer '[Vk.Obj.List 1 WVertex nmh],
		Vk.DscStLyt.Buffer '[Vk.Obj.List 1 WVertex nmh]  ] ->
	Vk.PplLyt.P spl '[ '(sds, CmpDscStLytArg nmdt nmh)] '[] ->
	IO a) -> IO a
createCmpPplLyt dv f =
	createCmpDscStLyt dv \dsl ->
	Vk.PplLyt.create dv (cmpPplLytInfo dsl) nil (f dsl)

cmpPplLytInfo :: Vk.DscStLyt.D sl bts -> Vk.PplLyt.CreateInfo
	'Nothing '[ '(sl, bts)] ('Vk.PshCnst.Layout '[] '[])
cmpPplLytInfo dsl = Vk.PplLyt.CreateInfo {
	Vk.PplLyt.createInfoNext = TMaybe.N,
	Vk.PplLyt.createInfoFlags = zeroBits,
	Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

createCmpDscStLyt :: Vk.Dvc.D sd -> (forall (sds :: Type) .
	Vk.DscStLyt.D sds (CmpDscStLytArg nmdt nmh) -> IO a) -> IO a
createCmpDscStLyt dv = Vk.DscStLyt.create dv cmpDscStLytInfo nil

type CmpDscStLytArg nmdt nmh =
	'[ BufferDiffTime nmdt, BufferVertex nmh, BufferVertex nmh ]

cmpDscStLytInfo :: Vk.DscStLyt.CreateInfo 'Nothing '[
	'Vk.DscStLyt.Buffer '[Vk.Obj.AtomNew 1 Float nmdt],
	'Vk.DscStLyt.Buffer '[Vk.Obj.List 1 WVertex nmh],
	'Vk.DscStLyt.Buffer '[Vk.Obj.List 1 WVertex nmh] ]
cmpDscStLytInfo = Vk.DscStLyt.CreateInfo {
	Vk.DscStLyt.createInfoNext = TMaybe.N,
	Vk.DscStLyt.createInfoFlags = zeroBits,
	Vk.DscStLyt.createInfoBindings = bdg0 :** bdg1 :** bdg1 :** HPList.Nil }
	where
	bdg0 = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }
	bdg1 = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

cmpRun :: forall slbts sc spl sg sds .
	(Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[], '[], '[]]]) =>
	Vk.Q.Q -> Vk.CBffr.C sc -> Vk.PplLyt.P spl '[slbts] '[] ->
	Vk.Ppl.Cmpt.C sg '(spl, '[slbts], '[]) ->
	Vk.DscSt.D sds slbts -> Word32 -> IO ()
cmpRun q cb pl cppl dss sz = do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute cppl \ccb ->
		Vk.Cmd.bindDescriptorSetsCompute
			ccb pl (HPList.Singleton $ U2 dss) def >>
		Vk.Cmd.dispatch ccb (sz `div` 256 + 1) 1 1
	Vk.Q.submit q (HPList.Singleton $ U4 sinfo) Nothing
	Vk.Q.waitIdle q
	where sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

newtype CmpDscSt sdsl nmdt nmh sds =
	CmpDscSt (Vk.DscSt.D sds '(sdsl, CmpDscStLytArg nmdt nmh))

createCmpDscSt' :: forall sd sp sdsl nmdt nmh sm0 sb0 bnmh0 a smsbbnm .
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.DscStLyt.D sdsl (CmpDscStLytArg nmdt nmh) ->
	Vk.Bffr.Binded sm0 sb0 bnmh0 '[AtomDiffTime nmdt] ->
	HPList.PL (VtxBffr' WVertex nmh) smsbbnm -> Int -> (forall sds .
		CmpDscSt sdsl nmdt nmh sds -> IO a) -> IO a
createCmpDscSt' dv dp dsl bf0 vbs i f =
	HPList.index vbs ((i - 1) `mod` maxFramesInFlight) \(U3 (VtxBffr lvb)) ->
	HPList.index vbs i \(U3 (VtxBffr cvb)) ->
	createCmpDscSt dv dp dsl bf0 lvb cvb (f . CmpDscSt)

createCmpDscSt :: forall sd sp sdsl nmdt nmh
	sm0 sb0 bnmh0
	sm1 sb1 bnmh1
	sm2 sb2 bnmh2 a
	.
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.DscStLyt.D sdsl '[
		BufferDiffTime nmdt,
		BufferVertex nmh,
		BufferVertex nmh
		] ->
	Vk.Bffr.Binded sm0 sb0 bnmh0 '[AtomDiffTime nmdt] ->
	Vk.Bffr.Binded sm1 sb1 bnmh1 '[ListVertex nmh] ->
	Vk.Bffr.Binded sm2 sb2 bnmh2 '[ListVertex nmh] ->
	(forall sds . Vk.DscSt.D sds '(sdsl, '[
		BufferDiffTime nmdt,
		BufferVertex nmh,
		BufferVertex nmh ]) -> IO a) -> IO a
createCmpDscSt dv dp dsl bf0 bf1 bf2 f =
	Vk.DscSt.allocateDs dv (cmpDscStInfo dp dsl) \(HPList.Singleton ds) ->
	Vk.DscSt.updateDs dv (
		U5 (cmpWriteDscStUniform @_ @nmdt ds bf0) :**
		U5 (cmpWriteDscStStorage0 @_ @nmh ds bf1) :**
		U5 (cmpWriteDscStStorage1 @_ @nmh ds bf2) :** HPList.Nil)
		HPList.Nil >>
	f ds

type BufferVertex nm = 'Vk.DscStLyt.Buffer '[ListVertex nm]
type ListVertex nm = Vk.Obj.List 1 WVertex nm

type BufferDiffTime nm = 'Vk.DscStLyt.Buffer '[AtomDiffTime nm]
type AtomDiffTime nm = Vk.Obj.AtomNew 1 Float nm

cmpDscStInfo :: Vk.DscPl.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.AllocateInfo 'Nothing sp '[ '(sl, bts)]
cmpDscStInfo dpl dsl = Vk.DscSt.AllocateInfo {
	Vk.DscSt.allocateInfoNext = TMaybe.N,
	Vk.DscSt.allocateInfoDescriptorPool = dpl,
	Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }

cmpWriteDscStUniform :: forall bnmh nmdt sds slbts sm sb os . (
	Show (HPList.PL Vk.Obj.Length os),
	Vk.Obj.OffsetRange (Vk.Obj.AtomNew 1 Float nmdt) os 0 ) =>
	Vk.DscSt.D sds slbts -> Vk.Bffr.Binded sm sb bnmh os ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer
		'[ '(sm, sb, bnmh, Vk.Obj.AtomNew 1 Float nmdt, 0)]) 0
cmpWriteDscStUniform ds bf = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSt.writeSources =
		Vk.DscSt.BufferInfos . HPList.Singleton . U5 $ Vk.Dsc.BufferInfo bf }

cmpWriteDscStStorage0 :: forall bnmh nmh sds slbts sm sb os . (
	Show (HPList.PL Vk.Obj.Length os),
	Vk.Obj.OffsetRange (Vk.Obj.List 1 WVertex nmh) os 0 ) =>
	Vk.DscSt.D sds slbts -> Vk.Bffr.Binded sm sb bnmh os ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer
		'[ '(sm, sb, bnmh, Vk.Obj.List 1 WVertex nmh, 0)]) 0
cmpWriteDscStStorage0 ds bf = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources =
		Vk.DscSt.BufferInfos . HPList.Singleton . U5 $ Vk.Dsc.BufferInfo bf }

cmpWriteDscStStorage1 :: forall bnmh nmh sds slbts sm sb os . (
	Show (HPList.PL Vk.Obj.Length os),
	Vk.Obj.OffsetRange (Vk.Obj.List 1 WVertex nmh) os 0 ) =>
	Vk.DscSt.D sds slbts -> Vk.Bffr.Binded sm sb bnmh os ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer
		'[ '(sm, sb, bnmh, Vk.Obj.List 1 WVertex nmh, 0)]) 1
cmpWriteDscStStorage1 ds bf = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources =
		Vk.DscSt.BufferInfos . HPList.Singleton . U5 $ Vk.Dsc.BufferInfo bf }

cmpShaderStages :: Vk.Ppl.ShdrSt.CreateInfo
	'Nothing 'Nothing 'GlslComputeShader 'Nothing '[]
cmpShaderStages = cinfo
	where
	cinfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = zeroBits,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageComputeBit,
		Vk.Ppl.ShdrSt.createInfoModule =
			(minfo glslComputeShaderMain, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }

type GlslComputeShaderArgs = '(
	'Nothing, 'Nothing,
	'GlslComputeShader, 'Nothing :: Maybe (Type, Type), '[] )

createPplLyt :: forall alu sd a . Vk.Dvc.D sd -> (forall sl sdsl .
	Vk.DscStLyt.D sdsl (DscStLytArg alu) ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] -> IO a) -> IO a
createPplLyt dv f = createDscStLyt dv \dsl ->
	Vk.PplLyt.create @_ @_ @_ @'[] dv (info dsl) nil $ f dsl
	where info dsl = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

createDscStLyt :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscStLyt.D s (DscStLytArg alu) -> IO a) -> IO a
createDscStLyt dv = Vk.DscStLyt.create dv info nil
	where
	info = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = mbd :** HPList.Nil }
	mbd = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

type DscStLytArg alu = '[BufferModelViewProj alu]
type BufferModelViewProj alu = 'Vk.DscStLyt.Buffer '[AtomModelViewProj alu]
type AtomModelViewProj alu = Vk.Obj.Atom alu WModelViewProj 'Nothing

createGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Sample.CountFlags ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) -> IO a) -> IO a
createGrPpl dv ex rp pl spcnt f = Vk.Ppl.Graphics.createGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl spcnt) nil
	\(HPList.Singleton (U3 p)) -> f p

recreateGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Sample.CountFlags ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) -> IO ()
recreateGrPpl dv ex rp pl spcnt p = Vk.Ppl.Graphics.unsafeRecreateGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl spcnt) nil
	(HPList.Singleton $ U3 p)

grPplInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Sample.CountFlags ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing
		'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
		'(	'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Pos), '(1, Color)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'Nothing '(sl, '[ '(sdsl, DscStLytArg alu)], '[])
		sr '(sb, vs, ts, plas)
grPplInfo ex rp pl spcnt = Vk.Ppl.Graphics.CreateInfo {
	Vk.Ppl.Graphics.createInfoNext = TMaybe.N,
	Vk.Ppl.Graphics.createInfoFlags = zeroBits,
	Vk.Ppl.Graphics.createInfoStages = shaderStages,
	Vk.Ppl.Graphics.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Graphics.createInfoInputAssemblyState = Just ia,
	Vk.Ppl.Graphics.createInfoViewportState = Just $ vwpSt ex,
	Vk.Ppl.Graphics.createInfoRasterizationState = Just rst,
	Vk.Ppl.Graphics.createInfoMultisampleState = Just ms,
	Vk.Ppl.Graphics.createInfoDepthStencilState = Nothing,
	Vk.Ppl.Graphics.createInfoColorBlendState = Just clrBlnd,
	Vk.Ppl.Graphics.createInfoDynamicState = Nothing,
	Vk.Ppl.Graphics.createInfoLayout = U3 pl,
	Vk.Ppl.Graphics.createInfoRenderPass = rp,
	Vk.Ppl.Graphics.createInfoSubpass = 0,
	Vk.Ppl.Graphics.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Graphics.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Graphics.createInfoTessellationState = Nothing }
	where
	ia = Vk.Ppl.InpAsmbSt.CreateInfo {
		Vk.Ppl.InpAsmbSt.createInfoNext = TMaybe.N,
		Vk.Ppl.InpAsmbSt.createInfoFlags = zeroBits,
		Vk.Ppl.InpAsmbSt.createInfoTopology =
			Vk.PrimitiveTopologyPointList,
		Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable = False }
	rst = Vk.Ppl.RstSt.CreateInfo {
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
	ms = Vk.Ppl.MltSmplSt.CreateInfo {
		Vk.Ppl.MltSmplSt.createInfoNext = TMaybe.N,
		Vk.Ppl.MltSmplSt.createInfoFlags = zeroBits,
		Vk.Ppl.MltSmplSt.createInfoSampleShadingEnable = False,
		Vk.Ppl.MltSmplSt.createInfoRasterizationSamplesAndMask =
			Vk.Sample.CountAndMask spcnt Nothing,
		Vk.Ppl.MltSmplSt.createInfoMinSampleShading = 1,
		Vk.Ppl.MltSmplSt.createInfoAlphaToCoverageEnable = False,
		Vk.Ppl.MltSmplSt.createInfoAlphaToOneEnable = False }

shaderStages :: HPList.PL (U5 Vk.Ppl.ShdrSt.CreateInfo)
	'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
shaderStages = U5 vinfo :** U5 finfo :** HPList.Nil
	where
	vinfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = zeroBits,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModule =
			(minfo glslVertexShaderMain, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	finfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = zeroBits,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModule =
			(minfo glslFragmentShaderMain, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }

minfo code = Vk.ShaderModule.CreateInfo {
	Vk.ShaderModule.createInfoNext = TMaybe.N,
	Vk.ShaderModule.createInfoFlags = zeroBits,
	Vk.ShaderModule.createInfoCode = code }

type GlslVertexShaderArgs = '(
	'Nothing, 'Nothing,
	'GlslVertexShader, 'Nothing :: Maybe (Type, Type), '[] )

type GlslFragmentShaderArgs = '(
	'Nothing, 'Nothing,
	'GlslFragmentShader, 'Nothing :: Maybe (Type, Type), '[] )

vwpSt :: Vk.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo 'Nothing
vwpSt ex = Vk.Ppl.ViewportSt.CreateInfo {
	Vk.Ppl.ViewportSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ViewportSt.createInfoFlags = zeroBits,
	Vk.Ppl.ViewportSt.createInfoViewports = [vp],
	Vk.Ppl.ViewportSt.createInfoScissors = [scssr] }
	where
	vp = Vk.Viewport {
		Vk.viewportX = 0, Vk.viewportY = 0,
		Vk.viewportWidth = fromIntegral $ Vk.extent2dWidth ex,
		Vk.viewportHeight = fromIntegral $ Vk.extent2dHeight ex,
		Vk.viewportMinDepth = 0, Vk.viewportMaxDepth = 1 }
	scssr = Vk.Rect2d {
		Vk.rect2dOffset = Vk.Offset2d 0 0, Vk.rect2dExtent = ex }

clrBlnd :: Vk.Ppl.ClrBlndSt.CreateInfo 'Nothing
clrBlnd = Vk.Ppl.ClrBlndSt.CreateInfo {
	Vk.Ppl.ClrBlndSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ClrBlndSt.createInfoFlags = zeroBits,
	Vk.Ppl.ClrBlndSt.createInfoLogicOpEnable = False,
	Vk.Ppl.ClrBlndSt.createInfoLogicOp = Vk.LogicOpCopy,
	Vk.Ppl.ClrBlndSt.createInfoAttachments = [att],
	Vk.Ppl.ClrBlndSt.createInfoBlendConstants =
		fromJust $ rgbaDouble 0 0 0 0 }
	where att = Vk.Ppl.ClrBlndAtt.State {
		Vk.Ppl.ClrBlndAtt.stateColorWriteMask =
			Vk.ClrCmp.RBit .|. Vk.ClrCmp.GBit .|.
			Vk.ClrCmp.BBit .|. Vk.ClrCmp.ABit,
		Vk.Ppl.ClrBlndAtt.stateBlendEnable = True,
		Vk.Ppl.ClrBlndAtt.stateSrcColorBlendFactor = Vk.BlendFactorSrcAlpha,
		Vk.Ppl.ClrBlndAtt.stateDstColorBlendFactor = Vk.BlendFactorOneMinusSrcAlpha,
		Vk.Ppl.ClrBlndAtt.stateColorBlendOp = Vk.BlendOpAdd,
		Vk.Ppl.ClrBlndAtt.stateSrcAlphaBlendFactor = Vk.BlendFactorOne,
		Vk.Ppl.ClrBlndAtt.stateDstAlphaBlendFactor = Vk.BlendFactorZero,
		Vk.Ppl.ClrBlndAtt.stateAlphaBlendOp = Vk.BlendOpAdd }

createFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	HPList.PL (Vk.ImgVw.I inm fmt) sis ->
	Vk.ImgVw.I clrnm clrfmt sciv ->
	(forall sfs . RecreateFrmbffrs sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HPList.Nil _ f = f HPList.Nil
createFrmbffrs dv ex rp (v :** vs) cvw f =
	Vk.Frmbffr.create dv (frmbffrInfo ex rp v cvw) nil \fb ->
	createFrmbffrs dv ex rp vs cvw \fbs -> f (fb :** fbs)

class RecreateFrmbffrs (sis :: [Type]) (sfs :: [Type]) where
	recreateFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
		HPList.PL (Vk.ImgVw.I inm fmt) sis ->
		Vk.ImgVw.I clrnm clrfmt clrsdiv ->
		HPList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFrmbffrs '[] '[] where
	recreateFrmbffrs _ _ _ HPList.Nil _ HPList.Nil = pure ()

instance RecreateFrmbffrs sis sfs =>
	RecreateFrmbffrs (si ': sis) (sf ': sfs) where
	recreateFrmbffrs d ex rp (v :** vs) cv (fb :** fbs) = do
		Vk.Frmbffr.unsafeRecreate d (frmbffrInfo ex rp v cv) nil fb
		recreateFrmbffrs d ex rp vs cv fbs

frmbffrInfo :: Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I inm fmt si ->
	Vk.ImgVw.I clrnm clrfmt sciv ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[
		'(clrnm, clrfmt, sciv),
		'(inm, fmt, si) ]
frmbffrInfo ex rp att clr = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments =
		U3 clr :** U3 att :** HPList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = ex

createCmdPl :: QFamIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfis dv = Vk.CmdPl.create dv info nil
	where info = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = grFam qfis }

prepareImg :: forall sd nm fmt a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mm.PropertyFlagBits -> Word32 -> Word32 ->
	Word32 -> Vk.Sample.CountFlags -> (forall si sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mm.M sm '[ '(si, 'Vk.Mm.ImageArg nm fmt)] -> IO a) ->
	IO a
prepareImg pd dv tl us pr w h ml spcnt a =
	Vk.Img.create @'Nothing dv (imgInfo w h ml spcnt tl us) nil \i -> do
	rqs <- Vk.Img.getMemoryRequirements dv i
	mt <- findMmType pd (Vk.Mm.requirementsMemoryTypeBits rqs) pr
	Vk.Mm.allocateBind @'Nothing dv
		(HPList.Singleton . U2 $ Vk.Mm.Image i) (memInfo mt) nil
		\(HPList.Singleton (U2 (Vk.Mm.ImageBinded b))) m -> a b m

reprepareImg :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mm.PropertyFlags -> Word32 -> Word32 ->
	Word32 -> Vk.Sample.CountFlags -> Vk.Img.Binded sm sb nm fmt ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)] -> IO ()
reprepareImg pd dv tl us pr w h ml spcnt i m = do
	Vk.Img.unsafeRecreate dv (imgInfo w h ml spcnt tl us) nil i
	rqs <- Vk.Img.getMemoryRequirementsBinded dv i
	mt <- findMmType pd (Vk.Mm.requirementsMemoryTypeBits rqs) pr
	Vk.Mm.unsafeReallocateBind dv
		(HPList.Singleton . U2 $ Vk.Mm.ImageBinded i) (memInfo mt) nil m

imgInfo :: Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags ->
	Vk.Img.Tiling -> Vk.Img.UsageFlags -> Vk.Img.CreateInfo 'Nothing fmt
imgInfo w h ml spcnt tl us = Vk.Img.CreateInfo {
		Vk.Img.createInfoNext = TMaybe.N,
		Vk.Img.createInfoImageType = Vk.Img.Type2d,
		Vk.Img.createInfoExtent = Vk.Extent3d {
			Vk.extent3dWidth = w, Vk.extent3dHeight = h,
			Vk.extent3dDepth = 1 },
		Vk.Img.createInfoMipLevels = ml,
		Vk.Img.createInfoArrayLayers = 1,
		Vk.Img.createInfoTiling = tl,
		Vk.Img.createInfoInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Img.createInfoUsage = us,
		Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Img.createInfoSamples = spcnt,
		Vk.Img.createInfoFlags = zeroBits,
		Vk.Img.createInfoQueueFamilyIndices = [] }

memInfo :: Vk.Mm.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
memInfo mt = Vk.Mm.AllocateInfo {
	Vk.Mm.allocateInfoNext = TMaybe.N,
	Vk.Mm.allocateInfoMemoryTypeIndex = mt }

singleTimeCmds :: forall sd sc a .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	(forall s . Vk.CBffr.C s -> IO a) -> IO a
singleTimeCmds dv gq cp cmd =
	Vk.CBffr.allocate dv (cmdBffrInfo @'[ '()] cp) \(cb :*. HPList.Nil) ->
	Vk.CBffr.begin @_ @'Nothing cb binfo (cmd cb) <* do
		Vk.Q.submit gq (HPList.Singleton . U4 $ sinfo cb) Nothing
		Vk.Q.waitIdle gq
	where
	sinfo cb = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }
	binfo = Vk.CBffr.BeginInfo {
		Vk.CBffr.beginInfoNext = TMaybe.N,
		Vk.CBffr.beginInfoFlags = Vk.CBffr.UsageOneTimeSubmitBit,
		Vk.CBffr.beginInfoInheritanceInfo = Nothing }

createVtxBffr :: (IsSequence lst, Element lst ~ WVertex) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> lst ->
	(forall smsbbnmvs .
		HPList.PL (VtxBffr' WVertex lnm) smsbbnmvs ->
		IO a) -> IO a
createVtxBffr pd d gq cp vtcs@(fromIntegral . olength -> ln) f =
	HPList.replicateM maxFramesInFlight (createVtxBffr'' pd d ln) \bs ->
	writeVtxBffr pd d gq cp bs vtcs >> f bs

createVtxBffr'' :: Storable t =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> (forall smsbbnmvs .
		VtxBffr' t lnm smsbbnmvs ->
		IO a) -> IO a
createVtxBffr'' pd dv ln f = createBffrLst pd dv ln
	(	Vk.Bffr.UsageVertexBufferBit .|.
		Vk.Bffr.UsageStorageBufferBit .|. Vk.Bffr.UsageTransferDstBit )
	Vk.Mm.PropertyDeviceLocalBit \b _ -> f . U3 $ VtxBffr b

newtype VtxBffr t lnm sm sb bnm =
	VtxBffr (Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List t lnm])

type VtxBffr' t lnm = U3 (VtxBffr t lnm)

writeVtxBffr :: forall sd sc lst lnm smsbbnms .
	(IsSequence lst, Storable' (Element lst)) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	HPList.PL (VtxBffr' (Element lst) lnm) smsbbnms ->
	lst -> IO ()
writeVtxBffr pd dv gq cp b xs@(fromIntegral . olength -> ln) =
		createBffrLst pd dv ln
			Vk.Bffr.UsageTransferSrcBit (
			Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) \
			(b' :: Vk.Bffr.Binded sm sb bnm' '[Vk.ObjNA.List t lnm'])
			bm' -> do
			Vk.Mm.write
				@bnm' @(Vk.ObjNA.List t lnm') @0 dv bm' zeroBits xs
			HPList.mapM_ (copy b') b
	where
	copy :: forall sm sb bnm s .
		Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List (Element lst) lnm] ->
		VtxBffr' (Element lst) lnm s -> IO ()
	copy s (U3 (VtxBffr d)) = singleTimeCmds dv gq cp \cb ->
		Vk.Cmd.copyBuffer @'[ '( '[Vk.ObjNA.List (Element lst) lnm], 0, 0)] cb s d

createMvpBffrs' :: KnownNat al =>
	Int -> Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sdsl (DscStLytArg al) ->
	(forall sls smsbs . (
		HPList.FromList sls, Vk.DscSt.DListFromMiddle sls,
		HPList.HomoList '(sdsl, DscStLytArg al) sls,
		Update al smsbs sls ) =>
		HPList.PL (U2 Vk.DscStLyt.D) sls ->
		HPList.PL (BindedModelViewProj al nm) smsbs ->
		HPList.PL (MemoryModelViewProj al nm) smsbs -> IO a) -> IO a
createMvpBffrs' n _ _ _ f | n < 1 = f HPList.Nil HPList.Nil HPList.Nil
createMvpBffrs' n pd dv dl f =
	createMvpBffr pd dv \b m ->
	createMvpBffrs' (n - 1) pd dv dl \dls bs ms -> f (U2 dl :** dls)
		(BindedModelViewProj b :** bs) (MemoryModelViewProj m :** ms)

data BindedModelViewProj al nm smsb where
	BindedModelViewProj ::
		Vk.Bffr.Binded sm sb nm '[AtomModelViewProj al] ->
		BindedModelViewProj al nm '(sm, sb)

data MemoryModelViewProj al nm smsb where
	MemoryModelViewProj ::
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg nm '[AtomModelViewProj al])] ->
		MemoryModelViewProj al nm '(sm, sb)

createMvpBffr :: KnownNat alu => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb mnm '[AtomModelViewProj alu] ->
	ModelViewProjMemory sm sb mnm alu -> IO b) -> IO b
createMvpBffr = createBffrAtm
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

type ModelViewProjMemory sm sb mnm alu =
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg mnm '[AtomModelViewProj alu])]

createBffrAtm :: forall al sd nm a b . (KnownNat al, Storable a) =>
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Vk.Obj.Atom al a 'Nothing] ->
		Vk.Mm.M sm '[ '(
			sb, 'Vk.Mm.BufferArg nm '[Vk.Obj.Atom al a 'Nothing] )] ->
		IO b) -> IO b
createBffrAtm us prs p dv = createBffr p dv Vk.Obj.LengthAtom us prs

createBffrLst :: forall al sd bnm lnm t a . (KnownNat al, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.Obj.List al t lnm] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm '[Vk.Obj.List al t lnm])] ->
		IO a) -> IO a
createBffrLst p dv ln = createBffr p dv $ Vk.Obj.LengthList ln

createBffr :: forall sd bnm o a . Vk.Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Obj.Length o ->
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
	Vk.Obj.Length o -> Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo 'Nothing '[o]
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

createDscPl :: Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDscPl dv = Vk.DscPl.create dv info nil
	where
	info = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = maxFramesInFlight,
		Vk.DscPl.createInfoPoolSizes = [sz0, sz1] }
	sz0 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPl.sizeDescriptorCount = maxFramesInFlight }
	sz1 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscPl.sizeDescriptorCount = maxFramesInFlight }

createDscSts :: (
	Vk.DscSt.DListFromMiddle sls,
	Update al smsbs sls ) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	HPList.PL (BindedModelViewProj al nm) smsbs ->
	HPList.PL (U2 Vk.DscStLyt.D) sls ->
	(forall sds . HPList.PL (Vk.DscSt.D sds) sls -> IO a) -> IO a
createDscSts dv dp mbs dls f =
	Vk.DscSt.allocateDs dv info $ (>>) <$> update dv mbs <*> f
	where info = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = dls }

class Update al smsbs slbtss where
	update :: Vk.Dvc.D sd -> HPList.PL (BindedModelViewProj al nm) smsbs ->
		HPList.PL (Vk.DscSt.D sds) slbtss -> IO ()

instance Update _al '[] '[] where update _ HPList.Nil HPList.Nil = pure ()

instance (
	KnownNat al,
	Vk.DscSt.BindingAndArrayElemBuffer
		cs '[Vk.Obj.Atom al WModelViewProj 'Nothing] 0,
	Vk.DscSt.UpdateDynamicLength
		cs '[Vk.Obj.Atom al WModelViewProj 'Nothing],
	Update al smsbs slbtss ) =>
	Update al (smsb ': smsbs) ('(ds, cs) ': slbtss) where
	update dv (BindedModelViewProj mb :** mbs) (ds :** dss) =
		Vk.DscSt.updateDs dv (
			U5 (dscWrite0 ds mb) :** HPList.Nil ) HPList.Nil >>
		update dv mbs dss

dscWrite0 :: KnownNat alu => Vk.DscSt.D sds slbts ->
	Vk.Bffr.Binded sm sb bnm '[AtomModelViewProj alu] ->
	Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgBuffer '[ '(
			sm, sb, bnm, Vk.Obj.Atom alu WModelViewProj 'Nothing, 0 )]) 0
dscWrite0 ds mb = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos
		. HPList.Singleton . U5 $ Vk.Dsc.BufferInfo mb }

type TxFmt = Vk.T.FormatR8g8b8a8Srgb

cmdBffrInfo :: forall n scp .
	Vk.CmdPl.C scp -> Vk.CBffr.AllocateInfo 'Nothing scp n
cmdBffrInfo cp = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cp,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

createSyncObjs :: forall n sd a . HPList.RepM n =>
	Vk.Dvc.D sd -> (forall ssos . SyncObjs ssos -> IO a) -> IO a
createSyncObjs dv f =
	HPList.repM @n (Vk.Semaphore.create @'Nothing dv def nil) \iass ->
	HPList.repM @n (Vk.Semaphore.create @'Nothing dv def nil) \rfss ->
	HPList.repM @n (Vk.Fence.create @'Nothing dv finfo nil) \iffs ->
	f $ SyncObjs iass rfss iffs
	where
	finfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

data SyncObjs (ssos :: ([Type], [Type], [Type])) where
	SyncObjs :: {
		_imageAvailableSemaphores :: HPList.PL Vk.Semaphore.S siass,
		_renderFinishedSemaphores :: HPList.PL Vk.Semaphore.S srfss,
		_inFlightFences :: HPList.PL Vk.Fence.F sfss } ->
		SyncObjs '(siass, srfss, sfss)

mainloop :: (
	Vk.T.FormatToValue scfmt,
	RecreateFrmbffrs ss sfs,
	HPList.HomoList '(sdsl, DscStLytArg alu) slyts, HPList.HomoList '() mff,
	KnownNat alu ) =>
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) ss ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	ClrRsrcs scfmt clrnm clrsi clrsm clrsiv ->
	HPList.PL (VtxBffr' WVertex nmv) smsbbnmvs ->
	HPList.PL (MemoryModelViewProj alu nmm) smsbs ->
	HPList.PL (Vk.DscSt.D sds) slyts ->
	HPList.LL (Vk.CBffr.C scb) mff -> SyncObjs ssoss -> UTCTime -> IO ()
mainloop fr w sfc pd qfis dv gq pq
	sc ex0 vs rp pl gp fbs crs vbs mms dss cbs soss tm0 = do
	($ Inf.cycle $ NE.fromList [0 .. maxFramesInFlight - 1])
		. ($ ex0) $ fix \go ex (cf :~ cfs) ->
		GlfwG.pollEvents >>
		getCurrentTime >>= \tm ->
		run fr w sfc pd qfis dv gq pq
			sc ex vs rp pl gp fbs crs
			vbs
			mms dss cbs soss (realToFrac $ tm `diffUTCTime` tm0)
			cf (`go` cfs)
	Vk.Dvc.waitIdle dv

run :: (
	HPList.HomoList '() mff, RecreateFrmbffrs svs sfs,
	Vk.T.FormatToValue scfmt,
	HPList.HomoList '(sdsl, DscStLytArg alu) slyts,
	KnownNat alu ) =>
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I inm scfmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] -> Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	ClrRsrcs scfmt clrnm clrsi clrsm clrsiv ->
	HPList.PL (VtxBffr' WVertex nmv) smsbbnmvs ->
	HPList.PL (MemoryModelViewProj alu nmm) smsbs ->
	HPList.PL (Vk.DscSt.D sds) slyts ->
	HPList.LL (Vk.CBffr.C scb) mff ->
	SyncObjs ssoss -> Float -> Int -> (Vk.Extent2d -> IO ()) -> IO ()
run fr w sfc pd qfis dv gq pq
	sc ex vs rp pl gp fbs crs vbs mms dss cbs soss tm cf go = do
	catchAndRecreate w sfc pd qfis dv sc vs rp pl gp fbs crs go
		$ draw dv gq pq sc ex rp pl gp fbs vbs
		mms dss cbs soss tm cf
	(,) <$> GlfwG.Win.shouldClose w <*> checkFlag fr >>= \case
		(True, _) -> pure (); (_, False) -> go ex
		(_, _) -> go =<< recreateAll
			w sfc pd qfis dv sc vs rp pl gp crs fbs

draw :: forall
	sd fmt ssc sr sl sdsl sg sfs nmv
	alu nmm smsbs sds sls scb mff ssos smsbbnmvs . (
	KnownNat alu,
	HPList.HomoList '() mff,
	HPList.HomoList '(sdsl, DscStLytArg alu) sls ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S fmt ssc ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] -> Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	HPList.PL (VtxBffr' WVertex nmv) smsbbnmvs ->
	HPList.PL (MemoryModelViewProj alu nmm) smsbs ->
	HPList.PL (Vk.DscSt.D sds) sls ->
	HPList.LL (Vk.CBffr.C scb) mff -> SyncObjs ssos -> Float -> Int -> IO ()
draw dv gq pq sc ex rp pl gp fbs
	vbs mms dss cbs (SyncObjs iass rfss iffs) tm cf =
	HPList.index iass cf \ias -> HPList.index rfss cf \rfs ->
	HPList.index iffs cf \(id &&& HPList.Singleton -> (iff, siff)) ->
	HPList.index mms cf \mm ->
	HPList.index vbs cf \(U3 (VtxBffr vb)) ->
	($ HPList.homoListIndex dss cf) \ds -> do
	Vk.Fence.waitForFs dv siff True Nothing >> Vk.Fence.resetFs dv siff
	ii <- Vk.Khr.acquireNextImageResult
		[Vk.Success, Vk.SuboptimalKhr] dv sc maxBound (Just ias) Nothing
	Vk.CBffr.reset cb def
	HPList.index fbs ii \fb -> recordCmdBffr cb ex rp pl gp fb vb ds
	updateModelViewProj dv mm ex tm
	Vk.Q.submit gq (HPList.Singleton . U4 $ sinfo ias rfs) $ Just iff
	catchAndSerialize . Vk.Khr.queuePresent pq $ pinfo rfs ii
	where
	HPList.Dummy cb = cbs `HPList.homoListIndex` cf ::
		HPList.Dummy (Vk.CBffr.C scb) '()
	sinfo :: Vk.Semaphore.S sias -> Vk.Semaphore.S srfs ->
		Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
	sinfo ias rfs = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks =
			HPList.Singleton $ Vk.SemaphorePipelineStageFlags
				ias Vk.Ppl.StageColorAttachmentOutputBit,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
	pinfo :: Vk.Semaphore.S srfs -> Word32 ->
		Vk.Khr.PresentInfo 'Nothing '[srfs] fmt '[ssc]
	pinfo rfs ii = Vk.Khr.PresentInfo {
		Vk.Khr.presentInfoNext = TMaybe.N,
		Vk.Khr.presentInfoWaitSemaphores = HPList.Singleton rfs,
		Vk.Khr.presentInfoSwapchainImageIndices =
			HPList.Singleton $ Vk.Khr.SwapchainImageIndex sc ii }

updateModelViewProj :: forall sd alu sm nmm . KnownNat alu => Vk.Dvc.D sd ->
	MemoryModelViewProj alu nmm sm -> Vk.Extent2d -> Float -> IO ()
updateModelViewProj dv (MemoryModelViewProj mm) Vk.Extent2d {
	Vk.extent2dWidth = fromIntegral -> w,
	Vk.extent2dHeight = fromIntegral -> h } tm =
	Vk.Mm.write @nmm @(Vk.Obj.Atom alu WModelViewProj 'Nothing) @0 dv mm zeroBits
		$ GStorable.W ModelViewProj {
			model = Glm.rotate Glm.mat4Identity (tm * Glm.rad 90)
				(Glm.Vec3 $ 0 :. 0 :. 1 :. NilL),
			view = Glm.lookat
				(Glm.Vec3 $ 2 :. 2 :. 2 :. NilL)
				(Glm.Vec3 $ 0 :. 0 :. 0 :. NilL)
				(Glm.Vec3 $ 0 :. 0 :. 1 :. NilL),
			projection = Glm.modifyMat4 1 1 negate
				$ Glm.perspective (Glm.rad 45) (w / h) 0.1 10 }

recordCmdBffr :: forall scb sr sl sg sf smv sbv bnmv nmv sds sdsl alu .
	Vk.CBffr.C scb -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) ->
	Vk.Frmbffr.F sf ->
	Vk.Bffr.Binded smv sbv bnmv '[Vk.Obj.List 1 WVertex nmv] ->
	Vk.DscSt.D sds '(sdsl, DscStLytArg alu) -> IO ()
recordCmdBffr cb ex rp pl gp fb vb ds =
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb info Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gp \cbb -> do
	Vk.Cmd.bindVertexBuffers cbb . HPList.Singleton
		. U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @nmv vb
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics pl
		(HPList.Singleton $ U2 ds)
		(HPList.Singleton $ HPList.Nil :** HPList.Nil)
	Vk.Cmd.draw cbb (bffrLstLn vb) 1 0 0
	where
	info :: Vk.RndrPss.BeginInfo 'Nothing sr sf '[
		'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32 ]
	info = Vk.RndrPss.BeginInfo {
		Vk.RndrPss.beginInfoNext = TMaybe.N,
		Vk.RndrPss.beginInfoRenderPass = rp,
		Vk.RndrPss.beginInfoFramebuffer = fb,
		Vk.RndrPss.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = ex },
		Vk.RndrPss.beginInfoClearValues =
			Vk.ClearValueColor (fromJust $ rgbaDouble 0 0 0 1) :**
			HPList.Nil }

bffrLstLn :: Num n =>
	Vk.Bffr.Binded sm sb bnm '[Vk.Obj.ListMaybeName al v mnm] -> n
bffrLstLn b = fromIntegral sz
	where HPList.Singleton (Vk.Obj.LengthList' sz) = Vk.Bffr.lengthBinded b

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate :: (
	Vk.T.FormatToValue scfmt,
	RecreateFrmbffrs svs sfs ) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QFamIndices ->
	Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	ClrRsrcs scfmt clrnm clrsi clrsm clrsiv ->
	(Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate w sfc pd qfis dv sc vs rp pl gp fbs crs go act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just (); _ -> Nothing) act \_ ->
	go =<< recreateAll w sfc pd qfis dv sc vs rp pl gp crs fbs

recreateAll :: (
	Vk.T.FormatToValue fmt,
	RecreateFrmbffrs svs sfs ) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QFamIndices ->
	Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S fmt ssc ->
	HPList.PL (Vk.ImgVw.I nm fmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] -> Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) ->
	ClrRsrcs fmt clnm clrsi clrsm clrsiv ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateAll w sfc pd qfis dv sc vs rp pl gp
	crs@(_, _, cvw, mss) fbs = do
	waitFramebufferSize w >> Vk.Dvc.waitIdle dv
	ex <- recreateSwpch w sfc pd qfis dv sc
	ex <$ do
		Vk.Khr.Swpch.getImages dv sc >>= \is -> recreateImgVws dv is vs
		recreateClrRsrcs pd dv ex crs
		recreateGrPpl dv ex rp pl mss gp
		recreateFrmbffrs dv ex rp vs cvw fbs

waitFramebufferSize :: GlfwG.Win.W sw -> IO ()
waitFramebufferSize w = GlfwG.Win.getFramebufferSize w >>= \sz ->
	when (zero sz) $ fix \go -> (`when` go) . zero =<<
		GlfwG.waitEvents *> GlfwG.Win.getFramebufferSize w
	where zero = uncurry (||) . ((== 0) *** (== 0))

type WModelViewProj = GStorable.W ModelViewProj

data ModelViewProj = ModelViewProj {
	model :: Glm.Mat4, view :: Glm.Mat4, projection :: Glm.Mat4 }
	deriving (Show, Generic)

instance GStorable.G ModelViewProj

newtype ImageRgba8 = ImageRgba8 (Image PixelRGBA8)
newtype PixelRgba8 = PixelRgba8 PixelRGBA8

instance Storable PixelRgba8 where
	sizeOf _ = 4 * sizeOf @Pixel8 undefined
	alignment _ = alignment @Pixel8 undefined
	peek p = PixelRgba8 . (\(r, g, b, a) -> PixelRGBA8 r g b a)
		. listToTuple4 <$> peekArray 4 (castPtr p)
	poke p (PixelRgba8 (PixelRGBA8 r g b a)) =
		pokeArray (castPtr p) [r, g, b, a]

instance BObj.IsImage ImageRgba8 where
	type ImagePixel ImageRgba8 = PixelRgba8
	type ImageFormat ImageRgba8 = TxFmt
	imageRow = BObj.imageWidth
	imageWidth (ImageRgba8 i) = fromIntegral $ imageWidth i
	imageHeight (ImageRgba8 i) = fromIntegral $ imageHeight i
	imageDepth _ = 1
	imageBody (ImageRgba8 i) = (<$> [0 .. imageHeight i - 1]) \y ->
		(<$> [0 .. imageWidth i - 1]) \x -> PixelRgba8 $ pixelAt i x y
	imageMake (fromIntegral -> w) (fromIntegral -> h) _d pss =
		ImageRgba8 $ generateImage
			(\x y -> let PixelRgba8 p = (pss' ! y) ! x in p) w h
		where pss' = listArray (0, h - 1) (listArray (0, w - 1) <$> pss)

vertices' :: StdGen -> [WVertex]
vertices' g = GStorable.W
	<$> take particleCount (L.unfoldr (Just . randomVertex) g)

randomVertex :: StdGen -> (Vertex, StdGen)
randomVertex g0 = let
	(r_, g1) = randomR (0.0, 1.0) g0
	(theta, g2) = randomR (0.0, 2 * pi) g1
	r = 0.25 * sqrt r_
	x = r * cos theta
	y = r * sin theta
	d = sqrt $ x ^ (2 :: Int) + y ^ (2 :: Int)
	vx = x / d * 0.00025
	vy = y / d * 0.00025
	(rd, g3) = randomR (0, 1.0) g2
	(g, g4) = randomR (0, 1.0) g3
	(b, g5) = randomR (0, 1.0) g4
	in (	Vertex (Pos . Glm.Vec2 $ x :. y :. NilL)
			(Glm.Vec2 $ vx :. vy :. NilL)
			(Color . Glm.Vec3 $ rd :. g :. b :. NilL),
		g5)

type WVertex = GStorable.W Vertex

data Vertex = Vertex {
	vertexPos :: Pos,
	vertexVelocity :: Glm.Vec2,
	vertexColor :: Color }
	deriving (Show, Eq, Ord, Generic)

newtype Pos = Pos Glm.Vec2
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype Color = Color Glm.Vec3
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

instance GStorable.G Vertex

[glslVertexShader|

#version 450

layout(binding = 0) uniform
	ModelViewProj { mat4 model; mat4 view; mat4 proj; } mvp;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inColor;

layout(location = 0) out vec3 fragColor;

void
main()
{
	gl_PointSize = 14.0;
	gl_Position = mvp.proj * mvp.view * mvp.model * vec4(inPosition, 1.0);
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

	vec2 coord = gl_PointCoord - vec2(0.5);
	outColor = vec4(fragColor, 0.5 - length(coord));

}

|]

[glslComputeShader|

#version 450

struct Particle {
	vec2 position;
	vec2 velocity;
	vec4 color;
};

layout (binding = 0) uniform ParameterUBO {
	float deltaTime;
} ubo;

layout(std140, binding = 1) readonly buffer ParticleSSBOIn {
	Particle particlesIn[ ];
};

layout(std140, binding = 2) buffer ParticleSSBOOut {
	Particle particlesOut[ ];
};

layout (local_size_x = 256, local_size_y = 1, local_size_z = 1) in;

void main()
{
	uint index = gl_GlobalInvocationID.x;

	Particle particleIn = particlesIn[index];

//	particlesOut[index].position = particleIn.position + particleIn.velocity.xy * ubo.deltaTime;
	particlesOut[index].position = particleIn.position + particleIn.velocity.xy * 1;
	particlesOut[index].velocity = particleIn.velocity;

	// Flip movement at window border
	if ((particlesOut[index].position.x <= -1.0) || (particlesOut[index].position.x >= 1.0)) {
	    particlesOut[index].velocity.x = -particlesOut[index].velocity.x;
	}
	if ((particlesOut[index].position.y <= -1.0) || (particlesOut[index].position.y >= 1.0)) {
	    particlesOut[index].velocity.y = -particlesOut[index].velocity.y;
	}

}

|]