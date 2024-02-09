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
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.TypeNats
import GHC.TypeLits (Symbol)

import Gpu.Vulkan.Memory qualified as Vk.Mm

import GHC.Generics
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.Marshal.Array
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object.Base qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Data.Ord.ToolsYj
import Data.Default
import Data.Bits
import Data.Array hiding (indices)
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.Proxy
import Data.Bool
import Data.Bool.ToolsYj
import Data.Maybe
import Data.Maybe.ToolsYj
import Data.List
import Data.List.ToolsYj
import Data.Tuple.ToolsYj
import Data.Function.ToolsYj
import Data.IORef
import Data.List.Length
import Data.Word
import Data.Color
import Data.Time
import Codec.Picture

import Data.List.NonEmpty qualified as NE
import Data.Vector.Storable qualified as V
import Data.Text.IO qualified as Txt
import Graphics.UI.GLFW qualified as Glfw hiding (createWindowSurface)
import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win
import Gpu.Vulkan.Khr.Surface.Glfw qualified as Glfw
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Cglm qualified as Cglm
import qualified Foreign.Storable.Generic

import Language.SpirV qualified as SpirV
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH

import Data.TypeLevel.ParMaybe (nil)

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Exception qualified as Vk
import Gpu.Vulkan.Instance.Internal qualified as Vk.Ist
import Gpu.Vulkan.Instance qualified as Vk.Ist
import Gpu.Vulkan.Khr qualified as Vk.Khr
import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.DbgUtls.Msngr
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam

import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc.M
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as 
	Vk.Khr.Sfc.PhysicalDevice
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as 
	Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Component qualified as Vk.Component
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderModule
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt
import Gpu.Vulkan.Pipeline.InputAssemblyState qualified as Vk.Ppl.InpAsmbSt
import Gpu.Vulkan.Pipeline.ViewportState qualified as Vk.Ppl.ViewportSt
import Gpu.Vulkan.Pipeline.RasterizationState qualified as Vk.Ppl.RstSt
import Gpu.Vulkan.Pipeline.MultisampleState qualified as Vk.Ppl.MltSmplSt
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.Pipeline.ColorBlendAttachment qualified as Vk.Ppl.ClrBlndAtt
import Gpu.Vulkan.ColorComponent qualified as Vk.ClrCmp
import Gpu.Vulkan.Pipeline.ColorBlendState qualified as Vk.Ppl.ClrBlndSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.PplLyt
import Gpu.Vulkan.Attachment qualified as Vk.Att
import Gpu.Vulkan.Subpass qualified as Vk.Subpass
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPss
import Gpu.Vulkan.Pipeline.Graphics qualified as Vk.Ppl.Graphics
import Gpu.Vulkan.Framebuffer qualified as Vk.Frmbffr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr.M
import Gpu.Vulkan.Semaphore qualified as Vk.Semaphore
import Gpu.Vulkan.Fence qualified as Vk.Fence
import Gpu.Vulkan.VertexInput qualified as Vk.VtxInp
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Memory qualified as Vk.Mm.M
import Gpu.Vulkan.Memory qualified as Vk.Dvc.Mem
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.Cmd qualified as Vk.Cmd

import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscSetLyt
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPool
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSet

import Gpu.Vulkan.Sampler qualified as Vk.Smplr
import Gpu.Vulkan.Sampler qualified as Vk.Smplr.M

import Tools (clampOld, checkBits, checkFlag)

import Data.Text.ToolsYj

import Debug

import Control.Monad.Trans
import Options.Declarative

main :: IO ()
main = run_ realMain

realMain ::
	Flag "t" '["texture"] "FILEPATH" "texture filepath"
		(Def "../../../../../files/images/texture.jpg" String) ->
	Cmd "Try Vulkan Texture" ()
realMain txfp = liftIO $ newIORef False >>= \fr -> withWindow fr \w ->
	createIst \ist -> bool id (dbgm ist) debug $ body (get txfp) fr w ist
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

body :: FilePath -> FramebufferResized -> GlfwG.Win.W sw -> Vk.Ist.I si -> IO ()
body txfp fr w ist =
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
	either error convertRGBA8 <$> readImage txfp >>= \txi ->
	createImg pd d gq cp (MyImage txi) \tx ->
	Vk.ImgVw.create @_ @'Vk.T.FormatR8g8b8a8Srgb d (imgVwInfo tx) nil \tv ->
	createTextureSampler pd d \txsp ->
	createVtxBffr pd d gq cp vertices \vb ->
	createIdxBffr pd d gq cp indices \ib ->
	createMvpBffr pd d \mb mbm ->
	createDscPl d \dp -> createDscSt d dp mb tv txsp dsl \ds ->
	createCmdBffr d cp \cb ->
	createSyncObjs d \sos ->
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
		Vk.Dvc.createInfoEnabledFeatures = Just def {
			Vk.Phd.featuresSamplerAnisotropy = True } }

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
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`find` fmts)
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
	error "number of Vk.Image.I and Vk.ImageView.I should be same"

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
		Vk.RndrPss.createInfoAttachments = ca :** HPList.Nil,
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

createPplLyt :: forall alm sd a . Vk.Dvc.D sd -> (forall sl sdsl .
	Vk.DscSetLyt.D sdsl '[BufferModelViewProj alm, TextureImage] ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alm)] '[] -> IO a) -> IO a
createPplLyt dv f = createDscStLyt dv \dsl ->
	Vk.PplLyt.create @_ @_ @_ @'[] dv (info dsl) nil $ f dsl
	where info dsl = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

createDscStLyt :: Vk.Dvc.D sd ->
	(forall s . Vk.DscSetLyt.D s (DscStLytArg alm) -> IO a) -> IO a
createDscStLyt dv = Vk.DscSetLyt.create dv info nil
	where
	info = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = mbd :** tbd :** HPList.Nil }
	mbd = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	tbd = Vk.DscSetLyt.BindingImage {
		Vk.DscSetLyt.bindingImageDescriptorType =
			Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscSetLyt.bindingImageStageFlags =
			Vk.ShaderStageFragmentBit }

type DscStLytArg alm = '[BufferModelViewProj alm, TextureImage]

createGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alm)] '[] ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, TexCoord)]
		'(sl, '[ '(sdsl, DscStLytArg alm)], '[]) -> IO a) -> IO a
createGrPpl dv ex rp pl f = Vk.Ppl.Graphics.createGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	\(HPList.Singleton (U3 p)) -> f p

recreateGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alm)] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, TexCoord)]
		'(sl, '[ '(sdsl, DscStLytArg alm)], '[]) -> IO ()
recreateGrPpl dv ex rp pl p = Vk.Ppl.Graphics.unsafeRecreateGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	(HPList.Singleton $ U3 p)

grPplInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alm)] '[] ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing
		'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
		'(	'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, TexCoord)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'Nothing '(sl, '[ '(sdsl, DscStLytArg alm)], '[])
		sr '(sb, vs, ts, plas)
grPplInfo ex rp pl = Vk.Ppl.Graphics.CreateInfo {
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
			Vk.PrimitiveTopologyTriangleList,
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
			Vk.Sample.CountAndMask Vk.Sample.Count1Bit Nothing,
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
		Vk.Ppl.ClrBlndAtt.stateBlendEnable = False,
		Vk.Ppl.ClrBlndAtt.stateSrcColorBlendFactor = Vk.BlendFactorOne,
		Vk.Ppl.ClrBlndAtt.stateDstColorBlendFactor = Vk.BlendFactorZero,
		Vk.Ppl.ClrBlndAtt.stateColorBlendOp = Vk.BlendOpAdd,
		Vk.Ppl.ClrBlndAtt.stateSrcAlphaBlendFactor = Vk.BlendFactorOne,
		Vk.Ppl.ClrBlndAtt.stateDstAlphaBlendFactor = Vk.BlendFactorZero,
		Vk.Ppl.ClrBlndAtt.stateAlphaBlendOp = Vk.BlendOpAdd }

createFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	HPList.PL (Vk.ImgVw.I inm fmt) sis -> (forall sfs .
		RecreateFrmbffrs sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HPList.Nil f = f HPList.Nil
createFrmbffrs dv ex rp (v :** vs) f =
	Vk.Frmbffr.create dv (frmbffrInfo ex rp v) nil \fb ->
	createFrmbffrs dv ex rp vs \fbs -> f (fb :** fbs)

class RecreateFrmbffrs (sis :: [Type]) (sfs :: [Type]) where
	recreateFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
		HPList.PL (Vk.ImgVw.I inm fmt) sis ->
		HPList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFrmbffrs '[] '[] where
	recreateFrmbffrs _ _ _ HPList.Nil HPList.Nil = pure ()

instance RecreateFrmbffrs sis sfs =>
	RecreateFrmbffrs (si ': sis) (sf ': sfs) where
	recreateFrmbffrs dv ex rp (v :** vs) (fb :** fbs) =
		Vk.Frmbffr.unsafeRecreate dv (frmbffrInfo ex rp v) nil fb >>
		recreateFrmbffrs dv ex rp vs fbs

frmbffrInfo :: Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I inm fmt si ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(inm, fmt, si)]
frmbffrInfo ex rp att = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = HPList.Singleton $ U3 att,
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

createImg :: forall sd sc img nm a . KObj.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> img ->
	(forall si sm .
		Vk.Img.Binded sm si nm (KObj.ImageFormat img) -> IO a) -> IO a
createImg pd dv gq cp img f =
	prepareImg pd dv w h Vk.Img.TilingOptimal
		(Vk.Img.UsageTransferDstBit .|. Vk.Img.UsageSampledBit)
		Vk.Mm.PropertyDeviceLocalBit \i _m -> do
		createBufferImage @img pd dv (r, w, h, d)
			Vk.Bffr.UsageTransferSrcBit
			(	Vk.Mm.PropertyHostVisibleBit .|.
				Vk.Mm.PropertyHostCoherentBit )
			\(b :: Vk.Bffr.Binded sm sb inmb '[bimg]) bm -> do
			Vk.Dvc.Mem.write @inmb @bimg dv bm zeroBits img
			transitionImageLayout dv gq cp i
				Vk.Img.LayoutUndefined
				Vk.Img.LayoutTransferDstOptimal
			copyBufferToImage dv gq cp b i w h
		transitionImageLayout dv gq cp i
			Vk.Img.LayoutTransferDstOptimal
			Vk.Img.LayoutShaderReadOnlyOptimal
		f i
	where
	r :: Integral i => i; r = fromIntegral $ KObj.imageRow img
	w :: Integral i => i; w = fromIntegral $ KObj.imageWidth img
	h :: Integral i => i; h = fromIntegral $ KObj.imageHeight img
	d :: Integral i => i; d = fromIntegral $ KObj.imageDepth img

prepareImg :: forall nm fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P ->
	Vk.Dvc.D sd -> Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mm.PropertyFlagBits -> (forall si sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Dvc.Mem.M sm
			'[ '(si, 'Vk.Mm.ImageArg nm fmt) ] ->
		IO a) -> IO a
prepareImg pd dvc wdt hgt tlng usg prps f =
	Vk.Img.create @'Nothing dvc imageInfo nil \img -> do
	reqs <- Vk.Img.getMemoryRequirements dvc img
	print reqs
	mt <- findMemoryType pd (Vk.Mm.M.requirementsMemoryTypeBits reqs) prps
	print mt
	Vk.Dvc.Mem.allocateBind @'Nothing dvc
		(HPList.Singleton . U2 $ Vk.Dvc.Mem.Image img) (memInfo mt)
		nil \(HPList.Singleton (U2 (Vk.Dvc.Mem.ImageBinded bnd))) m -> do
		f bnd m
	where
	imageInfo = Vk.Img.CreateInfo {
		Vk.Img.createInfoNext = TMaybe.N,
		Vk.Img.createInfoImageType = Vk.Img.Type2d,
		Vk.Img.createInfoExtent = Vk.Extent3d {
			Vk.extent3dWidth = wdt,
			Vk.extent3dHeight = hgt,
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
	memInfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

transitionImageLayout :: forall sd sc si sm nm fmt .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
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
				Vk.QFam.Ignored,
			Vk.Img.memoryBarrierDstQueueFamilyIndex =
				Vk.QFam.Ignored,
			Vk.Img.memoryBarrierImage = img,
			Vk.Img.memoryBarrierSubresourceRange = srr,
			Vk.Img.memoryBarrierSrcAccessMask = sam,
			Vk.Img.memoryBarrierDstAccessMask = dam }
		srr = Vk.Img.SubresourceRange {
			Vk.Img.subresourceRangeAspectMask =
				Vk.Img.AspectColorBit,
			Vk.Img.subresourceRangeBaseMipLevel = 0,
			Vk.Img.subresourceRangeLevelCount = 1,
			Vk.Img.subresourceRangeBaseArrayLayer = 0,
			Vk.Img.subresourceRangeLayerCount = 1 }
	Vk.Cmd.pipelineBarrier cb
		sstg dstg zeroBits HPList.Nil HPList.Nil (HPList.Singleton $ U5 barrier)
	where (sam, dam, sstg, dstg) = case (olyt, nlyt) of
		(Vk.Img.LayoutUndefined, Vk.Img.LayoutTransferDstOptimal) -> (
			zeroBits, Vk.AccessTransferWriteBit,
			Vk.Ppl.StageTopOfPipeBit, Vk.Ppl.StageTransferBit )
		(Vk.Img.LayoutTransferDstOptimal,
			Vk.Img.LayoutShaderReadOnlyOptimal ) -> (
			Vk.AccessTransferWriteBit, Vk.AccessShaderReadBit,
			Vk.Ppl.StageTransferBit, Vk.Ppl.StageFragmentShaderBit )
		_ -> error "unsupported layout transition!"

copyBufferToImage :: forall sd sc sm sb nm img inm si sm' nm' .
	Storable (KObj.ImagePixel img) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[ VObj.Image 1 img inm]  ->
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
		isr = Vk.Img.SubresourceLayers {
			Vk.Img.subresourceLayersAspectMask =
				Vk.Img.AspectColorBit,
			Vk.Img.subresourceLayersMipLevel = 0,
			Vk.Img.subresourceLayersBaseArrayLayer = 0,
			Vk.Img.subresourceLayersLayerCount = 1 }
	Vk.Cmd.copyBufferToImage @1
		cb bf img Vk.Img.LayoutTransferDstOptimal (HPList.Singleton region)

newtype MyImage = MyImage (Image PixelRGBA8)

newtype MyRgba8 = MyRgba8 { unMyRgba8 :: PixelRGBA8 }

instance Storable MyRgba8 where
	sizeOf _ = 4 * sizeOf @Pixel8 undefined
	alignment _ = alignment @Pixel8 undefined
	peek p = MyRgba8 . (\(r, g, b, a) -> PixelRGBA8 r g b a) . listToTuple4
		<$> peekArray 4 (castPtr p)
	poke p (MyRgba8 (PixelRGBA8 r g b a)) =
		pokeArray (castPtr p) [r, g, b, a]

instance KObj.IsImage MyImage where
	type ImagePixel MyImage = MyRgba8
	type ImageFormat MyImage = 'Vk.T.FormatR8g8b8a8Srgb
	imageRow = KObj.imageWidth
	imageWidth (MyImage img) = fromIntegral $ imageWidth img
	imageHeight (MyImage img) = fromIntegral $ imageHeight img
	imageDepth _ = 1
	imageBody (MyImage img) = (<$> [0 .. imageHeight img - 1]) \y ->
		(<$> [0 .. imageWidth img - 1]) \x -> MyRgba8 $ pixelAt img x y
	imageMake w h _d pss = MyImage
		$ generateImage (\x y -> let MyRgba8 p = (pss' ! y) ! x in p) (fromIntegral w) (fromIntegral h)
		where pss' = listArray (0, fromIntegral h - 1) (listArray (0, fromIntegral w - 1) <$> pss)

createTextureSampler ::
	Vk.Phd.P -> Vk.Dvc.D sd -> (forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTextureSampler phdv dvc f = do
	prp <- Vk.Phd.getProperties phdv
	print . Vk.Phd.limitsMaxSamplerAnisotropy $ Vk.Phd.propertiesLimits prp
	let	samplerInfo = Vk.Smplr.M.CreateInfo {
			Vk.Smplr.M.createInfoNext = TMaybe.N,
			Vk.Smplr.M.createInfoFlags = zeroBits,
			Vk.Smplr.M.createInfoMagFilter = Vk.FilterLinear,
			Vk.Smplr.M.createInfoMinFilter = Vk.FilterLinear,
			Vk.Smplr.M.createInfoMipmapMode =
				Vk.Smplr.MipmapModeLinear,
			Vk.Smplr.M.createInfoAddressModeU =
				Vk.Smplr.AddressModeRepeat,
			Vk.Smplr.M.createInfoAddressModeV =
				Vk.Smplr.AddressModeRepeat,
			Vk.Smplr.M.createInfoAddressModeW =
				Vk.Smplr.AddressModeRepeat,
			Vk.Smplr.M.createInfoMipLodBias = 0,
			Vk.Smplr.M.createInfoAnisotropyEnable = True,
			Vk.Smplr.M.createInfoMaxAnisotropy =
				Vk.Phd.limitsMaxSamplerAnisotropy
					$ Vk.Phd.propertiesLimits prp,
			Vk.Smplr.M.createInfoCompareEnable = False,
			Vk.Smplr.M.createInfoCompareOp = Vk.CompareOpAlways,
			Vk.Smplr.M.createInfoMinLod = 0,
			Vk.Smplr.M.createInfoMaxLod = 0,
			Vk.Smplr.M.createInfoBorderColor =
				Vk.BorderColorIntOpaqueBlack,
			Vk.Smplr.M.createInfoUnnormalizedCoordinates = False }
	Vk.Smplr.create @'Nothing dvc samplerInfo nil f

createVtxBffr :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	[WVertex] -> (forall sm sb al . KnownNat al => Vk.Bffr.Binded sm sb bnm
		'[VObj.List al WVertex lnm] -> IO a) -> IO a
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

createMvpBffr :: KnownNat alm => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
		Vk.Bffr.Binded sm sb "uniform-buffer" '[VObj.Atom alm WModelViewProj 'Nothing]  ->
		Vk.Dvc.Mem.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg "uniform-buffer"
				'[VObj.Atom alm WModelViewProj 'Nothing]) ] ->
		IO b) -> IO b
createMvpBffr phdvc dvc = createBufferAtom phdvc dvc
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

type BufferModelViewProj alm = 'Vk.DscSetLyt.Buffer '[AtomModelViewProj alm]

type AtomModelViewProj alm = VObj.Atom alm WModelViewProj 'Nothing

type TextureImage = 'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)]

createDscPl ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDscPl dvc = Vk.DscPool.create dvc poolInfo nil
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = TMaybe.N,
		Vk.DscPool.createInfoFlags =
			Vk.DscPool.CreateFreeDescriptorSetBit,
		Vk.DscPool.createInfoMaxSets = 1,
		Vk.DscPool.createInfoPoolSizes = [poolSize0, poolSize1] }
	poolSize0 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = 1 }
	poolSize1 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscPool.sizeDescriptorCount = 1 }

createDscSt :: KnownNat alm =>
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> Vk.Bffr.Binded sm sb nm '[VObj.Atom alm WModelViewProj 'Nothing] ->
	Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv  -> Vk.Smplr.S ss ->
	Vk.DscSetLyt.D sdsc '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom alm WModelViewProj 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ] ->
	(forall sds . Vk.DscSet.D sds '(sdsc, '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom alm WModelViewProj 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ]) -> IO a) -> IO a
createDscSt dvc dscp ub tximgvw txsmp dscslyt f =
	Vk.DscSet.allocateDs dvc allocInfo \(HPList.Singleton dscs) -> do
	Vk.DscSet.updateDs dvc (
		U5 (descriptorWrite0 ub dscs) :**
		U5 (descriptorWrite1 dscs tximgvw txsmp) :**
		HPList.Nil )
		HPList.Nil
	f dscs
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts =
			HPList.Singleton $ U2 dscslyt }

descriptorWrite0 :: KnownNat alm =>
	Vk.Bffr.Binded sm sb nm '[VObj.Atom alm WModelViewProj 'Nothing] ->
	Vk.DscSet.D sds slbts ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, VObj.Atom alm WModelViewProj  'Nothing)]) 0
descriptorWrite0 ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $ HPList.Singleton bufferInfo }
	where bufferInfo = U4 $ Vk.Dsc.BufferInfo ub

descriptorWrite1 ::
	Vk.DscSet.D sds slbts -> Vk.ImgVw.I nm fmt si -> Vk.Smplr.S ss ->
	Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgImage '[ '(ss, nm, fmt, si) ]) 0
descriptorWrite1 dscs tiv tsmp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeCombinedImageSampler,
	Vk.DscSet.writeSources = Vk.DscSet.ImageInfos . HPList.Singleton
		$ U4 Vk.Dsc.ImageInfo {
			Vk.Dsc.imageInfoImageLayout =
				Vk.Img.LayoutShaderReadOnlyOptimal,
			Vk.Dsc.imageInfoImageView = tiv,
			Vk.Dsc.imageInfoSampler = tsmp }
	}

createBufferAtom :: forall sd nm a b al . (Storable a, KnownNat al) => Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.Atom al a 'Nothing] ->
		Vk.Dvc.Mem.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[VObj.Atom al a 'Nothing] )] ->
			IO b) -> IO b
createBufferAtom p dv usg props = createBuffer p dv VObj.LengthAtom usg props

createBufferImage :: Storable (KObj.ImagePixel t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size, Vk.Dvc.Size) ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ VObj.Image 1 t inm] ->
		Vk.Dvc.Mem.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[ VObj.Image 1 t inm])] ->
		IO a) -> IO a
createBufferImage p dv (r, w, h, d) usg props =
	createBuffer p dv (VObj.LengthImage r w h d) usg props

createBuffer :: forall sd nm o a . VObj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Dvc.Mem.M sm
			'[ '(sb, 'Vk.Mm.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mm.M.requirementsMemoryTypeBits reqs) props
	Vk.Dvc.Mem.allocateBind dv (HPList.Singleton . U2 $ Vk.Dvc.Mem.Buffer b)
		(allcInfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Dvc.Mem.BufferBinded bnd))) -> bnd
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
	suitable props1 = fst <$> find ((&&)
		<$> (`Vk.Mm.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mm.M.mTypePropertyFlags . snd) tps
		where tps = Vk.Phd.memoryPropertiesMemoryTypes props1

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

beginSingleTimeCommands :: forall sd sc a .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	(forall s . Vk.CmdBffr.C s -> IO a) -> IO a
beginSingleTimeCommands dvc gq cp cmd = do
	Vk.CmdBffr.allocate
		dvc allocInfo \((cb :: Vk.CmdBffr.C s) :*. HPList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
				Vk.submitInfoCommandBuffers = HPList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HPList.Nil }
		Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo (cmd cb) <* do
			Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) Nothing
			Vk.Q.waitIdle gq
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

data SyncObjects (ssos :: (Type, Type, Type)) where
	SyncObjects :: {
		_imageAvailableSemaphores :: Vk.Semaphore.S sias,
		_renderFinishedSemaphores :: Vk.Semaphore.S srfs,
		_inFlightFences :: Vk.Fence.F sfs } ->
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

recordCommandBuffer :: forall scb sr sf sl sg sm sb nm sm' sb' nm' sdsl sds alm alv ali .
	(KnownNat alv, KnownNat ali) =>
	Vk.CmdBffr.C scb ->
	Vk.RndrPss.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.PplLyt.P sl '[PplLytArg sdsl alm] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, TexCoord)]
		'(sl, '[PplLytArg sdsl alm], '[]) ->
	Vk.Bffr.Binded sm sb nm '[VObj.List alv WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List ali Word16 ""] ->
	Vk.DscSet.D sds (PplLytArg sdsl alm) ->
	IO ()
recordCommandBuffer cb rp fb sce ppllyt gpl vb ib ubds =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb
		(HPList.Singleton . U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @"" vb) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word16 @"" ib) >>
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics ppllyt
		(HPList.Singleton $ U2 ubds)
		(HPList.Singleton (
			HPList.Nil :** HPList.Nil :**
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


type PplLytArg s al = '(s, DscStLytArg al)
mainloop ::
	(KnownNat alm, KnownNat alv, KnownNat ali, RecreateFrmbffrs ss sfs, Vk.T.FormatToValue scfmt) =>
	FramebufferResized ->
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) ss ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[PplLytArg sdsl alm] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, TexCoord)]
		'(sl, '[PplLytArg sdsl alm], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List alv WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List ali Word16 ""] ->
	Vk.Dvc.Mem.M sm2 '[ '(
		sb2,
		'Vk.Mm.BufferArg "uniform-buffer"
			'[VObj.Atom alm WModelViewProj 'Nothing] )] ->
	Vk.DscSet.D sds (PplLytArg sdsl alm) ->
	Vk.CmdBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> UTCTime -> IO ()
mainloop g w@(GlfwG.Win.W win) sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm0 = do
	($ ext0) $ fix \loop ext -> do
		Glfw.pollEvents
		tm <- getCurrentTime
		runLoop w sfc phdvc qfis dvc gq pq
			sc g ext scivs
			rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs
			(realToFrac $ tm `diffUTCTime` tm0) loop
	Vk.Dvc.waitIdle dvc

runLoop ::
	(RecreateFrmbffrs sis sfs, Vk.T.FormatToValue scfmt, KnownNat alm) =>
	(KnownNat alv, KnownNat ali) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> FramebufferResized -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[PplLytArg sdsl alm] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, TexCoord)]
		'(sl, '[PplLytArg sdsl alm], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List alv WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List ali Word16 ""] ->
	Vk.Dvc.Mem.M sm2 '[ '(
		sb2,
		'Vk.Mm.BufferArg "uniform-buffer"
			'[VObj.Atom alm WModelViewProj 'Nothing] )] ->
	Vk.DscSet.D sds (PplLytArg sdsl alm) ->
	Vk.CmdBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> Float ->
	(Vk.Extent2d -> IO ()) -> IO ()
runLoop w@(GlfwG.Win.W win) sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm loop = do
	catchAndRecreate w sfc phdvc qfis dvc sc scivs
		rp ppllyt gpl fbs loop
		$ drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib ubm ubds cb iasrfsifs tm
	cls <- Glfw.windowShouldClose win
	if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateSwapChainEtc
			w sfc phdvc qfis dvc sc scivs
			rp ppllyt gpl fbs)

drawFrame :: forall sfs sd ssc scfmt sr sl sg sm sb nm sm' sb' nm' sm2 sb2 scb sias srfs siff sdsl sds alm alv ali .
	(KnownNat alm, KnownNat alv, KnownNat ali) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[PplLytArg sdsl alm] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, TexCoord)]
		'(sl, '[PplLytArg sdsl alm], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List alv WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List ali Word16 ""] ->
	Vk.Dvc.Mem.M sm2 '[ '(
		sb2,
		'Vk.Mm.BufferArg "uniform-buffer"
			'[VObj.Atom alm WModelViewProj 'Nothing] )] ->
	Vk.DscSet.D sds (PplLytArg sdsl alm) ->
	Vk.CmdBffr.C scb -> SyncObjects '(sias, srfs, siff) -> Float -> IO ()
drawFrame dv gq pq sc ext rp ppllyt gpl fbs vb ib ubm ubds cb (SyncObjects ias rfs iff) tm = do
	let	siff = HPList.Singleton iff
	Vk.Fence.waitForFs dv siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dv sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dv siff
	Vk.CmdBffr.reset cb def
	HPList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext ppllyt gpl vb ib ubds
	updateUniformBuffer dv ubm ext tm
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers = HPList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
		presentInfo' = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphores = HPList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = HPList.Singleton
				$ Vk.Khr.SwapchainImageIndex sc imgIdx }
	Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfo'

updateUniformBuffer :: forall sd sm sb alm . KnownNat alm => Vk.Dvc.D sd ->
	Vk.Dvc.Mem.M sm '[ '(
		sb,
		'Vk.Mm.BufferArg "uniform-buffer"
			'[VObj.Atom alm WModelViewProj 'Nothing] )] ->
	Vk.Extent2d -> Float -> IO ()
updateUniformBuffer dvc um sce tm = do
	Vk.Dvc.Mem.write @"uniform-buffer"
		@(VObj.Atom alm WModelViewProj 'Nothing) dvc um zeroBits ubo
	where ubo = Foreign.Storable.Generic.W ModelViewProj {
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

catchAndRecreate ::
	(RecreateFrmbffrs sis sfs, Vk.T.FormatToValue scfmt) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[PplLytArg sdsl alm] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, TexCoord)]
		'(sl, '[PplLytArg sdsl alm], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	(Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate w@(GlfwG.Win.W win) sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateSwapChainEtc
		w sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs

recreateSwapChainEtc ::
	(RecreateFrmbffrs sis sfs, Vk.T.FormatToValue scfmt) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd ->
	Vk.Khr.Swpch.S scfmt ssc -> HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[PplLytArg sdsl alm] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, TexCoord)]
		'(sl, '[PplLytArg sdsl alm], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateSwapChainEtc w@(GlfwG.Win.W win) sfc phdvc qfis dvc sc scivs rp ppllyt gpl fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

--	ext <- recreateSwapChain win sfc phdvc qfis dvc sc
	ext <- recreateSwpch w sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swpch.getImages dvc sc >>= \imgs ->
			recreateImgVws dvc imgs scivs
		recreateGrPpl dvc ext rp ppllyt gpl
		recreateFrmbffrs dvc ext rp scivs fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize win = Glfw.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

type WVertex = Foreign.Storable.Generic.W Vertex

data Vertex = Vertex {
	vertexPos :: Cglm.Vec2, vertexColor :: Cglm.Vec3,
	vertexTexCoord :: TexCoord }
	deriving (Show, Generic)

newtype TexCoord = TexCoord Cglm.Vec2
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

instance Foreign.Storable.Generic.G Vertex where

vertices :: [WVertex]
vertices = Foreign.Storable.Generic.W <$> [
	Vertex (Cglm.Vec2 $ (- 0.5) :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL)
		(TexCoord . Cglm.Vec2 $ 1.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ 0.5 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL)
		(TexCoord . Cglm.Vec2 $ 0.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL)
		(TexCoord . Cglm.Vec2 $ 0.0 :. 1.0 :. NilL),
	Vertex (Cglm.Vec2 $ (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 1.0 :. NilL)
		(TexCoord . Cglm.Vec2 $ 1.0 :. 1.0 :. NilL) ]

indices :: [Word16]
indices = [0, 1, 2, 2, 3, 0]

type WModelViewProj = Foreign.Storable.Generic.W ModelViewProj

data ModelViewProj = ModelViewProj {
	uniformBufferObjectModel :: Cglm.Mat4,
	uniformBufferObjectView :: Cglm.Mat4,
	uniformBufferObjectProj :: Cglm.Mat4 }
	deriving (Show, Generic)

instance Storable ModelViewProj where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance Foreign.Storable.Generic.G ModelViewProj

[glslVertexShader|

#version 450

layout(binding = 0) uniform ModelViewProj {
	mat4 model;
	mat4 view;
	mat4 proj;
} ubo;

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec3 inColor;
layout(location = 2) in vec2 inTexCoord;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void
main()
{
	gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 0.0, 1.0);
	fragColor = inColor;
	fragTexCoord = inTexCoord;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragTexCoord;

layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) out vec4 outColor;

void
main()
{
//	outColor = texture(texSampler, fragTexCoord * 2.0);
	outColor = vec4(fragColor * texture(texSampler, fragTexCoord).rgb, 1.0);
}

|]
