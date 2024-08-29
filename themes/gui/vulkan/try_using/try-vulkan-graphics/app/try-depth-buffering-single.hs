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
import Control.Monad.Trans
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent.STM.ToolsYj
import Data.Kind
import Data.Proxy
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Default
import Data.Ord.ToolsYj
import Data.Bits
import Data.Bits.ToolsYj
import Data.Function.ToolsYj
import Data.Tuple.ToolsYj
import Data.Maybe
import Data.Maybe.ToolsYj
import Data.List qualified as L
import Data.List.Length
import Data.List.NonEmpty qualified as NE
import Data.List.ToolsYj
import Data.Array hiding (indices)
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.Bool
import Data.Bool.ToolsYj
import Data.Word
import Data.Text.IO qualified as Txt
import Data.Time
import Data.Color
import Codec.Picture
import Options.Declarative (Flag, Def, Cmd, run_, get)

import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH
import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object qualified as VObj
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
import Gpu.Vulkan.Sampler qualified as Vk.Smplr
import Gpu.Vulkan.Framebuffer qualified as Vk.Frmbffr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.Semaphore qualified as Vk.Semaphore
import Gpu.Vulkan.Fence qualified as Vk.Fence

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Pipeline.Graphics qualified as Vk.Ppl.Graphics
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.InputAssemblyState qualified as Vk.Ppl.InpAsmbSt
import Gpu.Vulkan.Pipeline.ViewportState qualified as Vk.Ppl.ViewportSt
import Gpu.Vulkan.Pipeline.RasterizationState qualified as Vk.Ppl.RstSt
import Gpu.Vulkan.Pipeline.MultisampleState qualified as Vk.Ppl.MltSmplSt
import Gpu.Vulkan.Pipeline.DepthStencilState qualified as Vk.Ppl.DptStnSt
import Gpu.Vulkan.Pipeline.ColorBlendAttachment qualified as Vk.Ppl.ClrBlndAtt
import Gpu.Vulkan.Pipeline.ColorBlendState qualified as Vk.Ppl.ClrBlndSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.PplLyt
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderModule
import Gpu.Vulkan.VertexInput qualified as Vk.VtxInp
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.ColorComponent qualified as Vk.ClrCmp
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPss
import Gpu.Vulkan.Attachment qualified as Vk.Att
import Gpu.Vulkan.Subpass qualified as Vk.Subpass
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSet
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt

import Gpu.Vulkan.Cglm qualified as Glm
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch
import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.DbgUtls.Msngr

import Vertex

import Debug

main :: IO ()
main = run_ realMain

realMain ::
	Flag "t" '["texture"] "FILEPATH" "texture filepath"
		(Def "../../../../../files/images/texture.jpg" String) ->
	Cmd "Try Vulkan Texture" ()
realMain txfp = liftIO $ atomically (newTVar False) >>= \fr -> withWin fr \w ->
	createIst \ist -> bool id (dbgm ist) debug $ body (get txfp) fr w ist
	where dbgm i = Vk.DbgUtls.Msngr.create i dbgMsngrInfo nil

type FramebufferResized = TVar Bool

withWin :: FramebufferResized -> (forall sw . GlfwG.Win.W sw -> IO a) -> IO a
withWin fr a = GlfwG.init error $ GlfwG.Win.group \g -> a =<< initWin fr g

initWin :: FramebufferResized -> GlfwG.Win.Group s () -> IO (GlfwG.Win.W s)
initWin fr g = do
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

body :: FilePath -> FramebufferResized -> GlfwG.Win.W sw -> Vk.Ist.I si -> IO ()
body txfp fr w ist =
	Vk.Khr.Sfc.Glfw.Win.create ist w nil \sfc ->
	pickPhd ist sfc >>= \(pd, qfis) ->
	createLgDvc pd qfis \d gq pq ->
	createCmdPl qfis d \cp ->
	createSwpch w sfc pd qfis d \(sc :: Vk.Khr.Swpch.S scifmt ss) ex ->
	Vk.Khr.Swpch.getImages d sc >>= \scis -> createImgVws d scis \scvs ->
	dptFmt pd Vk.Img.TilingOptimal \(_ :: Proxy dfmt) ->
	createDptRsrcs @dfmt pd d gq cp ex \drs@(_, _, dv) ->
	createRndrPss @scifmt @dfmt d \rp ->
	unfrmBffrOstAlgn pd \(_ :: Proxy alu) ->
	createPplLyt @alu d \dsl pl -> createGrPpl d ex rp pl \gp ->
	createFrmbffrs d ex rp scvs dv \fbs ->
	either error convertRGBA8 <$> readImage txfp >>= \txi ->
	createImg pd d gq cp (ImageRgba8 txi) \tx ->
	Vk.ImgVw.create d (imgVwInfo tx Vk.Img.AspectColorBit) nil \tv ->
	createTxSmplr pd d \txsp ->
	createVtxBffr pd d gq cp vertices \vb ->
	createIdxBffr pd d gq cp indices \ib ->
	createMvpBffr pd d \mb mbm ->
	createDscPl d \dp -> createDscSt d dp mb tv txsp dsl \ds ->
	Vk.CBffr.allocate d (cmdBffrInfo @'[ '()] cp) \(cb :*. HPList.Nil) ->
	createSyncObjs d \sos -> getCurrentTime >>=
	mainloop fr w sfc pd qfis d gq pq cp
		sc ex scvs rp pl gp fbs drs vb ib mbm ds cb sos

pickPhd :: Vk.Ist.I si -> Vk.Khr.Sfc.S ss -> IO (Vk.Phd.P, QFamIndices)
pickPhd ist sfc = Vk.Phd.enumerate ist >>= \case
	[] -> error "failed to find GPUs with Gpu.Vulkan support!"
	pds -> findMaybeM suit pds >>= \case
		Nothing -> error "failed to find a suitable GPU!"
		Just pdqfi -> pure pdqfi
	where
	suit pd = ((&&) <$> espt pd <*> sa pd) >>= bool (pure Nothing) do
		qfis <- findQFams pd sfc
		querySwpchSupport pd sfc \ss -> pure . bool qfis Nothing
			$	HPListC.null (snd $ formats ss) ||
				null (presentModes ss)
	espt pd = elemAll dvcExtensions
		. (Vk.Phd.extensionPropertiesExtensionName <$>)
		<$> Vk.Phd.enumerateExtensionProperties pd Nothing
	sa pd = Vk.Phd.featuresSamplerAnisotropy <$> Vk.Phd.getFeatures pd

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
		Vk.Dvc.createInfoEnabledFeatures = Just def {
			Vk.Phd.featuresSamplerAnisotropy = True } }

createSwpch :: GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> (forall ss scfmt .
		Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swpch.S scfmt ss -> Vk.Extent2d -> IO a) -> IO a
createSwpch w sfc pd qfis dv f = querySwpchSupport pd sfc \ss -> do
	ex <- swapExtent w $ capabilities ss
	let	cps = capabilities ss
		pm = findDefault Vk.Khr.Sfc.PresentModeFifo
			(== Vk.Khr.Sfc.PresentModeMailbox) $ presentModes ss
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
	presentModes :: [Vk.Khr.Sfc.PresentMode] }

deriving instance
	Show (HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts) =>
	Show (SwpchSupportDetails fmts)

chooseSwpSfcFmt :: (
	[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
	HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ) ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Khr.Sfc.Format fmt -> a) -> a
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`L.find` fmts)
	$ (== Vk.Khr.Sfc.ColorSpaceSrgbNonlinear) . Vk.Khr.Sfc.formatColorSpace
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
		pm = findDefault Vk.Khr.Sfc.PresentModeFifo
			(== Vk.Khr.Sfc.PresentModeMailbox) $ presentModesFmt ss
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
	presentModesFmt :: [Vk.Khr.Sfc.PresentMode] } deriving Show

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
	Vk.Khr.Swpch.createInfoImageUsage = Vk.Img.UsageColorAttachmentBit,
	Vk.Khr.Swpch.createInfoImageSharingMode = ism,
	Vk.Khr.Swpch.createInfoQueueFamilyIndices = qfis,
	Vk.Khr.Swpch.createInfoPreTransform =
		Vk.Khr.Sfc.capabilitiesCurrentTransform cps,
	Vk.Khr.Swpch.createInfoCompositeAlpha = Vk.Khr.Sfc.CompositeAlphaOpaqueBit,
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

dptFmt :: Vk.Phd.P -> Vk.Img.Tiling ->
	(forall (f :: Vk.T.Format) . Vk.T.FormatToValue f => Proxy f -> IO a) ->
	IO a
dptFmt pd tl a = (`Vk.T.formatToType` a) =<< spprt
	[Vk.FormatD32Sfloat, Vk.FormatD32SfloatS8Uint, Vk.FormatD24UnormS8Uint]
	Vk.FormatFeatureDepthStencilAttachmentBit where
	spprt :: [Vk.Format] -> Vk.FormatFeatureFlagBits -> IO Vk.Format
	spprt fs ffs = (fst <$>) . orErrorIO emsg
			. L.find (checkBits ffs . snd) . zip fs . (ftrs <$>) =<<
		Vk.Phd.getFormatProperties pd `mapM` fs
	ftrs = case tl of
		Vk.Img.TilingLinear -> Vk.formatPropertiesLinearTilingFeatures
		Vk.Img.TilingOptimal -> Vk.formatPropertiesOptimalTilingFeatures
		_ -> error "no such image tiling"
	emsg = "failed to find supported format!"

createDptRsrcs :: forall fmt sd sc nm a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> Vk.Extent2d ->
	(forall sm si sv .
		DptRsrcs si sm nm fmt sv -> IO a) -> IO a
createDptRsrcs pd dv gq cp (Vk.Extent2d w h) a =
	prepareImg pd dv
		Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mm.PropertyDeviceLocalBit w h \di dm ->
	Vk.ImgVw.create dv (imgVwInfo di Vk.Img.AspectDepthBit) nil \dvw ->
	transitionImgLyt dv gq cp di Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal >>
	a (di, dm, dvw)

recreateDptRsrcs :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> Vk.Extent2d ->
	DptRsrcs sb sm nm fmt sdvw -> IO ()
recreateDptRsrcs pd dv gq cp (Vk.Extent2d w h) (di, dm, dvw) = do
	reprepareImg pd dv
		Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mm.PropertyDeviceLocalBit w h di dm
	Vk.ImgVw.unsafeRecreate dv (imgVwInfo di Vk.Img.AspectDepthBit) nil dvw
	transitionImgLyt dv gq cp di Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal

type DptRsrcs sb sm nm fmt svw = (
	Vk.Img.Binded sm sb nm fmt,
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)], Vk.ImgVw.I nm fmt svw )

createImgVws :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Img.Binded ss ss inm fmt] ->
	(forall si . HPList.PL (Vk.ImgVw.I inm fmt) si -> IO a) -> IO a
createImgVws _dv [] f = f HPList.Nil
createImgVws dv (i : is) f =
	Vk.ImgVw.create dv (imgVwInfo i Vk.Img.AspectColorBit) nil \v ->
	createImgVws dv is \vs -> f $ v :** vs

recreateImgVws :: Vk.T.FormatToValue fmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss inm fmt] ->
	HPList.PL (Vk.ImgVw.I inm fmt) svs -> IO ()
recreateImgVws _dv [] HPList.Nil = pure ()
recreateImgVws dv (i : is) (v :** vs) =
	Vk.ImgVw.unsafeRecreate dv (imgVwInfo i Vk.Img.AspectColorBit) nil v >>
	recreateImgVws dv is vs
recreateImgVws _ _ _ =
	error "number of Vk.Image.I and Vk.ImageView.I should be same"

imgVwInfo :: Vk.Img.Binded sm si nm ifmt -> Vk.Img.AspectFlags ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt vfmt
imgVwInfo i a = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = i,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = def,
	Vk.ImgVw.createInfoSubresourceRange = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = a,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 } }

createRndrPss :: forall scifmt dptfmt sd a .
	(Vk.T.FormatToValue scifmt, Vk.T.FormatToValue dptfmt) =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPss.R sr -> IO a) -> IO a
createRndrPss dv = Vk.RndrPss.create @'Nothing @'[scifmt, dptfmt] dv info nil
	where
	info = Vk.RndrPss.CreateInfo {
		Vk.RndrPss.createInfoNext = TMaybe.N,
		Vk.RndrPss.createInfoFlags = zeroBits,
		Vk.RndrPss.createInfoAttachments = ca :** da :** HPList.Nil,
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
	da = Vk.Att.Description {
		Vk.Att.descriptionFlags = zeroBits,
		Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
		Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
		Vk.Att.descriptionStoreOp = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
		Vk.Att.descriptionStencilStoreOp = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Att.descriptionFinalLayout =
			Vk.Img.LayoutDepthStencilAttachmentOptimal }
	sbpss = Vk.Subpass.Description {
		Vk.Subpass.descriptionFlags = zeroBits,
		Vk.Subpass.descriptionPipelineBindPoint =
			Vk.Ppl.BindPointGraphics,
		Vk.Subpass.descriptionInputAttachments = [],
		Vk.Subpass.descriptionColorAndResolveAttachments = Left [car],
		Vk.Subpass.descriptionDepthStencilAttachment = Just dar,
		Vk.Subpass.descriptionPreserveAttachments = [] }
	car = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 0,
		Vk.Att.referenceLayout = Vk.Img.LayoutColorAttachmentOptimal }
	dar = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 1,
		Vk.Att.referenceLayout =
			Vk.Img.LayoutDepthStencilAttachmentOptimal }
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
	Vk.DscStLyt.D sdsl (DscStLytArg alu) ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] -> IO a) -> IO a
createPplLyt dv f = createDscStLyt dv \dsl ->
	Vk.PplLyt.create @_ @_ @_ @'[] dv (info dsl) nil $ f dsl
	where info dsl = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

createDscStLyt :: Vk.Dvc.D sd ->
	(forall s . Vk.DscStLyt.D s (DscStLytArg alu) -> IO a) -> IO a
createDscStLyt dv = Vk.DscStLyt.create dv info nil
	where
	info = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = mbd :** tbd :** HPList.Nil }
	mbd = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	tbd = Vk.DscStLyt.BindingImage {
		Vk.DscStLyt.bindingImageDescriptorType =
			Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscStLyt.bindingImageStageFlags =
			Vk.ShaderStageFragmentBit }

type DscStLytArg alu = '[BufferModelViewProj alu, TxImg]
type BufferModelViewProj alu = 'Vk.DscStLyt.Buffer '[AtomModelViewProj alu]
type AtomModelViewProj alu = VObj.Atom alu WModelViewProj 'Nothing
type TxImg = 'Vk.DscStLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)]

createGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) -> IO a) -> IO a
createGrPpl dv ex rp pl f = Vk.Ppl.Graphics.createGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	\(HPList.Singleton (U3 p)) -> f p

recreateGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) -> IO ()
recreateGrPpl dv ex rp pl p = Vk.Ppl.Graphics.unsafeRecreateGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	(HPList.Singleton $ U3 p)

grPplInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing
		'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
		'(	'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Pos), '(1, Color), '(2, TexCoord)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'Nothing '(sl, '[ '(sdsl, DscStLytArg alu)], '[])
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
	Vk.Ppl.Graphics.createInfoDepthStencilState = Just ds,
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
	ds = Vk.Ppl.DptStnSt.CreateInfo {
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
	HPList.PL (Vk.ImgVw.I inm fmt) sis -> Vk.ImgVw.I dptnm dptfmt siv ->
	(forall sfs . RecreateFrmbffrs sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HPList.Nil _ f = f HPList.Nil
createFrmbffrs dv ex rp (v :** vs) dvw f =
	Vk.Frmbffr.create dv (frmbffrInfo ex rp v dvw) nil \fb ->
	createFrmbffrs dv ex rp vs dvw \fbs -> f (fb :** fbs)

class RecreateFrmbffrs (sis :: [Type]) (sfs :: [Type]) where
	recreateFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
		HPList.PL (Vk.ImgVw.I inm fmt) sis ->
		Vk.ImgVw.I dptnm dptfmt siv ->
		HPList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFrmbffrs '[] '[] where
	recreateFrmbffrs _ _ _ HPList.Nil _ HPList.Nil = pure ()

instance RecreateFrmbffrs sis sfs =>
	RecreateFrmbffrs (si ': sis) (sf ': sfs) where
	recreateFrmbffrs dv ex rp (v :** vs) dvw (fb :** fbs) =
		Vk.Frmbffr.unsafeRecreate dv (frmbffrInfo ex rp v dvw) nil fb >>
		recreateFrmbffrs dv ex rp vs dvw fbs

frmbffrInfo :: Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I inm fmt si ->
	Vk.ImgVw.I dptnm dptfmt sdiv ->
	Vk.Frmbffr.CreateInfo 'Nothing sr
		'[ '(inm, fmt, si), '(dptnm, dptfmt, sdiv)]
frmbffrInfo ex rp att dpt = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = U3 att :** U3 dpt :** HPList.Nil,
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

createImg :: forall sd scp img inm a . BObj.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp -> img ->
	(forall si sm .
		Vk.Img.Binded sm si inm (BObj.ImageFormat img) -> IO a) -> IO a
createImg pd dv gq cp img a = prepareImg pd dv Vk.Img.TilingOptimal
	(Vk.Img.UsageTransferDstBit .|. Vk.Img.UsageSampledBit)
	Vk.Mm.PropertyDeviceLocalBit w h \i _m -> do
	createBffrImg pd dv
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) img
		\(b :: Vk.Bffr.Binded sm sb bnmi '[bimg]) bm -> do
		Vk.Mm.write @bnmi @bimg @0 dv bm zeroBits img
		transitionImgLyt dv gq cp i
			Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
		copyBffrToImg dv gq cp b i
	transitionImgLyt dv gq cp i
		Vk.Img.LayoutTransferDstOptimal
		Vk.Img.LayoutShaderReadOnlyOptimal
	a i
	where
	w = fromIntegral $ BObj.imageWidth img
	h = fromIntegral $ BObj.imageHeight img

prepareImg :: forall sd nm fmt a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mm.PropertyFlagBits -> Word32 -> Word32 ->
	(forall si sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mm.M sm '[ '(si, 'Vk.Mm.ImageArg nm fmt)] -> IO a) ->
	IO a
prepareImg pd dv tl us pr w h a =
	Vk.Img.create @'Nothing dv (imgInfo w h tl us) nil \i -> do
	rqs <- Vk.Img.getMemoryRequirements dv i
	mt <- findMmType pd (Vk.Mm.requirementsMemoryTypeBits rqs) pr
	Vk.Mm.allocateBind @'Nothing dv
		(HPList.Singleton . U2 $ Vk.Mm.Image i) (memInfo mt) nil
		\(HPList.Singleton (U2 (Vk.Mm.ImageBinded b))) m -> a b m

reprepareImg :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mm.PropertyFlags -> Word32 -> Word32 ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)] -> IO ()
reprepareImg pd dv tl us pr w h i m = do
	Vk.Img.unsafeRecreate @'Nothing dv (imgInfo w h tl us) nil i
	rqs <- Vk.Img.getMemoryRequirementsBinded dv i
	mt <- findMmType pd (Vk.Mm.requirementsMemoryTypeBits rqs) pr
	Vk.Mm.unsafeReallocateBind @'Nothing dv
		(HPList.Singleton . U2 $ Vk.Mm.ImageBinded i) (memInfo mt) nil m

imgInfo :: Word32 -> Word32 -> Vk.Img.Tiling -> Vk.Img.UsageFlags ->
	Vk.Img.CreateInfo 'Nothing fmt
imgInfo w h tl us = Vk.Img.CreateInfo {
		Vk.Img.createInfoNext = TMaybe.N,
		Vk.Img.createInfoImageType = Vk.Img.Type2d,
		Vk.Img.createInfoExtent = Vk.Extent3d {
			Vk.extent3dWidth = w, Vk.extent3dHeight = h,
			Vk.extent3dDepth = 1 },
		Vk.Img.createInfoMipLevels = 1,
		Vk.Img.createInfoArrayLayers = 1,
		Vk.Img.createInfoTiling = tl,
		Vk.Img.createInfoInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Img.createInfoUsage = us,
		Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Img.createInfoSamples = Vk.Sample.Count1Bit,
		Vk.Img.createInfoFlags = zeroBits,
		Vk.Img.createInfoQueueFamilyIndices = [] }

memInfo :: Vk.Mm.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
memInfo mt = Vk.Mm.AllocateInfo {
	Vk.Mm.allocateInfoNext = TMaybe.N,
	Vk.Mm.allocateInfoMemoryTypeIndex = mt }

createBffrImg :: forall sd img nm inm a . BObj.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	img -> (forall sm sb al . KnownNat al =>
		Vk.Bffr.Binded sm sb nm '[VObj.Image al img inm] ->
		Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.BufferArg nm '[VObj.Image al img inm]) ] ->
		IO a) -> IO a
createBffrImg p dv us prs img a =
	bffrAlgn @(VObj.Image 256 img inm) dv ln us \(_ :: Proxy al) ->
	createBffr @_ @_ @(VObj.Image al img inm) p dv ln us prs a
	where
	ln :: VObj.Length (VObj.Image al img inm)
	ln = VObj.LengthImage
		(BObj.imageRow img) (BObj.imageWidth img)
		(BObj.imageHeight img) (BObj.imageDepth img)

transitionImgLyt :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> Vk.Img.Binded sm si nm fmt ->
	Vk.Img.Layout -> Vk.Img.Layout -> IO ()
transitionImgLyt dv gq cp i ol nl = singleTimeCmds dv gq cp \cb ->
	Vk.Cmd.pipelineBarrier cb ss ds
		zeroBits HPList.Nil HPList.Nil . HPList.Singleton $ U5 brrr
	where
	brrr = Vk.Img.MemoryBarrier {
		Vk.Img.memoryBarrierNext = TMaybe.N,
		Vk.Img.memoryBarrierOldLayout = ol,
		Vk.Img.memoryBarrierNewLayout = nl,
		Vk.Img.memoryBarrierSrcQueueFamilyIndex = Vk.QFam.Ignored,
		Vk.Img.memoryBarrierDstQueueFamilyIndex = Vk.QFam.Ignored,
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
			bool zeroBits Vk.Img.AspectStencilBit stencil
		_ -> Vk.Img.AspectColorBit
	stencil = case Vk.T.formatToValue @fmt of
		Vk.FormatD32SfloatS8Uint -> True
		Vk.FormatD24UnormS8Uint -> True; _ -> False
	(sam, dam, ss, ds) = case (ol, nl) of
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

copyBffrToImg :: forall sd sc smb sbb nmb al img imgnm smi si nmi .
	(KnownNat al, Storable (BObj.ImagePixel img)) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded smb sbb nmb '[ VObj.Image al img imgnm]  ->
	Vk.Img.Binded smi si nmi (BObj.ImageFormat img) -> IO ()
copyBffrToImg dv gq cp bf img = singleTimeCmds dv gq cp \cb ->
	Vk.Cmd.copyBufferToImage @al @img @'[imgnm] cb bf img
		Vk.Img.LayoutTransferDstOptimal
		$ HPList.Singleton Vk.Bffr.ImageCopy {
			Vk.Bffr.imageCopyImageSubresource = isr,
			Vk.Bffr.imageCopyImageOffset = Vk.Offset3d 0 0 0,
			Vk.Bffr.imageCopyImageExtent = Vk.Extent3d w h 1 }
	where
	isr = Vk.Img.SubresourceLayers {
		Vk.Img.subresourceLayersAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceLayersMipLevel = 0,
		Vk.Img.subresourceLayersBaseArrayLayer = 0,
		Vk.Img.subresourceLayersLayerCount = 1 }
	VObj.LengthImage _r (fromIntegral -> w) (fromIntegral -> h) _d =
		VObj.lengthOf @(VObj.Image al img imgnm) $ Vk.Bffr.lengthBinded bf

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

createTxSmplr ::
	Vk.Phd.P -> Vk.Dvc.D sd -> (forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTxSmplr pd dv a = Vk.Phd.getProperties pd >>= \pr ->
	Vk.Smplr.create @'Nothing dv (info pr) nil a
	where info (Vk.Phd.propertiesLimits -> lm) = Vk.Smplr.CreateInfo {
		Vk.Smplr.createInfoNext = TMaybe.N,
		Vk.Smplr.createInfoFlags = zeroBits,
		Vk.Smplr.createInfoMagFilter = Vk.FilterLinear,
		Vk.Smplr.createInfoMinFilter = Vk.FilterLinear,
		Vk.Smplr.createInfoMipmapMode = Vk.Smplr.MipmapModeLinear,
		Vk.Smplr.createInfoAddressModeU = Vk.Smplr.AddressModeRepeat,
		Vk.Smplr.createInfoAddressModeV = Vk.Smplr.AddressModeRepeat,
		Vk.Smplr.createInfoAddressModeW = Vk.Smplr.AddressModeRepeat,
		Vk.Smplr.createInfoMipLodBias = 0,
		Vk.Smplr.createInfoAnisotropyEnable = True,
		Vk.Smplr.createInfoMaxAnisotropy =
			Vk.Phd.limitsMaxSamplerAnisotropy lm,
		Vk.Smplr.createInfoCompareEnable = False,
		Vk.Smplr.createInfoCompareOp = Vk.CompareOpAlways,
		Vk.Smplr.createInfoMinLod = 0,
		Vk.Smplr.createInfoMaxLod = 0,
		Vk.Smplr.createInfoBorderColor = Vk.BorderColorIntOpaqueBlack,
		Vk.Smplr.createInfoUnnormalizedCoordinates = False }

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
	bffrAlgn @(VObj.List 256 t lnm) dv (VObj.LengthList ln)
		(Vk.Bffr.UsageTransferDstBit .|. us) \(_ :: Proxy al) ->
	createBffrLst pd dv ln (Vk.Bffr.UsageTransferDstBit .|. us)
		Vk.Mm.PropertyDeviceLocalBit \b _ -> do
		createBffrLst pd dv ln
			Vk.Bffr.UsageTransferSrcBit (
			Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) \
			(b' :: Vk.Bffr.Binded sm sb bnm' '[VObj.List al t lnm'])
			bm' -> do
			Vk.Mm.write
				@bnm' @(VObj.List al t lnm') @0 dv bm' zeroBits xs
			copy b' b
		f b
	where
	copy :: forall sm sb bnm sm' sb' bnm' al . KnownNat al =>
		Vk.Bffr.Binded sm sb bnm '[VObj.List al t lnm] ->
		Vk.Bffr.Binded sm' sb' bnm' '[VObj.List al t lnm] -> IO ()
	copy s d = singleTimeCmds dv gq cp \cb ->
		Vk.Cmd.copyBuffer @'[ '( '[VObj.List al t lnm], 0, 0)] cb s d

bffrAlgn :: forall o sd a . VObj.SizeAlignment o =>
	Vk.Dvc.D sd -> VObj.Length o -> Vk.Bffr.UsageFlags ->
	(forall al . KnownNat al => Proxy al -> IO a) -> IO a
bffrAlgn dv ln us f = Vk.Bffr.create dv (bffrInfo ln us) nil \b ->
	(\(SomeNat p) -> f p) . someNatVal . fromIntegral =<<
	Vk.Mm.requirementsAlignment <$> Vk.Bffr.getMemoryRequirements dv b

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
		Vk.Bffr.Binded sm sb nm '[VObj.Atom al a 'Nothing] ->
		Vk.Mm.M sm '[ '(
			sb, 'Vk.Mm.BufferArg nm '[VObj.Atom al a 'Nothing] )] ->
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
	suit prs1 = fst <$> L.find ((&&)
		<$> (`Vk.Mm.elemTypeIndex` flt) . fst
		<*> checkBits prs . Vk.Mm.mTypePropertyFlags . snd)
			(Vk.Phd.memoryPropertiesMemoryTypes prs1)

createDscPl :: Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDscPl dv = Vk.DscPl.create dv info nil
	where
	info = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = 1,
		Vk.DscPl.createInfoPoolSizes = [sz0, sz1] }
	sz0 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPl.sizeDescriptorCount = 1 }
	sz1 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscPl.sizeDescriptorCount = 1 }

createDscSt :: KnownNat alu =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.Bffr.Binded sm sb bnm '[AtomModelViewProj alu] ->
	TextureImageView siv -> Vk.Smplr.S ss ->
	Vk.DscStLyt.D sdsl (DscStLytArg alu) ->
	(forall sds . Vk.DscSet.D sds '(sdsl, DscStLytArg alu) -> IO a) -> IO a
createDscSt dv dp bm tv ts dl a =
	Vk.DscSet.allocateDs dv info \(HPList.Singleton ds) -> (>> a ds)
	$ Vk.DscSet.updateDs dv (
		U5 (dscWrite0 ds bm) :** U5 (dscWrite1 ds tv ts) :**
		HPList.Nil ) HPList.Nil
	where info = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dp,
		Vk.DscSet.allocateInfoSetLayouts = HPList.Singleton $ U2 dl }

type TextureImageView siv = Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv

dscWrite0 :: KnownNat alu => Vk.DscSet.D sds slbts ->
	Vk.Bffr.Binded sm sb bnm '[AtomModelViewProj alu] ->
	Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgBuffer
			'[ '(sm, sb, bnm, AtomModelViewProj alu, 0)]) 0
dscWrite0 ds mb = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N, Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos
		. HPList.Singleton . U5 $ Vk.Dsc.BufferInfo mb }

dscWrite1 :: Vk.DscSet.D sds slbts -> Vk.ImgVw.I nm fmt si -> Vk.Smplr.S ss ->
	Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgImage '[ '(ss, nm, fmt, si) ]) 0
dscWrite1 ds v s = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N, Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeCombinedImageSampler,
	Vk.DscSet.writeSources = Vk.DscSet.ImageInfos . HPList.Singleton
		$ U4 Vk.Dsc.ImageInfo {
			Vk.Dsc.imageInfoImageLayout =
				Vk.Img.LayoutShaderReadOnlyOptimal,
			Vk.Dsc.imageInfoImageView = v,
			Vk.Dsc.imageInfoSampler = s } }

cmdBffrInfo :: forall n scp .
	Vk.CmdPl.C scp -> Vk.CBffr.AllocateInfo 'Nothing scp n
cmdBffrInfo cp = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cp,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

createSyncObjs :: Vk.Dvc.D sd -> (forall ssos . SyncObjs ssos -> IO a) -> IO a
createSyncObjs dv f =
	Vk.Semaphore.create @'Nothing dv def nil \ias ->
	Vk.Semaphore.create @'Nothing dv def nil \rfs ->
	Vk.Fence.create @'Nothing dv finfo nil \iff ->
	f $ SyncObjs ias rfs iff
	where
	finfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

data SyncObjs (ssos :: (Type, Type, Type)) where
	SyncObjs :: {
		_imageAvailableSemaphores :: Vk.Semaphore.S sias,
		_renderFinishedSemaphores :: Vk.Semaphore.S srfs,
		_inFlightFences :: Vk.Fence.F sfs } ->
		SyncObjs '(sias, srfs, sfs)

mainloop :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFrmbffrs svs sfs,
	KnownNat alu, KnownNat alv, KnownNat ali ) =>
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I inm scfmt) svs ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdvw ->
	Vk.Bffr.Binded smv sbv bnmv '[VObj.List alv WVertex nmv] ->
	Vk.Bffr.Binded smi sbi bnmi '[VObj.List ali Word16 nmi] ->
	ModelViewProjMemory smm sbm bnmm alu ->
	Vk.DscSet.D sds '(sdsl, DscStLytArg alu) ->
	Vk.CBffr.C scb -> SyncObjs ssos -> UTCTime -> IO ()
mainloop fr w sf pd qfis d gq pq cp sc ex0 vs rp pl gp fbs
	drs vb ib mm ds cb sos tm0 = do
	($ ex0) $ fix \go ex ->
		GlfwG.pollEvents >>
		getCurrentTime >>= \tm ->
		run fr w sf pd qfis d gq pq cp sc ex vs rp pl gp fbs drs vb ib
			mm ds cb sos (realToFrac $ tm `diffUTCTime` tm0) go
	Vk.Dvc.waitIdle d

run :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFrmbffrs svs sfs,
	KnownNat alu, KnownNat alv, KnownNat ali ) =>
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I inm scfmt) svs ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdvw ->
	Vk.Bffr.Binded smv sbv bnmv '[VObj.List alv WVertex nmv] ->
	Vk.Bffr.Binded smi sbi bnmi '[VObj.List ali Word16 nmi] ->
	ModelViewProjMemory smm sbm bnmm alu ->
	Vk.DscSet.D sds '(sdsl, DscStLytArg alu) ->
	Vk.CBffr.C scb -> SyncObjs ssos -> Float -> (Vk.Extent2d -> IO ()) ->
	IO ()
run fr w sfc pd qfis dv gq pq cp sc ex vs rp pl gp fbs
	drs vb ib mm ds cb sos tm go = do
	catchAndRecreate w sfc pd qfis dv gq cp sc vs rp pl gp drs fbs go
		$ draw dv gq pq sc ex rp pl gp fbs vb ib mm ds cb sos tm
	(,) <$> GlfwG.Win.shouldClose w <*> atomically (checkFlag fr) >>= \case
		(True, _) -> pure (); (_, False) -> go ex
		(_, _) -> go =<< recreateAll
			w sfc pd qfis dv gq cp sc vs rp pl gp drs fbs

draw :: forall sd fmt ssc sr sl sg sfs sds sdsl alu
	smv sbv bnmv alv nmv smi sbi bnmi ali nmi smm sbm bnmm scb ssos .
	(KnownNat alu, KnownNat alv, KnownNat ali) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S fmt ssc ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded smv sbv bnmv '[VObj.List alv WVertex nmv] ->
	Vk.Bffr.Binded smi sbi bnmi '[VObj.List ali Word16 nmi] ->
	ModelViewProjMemory smm sbm bnmm alu ->
	Vk.DscSet.D sds '(sdsl, DscStLytArg alu) ->
	Vk.CBffr.C scb -> SyncObjs ssos -> Float -> IO ()
draw dv gq pq sc ex rp pl gp fbs vb ib mm ds cb (SyncObjs ias rfs iff) tm = do
	Vk.Fence.waitForFs dv siff True Nothing >> Vk.Fence.resetFs dv siff
	ii <- Vk.Khr.Swpch.acquireNextImageResult
		[Vk.Success, Vk.SuboptimalKhr] dv sc maxBound (Just ias) Nothing
	Vk.CBffr.reset cb def
	HPList.index fbs ii \fb -> recordCmdBffr cb ex rp pl gp fb vb ib ds
	updateModelViewProj dv mm ex tm
	Vk.Q.submit gq (HPList.Singleton $ U4 sinfo) $ Just iff
	catchAndSerialize . Vk.Khr.Swpch.queuePresent pq $ pinfo ii
	where
	siff = HPList.Singleton iff
	sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks =
			HPList.Singleton $ Vk.SemaphorePipelineStageFlags
				ias Vk.Ppl.StageColorAttachmentOutputBit,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
	pinfo ii = Vk.Khr.Swpch.PresentInfo {
		Vk.Khr.Swpch.presentInfoNext = TMaybe.N,
		Vk.Khr.Swpch.presentInfoWaitSemaphores = HPList.Singleton rfs,
		Vk.Khr.Swpch.presentInfoSwapchainImageIndices =
			HPList.Singleton $ Vk.Khr.Swpch.SwapchainImageIndex sc ii }

updateModelViewProj :: forall sd smm sbm bnmm alu . KnownNat alu =>
	Vk.Dvc.D sd -> ModelViewProjMemory smm sbm bnmm alu ->
	Vk.Extent2d -> Float -> IO ()
updateModelViewProj dv mm Vk.Extent2d {
	Vk.extent2dWidth = fromIntegral -> w,
	Vk.extent2dHeight = fromIntegral -> h } tm =
	Vk.Mm.write @bnmm @(VObj.Atom alu WModelViewProj 'Nothing) @0
		dv mm zeroBits $ GStorable.W ModelViewProj {
			model = Glm.rotate Glm.mat4Identity (tm * Glm.rad 90)
				(Glm.Vec3 $ 0 :. 0 :. 1 :. NilL),
			view = Glm.lookat
				(Glm.Vec3 $ 2 :. 2 :. 2 :. NilL)
				(Glm.Vec3 $ 0 :. 0 :. 0 :. NilL)
				(Glm.Vec3 $ 0 :. 0 :. 1 :. NilL),
			projection = Glm.modifyMat4 1 1 negate
				$ Glm.perspective (Glm.rad 45) (w / h) 0.1 10 }

recordCmdBffr :: forall
	scb sr sf sl sg smv sbv bnmv smi sbi bnmi sdsl sds alu alv ali nmv nmi .
	(KnownNat alv, KnownNat ali) =>
	Vk.CBffr.C scb -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) ->
	Vk.Frmbffr.F sf ->
	Vk.Bffr.Binded smv sbv bnmv '[VObj.List alv WVertex nmv] ->
	Vk.Bffr.Binded smi sbi bnmi '[VObj.List ali Word16 nmi] ->
	Vk.DscSet.D sds '(sdsl, DscStLytArg alu) -> IO ()
recordCmdBffr cb ex rp pl gp fb vb ib ds =
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb info Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gp \cbb -> do
	Vk.Cmd.bindVertexBuffers cbb . HPList.Singleton
		. U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @nmv vb
	Vk.Cmd.bindIndexBuffer cbb
		$ Vk.Bffr.IndexedForList @_ @_ @_ @Word16 @nmi ib
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics pl
		(HPList.Singleton $ U2 ds)
		(HPList.Singleton $ HPList.Nil :** HPList.Nil :** HPList.Nil)
	Vk.Cmd.drawIndexed cbb indicesNum 1 0 0 0
	where
	info :: Vk.RndrPss.BeginInfo 'Nothing sr sf '[
		'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32,
		'Vk.ClearTypeDepthStencil ]
	info = Vk.RndrPss.BeginInfo {
		Vk.RndrPss.beginInfoNext = TMaybe.N,
		Vk.RndrPss.beginInfoRenderPass = rp,
		Vk.RndrPss.beginInfoFramebuffer = fb,
		Vk.RndrPss.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = ex },
		Vk.RndrPss.beginInfoClearValues =
			Vk.ClearValueColor (fromJust $ rgbaDouble 0 0 0 1) :**
			Vk.ClearValueDepthStencil (Vk.ClearDepthStencilValue 1 0) :**
			HPList.Nil }

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFrmbffrs svs sfs ) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QFamIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) svs ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdvw ->
	HPList.PL Vk.Frmbffr.F sfs -> (Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate w sfc pd qfis dv gq cp sc vs rp pl gp drs fbs go a =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just (); _ -> Nothing) a
	\_ -> go =<< recreateAll w sfc pd qfis dv gq cp sc vs rp pl gp drs fbs

recreateAll :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFrmbffrs svs sfs ) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QFamIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu)] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[ '(sdsl, DscStLytArg alu)], '[]) ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdvw ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateAll w sfc pd qfis dv gq cp sc vs rp pl gp drs@(_, _, dvw) fbs = do
	waitFramebufferSize w >> Vk.Dvc.waitIdle dv
	ex <- recreateSwpch w sfc pd qfis dv sc
	ex <$ do
		Vk.Khr.Swpch.getImages dv sc >>= \is -> recreateImgVws dv is vs
		recreateDptRsrcs pd dv gq cp ex drs
		recreateGrPpl dv ex rp pl gp
		recreateFrmbffrs dv ex rp vs dvw fbs

waitFramebufferSize :: GlfwG.Win.W sw -> IO ()
waitFramebufferSize w = GlfwG.Win.getFramebufferSize w >>= \sz ->
	when (zero sz) $ fix \go -> (`when` go) . zero =<<
		GlfwG.waitEvents *> GlfwG.Win.getFramebufferSize w
	where zero = uncurry (||) . ((== 0) *** (== 0))

vertices :: [WVertex]
vertices = GStorable.W <$> [
	Vertex (Pos . Glm.Vec3 $ (- 0.5) :. (- 0.5) :. 0 :. NilL)
		(Color . Glm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL)
		(TexCoord . Glm.Vec2 $ 1.0 :. 0.0 :. NilL),
	Vertex (Pos . Glm.Vec3 $ 0.5 :. (- 0.5) :. 0 :. NilL)
		(Color . Glm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL)
		(TexCoord . Glm.Vec2 $ 0.0 :. 0.0 :. NilL),
	Vertex (Pos . Glm.Vec3 $ 0.5 :. 0.5 :. 0 :. NilL)
		(Color . Glm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL)
		(TexCoord . Glm.Vec2 $ 0.0 :. 1.0 :. NilL),
	Vertex (Pos . Glm.Vec3 $ (- 0.5) :. 0.5 :. 0 :. NilL)
		(Color . Glm.Vec3 $ 1.0 :. 1.0 :. 1.0 :. NilL)
		(TexCoord . Glm.Vec2 $ 1.0 :. 1.0 :. NilL),

	Vertex (Pos . Glm.Vec3 $ (- 0.5) :. (- 0.5) :. (- 0.5) :. NilL)
		(Color . Glm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL)
		(TexCoord . Glm.Vec2 $ 1.0 :. 0.0 :. NilL),
	Vertex (Pos . Glm.Vec3 $ 0.5 :. (- 0.5) :. (- 0.5) :. NilL)
		(Color . Glm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL)
		(TexCoord . Glm.Vec2 $ 0.0 :. 0.0 :. NilL),
	Vertex (Pos . Glm.Vec3 $ 0.5 :. 0.5 :. (- 0.5) :. NilL)
		(Color . Glm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL)
		(TexCoord . Glm.Vec2 $ 0.0 :. 1.0 :. NilL),
	Vertex (Pos . Glm.Vec3 $ (- 0.5) :. 0.5 :. (- 0.5) :. NilL)
		(Color . Glm.Vec3 $ 1.0 :. 1.0 :. 1.0 :. NilL)
		(TexCoord . Glm.Vec2 $ 1.0 :. 1.0 :. NilL) ]

indicesNum :: Word32
indicesNum = fromIntegral $ length indices

indices :: [Word16]
indices = [
	0, 1, 2, 2, 3, 0,
	4, 5, 6, 6, 7, 4 ]

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
	type ImageFormat ImageRgba8 = 'Vk.T.FormatR8g8b8a8Srgb
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

[glslVertexShader|

#version 450

layout(binding = 0) uniform
	ModelViewProj { mat4 model; mat4 view; mat4 proj; } mvp;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inColor;
layout(location = 2) in vec2 inTexCoord;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void
main()
{
	gl_Position = mvp.proj * mvp.view * mvp.model * vec4(inPosition, 1.0);
	fragColor = inColor;
	fragTexCoord = inTexCoord;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragTexCoord;
layout(location = 0) out vec4 outColor;

layout(binding = 1) uniform sampler2D texSampler;

void
main()
{
//	outColor = texture(texSampler, fragTexCoord * 2.0);
	outColor = vec4(fragColor * texture(texSampler, fragTexCoord).rgb, 1.0);
}

|]
