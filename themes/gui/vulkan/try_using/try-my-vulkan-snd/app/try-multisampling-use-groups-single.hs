{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import qualified Gpu.Vulkan.Memory as Vk.Mm

import GHC.Generics
import GHC.TypeNats
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
import Data.Foldable
import Data.Default
import Data.Bits
import Data.Array hiding (indices)
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import qualified Data.HeteroParList as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Proxy
import Data.Bool
import Data.Maybe
import Data.List
import Data.IORef
import Data.List.Length
import Data.Word
import Data.Int
import Data.Color
import Data.Time
import Codec.Picture

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Storable as V
import qualified Data.Text.IO as Txt
import qualified Graphics.UI.GLFW as Glfw hiding (createWindowSurface)
import qualified Gpu.Vulkan.Cglm as Cglm
import qualified Foreign.Storable.Generic as GStorable

import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH

import Data.TypeLevel.ParMaybe (nil)

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.TypeEnum as Vk.T
import qualified Gpu.Vulkan.Exception as Vk
import qualified Gpu.Vulkan.Instance.Internal as Vk.Ist
import qualified Gpu.Vulkan.Khr as Vk.Khr
import qualified Gpu.Vulkan.Ext.DebugUtils as Vk.DbgUtls
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger as Vk.DbgUtls.Msngr
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam

import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Device as Vk.Dvc.M
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Sfc
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Sfc.Phd
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swpch
import qualified Gpu.Vulkan.Image as Vk.Img
import qualified Gpu.Vulkan.Image as Vk.Img.M
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
import qualified Gpu.Vulkan.PipelineLayout as Vk.PplLyt
import qualified Gpu.Vulkan.Attachment as Vk.Att
import qualified Gpu.Vulkan.Subpass as Vk.Subpass
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPss
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Framebuffer as Vk.Frmbffr
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr.M
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Mm.M
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet

import qualified Gpu.Vulkan.Sampler as Vk.Smplr
import qualified Gpu.Vulkan.Sampler as Vk.Smplr.M
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as Vk.Ppl.DptStnSt

import Tools (readRgba8, indexingVector)
import Vertex
import Vertex.Wavefront

import Control.Concurrent.STM
import Keyboard qualified as K

import Options.Declarative (Flag, Def, Cmd, run_, get)
import Control.Monad.Trans
import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win
import Data.Bits.ToolsYj
import Data.Function.ToolsYj
import Data.Tuple.ToolsYj
import Data.Bool.ToolsYj
import Data.List.ToolsYj
import Data.IORef.ToolsYj
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.Maybe.ToolsYj
import Data.Ord.ToolsYj
import Debug

main :: IO ()
main = run_ realMain

realMain ::
	Flag "t" '["texture"] "FILEPATH" "texture filepath"
		(Def "../../../../../files/models/viking_room.png" String) ->
	Flag "m" '["model"] "FILEPATH" "model filepath"
		(Def "../../../../../files/models/viking_room.obj" String) ->
	Flag "l" '["minLod"] "FLOAT" "minimal mip level" (Def "0.0" Double) ->
	Cmd "Try Vulkan Texture" ()
realMain txfp mdlfp mnld = liftIO $ newIORef False >>= \fr -> withWindow fr \w ->
	createIst \ist -> bool id (dbgm ist) debug
		$ body (get txfp) (get mdlfp) (realToFrac $ get mnld) fr w ist
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

body :: FilePath -> FilePath -> Float ->
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Ist.I si -> IO ()
body txfp mdlfp mnld fr w ist =
	Vk.Khr.Sfc.Glfw.Win.create ist w nil \sfc ->
	pickPhd ist sfc >>= \(pd, qfis, spcnt) ->
	createLgDvc pd qfis \d gq pq ->
	createCmdPl qfis d \cp ->
	createSwpch w sfc pd qfis d \(sc :: Vk.Khr.Swpch.S scifmt ss) ex ->
	Vk.Khr.Swpch.getImages d sc >>= \scis -> createImgVws d scis \scvs ->
	createClrRsrcs @scifmt pd d ex spcnt \crs@(_, _, cv, _) ->
	dptFmt pd Vk.Img.TilingOptimal \(_ :: Proxy dfmt) ->
	createDptRsrcs @dfmt pd d gq cp ex spcnt \drs@(_, _, dv) ->
	createRndrPss @scifmt @dfmt d spcnt \rp ->
	unfrmBffrOstAlgn pd \(_ :: Proxy alu) ->
	createPplLyt @alu d \dsl pl -> createGrPpl d ex rp pl spcnt \gp ->
	createFrmbffrs d ex rp scvs dv cv \fbs ->
	createTextureImage pd d gq cp txfp \tximg mplvs ->
	createImageView @'Vk.T.FormatR8g8b8a8Srgb d tximg Vk.Img.AspectColorBit mplvs \tximgvw ->
	createTextureSampler pd d mplvs mnld \txsmplr ->
	loadModel mdlfp >>= \(vtcs, idcs) ->

	createUniformBuffer pd d \ub ubm ->
	createDescriptorPool d \dscp ->
	createDescriptorSet d dscp ub tximgvw txsmplr dsl \ubds ->
	createCommandBuffer d cp \cb ->
	createSyncObjects d \sos ->

	K.newChans' K.gf >>= \(cke, kenvs) ->

	Vk.Bffr.group d nil \grp -> Vk.Mm.group d nil \mng ->
	Vk.Bffr.group d nil \grp' -> Vk.Mm.group d nil \mng' ->

	createVertexBuffer' pd d grp mng Glfw.Key'G gq cp vtcs >>= \vb ->
	createIndexBuffer' pd d grp' mng' Glfw.Key'G gq cp idcs >>= \ib ->

	getCurrentTime >>=
	mainLoop fr w sfc pd qfis d gq pq sc ex scvs rp pl gp fbs cp
		crs
		drs idcs (grp, mng) (grp', mng') (vb, ib) ubm ubds cb sos cke kenvs

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
		cnts <- ((.&.)	<$> Vk.Phd.limitsFramebufferDepthSampleCounts
				<*> Vk.Phd.limitsFramebufferColorSampleCounts)
			. Vk.Phd.propertiesLimits <$> Vk.Phd.getProperties pd
		pure . fromMaybe Vk.Sample.Count1Bit $ findBit [
			Vk.Sample.Count64Bit, Vk.Sample.Count32Bit,
			Vk.Sample.Count16Bit, Vk.Sample.Count8Bit,
			Vk.Sample.Count4Bit, Vk.Sample.Count2Bit ] cnts
	findBit bl bs = find ((/= zeroBits) . (.&. bs)) bl

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

data QFamIndices =
	QFamIndices { grFam :: Vk.QFam.Index, prFam :: Vk.QFam.Index }

findQFams :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (Maybe QFamIndices)
findQFams pd sfc = do
	prps@((fst <$>) -> is) <- Vk.Phd.getQueueFamilyProperties pd
	mp <- listToMaybe
		<$> filterM (flip (Vk.Khr.Sfc.Phd.getSupport pd) sfc) is
	pure $ QFamIndices <$> (fst <$> find (grbit . snd) prps) <*> mp
	where grbit = checkBits Vk.Q.GraphicsBit . Vk.QFam.propertiesQueueFlags

type QueueFamilyIndices = QFamIndices

dvcExtensions :: [Vk.Phd.ExtensionName]
dvcExtensions = [Vk.Khr.Swpch.extensionName]

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
	qinfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }
	uniqueQFams = nub [grFam qfis, prFam qfis]
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

chooseSwpSfcFmt :: (
	[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
	HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ) ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Khr.Sfc.Format fmt -> a) -> a
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`find` fmts)
	$ (== Vk.Khr.ColorSpaceSrgbNonlinear) . Vk.Khr.Sfc.formatColorSpace
chooseSwpSfcFmt (_, HPListC.Nil) _ = error "no available swap surface formats"

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

createImageView :: forall ivfmt sd si sm nm ifmt a .
	Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	Vk.Img.AspectFlags -> Word32 ->
	(forall siv . Vk.ImgVw.I nm ivfmt siv -> IO a) -> IO a
createImageView dvc timg asps mplvs f =
	Vk.ImgVw.create dvc (mkImageViewCreateInfo timg asps mplvs) nil f

mkImageViewCreateInfo ::
	Vk.Img.Binded sm si nm ifmt -> Vk.Img.AspectFlags -> Word32 ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt ivfmt
mkImageViewCreateInfo sci asps mplvs = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = sci,
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
		Vk.Img.M.subresourceRangeLevelCount = mplvs,
		Vk.Img.M.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.M.subresourceRangeLayerCount = 1 }

createRndrPss :: forall fmt dptfmt sd a .
	(Vk.T.FormatToValue fmt, Vk.T.FormatToValue dptfmt) =>
	Vk.Dvc.D sd -> Vk.Sample.CountFlags ->
	(forall sr . Vk.RndrPss.R sr -> IO a) -> IO a
createRndrPss dv spcnt = Vk.RndrPss.create @_ @'[fmt, dptfmt, fmt] dv info nil
	where
	info = Vk.RndrPss.CreateInfo {
		Vk.RndrPss.createInfoNext = TMaybe.N,
		Vk.RndrPss.createInfoFlags = zeroBits,
		Vk.RndrPss.createInfoAttachments =
			ca :** da :** rslv :** HPList.Nil,
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
	da = Vk.Att.Description {
		Vk.Att.descriptionFlags = zeroBits,
		Vk.Att.descriptionSamples = spcnt,
		Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
		Vk.Att.descriptionStoreOp = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
		Vk.Att.descriptionStencilStoreOp = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Att.descriptionFinalLayout =
			Vk.Img.LayoutDepthStencilAttachmentOptimal }
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
		Vk.Subpass.descriptionDepthStencilAttachment = Just dar,
		Vk.Subpass.descriptionPreserveAttachments = [] }
	car = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 0,
		Vk.Att.referenceLayout = Vk.Img.LayoutColorAttachmentOptimal }
	dar = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 1,
		Vk.Att.referenceLayout =
			Vk.Img.LayoutDepthStencilAttachmentOptimal }
	rslvr = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 2,
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

type AtomUbo s alu = '(s, '[
	'Vk.DscSetLyt.Buffer '[VObj.Atom alu WModelViewProj 'Nothing],
	'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ])

unfrmBffrOstAlgn ::
	Vk.Phd.P -> (forall a . KnownNat a => Proxy a -> IO b) -> IO b
unfrmBffrOstAlgn pd f = (\(SomeNat p) -> f p) . someNatVal . fromIntegral
	. Vk.Phd.limitsMinUniformBufferOffsetAlignment . Vk.Phd.propertiesLimits
	=<< Vk.Phd.getProperties pd

createPplLyt :: forall alm sd a . Vk.Dvc.D sd -> (forall sl sdsl .
	Vk.DscSetLyt.D sdsl (DscStLytArg alm) ->
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

type DscStLytArg alm = '[BufferModelViewProj alm, TxImg]
type BufferModelViewProj alm = 'Vk.DscSetLyt.Buffer '[AtomModelViewProj alm]
type AtomModelViewProj alm = VObj.Atom alm WModelViewProj 'Nothing
type TxImg = 'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)]

createGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alm)] '[] ->
	Vk.Sample.CountFlags ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[ '(sdsl, DscStLytArg alm)], '[]) -> IO a) -> IO a
createGrPpl dv ex rp pl spcnt f = Vk.Ppl.Graphics.createGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl spcnt) nil
	\(HPList.Singleton (U3 p)) -> f p

recreateGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alm)] '[] ->
	Vk.Sample.CountFlags ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[ '(sdsl, DscStLytArg alm)], '[]) -> IO ()
recreateGrPpl dv ex rp pl spcnt p = Vk.Ppl.Graphics.unsafeRecreateGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl spcnt) nil
	(HPList.Singleton $ U3 p)

grPplInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alm)] '[] ->
	Vk.Sample.CountFlags ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing
		'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
		'(	'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Pos), '(1, Color), '(2, TexCoord)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'Nothing '(sl, '[ '(sdsl, DscStLytArg alm)], '[])
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
			Vk.Sample.CountAndMask spcnt Nothing,
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
	Vk.ImgVw.I clrnm clrfmt sciv ->
	(forall sfs . RecreateFrmbffrs sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HPList.Nil _ _ f = f HPList.Nil
createFrmbffrs dv ex rp (v :** vs) dvw cvw f =
	Vk.Frmbffr.create dv (frmbffrInfo ex rp v dvw cvw) nil \fb ->
	createFrmbffrs dv ex rp vs dvw cvw \fbs -> f (fb :** fbs)

class RecreateFrmbffrs (sis :: [Type]) (sfs :: [Type]) where
	recreateFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
		HPList.PL (Vk.ImgVw.I inm fmt) sis ->
		Vk.ImgVw.I dptnm dptfmt siv ->
		Vk.ImgVw.I clrnm clrfmt clrsdiv ->
		HPList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFrmbffrs '[] '[] where
	recreateFrmbffrs _ _ _ HPList.Nil _ _ HPList.Nil = pure ()

instance RecreateFrmbffrs sis sfs =>
	RecreateFrmbffrs (si ': sis) (sf ': sfs) where
	recreateFrmbffrs d ex rp (v :** vs) dv cv (fb :** fbs) = do
		Vk.Frmbffr.unsafeRecreate d (frmbffrInfo ex rp v dv cv) nil fb
		recreateFrmbffrs d ex rp vs dv cv fbs

frmbffrInfo :: Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I inm fmt si ->
	Vk.ImgVw.I dptnm dptfmt sdiv ->
	Vk.ImgVw.I clrnm clrfmt sciv ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[
		'(clrnm, clrfmt, sciv),
		'(dptnm, dptfmt, sdiv), '(inm, fmt, si) ]
frmbffrInfo ex rp att dpt clr = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments =
		U3 clr :** U3 dpt :** U3 att :** HPList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = ex

type RecreateFramebuffers = RecreateFrmbffrs

createCmdPl :: QFamIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfis dv = Vk.CmdPl.create dv info nil
	where info = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = grFam qfis }

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

type ColorResources fmt nm si sm siv = (
	Vk.Img.Binded sm si nm fmt,
	Vk.Mm.M sm '[ '(si, 'Vk.Mm.ImageArg nm fmt)],
	Vk.ImgVw.I nm fmt siv,
	Vk.Sample.CountFlags )

createDptRsrcs :: forall fmt sd sc nm a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> Vk.Extent2d ->
	Vk.Sample.CountFlags -> (forall sm si sv .
		DptRsrcs si sm nm fmt sv -> IO a) -> IO a
createDptRsrcs pd dv gq cp (Vk.Extent2d w h) spcnt a =
	prepareImg pd dv
		Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mm.PropertyDeviceLocalBit w h 1 spcnt \di dm ->
	Vk.ImgVw.create dv (imgVwInfo di Vk.Img.AspectDepthBit 1) nil \dvw ->
	transitionImgLyt dv gq cp di Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal 1 >>
	a (di, dm, dvw)

recreateDptRsrcs :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> Vk.Extent2d ->
	Vk.Sample.CountFlags -> DptRsrcs sb sm nm fmt sdvw -> IO ()
recreateDptRsrcs pd dv gq cp (Vk.Extent2d w h) spcnt (di, dm, dvw) = do
	reprepareImg pd dv
		Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mm.PropertyDeviceLocalBit w h 1 spcnt di dm
	Vk.ImgVw.unsafeRecreate dv (imgVwInfo di Vk.Img.AspectDepthBit 1) nil dvw
	transitionImgLyt dv gq cp di Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal 1

type DptRsrcs sb sm nm fmt svw = (
	Vk.Img.Binded sm sb nm fmt,
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)], Vk.ImgVw.I nm fmt svw )

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
	Vk.Img.unsafeRecreate @'Nothing dv (imgInfo w h ml spcnt tl us) nil i
	rqs <- Vk.Img.getMemoryRequirementsBinded dv i
	mt <- findMmType pd (Vk.Mm.requirementsMemoryTypeBits rqs) pr
	Vk.Mm.unsafeReallocateBind @'Nothing dv
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

transitionImgLyt :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> Vk.Img.Binded sm si nm fmt ->
	Vk.Img.Layout -> Vk.Img.Layout -> Word32 -> IO ()
transitionImgLyt dv gq cp i ol nl ml = singleTimeCmds dv gq cp \cb ->
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
		Vk.Img.subresourceRangeLevelCount = ml,
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

cmdBffrInfo :: forall n scp .
	Vk.CmdPl.C scp -> Vk.CBffr.AllocateInfo 'Nothing scp n
cmdBffrInfo cp = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cp,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

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

dptFmt :: Vk.Phd.P -> Vk.Img.Tiling ->
	(forall (f :: Vk.T.Format) . Vk.T.FormatToValue f => Proxy f -> IO a) ->
	IO a
dptFmt pd tl a = (`Vk.T.formatToType` a) =<< spprt
	[Vk.FormatD32Sfloat, Vk.FormatD32SfloatS8Uint, Vk.FormatD24UnormS8Uint]
	Vk.FormatFeatureDepthStencilAttachmentBit where
	spprt :: [Vk.Format] -> Vk.FormatFeatureFlagBits -> IO Vk.Format
	spprt fs fffs = (fst <$>) . orErrorIO emsg
			. find (checkBits fffs . snd) . zip fs . (ftrs <$>) =<<
		Vk.Phd.getFormatProperties pd `mapM` fs
	ftrs = case tl of
		Vk.Img.TilingLinear -> Vk.formatPropertiesLinearTilingFeatures
		Vk.Img.TilingOptimal -> Vk.formatPropertiesOptimalTilingFeatures
		_ -> error "no such image tiling"
	emsg = "failed to find supported format!"

createTextureImage ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> FilePath -> (
		forall si sm .
		Vk.Img.Binded sm si nm 'Vk.T.FormatR8g8b8a8Srgb -> Word32 -> IO a ) ->
	IO a
createTextureImage pd dvc gq cp fp f = do
	img <- readRgba8 fp
	putStrLn "CREATE TEXTURE"
	print . V.length $ imageData img
	let	wdt_ = imageWidth img
		hgt_ = imageHeight img
		wdt = fromIntegral wdt_
		hgt = fromIntegral hgt_
		mipLevels :: Word32 = floor @Double . logBase 2
			$ max (fromIntegral wdt_) (fromIntegral hgt_)
	print (mipLevels :: Word32)
	createImage @_ @'Vk.T.FormatR8g8b8a8Srgb pd dvc wdt hgt mipLevels
		Vk.Sample.Count1Bit Vk.Img.TilingOptimal
		(	Vk.Img.UsageTransferSrcBit .|.
			Vk.Img.UsageTransferDstBit .|.
			Vk.Img.UsageSampledBit)
		Vk.Mm.PropertyDeviceLocalBit \tximg _txmem -> do
		createBufferImage @MyImage @_ pd dvc
			(fromIntegral wdt, fromIntegral wdt, fromIntegral hgt, 1)
			Vk.Bffr.UsageTransferSrcBit
			(	Vk.Mm.PropertyHostVisibleBit .|.
				Vk.Mm.PropertyHostCoherentBit )
			\(sb :: Vk.Bffr.Binded
				sm sb "texture-buffer" '[ VObj.Image 1 a inm]) sbm -> do
			Vk.Mm.write @"texture-buffer"
				@(VObj.Image 1 MyImage inm) dvc sbm zeroBits (MyImage img)
			print sb
			transitionImageLayout dvc gq cp tximg
				Vk.Img.LayoutUndefined
				Vk.Img.LayoutTransferDstOptimal mipLevels
			copyBufferToImage dvc gq cp sb tximg wdt hgt
			generateMipmaps pd dvc gq cp tximg mipLevels (fromIntegral wdt_) (fromIntegral hgt_)
			f tximg mipLevels

checkImageFilterLinearBit ::
	forall fmt . Vk.T.FormatToValue fmt => Vk.Phd.P -> IO ()
checkImageFilterLinearBit pd = do
	let	fmt = Vk.T.formatToValue @fmt
	formatProperties <- Vk.Phd.getFormatProperties pd fmt
	let	bt = Vk.formatPropertiesOptimalTilingFeatures
				formatProperties .&.
			Vk.FormatFeatureSampledImageFilterLinearBit
	when (bt == zeroBits) $ error
		"texture image format does not support linear blitting!"

generateMipmaps :: forall sd scp si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp ->
	Vk.Img.Binded sm si nm fmt -> Word32 -> Int32 -> Int32 -> IO ()
generateMipmaps pd dvc gq cp img mlvs wdt hgt = do
	putStrLn "GENERATE MIPMAPS"
	checkImageFilterLinearBit @fmt pd
	beginSingleTimeCommands dvc gq cp \cb -> do
		for_ iwhs \(i, (w, h)) -> generateMipmap1 cb img i w h
		Vk.Cmd.pipelineBarrier cb
			Vk.Ppl.StageTransferBit Vk.Ppl.StageFragmentShaderBit zeroBits
			HPList.Nil HPList.Nil (HPList.Singleton $ U5 barrier)
	where
	iwhs = [1 .. mlvs - 1] `zip` (halves wdt `zip` halves hgt)
	barrier :: Vk.Img.MemoryBarrier 'Nothing sm si nm fmt
	barrier = mipmapBarrier
		Vk.AccessTransferWriteBit Vk.AccessShaderReadBit
		Vk.Img.LayoutTransferDstOptimal
		Vk.Img.LayoutShaderReadOnlyOptimal img mlvs

mipmapBarrier :: Vk.AccessFlags -> Vk.AccessFlags ->
	Vk.Img.Layout -> Vk.Img.Layout -> Vk.Img.Binded sm si nm fmt ->
	Word32 -> Vk.Img.MemoryBarrier 'Nothing sm si nm fmt
mipmapBarrier sam dam olyt nlyt img i = Vk.Img.MemoryBarrier {
	Vk.Img.memoryBarrierNext = TMaybe.N,
	Vk.Img.memoryBarrierSrcAccessMask = sam,
	Vk.Img.memoryBarrierDstAccessMask = dam,
	Vk.Img.memoryBarrierOldLayout = olyt,
	Vk.Img.memoryBarrierNewLayout = nlyt,
	Vk.Img.memoryBarrierSrcQueueFamilyIndex = Vk.QFam.Ignored,
	Vk.Img.memoryBarrierDstQueueFamilyIndex = Vk.QFam.Ignored,
	Vk.Img.memoryBarrierImage = img,
	Vk.Img.memoryBarrierSubresourceRange = srr }
	where
	srr = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = i - 1,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 }

generateMipmap1 :: forall scb si sm nm fmt . Vk.CBffr.C scb ->
	Vk.Img.Binded sm si nm fmt -> Word32 -> Int32 -> Int32 -> IO ()
generateMipmap1 cb img i w h = do
	Vk.Cmd.pipelineBarrier cb
		Vk.Ppl.StageTransferBit Vk.Ppl.StageTransferBit zeroBits
		HPList.Nil HPList.Nil . HPList.Singleton $ U5 barrier'
	Vk.Cmd.blitImage cb
		img Vk.Img.LayoutTransferSrcOptimal
		img Vk.Img.LayoutTransferDstOptimal
		[blit] Vk.FilterLinear
	Vk.Cmd.pipelineBarrier cb
		Vk.Ppl.StageTransferBit Vk.Ppl.StageFragmentShaderBit zeroBits
		HPList.Nil HPList.Nil . HPList.Singleton $ U5 barrier''
	where
	barrier' :: Vk.Img.MemoryBarrier 'Nothing sm si nm fmt
	barrier' = mipmapBarrier Vk.AccessTransferWriteBit Vk.AccessTransferReadBit
		Vk.Img.LayoutTransferDstOptimal Vk.Img.LayoutTransferSrcOptimal
		img i
	barrier'' :: Vk.Img.MemoryBarrier 'Nothing sm si nm fmt
	barrier'' = mipmapBarrier Vk.AccessTransferReadBit Vk.AccessShaderReadBit
		Vk.Img.LayoutTransferSrcOptimal Vk.Img.LayoutShaderReadOnlyOptimal
		img i
	blit = Vk.Img.M.Blit {
		Vk.Img.M.blitSrcSubresource = bssr,
		Vk.Img.M.blitSrcOffsetFrom = Vk.Offset3d 0 0 0,
		Vk.Img.M.blitSrcOffsetTo = Vk.Offset3d w h 1,
		Vk.Img.M.blitDstSubresource = bdsr,
		Vk.Img.M.blitDstOffsetFrom = Vk.Offset3d 0 0 0,
		Vk.Img.M.blitDstOffsetTo = Vk.Offset3d (half w) (half h) 1 }
	bssr = Vk.Img.M.SubresourceLayers {
		Vk.Img.M.subresourceLayersAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.M.subresourceLayersMipLevel = i - 1,
		Vk.Img.M.subresourceLayersBaseArrayLayer = 0,
		Vk.Img.M.subresourceLayersLayerCount = 1 }
	bdsr = Vk.Img.M.SubresourceLayers {
		Vk.Img.M.subresourceLayersAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.M.subresourceLayersMipLevel = i,
		Vk.Img.M.subresourceLayersBaseArrayLayer = 0,
		Vk.Img.M.subresourceLayersLayerCount = 1 }

half :: Integral i => i -> i
half n = bool 1 (n `div` 2) (n > 1)

halves :: Integral i => i -> [i]
halves = iterate half

newtype MyImage = MyImage (Image PixelRGBA8)

newtype MyRgba8 = MyRgba8 { _unMyRgba8 :: PixelRGBA8 }

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

createImage :: forall nm fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P ->
	Vk.Dvc.D sd -> Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mm.PropertyFlagBits -> (forall si sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mm.M sm
			'[ '(si, 'Vk.Mm.ImageArg nm fmt) ] ->
		IO a) -> IO a
createImage pd dvc wdt hgt mplvs nss tlng usg prps f = Vk.Img.create @'Nothing dvc
		(imageInfo wdt hgt mplvs nss tlng usg) nil \img -> do
	mi <- imageMemoryInfo pd dvc prps img
	imageAllocateBind dvc img mi f

imageInfo ::
	Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags -> Vk.Img.Tiling -> Vk.Img.UsageFlags ->
	Vk.Img.CreateInfo 'Nothing fmt
imageInfo wdt hgt mplvs numSamples tlng usg = Vk.Img.CreateInfo {
		Vk.Img.createInfoNext = TMaybe.N,
		Vk.Img.createInfoImageType = Vk.Img.Type2d,
		Vk.Img.createInfoExtent = Vk.Extent3d {
			Vk.extent3dWidth = wdt,
			Vk.extent3dHeight = hgt,
			Vk.extent3dDepth = 1 },
		Vk.Img.createInfoMipLevels = mplvs,
		Vk.Img.createInfoArrayLayers = 1,
		Vk.Img.createInfoTiling = tlng,
		Vk.Img.createInfoInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Img.createInfoUsage = usg,
		Vk.Img.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Img.createInfoSamples = numSamples,
		Vk.Img.createInfoFlags = zeroBits,
		Vk.Img.createInfoQueueFamilyIndices = [] }

imageAllocateBind :: Vk.Dvc.D sd -> Vk.Img.I si nm fmt ->
	Vk.Mm.AllocateInfo 'Nothing -> (forall sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mm.M sm
			'[ '(si, 'Vk.Mm.ImageArg nm fmt) ] ->
		IO a) -> IO a
imageAllocateBind dvc img mi f =
	Vk.Mm.allocateBind @'Nothing dvc
		(HPList.Singleton . U2 $ Vk.Mm.Image img) mi
		nil \(HPList.Singleton (U2 (Vk.Mm.ImageBinded bnd))) m -> do
		f bnd m

imageMemoryInfo ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Mm.PropertyFlags ->
	Vk.Img.I s nm fmt -> IO (Vk.Mm.AllocateInfo 'Nothing)
imageMemoryInfo pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirements dvc img
	mt <- findMemoryType pd (Vk.Mm.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

transitionImageLayout :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Img.Binded sm si nm fmt -> Vk.Img.Layout -> Vk.Img.Layout ->
	Word32 ->
	IO ()
transitionImageLayout dvc gq cp img olyt nlyt mplvs =
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
			Vk.Img.subresourceRangeAspectMask = asps,
			Vk.Img.subresourceRangeBaseMipLevel = 0,
			Vk.Img.subresourceRangeLevelCount = mplvs,
			Vk.Img.subresourceRangeBaseArrayLayer = 0,
			Vk.Img.subresourceRangeLayerCount = 1 }
	Vk.Cmd.pipelineBarrier cb
		sstg dstg zeroBits HPList.Nil HPList.Nil (HPList.Singleton $ U5 barrier)
	where
	asps = case (nlyt, hasStencilComponentType @fmt) of
		(Vk.Img.LayoutDepthStencilAttachmentOptimal, hsst) ->
			Vk.Img.AspectDepthBit .|.
			bool zeroBits Vk.Img.AspectStencilBit hsst
		_ -> Vk.Img.AspectColorBit
	(sam, dam, sstg, dstg) = case (olyt, nlyt) of
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

hasStencilComponentType ::
	forall (fmt :: Vk.T.Format) . Vk.T.FormatToValue fmt => Bool
hasStencilComponentType = hasStencilComponent (Vk.T.formatToValue @fmt)

hasStencilComponent :: Vk.Format -> Bool
hasStencilComponent = \case
	Vk.FormatD32SfloatS8Uint -> True
	Vk.FormatD24UnormS8Uint -> True
	_ -> False

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
		isr = Vk.Img.M.SubresourceLayers {
			Vk.Img.M.subresourceLayersAspectMask =
				Vk.Img.AspectColorBit,
			Vk.Img.M.subresourceLayersMipLevel = 0,
			Vk.Img.M.subresourceLayersBaseArrayLayer = 0,
			Vk.Img.M.subresourceLayersLayerCount = 1 }
	Vk.Cmd.copyBufferToImage @1
		cb bf img Vk.Img.LayoutTransferDstOptimal (HPList.Singleton region)

createTextureSampler :: Vk.Phd.P -> Vk.Dvc.D sd ->
	Word32 -> Float -> (forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTextureSampler pd dvc mplvs mnld f = do
	prp <- Vk.Phd.getProperties pd
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
			Vk.Smplr.M.createInfoMinLod = mnld,
			Vk.Smplr.M.createInfoMaxLod = fromIntegral mplvs,
			Vk.Smplr.M.createInfoBorderColor =
				Vk.BorderColorIntOpaqueBlack,
			Vk.Smplr.M.createInfoUnnormalizedCoordinates = False }
	Vk.Smplr.create @'Nothing dvc samplerInfo nil f

loadModel :: FilePath -> IO (V.Vector WVertex, V.Vector Word32)
loadModel fp = do
	vtcs <- readVertices fp
	let	(vtcs', idcs') = indexingVector vtcs
	putStrLn "LOAD MODEL"
	putStrLn $ "vtcs : " ++ show (V.length (vtcs :: V.Vector WVertex))
	putStrLn $ "vtcs': " ++ show (V.length (vtcs' :: V.Vector WVertex))
	putStrLn $ "idcs': " ++ show (V.length (idcs':: V.Vector Word32))
	pure (vtcs', idcs')

createVertexBuffer' :: Ord k => Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[VObj.List 256 WVertex ""] ->
	Vk.Mm.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mm.BufferArg nm '[VObj.List 256 WVertex ""])] ->
	k -> Vk.Q.Q -> Vk.CmdPl.C sc -> V.Vector WVertex ->
	IO (Vk.Bffr.Binded sm sb nm '[ VObj.List 256 WVertex ""])
createVertexBuffer' pd dvc grp mng k gq cp vtcs =
	createBufferList' pd dvc grp mng k (fromIntegral $ V.length vtcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit >>= \(b, _) -> do
	createBufferList pd dvc (fromIntegral $ V.length vtcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
		\(b' :: Vk.Bffr.Binded sm sb "vertex-buffer" '[ VObj.List 256 t ""])
			(bm' :: Vk.Mm.M sm '[ '(
				sb,
				'Vk.Mm.BufferArg "vertex-buffer"
					'[ VObj.List 256 WVertex ""] ) ]) -> do
		Vk.Mm.write
			@"vertex-buffer" @(VObj.List 256 WVertex "") dvc bm' zeroBits vtcs
		copyBuffer dvc gq cp b' b
	pure b

createIndexBuffer' :: Ord k => Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[VObj.List 256 Word32 ""] ->
	Vk.Mm.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mm.BufferArg nm '[VObj.List 256 Word32 ""])]  ->
	k -> Vk.Q.Q -> Vk.CmdPl.C sc -> V.Vector Word32 ->
	IO (Vk.Bffr.Binded sm sb nm '[ VObj.List 256 Word32 ""])
createIndexBuffer' pd dvc grp mng k gq cp idcs = do
	(b, _) <- createBufferList' pd dvc grp mng k (fromIntegral $ V.length idcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageIndexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit
	createBufferList pd dvc (fromIntegral $ V.length idcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
		\(b' :: Vk.Bffr.Binded sm sb "index-buffer" '[ VObj.List 256 t ""])
			(bm' :: Vk.Mm.M sm '[ '(
				sb,
				'Vk.Mm.BufferArg "index-buffer"
					'[ VObj.List 256 Word32 ""] ) ]) -> do
		Vk.Mm.write
			@"index-buffer" @(VObj.List 256 Word32 "") dvc bm' zeroBits idcs
		copyBuffer dvc gq cp b' b
	pure b

createUniformBuffer :: KnownNat alu => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
		Vk.Bffr.Binded sm sb "uniform-buffer" '[ VObj.Atom alu WModelViewProj 'Nothing]  ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg "uniform-buffer"
				'[ VObj.Atom alu WModelViewProj 'Nothing]) ] ->
		IO b) -> IO b
createUniformBuffer pd dvc = createBufferAtom pd dvc
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDescriptorPool dvc = Vk.DscPool.create dvc poolInfo nil
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

createDescriptorSet :: KnownNat alu =>
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> Vk.Bffr.Binded sm sb nm '[VObj.Atom alu WModelViewProj 'Nothing] ->
	Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv  -> Vk.Smplr.S ss ->
	Vk.DscSetLyt.D sdsc '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom alu WModelViewProj 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ] ->
	(forall sds . Vk.DscSet.D sds '(sdsc, '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom alu WModelViewProj 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ]) -> IO a) -> IO a
createDescriptorSet dvc dscp ub tximgvw txsmp dscslyt f =
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

descriptorWrite0 :: KnownNat alu =>
	Vk.Bffr.Binded sm sb nm '[VObj.Atom alu WModelViewProj 'Nothing] ->
	Vk.DscSet.D sds slbts ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, VObj.Atom alu WModelViewProj  'Nothing)]) 0
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

createBufferAtom :: forall alu sd nm a b . (KnownNat alu, Storable a) => Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ VObj.Atom alu a 'Nothing] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[ VObj.Atom alu a 'Nothing] )] ->
			IO b) -> IO b
createBufferAtom p dv usg props = createBuffer p dv VObj.LengthAtom usg props

createBufferList :: forall sd nm t a . Storable t =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.M.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ VObj.List 256 t ""] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[ VObj.List 256 t ""] ) ] ->
		IO a) ->
	IO a
createBufferList p dv ln usg props f =
	Vk.Bffr.group dv nil \grp -> Vk.Mm.group dv nil \mng ->
	uncurry f =<< createBufferList' p dv grp mng () ln usg props

createBufferList' :: forall sd nm t sm sb k . (Ord k, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[VObj.List 256 t ""] ->
	Vk.Mm.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mm.BufferArg nm '[VObj.List 256 t ""])] ->
	k -> Vk.Dvc.M.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[ VObj.List 256 t ""],
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[ VObj.List 256 t ""] ) ])
createBufferList' p dv grp mng k ln usg props =
	createBuffer' p dv grp mng k (VObj.LengthList ln) usg props

createBufferImage :: Storable (KObj.ImagePixel t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (Vk.Dvc.M.Size, Vk.Dvc.M.Size, Vk.Dvc.M.Size, Vk.Dvc.M.Size) ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ VObj.Image 1 t inm] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[ VObj.Image 1 t inm])] ->
		IO a) -> IO a
createBufferImage p dv (r, w, h, d) usg props =
	createBuffer p dv (VObj.LengthImage r w h d) usg props

createBuffer :: forall sd nm o a . VObj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer p dv ln usg props f =
	Vk.Bffr.group dv nil \grp -> Vk.Mm.group dv nil \mng ->
	uncurry f =<< createBuffer' p dv grp mng () ln usg props

createBuffer' :: forall sd nm o sm sb k . (Ord k, VObj.SizeAlignment o) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[o] ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] ->
	k -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[o],
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] )
createBuffer' p dv grp mng k ln usg props = do
	Right b <- Vk.Bffr.create' grp k bffrInfo
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mm.M.requirementsMemoryTypeBits reqs) props
	Right ((HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))), m) <- do
		Vk.Mm.allocateBind' mng k (HPList.Singleton . U2 $ Vk.Mm.Buffer b) (allcInfo mt)
	pure (bnd, m)
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
findMemoryType pd flt props =
	fromMaybe (error msg) . suitable <$> Vk.Phd.getMemoryProperties pd
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> find ((&&)
		<$> (`Vk.Mm.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mm.M.mTypePropertyFlags . snd) tps
		where tps = Vk.Phd.memoryPropertiesMemoryTypes props1

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a . Storable' a =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[ VObj.List 256 a ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ VObj.List 256 a ""] -> IO ()
copyBuffer dvc gq cp src dst = beginSingleTimeCommands dvc gq cp \cb ->
	Vk.Cmd.copyBuffer @'[ '[ VObj.List 256 a ""]] cb src dst

beginSingleTimeCommands :: forall sd sc a .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	(forall s . Vk.CBffr.C s -> IO a) -> IO a
beginSingleTimeCommands dvc gq cp cmd = do
	Vk.CBffr.allocate
		dvc allocInfo \((cb :: Vk.CBffr.C s) :*. HPList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
				Vk.submitInfoCommandBuffers = HPList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HPList.Nil }
		Vk.CBffr.begin @'Nothing @'Nothing cb beginInfo (cmd cb) <* do
			Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) Nothing
			Vk.Q.waitIdle gq
	where
	allocInfo :: Vk.CBffr.AllocateInfo 'Nothing sc '[ '()]
	allocInfo = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cp,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }
	beginInfo = Vk.CBffr.M.BeginInfo {
		Vk.CBffr.beginInfoNext = TMaybe.N,
		Vk.CBffr.beginInfoFlags = Vk.CBffr.UsageOneTimeSubmitBit,
		Vk.CBffr.beginInfoInheritanceInfo = Nothing }

createCommandBuffer ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb . Vk.CBffr.C scb -> IO a) ->
	IO a
createCommandBuffer dvc cp f = Vk.CBffr.allocate dvc allocInfo $ f . \(cb :*. HPList.Nil) -> cb
	where
	allocInfo :: Vk.CBffr.AllocateInfo 'Nothing scp '[ '()]
	allocInfo = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cp,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

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
	Vk.CBffr.C scb ->
	Vk.RndrPss.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.PplLyt.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl alu], '[]) ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[ VObj.List 256 WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ VObj.List 256 Word32 ""] ->
	Vk.DscSet.D sds (AtomUbo sdsl alu) ->
	IO ()
recordCommandBuffer cb rp fb sce ppllyt gpl idcs vb ib ubds =
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb
		(HPList.Singleton . U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @"" vb) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word32 @"" ib) >>
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics ppllyt
		(HPList.Singleton $ U2 ubds)
		(HPList.Singleton
			(HPList.Nil :** HPList.Nil :** HPList.Nil)) >>
--	Vk.Cmd.drawIndexed cbb (fromIntegral $ V.length idcs) 1 0 0 0
	Vk.Cmd.drawIndexed cbb (indexBufferLength ib) 1 0 0 0
	where
	rpInfo :: Vk.RndrPss.BeginInfo 'Nothing sr sf '[
		'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32,
		'Vk.ClearTypeDepthStencil ]
	rpInfo = Vk.RndrPss.BeginInfo {
		Vk.RndrPss.beginInfoNext = TMaybe.N,
		Vk.RndrPss.beginInfoRenderPass = rp,
		Vk.RndrPss.beginInfoFramebuffer = fb,
		Vk.RndrPss.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = sce },
		Vk.RndrPss.beginInfoClearValues =
			Vk.ClearValueColor (fromJust $ rgbaDouble 0 0 0 1) :**
			Vk.ClearValueDepthStencil (Vk.ClearDepthStencilValue 1 0) :**
			HPList.Nil }

type VertexIndexBuffer sm sb sm' sb' nm nm' = (
	Vk.Bffr.Binded sm sb nm '[ VObj.List 256 WVertex ""],
	Vk.Bffr.Binded sm' sb' nm' '[ VObj.List 256 Word32 ""])

type GroupAndManager sd sm sb k nm t = (
	Vk.Bffr.Group sd 'Nothing sb k nm '[VObj.List 256 t ""],
	Vk.Mm.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mm.BufferArg nm '[VObj.List 256 t ""])])

bindedBuffer ::
	Vk.Mm.M sm
		'[ '(sb, 'Vk.Mm.BufferArg nm '[VObj.List 256 t ""])] ->
	IO (Vk.Bffr.Binded sm sb nm '[VObj.List 256 t ""])
bindedBuffer m = do
	HPList.Singleton (U2 (Vk.Mm.BufferBinded b)) <- Vk.Mm.getBinded m
	pure b

mainLoop :: (
	RecreateFramebuffers ss sfs,
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt, KnownNat alu ) =>
	FramebufferResized ->
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) ss ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl alu], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPl.C sc ->
	ColorResources scfmt crnm crsi crsm crsiv ->
	(	Vk.Img.Binded sdm sdi "depth-buffer" dptfmt,
		Vk.Mm.M
			sdm '[ '(sdi, 'Vk.Mm.ImageArg "depth-buffer" dptfmt)],
		Vk.ImgVw.I "depth-buffer" dptfmt sdiv ) ->
	V.Vector Word32 ->
	GroupAndManager sd sm sb Glfw.Key nm WVertex ->
	GroupAndManager sd sm' sb' Glfw.Key nm' Word32 ->
	VertexIndexBuffer sm sb sm' sb' nm nm' ->
	Vk.Mm.M sm2 '[ '(
		sb2,
		'Vk.Mm.BufferArg "uniform-buffer"
			'[ VObj.Atom alu WModelViewProj 'Nothing] )] ->
	Vk.DscSet.D sds (AtomUbo sdsl alu) ->
	Vk.CBffr.C scb ->
	SyncObjects '(sias, srfs, siff) ->
	TChan K.KeyEvent ->
	K.Envs -> UTCTime -> IO ()
mainLoop g w@(GlfwG.Win.W win) sfc pd qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs cp clrrscs
	(dptImg, dptImgMem, dptImgVw) idcs0 grpmng grpmng' vbib0 ubm ubds cb iasrfsifs cke kenvs tm0 = do
	($ idcs0) . ($ vbib0) . ($ ext0) $ fix \loop ext vbib idcs -> do
		K.sendKeys win kenvs
		Glfw.pollEvents
		tm <- getCurrentTime
--		let	vbib = vbib0
		runLoop w sfc pd qfis dvc gq pq
			sc g ext scivs
			rp ppllyt gpl clrrscs dptImg dptImgMem dptImgVw cp fbs idcs cke grpmng grpmng' vbib ubm ubds cb iasrfsifs
			(realToFrac $ tm `diffUTCTime` tm0) loop
	Vk.Dvc.waitIdle dvc

runLoop :: (
	RecreateFramebuffers sis sfs,
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt, KnownNat alu ) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> FramebufferResized -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl alu], '[]) ->
	ColorResources scfmt crnm crsi crsm crsiv ->
	Vk.Img.Binded sdm sdi "depth-buffer" dptfmt ->
	Vk.Mm.M
		sdm '[ '(sdi, 'Vk.Mm.ImageArg "depth-buffer" dptfmt)] ->
	Vk.ImgVw.I "depth-buffer" dptfmt sdiv ->
	Vk.CmdPl.C sc ->
	HPList.PL Vk.Frmbffr.F sfs ->
	V.Vector Word32 ->
	TChan K.KeyEvent ->
	GroupAndManager sd sm sb Glfw.Key nm WVertex ->
	GroupAndManager sd sm' sb' Glfw.Key nm' Word32 ->
	VertexIndexBuffer sm sb sm' sb' nm nm' ->
	Vk.Mm.M sm2 '[ '(
		sb2,
		'Vk.Mm.BufferArg "uniform-buffer"
			'[ VObj.Atom alu WModelViewProj 'Nothing] )] ->
	Vk.DscSet.D sds (AtomUbo sdsl alu) ->
	Vk.CBffr.C scb ->
	SyncObjects '(sias, srfs, siff) -> Float ->
	(Vk.Extent2d -> VertexIndexBuffer sm sb sm' sb' nm nm' -> V.Vector Word32 -> IO ()) -> IO ()
runLoop w@(GlfwG.Win.W win) sfc pd qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl clrrscs
	dptImg dptImgMem dptImgVw cp fbs idcs cke (grp, mng) (grp', mng') vbib@(vb, ib) ubm ubds cb iasrfsifs tm loop = do
	me <- atomically do
		b <- isEmptyTChan cke
		if b then pure Nothing else Just <$> readTChan cke
	(vbib', idcs'') <- maybe (pure (vbib, idcs)) (\case
		K.First Glfw.Key'F -> loadModel "../../../../../files/models/monkey_smooth.obj" >>= \(vtcs, idcs') -> do
			vb' <- createVertexBuffer' pd dvc grp mng Glfw.Key'F gq cp vtcs
			ib' <- createIndexBuffer' pd dvc grp' mng' Glfw.Key'F gq cp idcs'
			print $ Vk.Bffr.lengthBinded ib'
			pure ((vb', ib'), idcs')
		K.Key k | K.isGf k -> do
			Just vm <- Vk.Mm.lookup mng k
			Just im <- Vk.Mm.lookup mng' k
			vb' <- bindedBuffer vm
			ib' <- bindedBuffer im
			pure ((vb', ib'), idcs)
		e -> do	print e
			pure (vbib, idcs)
		) me
	catchAndRecreate w sfc pd qfis dvc gq sc scivs
		rp ppllyt gpl clrrscs dptImg dptImgMem dptImgVw cp fbs vbib' idcs'' loop
		$ drawFrame dvc gq pq sc ext rp ppllyt gpl fbs idcs'' vb ib ubm ubds cb iasrfsifs tm
	cls <- Glfw.windowShouldClose win
	if cls then (pure ()) else checkFlag frszd >>= bool (loop ext vbib' idcs'')
		(do	ext' <- recreateSwapChainEtc
				w sfc pd qfis dvc gq sc scivs
				rp ppllyt gpl clrrscs dptImg dptImgMem dptImgVw cp fbs
			loop ext' vbib' idcs'')

indexBufferLength :: Vk.Bffr.Binded sm' sb' nm' '[ VObj.List 256 Word32 ""] -> Word32
indexBufferLength ib = let
	HPList.Singleton (VObj.LengthList' sz) = Vk.Bffr.lengthBinded ib
	in fromIntegral sz

drawFrame :: forall sfs sd ssc scfmt sr sl sg sm sb nm sm' sb' nm' sm2 sb2 scb sias srfs siff sdsl sds alu .
	KnownNat alu =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl alu], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[ VObj.List 256 WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[ VObj.List 256 Word32 ""] ->
	Vk.Mm.M sm2 '[ '(
		sb2,
		'Vk.Mm.BufferArg "uniform-buffer"
			'[ VObj.Atom alu WModelViewProj 'Nothing] )] ->
	Vk.DscSet.D sds (AtomUbo sdsl alu) ->
	Vk.CBffr.C scb -> SyncObjects '(sias, srfs, siff) -> Float -> IO ()
drawFrame dvc gq pq sc ext rp ppllyt gpl fbs idcs vb ib ubm ubds cb (SyncObjects ias rfs iff) tm = do
	let	siff = HPList.Singleton iff
	Vk.Fence.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CBffr.reset cb def
	HPList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext ppllyt gpl idcs vb ib ubds
	updateUniformBuffer dvc ubm ext tm
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

updateUniformBuffer :: forall sd sm sb alu . KnownNat alu => Vk.Dvc.D sd ->
	Vk.Mm.M sm '[ '(
		sb,
		'Vk.Mm.BufferArg "uniform-buffer"
			'[ VObj.Atom alu WModelViewProj 'Nothing] )] ->
	Vk.Extent2d -> Float -> IO ()
updateUniformBuffer dvc um sce tm = do
	Vk.Mm.write @"uniform-buffer"
		@(VObj.Atom alu WModelViewProj 'Nothing) dvc um zeroBits ubo
	where ubo = GStorable.W UniformBufferObject {
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

catchAndRecreate :: (
	RecreateFramebuffers sis sfs,
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl alu], '[]) ->
	ColorResources scfmt crnm crsi crsm crsiv ->
	Vk.Img.Binded sdm sdi "depth-buffer" dptfmt ->
	Vk.Mm.M
		sdm '[ '(sdi, 'Vk.Mm.ImageArg "depth-buffer" dptfmt)] ->
	Vk.ImgVw.I "depth-buffer" dptfmt sdiv ->
	Vk.CmdPl.C sc ->
	HPList.PL Vk.Frmbffr.F sfs ->
	VertexIndexBuffer sm sm' sb sb' nm nm' -> V.Vector Word32 ->
	(Vk.Extent2d -> VertexIndexBuffer sm sm' sb sb' nm nm' -> V.Vector Word32 -> IO ()) -> IO () -> IO ()
catchAndRecreate w sfc pd qfis dvc gq sc scivs rp ppllyt gpl clrrscs dptImg dptImgMem dptImgVw cp fbs vbib idcs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> do
		ext' <- recreateSwapChainEtc
			w sfc pd qfis dvc gq sc scivs rp ppllyt gpl clrrscs dptImg dptImgMem dptImgVw cp fbs
		loop ext' vbib idcs

recreateSwapChainEtc :: (
	RecreateFramebuffers sis sfs,
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt ) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl alu], '[]) ->
	ColorResources scfmt crnm crsi crsm crsiv ->
	Vk.Img.Binded sdm sdi "depth-buffer" dptfmt ->
	Vk.Mm.M
		sdm '[ '(sdi, 'Vk.Mm.ImageArg "depth-buffer" dptfmt)] ->
	Vk.ImgVw.I "depth-buffer" dptfmt sdiv ->
	Vk.CmdPl.C sc ->
	HPList.PL Vk.Frmbffr.F sfs ->
	IO Vk.Extent2d
recreateSwapChainEtc w@(GlfwG.Win.W win) sfc pd qfis dvc gq sc scivs rp ppllyt gpl
	(clrimg, clrimgm, clrimgvw, mss) dptImg dptImgMem dptImgVw cp fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwpch w sfc pd qfis dvc sc
	ext <$ do
		Vk.Khr.Swpch.getImages dvc sc >>= \imgs ->
			recreateImgVws dvc imgs scivs
		recreateClrRsrcs pd dvc ext (clrimg, clrimgm, clrimgvw, mss)
		recreateDptRsrcs pd dvc gq cp ext mss (dptImg, dptImgMem, dptImgVw)
		recreateGrPpl dvc ext rp ppllyt mss gpl
		recreateFrmbffrs dvc ext rp scivs dptImgVw clrimgvw fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize win = Glfw.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

type WModelViewProj = GStorable.W UniformBufferObject

data UniformBufferObject = UniformBufferObject {
	uniformBufferObjectModel :: Cglm.Mat4,
	uniformBufferObjectView :: Cglm.Mat4,
	uniformBufferObjectProj :: Cglm.Mat4 }
	deriving (Show, Generic)

instance GStorable.G UniformBufferObject

[glslVertexShader|

#version 450

layout(binding = 0) uniform UniformBufferObject {
	mat4 model;
	mat4 view;
	mat4 proj;
} ubo;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inColor;
layout(location = 2) in vec2 inTexCoord;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void
main()
{
	gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 1.0);
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
