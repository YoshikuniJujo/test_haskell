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
import GHC.TypeLits
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.Foldable
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List qualified as TLength
import Data.TypeLevel.List qualified as TpLvlLst
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Proxy
import Data.Bool
import Data.Maybe
import Data.List
import Data.IORef
import Data.List.Length
import Data.Word
import Data.Color

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS
import qualified Data.Text.IO as Txt
import qualified Graphics.UI.GLFW as Glfw hiding (createWindowSurface)
import qualified Gpu.Vulkan.Cglm as Cglm
import qualified Foreign.Storable.Generic as GStorable

import qualified Language.SpirV as SpirV
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
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Sfc
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as Vk.Khr.Sfc.Phd
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swpch
import qualified Gpu.Vulkan.Image as Vk.Img
import qualified Gpu.Vulkan.Image as Vk.Img.M
import qualified Gpu.Vulkan.ImageView as Vk.ImgVw
import qualified Gpu.Vulkan.Component as Vk.Component
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderModule
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt
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
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBffr
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBffr.M
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Mem.M
import qualified Gpu.Vulkan.Memory as Vk.Mem
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.Cmd as Vk.Cmd
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as Vk.Ppl.DptStnSt
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet

import qualified Codec.WavefrontObj.ReadFaceSimple as WNew

import Control.Monad.Trans
import Data.Ord.ToolsYj
import Data.Bits.ToolsYj
import Data.Function.ToolsYj
import Data.Tuple.ToolsYj
import Data.Bool.ToolsYj
import Data.Maybe.ToolsYj
import Data.List.ToolsYj
import Data.IORef.ToolsYj
import Data.TypeLevel.List qualified as TList
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.List.Infinite qualified as Inf
import Data.List.Infinite (pattern (:~))
import Options.Declarative (Flag, Def, Cmd, run_, get)

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win

import Debug

main :: IO ()
main = run_ realMain

realMain ::
	Flag "m" '["model"] "FILEPATH" "model filepath"
		(Def "../../../../../files/models/viking_room.obj" String) ->
	Cmd "Try Vulkan Guide" ()
realMain mdlfp = liftIO $ newIORef False >>= \fr -> withWindow fr \w ->
	createIst \ist -> bool id (dbgm ist) debug
		$ body (get mdlfp) fr w ist
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

body :: FilePath ->
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Ist.I si -> IO ()
body mdlfp fr w ist =
	Vk.Khr.Sfc.Glfw.Win.create ist w nil \sfc ->
	pickPhd ist sfc >>= \(pd, qfis) ->
	createLgDvc pd qfis \d gq pq ->
	createCmdPl qfis d \cp ->
	createSwpch w sfc pd qfis d \(sc :: Vk.Khr.Swpch.S scifmt ss) ex ->
	Vk.Khr.Swpch.getImages d sc >>= \scis -> createImgVws d scis \scvs ->
	findDepthFormat pd >>= \dptfmt ->
	Vk.T.formatToType dptfmt \(_ :: Proxy dptfmt) ->
	createDescriptorSetLayout d \cmdslyt ->
	createRenderPass @scifmt @dptfmt d \rp ->
	putStrLn "MIN ALIGN" >>
	(print . Vk.Phd.limitsMinUniformBufferOffsetAlignment . Vk.Phd.propertiesLimits =<< Vk.Phd.getProperties pd) >>
	createPipelineLayout d cmdslyt \ppllyt ->
	createGraphicsPipeline d ex rp ppllyt 0 \gpl0 ->
	createGraphicsPipeline d ex rp ppllyt 1 \gpl1 ->
	createDepthResources pd d gq cp ex \dptImg dptImgMem dptImgVw ->
	createFramebuffers d ex rp scvs dptImgVw \fbs ->
	createCameraBuffersNew @'["scene-data-0", "scene-data-1"] pd d cmdslyt \_ cmlyts cmbs cmms ->
	createSceneBuffer pd d \scnb scnm ->
	BS.readFile mdlfp >>= \obj ->
	let	evns = V.map positionNormalToVertex
			<$> WNew.posNormal (WNew.r obj) in
	either error pure evns >>= \vns ->
	createVertexBuffer pd d gq cp vns \vb ->
	createVertexBuffer pd d gq cp triangle \vbtri ->
	tnum maxFramesInFlight \(_ :: Proxy mff) ->
	createCommandBuffers d cp \cbs ->
	createSyncObjects d \sos ->
	createDescriptorPool d \cmdp ->
	createDescriptorSetsNew d cmdp cmbs cmlyts scnb \cmds ->
	mainLoop fr w sfc pd qfis d gq pq sc ex scvs rp ppllyt
		gpl0 gpl1 cp (dptImg, dptImgMem, dptImgVw) fbs vb vbtri cbs sos cmms scnm cmds (fromIntegral $ V.length vns)
	where
	tnum :: Int -> (forall (n :: [()]) . (
		TList.Length n, HPList.FromList n,
		HPList.HomoList '() n, HPList.RepM n ) => Proxy n -> a) -> a
	tnum 0 f = f (Proxy @'[])
	tnum n f = tnum (n - 1) \p -> f $ plus1 p
		where plus1 :: Proxy n -> Proxy ('() ': n); plus1 Proxy = Proxy

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
	Vk.ImgVw.create dv (imgVwInfo i Vk.Img.AspectColorBit) nil \v ->
	createImgVws dv is \vs -> f $ v :** vs

recreateImgVws :: Vk.T.FormatToValue fmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss inm fmt] ->
	HPList.PL (Vk.ImgVw.I inm fmt) sis -> IO ()
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

recreateImageViews :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss nm scfmt] -> HPList.PL (Vk.ImgVw.I nm scfmt) sis -> IO ()
recreateImageViews _dvc [] HPList.Nil = pure ()
recreateImageViews dvc (sci : scis) (iv :** ivs) =
	Vk.ImgVw.unsafeRecreate dvc (mkImageViewCreateInfo sci Vk.Img.AspectColorBit) nil iv >>
	recreateImageViews dvc scis ivs
recreateImageViews _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

createImageView :: forall ivfmt sd si sm nm ifmt a .
	Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	Vk.Img.AspectFlags ->
	(forall siv . Vk.ImgVw.I nm ivfmt siv -> IO a) -> IO a
createImageView dvc timg asps f =
	Vk.ImgVw.create dvc (mkImageViewCreateInfo timg asps) nil f

recreateImageView :: Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	Vk.Img.AspectFlags ->
	Vk.ImgVw.I nm ivfmt s -> IO ()
recreateImageView dvc timg asps iv =
	Vk.ImgVw.unsafeRecreate dvc (mkImageViewCreateInfo timg asps) nil iv

mkImageViewCreateInfo ::
	Vk.Img.Binded sm si nm ifmt -> Vk.Img.AspectFlags ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt ivfmt
mkImageViewCreateInfo sci asps = Vk.ImgVw.CreateInfo {
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
		Vk.Img.M.subresourceRangeLevelCount = 1,
		Vk.Img.M.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.M.subresourceRangeLayerCount = 1 }

createRenderPass ::
	forall (scifmt :: Vk.T.Format) (dptfmt :: Vk.T.Format) sd a . (
	Vk.T.FormatToValue scifmt, Vk.T.FormatToValue dptfmt ) =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPass.R sr -> IO a) -> IO a
createRenderPass dvc f = do
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
				Vk.Img.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Img.LayoutPresentSrcKhr }
		colorAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 0,
			Vk.Att.referenceLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
		depthAttachment :: Vk.Att.Description dptfmt
		depthAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = zeroBits,
			Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
			Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
			Vk.Att.descriptionStoreOp = Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionStencilLoadOp =
				Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOp =
				Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayout =
				Vk.Img.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Img.LayoutDepthStencilAttachmentOptimal }
		depthAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 1,
			Vk.Att.referenceLayout =
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
			Vk.RndrPass.M.createInfoAttachments =
				colorAttachment :** depthAttachment :** HPList.Nil,
			Vk.RndrPass.M.createInfoSubpasses = [subpass],
			Vk.RndrPass.M.createInfoDependencies = [dependency] }
	Vk.RndrPass.create @'Nothing @'[scifmt, dptfmt] dvc renderPassInfo nil \rp -> f rp

createPipelineLayout :: forall sd s b .
	Vk.Dvc.D sd ->
	Vk.DscSetLyt.D s '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer '[
			VObj.Atom 256 GpuSceneData0 'Nothing ] ] -> (forall sl .
		Vk.Ppl.Layout.P sl
			'[ '(s, '[
				'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
				'Vk.DscSetLyt.Buffer '[
					VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
			'[WrapMeshPushConstants] ->
		IO b) -> IO b
createPipelineLayout dvc cmdslyt f = Vk.Ppl.Layout.create dvc crInfo nil f
	where
	crInfo :: Vk.Ppl.Layout.CreateInfo 'Nothing
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing] ]) ]
		(
		'Vk.PushConstant.Layout
			'[ WrapMeshPushConstants]
			'[ 'Vk.PushConstant.Range
				'[ 'Vk.T.ShaderStageVertexBit] '[WrapMeshPushConstants] ])
	crInfo = Vk.Ppl.Layout.CreateInfo {
		Vk.Ppl.Layout.createInfoNext = TMaybe.N,
		Vk.Ppl.Layout.createInfoFlags = zeroBits,
		Vk.Ppl.Layout.createInfoSetLayouts = HPList.Singleton $ U2 cmdslyt }

createGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.P sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	Int ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) -> IO a) -> IO a
createGraphicsPipeline dvc sce rp ppllyt sdrn f =
	Vk.Ppl.Graphics.createGs dvc Nothing (HPList.Singleton $ U14 pplInfo) nil
		\(HPList.Singleton (U3 gpl)) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo sce rp ppllyt sdrn

type Foo s = '[ '(s, '[
	'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
	'Vk.DscSetLyt.Buffer '[
		VObj.Atom 256 GpuSceneData0 'Nothing ] ])]

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.P sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] -> Int ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) -> IO ()
recreateGraphicsPipeline dvc sce rp ppllyt sdrn gpls = Vk.Ppl.Graphics.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HPList.Nil) nil (U3 gpls :** HPList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo sce rp ppllyt sdrn

mkGraphicsPipelineCreateInfo ::
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.P sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] -> Int ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing, '[ '(Vertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Position), '(1, Normal), '(2, Color)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'(sl,	'[ '(s, '[
				'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
				'Vk.DscSetLyt.Buffer '[
					VObj.Atom 256 GpuSceneData0 'Nothing ] ])],
			'[WrapMeshPushConstants])
		sr '(sb, vs', ts', sbtss')
mkGraphicsPipelineCreateInfo sce rp ppllyt sdrn = Vk.Ppl.Graphics.CreateInfo {
	Vk.Ppl.Graphics.createInfoNext = TMaybe.N,
	Vk.Ppl.Graphics.createInfoFlags = Vk.Ppl.CreateFlagsZero,
	Vk.Ppl.Graphics.createInfoStages = uncurry shaderStages
		case sdrn `mod` 2 of
			0 -> shaderPair0
			1 -> shaderPair1
			_ -> error "never occur",
	Vk.Ppl.Graphics.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Graphics.createInfoInputAssemblyState = Just inputAssembly,
	Vk.Ppl.Graphics.createInfoViewportState = Just $ mkViewportState sce,
	Vk.Ppl.Graphics.createInfoRasterizationState = Just rasterizer,
	Vk.Ppl.Graphics.createInfoMultisampleState = Just multisampling,
	Vk.Ppl.Graphics.createInfoDepthStencilState = Just depthStencil,
	Vk.Ppl.Graphics.createInfoColorBlendState = Just colorBlending,
	Vk.Ppl.Graphics.createInfoDynamicState = Nothing,
	Vk.Ppl.Graphics.createInfoLayout = U3 ppllyt,
	Vk.Ppl.Graphics.createInfoRenderPass = rp,
	Vk.Ppl.Graphics.createInfoSubpass = 0,
	Vk.Ppl.Graphics.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Graphics.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Graphics.createInfoTessellationState = Nothing }
	where depthStencil = Vk.Ppl.DptStnSt.CreateInfo {
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

inputAssembly :: Vk.Ppl.InpAsmbSt.CreateInfo 'Nothing
inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
	Vk.Ppl.InpAsmbSt.createInfoNext = TMaybe.N,
	Vk.Ppl.InpAsmbSt.createInfoFlags = zeroBits,
	Vk.Ppl.InpAsmbSt.createInfoTopology = Vk.PrimitiveTopologyTriangleList,
	Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable = False }

mkViewportState :: Vk.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo 'Nothing
mkViewportState sce = def {
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
	Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeNone, -- Vk.CullModeBackBit,
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

createDepthResources ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Extent2d ->
	(forall si sm fmt siv . Vk.T.FormatToValue fmt =>
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.ImageArg nm fmt) ] ->
		Vk.ImgVw.I nm fmt siv ->
		IO a) -> IO a
createDepthResources phdvc dvc gq cp ext f = do
	fmt <- findDepthFormat phdvc
	print fmt
	print ext
	Vk.T.formatToType fmt \(_ :: Proxy fmt) ->
		createImage @_ @fmt phdvc dvc
			(Vk.extent2dWidth ext) (Vk.extent2dHeight ext)
			Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
			Vk.Mem.PropertyDeviceLocalBit \dptImg dptImgMem ->
		createImageView @fmt dvc dptImg Vk.Img.AspectDepthBit \dptImgVw -> do
			transitionImageLayout dvc gq cp dptImg Vk.Img.LayoutUndefined
				Vk.Img.LayoutDepthStencilAttachmentOptimal
			f dptImg dptImgMem dptImgVw

recreateDepthResources :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Extent2d ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.ImageArg nm fmt)] ->
	Vk.ImgVw.I nm fmt sdiv -> IO ()
recreateDepthResources phdvc dvc gq cp ext dptImg dptImgMem dptImgVw = do
	print ext
	recreateImage phdvc dvc
		(Vk.extent2dWidth ext) (Vk.extent2dHeight ext)
		Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mem.PropertyDeviceLocalBit dptImg dptImgMem
	recreateImageView dvc dptImg Vk.Img.AspectDepthBit dptImgVw
	transitionImageLayout dvc gq cp dptImg Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal

type DepthResources sb sm nm fmt sdiv = (
	Vk.Img.Binded sm sb nm fmt,
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.ImageArg nm fmt)],
	Vk.ImgVw.I nm fmt sdiv )

findDepthFormat :: Vk.Phd.P -> IO Vk.Format
findDepthFormat phdvc = findSupportedFormat phdvc
	[Vk.FormatD32Sfloat, Vk.FormatD32SfloatS8Uint, Vk.FormatD24UnormS8Uint]
	Vk.Img.TilingOptimal
	Vk.FormatFeatureDepthStencilAttachmentBit

findSupportedFormat ::
	Vk.Phd.P -> [Vk.Format] -> Vk.Img.Tiling -> Vk.FormatFeatureFlags -> IO Vk.Format
findSupportedFormat phdvc fs tlng fffs = do
	props <- Vk.Phd.getFormatProperties phdvc `mapM` fs
	case tlng of
		Vk.Img.TilingLinear -> do
			putStrLn "LINEAR"
			pure . orError . find (checkFeatures fffs . snd) . zip fs
				$ Vk.formatPropertiesLinearTilingFeatures <$> props
		Vk.Img.TilingOptimal -> do
			putStrLn "OPTIMAL"
			pure . orError . find (checkFeatures fffs . snd) . zip fs
				$ Vk.formatPropertiesOptimalTilingFeatures <$> props
		_ -> error "no such image tiling"
	where orError = \case
		Just (x, _) -> x
		Nothing -> error "failed to find supported format!"

checkFeatures :: Vk.FormatFeatureFlags -> Vk.FormatFeatureFlags -> Bool
checkFeatures wntd ftrs = wntd .&. ftrs == wntd

createImage :: forall nm fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P ->
	Vk.Dvc.D sd -> Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mem.PropertyFlagBits -> (forall si sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.ImageArg nm fmt) ] ->
		IO a) -> IO a
createImage pd dvc wdt hgt tlng usg prps f = Vk.Img.create @'Nothing dvc
		(imageInfo wdt hgt tlng usg) nil \img -> do
	memInfo <- imageMemoryInfo pd dvc prps img
	imageAllocateBind dvc img memInfo f

recreateImage :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mem.PropertyFlags ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.ImageArg nm fmt)] -> IO ()
recreateImage pd dvc wdt hgt tlng usg prps img mem = do
	Vk.Img.unsafeRecreate @'Nothing dvc
		(imageInfo wdt hgt tlng usg) nil img
	memInfo <- imageMemoryInfoBinded pd dvc prps img
	imageReallocateBind dvc img memInfo mem

imageInfo ::
	Word32 -> Word32 -> Vk.Img.Tiling -> Vk.Img.UsageFlags ->
	Vk.Img.CreateInfo 'Nothing fmt
imageInfo wdt hgt tlng usg = Vk.Img.CreateInfo {
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

imageAllocateBind :: Vk.Dvc.D sd -> Vk.Img.I si nm fmt ->
	Vk.Mem.AllocateInfo 'Nothing -> (forall sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.ImageArg nm fmt) ] ->
		IO a) -> IO a
imageAllocateBind dvc img memInfo f =
	Vk.Mem.allocateBind @'Nothing dvc
		(HPList.Singleton . U2 $ Vk.Mem.Image img) memInfo
		nil \(HPList.Singleton (U2 (Vk.Mem.ImageBinded bnd))) m -> do
		f bnd m

imageReallocateBind ::
	Vk.Dvc.D sd -> Vk.Img.Binded sm sb nm fmt ->
	Vk.Mem.AllocateInfo 'Nothing ->
	Vk.Mem.M sm '[ '(sb, 'Vk.Mem.ImageArg nm fmt)] -> IO ()
imageReallocateBind dvc img memInfo m =
	Vk.Mem.unsafeReallocateBind @'Nothing dvc
		(HPList.Singleton . U2 $ Vk.Mem.ImageBinded img) memInfo
		nil m

imageMemoryInfo ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Mem.PropertyFlags ->
	Vk.Img.I s nm fmt -> IO (Vk.Mem.AllocateInfo 'Nothing)
imageMemoryInfo pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirements dvc img
	mt <- findMemoryType pd (Vk.Mem.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

imageMemoryInfoBinded ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Mem.PropertyFlags ->
	Vk.Img.Binded sm si nm fmt -> IO (Vk.Mem.AllocateInfo 'Nothing)
imageMemoryInfoBinded pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirementsBinded dvc img
	mt <- findMemoryType pd (Vk.Mem.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

transitionImageLayout :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
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
			Vk.Img.subresourceRangeAspectMask = asps,
			Vk.Img.subresourceRangeBaseMipLevel = 0,
			Vk.Img.subresourceRangeLevelCount = 1,
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

createFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
	Vk.RndrPass.R sr -> HPList.PL (Vk.ImgVw.I nm fmt) sis ->
	Vk.ImgVw.I dptfmt dptnm siv ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HPList.Nil _ f = f HPList.Nil
createFramebuffers dvc sce rp (iv :** ivs) dptiv f =
	Vk.Frmbffr.create dvc (mkFramebufferCreateInfo sce rp iv dptiv) nil \fb ->
	createFramebuffers dvc sce rp ivs dptiv \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPass.R sr -> HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
		Vk.ImgVw.I dptfmt dptnm sdiv ->
		HPList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HPList.Nil _ HPList.Nil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dvc sce rp (sciv :** scivs) dptiv (fb :** fbs) =
		Vk.Frmbffr.unsafeRecreate dvc
			(mkFramebufferCreateInfo sce rp sciv dptiv) nil fb >>
		recreateFramebuffers dvc sce rp scivs dptiv fbs

mkFramebufferCreateInfo ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.ImgVw.I nm fmt si ->
	Vk.ImgVw.I dptfmt dptnm sdiv ->
	Vk.Frmbffr.CreateInfo 'Nothing sr
		'[ '(nm, fmt, si), '(dptfmt, dptnm, sdiv)]
mkFramebufferCreateInfo sce rp attch dpt = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = U3 attch :** U3 dpt :** HPList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where
	Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = sce

createCmdPl :: QFamIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfis dv = Vk.CmdPl.create dv info nil
	where info = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = grFam qfis }

createVertexBuffer :: forall sd sc vbnm a . Vk.Phd.P ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> V.Vector Vertex ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb vbnm '[VObj.List 256 Vertex ""] -> IO a ) -> IO a
createVertexBuffer phdvc dvc gq cp vtcs f =
	createBuffer phdvc dvc (HPList.Singleton . VObj.LengthList . fromIntegral $ V.length vtcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBuffer phdvc dvc (HPList.Singleton . VObj.LengthList . fromIntegral $ V.length vtcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
			\b' (bm' :: Vk.Mem.M sm '[
				'(sb, 'Vk.Mem.BufferArg vbnm '[VObj.List 256 Vertex ""])
				]) -> do
	Vk.Mem.write @vbnm @(VObj.List 256 Vertex "") dvc bm' zeroBits vtcs
	copyBuffer dvc gq cp b' b
	f b

class CreateCameraBuffersNew (sds :: [Symbol]) where
	createCameraBuffersNew ::
		snb ~ '[
			VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
			VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ] =>
		Vk.Phd.P -> Vk.Dvc.D sd ->
		Vk.DscSetLyt.D sdsc '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ] ->
--		Int ->
--		(forall slyts sbsms sds . (
		(forall slyts sbsms . (
			Vk.DscSet.DListFromMiddle slyts,
			HPList.FromList slyts,
			UpdateNew snb sbsms slyts sds,
			HPList.HomoList '(sdsc, '[
				'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
				'Vk.DscSetLyt.Buffer '[
					VObj.Atom 256 GpuSceneData0 'Nothing ] ]) slyts ) =>
			Proxy sds ->
			HPList.PL (U2 Vk.DscSetLyt.D) slyts ->
			HPList.PL BindedGcd sbsms ->
			HPList.PL MemoryGcd sbsms -> IO a) -> IO a

instance CreateCameraBuffersNew '[] where
	createCameraBuffersNew _ _ _ f = f @_ @_ Proxy HPList.Nil HPList.Nil HPList.Nil

instance (
	snb ~ '[
		VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
		VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ],
	VObj.OffsetRange (VObj.Atom 256 GpuSceneData0 (Just sd)) snb,
	CreateCameraBuffersNew sds ) =>
	CreateCameraBuffersNew (sd ': sds) where
	createCameraBuffersNew phdvc dvc lyt f = createCameraBuffer phdvc dvc \bnd mem ->
		createCameraBuffersNew @sds phdvc dvc lyt \(Proxy :: Proxy sds) lyts bnds mems ->
		f (Proxy :: Proxy (sd ': sds)) (U2 lyt :** lyts) (BindedGcd bnd :** bnds) (MemoryGcd mem :** mems)
--		f (Proxy :: Proxy ("scene-data-0" ': sds)) (U2 lyt :** lyts) (BindedGcd bnd :** bnds) (MemoryGcd mem :** mems)

createCameraBuffer :: Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 GpuCameraData 'Nothing] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm
				'[VObj.Atom 256 GpuCameraData 'Nothing]) ] ->
		IO a) -> IO a
createCameraBuffer phdvc dvc = createBuffer phdvc dvc (HPList.Singleton VObj.LengthAtom)
	Vk.Bffr.UsageUniformBufferBit Vk.Mem.PropertyHostVisibleBit

createSceneBuffer :: Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[
			VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
			VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ] ) ] ->
		IO a) -> IO a
createSceneBuffer phdvc dvc = createBuffer2 phdvc dvc
	(VObj.LengthAtom :** VObj.LengthAtom :** HPList.Nil)
	Vk.Bffr.UsageUniformBufferBit Vk.Mem.PropertyHostVisibleBit

createBuffer :: forall obj nm sd a . (
	VObj.SizeAlignmentList '[obj],
	VObj.SizeAlignment obj
--	Vk.Mem.Alignments '[
--		'(s, 'Vk.Mem.BufferArg nm objs) ]
	) => -- VObj.SizeAlignment obj =>
	Vk.Phd.P -> Vk.Dvc.D sd -> HPList.PL VObj.Length '[obj] ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[obj] ->
		Vk.Mem.M sm '[
			'(sb, 'Vk.Mem.BufferArg nm '[obj])
			] -> IO a ) -> IO a
createBuffer p dv lns usg props f = Vk.Bffr.create dv bffrInfo nil
		\b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Mem.allocateBind dv
		(HPList.Singleton . U2 $ Vk.Mem.Buffer b) (allcInfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mem.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[obj]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = lns,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.M.TypeIndex -> Vk.Mem.AllocateInfo 'Nothing
	allcInfo mt = Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

createBuffer2 :: forall obj obj2 nm sd a . (
	VObj.SizeAlignmentList '[obj, obj2],
	VObj.SizeAlignment obj, VObj.SizeAlignment obj2
--	Vk.Mem.Alignments '[
--		'(s, 'Vk.Mem.BufferArg nm objs) ]
	) => -- VObj.SizeAlignment obj =>
	Vk.Phd.P -> Vk.Dvc.D sd -> HPList.PL VObj.Length '[obj, obj2] ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[obj, obj2] ->
		Vk.Mem.M sm '[
			'(sb, 'Vk.Mem.BufferArg nm '[obj, obj2])
			] -> IO a ) -> IO a
createBuffer2 p dv lns usg props f = Vk.Bffr.create dv bffrInfo nil
		\b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Mem.allocateBind dv
		(HPList.Singleton . U2 $ Vk.Mem.Buffer b) (allcInfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mem.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[obj, obj2]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = lns,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.M.TypeIndex -> Vk.Mem.AllocateInfo 'Nothing
	allcInfo mt = Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

findMemoryType :: Vk.Phd.P -> Vk.Mem.M.TypeBits -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.M.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.Phd.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> find ((&&)
		<$> (`Vk.Mem.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mem.M.mTypePropertyFlags . snd) tps
		where tps = Vk.Phd.memoryPropertiesMemoryTypes props1

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscSetLyt.D s '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer
			'[VObj.Atom 256 GpuSceneData0 'Nothing]
		] -> IO a) ->
	IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer
			'[VObj.Atom 256 GpuSceneData0 'Nothing] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings =
			camBufferBinding :** sceneBind :** HPList.Nil }
	camBufferBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing])
	camBufferBinding = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	sceneBind :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuSceneData0 'Nothing])
	sceneBind = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags =
			Vk.ShaderStageVertexBit .|.
			Vk.ShaderStageFragmentBit }

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDescriptorPool dvc = Vk.DscPool.create dvc poolInfo nil
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = TMaybe.N,
		Vk.DscPool.createInfoFlags =
			Vk.DscPool.CreateFreeDescriptorSetBit,
		Vk.DscPool.createInfoMaxSets = 10,
		Vk.DscPool.createInfoPoolSizes = [poolSize0] }
	poolSize0 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

createDescriptorSetsNew :: forall snb ss sd sp smsbs sm sb a . (
	snb ~ '[
		VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
		VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ],
	Vk.DscSet.DListFromMiddle ss,
	HPList.FromList ss,
	UpdateNew snb smsbs ss '["scene-data-0", "scene-data-1"] ) =>
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> HPList.PL BindedGcd smsbs ->
	HPList.PL (U2 Vk.DscSetLyt.D) ss ->
	Vk.Bffr.Binded sm sb "scene-buffer" snb ->
	(forall sds . HPList.PL (Vk.DscSet.D sds) ss -> IO a) -> IO a
createDescriptorSetsNew dvc dscp ubs dscslyts scnb f =
	Vk.DscSet.allocateDs dvc allocInfo \dscss -> do
	updateNew @snb @_ @_ @'["scene-data-0", "scene-data-1"] dvc ubs dscss scnb
	f dscss
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = dscslyts }

class UpdateNew snb smsbs slbtss (sns :: [Symbol]) where
	updateNew ::
		Vk.Dvc.D sd ->
		HPList.PL BindedGcd smsbs ->
		HPList.PL (Vk.DscSet.D sds) slbtss ->
		Vk.Bffr.Binded sm sb "scene-buffer" snb -> IO ()

instance UpdateNew _snb '[] '[] '[] where
	updateNew _ HPList.Nil HPList.Nil _ = pure ()

instance (
	Vk.DscSet.BindingAndArrayElemBuffer cs '[VObj.Atom 256 GpuCameraData 'Nothing] 0,
	Vk.DscSet.BindingAndArrayElemBuffer cs '[VObj.Atom 256 GpuSceneData0 ('Just sn)] 0,
	Vk.DscSet.UpdateDynamicLength cs '[VObj.Atom 256 GpuCameraData 'Nothing],
	Vk.DscSet.UpdateDynamicLength cs '[VObj.Atom 256 GpuSceneData0 ('Just sn)],
	UpdateNew snb ubs dscss sns,
	VObj.OffsetRange (VObj.Atom 256 GpuSceneData0 (Just sn)) snb,

	Show (HPList.PL VObj.Length snb)
	) =>
	UpdateNew snb (ub ': ubs) ('(ds, cs) ': dscss) (sn ': sns) where
	updateNew dvc (BindedGcd ub :** ubs) (dscs :** dscss) scnb = do
		Vk.DscSet.updateDs dvc (
			U5 (descriptorWrite0 @GpuCameraData @Nothing ub dscs Vk.Dsc.TypeUniformBuffer) :**
			U5 (descriptorWrite0 @GpuSceneData0 @('Just sn) scnb dscs Vk.Dsc.TypeUniformBuffer) :**
			HPList.Nil ) HPList.Nil
		updateNew @snb @_ @_ @sns dvc ubs dscss scnb

descriptorWrite0 :: forall tp objnm objs sm sb nm slbts sds . (
	Show (HPList.PL VObj.Length objs),
	VObj.OffsetRange (VObj.Atom 256 tp objnm) objs ) =>
	Vk.Bffr.Binded sm sb nm objs ->
	Vk.DscSet.D sds slbts -> Vk.Dsc.Type ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, VObj.Atom 256 tp objnm )]) 0
descriptorWrite0 ub dscs tp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = tp,
	Vk.DscSet.writeSources= Vk.DscSet.BufferInfos $
		HPList.Singleton bufferInfo }
	where bufferInfo = U4 $ Vk.Dsc.BufferInfo ub

data BindedGcd smsb where
	BindedGcd ::
		Vk.Bffr.Binded sm sb "camera-buffer" '[VObj.Atom 256 GpuCameraData 'Nothing] ->
		BindedGcd '(sm, sb)

data MemoryGcd smsb where
	MemoryGcd ::
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg "camera-buffer"
				'[VObj.Atom 256 GpuCameraData 'Nothing] )] ->
		MemoryGcd '(sm, sb)

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Vertex ""] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CmdBffr.allocate
		dvc allocInfo \((cb :: Vk.CmdBffr.C s) :*. HPList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
				Vk.submitInfoCommandBuffers = HPList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HPList.Nil }
		Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo do
			Vk.Cmd.copyBuffer @'[ '[VObj.List 256 Vertex ""]] cb src dst
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

createCommandBuffers ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb vss . (
		TLength.Length vss, HPList.HomoList '() vss ) =>
		HPList.LL (Vk.CmdBffr.C scb) (vss :: [()]) -> IO a) ->
	IO a
createCommandBuffers dvc cp f = mkVss maxFramesInFlight \(_p :: Proxy vss1) ->
	Vk.CmdBffr.allocate @_ @vss1 dvc (allcInfo @vss1) (f @_ @vss1)
	where
	allcInfo :: forall vss . Vk.CmdBffr.AllocateInfo 'Nothing scp vss
	allcInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }

mkVss :: Int -> (forall (vss :: [()]) . (
	TpLvlLst.Length vss, TLength.Length vss,
	HPList.FromList vss, HPList.HomoList '() vss ) =>
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

createSyncObjects ::
	Vk.Dvc.D sd -> (forall ssos . SyncObjects ssos -> IO a ) -> IO a
createSyncObjects dvc f =
	HPList.replicateM maxFramesInFlight
		(Vk.Semaphore.create @'Nothing dvc def nil) \iass ->
	HPList.replicateM maxFramesInFlight
		(Vk.Semaphore.create @'Nothing dvc def nil) \rfss ->
	HPList.replicateM maxFramesInFlight
		(Vk.Fence.create @'Nothing dvc fncInfo nil) \iffs ->
	f $ SyncObjects iass rfss iffs
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sg slyt sdlyt sm sb nm smtri sbtri nmtri sds .
	Vk.CmdBffr.C scb ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt, Foo sdlyt, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Layout.P slyt
		'[ '(sdlyt, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[VObj.List 256 Vertex ""] -> Int ->
	Vk.DscSet.D sds '(sdlyt, '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer '[
			VObj.Atom 256 GpuSceneData0 'Nothing ] ]) ->
	Word32 -> IO ()
recordCommandBuffer cb rp fb sce gpl lyt vb vbtri fn cmd vn =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb cbInfo $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	newIORef Nothing >>= \om ->
	drawObject om cb cmd RenderObject {
		renderObjectPipeline = gpl,
		renderObjectPipelineLayout = lyt,
		renderObjectMesh = vb,
		renderObjectMeshSize = vn,
		renderObjectTransformMatrix = model } >>
	newIORef Nothing >>= \omtri ->
	for_ [- 20 .. 20] \x -> for_ [- 20 .. 20] \y ->
		drawObject omtri cb cmd RenderObject {
			renderObjectPipeline = gpl,
			renderObjectPipelineLayout = lyt,
			renderObjectMesh = vbtri,
			renderObjectMeshSize = 3,
			renderObjectTransformMatrix =
				Cglm.mat4Mul (translation x y) scale }
	where
	model = Cglm.rotate
		Cglm.mat4Identity
		(fromIntegral fn * Cglm.rad 1)
		(Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL)
	translation x y = Cglm.translate
		Cglm.mat4Identity (Cglm.Vec3 $ x :. 0 :. y :. NilL)
	scale = Cglm.scale
		Cglm.mat4Identity (Cglm.Vec3 $ 0.2 :. 0.2 :. 0.2 :. NilL)
	cbInfo :: Vk.CmdBffr.BeginInfo 'Nothing 'Nothing
	cbInfo = def {
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit }
	rpInfo :: Vk.RndrPass.BeginInfo 'Nothing sr sf '[
		'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32,
		'Vk.ClearTypeDepthStencil ]
	rpInfo = Vk.RndrPass.BeginInfo {
		Vk.RndrPass.beginInfoNext = TMaybe.N,
		Vk.RndrPass.beginInfoRenderPass = rp,
		Vk.RndrPass.beginInfoFramebuffer = fb,
		Vk.RndrPass.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = sce },
		Vk.RndrPass.beginInfoClearValues =
			Vk.ClearValueColor (fromJust $ rgbaDouble 0 0 blue 1) :**
			Vk.ClearValueDepthStencil (Vk.ClearDepthStencilValue 1 0) :**
			HPList.Nil }
	blue = 0.5 + sin (fromIntegral fn / (180 * frashRate) * pi) / 2

frashRate :: Num n => n
frashRate = 2

data RenderObject sg sl sdlyt sm sb nm = RenderObject {
	renderObjectPipeline :: Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo sdlyt, '[WrapMeshPushConstants]),
	renderObjectPipelineLayout ::
		Vk.Ppl.Layout.P sl
			'[ '(sdlyt, '[
				'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
				'Vk.DscSetLyt.Buffer '[
					VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
			'[WrapMeshPushConstants],
	renderObjectMesh :: Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""],
	renderObjectMeshSize :: Word32,
	renderObjectTransformMatrix :: Cglm.Mat4 }

drawObject :: IORef (Maybe (Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""])) ->
	Vk.CmdBffr.C scb ->
	Vk.DscSet.D sds '(sdlyt, '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer '[
			VObj.Atom 256 GpuSceneData0 'Nothing ] ]) ->
	RenderObject sg sl sdlyt sm sb nm -> IO ()
drawObject om cb cmd RenderObject {
	renderObjectPipeline = gpl,
	renderObjectPipelineLayout = lyt,
	renderObjectMesh = vb,
	renderObjectMeshSize = vn,
	renderObjectTransformMatrix = model } =
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics lyt
		(HPList.Singleton $ U2 cmd)
		(HPList.Singleton (
			HPList.Nil :** HPList.Nil :**
			HPList.Nil )) >>
	readIORef om >>= \movb ->
	(case movb of
		Just ovb | vb == ovb -> pure ()
		_ -> do	Vk.Cmd.bindVertexBuffers cbb . HPList.Singleton
				. U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @Vertex @"" vb
			writeIORef om $ Just vb) >>
	Vk.Cmd.pushConstantsGraphics @'[ 'Vk.T.ShaderStageVertexBit ] cbb lyt (HPList.Id (GStorable.W
		MeshPushConstants {
			meshPushConstantsData = Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
			meshPushConstantsRenderMatrix = model
			}) :** HPList.Nil) >>
	Vk.Cmd.draw cbb vn 1 0 0

view :: Cglm.Mat4
view = Cglm.lookat
	(Cglm.Vec3 $ 0 :. 6 :. 10 :. NilL)
	(Cglm.Vec3 $ 0 :. 0 :. 0 :. NilL)
	(Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL)

projection :: Vk.Extent2d -> Cglm.Mat4
projection sce = Cglm.modifyMat4 1 1 negate $ Cglm.perspective
	(Cglm.rad 70) (fromIntegral (Vk.extent2dWidth sce) /
		fromIntegral (Vk.extent2dHeight sce)) 0.1 200

mainLoop :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers ss sfs,
	HPList.HomoList '(s, '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer '[
			VObj.Atom 256 GpuSceneData0 'Nothing ] ]) slyts,
	HPList.HomoList '() vss ) =>
	FramebufferResized ->
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) ss ->
	Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.P sl '[ '(s, '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
		'Vk.DscSetLyt.Buffer '[
			VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Graphics.G sg0
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Graphics.G sg1
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.CmdPl.C scp ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[VObj.List 256 Vertex ""] ->
	HPList.LL (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	HPList.PL MemoryGcd sbsms ->
	Vk.Mem.M sscnm
		'[ '(sscnb, 'Vk.Mem.BufferArg
			"scene-buffer" '[
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ])] ->
	HPList.PL (Vk.DscSet.D sds) slyts ->
	Word32 -> IO ()
mainLoop g w@(GlfwG.Win.W win) sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs vb vbtri cbs iasrfsifs cmms scnm cmds vn = do
	($ 0) . ($ Glfw.KeyState'Released) . ($ 0)
		. ($ Inf.cycle $ NE.fromList [0 .. maxFramesInFlight - 1])
		. ($ ext0) $ fix \loop ext (cf :~ cfs) fn spst0 sdrn -> do
		Glfw.pollEvents
		spst <- Glfw.getKey win Glfw.Key'Space
		let	prsd = case (spst0, spst) of
				(Glfw.KeyState'Released, Glfw.KeyState'Pressed) -> True
				_ -> False
			sdrn' = bool id (+ 1) prsd sdrn
		when prsd $ print sdrn'
		runLoop w sfc phdvc qfis dvc gq pq
			sc g ext scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs vb vbtri cbs iasrfsifs cf fn sdrn' cmms scnm cmds vn
			(\ex -> loop ex cfs ((fn + 1) `mod` (360 * frashRate)) spst sdrn')
	Vk.Dvc.waitIdle dvc

runLoop :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs,
	HPList.HomoList
		'(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ]) slyts,
	HPList.HomoList '() vss ) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> FramebufferResized -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.P sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Graphics.G sg0 '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Graphics.G sg1 '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.CmdPl.C scp ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[VObj.List 256 Vertex ""] ->
	HPList.LL (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	Int -> Int -> Int ->
	HPList.PL MemoryGcd sbsms ->
	Vk.Mem.M sscnm
		'[ '(sscnb, 'Vk.Mem.BufferArg
			"scene-buffer" '[
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ])] ->
	HPList.PL (Vk.DscSet.D sds) slyts ->
	Word32 ->
	(Vk.Extent2d -> IO ()) -> IO ()
runLoop w@(GlfwG.Win.W win) sfc phdvc qfis dvc gq pq sc frszd ext scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs vb vbtri cbs iasrfsifs cf fn sdrn cmms scnm cmds vn loop = do
	catchAndRecreate w sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs loop
		$ drawFrame dvc gq pq sc ext rp gpl0 gpl1 ppllyt fbs vb vbtri cbs iasrfsifs cf fn sdrn cmms scnm cmds vn
	cls <- Glfw.windowShouldClose win
	if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
		(loop =<< recreateSwapchainEtc
			w sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs)

drawFrame ::
	forall sfs sd ssc scfmt sr sg0 sg1 slyt s sm sb nm smtri sbtri nmtri
		scb ssos vss sbsms sscnm sscnb slyts sds . (
	HPList.HomoList
		'(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ]) slyts,
	HPList.HomoList '() vss ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Graphics.G sg0 '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt, Foo s, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Graphics.G sg1 '[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt, Foo s, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Layout.P slyt
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[VObj.List 256 Vertex ""] ->
	HPList.LL (Vk.CmdBffr.C scb) vss -> SyncObjects ssos -> Int -> Int -> Int ->
	HPList.PL MemoryGcd sbsms ->
	Vk.Mem.M sscnm
		'[ '(sscnb, 'Vk.Mem.BufferArg
			"scene-buffer" '[
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"),
				VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1") ])] ->
	HPList.PL (Vk.DscSet.D sds) slyts ->
	Word32 -> IO ()
drawFrame dvc gq pq sc ext rp gpl0 gpl1 lyt fbs vb vbtri cbs (SyncObjects iass rfss iffs) cf fn sdrn cmms scnm cmds vn =
	HPList.index iass cf \(ias :: Vk.Semaphore.S sias) ->
	HPList.index rfss cf \(rfs :: Vk.Semaphore.S srfs) ->
	HPList.index iffs cf \(id &&& HPList.Singleton -> (iff, siff)) ->
	HPList.index cmms cf \(MemoryGcd cmm) ->
	($ HPList.homoListIndex cmds cf) \cmd -> do
	Vk.Mem.write @"camera-buffer" @(VObj.Atom 256 GpuCameraData 'Nothing) dvc cmm zeroBits (gpuCameraData ext)
	if cf == 0
		then Vk.Mem.write @"scene-buffer"
			@(VObj.Atom 256 GpuSceneData0 ('Just "scene-data-0"))
			dvc scnm zeroBits (GpuSceneData0 $ gpuSceneData fn)
		else Vk.Mem.write @"scene-buffer"
			@(VObj.Atom 256 GpuSceneData0 ('Just "scene-data-1"))
			dvc scnm zeroBits (GpuSceneData0 $ gpuSceneData fn)
	Vk.Fence.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HPList.index fbs imgIdx \fb -> case sdrn `mod` 2 of
		0 -> recordCommandBuffer cb rp fb ext gpl0 lyt vb vbtri fn cmd vn
		1 -> recordCommandBuffer cb rp fb ext gpl1 lyt vb vbtri fn cmd vn
		_ -> error "never occur"
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers = HPList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
		presentInfoNew = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphores = HPList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = HPList.Singleton
				$ Vk.Khr.SwapchainImageIndex sc imgIdx }
	Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfoNew
	where	HPList.Dummy cb = cbs `HPList.homoListIndex` cf ::
			HPList.Dummy (Vk.CmdBffr.C scb) '()

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs ) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.P sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Graphics.G sg0
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Graphics.G sg1
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.CmdPl.C scp ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs ->
	(Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate w sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateSwapchainEtc
		w sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl0 gpl1 cp drsrcs fbs

recreateSwapchainEtc :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs ) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.P sl
		'[ '(s, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 GpuCameraData 'Nothing],
			'Vk.DscSetLyt.Buffer '[
				VObj.Atom 256 GpuSceneData0 'Nothing ] ])]
		'[WrapMeshPushConstants] ->
	Vk.Ppl.Graphics.G sg0
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.Ppl.Graphics.G sg1
		'[ '(Vertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, Foo s, '[WrapMeshPushConstants]) ->
	Vk.CmdPl.C scp ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateSwapchainEtc w@(GlfwG.Win.W win) sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl0 gpl1 cp (dimg, dim, divw) fbs = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwpch w sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swpch.getImages dvc sc >>= \imgs ->
			recreateImageViews dvc imgs scivs
		recreateDepthResources phdvc dvc gq cp ext dimg dim divw
		recreateGraphicsPipeline dvc ext rp ppllyt 0 gpl0
		recreateGraphicsPipeline dvc ext rp ppllyt 1 gpl1
		recreateFramebuffers dvc ext rp scivs divw fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize win = Glfw.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

positionNormalToVertex :: GStorable.W (GStorable.W WNew.Position, GStorable.W WNew.Normal) -> Vertex
positionNormalToVertex (GStorable.W ((,)
	(GStorable.W (WNew.Position x y z)) (GStorable.W (WNew.Normal v w u)))) =
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ x :. y :. z :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ v :. w :. u :. NilL,
		vertexColor = Color . Cglm.Vec3 $ v :. w :. u :. NilL }

triangle :: V.Vector Vertex
triangle = V.fromList [
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ 1 :. 1 :. 0.5 :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vertexColor = Color . Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL },
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ (- 1) :. 1 :. 0.5 :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vertexColor = Color . Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL },
	Vertex {
		vertexPos = Position . Cglm.Vec3 $ 0 :. (- 1) :. 0.5 :. NilL,
		vertexNormal = Normal . Cglm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vertexColor = Color . Cglm.Vec3 $ 0 :. 1 :. 0 :. NilL } ]

data Vertex = Vertex {
	vertexPos :: Position,
	vertexNormal :: Normal,
	vertexColor :: Color }
	deriving (Show, Generic)

newtype Position = Position Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype Normal = Normal Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype Color = Color Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

instance Storable Vertex where
	sizeOf = GStorable.gSizeOf
	alignment = GStorable.gAlignment
	peek = GStorable.gPeek
	poke = GStorable.gPoke


instance GStorable.G Vertex where

data MeshPushConstants = MeshPushConstants {
	meshPushConstantsData :: Cglm.Vec4,
	meshPushConstantsRenderMatrix :: Cglm.Mat4 } deriving (Show, Generic)

type WrapMeshPushConstants = GStorable.W MeshPushConstants

instance GStorable.G MeshPushConstants

data GpuCameraData = GpuCameraData {
	gpuCameraDataView :: View,
	gpuCameraDataProj :: Proj,
	gpuCameraDAtaViewProj :: ViewProj }
	deriving (Show, Generic)

gpuCameraData :: Vk.Extent2d -> GpuCameraData
gpuCameraData sce = GpuCameraData (View view) (Proj $ projection sce)
--	(ViewProj . Cglm.mat4Mul view $ projection sce)
	(ViewProj $ Cglm.mat4Mul (projection sce) view)

instance Storable GpuCameraData where
	sizeOf = GStorable.gSizeOf
	alignment = GStorable.gAlignment
	peek = GStorable.gPeek
	poke = GStorable.gPoke

instance GStorable.G GpuCameraData

newtype View = View Cglm.Mat4 deriving (Show, Storable)
newtype Proj = Proj Cglm.Mat4 deriving (Show, Storable)
newtype ViewProj = ViewProj Cglm.Mat4 deriving (Show, Storable)

newtype GpuSceneData0 = GpuSceneData0 GpuSceneData deriving (Show, Storable)
newtype GpuSceneData1 = GpuSceneData1 GpuSceneData deriving (Show, Storable)

data GpuSceneData = GpuSceneData {
	gpuSceneDataFogColor :: FogColor,
	gpuSceneDataFogDistances :: FogDistances,
	gpuSceneDataAmbientColor :: AmbientColor,
	gpuSceneDataSunlightDirection :: SunlightDirection,
	gpuSceneDataSunlightColor :: SunlightColor }
	deriving (Show, Generic)

gpuSceneData :: Int -> GpuSceneData
gpuSceneData fn = GpuSceneData {
	gpuSceneDataFogColor = FogColor . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	gpuSceneDataFogDistances =
		FogDistances . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	gpuSceneDataAmbientColor =
		AmbientColor . Cglm.Vec4 $ r :. 0 :. b :. 0 :. NilL,
	gpuSceneDataSunlightDirection =
		SunlightDirection . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	gpuSceneDataSunlightColor =
		SunlightColor . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL }
	where
	r = sin (fromIntegral fn / (180 * frashRate) * pi)
	b = cos (fromIntegral fn / (180 * frashRate) * pi)

instance Storable GpuSceneData where
	sizeOf = GStorable.gSizeOf
	alignment = GStorable.gAlignment
	peek = GStorable.gPeek
	poke = GStorable.gPoke

instance GStorable.G GpuSceneData

newtype FogColor = FogColor Cglm.Vec4 deriving (Show, Storable)
newtype FogDistances = FogDistances Cglm.Vec4 deriving (Show, Storable)
newtype AmbientColor = AmbientColor Cglm.Vec4 deriving (Show, Storable)
newtype SunlightDirection =
	SunlightDirection Cglm.Vec4 deriving (Show, Storable)
newtype SunlightColor = SunlightColor Cglm.Vec4 deriving (Show, Storable)

shaderStages ::
	SpirV.S 'GlslVertexShader -> SpirV.S 'GlslFragmentShader ->
	HPList.PL (U5 Vk.Ppl.ShdrSt.CreateInfo) '[
		'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
		'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
shaderStages vs fs = U5 vertShaderStageInfo :** U5 fragShaderStageInfo :** HPList.Nil
	where
	vertShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModule = (crInfo vs, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	fragShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModule = (crInfo fs, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	crInfo cd = Vk.ShaderModule.CreateInfo {
			Vk.ShaderModule.createInfoNext = TMaybe.N,
			Vk.ShaderModule.createInfoFlags = def,
			Vk.ShaderModule.createInfoCode = cd }

shaderPair0 :: (SpirV.S 'GlslVertexShader, SpirV.S 'GlslFragmentShader)
shaderPair0 = (glslVertexShaderMain0, glslFragmentShaderMain0)

glslVertexShaderMain0 :: SpirV.S 'GlslVertexShader
glslVertexShaderMain0 = [glslVertexShader|

#version 450

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inColor;

void
main()
{
	gl_Position = vec4(inPosition, 1.0);
}

|]

glslFragmentShaderMain0 :: SpirV.S 'GlslFragmentShader
glslFragmentShaderMain0 = [glslFragmentShader|

#version 450

layout(location = 0) out vec4 outColor;

void
main()
{
	outColor = vec4(1.0, 0.0, 0.0, 1.0);
}

|]

shaderPair1 :: (SpirV.S 'GlslVertexShader, SpirV.S 'GlslFragmentShader)
shaderPair1 = (glslVertexShaderMain1, glslFragmentShaderMain1)

glslVertexShaderMain1 :: SpirV.S 'GlslVertexShader
glslVertexShaderMain1 = [glslVertexShader|

#version 450

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inColor;

layout(location = 0) out vec3 outColor;

layout (set = 0, binding = 0) uniform CameraBuffer {
	mat4 view;
	mat4 proj;
	mat4 viewproj;
} cameraData;

layout(push_constant) uniform constants
{
	vec4 data;
	mat4 render_matrix;
} PushConstants;

void
main()
{
	mat4 transformMatrix = (cameraData.viewproj * PushConstants.render_matrix);
//	mat4 transformMatrix = (cameraData.proj * cameraData.view * PushConstants.render_matrix);
	gl_Position = transformMatrix * vec4(inPosition, 1.0);
	outColor = inColor;
}

|]

glslFragmentShaderMain1 :: SpirV.S 'GlslFragmentShader
glslFragmentShaderMain1 = [glslFragmentShader|

#version 450

layout(location = 0) in vec3 inColor;
layout(location = 0) out vec4 outColor;

layout (set = 0, binding = 1) uniform SceneData {
	vec4 fogColor;
	vec4 fogDistances;
	vec4 ambientColor;
	vec4 sunlightDirection;
	vec4 sunlightColor;
} sceneData;

void
main()
{
	outColor = vec4(inColor + sceneData.ambientColor.xyz, 1.0);
//	outColor = vec4(inColor, 1.0);
}

|]
