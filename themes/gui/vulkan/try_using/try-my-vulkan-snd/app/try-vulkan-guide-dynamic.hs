{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.Generics
import GHC.TypeNats
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.Storable.Generic qualified as GStorable
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object qualified as Obj
import Data.Foldable
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
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
import qualified Gpu.Vulkan.Cglm as Glm

import Foreign.Storable.Generic qualified as Str.G

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
import qualified Gpu.Vulkan.ImageView as Vk.ImgVw
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderModule
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShdrSt
import qualified Gpu.Vulkan.Pipeline.VertexInputState as Vk.Ppl.VtxIptSt
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
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Grph
import qualified Gpu.Vulkan.Framebuffer as Vk.Frmbffr
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fnc
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.Cmd as Vk.Cmd
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as Vk.Ppl.DptStnSt
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPl
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet

import qualified Codec.WavefrontObj.ReadFaceSimple as WvRf

import Control.Monad.Trans
import Options.Declarative (Flag, Def, Cmd, run_, get)

import Data.Ord.ToolsYj
import Data.Bits.ToolsYj
import Data.Tuple.ToolsYj
import Data.Function.ToolsYj
import Data.Bool.ToolsYj
import Data.Maybe.ToolsYj
import Data.List.ToolsYj
import Data.IORef.ToolsYj
import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.List.Infinite (pattern (:~))
import Data.List.Infinite qualified as Inf
import Data.TypeLevel.List qualified as TList
import Data.MonoTraversable
import Data.Sequences (IsSequence)

import Debug

main :: IO ()
main = run_ realMain

realMain ::
	Flag "m" '["model"] "FILEPATH" "model filepath"
		(Def "../../../../../files/models/viking_room.obj" String) ->
	Flag "f" '["frames"] "NUMBER" "max frames in flight" (Def "2" Int) ->
	Cmd "Try Vulkan Guide" ()
realMain mdlfp mff = liftIO $ newIORef False >>= \fr -> withWindow fr \w ->
	createIst \ist -> bool id (dbgm ist) debug
		$ body (get mdlfp) (fromIntegral $ get mff) fr w ist
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

body :: FilePath -> Natural -> FramebufferResized -> GlfwG.Win.W sw -> Vk.Ist.I si -> IO ()
body mdlfp mff fr w ist =
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
	fromNat @alu mff \(_ :: Proxy mff) (_ :: Proxy mff') ->
	createPplLyt @alu @mff' d \dsl pl ->
	createGrPpl d ex rp pl \gp ->
	createFrmbffrs d ex rp scvs dv \fbs ->
	readVtcs mdlfp >>= \vns ->
	createVtxBffr pd d gq cp vns \vb ->
	createVtxBffr pd d gq cp triangle \vbtri ->
	createVpBffrs @_ @_ @mff pd d dsl \dsls vpbs vbpms ->
	createScnBffr pd d \scb scbm ->
	Vk.CBffr.allocate @_ @mff d (cmdBffrInfo cp) \cbs ->
	createSyncObjs @mff d \sos ->
	createDscPl mff d \dp -> createDscSts d dp dsls vpbs scb \dss ->
	mainloop mff fr w sfc pd qfis d gq pq cp
		sc ex scvs rp pl gp fbs drs vb vbtri vbpms scbm dss cbs sos
	where
	fromNat :: forall alu a . KnownNat alu => Natural -> (forall n n' . (
		TList.Length n, HPList.FromList n, HPList.RepM n,
		HPList.HomoList '() n, KnownNat n', CreateVpBffrs alu n' n ) =>
		Proxy n -> Proxy n' -> a) -> a
	fromNat n f = ($ someNatVal n) \(SomeNat (pn' :: Proxy n')) ->
		tnum @alu @n' n \pn -> f pn pn'
	tnum :: forall alu mff a . (KnownNat alu, KnownNat mff) => Natural ->
		(forall (n :: [()]) . (
			TList.Length n, HPList.FromList n, HPList.RepM n,
			HPList.HomoList '() n, CreateVpBffrs alu mff n ) =>
			Proxy n -> a) -> a
	tnum 0 f = f (Proxy @'[])
	tnum n f = tnum @alu @mff (n - 1) $ f . scc
		where scc :: Proxy n -> Proxy ('() ': n); scc Proxy = Proxy

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
recreateSwpch win sfc pd qfis0 dv sc = do
	ss <- querySwpchSupportFmt @fmt pd sfc
	ex <- swapExtent win $ capabilitiesFmt ss
	let	cps = capabilitiesFmt ss
		Vk.Khr.Sfc.Format cs = fromMaybe
			(error "no available swap surface formats")
			. listToMaybe $ formatsFmt ss
		pm = findDefault Vk.Khr.PresentModeFifo
			(== Vk.Khr.PresentModeMailbox) $ presentModesFmt ss
	ex <$ Vk.Khr.Swpch.unsafeRecreate dv
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

createPplLyt :: forall alu mff sd a . Vk.Dvc.D sd -> (forall sl sdsl .
	Vk.DscSetLyt.D sdsl (DscStLytArg alu mff) ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mff)] '[WMeshPushConsts] ->
	IO a) -> IO a
createPplLyt dv f = createDscStLyt @_ @mff dv \dsl ->
	Vk.PplLyt.create dv (info dsl) nil $ f dsl
	where
	info :: Vk.DscSetLyt.D s (DscStLytArg alu mff) ->
		Vk.PplLyt.CreateInfo 'Nothing
			'[ '(s, DscStLytArg alu mff)]
			('Vk.PushConstant.Layout '[ WMeshPushConsts]
				'[ 'Vk.PushConstant.Range
					'[ 'Vk.T.ShaderStageVertexBit]
					'[WMeshPushConsts] ])
	info dsl = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

createDscStLyt :: forall alu mff sd a . Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscSetLyt.D s (DscStLytArg alu mff) -> IO a) -> IO a
createDscStLyt dv = Vk.DscSetLyt.create dv info nil
	where
	info = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = vpbd :** snbd :** HPList.Nil }
	vpbd = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	snbd = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBufferDynamic,
		Vk.DscSetLyt.bindingBufferStageFlags =
			Vk.ShaderStageVertexBit .|. Vk.ShaderStageFragmentBit }

type DscStLytArg alu mff = '[BufferViewProj alu, BufferSceneData alu mff]
type BufferViewProj alu = 'Vk.DscSetLyt.Buffer '[AtomViewProj alu]
type BufferSceneData alu mff = 'Vk.DscSetLyt.Buffer '[AtomSceneData alu mff]

type AtomViewProj alu = Obj.Atom alu WViewProj 'Nothing
type AtomSceneData alu mff = Obj.DynAtom mff alu WScene 'Nothing

createGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mff)] '[WMeshPushConsts] ->
	(forall sg . Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(sdsl, DscStLytArg alu mff)], '[WMeshPushConsts]) ->
		IO a) -> IO a
createGrPpl dv ex rp pl f = Vk.Ppl.Grph.createGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	\(HPList.Singleton (U3 p)) -> f p

recreateGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mff)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(sdsl, DscStLytArg alu mff)], '[WMeshPushConsts]) ->
	IO ()
recreateGrPpl dv ex rp pl p = Vk.Ppl.Grph.unsafeRecreateGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	(HPList.Singleton $ U3 p)

grPplInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mff)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.CreateInfo 'Nothing
		'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
		'(	'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Position), '(1, Normal), '(2, Color)] ) 'Nothing
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'(sl, '[ '(sdsl, DscStLytArg alu mff)], '[WMeshPushConsts])
		sr '(sb, vs, ts, plas)
grPplInfo ex rp pl = Vk.Ppl.Grph.CreateInfo {
	Vk.Ppl.Grph.createInfoNext = TMaybe.N,
	Vk.Ppl.Grph.createInfoFlags = zeroBits,
	Vk.Ppl.Grph.createInfoStages = uncurry shaderStages shaderPair,
	Vk.Ppl.Grph.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Grph.createInfoInputAssemblyState = Just ia,
	Vk.Ppl.Grph.createInfoViewportState = Just $ vwpSt ex,
	Vk.Ppl.Grph.createInfoRasterizationState = Just rst,
	Vk.Ppl.Grph.createInfoMultisampleState = Just ms,
	Vk.Ppl.Grph.createInfoDepthStencilState = Just ds,
	Vk.Ppl.Grph.createInfoColorBlendState = Just clrBlnd,
	Vk.Ppl.Grph.createInfoDynamicState = Nothing,
	Vk.Ppl.Grph.createInfoLayout = U3 pl,
	Vk.Ppl.Grph.createInfoRenderPass = rp,
	Vk.Ppl.Grph.createInfoSubpass = 0,
	Vk.Ppl.Grph.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Grph.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Grph.createInfoTessellationState = Nothing }
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

shaderPair :: (SpirV.S 'GlslVertexShader, SpirV.S 'GlslFragmentShader)
shaderPair = (glslVertexShaderMain, glslFragmentShaderMain)

shaderStages ::
	SpirV.S 'GlslVertexShader -> SpirV.S 'GlslFragmentShader ->
	HPList.PL (U5 Vk.Ppl.ShdrSt.CreateInfo)
	'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
shaderStages vs fs = U5 vinfo :** U5 finfo :** HPList.Nil
	where
	vinfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = zeroBits,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModule = (minfo vs, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	finfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = zeroBits,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModule = (minfo fs, nil),
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

readVtcs :: FilePath -> IO (V.Vector WVertex)
readVtcs fp = either error pure
	=<< (V.map pn2v <$>) . WvRf.posNormal . WvRf.r <$> BS.readFile fp
	where pn2v (GStorable.W ((,)
		(GStorable.W (WvRf.Position x y z))
		(GStorable.W (WvRf.Normal v w u)))) =
		GStorable.W Vtx {
			vtxPos = Position . Glm.Vec3 $ x :. y :. z :. NilL,
			vtxNormal = Normal . Glm.Vec3 $ v :. w :. u :. NilL,
			vtxColor = Color . Glm.Vec3 $ v :. w :. u :. NilL }

createVtxBffr :: (IsSequence lst, Element lst ~ WVertex) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> lst ->
	(forall sm sb al . KnownNat al => (
		Vk.Bffr.Binded sm sb bnm '[Obj.List al WVertex lnm],
		Word32 ) -> IO a) -> IO a
createVtxBffr = createBffrMem Vk.Bffr.UsageVertexBufferBit

createBffrMem :: forall sd sc lst nm lnm a .
	(IsSequence lst, Storable' (Element lst)) =>
	Vk.Bffr.UsageFlags -> Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> lst ->
	(forall sm sb al . KnownNat al => (
		Vk.Bffr.Binded sm sb nm '[Obj.List al (Element lst) lnm],
		Word32 ) -> IO a) -> IO a
createBffrMem us pd dv gq cp
	xs@((fromIntegral &&& fromIntegral) . olength -> (ln, ln')) f =
	bffrAlgn @(Obj.List 256 (Element lst) lnm) dv (Obj.LengthList ln)
		(Vk.Bffr.UsageTransferDstBit .|. us) \(_ :: Proxy al) ->
	createBffrLst pd dv ln (Vk.Bffr.UsageTransferDstBit .|. us)
		Vk.Mm.PropertyDeviceLocalBit \b _ -> do
		createBffrLst pd dv ln
			Vk.Bffr.UsageTransferSrcBit (
			Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) \
			(b' :: Vk.Bffr.Binded sm sb bnm' '[Obj.List al t lnm'])
			bm' -> do
			Vk.Mm.write
				@bnm' @(Obj.List al t lnm') dv bm' zeroBits xs
			copy b' b
		f (b, ln')
	where
	copy :: forall sm sb bnm sm' sb' bnm' al . KnownNat al =>
		Vk.Bffr.Binded sm sb bnm '[Obj.List al (Element lst) lnm] ->
		Vk.Bffr.Binded sm' sb' bnm' '[Obj.List al (Element lst) lnm] -> IO ()
	copy s d = singleTimeCmds dv gq cp \cb ->
		Vk.Cmd.copyBuffer @'[ '[Obj.List al (Element lst) lnm]] cb s d

bffrAlgn :: forall o sd a . Obj.SizeAlignment o =>
	Vk.Dvc.D sd -> Obj.Length o -> Vk.Bffr.UsageFlags ->
	(forall al . KnownNat al => Proxy al -> IO a) -> IO a
bffrAlgn dv ln us f =
	Vk.Bffr.create dv (bffrInfo (HPList.Singleton ln) us) nil \b ->
	(\(SomeNat p) -> f p) . someNatVal . fromIntegral
		=<< Vk.Mm.requirementsAlignment
		<$> Vk.Bffr.getMemoryRequirements dv b

class CreateVpBffrs alvp mff (sds :: [()]) where
	createVpBffrs :: Vk.Phd.P -> Vk.Dvc.D sd ->
		Vk.DscSetLyt.D sdsc (DscStLytArg alvp mff) -> (forall dlas svp . (
			HPList.FromList dlas, Vk.DscSet.DListFromMiddle dlas,
			Update alvp mff svp dlas,
			HPList.HomoList '(sdsc, DscStLytArg alvp mff) dlas ) =>
			HPList.PL (U2 Vk.DscSetLyt.D) dlas ->
			HPList.PL (BindedVp alvp bnmvp) svp ->
			HPList.PL (MemoryVp alvp bnmvp) svp -> IO a) -> IO a

data BindedVp alvp bnmvp smsb where
	BindedVp :: Vk.Bffr.Binded sm sb bnmvp '[AtomViewProj alvp] ->
		BindedVp alvp bnmvp '(sm, sb)

data MemoryVp alvp bnmvp smsb where
	MemoryVp ::
		Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.BufferArg bnmvp '[AtomViewProj alvp])] ->
		MemoryVp alvp bnmvp '(sm, sb)

instance CreateVpBffrs al _mff '[] where
	createVpBffrs _ _ _ f = f HPList.Nil HPList.Nil HPList.Nil

instance (
	KnownNat alvp, KnownNat mff,
	CreateVpBffrs alvp mff sds ) => CreateVpBffrs alvp mff (sd ': sds) where
	createVpBffrs pd dv dl f = createVpBffr pd dv \bnd mm ->
		createVpBffrs @alvp @_ @sds pd dv dl \dls bnds mms ->
		f (U2 dl :** dls) (BindedVp bnd :** bnds) (MemoryVp mm :** mms)

createVpBffr :: KnownNat alvp => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[AtomViewProj alvp] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[AtomViewProj alvp])] ->
	IO a) -> IO a
createVpBffr =
	createBffrAtm Vk.Bffr.UsageUniformBufferBit Vk.Mm.PropertyHostVisibleBit

createScnBffr :: (KnownNat alsn, KnownNat mff) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[AtomSceneData alsn mff] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[AtomSceneData alsn mff]) ] ->
		IO a) -> IO a
createScnBffr pd dv = createBffr pd dv
	(Obj.LengthDynAtom :** HPList.Nil)
	Vk.Bffr.UsageUniformBufferBit Vk.Mm.PropertyHostVisibleBit

createBffrAtm :: forall al sd nm a b . (KnownNat al, Storable a) =>
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Obj.Atom al a 'Nothing] ->
		Vk.Mm.M sm '[ '(
			sb, 'Vk.Mm.BufferArg nm '[Obj.Atom al a 'Nothing] )] ->
		IO b) -> IO b
createBffrAtm us prs p dv =
	createBffr p dv (HPList.Singleton Obj.LengthAtom) us prs

createBffrLst :: forall al sd bnm lnm t a . (KnownNat al, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Obj.List al t lnm] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm '[Obj.List al t lnm])] ->
		IO a) -> IO a
createBffrLst p dv ln = createBffr p dv . HPList.Singleton $ Obj.LengthList ln

createBffr :: forall sd bnm os a . (
	Obj.SizeAlignmentList os, Obj.WholeAlign os ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> HPList.PL Obj.Length os ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm os -> Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm os)] -> IO a) -> IO a
createBffr p dv lns us prs f = Vk.Bffr.create dv (bffrInfo lns us) nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits reqs) prs
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(ainfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bd))) -> bd
	where ainfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

bffrInfo :: HPList.PL Obj.Length os ->
	Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo 'Nothing os
bffrInfo lns us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N, Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = lns,
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

createDscPl :: Natural -> Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDscPl (fromIntegral -> mff) dv = Vk.DscPl.create dv info nil
	where
	info = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = mff,
		Vk.DscPl.createInfoPoolSizes = [sz0, sz1] }
	sz0 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPl.sizeDescriptorCount = mff }
	sz1 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBufferDynamic,
		Vk.DscPl.sizeDescriptorCount = mff }

createDscSts :: (
	HPList.FromList sls, Vk.DscSet.DListFromMiddle sls,
	Update alvp mff smsbs sls ) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	HPList.PL (U2 Vk.DscSetLyt.D) sls ->
	HPList.PL (BindedVp alvp bnmvp) smsbs ->
	Vk.Bffr.Binded smsn sbsn bnmsn '[AtomSceneData alvp mff] ->
	(forall sds . HPList.PL (Vk.DscSet.D sds) sls -> IO a) -> IO a
createDscSts dv dp dls bvps snb f =
	Vk.DscSet.allocateDs dv info \dss ->
	update dv dss bvps snb >> f dss
	where info = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dp,
		Vk.DscSet.allocateInfoSetLayouts = dls }

class Update alu mff smsbsvp slbtss where
	update :: Vk.Dvc.D sd ->
		HPList.PL (Vk.DscSet.D sds) slbtss ->
		HPList.PL (BindedVp alu bnmvp) smsbsvp ->
		Vk.Bffr.Binded smsn sbsn bnmsn '[AtomSceneData alu mff] -> IO ()

instance Update _alu _mff '[] '[] where update _ HPList.Nil HPList.Nil _ = pure ()

instance (
	KnownNat alu, KnownNat mff,
	Vk.DscSet.BindingAndArrayElemBuffer bts '[AtomViewProj alu] 0,
	Vk.DscSet.BindingAndArrayElemBuffer bts '[AtomSceneData alu mff] 0,
	Vk.DscSet.UpdateDynamicLength bts '[AtomViewProj alu],
	Vk.DscSet.UpdateDynamicLength bts '[AtomSceneData alu mff],
	Update alu mff smsbsvp slbtss) =>
	Update alu mff (smsbvp ': smsbsvp) ('(slyt, bts) ': slbtss) where

	update dv (ds :** dss) (BindedVp bvp :** bvps) scnb = do
		Vk.DscSet.updateDs dv (
			U5 (dscWrite @(AtomViewProj alu)
				ds bvp Vk.Dsc.TypeUniformBuffer) :**
			U5 (dscWrite @(AtomSceneData alu mff)
				ds scnb Vk.Dsc.TypeUniformBufferDynamic) :**
			HPList.Nil ) HPList.Nil
		update dv dss bvps scnb

dscWrite :: forall obj sds slbts sm sb nm objs . (
	Obj.OffsetRange obj objs, Show (HPList.PL Obj.Length objs) ) =>
	Vk.DscSet.D sds slbts -> Vk.Bffr.Binded sm sb nm objs -> Vk.Dsc.Type ->
	Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgBuffer '[ '(sm, sb, nm, obj)]) 0
dscWrite ds b tp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N, Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = tp,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos
		. HPList.Singleton . U4 $ Vk.Dsc.BufferInfo b }

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
	HPList.repM @n (Vk.Fnc.create @'Nothing dv finfo nil) \iffs ->
	f $ SyncObjs iass rfss iffs
	where
	finfo = def { Vk.Fnc.createInfoFlags = Vk.Fnc.CreateSignaledBit }

data SyncObjs (ssos :: ([Type], [Type], [Type])) where
	SyncObjs :: {
		_imageAvailableSemaphores :: HPList.PL Vk.Semaphore.S siass,
		_renderFinishedSemaphores :: HPList.PL Vk.Semaphore.S srfss,
		_inFlightFences :: HPList.PL Vk.Fnc.F sfss } ->
		SyncObjs '(siass, srfss, sfss)

mainloop :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFrmbffrs svs sfs,
	HPList.HomoList '(sdsl, DscStLytArg alu mffn) dlas,
	HPList.HomoList '() mff,
	KnownNat mffn,
	KnownNat alu, KnownNat alvmk, KnownNat alvtr ) =>
	Natural ->
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I inm scfmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mffn)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(sdsl, DscStLytArg alu mffn)], '[WMeshPushConsts]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdiv ->
	VtxBffr smvmk sbvmk bnmvmk alvmk nmvmk ->
	VtxBffr smvtr sbvtr bnmvtr alvtr nmvtr ->
	HPList.PL (MemoryVp alu bnmvp) sbsms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg bnmsn '[AtomSceneData alu mffn])] ->
	HPList.PL (Vk.DscSet.D sds) dlas ->
	HPList.LL (Vk.CBffr.C scb) mff -> SyncObjs ssoss -> IO ()
mainloop (fromIntegral -> mff) fr w sfc pd qfis dv gq pq cp
	sc ex0 vs rp pl gp fbs drs vbmk vbtr vpms snm dss cbs sos = do
	($ 0) . ($ Inf.cycle $ NE.fromList [0 .. mff - 1]) . ($ ex0) $ fix
		\go ex (cf :~ cfs) fn -> do
		Glfw.pollEvents
		run fr w sfc pd qfis dv gq pq cp
			sc ex vs rp pl gp fbs drs vbmk vbtr
			vpms snm dss cbs sos cf fn
			(\e -> go e cfs ((fn + 1) `mod` (360 * frashRate)))
	Vk.Dvc.waitIdle dv

frashRate :: Num n => n
frashRate = 2

type VtxBffr smv sbv bnmv alv nmv = (
	Vk.Bffr.Binded smv sbv bnmv '[Obj.List alv WVertex nmv],
	Vk.Cmd.VertexCount )

run :: (
	HPList.HomoList '() mff, RecreateFrmbffrs svs sfs,
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	HPList.HomoList '(sdsl, DscStLytArg alu mffn) slyts,
	KnownNat mffn, KnownNat alu, KnownNat alvmk, KnownNat alvtr ) =>
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I inm scfmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mffn)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(sdsl, DscStLytArg alu mffn)], '[WMeshPushConsts]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdiv ->
	VtxBffr smvmk sbvmk bnmvmk alvmk nmvmk ->
	VtxBffr smvtr sbvtr bnmvtr alvtr nmvtr ->
	HPList.PL (MemoryVp alu bnmvp) sbsms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg bnmsn '[AtomSceneData alu mffn])] ->
	HPList.PL (Vk.DscSet.D sds) slyts ->
	HPList.LL (Vk.CBffr.C scb) mff -> SyncObjs ssoss ->
	Int -> Int -> (Vk.Extent2d -> IO ()) -> IO ()
run fr w sfc pd qfis dv gq pq cp
	sc ex vs rp pl gp fbs drs vbmk vbtr vpms snm dss cbs soss
	cf fn go = do
	catchAndRecreate w sfc pd qfis dv gq cp sc vs rp pl gp drs fbs go
		$ draw dv gq pq sc ex rp pl
			gp fbs vbmk vbtr vpms snm dss cbs soss cf fn
	(,) <$> GlfwG.Win.shouldClose w <*> checkFlag fr >>= \case
		(True, _) -> pure (); (_, False) -> go ex
		(_, _) -> go =<< recreateAll
			w sfc pd qfis dv gq cp sc vs rp pl gp drs fbs

draw :: forall
	sd fmt ssc sr sl sdsl sg sfs
	smvmk sbvmk bnmvmk alvmk nmvmk smvtr sbvtr bnmvtr alvtr nmvtr
	alu bnmvp smsn sbsn bnmsn smsbs sds sls scb mffn mff ssos . (
	KnownNat mffn, KnownNat alu, KnownNat alvmk, KnownNat alvtr,
	HPList.HomoList '() mff,
	HPList.HomoList '(sdsl, BuffersMff alu mffn) sls ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S fmt ssc ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, BuffersMff alu mffn)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(sdsl, BuffersMff alu mffn)], '[WMeshPushConsts]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	VtxBffr smvmk sbvmk bnmvmk alvmk nmvmk ->
	VtxBffr smvtr sbvtr bnmvtr alvtr nmvtr ->
	HPList.PL (MemoryVp alu bnmvp) smsbs ->
	Vk.Mm.M smsn '[
		'(sbsn, 'Vk.Mm.BufferArg bnmsn '[AtomSceneData alu mffn]) ] ->
	HPList.PL (Vk.DscSet.D sds) sls ->
	HPList.LL (Vk.CBffr.C scb) mff -> SyncObjs ssos ->
	Int -> Int -> IO ()
draw dv gq pq sc ex rp lyt gp fbs
	vb vbtr vpms scnm dss cbs (SyncObjs iass rfss iffs) cf fn =
	HPList.index iass cf \ias -> HPList.index rfss cf \rfs ->
	HPList.index iffs cf \(id &&& HPList.Singleton -> (iff, siff)) ->
	HPList.index vpms cf \(MemoryVp vpm) ->
	($ HPList.homoListIndex dss cf) \ds -> do
	Vk.Fnc.waitForFs dv siff True Nothing >> Vk.Fnc.resetFs dv siff
	Vk.Mm.write @bnmvp @(AtomViewProj alu) dv vpm zeroBits (viewProjData ex)
	Vk.Mm.write @bnmsn @(AtomSceneData alu mffn) dv scnm zeroBits . (!! cf)
		$ iterate (Nothing :) [Just $ sceneData fn]
	ii <- Vk.Khr.acquireNextImageResult
		[Vk.Success, Vk.SuboptimalKhr] dv sc maxBound (Just ias) Nothing
	Vk.CBffr.reset cb zeroBits
	HPList.index fbs ii \fb ->
		recordCmdBffr cb ex rp lyt gp fb vb vbtr cf fn ds
	Vk.Q.submit gq (HPList.Singleton . U4 $ sinfo ias rfs) $ Just iff
	catchAndSerialize . Vk.Khr.queuePresent pq $ pinfo rfs ii
	where
	HPList.Dummy cb = HPList.homoListIndex @'() cbs cf
	sinfo :: Vk.Semaphore.S ssi -> Vk.Semaphore.S ssr ->
		Vk.SubmitInfo 'Nothing '[ssi] '[scb] '[ssr]
	sinfo ias rfs = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks =
			HPList.Singleton $ Vk.SemaphorePipelineStageFlags
				ias Vk.Ppl.StageColorAttachmentOutputBit,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
	pinfo :: Vk.Semaphore.S ssr -> Word32 ->
		Vk.Khr.PresentInfo 'Nothing '[ssr] fmt '[ssc]
	pinfo rfs ii = Vk.Khr.PresentInfo {
		Vk.Khr.presentInfoNext = TMaybe.N,
		Vk.Khr.presentInfoWaitSemaphores = HPList.Singleton rfs,
		Vk.Khr.presentInfoSwapchainImageIndices =
			HPList.Singleton $ Vk.Khr.SwapchainImageIndex sc ii }
	catchAndSerialize = (`catch`
		\(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

type BuffersMff alu mff = DscStLytArg alu mff

recordCmdBffr ::
	forall sr slyt sg sdlyt sf sm sb nm smtri sbtri nmtri scb sds mff alu alv alvtr nmvmk nmvtr .
	(KnownNat alu, KnownNat alv, KnownNat alvtr) =>
	Vk.CBffr.C scb ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P slyt '[ '(sdlyt, BuffersMff alu mff)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt,	'[ '(sdlyt, BuffersMff alu mff)], '[WMeshPushConsts]) ->
	Vk.Frmbffr.F sf ->
	(Vk.Bffr.Binded sm sb nm '[Obj.List alv WVertex nmvmk], Word32) ->
	(Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List alvtr WVertex nmvtr], Word32) ->
	Int -> Int ->
	Vk.DscSet.D sds '(sdlyt, BuffersMff alu mff) ->
	IO ()
recordCmdBffr cb sce rp lyt gpl fb (vb, vn) (vbt, vntr) (fromIntegral -> cf) (fromIntegral -> fn) ds =
	Vk.CBffr.begin @'Nothing @'Nothing cb binfo $
	Vk.Cmd.beginRenderPass cb rpinfo Vk.Subpass.ContentsInline do
	ovb <- newIORef Nothing
	drawObject ovb cb ds RenderObject {
		renderObjectPipeline = gpl,
		renderObjectPipelineLayout = lyt,
		renderObjectMesh = vb, renderObjectMeshSize = vn,
		renderObjectTransformMatrix = model } cf
	ovbtri <- newIORef Nothing
	for_ [- 20 .. 20] \x -> for_ [- 20 .. 20] \y ->
		drawObject ovbtri cb ds RenderObject {
			renderObjectPipeline = gpl,
			renderObjectPipelineLayout = lyt,
			renderObjectMesh = vbt, renderObjectMeshSize = 3,
			renderObjectTransformMatrix =
				trans x y `Glm.mat4Mul` scale } cf
	where
	binfo :: Vk.CBffr.BeginInfo 'Nothing 'Nothing
	binfo = def { Vk.CBffr.beginInfoFlags = Vk.CBffr.UsageOneTimeSubmitBit }
	rpinfo :: Vk.RndrPss.BeginInfo 'Nothing sr sf '[
		'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32,
		'Vk.ClearTypeDepthStencil ]
	rpinfo = Vk.RndrPss.BeginInfo {
		Vk.RndrPss.beginInfoNext = TMaybe.N,
		Vk.RndrPss.beginInfoRenderPass = rp,
		Vk.RndrPss.beginInfoFramebuffer = fb,
		Vk.RndrPss.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = sce },
		Vk.RndrPss.beginInfoClearValues =
			Vk.ClearValueColor
				(fromJust $ rgbaDouble 0 0 blue 1) :**
			Vk.ClearValueDepthStencil
				(Vk.ClearDepthStencilValue 1 0) :** HPList.Nil }
	blue = 0.5 + sin (fn / (180 * frashRate) * pi) / 2
	model = Glm.rotate Glm.mat4Identity
		(fn * Glm.rad 1) (Glm.Vec3 $ 0 :. 1 :. 0 :. NilL)
	trans x y = Glm.translate
		Glm.mat4Identity (Glm.Vec3 $ x :. 0 :. y :. NilL)
	scale = Glm.scale
		Glm.mat4Identity (Glm.Vec3 $ 0.2 :. 0.2 :. 0.2 :. NilL)

drawObject :: forall sm sb nm alv nmvmk scb sds sdlyt alu mff sg sl .
	(KnownNat alu, KnownNat alv) =>
	IORef (Maybe (Vk.Bffr.Binded sm sb nm '[Obj.List alv WVertex nmvmk])) ->
	Vk.CBffr.C scb -> Vk.DscSet.D sds '(sdlyt, BuffersMff alu mff) ->
	RenderObject alu alv mff sg sl sdlyt sm sb nm nmvmk -> Word32 -> IO ()
drawObject ovb cb0 ds RenderObject {
	renderObjectPipeline = gpl,
	renderObjectPipelineLayout = lyt,
	renderObjectMesh = vb, renderObjectMeshSize = vn,
	renderObjectTransformMatrix = model } ffn =
	Vk.Cmd.bindPipelineGraphics cb0 Vk.Ppl.BindPointGraphics gpl \cb -> do
	Vk.Cmd.bindDescriptorSetsGraphics cb Vk.Ppl.BindPointGraphics lyt
		(HPList.Singleton $ U2 ds) . HPList.Singleton $
		(HPList.Nil :** (Vk.Cmd.DynamicIndex ffn :** HPList.Nil) :** HPList.Nil)
	readIORef ovb >>= \case
		Just o | vb == o -> pure ()
		_ -> do	Vk.Cmd.bindVertexBuffers cb . HPList.Singleton
				. U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @nmvmk vb
			writeIORef ovb $ Just vb
	Vk.Cmd.pushConstantsGraphics @'[ 'Vk.T.ShaderStageVertexBit] cb lyt
		$ HPList.Id (GStorable.W MeshPushConstants {
			meshPushConstantsData =
				Glm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
			meshPushConstantsRenderMatrix = model }) :** HPList.Nil
	Vk.Cmd.draw cb vn 1 0 0

data RenderObject alu alv mff sg sl sdlyt sm sb nm nmvmk = RenderObject {
	renderObjectPipeline :: Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(sdlyt, BuffersMff alu mff)], '[WMeshPushConsts]),
	renderObjectPipelineLayout ::
		Vk.PplLyt.P sl '[ '(sdlyt, BuffersMff alu mff)] '[WMeshPushConsts],
	renderObjectMesh :: Vk.Bffr.Binded sm sb nm '[Obj.List alv WVertex nmvmk],
	renderObjectMeshSize :: Word32,
	renderObjectTransformMatrix :: Glm.Mat4 }

catchAndRecreate :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFrmbffrs sis sfs) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QFamIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C scp ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(s, BuffersMff alu mff)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(s, BuffersMff alu mff)], '[WMeshPushConsts]) ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs -> (Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate w sfc pd qfis dv gq cp sc scivs rp lyt gpl drs fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just (); Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\() -> loop =<< recreateAll
		w sfc pd qfis dv gq cp sc scivs rp lyt gpl drs fbs

recreateAll :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFrmbffrs sis sfs) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QFamIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C scp ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(slyt, BuffersMff alu mff)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(slyt, BuffersMff alu mff)], '[WMeshPushConsts]) ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateAll w@(GlfwG.Win.W win) sfc pd qfs dv gq cp sc scivs rp lyt gpl drs@(_, _, divw) fbs =
	waitFramebufferSize win >> Vk.Dvc.waitIdle dv >>
	recreateSwpch w sfc pd qfs dv sc >>= \ex ->
	ex <$ do
	Vk.Khr.Swpch.getImages dv sc >>= \i -> recreateImgVws dv i scivs
	recreateDptRsrcs pd dv gq cp ex drs
	recreateGrPpl dv ex rp lyt gpl
	recreateFrmbffrs dv ex rp scivs divw fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize w = Glfw.getFramebufferSize w >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize w
	where zero = uncurry (||) . ((== 0) *** (== 0))

-- VERTEX

type WVertex = GStorable.W Vertex

data Vertex = Vtx {
	vtxPos :: Position, vtxNormal :: Normal, vtxColor :: Color }
	deriving (Show, Generic)

newtype Position = Position Glm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Normal = Normal Glm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Color = Color Glm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

instance Str.G.G Vertex

instance Storable Vertex where
	sizeOf = Str.G.gSizeOf; alignment = Str.G.gAlignment
	peek = Str.G.gPeek; poke = Str.G.gPoke

triangle :: V.Vector WVertex
triangle = V.fromList $ GStorable.W <$> [
	Vtx {	vtxPos = Position . Glm.Vec3 $ 1 :. 1 :. 0.5 :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vtxColor = Color . Glm.Vec3 $ 0 :. 1 :. 0 :. NilL },
	Vtx {	vtxPos = Position . Glm.Vec3 $ (- 1) :. 1 :. 0.5 :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vtxColor = Color . Glm.Vec3 $ 0 :. 1 :. 0 :. NilL },
	Vtx {	vtxPos = Position . Glm.Vec3 $ 0 :. (- 1) :. 0.5 :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vtxColor = Color . Glm.Vec3 $ 0 :. 1 :. 0 :. NilL } ]

-- CAMERA DATA

type WViewProj = GStorable.W CameraData

data CameraData = CameraData {
	cameraDataView :: View, cameraDataProj :: Proj,
	cameraDataViewProj :: ViewProj } deriving (Show, Generic)

newtype View = View Glm.Mat4 deriving (Show, Storable)
newtype Proj = Proj Glm.Mat4 deriving (Show, Storable)
newtype ViewProj = ViewProj Glm.Mat4 deriving (Show, Storable)

instance Storable CameraData where
	sizeOf = Str.G.gSizeOf; alignment = Str.G.gAlignment
	peek = Str.G.gPeek; poke = Str.G.gPoke

instance Str.G.G CameraData

viewProjData :: Vk.Extent2d -> WViewProj
viewProjData ex = GStorable.W $ CameraData (View view) (Proj $ projection ex)
	(ViewProj $ Glm.mat4Mul (projection ex) view)

view :: Glm.Mat4
view = Glm.lookat
	(Glm.Vec3 $ 0 :. 6 :. 10 :. NilL)
	(Glm.Vec3 $ 0 :. 0 :. 0 :. NilL)
	(Glm.Vec3 $ 0 :. 1 :. 0 :. NilL)

projection :: Vk.Extent2d -> Glm.Mat4
projection Vk.Extent2d {
	Vk.extent2dWidth = fromIntegral -> w,
	Vk.extent2dHeight = fromIntegral -> h } = Glm.modifyMat4 1 1 negate
	$ Glm.perspective (Glm.rad 70) (w / h) 0.1 200

-- SCENE DATA

type WScene = GStorable.W SceneData

data SceneData = SceneData {
	sceneDataFogColor :: FogColor, sceneDataFogDists :: FogDists,
	sceneDataAmbColor :: AmbColor,
	sceneDataSunDir :: SunDir, sceneDataSunColor :: SunColor }
	deriving (Show, Generic)

newtype FogColor = FogColor Glm.Vec4 deriving (Show, Storable)
newtype FogDists = FogDists Glm.Vec4 deriving (Show, Storable)
newtype AmbColor = AmbColor Glm.Vec4 deriving (Show, Storable)
newtype SunDir = SunDir Glm.Vec4 deriving (Show, Storable)
newtype SunColor = SunColor Glm.Vec4 deriving (Show, Storable)

instance Storable SceneData where
	sizeOf = Str.G.gSizeOf; alignment = Str.G.gAlignment
	peek = Str.G.gPeek; poke = Str.G.gPoke

instance Str.G.G SceneData

sceneData :: Int -> WScene
sceneData fn = GStorable.W SceneData {
	sceneDataFogColor = FogColor . Glm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	sceneDataFogDists = FogDists . Glm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	sceneDataAmbColor = AmbColor . Glm.Vec4 $ r :. 0 :. b :. 0 :. NilL,
	sceneDataSunDir = SunDir . Glm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	sceneDataSunColor = SunColor . Glm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL }
	where
	r = sin (fromIntegral fn / (180 * frashRate) * pi)
	b = cos (fromIntegral fn / (180 * frashRate) * pi)

-- MESH PUSH CONSTANTS

data MeshPushConstants = MeshPushConstants {
	meshPushConstantsData :: Glm.Vec4,
	meshPushConstantsRenderMatrix :: Glm.Mat4 } deriving (Show, Generic)

type WMeshPushConsts = GStorable.W MeshPushConstants

instance Str.G.G MeshPushConstants

-- OTHER TYPES

-- SHADER

[glslVertexShader|

#version 450

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inColor;

layout(location = 0) out vec3 outColor;

layout (set = 0, binding = 0) uniform CameraBuffer {
	mat4 view; mat4 proj; mat4 viewproj; } cameraData;

layout(push_constant) uniform constants {
	vec4 data; mat4 render_matrix; } PushConstants;

void
main()
{
	mat4 transformMatrix =
		(cameraData.viewproj * PushConstants.render_matrix);
	gl_Position = transformMatrix * vec4(inPosition, 1.0);
	outColor = inColor;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec3 inColor;
layout(location = 0) out vec4 outColor;

layout (set = 0, binding = 1) uniform SceneData {
	vec4 fogColor; vec4 fogDists;
	vec4 ambientColor;
	vec4 sunlightDir; vec4 sunlightColor; } sceneData;

void
main()
{
	outColor = vec4(inColor + sceneData.ambientColor.xyz, 1.0);
}

|]
