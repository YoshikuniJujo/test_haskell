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
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import qualified Gpu.Vulkan.Memory as Vk.Mem

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
import Data.TypeLevel.Tuple.Index qualified as TIndex
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
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr.M
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fnc
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Mm.M
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

body :: FilePath -> FramebufferResized -> GlfwG.Win.W sw -> Vk.Ist.I si -> IO ()
body mdlfp fr w ist =
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
	tnum' maxFramesInFlight \(_ :: Proxy mff') ->
	createPplLyt @_ @MaxFramesInFlight d \dsl pl ->
	createGraphicsPipeline @MaxFramesInFlight d ex rp pl \gp ->
	createFramebuffers d ex rp scvs dv \fbs ->

	tnum maxFramesInFlight \(_ :: Proxy mff) ->
	createVpBffrs @256 @mff pd d dsl \lyts cmbs cmms ->
	createSceneBuffer @MaxFramesInFlight pd d \scnb scnm ->
	createDescriptorPool d \dp ->
	createDescriptorSets d dp cmbs lyts scnb \dss ->
	readVtcs mdlfp >>= \(id &&& fromIntegral . V.length -> (vns, vnsln)) ->
	createVertexBuffer pd d gq cp vns \vb ->
	createVertexBuffer pd d gq cp triangle \vbtri ->
	createCommandBuffers @MaxFramesInFlight d cp \cbs ->
	createSyncObjs @mff d \sos ->

	mainLoop w fr sfc pd qfis d gq pq sc ex scvs rp pl gp cp drs fbs
		cmms scnm dss vb vbtri cbs sos vnsln
	where
	tnum :: Int -> (forall (n :: [()]) . (
		TList.Length n, HPList.FromList n,
		HPList.HomoList '() n, HPList.RepM n,
--		CreateVpBffrs n ) => Proxy n -> a) -> a
		CreateVpBffrs 256 n ) => Proxy n -> a) -> a
	tnum 0 f = f (Proxy @'[])
	tnum n f = tnum (n - 1) \p -> f $ plus1 p
		where plus1 :: Proxy n -> Proxy ('() ': n); plus1 Proxy = Proxy
	tnum' :: Int -> (forall (nt :: Nat) . KnownNat nt => Proxy nt -> a) -> a
	tnum' n f = ($ someNatVal $ fromIntegral n) \(SomeNat p) -> f p

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

maxFramesInFlight :: Integral n => n
maxFramesInFlight = 2

type MaxFramesInFlight = 2

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

type QueueFamilyIndices = QFamIndices

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
-- type SceneObj dn = Obj.DynAtom dn 256 SceneData 'Nothing

type BuffersMff mff = '[
	'Vk.DscSetLyt.Buffer '[CameraObj], 'Vk.DscSetLyt.Buffer '[SceneObj mff] ]

type CameraObj = Obj.Atom 256 WViewProj 'Nothing
type SceneObj dn = Obj.DynAtom dn 256 WScene 'Nothing

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

createGraphicsPipeline :: forall mff sd sr sl sdl a . Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdl, BuffersMff mff)] '[WMeshPushConsts] ->
	(forall sg . Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(sdl, BuffersMff mff)], '[WMeshPushConsts]) -> IO a) ->
	IO a
createGraphicsPipeline dv sce rp lyt f = Vk.Ppl.Grph.createGs dv Nothing
	(HPList.Singleton . U14 $ graphicsPipelineCreateInfo sce rp lyt) nil
	\(HPList.Singleton (U3 gpl)) -> f gpl

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdl, BuffersMff mff)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(sdl, BuffersMff mff)], '[WMeshPushConsts]) -> IO ()
recreateGraphicsPipeline dv sce rp lyt gpls = Vk.Ppl.Grph.unsafeRecreateGs dv Nothing
	(U14 (graphicsPipelineCreateInfo sce rp lyt) :** HPList.Nil) nil
	(U3 gpls :** HPList.Nil)

graphicsPipelineCreateInfo :: forall mff sr sl sdl sb vs' ts' larg .
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdl, BuffersMff mff)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.CreateInfo 'Nothing
		'[	'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[])]
		'( 'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Position), '(1, Normal), '(2, Color)])
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'(sl, '[ '(sdl, BuffersMff mff)], '[WMeshPushConsts]) sr
		'(sb, vs', ts', larg)
graphicsPipelineCreateInfo sce rp lyt = Vk.Ppl.Grph.CreateInfo {
	Vk.Ppl.Grph.createInfoNext = TMaybe.N,
	Vk.Ppl.Grph.createInfoFlags = zeroBits,
	Vk.Ppl.Grph.createInfoStages =
		shaderStages glslVertexShaderMain glslFragmentShaderMain,
	Vk.Ppl.Grph.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Grph.createInfoInputAssemblyState = Just inputAssembly,
	Vk.Ppl.Grph.createInfoViewportState = Just $ viewportState sce,
	Vk.Ppl.Grph.createInfoRasterizationState = Just rasterizer,
	Vk.Ppl.Grph.createInfoMultisampleState = Just multisampling,
	Vk.Ppl.Grph.createInfoDepthStencilState = Just depthStencil,
	Vk.Ppl.Grph.createInfoColorBlendState = Just colorBlending,
	Vk.Ppl.Grph.createInfoDynamicState = Nothing,
	Vk.Ppl.Grph.createInfoLayout = U3 lyt,
	Vk.Ppl.Grph.createInfoRenderPass = rp,
	Vk.Ppl.Grph.createInfoSubpass = 0,
	Vk.Ppl.Grph.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Grph.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Grph.createInfoTessellationState = Nothing }

inputAssembly :: Vk.Ppl.InpAsmbSt.CreateInfo 'Nothing
inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
	Vk.Ppl.InpAsmbSt.createInfoNext = TMaybe.N,
	Vk.Ppl.InpAsmbSt.createInfoFlags = zeroBits,
	Vk.Ppl.InpAsmbSt.createInfoTopology = Vk.PrimitiveTopologyTriangleList,
	Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable = False }

viewportState :: Vk.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo 'Nothing
viewportState sce = def {
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
	Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeNone,
	Vk.Ppl.RstSt.createInfoFrontFace = Vk.FrontFaceClockwise,
	Vk.Ppl.RstSt.createInfoDepthBiasEnable = False,
	Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0 }

depthStencil :: Vk.Ppl.DptStnSt.CreateInfo 'Nothing
depthStencil = Vk.Ppl.DptStnSt.CreateInfo {
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

createCmdPl :: QFamIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfis dv = Vk.CmdPl.create dv info nil
	where info = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = grFam qfis }

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

type DepthResources sb sm nm fmt sdiv = (
	Vk.Img.Binded sm sb nm fmt,
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)],
	Vk.ImgVw.I nm fmt sdiv )

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

cmdBffrInfo :: forall n scp .
	Vk.CmdPl.C scp -> Vk.CBffr.AllocateInfo 'Nothing scp n
cmdBffrInfo cp = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cp,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

findMemoryType :: Vk.Phd.P ->
	Vk.Mm.M.TypeBits -> Vk.Mm.PropertyFlags -> IO Vk.Mm.M.TypeIndex
findMemoryType pd ts prs0 =
	maybe (error msg) pure . suitable =<< Vk.Phd.getMemoryProperties pd
	where
	msg = "failed to find suitable memory type!"
	suitable prs1 = fst <$> find ((&&)
		<$> (`Vk.Mm.M.elemTypeIndex` ts) . fst
		<*> checkBits prs0 . Vk.Mm.M.mTypePropertyFlags . snd)
		(Vk.Phd.memoryPropertiesMemoryTypes prs1)

beginSingleTimeCommands :: forall sd sc a .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	(forall s . Vk.CBffr.C s -> IO a) -> IO a
beginSingleTimeCommands dv gq cp cmds =
	Vk.CBffr.allocate dv allocInfo \(cb :*. HPList.Nil) ->
	Vk.CBffr.begin @'Nothing @'Nothing cb beginInfo (cmds cb) <* do
	Vk.Q.submit gq (HPList.Singleton . U4 $ submitInfo cb) Nothing
	Vk.Q.waitIdle gq
	where
	allocInfo :: Vk.CBffr.AllocateInfo 'Nothing sc  '[ '()]
	allocInfo = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cp,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }
	beginInfo = Vk.CBffr.M.BeginInfo {
		Vk.CBffr.beginInfoNext = TMaybe.N,
		Vk.CBffr.beginInfoFlags = Vk.CBffr.UsageOneTimeSubmitBit,
		Vk.CBffr.beginInfoInheritanceInfo = Nothing }
	submitInfo :: forall s . Vk.CBffr.C s -> Vk.SubmitInfo 'Nothing '[] '[s] '[]
	submitInfo cb = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

createFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
	Vk.RndrPss.R sr -> HPList.PL (Vk.ImgVw.I nm fmt) sis ->
	Vk.ImgVw.I dfmt dnm siv ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HPList.Nil _ f = f HPList.Nil
createFramebuffers dv sce rp (iv :** ivs) dptiv f =
	Vk.Frmbffr.create dv (framebufferInfo sce rp iv dptiv) nil \fb ->
	createFramebuffers dv sce rp ivs dptiv \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPss.R sr -> HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
		Vk.ImgVw.I dfmt dnm sdiv -> HPList.PL Vk.Frmbffr.F sfs ->
		IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _ _ _ HPList.Nil _ HPList.Nil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dv sce rp (sciv :** scivs) dptiv (fb :** fbs) =
		Vk.Frmbffr.unsafeRecreate dv
			(framebufferInfo sce rp sciv dptiv) nil fb >>
		recreateFramebuffers dv sce rp scivs dptiv fbs

framebufferInfo ::
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I nm fmt si ->
	Vk.ImgVw.I dfmt dnm sdiv ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(nm, fmt, si), '(dfmt, dnm, sdiv)]
framebufferInfo Vk.Extent2d {
	Vk.extent2dWidth = w, Vk.extent2dHeight = h } rp attch dpt =
	Vk.Frmbffr.CreateInfo {
		Vk.Frmbffr.createInfoNext = TMaybe.N,
		Vk.Frmbffr.createInfoFlags = zeroBits,
		Vk.Frmbffr.createInfoRenderPass = rp,
		Vk.Frmbffr.createInfoAttachments =
			U3 attch :** U3 dpt :** HPList.Nil,
		Vk.Frmbffr.createInfoWidth = w,
		Vk.Frmbffr.createInfoHeight = h,
		Vk.Frmbffr.createInfoLayers = 1 }

class CreateVpBffrs (alvp :: Nat) (sds :: [()]) where
	createVpBffrs :: Vk.Phd.P -> Vk.Dvc.D sd ->
--		Vk.DscSetLyt.D sdsc (DscStLytArg alvp) -> (forall dlas svp . (
		Vk.DscSetLyt.D sdsc (DscStLytArg 256 MaxFramesInFlight) -> (forall dlas svp . (
			HPList.FromList dlas, Vk.DscSet.DListFromMiddle dlas,
			Update svp dlas,
--			HPList.HomoList '(sdsc, DscStLytArg alvp) dlas ) =>
			HPList.HomoList '(sdsc, DscStLytArg 256 MaxFramesInFlight) dlas ) =>
			HPList.PL (U2 Vk.DscSetLyt.D) dlas ->
			HPList.PL (BindedVp 256 nmvp) svp ->
			HPList.PL (MemoryVp 256 nmvp) svp -> IO a) -> IO a
--			HPList.PL (BindedVp alvp nmvp) svp ->
--			HPList.PL (MemoryVp alvp nmvp) svp -> IO a) -> IO a

data BindedVp alvp nmvp smsb where
	BindedVp :: Vk.Bffr.Binded sm sb nmvp '[AtomViewProj alvp] ->
		BindedVp alvp nmvp '(sm, sb)

data MemoryVp alvp bnmvp smsb where
	MemoryVp ::
		Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.BufferArg bnmvp '[AtomViewProj alvp])] ->
		MemoryVp alvp bnmvp '(sm, sb)

instance CreateVpBffrs al '[] where
	createVpBffrs _ _ _ f = f HPList.Nil HPList.Nil HPList.Nil

instance (
	KnownNat alvp,
--	Obj.OffsetRange (Obj.Atom alvp WScene (Just sd))
--		(SceneBffrArg alvp SceneNames),
	CreateVpBffrs alvp sds ) => CreateVpBffrs alvp (sd ': sds) where
	createVpBffrs pd dv dl f = createVpBffr pd dv \bnd mm ->
		createVpBffrs @alvp @sds pd dv dl \dls bnds mms ->
		f (U2 dl :** dls) (BindedVp bnd :** bnds) (MemoryVp mm :** mms)

createVpBffr :: KnownNat alvp => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[AtomViewProj alvp] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[AtomViewProj alvp])] ->
	IO a) -> IO a
createVpBffr =
	createBffrAtm Vk.Bffr.UsageUniformBufferBit Vk.Mm.PropertyHostVisibleBit

createBffrAtm :: forall al sd nm a b . (KnownNat al, Storable a) =>
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Obj.Atom al a 'Nothing] ->
		Vk.Mm.M sm '[ '(
			sb, 'Vk.Mm.BufferArg nm '[Obj.Atom al a 'Nothing] )] ->
		IO b) -> IO b
createBffrAtm us prs p dv =
	createBffr p dv (HPList.Singleton Obj.LengthAtom) us prs

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

type BindedCamera = BindedVp 256 "camera-buffer"
type MemoryCamera = MemoryVp 256 "camera-buffer"

createBuffer :: forall objs nm sd a . (
	Obj.SizeAlignmentList objs, forall s . SizeAlignmentAll s nm objs, Obj.WholeAlign objs ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> HPList.PL Obj.Length objs ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm objs ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm objs)] -> IO a) ->
	IO a
createBuffer p dv lns usg prs f =
	Vk.Bffr.create dv (bufferInfo lns usg) nil \b ->
	Vk.Bffr.getMemoryRequirements dv b >>= \rs ->
	findMemoryType p (Vk.Mm.M.requirementsMemoryTypeBits rs) prs >>= \mt ->
	Vk.Mm.allocateBind dv
		(HPList.Singleton . U2 $ Vk.Mm.Buffer b) (memoryInfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) -> bnd

class Vk.Mm.Bindable '[ '(s, 'Vk.Mm.BufferArg nm objs)] =>
	SizeAlignmentAll s nm (objs :: [Obj.O])

instance Vk.Mm.Bindable '[ '(s, 'Vk.Mm.BufferArg nm '[obj])] =>
	SizeAlignmentAll s nm '[obj]

instance {-# OVERLAPPABLE #-} (
	Obj.SizeAlignment obj, SizeAlignmentAll s nm objs, Obj.WholeAlign objs ) =>
	SizeAlignmentAll s nm (obj ': objs)

bufferInfo :: HPList.PL Obj.Length objs ->
	Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo 'Nothing objs
bufferInfo lns usg = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = lns,
	Vk.Bffr.createInfoUsage = usg,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

memoryInfo :: Vk.Mm.M.TypeIndex -> Vk.Mem.AllocateInfo 'Nothing
memoryInfo mt = Vk.Mem.AllocateInfo {
	Vk.Mem.allocateInfoNext = TMaybe.N,
	Vk.Mem.allocateInfoMemoryTypeIndex = mt }

createSceneBuffer :: forall mff sd nm a . KnownNat mff =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[SceneObj mff] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[SceneObj mff])] ->
	IO a) -> IO a
createSceneBuffer pd dv = createBuffer pd dv
	(HPList.Singleton Obj.LengthDynAtom)
	Vk.Bffr.UsageUniformBufferBit Vk.Mm.PropertyHostVisibleBit

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDescriptorPool dv = Vk.DscPl.create dv poolInfo nil
	where poolInfo = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = 10,
		Vk.DscPl.createInfoPoolSizes = [
			Vk.DscPl.Size {
				Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBuffer,
				Vk.DscPl.sizeDescriptorCount = 10 },
			Vk.DscPl.Size {
				Vk.DscPl.sizeType =
					Vk.Dsc.TypeUniformBufferDynamic,
				Vk.DscPl.sizeDescriptorCount = 10 } ] }

createDescriptorSets ::
	(Vk.DscSet.DListFromMiddle lyts, HPList.FromList lyts, Update cmbs lyts) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	HPList.PL BindedCamera cmbs -> HPList.PL (U2 Vk.DscSetLyt.D) lyts ->
	Vk.Bffr.Binded ssb ssm "scene-buffer" '[SceneObj MaxFramesInFlight] ->
	(forall sds . HPList.PL (Vk.DscSet.D sds) lyts -> IO a) -> IO a
createDescriptorSets dv dscp cmbs lyts scnb f =
	Vk.DscSet.allocateDs dv allocInfo \dscss -> do
	update dv dscss cmbs scnb
	f dscss
	where allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = lyts }

class Update csbs lyts where
	update :: Vk.Dvc.D sd -> HPList.PL (Vk.DscSet.D sds) lyts ->
		HPList.PL BindedCamera csbs ->
		Vk.Bffr.Binded ssb ssm "scene-buffer" '[SceneObj MaxFramesInFlight] -> IO ()

instance Update '[] '[] where update _ HPList.Nil HPList.Nil _ = pure ()

instance (
	Vk.DscSet.BindingAndArrayElemBuffer
		(TIndex.I1_2 '(slyt, bs))
		'[CameraObj] 0,
	Vk.DscSet.BindingAndArrayElemBuffer
		(TIndex.I1_2 '(slyt, bs))
		'[SceneObj MaxFramesInFlight] 0,
	Vk.DscSet.UpdateDynamicLength
		(TIndex.I1_2 '(slyt, bs))
		'[CameraObj],
	Vk.DscSet.UpdateDynamicLength
		(TIndex.I1_2 '(slyt, bs))
		'[SceneObj MaxFramesInFlight],
	Update csbs lyts ) => Update (csb ': csbs) ('(slyt, bs) ': lyts) where
	update dv (dscs :** dscss) (BindedVp csb :** csbs) scnb = do
		Vk.DscSet.updateDs dv (
			U5 (descriptorWrite @CameraObj
				dscs csb Vk.Dsc.TypeUniformBuffer) :**
			U5 (descriptorWrite @(SceneObj MaxFramesInFlight)
				dscs scnb Vk.Dsc.TypeUniformBufferDynamic) :**
			HPList.Nil )
			HPList.Nil
		update dv dscss csbs scnb

descriptorWrite :: forall obj slbts sb sm nm objs sds . (
	Show (HPList.PL Obj.Length objs), Obj.OffsetRange obj objs ) =>
	Vk.DscSet.D sds slbts -> Vk.Bffr.Binded sm sb nm objs ->
	Vk.Dsc.Type -> Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgBuffer '[ '(sm, sb, nm, obj)]) 0
descriptorWrite dscs ub tp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = tp,
	Vk.DscSet.writeSources =
		Vk.DscSet.BufferInfos . HPList.Singleton . U4 $ Vk.Dsc.BufferInfo ub }

createVertexBuffer :: forall sd sc nm a . Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C sc -> V.Vector WVertex -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] -> IO a ) ->
	IO a
createVertexBuffer pd dv gq cp vs f =
	createBuffer pd dv lns
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit \b _ ->
	createBuffer pd dv lns
		Vk.Bffr.UsageTransferSrcBit
		(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)
		\b' (bm' :: Vk.Mm.M sm '[ '(s,
			'Vk.Mm.BufferArg nm '[Obj.List 256 WVertex ""])]) -> do
	Vk.Mm.write @nm @(Obj.List 256 WVertex "") dv bm' zeroBits vs
	beginSingleTimeCommands dv gq cp \cb ->
		Vk.Cmd.copyBuffer @'[ '[Obj.List 256 WVertex ""]] cb b' b
	f b
	where lns = HPList.Singleton . Obj.LengthList . fromIntegral $ V.length vs

createCommandBuffers ::
	forall mff sd scp a . (
	TList.Length (HPList.Dummies mff),
	HPList.FromList (HPList.Dummies mff) ) =>
	Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb . HPList.LL' (Vk.CBffr.C scb) mff -> IO a) -> IO a
createCommandBuffers dv cp f = Vk.CBffr.allocate dv allcInfo f
	where
	allcInfo :: Vk.CBffr.AllocateInfo 'Nothing scp (HPList.Dummies mff)
	allcInfo = Vk.CBffr.AllocateInfo {
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

type SyncObjects = SyncObjs

mainLoop :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs, HPList.HomoList '(slyt, BuffersMff MaxFramesInFlight) lyts) =>
	GlfwG.Win.W sw -> FramebufferResized -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(slyt, BuffersMff MaxFramesInFlight)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(slyt, BuffersMff MaxFramesInFlight)], '[WMeshPushConsts]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs -> HPList.PL MemoryCamera scms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg "scene-buffer" '[SceneObj MaxFramesInFlight])] ->
	HPList.PL (Vk.DscSet.D sds) lyts ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 WVertex ""] ->
	HPList.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> SyncObjects sos ->
	VertexNumber -> IO ()
mainLoop w rszd sfc pd qfis dv gq pq sc ex0 scivs rp lyt gpl cp drs fbs
	cmms scnm dss vb vbtri cbs sos vnsln =
	($ 0) . ($ Inf.cycle $ NE.fromList [0 .. maxFramesInFlight - 1]) . ($ ex0) $ fix
		\loop ex (ffn :~ ffns) fn -> Glfw.pollEvents >>
	step w rszd sfc pd qfis dv gq pq sc ex scivs rp lyt gpl cp drs fbs
		cmms scnm dss vb vbtri cbs sos vnsln ffn fn
		(\ex' -> loop ex' ffns ((fn + 1) `mod` (360 * frashRate))) >>
	Vk.Dvc.waitIdle dv

frashRate :: Num n => n
frashRate = 2

type VertexNumber = Word32

step :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs, HPList.HomoList '(slyt, BuffersMff MaxFramesInFlight) lyts) =>
	GlfwG.Win.W sw -> FramebufferResized -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(slyt, BuffersMff MaxFramesInFlight)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(slyt, BuffersMff MaxFramesInFlight)], '[WMeshPushConsts]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs -> HPList.PL MemoryCamera scms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg "scene-buffer" '[SceneObj MaxFramesInFlight])] ->
	HPList.PL (Vk.DscSet.D sds) lyts ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 WVertex ""] ->
	HPList.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> SyncObjects sos ->
	Word32 -> Int -> Int -> (Vk.Extent2d -> IO ()) -> IO ()
step w@(GlfwG.Win.W win) frszd sfc pd qfis dv gq pq sc ex scivs rp lyt gpl cp drs fbs
	cmms scnm dss vb vbtri cbs sos vnsln ffn fn loop = do
	catchAndRecreate w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs loop
		$ drawFrame dv gq pq sc ex rp lyt gpl fbs
			cmms scnm dss vb vbtri cbs sos vnsln ffn fn
	(Glfw.windowShouldClose win >>=) . flip bool (pure ()) $
		(checkFlag frszd >>=) . bool (loop ex) $
		loop =<< recreateAll
			w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs

catchAndRecreate :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QueueFamilyIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(s, BuffersMff MaxFramesInFlight)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(s, BuffersMff MaxFramesInFlight)], '[WMeshPushConsts]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs -> (Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just (); Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\() -> loop =<< recreateAll
		w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs

recreateAll :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QueueFamilyIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(slyt, BuffersMff MaxFramesInFlight)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(slyt, BuffersMff MaxFramesInFlight)], '[WMeshPushConsts]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateAll w@(GlfwG.Win.W win) sfc pd qfs dv gq sc scivs rp lyt gpl cp drs@(_, _, divw) fbs =
	waitFramebufferSize win >> Vk.Dvc.waitIdle dv >>
	recreateSwpch w sfc pd qfs dv sc >>= \ex ->
	ex <$ do
	Vk.Khr.Swpch.getImages dv sc >>= \i -> recreateImgVws dv i scivs
	recreateDptRsrcs pd dv gq cp ex drs
	recreateGraphicsPipeline dv ex rp lyt gpl
	recreateFramebuffers dv ex rp scivs divw fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize w = Glfw.getFramebufferSize w >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize w
	where zero = uncurry (||) . ((== 0) *** (== 0))

drawFrame ::
	forall sd ssc scfmt sr slyt sl sg sfs scmmbs ssm ssb lyts
	sm sb nm smtri sbtri nmtri scb ssos sds .
	(HPList.HomoList '(sl, BuffersMff MaxFramesInFlight) lyts ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P slyt '[ '(sl, BuffersMff MaxFramesInFlight)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt,	'[ '(sl, BuffersMff MaxFramesInFlight)], '[WMeshPushConsts]) ->
	HPList.PL Vk.Frmbffr.F sfs -> HPList.PL MemoryCamera scmmbs ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg "scene-buffer" '[SceneObj MaxFramesInFlight])] ->
	HPList.PL (Vk.DscSet.D sds) lyts ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 WVertex ""] ->
	HPList.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> SyncObjects ssos ->
	Word32 -> Int -> Int -> IO ()
drawFrame dv gq pq sc ex rp lyt gpl fbs cmms scnm dss vb vbtri cbs
	(SyncObjs iass rfss iffs) vnsln ffn fn =
	HPList.index iass ffn \(ias :: Vk.Semaphore.S sias) ->
	HPList.index rfss ffn \(rfs :: Vk.Semaphore.S srfs) ->
	HPList.index iffs ffn \(id &&& HPList.Singleton -> (iff, siff)) ->
--	HPList.index cmms ffn \(MemoryCamera cmm) -> do
	HPList.index cmms ffn \(MemoryVp cmm) -> do
	Vk.Mm.write @"camera-buffer" @CameraObj dv cmm zeroBits (cameraData ex)
	Vk.Mm.write @"scene-buffer" @(SceneObj MaxFramesInFlight) dv scnm zeroBits . (!! ffn)
		$ iterate (Nothing :) [Just $ sceneData fn]
	Vk.Fnc.waitForFs dv siff True Nothing
	iid <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dv sc maxBound (Just ias) Nothing
	Vk.Fnc.resetFs dv siff
	Vk.CBffr.reset cb zeroBits
	HPList.index fbs iid \fb -> recordCommandBuffer
		ex rp lyt gpl fb ds vb vbtri cb vnsln (fromIntegral ffn) fn
	Vk.Q.submit gq (HPList.Singleton . U4 $ submitInfo ias rfs) $ Just iff
	catchAndSerialize . Vk.Khr.queuePresent @'Nothing pq $ presentInfo rfs iid
	where
	submitInfo :: Vk.Semaphore.S ssi -> Vk.Semaphore.S ssr ->
		Vk.SubmitInfo 'Nothing '[ssi] '[scb] '[ssr]
	submitInfo ias rfs = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Singleton
			$ Vk.SemaphorePipelineStageFlags ias
				Vk.Ppl.StageColorAttachmentOutputBit,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
	presentInfo :: Vk.Semaphore.S ssr -> Word32 ->
		Vk.Khr.PresentInfo 'Nothing '[ssr] scfmt '[ssc]
	presentInfo rfs iid = Vk.Khr.PresentInfo {
		Vk.Khr.presentInfoNext = TMaybe.N,
		Vk.Khr.presentInfoWaitSemaphores = HPList.Singleton rfs,
		Vk.Khr.presentInfoSwapchainImageIndices = HPList.Singleton
			$ Vk.Khr.SwapchainImageIndex sc iid }
	HPList.Dummy cb = HPList.homoListIndex @'() cbs ffn
	ds = HPList.homoListIndex @'(sl, BuffersMff MaxFramesInFlight) dss ffn
	catchAndSerialize = (`catch`
		\(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

recordCommandBuffer ::
	forall sr slyt sg sdlyt sf sm sb nm smtri sbtri nmtri scb sds .
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P slyt '[ '(sdlyt, BuffersMff MaxFramesInFlight)] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt,	'[ '(sdlyt, BuffersMff MaxFramesInFlight)], '[WMeshPushConsts]) ->
	Vk.Frmbffr.F sf -> Vk.DscSet.D sds '(sdlyt, BuffersMff MaxFramesInFlight) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 WVertex ""] ->
	Vk.CBffr.C scb -> Word32 -> Word32 -> Int -> IO ()
recordCommandBuffer sce rp lyt gpl fb ds vb vbt cb vn ffn (fromIntegral -> fn) =
	Vk.CBffr.begin @'Nothing @'Nothing cb binfo $
	Vk.Cmd.beginRenderPass cb rpinfo Vk.Subpass.ContentsInline do
	ovb <- newIORef Nothing
	drawObject ovb cb ds RenderObject {
		renderObjectPipeline = gpl,
		renderObjectPipelineLayout = lyt,
		renderObjectMesh = vb, renderObjectMeshSize = vn,
		renderObjectTransformMatrix = model } ffn
	ovbtri <- newIORef Nothing
	for_ [- 20 .. 20] \x -> for_ [- 20 .. 20] \y ->
		drawObject ovbtri cb ds RenderObject {
			renderObjectPipeline = gpl,
			renderObjectPipelineLayout = lyt,
			renderObjectMesh = vbt, renderObjectMeshSize = 3,
			renderObjectTransformMatrix =
				trans x y `Glm.mat4Mul` scale } ffn
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

drawObject ::
	IORef (Maybe (Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""])) ->
	Vk.CBffr.C scb -> Vk.DscSet.D sds '(sdlyt, BuffersMff MaxFramesInFlight) ->
	RenderObject sg sl sdlyt sm sb nm -> Word32 -> IO ()
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
				. U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @"" vb
			writeIORef ovb $ Just vb
	Vk.Cmd.pushConstantsGraphics @'[ 'Vk.T.ShaderStageVertexBit] cb lyt
		$ HPList.Id (GStorable.W MeshPushConstants {
			meshPushConstantsData =
				Glm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
			meshPushConstantsRenderMatrix = model }) :** HPList.Nil
	Vk.Cmd.draw cb vn 1 0 0

data RenderObject sg sl sdlyt sm sb nm = RenderObject {
	renderObjectPipeline :: Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(sdlyt, BuffersMff MaxFramesInFlight)], '[WMeshPushConsts]),
	renderObjectPipelineLayout ::
		Vk.PplLyt.P sl '[ '(sdlyt, BuffersMff MaxFramesInFlight)] '[WMeshPushConsts],
	renderObjectMesh :: Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""],
	renderObjectMeshSize :: Word32,
	renderObjectTransformMatrix :: Glm.Mat4 }

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

cameraData :: Vk.Extent2d -> WViewProj
cameraData ex = GStorable.W $ CameraData (View view) (Proj $ projection ex)
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

shaderStages ::
	SpirV.S 'GlslVertexShader -> SpirV.S 'GlslFragmentShader ->
	HPList.PL (U5 Vk.Ppl.ShdrSt.CreateInfo) '[
		'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
		'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
shaderStages vs fs = U5 vertinfo :** U5 fraginfo :** HPList.Nil where
	vertinfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModule = (crInfo vs, nil), -- mdl vs,
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	fraginfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModule = (crInfo fs, nil), -- mdl fs,
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	crInfo cd = Vk.ShaderModule.CreateInfo {
			Vk.ShaderModule.createInfoNext = TMaybe.N,
			Vk.ShaderModule.createInfoFlags = zeroBits,
			Vk.ShaderModule.createInfoCode = cd }

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
