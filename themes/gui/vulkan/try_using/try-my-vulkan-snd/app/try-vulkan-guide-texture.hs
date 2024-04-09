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
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import qualified Gpu.Vulkan.Memory as Vk.Mem

import GHC.Generics
import GHC.TypeNats
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as KObj
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
import Foreign.Storable.Generic qualified as GStorable

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
import qualified Gpu.Vulkan.Device as Vk.Dvc.M
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Sfc
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as Vk.Khr.Sfc.Phd
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swpch
import qualified Gpu.Vulkan.Image as Vk.Img
import qualified Gpu.Vulkan.Image as Vk.Img.M
import qualified Gpu.Vulkan.ImageView as Vk.ImgVw
import qualified Gpu.Vulkan.Component as Vk.Component
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderModule
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.VertexInputState as Vk.Ppl.VtxIptSt
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
import qualified Gpu.Vulkan.Memory as Vk.Dvc.Mem
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.Cmd as Vk.Cmd
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as Vk.Ppl.DptStnSt
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DSLt
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPl
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSt

import qualified Codec.WavefrontObj.ReadFaceSimple as WvNew
import qualified Codec.WavefrontObj.ReadFaceSimple as WvRf
import Tools (readRgba8)

import Foreign.Ptr
import Foreign.Marshal.Array
import Data.Array
import Codec.Picture

import Gpu.Vulkan.Sampler as Vk.Smplr
import Gpu.Vulkan.Sampler as Vk.Smplr.M

import Options.Declarative (Flag, Def, Cmd, run_, get)
import Control.Monad.Trans
import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win

import Data.Bits.ToolsYj
import Data.Function.ToolsYj
import Data.Tuple.ToolsYj
import Data.Bool.ToolsYj
import Data.Maybe.ToolsYj
import Data.List.ToolsYj
import Data.IORef.ToolsYj
import Data.Ord.ToolsYj
import Data.TypeLevel.List qualified as TList

import Debug
import Data.List.Infinite (pattern (:~))
import Data.List.Infinite qualified as Inf
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.MonoTraversable (Element, olength)
import Data.Sequences (IsSequence)

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
withWindow fr a = GlfwG.init error $ GlfwG.Win.group $ (a =<<) . initWindow
	where
	initWindow :: GlfwG.Win.Group s () -> IO (GlfwG.Win.W s)
	initWindow g = do
		Right w <- do
			GlfwG.Win.hint noApi
			uncurryDup (GlfwG.Win.create' g ())
				sizeName Nothing Nothing
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
		Vk.applicationInfoApiVersion = Vk.apiVersion_1_1 }

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

body :: FilePath -> Natural ->
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Ist.I si -> IO ()
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
	strgBffrOstAlgn pd \(_ :: Proxy als) ->
	fromNat @alu @als mff \(_ :: Proxy mff) (_ :: Proxy mffn) ->
	createPplLyt @alu @_ @mffn d \dsl dslo dslt pl ->
	createGrPpl d ex rp pl \gp ->
	createFrmbffrs d ex rp scvs dv \fbs ->

	readVtcs mdlfp >>= \vns ->
--	createVtxBffr pd d gq cp vns \(vb, vnsln) ->
	let vnsln = fromIntegral $ V.length vns in
	createVertexBuffer pd d gq cp vns \vb ->
	createVertexBuffer pd d gq cp triangle \vbtri ->

	createVpBffrs @_ @_ @_ @mff pd d dsl dslo
		\dsls vpbs vpbms dslos odbs odbms ->
	createScnBffr pd d \snb snbm ->

	createTextureImage pd d gq cp "assets/lost_empire-RGBA.png" \timg ->
	createTextureImageView d timg \timgvw ->
--	Vk.CBffr.allocate @_ @mff d (cmdBffrInfo cp) \cbs ->
	createCommandBuffers d cp \cbs ->
	createSyncObjects d \sos ->

	createDescriptorPool d \dp ->
	allocateTextureDescriptorSets d dp dslt \dscstx ->
	writeTexture1 d dscstx timgvw \wtx ->
	Vk.DscSt.updateDs d (HPList.Singleton $ U5 wtx) HPList.Nil >>
	createDscSts d dp dsls vpbs dslos odbs snb \dss dsso ->

	mainLoop w fr sfc pd qfis d gq pq sc ex scvs rp pl gp cp drs fbs
		vpbms snbm dss odbms dsso dscstx vb vbtri cbs sos vnsln
	where
	fromNat :: forall alu als a . (KnownNat alu, KnownNat als) =>
		Natural -> (forall n n' . (
			TList.Length n, HPList.FromList n, HPList.RepM n,
			HPList.HomoList '() n, KnownNat n',
--			CreateVpBffrs alu als n' n ) =>
			CreateVpBffrs alu 256 n' n ) =>
			Proxy n -> Proxy n' -> a) -> a
	fromNat n f = ($ someNatVal n) \(SomeNat (pn' :: Proxy n')) ->
		tnum @alu @als @n' n \pn -> f pn pn'
	tnum :: forall alu als mff a .
		(KnownNat alu, KnownNat als, KnownNat mff) =>
		Natural -> (forall (n :: [()]) . (
			TList.Length n, HPList.FromList n, HPList.RepM n,
			HPList.HomoList '() n,
--			CreateVpBffrs alu als mff n ) =>
			CreateVpBffrs alu 256 mff n ) =>
			Proxy n -> a) -> a
	tnum 0 f = f (Proxy @'[])
	tnum n f = tnum @alu @als @mff (n - 1) $ f . scc
		where scc :: Proxy n -> Proxy ('() ': n); scc Proxy = Proxy

readVtcs :: FilePath -> IO (V.Vector WVertex)
readVtcs fp = either error pure
	=<< (V.map pn2v <$>) . WvNew.posTexNormal . WvRf.r <$> BS.readFile fp
	where
	pn2v = GStorable.W . posTxtNormalToVertex

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
		Vk.Dvc.createInfoNext = TMaybe.J drawParFeatures,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames = bool [] vldLayers debug,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures = Just def {
			Vk.Phd.featuresSamplerAnisotropy = True } }
	drawParFeatures :: Vk.Phd.ShaderDrawParametersFeatures 'Nothing
	drawParFeatures = Vk.Phd.ShaderDrawParametersFeatures {
		Vk.Phd.shaderDrawParametersFeaturesNext = TMaybe.N,
		Vk.Phd.shaderDrawParametersFeaturesShaderDrawParameters = True }

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

strgBffrOstAlgn ::
	Vk.Phd.P -> (forall a . KnownNat a => Proxy a -> IO b) -> IO b
strgBffrOstAlgn pd f = (\(SomeNat p) -> f p) . someNatVal . fromIntegral
	. Vk.Phd.limitsMinStorageBufferOffsetAlignment . Vk.Phd.propertiesLimits
	=<< Vk.Phd.getProperties pd

createDscStLyt :: forall alu mff sd a . Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DSLt.D s (DscStLytArg alu mff) -> IO a) -> IO a
createDscStLyt dv = Vk.DSLt.create dv info nil
	where
	info = Vk.DSLt.CreateInfo {
		Vk.DSLt.createInfoNext = TMaybe.N,
		Vk.DSLt.createInfoFlags = zeroBits,
		Vk.DSLt.createInfoBindings = vpbd :** snbd :** HPList.Nil }
	vpbd = Vk.DSLt.BindingBuffer {
		Vk.DSLt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DSLt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	snbd = Vk.DSLt.BindingBuffer {
		Vk.DSLt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBufferDynamic,
		Vk.DSLt.bindingBufferStageFlags =
			Vk.ShaderStageVertexBit .|. Vk.ShaderStageFragmentBit }

createDscStLytOd :: forall als sd a . Vk.Dvc.D sd ->
	(forall (s :: Type) . Vk.DSLt.D s (DscStLytArgOd als) -> IO a) -> IO a
createDscStLytOd dv = Vk.DSLt.create dv info nil
	where
	info = Vk.DSLt.CreateInfo {
		Vk.DSLt.createInfoNext = TMaybe.N,
		Vk.DSLt.createInfoFlags = zeroBits,
		Vk.DSLt.createInfoBindings = objd :** HPList.Nil }
	objd = Vk.DSLt.BindingBuffer {
		Vk.DSLt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
		Vk.DSLt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

type Buffers mffn alu = '[
	'Vk.DSLt.Buffer '[CameraObj alu], 'Vk.DSLt.Buffer '[SceneObj mffn alu] ]

type ObjDataBuffers = '[ 'Vk.DSLt.Buffer '[ObjDataList] ]

type CameraObj alu = Obj.Atom alu WViewProj 'Nothing
type SceneObj mffn alu = Obj.DynAtom mffn alu WScene 'Nothing

type ObjDataList = Obj.List 256 WObjData ""

createSingleTextureSetLayout :: Vk.Dvc.D sd ->
	(forall s .
		Vk.DSLt.D s '[
			'Vk.DSLt.Image '[
				'("texture", 'Vk.T.FormatR8g8b8a8Srgb) ] ] ->
		IO a) -> IO a
createSingleTextureSetLayout dv = Vk.DSLt.create dv layoutInfo nil where
	layoutInfo = Vk.DSLt.CreateInfo {
		Vk.DSLt.createInfoNext = TMaybe.N,
		Vk.DSLt.createInfoFlags = zeroBits,
		Vk.DSLt.createInfoBindings = textureBind :** HPList.Nil }
	textureBind = Vk.DSLt.BindingImage {
		Vk.DSLt.bindingImageDescriptorType =
			Vk.Dsc.TypeCombinedImageSampler,
		Vk.DSLt.bindingImageStageFlags =
			Vk.ShaderStageFragmentBit }

createPplLyt :: forall alu als mff sd a . Vk.Dvc.D sd -> (forall sl sdsl sdslo sdltx .
	Vk.DSLt.D sdsl (DscStLytArg alu mff) ->
	Vk.DSLt.D sdslo (DscStLytArgOd als) ->
	Vk.DSLt.D sdltx '[ 'Vk.DSLt.Image '[ '("texture", Vk.T.FormatR8g8b8a8Srgb)]] ->
	Vk.PplLyt.P sl
		'[	'(sdsl, DscStLytArg alu mff),
			'(sdslo, DscStLytArgOd als),
			'(sdltx, '[ 'Vk.DSLt.Image '[ '("texture", Vk.T.FormatR8g8b8a8Srgb)]]) ]
		'[WMeshPushConsts] -> IO a) -> IO a
createPplLyt dv f =
	createDscStLyt @_ @mff dv \dsl -> createDscStLytOd dv \dslo ->
	createSingleTextureSetLayout dv \dslyttx ->
	Vk.PplLyt.create dv (info dsl dslo dslyttx) nil $ f dsl dslo dslyttx
	where
	info :: Vk.DSLt.D s (DscStLytArg alu mff) ->
		Vk.DSLt.D so (DscStLytArgOd als) ->
		Vk.DSLt.D st '[ 'Vk.DSLt.Image '[ '("texture", Vk.T.FormatR8g8b8a8Srgb)]] ->
		Vk.PplLyt.CreateInfo 'Nothing
			'[	'(s, DscStLytArg alu mff),
				'(so, DscStLytArgOd als),
				'(st, '[ 'Vk.DSLt.Image '[ '("texture", Vk.T.FormatR8g8b8a8Srgb)]])
				]
			('Vk.PushConstant.Layout '[ WMeshPushConsts]
				'[ 'Vk.PushConstant.Range
					'[ 'Vk.T.ShaderStageVertexBit]
					'[WMeshPushConsts] ])
	info dsl dslo dslt = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts =
			U2 dsl :** U2 dslo :** U2 dslt :** HPList.Nil }

type Foo = '[ 'Vk.DSLt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)]]
type Bar sdslt = '(sdslt, Foo)

createGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[	'(sdsl, DscStLytArg alu mff),
			'(sdslo, DscStLytArgOd als),
			'(sdslt, '[ 'Vk.DSLt.Image '[ '("texture", Vk.T.FormatR8g8b8a8Srgb)]])
			]
		'[WMeshPushConsts] ->
	(forall sg . Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(	sl,
			'[	'(sdsl, DscStLytArg alu mff),
				'(sdslo, DscStLytArgOd als), Bar sdslt ],
			'[WMeshPushConsts] ) -> IO a) -> IO a
createGrPpl dv ex rp pl f = Vk.Ppl.Grph.createGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	\(HPList.Singleton (U3 p)) -> f p

recreateGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[ '(sdsl, DscStLytArg alu mff), '(sdslo, DscStLytArgOd als), Bar sdslt]
		'[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(	sl,
			'[	'(sdsl, DscStLytArg alu mff),
				'(sdslo, DscStLytArgOd als),
				Bar sdslt ],
			'[WMeshPushConsts] ) -> IO ()
recreateGrPpl dv ex rp pl p = Vk.Ppl.Grph.unsafeRecreateGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	(HPList.Singleton $ U3 p)

grPplInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[ '(sdsl, DscStLytArg alu mff), '(sdslo, DscStLytArgOd als), Bar sdslt ]
		'[WMeshPushConsts] ->
	Vk.Ppl.Grph.CreateInfo 'Nothing
		'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
		'(	'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)] ) 'Nothing
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'(	sl,
			'[	'(sdsl, DscStLytArg alu mff),
				'(sdslo, DscStLytArgOd als),
				Bar sdslt ],
			'[WMeshPushConsts] )
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

type DepthResources sb sm nm fmt sdiv = (
	Vk.Img.Binded sm sb nm fmt,
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)],
	Vk.ImgVw.I nm fmt sdiv )

imageInfo :: Vk.Extent2d ->
	Vk.Img.Tiling -> Vk.Img.UsageFlags -> Vk.Img.CreateInfo 'Nothing fmt
imageInfo ex tlng usg = Vk.Img.CreateInfo {
		Vk.Img.createInfoNext = TMaybe.N,
		Vk.Img.createInfoImageType = Vk.Img.Type2d,
		Vk.Img.createInfoExtent = Vk.Extent3d {
			Vk.extent3dWidth = Vk.extent2dWidth ex,
			Vk.extent3dHeight = Vk.extent2dHeight ex,
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

memInfo :: Vk.Mm.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
memInfo mt = Vk.Mm.AllocateInfo {
	Vk.Mm.allocateInfoNext = TMaybe.N,
	Vk.Mm.allocateInfoMemoryTypeIndex = mt }

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

transitionImageLayout :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Img.Binded sm si nm fmt -> Vk.Img.Layout -> Vk.Img.Layout -> IO ()
transitionImageLayout dv gq cp i ol nl = beginSingleTimeCommands dv gq cp \cb ->
	Vk.Cmd.pipelineBarrier cb
		sstg dstg zeroBits HPList.Nil HPList.Nil (HPList.Singleton $ U5 barrier)
	where
	barrier :: Vk.Img.MemoryBarrier 'Nothing sm si nm fmt
	barrier = Vk.Img.MemoryBarrier {
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
			case Vk.T.formatToValue @fmt of
				Vk.FormatD32SfloatS8Uint ->
					Vk.Img.AspectStencilBit
				Vk.FormatD24UnormS8Uint ->
					Vk.Img.AspectStencilBit
				_ -> zeroBits
		_ -> Vk.Img.AspectColorBit
	(sam, dam, sstg, dstg) = case (ol, nl) of
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

beginSingleTimeCommands :: forall sd sc a .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	(forall s . Vk.CBffr.C s -> IO a) -> IO a
beginSingleTimeCommands dv gq cp cmds =
	Vk.CBffr.allocate dv allocInfo \(cb :*. HPList.Nil) ->
	Vk.CBffr.begin @'Nothing @'Nothing cb beginInfo (cmds cb) <* do
	Vk.Q.submit gq (HPList.Singleton . U4 $ sminfo cb) Nothing
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
	sminfo :: forall s . Vk.CBffr.C s -> Vk.SubmitInfo 'Nothing '[] '[s] '[]
	sminfo cb = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

createFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	HPList.PL (Vk.ImgVw.I inm fmt) svs -> Vk.ImgVw.I dptnm dptfmt siv ->
	(forall sfs . RecreateFrmbffrs svs sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HPList.Nil _ f = f HPList.Nil
createFrmbffrs dv ex rp (v :** vs) dvw f =
	Vk.Frmbffr.create dv (frmbffrInfo ex rp v dvw) nil \fb ->
	createFrmbffrs dv ex rp vs dvw \fbs -> f (fb :** fbs)

class RecreateFrmbffrs (svs :: [Type]) (sfs :: [Type]) where
	recreateFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
		HPList.PL (Vk.ImgVw.I inm fmt) svs ->
		Vk.ImgVw.I dptnm dptfmt siv ->
		HPList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFrmbffrs '[] '[] where
	recreateFrmbffrs _ _ _ HPList.Nil _ HPList.Nil = pure ()

instance RecreateFrmbffrs svs sfs =>
	RecreateFrmbffrs (si ': svs) (sf ': sfs) where
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

type RecreateFramebuffers = RecreateFrmbffrs

class CreateVpBffrs alu als mff (n :: [()]) where
	createVpBffrs :: Vk.Phd.P -> Vk.Dvc.D sd ->
		Vk.DSLt.D sdsc (DscStLytArg alu mff) ->
		Vk.DSLt.D sdsco (DscStLytArgOd als) ->
		(forall dlas svp dlaso svpo . (
			HPList.FromList dlas, Vk.DscSt.DListFromMiddle dlas,
			HPList.FromList dlaso, Vk.DscSt.DListFromMiddle dlaso,
			Update alu als mff svp dlas svpo dlaso,
			HPList.HomoList '(sdsc, DscStLytArg alu mff) dlas,
			HPList.HomoList '(sdsco, DscStLytArgOd als) dlaso ) =>
			HPList.PL (U2 Vk.DSLt.D) dlas ->
			HPList.PL (BndVp alu bnmvp) svp ->
			HPList.PL (MemVp alu bnmvp) svp ->
			HPList.PL (U2 Vk.DSLt.D) dlaso ->
			HPList.PL (BndOd als bnmo) svpo ->
			HPList.PL (MemOd als bnmo) svpo -> IO a) -> IO a

instance CreateVpBffrs _alu _als _mff '[] where
	createVpBffrs _ _ _ _ f = f
		HPList.Nil HPList.Nil HPList.Nil
		HPList.Nil HPList.Nil HPList.Nil

instance (
	KnownNat alu, KnownNat als, KnownNat mff,
	CreateVpBffrs alu als mff n ) =>
	CreateVpBffrs alu als mff ('() ': n) where
	createVpBffrs pd dv l lo f =
		createVpBffr pd dv \v vm -> createOdBffr pd dv \o om ->
		createVpBffrs @_ @_ @_ @n pd dv l lo \ls vs vms los os oms -> f
			(U2 l :** ls) (BndVp v :** vs) (MemVp vm :** vms)
			(U2 lo :** los) (BndOd o :** os) (MemOd om :** oms)

createVpBffr :: KnownNat alu => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[AtmViewProj alu] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[AtmViewProj alu])] ->
	IO a) -> IO a
createVpBffr =
	createBffrAtm Vk.Bffr.UsageUniformBufferBit Vk.Mm.PropertyHostVisibleBit

createOdBffr :: KnownNat als => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[ListObjData als] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[ListObjData als])] ->
	IO a) -> IO a
createOdBffr pd dv = createBffrLst pd dv maxObjects
	Vk.Bffr.UsageStorageBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyDeviceLocalBit)

maxObjects :: Vk.Dvc.Size
maxObjects = 10000

createScnBffr :: (KnownNat alsn, KnownNat mff) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[AtmScene alsn mff] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[AtmScene alsn mff]) ] ->
		IO a) -> IO a
createScnBffr pd dv = createBffr pd dv
	(Obj.LengthDynAtom :** HPList.Nil)
	Vk.Bffr.UsageUniformBufferBit Vk.Mm.PropertyHostVisibleBit

type DscStLytArg alu mff = '[BufferViewProj alu, BufferSceneData alu mff]
type DscStLytArgOd als = '[ 'Vk.DSLt.Buffer '[ListObjData als]]
type BufferViewProj alu = 'Vk.DSLt.Buffer '[AtmViewProj alu]
type BufferSceneData alu mff = 'Vk.DSLt.Buffer '[AtmScene alu mff]

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

type BindedCamera = BndVp 256 "camera-buffer"
type BindedObjData = BndOd 256 "object-data-buffer"
type MemoryCamera alu = MemVp alu "camera-buffer"
type MemoryObjData = MemOd 256 "object-data-buffer"

createBuffer :: forall objs nm sd a . (
	Obj.SizeAlignmentList objs, forall s . SizeAlignmentAll s nm objs, Obj.WholeAlign objs ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> HPList.PL Obj.Length objs ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm objs ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm objs)] -> IO a) ->
	IO a
createBuffer pd dv lns usg prs f =
	Vk.Bffr.create dv (bufferInfo lns usg) nil \b ->
	Vk.Bffr.getMemoryRequirements dv b >>= \rs ->
	findMemoryType pd (Vk.Mm.M.requirementsMemoryTypeBits rs) prs >>= \mt ->
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
				Vk.DscPl.sizeDescriptorCount = 10 },
			Vk.DscPl.Size {
				Vk.DscPl.sizeType =
					Vk.Dsc.TypeStorageBuffer,
				Vk.DscPl.sizeDescriptorCount = 10 },
			Vk.DscPl.Size {
				Vk.DscPl.sizeType =
					Vk.Dsc.TypeCombinedImageSampler,
				Vk.DscPl.sizeDescriptorCount = 10 } ] }

createDscSts :: forall sd sp
	sls bnmvp smsbs slso bnmod smsbso smsn sbsn bnmsn alu als mff a . (
	HPList.FromList sls, Vk.DscSt.DListFromMiddle sls,
	HPList.FromList slso, Vk.DscSt.DListFromMiddle slso,
	Update alu als mff smsbs sls smsbso slso ) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	HPList.PL (U2 Vk.DSLt.D) sls -> HPList.PL (BndVp alu bnmvp) smsbs ->
	HPList.PL (U2 Vk.DSLt.D) slso -> HPList.PL (BndOd als bnmod) smsbso ->
	Vk.Bffr.Binded smsn sbsn bnmsn '[AtmScene alu mff] ->
	(forall sds sdso .
		HPList.PL (Vk.DscSt.D sds) sls ->
		HPList.PL (Vk.DscSt.D sdso) slso -> IO a) -> IO a
createDscSts dv dp dls bvps dlos bods snb f =
	Vk.DscSt.allocateDs dv (info dls) \dss ->
	Vk.DscSt.allocateDs dv (info dlos) \dsos ->
	update dv dss bvps dsos bods snb >> f dss dsos
	where
	info :: HPList.PL (U2 Vk.DSLt.D) s -> Vk.DscSt.AllocateInfo Nothing sp s
	info l = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = l }

allocateTextureDescriptorSets :: forall slyt foo sd sp a .
	Default (HPList.PL (HPList.PL KObj.Length)
		(Vk.DSLt.BindingTypeListBufferOnlyDynamics foo)) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.DSLt.D slyt foo ->
	(forall sds . Vk.DscSt.D sds '(slyt, foo) -> IO a) -> IO a
allocateTextureDescriptorSets dv dscpl lyt f =
	Vk.DscSt.allocateDs dv Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dscpl,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 lyt
		} \(HPList.Singleton dscs) -> f dscs

class Update alu als mff smsbsvp slbtss smsbso slbtsso where
	update :: Vk.Dvc.D sd ->
		HPList.PL (Vk.DscSt.D sds) slbtss ->
		HPList.PL (BndVp alu bnmvp) smsbsvp ->
		HPList.PL (Vk.DscSt.D sdsod) slbtsso ->
		HPList.PL (BndOd als bnmod) smsbso ->
		Vk.Bffr.Binded smsn sbsn bnmsn '[AtmScene alu mff] -> IO ()

data BndVp alvp bnmvp smsb where
	BndVp :: Vk.Bffr.Binded sm sb bnmvp '[AtmViewProj alvp] ->
		BndVp alvp bnmvp '(sm, sb)

data MemVp alvp bnmvp smsb where
	MemVp :: Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.BufferArg bnmvp '[AtmViewProj alvp])] ->
		MemVp alvp bnmvp '(sm, sb)

data BndOd als bnmod smsb where
	BndOd :: Vk.Bffr.Binded sm sb bnmod '[ListObjData als] ->
		BndOd als bnmod '(sm, sb)

data MemOd als bnmod smsb where
	MemOd :: Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnmod '[ListObjData als])] ->
		MemOd als bnmod '(sm, sb)

type AtmViewProj alu = Obj.Atom alu WViewProj 'Nothing
type AtmScene alu mff = Obj.DynAtom mff alu WScene 'Nothing
type ListObjData als = Obj.List als WObjData ""


instance Update _alu _als _mff '[] '[] '[] '[] where
	update _ HPList.Nil HPList.Nil HPList.Nil HPList.Nil _ = pure ()

instance (
	KnownNat alu, KnownNat als, KnownNat mff,
	Vk.DscSt.BindingAndArrayElemBuffer bts '[AtmViewProj alu] 0,
	Vk.DscSt.BindingAndArrayElemBuffer bts '[AtmScene alu mff] 0,
	Vk.DscSt.BindingAndArrayElemBuffer btso '[ListObjData als] 0,
	Vk.DscSt.UpdateDynamicLength bts '[AtmViewProj alu],
	Vk.DscSt.UpdateDynamicLength bts '[AtmScene alu mff],
	Vk.DscSt.UpdateDynamicLength btso '[ListObjData als],
	Update alu als mff smsbsvp slbtss smsbso slbtsso) =>
	Update alu als mff
		(smsbvp ': smsbsvp) ('(sdsl, bts) ': slbtss)
		(smsbo ': smsbso) ('(slyto, btso) ': slbtsso)
		where

	update dv
		(ds :** dss) (BndVp bvp :** bvps)
		(dsod :** dssod) (BndOd bod :** bods) scnb = do
		Vk.DscSt.updateDs dv (
			U5 (dscWrite @(AtmViewProj alu)
				ds bvp Vk.Dsc.TypeUniformBuffer) :**
			U5 (dscWrite @(AtmScene alu mff)
				ds scnb Vk.Dsc.TypeUniformBufferDynamic) :**
			U5 (dscWrite @(ListObjData als)
				dsod bod Vk.Dsc.TypeStorageBuffer) :**
			HPList.Nil ) HPList.Nil
		update dv dss bvps dssod bods scnb

dscWrite :: forall obj sds slbts sm sb nm objs . (
	Obj.OffsetRange obj objs, Show (HPList.PL Obj.Length objs) ) =>
	Vk.DscSt.D sds slbts -> Vk.Bffr.Binded sm sb nm objs -> Vk.Dsc.Type ->
	Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgBuffer '[ '(sm, sb, nm, obj)]) 0
dscWrite ds b tp = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = tp,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos
		. HPList.Singleton . U4 $ Vk.Dsc.BufferInfo b }

class UpdateOld cmbs lyts odbs lytods where
	updateOld :: Vk.Dvc.D sd ->
		HPList.PL (Vk.DscSt.D sds) lyts -> HPList.PL BindedCamera cmbs ->
		HPList.PL (Vk.DscSt.D sdso) lytods -> HPList.PL BindedObjData odbs ->
		Vk.Bffr.Binded ssb ssm "scene-buffer" '[SceneObj 2 256] -> IO ()

instance UpdateOld '[] '[] '[] '[] where updateOld _ HPList.Nil HPList.Nil HPList.Nil HPList.Nil _ = pure ()

instance (
	Vk.DscSt.BindingAndArrayElemBuffer
		(TIndex.I1_2 '(slyt, bs))
		'[CameraObj 256] 0,
	Vk.DscSt.BindingAndArrayElemBuffer
		(TIndex.I1_2 '(slyt, bs))
		'[SceneObj 2 256] 0,
	Vk.DscSt.BindingAndArrayElemBuffer bods '[ObjDataList] 0,
	Vk.DscSt.UpdateDynamicLength
		(TIndex.I1_2 '(slyt, bs))
		'[CameraObj 256],
	Vk.DscSt.UpdateDynamicLength
		(TIndex.I1_2 '(slyt, bs))
		'[SceneObj 2 256],
	Vk.DscSt.UpdateDynamicLength bods '[ObjDataList],
	UpdateOld cmbs lyts odbs lytods ) =>
	UpdateOld (cmb ': cmbs) ('(slyt, bs) ': lyts)
		(odb ': odbs) ('(slytod, bods) ': lytods) where
	updateOld dv (dscs :** dscss) (BndVp cmb :** cmbs)
		(dscsod :** dscsods) (BndOd odb :** odbs) scnb = do
		Vk.DscSt.updateDs dv (
			U5 (descriptorWrite @(CameraObj 256)
				dscs cmb Vk.Dsc.TypeUniformBuffer) :**
			U5 (descriptorWrite @(SceneObj 2 256)
				dscs scnb Vk.Dsc.TypeUniformBufferDynamic) :**
			U5 (descriptorWrite @ObjDataList
				dscsod odb Vk.Dsc.TypeStorageBuffer) :**
			HPList.Nil )
			HPList.Nil
		updateOld @_ @_ @odbs @lytods dv dscss cmbs dscsods odbs scnb

descriptorWrite :: forall obj slbts sb sm nm objs sds . (
	Show (HPList.PL Obj.Length objs), Obj.OffsetRange obj objs ) =>
	Vk.DscSt.D sds slbts -> Vk.Bffr.Binded sm sb nm objs ->
	Vk.Dsc.Type -> Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgBuffer '[ '(sm, sb, nm, obj)]) 0
descriptorWrite dscs ub tp = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N,
	Vk.DscSt.writeDstSet = dscs,
	Vk.DscSt.writeDescriptorType = tp,
	Vk.DscSt.writeSources =
		Vk.DscSt.BufferInfos . HPList.Singleton . U4 $ Vk.Dsc.BufferInfo ub }

writeTexture1 ::
	Vk.Dvc.D sd -> Vk.DscSt.D sds lyt -> Vk.ImgVw.I "texture" ifmt siv -> (
		forall ss . 
		Vk.DscSt.Write 'Nothing sds lyt
			('Vk.DscSt.WriteSourcesArgImage '[ '(ss, "texture", ifmt, siv)]) 0 ->
		IO a) -> IO a
writeTexture1 dv dscs tiv f =
	createTextureSampler dv \smplr ->
	f $ writeDescriptorImage
		Vk.Dsc.TypeCombinedImageSampler dscs
		(textureImageBufferInfo tiv smplr)

writeDescriptorImage :: Vk.Dsc.Type ->
	Vk.DscSt.D sds slbts -> Vk.Dsc.ImageInfo ss nm fmt si ->
	Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgImage '[ '(ss, nm, fmt, si)]) 0
writeDescriptorImage tp dscs ii = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N,
	Vk.DscSt.writeDstSet = dscs,
	Vk.DscSt.writeDescriptorType = tp,
	Vk.DscSt.writeSources =
		Vk.DscSt.ImageInfos . HPList.Singleton $ U4 ii }

textureImageBufferInfo :: Vk.ImgVw.I nm fmt si ->
	Vk.Smplr.S ss -> Vk.Dsc.ImageInfo ss nm fmt si
textureImageBufferInfo tiv tsmp = Vk.Dsc.ImageInfo {
	Vk.Dsc.imageInfoImageLayout =
		Vk.Img.LayoutShaderReadOnlyOptimal,
	Vk.Dsc.imageInfoImageView = tiv,
	Vk.Dsc.imageInfoSampler = tsmp }

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
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb . HPList.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> IO a) -> IO a
createCommandBuffers dv cp f = Vk.CBffr.allocate dv allcInfo f
	where
	allcInfo :: Vk.CBffr.AllocateInfo 'Nothing scp (HPList.Dummies MaxFramesInFlight)
	allcInfo = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cp,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

type MaxFramesInFlight = 2

createSyncObjects ::
	Vk.Dvc.D sd -> (forall ssos . SyncObjects ssos -> IO a ) -> IO a
createSyncObjects dv f =
	HPList.replicateM maxFramesInFlight
		(Vk.Semaphore.create @'Nothing dv def nil) \iass ->
	HPList.replicateM maxFramesInFlight
		(Vk.Semaphore.create @'Nothing dv def nil) \rfss ->
	HPList.replicateM maxFramesInFlight
		(Vk.Fnc.create @'Nothing dv inf nil) \iffs ->
	f $ SyncObjects iass rfss iffs
	where inf = def { Vk.Fnc.createInfoFlags = Vk.Fnc.CreateSignaledBit }

data SyncObjects (ssos :: ([Type], [Type], [Type])) where
	SyncObjects :: {
		_imageAvailableSemaphores :: HPList.PL Vk.Semaphore.S siass,
		_renderFinishedSemaphores :: HPList.PL Vk.Semaphore.S srfss,
		_inFlightFences :: HPList.PL Vk.Fnc.F sffs } ->
		SyncObjects '(siass, srfss, siffs)

mainLoop :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs,
	HPList.HomoList '(slyt, Buffers mffn alu) lyts,
	HPList.HomoList '(slytod, ObjDataBuffers) lytods,
	HPList.HomoList '() mff, KnownNat mffn, KnownNat alu
	) =>
	GlfwG.Win.W sw -> FramebufferResized -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[ '(slyt, Buffers mffn alu), '(slytod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[ '(slyt, Buffers mffn alu), '(slytod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConsts]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs ->
	HPList.PL (MemoryCamera alu) scms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg "scene-buffer" '[SceneObj mffn alu])] ->
	HPList.PL (Vk.DscSt.D sds) lyts ->
	HPList.PL MemoryObjData sods ->
	HPList.PL (Vk.DscSt.D sds') lytods ->
	Vk.DscSt.D sds'' '(sfoo, Foo) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 WVertex ""] ->
	HPList.LL (Vk.CBffr.C scb) mff -> SyncObjects sos ->
	VertexNumber -> IO ()
mainLoop w rszd sfc pd qfis dv gq pq sc ex0 scivs rp lyt gpl cp drs fbs
	cmms scnm dss odms dssod dstx vb vbtri cbs sos vnsln =
	($ 0) . ($ Inf.cycle $ NE.fromList [0 .. maxFramesInFlight - 1]) . ($ ex0) $ fix
		\loop ex (ffn :~ ffns) fn -> Glfw.pollEvents >>
	step w rszd sfc pd qfis dv gq pq sc ex scivs rp lyt gpl cp drs fbs
		cmms scnm dss odms dssod dstx vb vbtri cbs sos vnsln ffn fn
		(\ex' -> loop ex' ffns ((fn + 1) `mod` (3600 * frashRate))) >>
	Vk.Dvc.waitIdle dv

type VertexNumber = Word32

frashRate :: Num n => n
frashRate = 2

step :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs,
	HPList.HomoList '(slyt, Buffers mffn alu) lyts,
	HPList.HomoList '(slytod, ObjDataBuffers) lytods,
	HPList.HomoList '() mff, KnownNat mffn, KnownNat alu
	) =>
	GlfwG.Win.W sw -> FramebufferResized -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[ '(slyt, Buffers mffn alu), '(slytod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[ '(slyt, Buffers mffn alu), '(slytod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConsts]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs -> HPList.PL (MemoryCamera alu) scms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg "scene-buffer" '[SceneObj mffn alu])] ->
	HPList.PL (Vk.DscSt.D sds) lyts ->
	HPList.PL MemoryObjData sods ->
	HPList.PL (Vk.DscSt.D sds') lytods ->
	Vk.DscSt.D sds'' '(sfoo, Foo) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 WVertex ""] ->
	HPList.LL (Vk.CBffr.C scb) mff -> SyncObjects sos ->
	Word32 -> Int -> Int -> (Vk.Extent2d -> IO ()) -> IO ()
step w@(GlfwG.Win.W win) frszd sfc pd qfis dv gq pq sc ex scivs rp lyt gpl cp drs fbs
	cmms scnm dss odms dssod dstx vb vbtri cbs sos vnsln ffn fn loop = do
	catchAndRecreate w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs loop
		$ drawFrame dv gq pq sc ex rp lyt gpl fbs
			cmms scnm dss odms dssod dstx vb vbtri cbs sos vnsln ffn fn
	(Glfw.windowShouldClose win >>=) . flip bool (pure ()) $
		(checkFlag frszd >>=) . bool (loop ex) $
		loop =<< recreateAll
			w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs

catchAndRecreate :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QueueFamilyIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[ '(s, Buffers mffn alu), '(sod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[ '(s, Buffers mffn alu), '(sod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConsts]) ->
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
	Vk.PplLyt.P sl
		'[ '(slyt, Buffers mffn alu), '(slytod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[ '(slyt, Buffers mffn alu), '(slytod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConsts]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateAll w@(GlfwG.Win.W win) sfc pd qfs dv gq sc scivs rp lyt gpl cp drs@(_, _, divw) fbs =
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

drawFrame ::
	forall sd ssc scfmt sr slyt sl slod sfoo sg sfs scmmbs ssm ssb lyts
	sm sb nm smtri sbtri nmtri scb ssos sods lytods sds sds' sds'' mff mffn alu .  (
	HPList.HomoList '(sl, Buffers mffn alu) lyts,
	HPList.HomoList '(slod, ObjDataBuffers) lytods,
	HPList.HomoList '() mff, KnownNat mffn, KnownNat alu
	) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P slyt
		'[ '(sl, Buffers mffn alu), '(slod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(slyt,	'[ '(sl, Buffers mffn alu), '(slod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConsts]) ->
	HPList.PL Vk.Frmbffr.F sfs -> HPList.PL (MemoryCamera alu) scmmbs ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg "scene-buffer" '[SceneObj mffn alu])] ->
	HPList.PL (Vk.DscSt.D sds) lyts ->
	HPList.PL MemoryObjData sods ->
	HPList.PL (Vk.DscSt.D sds') lytods ->
	Vk.DscSt.D sds'' '(sfoo, Foo) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 WVertex ""] ->
	HPList.LL (Vk.CBffr.C scb) mff -> SyncObjects ssos ->
	Word32 -> Int -> Int -> IO ()
drawFrame dv gq pq sc ex rp lyt gpl fbs cmms scnm dss odms dssod dstx vb vbtri cbs
	(SyncObjects iass rfss iffs) vnsln ffn fn =
	HPList.index iass ffn \(ias :: Vk.Semaphore.S sias) ->
	HPList.index rfss ffn \(rfs :: Vk.Semaphore.S srfs) ->
	HPList.index iffs ffn \(id &&& HPList.Singleton -> (iff, siff)) ->
	HPList.index cmms ffn \(MemVp cmm) ->
	HPList.index odms ffn \(MemOd odm) -> do
	Vk.Mm.write @"camera-buffer" @(CameraObj alu) dv cmm zeroBits (cameraData ex)
	Vk.Mm.write @"scene-buffer" @(SceneObj mffn alu) dv scnm zeroBits . (!! ffn)
		$ iterate (Nothing :) [Just $ sceneData fn]
	Vk.Mm.write @"object-data-buffer" @ObjDataList dv odm zeroBits . map (GStorable.W . ObjData) $
		model (fromIntegral fn) : [ objectMatrix x y | x <- [- 20 .. 20], y <- [- 20 .. 20] ]
	Vk.Fnc.waitForFs dv siff True Nothing
	iid <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dv sc maxBound (Just ias) Nothing
	Vk.Fnc.resetFs dv siff
	Vk.CBffr.reset cb zeroBits
	HPList.index fbs iid \fb -> recordCommandBuffer
		ex rp lyt gpl fb ds dsod dstx vb vbtri cb vnsln (fromIntegral ffn) fn
	Vk.Q.submit gq (HPList.Singleton . U4 $ sminfo ias rfs) $ Just iff
	catchAndSerialize . Vk.Khr.queuePresent @'Nothing pq $ presentInfo rfs iid
	where
	sminfo :: Vk.Semaphore.S ssi -> Vk.Semaphore.S ssr ->
		Vk.SubmitInfo 'Nothing '[ssi] '[scb] '[ssr]
	sminfo ias rfs = Vk.SubmitInfo {
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
	ds = HPList.homoListIndex @'(sl, Buffers mffn alu) dss ffn
	dsod = HPList.homoListIndex @'(slod, ObjDataBuffers) dssod ffn
	catchAndSerialize = (`catch`
		\(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

recordCommandBuffer ::
	forall sr slyt sg sdlyt sdlytod sfoo sf sm sb nm smtri sbtri nmtri scb sds sds' sds'' mffn alu .
	KnownNat alu =>
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P slyt
		'[ '(sdlyt, Buffers mffn alu), '(sdlytod, ObjDataBuffers), '(sfoo, Foo)]
		'[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(slyt,	'[ '(sdlyt, Buffers mffn alu), '(sdlytod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConsts]) ->
	Vk.Frmbffr.F sf ->
	Vk.DscSt.D sds '(sdlyt, Buffers mffn alu) ->
	Vk.DscSt.D sds' '(sdlytod, ObjDataBuffers) ->
	Vk.DscSt.D sds'' '(sfoo, Foo) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 WVertex ""] ->
	Vk.CBffr.C scb -> Word32 -> Word32 -> Int -> IO ()
recordCommandBuffer sce rp lyt gpl fb ds dsod dstx vb vbt cb vn ffn (fromIntegral -> fn) =
	Vk.CBffr.begin @'Nothing @'Nothing cb binfo $
	Vk.Cmd.beginRenderPass cb rpinfo Vk.Subpass.ContentsInline do
	ovb <- newIORef Nothing
	drawObject ovb cb ds dsod dstx RenderObject {
		renderObjectPipeline = gpl,
		renderObjectPipelineLayout = lyt,
		renderObjectMesh = vb, renderObjectMeshSize = vn,
		renderObjectTransformMatrix = model fn } ffn 0
	ovbtri <- newIORef Nothing
	for_ [- 20 .. 20] \x -> for_ [- 20 .. 20] \y ->
		drawObject ovbtri cb ds dsod dstx RenderObject {
			renderObjectPipeline = gpl,
			renderObjectPipelineLayout = lyt,
			renderObjectMesh = vbt, renderObjectMeshSize = 3,
			renderObjectTransformMatrix =
				trans x y `Glm.mat4Mul` scale } ffn
			(((round x + 20) * 41) + (round y + 20) + 1)
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
--				(fromJust $ rgbaDouble 0 0 blue 1) :**
				(fromJust $ rgbaDouble 0 0 0 1) :**
			Vk.ClearValueDepthStencil
				(Vk.ClearDepthStencilValue 1 0) :** HPList.Nil }

model :: Float -> Glm.Mat4
model fn = Glm.rotate
	Glm.mat4Identity (fn / 10 * Glm.rad 1) (Glm.Vec3 $ 0 :. 1 :. 0 :. NilL)

objectMatrix :: Float -> Float -> Glm.Mat4
objectMatrix x y = trans x y `Glm.mat4Mul` scale

trans :: Float -> Float -> Glm.Mat4
trans x y = Glm.translate Glm.mat4Identity (Glm.Vec3 $ x :. 0 :. y :. NilL)

scale :: Glm.Mat4
scale = Glm.scale Glm.mat4Identity (Glm.Vec3 $ 0.2 :. 0.2 :. 0.2 :. NilL)

drawObject :: KnownNat alu =>
	IORef (Maybe (Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""])) ->
	Vk.CBffr.C scb ->
	Vk.DscSt.D sds '(sdlyt, Buffers mffn alu) ->
	Vk.DscSt.D sds' '(sdlytod, ObjDataBuffers) ->
	Vk.DscSt.D sds'' '(sfoo, Foo) ->
	RenderObject sg sl sdlyt sdlytod sfoo sm sb nm mffn alu -> Word32 -> Word32 -> IO ()
drawObject ovb cb0 ds dsod dstx RenderObject {
	renderObjectPipeline = gpl,
	renderObjectPipelineLayout = lyt,
	renderObjectMesh = vb, renderObjectMeshSize = vn,
	renderObjectTransformMatrix = mdl } ffn i =
	Vk.Cmd.bindPipelineGraphics cb0 Vk.Ppl.BindPointGraphics gpl \cb -> do
	Vk.Cmd.bindDescriptorSetsGraphics cb Vk.Ppl.BindPointGraphics lyt
		(U2 ds :** U2 dsod :** U2 dstx :** HPList.Nil) $
		(HPList.Nil :** (Vk.Cmd.DynamicIndex ffn :** HPList.Nil) :** HPList.Nil) :**
		(HPList.Nil :** HPList.Nil) :** (HPList.Nil :** HPList.Nil) :** HPList.Nil
	readIORef ovb >>= \case
		Just o | vb == o -> pure ()
		_ -> do	Vk.Cmd.bindVertexBuffers cb . HPList.Singleton
				. U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @"" vb
			writeIORef ovb $ Just vb
	Vk.Cmd.pushConstantsGraphics @'[ 'Vk.T.ShaderStageVertexBit] cb lyt
		$ HPList.Id (GStorable.W MeshPushConstants {
			meshPushConstantsData =
				Glm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
			meshPushConstantsRenderMatrix = mdl }) :** HPList.Nil
	Vk.Cmd.draw cb vn 1 0 i

data RenderObject sg sl sdlyt sdlytod sfoo sm sb nm mffn alu = RenderObject {
	renderObjectPipeline :: Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[ '(sdlyt, Buffers mffn alu), '(sdlytod, ObjDataBuffers), '(sfoo, Foo)],
			'[WMeshPushConsts]),
	renderObjectPipelineLayout ::
		Vk.PplLyt.P sl
			'[ '(sdlyt, Buffers mffn alu), '(sdlytod, ObjDataBuffers), '(sfoo, Foo)]
			'[WMeshPushConsts],
	renderObjectMesh :: Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""],
	renderObjectMeshSize :: Word32,
	renderObjectTransformMatrix :: Glm.Mat4 }

-- VERTEX

type WVertex = GStorable.W Vertex

data Vertex = Vtx {
	vtxPos :: Position,
	vtxNormal :: Normal,
	vtxColor :: Color,
	vtxUv :: Uv } deriving (Show, Generic)

newtype Position = Position Glm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Normal = Normal Glm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Color = Color Glm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Uv = Uv Glm.Vec2
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

instance Str.G.G Vertex

instance Storable Vertex where
	sizeOf = Str.G.gSizeOf; alignment = Str.G.gAlignment
	peek = Str.G.gPeek; poke = Str.G.gPoke

posTxtNormalToVertex :: GStorable.W (
	GStorable.W WvNew.Position,
	GStorable.W WvNew.TexCoord,
	GStorable.W WvNew.Normal ) -> Vertex
posTxtNormalToVertex (GStorable.W ((,,)
	(GStorable.W (WvNew.Position x y z)) (GStorable.W (WvNew.TexCoord p q)) (GStorable.W (WvNew.Normal v w u)))) =
	Vtx {
		vtxPos = Position . Glm.Vec3 $ x :. y :. z :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ v :. w :. u :. NilL,
		vtxColor = Color . Glm.Vec3 $ v :. w :. u :. NilL,
		vtxUv = Uv . Glm.Vec2 $ p :. (1 - q) :. NilL }

triangle :: V.Vector WVertex
triangle = V.fromList $ GStorable.W <$> [
	Vtx {
		vtxPos = Position . Glm.Vec3 $ 1 :. 1 :. 0.5 :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vtxColor = Color . Glm.Vec3 $ 0 :. 1 :. 0 :. NilL,
		vtxUv = Uv . Glm.Vec2 $ 0 :. 0 :. NilL },
	Vtx {
		vtxPos = Position . Glm.Vec3 $ (- 1) :. 1 :. 0.5 :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vtxColor = Color . Glm.Vec3 $ 0 :. 1 :. 0 :. NilL,
		vtxUv = Uv . Glm.Vec2 $ 0 :. 0 :. NilL },
	Vtx {
		vtxPos = Position . Glm.Vec3 $ 0 :. (- 1) :. 0.5 :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vtxColor = Color . Glm.Vec3 $ 0 :. 1 :. 0 :. NilL,
		vtxUv = Uv . Glm.Vec2 $ 0 :. 0 :. NilL } ]

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
	(Glm.Vec3 $ 0 :. 20 :. 20 :. NilL)
	(Glm.Vec3 $ 0 :. 10 :. 0 :. NilL)
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

-- OBJECT DATA

type WObjData = GStorable.W ObjData

newtype ObjData = ObjData {
	objectDataModelMatrix :: Glm.Mat4 } deriving (Show, Generic)

instance Storable ObjData where
	sizeOf = Str.G.gSizeOf; alignment = Str.G.gAlignment
	peek = Str.G.gPeek; poke = Str.G.gPoke

instance Str.G.G ObjData
	
-- OTHER TYPES

-- TEXTURE

createTextureImage ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> FilePath -> (
		forall si sm .
		Vk.Img.Binded sm si nm 'Vk.T.FormatR8g8b8a8Srgb -> IO a ) ->
	IO a
createTextureImage pd dv gq cp fp f =
	readRgba8 fp >>= \img ->
	print (V.length $ imageData img) >>
	let	wdt, hgt :: Integral n => n
		wdt = fromIntegral $ imageWidth img
		hgt = fromIntegral $ imageHeight img in
	createImage' @_ @'Vk.T.FormatR8g8b8a8Srgb
		pd dv wdt hgt Vk.Img.TilingOptimal
		(Vk.Img.UsageTransferDstBit .|.  Vk.Img.UsageSampledBit)
		Vk.Mm.PropertyDeviceLocalBit \tximg _txmem -> do
	createBufferImage @MyImage @_ pd dv (wdt, wdt, hgt, 1)
		Vk.Bffr.UsageTransferSrcBit
		(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)
		\(sb :: Vk.Bffr.Binded
			sm sb "texture-buffer" '[ Obj.Image 1 a inm]) sbm ->
		Vk.Dvc.Mem.write @"texture-buffer" @(Obj.Image 1 MyImage inm)
			dv sbm zeroBits (MyImage img) >>
		print sb >>
		transitionImageLayout dv gq cp tximg Vk.Img.LayoutUndefined
			Vk.Img.LayoutTransferDstOptimal >>
		copyBufferToImage dv gq cp sb tximg wdt hgt >>
		transitionImageLayout dv gq cp tximg
			Vk.Img.LayoutTransferDstOptimal
			Vk.Img.LayoutShaderReadOnlyOptimal
	f tximg

createTextureImageView :: Vk.T.FormatToValue ifmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	(forall siv . Vk.ImgVw.I nm ifmt siv -> IO a) -> IO a
createTextureImageView dv timg f =
	Vk.ImgVw.create dv (textureImageViewCreateInfo timg) nil f

textureImageViewCreateInfo ::
	Vk.Img.Binded sm si nm ifmt -> Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt ifmt
textureImageViewCreateInfo timg = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = zeroBits,
	Vk.ImgVw.createInfoImage = timg,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = components,
	Vk.ImgVw.createInfoSubresourceRange = srr }
	where
	components = Vk.Component.Mapping {
		Vk.Component.mappingR = def, Vk.Component.mappingG = def,
		Vk.Component.mappingB = def, Vk.Component.mappingA = def }
	srr = Vk.Img.M.SubresourceRange {
		Vk.Img.M.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.M.subresourceRangeBaseMipLevel = 0,
		Vk.Img.M.subresourceRangeLevelCount = 1,
		Vk.Img.M.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.M.subresourceRangeLayerCount = 1 }

createTextureSampler ::
	Vk.Dvc.D sd -> (forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTextureSampler dv = Vk.Smplr.create dv textureSamplerCreateInfo nil

textureSamplerCreateInfo :: Vk.Smplr.M.CreateInfo 'Nothing
textureSamplerCreateInfo = Vk.Smplr.M.CreateInfo {
	Vk.Smplr.M.createInfoNext = TMaybe.N,
	Vk.Smplr.M.createInfoFlags = zeroBits,
	Vk.Smplr.M.createInfoMagFilter = Vk.FilterNearest,
	Vk.Smplr.M.createInfoMinFilter = Vk.FilterNearest,
	Vk.Smplr.M.createInfoMipmapMode = Vk.Smplr.MipmapModeLinear,
	Vk.Smplr.M.createInfoAddressModeU = Vk.Smplr.AddressModeRepeat,
	Vk.Smplr.M.createInfoAddressModeV = Vk.Smplr.AddressModeRepeat,
	Vk.Smplr.M.createInfoAddressModeW = Vk.Smplr.AddressModeRepeat,
	Vk.Smplr.M.createInfoMipLodBias = 0,
	Vk.Smplr.M.createInfoAnisotropyEnable = False,
	Vk.Smplr.M.createInfoMaxAnisotropy = 0,
	Vk.Smplr.M.createInfoCompareEnable = False,
	Vk.Smplr.M.createInfoCompareOp = Vk.CompareOpAlways,
	Vk.Smplr.M.createInfoMinLod = 0,
	Vk.Smplr.M.createInfoMaxLod = 0,
	Vk.Smplr.M.createInfoBorderColor = Vk.BorderColorIntOpaqueBlack,
	Vk.Smplr.M.createInfoUnnormalizedCoordinates = False }

newtype MyImage = MyImage (Image PixelRGBA8)

-- type instance Vk.Bffr.ImageFormat MyImage = 'Vk.T.FormatR8g8b8a8Srgb

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

createImage' :: forall nm fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P ->
	Vk.Dvc.D sd -> Word32 -> Word32 -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mm.PropertyFlagBits -> (forall si sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Dvc.Mem.M sm
			'[ '(si, 'Vk.Mm.ImageArg nm fmt) ] ->
		IO a) -> IO a
createImage' pd dvc wdt hgt tlng usg prps f =
	Vk.Img.create @'Nothing
		dvc (imageInfo ext tlng usg) nil \img -> do
	reqs <- Vk.Img.getMemoryRequirements dvc img
	print reqs
	mt <- findMemoryType pd (Vk.Mm.M.requirementsMemoryTypeBits reqs) prps
	print mt
	Vk.Dvc.Mem.allocateBind @'Nothing dvc
		(HPList.Singleton . U2 $ Vk.Dvc.Mem.Image img) (mminfo mt)
		nil \(HPList.Singleton (U2 (Vk.Dvc.Mem.ImageBinded bnd))) m -> do
		f bnd m
	where
	ext = Vk.Extent2d {
		Vk.extent2dWidth = wdt, Vk.extent2dHeight = hgt }
	mminfo mt = Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

createBufferImage :: Storable (KObj.ImagePixel t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (Vk.Dvc.M.Size, Vk.Dvc.M.Size, Vk.Dvc.M.Size, Vk.Dvc.M.Size) ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ Obj.Image 1 t inm] ->
		Vk.Dvc.Mem.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[ Obj.Image 1 t inm])] ->
		IO a) -> IO a
createBufferImage p dv (r, w, h, d) usg props =
	createBuffer p dv (HPList.Singleton $ Obj.LengthImage r w h d) usg props

copyBufferToImage :: forall sd sc sm sb nm img inm si sm' nm' .
	Storable (KObj.ImagePixel img) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[ Obj.Image 1 img inm]  ->
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

-- SHADER

[glslVertexShader|

#version 460

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inColor;
layout(location = 3) in vec2 vTexCoord;

layout(location = 0) out vec3 outColor;
layout(location = 1) out vec2 texCoord;

layout (set = 0, binding = 0) uniform CameraBuffer {
	mat4 view; mat4 proj; mat4 viewproj; } cameraData;

struct ObjectData { mat4 model; };

layout (std140, set = 1, binding = 0) readonly buffer ObjectBuffer {
	ObjectData objects[];
} objectBuffer;

layout(push_constant) uniform constants {
	vec4 data; mat4 render_matrix; } PushConstants;

void
main()
{
	mat4 modelMatrix = objectBuffer.objects[gl_BaseInstance].model;
	mat4 transformMatrix = cameraData.viewproj * modelMatrix;
	gl_Position = transformMatrix * vec4(inPosition, 1.0);
	outColor = inColor;
	texCoord = vTexCoord;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec3 inColor;
layout(location = 1) in vec2 texCoord;

layout(location = 0) out vec4 outFragColor;

layout (set = 0, binding = 1) uniform SceneData {
	vec4 fogColor; vec4 fogDists;
	vec4 ambientColor;
	vec4 sunlightDir; vec4 sunlightColor; } sceneData;

layout (set = 2, binding = 0) uniform sampler2D tex1;

void
main()
{
	vec3 color = texture(tex1, texCoord).xyz;
//	outFragColor = vec4(texCoord.x, texCoord.y, 0.5f, 1.0f);
	outFragColor = vec4(color, 1.0f);
}

|]
