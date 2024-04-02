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
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import qualified Gpu.Vulkan.Memory as Vk.Mem

import GHC.TypeLits (Symbol)
import GHC.TypeNats
import GHC.Generics
import Foreign.Storable
import Foreign.Storable.PeekPoke
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
import Data.List.Infinite qualified as Inf
import Data.List.Infinite (pattern (:~))
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
import qualified Gpu.Vulkan.ImageView as Vk.ImgVw
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderModule.M
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
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
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
import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win
import Data.Ord.ToolsYj
import Data.Bits.ToolsYj
import Data.Tuple.ToolsYj
import Data.Function.ToolsYj
import Data.Maybe.ToolsYj
import Data.List.ToolsYj
import Data.Bool.ToolsYj
import Data.IORef.ToolsYj
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.TypeLevel.List qualified as TList

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

	putStrLn "MIN ALIGN" >>
	(print . Vk.Phd.limitsMinUniformBufferOffsetAlignment
		. Vk.Phd.propertiesLimits =<< Vk.Phd.getProperties pd) >>
	unfrmBffrOstAlgn pd \(_ :: Proxy alu) ->
	strgBffrOstAlgn pd \(_ :: Proxy als) ->
	fromNat @alu @als mff \(_ :: Proxy mff) (_ :: Proxy mff') ->

	createDscStLyt d \dslyt ->
	createDescriptorSetLayoutObjData d \dslyto ->
	createPipelineLayout d dslyt dslyto \lyt ->
	createGraphicsPipeline d ex rp lyt \gpl ->
	createFramebuffers d ex rp scvs dv \fbs ->

	readVtcs mdlfp >>= \vns ->
	let	vnsln = fromIntegral $ V.length vns in
	createVertexBuffer pd d gq cp vns \vb ->
	createVertexBuffer pd d gq cp triangle \vbtri ->

--	createVpBffrs @alu @als @mff' @mff pd d dslyt dslyto
	createVpBffrs @_ @_ @mff' @mff pd d dslyt dslyto
		\lyts cmbs cmms
			(lytods :: HPList.PL (U2 Vk.DscSetLyt.D) slytods)
			(odbs :: HPList.PL BindedObjData sbsmods) odms ->

	createScnBffr @_ @mff' pd d \scnb scnm ->

--	createDscPl mff d \dp ->
	createDescriptorPool d \dp ->

	createDescriptorSets @_ @sbsmods @slytods d dp cmbs lyts odbs lytods scnb \dss dssod ->

	createCommandBuffers d cp \cbs ->
	createSyncObjects d \sos ->

	mainLoop mff w fr sfc pd qfis d gq pq sc ex scvs rp lyt gpl cp drs fbs
		cmms scnm dss odms dssod vb vbtri cbs sos vnsln
	where
	fromNat :: forall alu als a . (KnownNat alu, KnownNat als) => Natural -> (forall n n' . (
		TList.Length n, HPList.FromList n, HPList.RepM n,
--		HPList.HomoList '() n, KnownNat n', CreateVpBffrs alu als n' n ) =>
		HPList.HomoList '() n, KnownNat n', CreateVpBffrs 256 256 n' n ) =>
		Proxy n -> Proxy n' -> a) -> a
	fromNat n f = ($ someNatVal n) \(SomeNat (pn' :: Proxy n')) ->
		tnum @alu @als @n' n \pn -> f pn pn'
	tnum :: forall alu als mff a . (KnownNat alu, KnownNat als, KnownNat mff) => Natural ->
		(forall (n :: [()]) . (
			TList.Length n, HPList.FromList n, HPList.RepM n,
--			HPList.HomoList '() n, CreateVpBffrs alu als mff n ) =>
			HPList.HomoList '() n, CreateVpBffrs 256 256 mff n ) =>
			Proxy n -> a) -> a
	tnum 0 f = f (Proxy @'[])
	tnum n f = tnum @alu @als @mff (n - 1) $ f . scc
		where scc :: Proxy n -> Proxy ('() ': n); scc Proxy = Proxy

type MaxFramesInFlight = 8

frashRate :: Num n => n
frashRate = 2

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

data QFamIndices =
	QFamIndices { grFam :: Vk.QFam.Index, prFam :: Vk.QFam.Index }

findQFams :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (Maybe QFamIndices)
findQFams pd sfc = do
	prps@((fst <$>) -> is) <- Vk.Phd.getQueueFamilyProperties pd
	mp <- listToMaybe
		<$> filterM (flip (Vk.Khr.Sfc.Phd.getSupport pd) sfc) is
	pure $ QFamIndices <$> (fst <$> find (grbit . snd) prps) <*> mp
	where grbit = checkBits Vk.Q.GraphicsBit . Vk.QFam.propertiesQueueFlags

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

type Buffers mff = '[
	'Vk.DscSetLyt.Buffer '[CameraObj], 'Vk.DscSetLyt.Buffer '[SceneObj mff] ]

type ObjDataBuffers = '[ 'Vk.DscSetLyt.Buffer '[ObjDataList] ]

type CameraObj = Obj.Atom 256 WViewProj 'Nothing
type SceneObj mff = Obj.DynAtom mff 256 WScene 'Nothing

createDescriptorSetLayoutObjData :: Vk.Dvc.D sd ->
	(forall (s :: Type) . Vk.DscSetLyt.D s '[ 'Vk.DscSetLyt.Buffer '[ObjDataList]] -> IO a) -> IO a
createDescriptorSetLayoutObjData dv = Vk.DscSetLyt.create dv layoutInfo nil where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[ 'Vk.DscSetLyt.Buffer '[ObjDataList]]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = objd :** HPList.Nil }
	objd :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer '[ObjDataList])
	objd = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

type ObjDataList = Obj.List 256 ObjData ""

createPipelineLayout :: forall mff sd sdl sdlod a . Vk.Dvc.D sd ->
	Vk.DscSetLyt.D sdl (Buffers mff) ->
	Vk.DscSetLyt.D sdlod ObjDataBuffers ->
	(forall sl . Vk.Ppl.Lyt.P sl
			'[ '(sdl, Buffers mff), '(sdlod, ObjDataBuffers) ]
			'[WMeshPushConstants] ->
		IO a) -> IO a
createPipelineLayout dv dslyt dslytod f = Vk.Ppl.Lyt.create dv ci nil f where
	ci :: Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sdl, Buffers mff), '(sdlod, ObjDataBuffers) ] (
		'Vk.PushConstant.Layout
			'[ WMeshPushConstants]
			'[ 'Vk.PushConstant.Range '[ 'Vk.T.ShaderStageVertexBit]
				'[WMeshPushConstants]] )
	ci = Vk.Ppl.Lyt.CreateInfo {
		Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
		Vk.Ppl.Lyt.createInfoFlags = zeroBits,
		Vk.Ppl.Lyt.createInfoSetLayouts =
			U2 dslyt :** U2 dslytod :** HPList.Nil }

createGraphicsPipeline :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.P sl
		'[ '(sdl, Buffers mff), '(sdlod, ObjDataBuffers)]
		'[WMeshPushConstants] ->
	(forall sg . Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl, '[ '(sdl, Buffers mff), '(sdlod, ObjDataBuffers)],
			'[WMeshPushConstants]) -> IO a) ->
	IO a
createGraphicsPipeline dv sce rp lyt f = Vk.Ppl.Grph.createGs dv Nothing
	(HPList.Singleton . U14 $ graphicsPipelineCreateInfo sce rp lyt) nil
	\(HPList.Singleton (U3 gpl)) -> f gpl

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.P sl
		'[ '(sdl, Buffers mff), '(sdlod, ObjDataBuffers)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(sdl, Buffers mff), '(sdlod, ObjDataBuffers)],
			'[WMeshPushConstants]) -> IO ()
recreateGraphicsPipeline dv sce rp lyt gpls = Vk.Ppl.Grph.unsafeRecreateGs dv Nothing
	(U14 (graphicsPipelineCreateInfo sce rp lyt) :** HPList.Nil) nil
	(U3 gpls :** HPList.Nil)

graphicsPipelineCreateInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.P sl
		'[ '(sdl, Buffers mff), '(sdlod, ObjDataBuffers)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.CreateInfo 'Nothing
		'[	'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[])]
		'( 'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Position), '(1, Normal), '(2, Color)])
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'(sl,	'[ '(sdl, Buffers mff), '(sdlod, ObjDataBuffers)],
			'[WMeshPushConstants]) sr
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

type DepthResources sb sm nm fmt sdiv = (
	Vk.Img.Binded sm sb nm fmt,
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)],
	Vk.ImgVw.I nm fmt sdiv )

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
	allocInfo :: Vk.CBffr.AllocateInfo 'Nothing sc '[ '()]
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
	Vk.ImgVw.I dnm dfmt siv ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HPList.Nil _ f = f HPList.Nil
createFramebuffers dv sce rp (iv :** ivs) dptiv f =
	Vk.Frmbffr.create dv (framebufferInfo sce rp iv dptiv) nil \fb ->
	createFramebuffers dv sce rp ivs dptiv \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPss.R sr -> HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
		Vk.ImgVw.I dnm dfmt sdiv -> HPList.PL Vk.Frmbffr.F sfs ->
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
	Vk.ImgVw.I dnm dfmt sdiv ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(nm, fmt, si), '(dnm, dfmt, sdiv)]
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

class CreateVpBffrs alvp als mff (sds :: [()]) where
	createVpBffrs :: Vk.Phd.P -> Vk.Dvc.D sd ->
		Vk.DscSetLyt.D sdsc (DscStLytArg alvp mff) ->
		Vk.DscSetLyt.D sdscod '[BufferObjData als] ->
		(forall dlas svp dlasod svpod . (
			HPList.FromList dlas, Vk.DscSet.DListFromMiddle dlas,
			HPList.FromList dlasod, Vk.DscSet.DListFromMiddle dlasod,
			Update alvp als mff svp dlas svpod dlasod,
			HPList.HomoList '(sdsc, DscStLytArg alvp mff) dlas,
			HPList.HomoList '(sdscod, '[BufferObjData als]) dlasod
			) =>
			HPList.PL (U2 Vk.DscSetLyt.D) dlas ->
			HPList.PL (BindedVp alvp bnmvp) svp ->
			HPList.PL (MemoryVp alvp bnmvp) svp ->
			HPList.PL (U2 Vk.DscSetLyt.D) dlasod ->
			HPList.PL (BindedOd als bnmod) svpod ->
			HPList.PL (MemoryOd als bnmod) svpod ->
			IO a) -> IO a

type BindedCamera = BindedVp 256 "camera-buffer"
type MemoryCamera = MemoryVp 256 "camera-buffer"
type BindedObjData = BindedOd 256 "object-data-buffer"
type MemoryObjData = MemoryOd 256 "object-data-buffer"

instance CreateVpBffrs _alu _als _mff '[] where
	createVpBffrs _ _ _ _ f = f
		HPList.Nil HPList.Nil HPList.Nil
		HPList.Nil HPList.Nil HPList.Nil

instance (
	KnownNat alvp, KnownNat als, KnownNat mff,
	CreateVpBffrs alvp als mff sds ) => CreateVpBffrs alvp als mff (sd ': sds) where
	createVpBffrs pd dv dl dlod f =
		createVpBffr pd dv \bnd mm ->
		createOdBuffer pd dv \bndod mmod ->
		createVpBffrs @alvp @als @_ @sds pd dv dl dlod \dls bnds mms dlods bndods mmods -> f
			(U2 dl :** dls) (BindedVp bnd :** bnds) (MemoryVp mm :** mms)
			(U2 dlod :** dlods) (BindedOd bndod :** bndods) (MemoryOd mmod :** mmods)

createVpBffr :: KnownNat alvp => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[AtomViewProj alvp] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[AtomViewProj alvp])] ->
	IO a) -> IO a
createVpBffr =
	createBffrAtm Vk.Bffr.UsageUniformBufferBit Vk.Mm.PropertyHostVisibleBit

createBuffer :: forall objs nm sd a . (
	Obj.SizeAlignmentList objs,
	forall s . SizeAlignmentAll s nm objs,
	Obj.WholeAlign objs ) =>
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
	SizeAlignmentAll s (nm :: Symbol) (objs :: [Obj.O])

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

maxObjects :: Vk.Dvc.M.Size
maxObjects = 10000

createOdBuffer ::
	KnownNat als =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[ListObjData als] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[ListObjData als])] ->
	IO a) -> IO a
createOdBuffer pd dv = createBffr pd dv
	(HPList.Singleton $ Obj.LengthList maxObjects)
	Vk.Bffr.UsageStorageBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyDeviceLocalBit)

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
				Vk.DscPl.sizeDescriptorCount = 10 } ] }

createDescriptorSets ::
	forall mff odbs lytods lyts cmbs sd sp ssb ssm a . (
	Vk.DscSet.DListFromMiddle lyts,
	Vk.DscSet.DListFromMiddle lytods,
	HPList.FromList lyts,
	Update 256 256 mff cmbs lyts odbs lytods) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	HPList.PL BindedCamera cmbs -> HPList.PL (U2 Vk.DscSetLyt.D) lyts ->
	HPList.PL BindedObjData odbs -> HPList.PL (U2 Vk.DscSetLyt.D) lytods ->
	Vk.Bffr.Binded ssb ssm "scene-buffer" '[SceneObj mff] ->
	(forall sds sds' .
		HPList.PL (Vk.DscSet.D sds) lyts ->
		HPList.PL (Vk.DscSet.D sds') lytods -> IO a) -> IO a
createDescriptorSets dv dscp cmbs lyts odbs lytods scnb f =
	Vk.DscSet.allocateDs dv allocInfo \dscss ->
	Vk.DscSet.allocateDs dv allocInfoOd \dscsods -> do
	update @_ @_ @_ @_ @_ @odbs @lytods dv dscss cmbs dscsods odbs scnb
	f dscss dscsods
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = lyts }
	allocInfoOd = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = lytods }

class Update alu als mff smsbsvp slbtss smsbsod slbtssod where
	update :: Vk.Dvc.D sd ->
		HPList.PL (Vk.DscSet.D sds) slbtss ->
		HPList.PL (BindedVp alu bnmvp) smsbsvp ->
		HPList.PL (Vk.DscSet.D sdsod) slbtssod ->
		HPList.PL (BindedOd als bnmod) smsbsod ->
		Vk.Bffr.Binded smsn sbsn bnmsn '[AtomSceneData alu mff] -> IO ()

data BindedVp alvp bnmvp smsb where
	BindedVp :: Vk.Bffr.Binded sm sb bnmvp '[AtomViewProj alvp] ->
		BindedVp alvp bnmvp '(sm, sb)

data MemoryVp alvp bnmvp smsb where
	MemoryVp ::
		Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.BufferArg bnmvp '[AtomViewProj alvp])] ->
		MemoryVp alvp bnmvp '(sm, sb)

data BindedOd als bnmod smsb where
	BindedOd ::
		Vk.Bffr.Binded sm sb bnmod '[ListObjData als] ->
		BindedOd als bnmod '(sm, sb)

data MemoryOd als bnmod smsb where
	MemoryOd ::
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnmod '[ListObjData als])] ->
		MemoryOd als bnmod '(sm, sb)

type ListObjData als = Obj.List als ObjData ""

type DscStLytArg alu mff = '[BufferViewProj alu, BufferSceneData alu mff]
type BufferViewProj alu = 'Vk.DscSetLyt.Buffer '[AtomViewProj alu]
type BufferSceneData alu mff = 'Vk.DscSetLyt.Buffer '[AtomSceneData alu mff]

type AtomViewProj alu = Obj.Atom alu WViewProj 'Nothing
type AtomSceneData alu mff = Obj.DynAtom mff alu WScene 'Nothing

type BufferObjData als = 'Vk.DscSetLyt.Buffer '[ListObjData als]

instance Update _alu _als _mff '[] '[] '[] '[] where
	update _ HPList.Nil HPList.Nil HPList.Nil HPList.Nil _ = pure ()

instance (
	KnownNat alu, KnownNat als, KnownNat mff,
	Vk.DscSet.BindingAndArrayElemBuffer bts '[AtomViewProj alu] 0,
	Vk.DscSet.BindingAndArrayElemBuffer bts '[AtomSceneData alu mff] 0,
	Vk.DscSet.BindingAndArrayElemBuffer btsod '[ListObjData als] 0,
	Vk.DscSet.UpdateDynamicLength bts '[AtomViewProj alu],
	Vk.DscSet.UpdateDynamicLength bts '[AtomSceneData alu mff],
	Vk.DscSet.UpdateDynamicLength btsod '[ListObjData als],
	Update alu als mff smsbsvp slbtss smsbsod slbtssod) =>
	Update alu als mff
		(smsbvp ': smsbsvp) ('(slyt, bts) ': slbtss)
		(smsbod ': smsbsod) ('(slytod, btsod) ': slbtssod)
		where

	update dv (ds :** dss) (BindedVp bvp :** bvps) (dsod :** dssod) (BindedOd bod :** bods) scnb = do
		Vk.DscSet.updateDs dv (
			U5 (dscWrite @(AtomViewProj alu)
				ds bvp Vk.Dsc.TypeUniformBuffer) :**
			U5 (dscWrite @(AtomSceneData alu mff)
				ds scnb Vk.Dsc.TypeUniformBufferDynamic) :**
			U5 (dscWrite @(ListObjData als)
				dsod bod Vk.Dsc.TypeStorageBuffer) :**
			HPList.Nil ) HPList.Nil
		update dv dss bvps dssod bods scnb

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

createSyncObjects ::
	Vk.Dvc.D sd -> (forall ssos . SyncObjects ssos -> IO a ) -> IO a
createSyncObjects dv f =
	HPList.replicateM 9
		(Vk.Semaphore.create @'Nothing dv def nil) \iass ->
	HPList.replicateM 9
		(Vk.Semaphore.create @'Nothing dv def nil) \rfss ->
	HPList.replicateM 9
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
	HPList.HomoList '(slyt, Buffers mff) lyts,
	HPList.HomoList '(slytod, ObjDataBuffers) lytods,
	KnownNat mff
	) =>
	Natural ->
	GlfwG.Win.W sw -> FramebufferResized -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.P sl
		'[ '(slyt, Buffers mff), '(slytod, ObjDataBuffers)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(slyt, Buffers mff), '(slytod, ObjDataBuffers)],
			'[WMeshPushConstants]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs ->
	HPList.PL MemoryCamera scms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg "scene-buffer" '[SceneObj mff])] ->
	HPList.PL (Vk.DscSet.D sds) lyts ->
	HPList.PL MemoryObjData sods -> HPList.PL (Vk.DscSet.D sds') lytods ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 WVertex ""] ->
	HPList.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> SyncObjects sos ->
	VertexNumber -> IO ()
mainLoop mff w rszd sfc pd qfis dv gq pq sc ex0 scivs rp lyt gpl cp drs fbs
	cmms scnm dss odms dssod vb vbtri cbs sos vnsln =
	($ 0) . ($ Inf.cycle $ NE.fromList [0 .. fromIntegral mff - 1]) . ($ ex0) $ fix
		\loop ex (ffn :~ ffns) fn -> Glfw.pollEvents >>
	step w rszd sfc pd qfis dv gq pq sc ex scivs rp lyt gpl cp drs fbs
		cmms scnm dss odms dssod vb vbtri cbs sos vnsln ffn fn
		(\ex' -> loop ex' ffns ((fn + 1) `mod` (360 * frashRate))) >>
	Vk.Dvc.waitIdle dv

type VertexNumber = Word32

step :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs,
	HPList.HomoList '(slyt, Buffers mff) lyts,
	HPList.HomoList '(slytod, ObjDataBuffers) lytods,
	KnownNat mff
	) =>
	GlfwG.Win.W sw -> FramebufferResized -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.P sl
		'[ '(slyt, Buffers mff), '(slytod, ObjDataBuffers)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(slyt, Buffers mff), '(slytod, ObjDataBuffers)],
			'[WMeshPushConstants]) ->
	Vk.CmdPl.C scp -> DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	HPList.PL Vk.Frmbffr.F sfs -> HPList.PL MemoryCamera scms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg "scene-buffer" '[SceneObj mff])] ->
	HPList.PL (Vk.DscSet.D sds) lyts ->
	HPList.PL MemoryObjData sods -> HPList.PL (Vk.DscSet.D sds') lytods ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 WVertex ""] ->
	HPList.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> SyncObjects sos ->
	Word32 -> Int -> Int -> (Vk.Extent2d -> IO ()) -> IO ()
step w@(GlfwG.Win.W win) frszd sfc pd qfis dv gq pq sc ex scivs rp lyt gpl cp drs fbs
	cmms scnm dss odms dssod vb vbtri cbs sos vnsln ffn fn loop = do
	catchAndRecreate w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs loop
		$ drawFrame dv gq pq sc ex rp lyt gpl fbs
			cmms scnm dss odms dssod vb vbtri cbs sos vnsln ffn fn
	(Glfw.windowShouldClose win >>=) . flip bool (pure ()) $
		(checkFlag frszd >>=) . bool (loop ex) $
		loop =<< recreateAll
			w sfc pd qfis dv gq sc scivs rp lyt gpl cp drs fbs

catchAndRecreate :: (Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QueueFamilyIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.P sl
		'[ '(s, Buffers mff), '(sod, ObjDataBuffers)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(s, Buffers mff), '(sod, ObjDataBuffers)],
			'[WMeshPushConstants]) ->
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
	Vk.Ppl.Lyt.P sl
		'[ '(slyt, Buffers mff), '(slytod, ObjDataBuffers)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(slyt, Buffers mff), '(slytod, ObjDataBuffers)],
			'[WMeshPushConstants]) ->
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
	forall sd ssc scfmt sr slyt sl slod sg sfs scmmbs ssm ssb lyts
	sm sb nm smtri sbtri nmtri scb ssos sods lytods sds sds' mff .  (
	HPList.HomoList '(sl, Buffers mff) lyts,
	HPList.HomoList '(slod, ObjDataBuffers) lytods,
	KnownNat mff
	) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.P slyt
		'[ '(sl, Buffers mff), '(slod, ObjDataBuffers)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt,	'[ '(sl, Buffers mff), '(slod, ObjDataBuffers)],
			'[WMeshPushConstants]) ->
	HPList.PL Vk.Frmbffr.F sfs -> HPList.PL MemoryCamera scmmbs ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg "scene-buffer" '[SceneObj mff])] ->
	HPList.PL (Vk.DscSet.D sds) lyts ->
	HPList.PL MemoryObjData sods -> HPList.PL (Vk.DscSet.D sds') lytods ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 WVertex ""] ->
	HPList.LL' (Vk.CBffr.C scb) MaxFramesInFlight -> SyncObjects ssos ->
	Word32 -> Int -> Int -> IO ()
drawFrame dv gq pq sc ex rp lyt gpl fbs cmms scnm dss odms dssod vb vbtri cbs
	(SyncObjects iass rfss iffs) vnsln ffn fn =
	HPList.index iass ffn \(ias :: Vk.Semaphore.S sias) ->
	HPList.index rfss ffn \(rfs :: Vk.Semaphore.S srfs) ->
	HPList.index iffs ffn \(id &&& HPList.Singleton -> (iff, siff)) ->
	HPList.index cmms ffn \(MemoryVp cmm) ->
	HPList.index odms ffn \(MemoryOd odm) -> do
	Vk.Mm.write @"camera-buffer" @CameraObj dv cmm zeroBits (cameraData ex)
	Vk.Mm.write @"scene-buffer" @(SceneObj mff) dv scnm zeroBits . (!! ffn)
		$ iterate (Nothing :) [Just $ sceneData fn]
	Vk.Mm.write @"object-data-buffer" @ObjDataList dv odm zeroBits . map ObjData $
		model (fromIntegral fn) : [ objectMatrix x y | x <- [- 20 .. 20], y <- [- 20 .. 20] ]
	Vk.Fnc.waitForFs dv siff True Nothing
	iid <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dv sc maxBound (Just ias) Nothing
	Vk.Fnc.resetFs dv siff
	Vk.CBffr.reset cb zeroBits
	HPList.index fbs iid \fb -> recordCommandBuffer
		ex rp lyt gpl fb ds dsod vb vbtri cb vnsln (fromIntegral ffn) fn
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
	ds = HPList.homoListIndex @'(sl, Buffers mff) dss ffn
	dsod = HPList.homoListIndex @'(slod, ObjDataBuffers) dssod ffn
	catchAndSerialize = (`catch`
		\(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

recordCommandBuffer ::
	forall sr slyt sg sdlyt sdlytod sf sm sb nm smtri sbtri nmtri scb sds sds' mff .
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Lyt.P slyt
		'[ '(sdlyt, Buffers mff), '(sdlytod, ObjDataBuffers)]
		'[WMeshPushConstants] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(slyt,	'[ '(sdlyt, Buffers mff), '(sdlytod, ObjDataBuffers)],
			'[WMeshPushConstants]) ->
	Vk.Frmbffr.F sf ->
	Vk.DscSet.D sds '(sdlyt, Buffers mff) ->
	Vk.DscSet.D sds' '(sdlytod, ObjDataBuffers) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded smtri sbtri nmtri '[Obj.List 256 WVertex ""] ->
	Vk.CBffr.C scb -> Word32 -> Word32 -> Int -> IO ()
recordCommandBuffer sce rp lyt gpl fb ds dsod vb vbt cb vn ffn (fromIntegral -> fn) =
	Vk.CBffr.begin @'Nothing @'Nothing cb binfo $
	Vk.Cmd.beginRenderPass cb rpinfo Vk.Subpass.ContentsInline do
	ovb <- newIORef Nothing
	drawObject ovb cb ds dsod RenderObject {
		renderObjectPipeline = gpl,
		renderObjectPipelineLayout = lyt,
		renderObjectMesh = vb, renderObjectMeshSize = vn,
		renderObjectTransformMatrix = model fn } ffn 0
	ovbtri <- newIORef Nothing
	for_ [- 20 .. 20] \x -> for_ [- 20 .. 20] \y ->
		drawObject ovbtri cb ds dsod RenderObject {
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
				(fromJust $ rgbaDouble 0 0 blue 1) :**
			Vk.ClearValueDepthStencil
				(Vk.ClearDepthStencilValue 1 0) :** HPList.Nil }
	blue = 0.5 + sin (fn / (180 * frashRate) * pi) / 2

model :: Float -> Glm.Mat4
model fn = Glm.rotate
	Glm.mat4Identity (fn * Glm.rad 1) (Glm.Vec3 $ 0 :. 1 :. 0 :. NilL)

objectMatrix :: Float -> Float -> Glm.Mat4
objectMatrix x y = trans x y `Glm.mat4Mul` scale

trans :: Float -> Float -> Glm.Mat4
trans x y = Glm.translate Glm.mat4Identity (Glm.Vec3 $ x :. 0 :. y :. NilL)

scale :: Glm.Mat4
scale = Glm.scale Glm.mat4Identity (Glm.Vec3 $ 0.2 :. 0.2 :. 0.2 :. NilL)

drawObject ::
	IORef (Maybe (Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""])) ->
	Vk.CBffr.C scb ->
	Vk.DscSet.D sds '(sdlyt, Buffers mff) ->
	Vk.DscSet.D sds' '(sdlytod, ObjDataBuffers) ->
	RenderObject mff sg sl sdlyt sdlytod sm sb nm -> Word32 -> Word32 -> IO ()
drawObject ovb cb0 ds dsod RenderObject {
	renderObjectPipeline = gpl,
	renderObjectPipelineLayout = lyt,
	renderObjectMesh = vb, renderObjectMeshSize = vn,
	renderObjectTransformMatrix = mdl } ffn i =
	Vk.Cmd.bindPipelineGraphics cb0 Vk.Ppl.BindPointGraphics gpl \cb -> do
	Vk.Cmd.bindDescriptorSetsGraphics cb Vk.Ppl.BindPointGraphics lyt
		(U2 ds :** U2 dsod :** HPList.Nil) $
		(HPList.Nil :** (Vk.Cmd.DynamicIndex ffn :** HPList.Nil) :** HPList.Nil) :**
		(HPList.Nil :** HPList.Nil) :** HPList.Nil
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

data RenderObject mff sg sl sdlyt sdlytod sm sb nm = RenderObject {
	renderObjectPipeline :: Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color)]
		'(sl,	'[ '(sdlyt, Buffers mff), '(sdlytod, ObjDataBuffers)],
			'[WMeshPushConstants]),
	renderObjectPipelineLayout ::
		Vk.Ppl.Lyt.P sl
			'[ '(sdlyt, Buffers mff), '(sdlytod, ObjDataBuffers)]
			'[WMeshPushConstants],
	renderObjectMesh :: Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""],
	renderObjectMeshSize :: Word32,
	renderObjectTransformMatrix :: Glm.Mat4 }

-- VERTEX

data Vertex = Vtx {
	vtxPos :: Position, vtxNormal :: Normal, vtxColor :: Color }
	deriving (Show, Generic)

newtype Position = Position Glm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Normal = Normal Glm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Color = Color Glm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

type WVertex = Str.G.W Vertex

instance Str.G.G Vertex

instance Storable Vertex where
	sizeOf = Str.G.gSizeOf; alignment = Str.G.gAlignment
	peek = Str.G.gPeek; poke = Str.G.gPoke

triangle :: V.Vector WVertex
triangle = V.fromList $ Str.G.W <$> [
	Vtx {
		vtxPos = Position . Glm.Vec3 $ 1 :. 1 :. 0.5 :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vtxColor = Color . Glm.Vec3 $ 0 :. 1 :. 0 :. NilL },
	Vtx {
		vtxPos = Position . Glm.Vec3 $ (- 1) :. 1 :. 0.5 :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vtxColor = Color . Glm.Vec3 $ 0 :. 1 :. 0 :. NilL },
	Vtx {
		vtxPos = Position . Glm.Vec3 $ 0 :. (- 1) :. 0.5 :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vtxColor = Color . Glm.Vec3 $ 0 :. 1 :. 0 :. NilL } ]

-- CAMERA DATA


type WViewProj = Str.G.W CameraData

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

cameraData :: Vk.Extent2d -> Str.G.W CameraData
cameraData ex = Str.G.W $ CameraData (View view) (Proj $ projection ex)
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

type WScene = Str.G.W SceneData

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

sceneData :: Int -> Str.G.W SceneData
sceneData fn = Str.G.W SceneData {
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

type WMeshPushConstants = GStorable.W MeshPushConstants

instance Str.G.G MeshPushConstants

-- OBJECT DATA

newtype ObjData = ObjData {
	objectDataModelMatrix :: Glm.Mat4 } deriving (Show, Generic)

instance Storable ObjData where
	sizeOf = Str.G.gSizeOf; alignment = Str.G.gAlignment
	peek = Str.G.gPeek; poke = Str.G.gPoke

instance Str.G.G ObjData
	
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
		Vk.Ppl.ShdrSt.createInfoModule = (crInfo vs, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	fraginfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModule = (crInfo fs, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	crInfo cd = Vk.ShaderModule.M.CreateInfo {
			Vk.ShaderModule.M.createInfoNext = TMaybe.N,
			Vk.ShaderModule.M.createInfoFlags = zeroBits,
			Vk.ShaderModule.M.createInfoCode = cd }

[glslVertexShader|

#version 460

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inColor;

layout(location = 0) out vec3 outColor;

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
	mat4 transformMatrix =
//		cameraData.viewproj * PushConstants.render_matrix;
		cameraData.viewproj * modelMatrix;
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
