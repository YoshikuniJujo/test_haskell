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
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.List.Infinite (pattern (:~))
import Data.List.Infinite qualified as Inf
import Data.List.Length
import Data.List.ToolsYj
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.Array
import Data.Vector.Storable qualified as V
import Data.Word
import Data.ByteString qualified as BS
import Data.Text.IO qualified as Txt
import Data.Color
import Data.IORef
import Data.IORef.ToolsYj
import Codec.Picture
import Codec.WavefrontObj.ReadFaceSimple qualified as WvRf
import Options.Declarative (Flag, Def, Cmd, run_, get)

import Language.SpirV qualified as SpirV
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH
import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object qualified as Obj
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
import Gpu.Vulkan.Fence qualified as Vk.Fnc

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Pipeline.Graphics qualified as Vk.Ppl.Grph
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.VertexInputState qualified as Vk.Ppl.VtxIptSt
import Gpu.Vulkan.Pipeline.InputAssemblyState qualified as Vk.Ppl.InpAsmbSt
import Gpu.Vulkan.Pipeline.ViewportState qualified as Vk.Ppl.ViewportSt
import Gpu.Vulkan.Pipeline.RasterizationState qualified as Vk.Ppl.RstSt
import Gpu.Vulkan.Pipeline.MultisampleState qualified as Vk.Ppl.MltSmplSt
import Gpu.Vulkan.Pipeline.DepthStencilState qualified as Vk.Ppl.DptStnSt
import Gpu.Vulkan.Pipeline.ColorBlendAttachment qualified as Vk.Ppl.ClrBlndAtt
import Gpu.Vulkan.Pipeline.ColorBlendState qualified as Vk.Ppl.ClrBlndSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.PplLyt
import Gpu.Vulkan.PushConstant qualified as Vk.PushConstant
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
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DSLt

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

main :: IO ()
main = run_ realMain

realMain ::
	Flag "m" '["model"] "FILEPATH" "model filepath"
		(Def "./assets/lost_empire.obj" String) ->
	Flag "t" '["texture"] "FILEPATH" "texture filepath"
		(Def "./assets/lost_empire-RGBA.png" String) ->
	Flag "f" '["frames"] "NUMBER" "max frames in flight" (Def "2" Int) ->
	Cmd "Try Vulkan Guide" ()
realMain mdlfp txfp mff = liftIO $ newIORef False >>= \fr -> withWindow fr \w ->
	createIst \ist -> bool id (dbgm ist) debug
		$ body (get mdlfp) (get txfp) (fromIntegral $ get mff) fr w ist
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

body :: FilePath -> FilePath -> Natural ->
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Ist.I si -> IO ()
body mdlfp txfp mff fr w ist =
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
	createPplLyt @alu @als @mffn d \dsl dslo dslt pl ->
	createGrPpl d ex rp pl \gp ->
	createFrmbffrs d ex rp scvs dv \fbs ->
	readVtcs mdlfp >>= \vns ->
	createVtxBffr pd d gq cp vns \vb ->
	createVtxBffr pd d gq cp triangle \vbtri ->
	createVpBffrs @_ @_ @_ @mff pd d dsl dslo
		\dsls vpbs vpbms dslos odbs odbms ->
	createScnBffr pd d \snb snbm ->
	createDscPl mff d \dp ->
	createDscSts d dp dsls vpbs dslos odbs snb \dss dsso ->
	either error convertRGBA8 <$> readImage txfp >>= \txi ->
	createTxImg pd d gq cp (ImageRgba8 txi) \tx ->
	Vk.ImgVw.create d (imgVwInfo tx Vk.Img.AspectColorBit) nil \tv ->
	createDscStTx d dp dslt tv \dst ->
	Vk.CBffr.allocate @_ @mff d (cmdBffrInfo cp) \cbs ->
	createSyncObjs @mff d \sos ->
	mainloop mff fr w sfc pd qfis d gq pq cp
		sc ex scvs rp pl gp fbs drs
		vb vbtri vpbms snbm dss odbms dsso dst cbs sos
	where
	fromNat :: forall alu als a . (KnownNat alu, KnownNat als) =>
		Natural -> (forall n n' . (
			TList.Length n, HPList.FromList n, HPList.RepM n,
			HPList.HomoList '() n, KnownNat n',
			CreateVpBffrs alu als n' n ) =>
			Proxy n -> Proxy n' -> a) -> a
	fromNat n f = ($ someNatVal n) \(SomeNat (pn' :: Proxy n')) ->
		tnum @alu @als @n' n \pn -> f pn pn'
	tnum :: forall alu als mff a .
		(KnownNat alu, KnownNat als, KnownNat mff) =>
		Natural -> (forall (n :: [()]) . (
			TList.Length n, HPList.FromList n, HPList.RepM n,
			HPList.HomoList '() n, CreateVpBffrs alu als mff n ) =>
			Proxy n -> a) -> a
	tnum 0 f = f (Proxy @'[])
	tnum n f = tnum @alu @als @mff (n - 1) $ f . scc
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

createCmdPl :: QFamIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfis dv = Vk.CmdPl.create dv info nil
	where info = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = grFam qfis }

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

strgBffrOstAlgn ::
	Vk.Phd.P -> (forall a . KnownNat a => Proxy a -> IO b) -> IO b
strgBffrOstAlgn pd f = (\(SomeNat p) -> f p) . someNatVal . fromIntegral
	. Vk.Phd.limitsMinStorageBufferOffsetAlignment . Vk.Phd.propertiesLimits
	=<< Vk.Phd.getProperties pd

createPplLyt ::
	forall alu als mff sd a . Vk.Dvc.D sd -> (forall sl sdsl sdslo sdslt .
	Vk.DSLt.D sdsl (DscStLytArg alu mff) ->
	Vk.DSLt.D sdslo (DscStLytArgOd als) ->
	Vk.DSLt.D sdslt DscStLytArgTx ->
	Vk.PplLyt.P sl
		'[	'(sdsl, DscStLytArg alu mff),
			'(sdslo, DscStLytArgOd als),
			'(sdslt, DscStLytArgTx) ]
		'[WMeshPushConsts] -> IO a) -> IO a
createPplLyt dv f =
	createDscStLyt @_ @mff dv \dsl -> createDscStLytOd dv \dslo ->
	createDscStLytTx dv \dslt ->
	Vk.PplLyt.create dv (info dsl dslo dslt) nil $ f dsl dslo dslt
	where
	info :: Vk.DSLt.D s (DscStLytArg alu mff) ->
		Vk.DSLt.D so (DscStLytArgOd als) ->
		Vk.DSLt.D st DscStLytArgTx ->
		Vk.PplLyt.CreateInfo 'Nothing
			'[	'(s, DscStLytArg alu mff),
				'(so, DscStLytArgOd als),
				'(st, DscStLytArgTx) ]
			('Vk.PushConstant.Layout '[ WMeshPushConsts]
				'[ 'Vk.PushConstant.Range
					'[ 'Vk.T.ShaderStageVertexBit]
					'[WMeshPushConsts] ])
	info dsl dslo dslt = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts =
			U2 dsl :** U2 dslo :** U2 dslt :** HPList.Nil }

createDscStLyt :: forall alu mff sd a . Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DSLt.D s (DscStLytArg alu mff) -> IO a) -> IO a
createDscStLyt dv = Vk.DSLt.create dv info nil where
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
createDscStLytOd dv = Vk.DSLt.create dv info nil where
	info = Vk.DSLt.CreateInfo {
		Vk.DSLt.createInfoNext = TMaybe.N,
		Vk.DSLt.createInfoFlags = zeroBits,
		Vk.DSLt.createInfoBindings = objd :** HPList.Nil }
	objd = Vk.DSLt.BindingBuffer {
		Vk.DSLt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
		Vk.DSLt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

createDscStLytTx :: Vk.Dvc.D sd ->
	(forall (s :: Type) . Vk.DSLt.D s DscStLytArgTx -> IO a) -> IO a
createDscStLytTx dv = Vk.DSLt.create dv info nil where
	info = Vk.DSLt.CreateInfo {
		Vk.DSLt.createInfoNext = TMaybe.N,
		Vk.DSLt.createInfoFlags = zeroBits,
		Vk.DSLt.createInfoBindings = txbd :** HPList.Nil }
	txbd = Vk.DSLt.BindingImage {
		Vk.DSLt.bindingImageDescriptorType =
			Vk.Dsc.TypeCombinedImageSampler,
		Vk.DSLt.bindingImageStageFlags = Vk.ShaderStageFragmentBit }

type DscStLytArg alu mff = '[
	'Vk.DSLt.Buffer '[AtmViewProj alu],
	'Vk.DSLt.Buffer '[AtmScene alu mff] ]
type DscStLytArgOd als = '[ 'Vk.DSLt.Buffer '[ListObjData als]]
type DscStLytArgTx = '[ 'Vk.DSLt.Image '[Tx]]

type AtmViewProj alu = Obj.AtomMaybeName alu WViewProj 'Nothing
type AtmScene alu mff = Obj.DynAtom mff alu WScene 'Nothing
type ListObjData als = Obj.List als WObjData ""
type Tx = '(TxName, TxFormat)
type TxName = "texture"; type TxFormat = Vk.T.FormatR8g8b8a8Srgb

createGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[	'(sdsl, DscStLytArg alu mff),
			'(sdslo, DscStLytArgOd als),
			'(sdslt, DscStLytArgTx) ] '[WMeshPushConsts] ->
	(forall sg . Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(	sl,
			'[	'(sdsl, DscStLytArg alu mff),
				'(sdslo, DscStLytArgOd als),
				'(sdslt, DscStLytArgTx) ],
			'[WMeshPushConsts] ) -> IO a) -> IO a
createGrPpl dv ex rp pl f = Vk.Ppl.Grph.createGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	\(HPList.Singleton (U3 p)) -> f p

recreateGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[	'(sdsl, DscStLytArg alu mff),
			'(sdslo, DscStLytArgOd als),
			'(sdslt, DscStLytArgTx) ] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(	sl,
			'[	'(sdsl, DscStLytArg alu mff),
				'(sdslo, DscStLytArgOd als),
				'(sdslt, DscStLytArgTx) ],
			'[WMeshPushConsts] ) -> IO ()
recreateGrPpl dv ex rp pl p = Vk.Ppl.Grph.unsafeRecreateGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	(HPList.Singleton $ U3 p)

grPplInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[
		'(sdsl, DscStLytArg alu mff),
		'(sdslo, DscStLytArgOd als),
		'(sdslt, DscStLytArgTx) ] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.CreateInfo 'Nothing
		'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
		'(	'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)] ) 'Nothing
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'(	sl,
			'[	'(sdsl, DscStLytArg alu mff),
				'(sdslo, DscStLytArgOd als),
				'(sdslt, DscStLytArgTx) ],
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
	=<< (V.map pn2v <$>) . WvRf.posTexNormal . WvRf.r <$> BS.readFile fp
	where pn2v (GStorable.W ((,,)
		(GStorable.W (WvRf.Position x y z))
		(GStorable.W (WvRf.TexCoord p q))
		(GStorable.W (WvRf.Normal v w u)))) =
		GStorable.W Vtx {
			vtxPos = Position . Glm.Vec3 $ x :. y :. z :. NilL,
			vtxNormal = Normal . Glm.Vec3 $ v :. w :. u :. NilL,
			vtxColor = Color . Glm.Vec3 $ v :. w :. u :. NilL,
			vtxUv = Uv . Glm.Vec2 $ p :. (1 - q) :. NilL }

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
				@bnm' @(Obj.List al t lnm') @0 dv bm' zeroBits xs
			copy b' b
		f (b, ln')
	where
	copy :: forall sm sb bnm sm' sb' bnm' al . KnownNat al =>
		Vk.Bffr.Binded sm sb bnm '[Obj.List al (Element lst) lnm] ->
		Vk.Bffr.Binded sm' sb' bnm' '[Obj.List al (Element lst) lnm] -> IO ()
	copy s d = singleTimeCmds dv gq cp \cb ->
		Vk.Cmd.copyBuffer @'[ '( '[Obj.List al (Element lst) lnm], 0, 0)] cb s d

bffrAlgn :: forall o sd a . Obj.SizeAlignment o =>
	Vk.Dvc.D sd -> Obj.Length o -> Vk.Bffr.UsageFlags ->
	(forall al . KnownNat al => Proxy al -> IO a) -> IO a
bffrAlgn dv ln us f =
	Vk.Bffr.create dv (bffrInfo (HPList.Singleton ln) us) nil \b ->
	(\(SomeNat p) -> f p) . someNatVal . fromIntegral
		=<< Vk.Mm.requirementsAlignment
		<$> Vk.Bffr.getMemoryRequirements dv b

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

createBffrAtm :: forall al sd nm a b . (KnownNat al, Storable a) =>
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Obj.AtomMaybeName al a 'Nothing] ->
		Vk.Mm.M sm '[ '(
			sb, 'Vk.Mm.BufferArg nm '[Obj.AtomMaybeName al a 'Nothing] )] ->
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

createBffrImg :: forall sd bnm inm img a . BObj.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	img -> (forall sm sb al . KnownNat al =>
		Vk.Bffr.Binded sm sb bnm '[Obj.Image al img inm] ->
		Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.BufferArg bnm '[Obj.Image al img inm]) ] ->
		IO a) -> IO a
createBffrImg p dv us prs img a =
	bffrAlgn @(Obj.Image 256 img inm) dv ln us \(_ :: Proxy al) ->
	createBffr @_ @_ @'[Obj.Image al img inm]
		p dv (HPList.Singleton ln) us prs a
	where
	ln :: Obj.Length (Obj.Image al img inm)
	ln = Obj.LengthImage
		(BObj.imageRow img) (BObj.imageWidth img)
		(BObj.imageHeight img) (BObj.imageDepth img)

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
		(memInfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bd))) -> bd

bffrInfo :: HPList.PL Obj.Length os ->
	Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo 'Nothing os
bffrInfo lns us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N, Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = lns,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

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

memInfo :: Vk.Mm.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
memInfo mt = Vk.Mm.AllocateInfo {
	Vk.Mm.allocateInfoNext = TMaybe.N,
	Vk.Mm.allocateInfoMemoryTypeIndex = mt }

copyBffrToImg :: forall sd sc smb sbb nmb al img imgnm smi si nmi .
	(KnownNat al, Storable (BObj.ImagePixel img)) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded smb sbb nmb '[ Obj.Image al img imgnm]  ->
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
	Obj.LengthImage _r (fromIntegral -> w) (fromIntegral -> h) _d =
		Obj.lengthOf @(Obj.Image al img imgnm) $ Vk.Bffr.lengthBinded bf

createDscPl :: Natural -> Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDscPl (fromIntegral -> mff) dv = Vk.DscPl.create dv info nil
	where
	info = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = mff * 3,
		Vk.DscPl.createInfoPoolSizes = [sz0, sz1, sz2, sz3] }
	sz0 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPl.sizeDescriptorCount = mff }
	sz1 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBufferDynamic,
		Vk.DscPl.sizeDescriptorCount = mff }
	sz2 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPl.sizeDescriptorCount = mff }
	sz3 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscPl.sizeDescriptorCount = mff }

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
	Obj.OffsetRange obj objs 0, Show (HPList.PL Obj.Length objs) ) =>
	Vk.DscSt.D sds slbts -> Vk.Bffr.Binded sm sb nm objs -> Vk.Dsc.Type ->
	Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgBuffer '[ '(sm, sb, nm, obj, 0)]) 0
dscWrite ds b tp = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = tp,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos
		. HPList.Singleton . U5 $ Vk.Dsc.BufferInfo b }

createTxImg :: forall sd sc img inm a .
	(BObj.IsImage img, Vk.T.FormatToValue (BObj.ImageFormat img)) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> img ->
	(forall sm si .
		Vk.Img.Binded sm si inm (BObj.ImageFormat img) -> IO a) -> IO a
createTxImg pd d gq cp img a =
	prepareImg pd d Vk.Img.TilingOptimal
		(Vk.Img.UsageTransferDstBit .|. Vk.Img.UsageSampledBit)
		Vk.Mm.PropertyDeviceLocalBit w h \i _m -> do
	createBffrImg pd d
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) img
		\(b :: Vk.Bffr.Binded sm sb inm '[bimg]) bm -> do
		Vk.Mm.write @inm @bimg @0 d bm zeroBits img
		transitionImgLyt d gq cp i
			Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal
		copyBffrToImg d gq cp b i
	transitionImgLyt d gq cp i
		Vk.Img.LayoutTransferDstOptimal
		Vk.Img.LayoutShaderReadOnlyOptimal
	a i
	where
	w = fromIntegral $ BObj.imageWidth img
	h = fromIntegral $ BObj.imageHeight img

createDscStTx :: (
	Default (HPList.PL (HPList.PL BObj.Length)
		(Vk.DSLt.BindingTypeListBufferOnlyDynamics dsla)),
	Vk.DscSt.BindingAndArrayElemImage dsla '[Tx] 0 ) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp -> Vk.DSLt.D (sdsl :: Type) dsla ->
	Vk.ImgVw.I TxName TxFormat siv ->
	(forall sds . Vk.DscSt.D sds '(sdsl, dsla) -> IO a) -> IO a
createDscStTx d dp dsl tv a =
	allocateDscStsTx d dp dsl \ds -> createTxSmplr d \sp ->
	Vk.DscSt.updateDs d
		(HPList.Singleton . U5 $ dscWriteTx ds tv sp) HPList.Nil >>
	a ds

allocateDscStsTx ::
	Default (HPList.PL (HPList.PL BObj.Length)
		(Vk.DSLt.BindingTypeListBufferOnlyDynamics dsla)) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp -> Vk.DSLt.D sdsl dsla ->
	(forall sds . Vk.DscSt.D sds '(sdsl, dsla) -> IO a) -> IO a
allocateDscStsTx dv dp dsl a =
	Vk.DscSt.allocateDs dv info \(HPList.Singleton dss) -> a dss
	where info = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }

createTxSmplr :: Vk.Dvc.D sd -> (forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTxSmplr dv = Vk.Smplr.create dv info nil
	where info = Vk.Smplr.CreateInfo {
		Vk.Smplr.createInfoNext = TMaybe.N,
		Vk.Smplr.createInfoFlags = zeroBits,
		Vk.Smplr.createInfoMagFilter = Vk.FilterNearest,
		Vk.Smplr.createInfoMinFilter = Vk.FilterNearest,
		Vk.Smplr.createInfoMipmapMode = Vk.Smplr.MipmapModeLinear,
		Vk.Smplr.createInfoAddressModeU = Vk.Smplr.AddressModeRepeat,
		Vk.Smplr.createInfoAddressModeV = Vk.Smplr.AddressModeRepeat,
		Vk.Smplr.createInfoAddressModeW = Vk.Smplr.AddressModeRepeat,
		Vk.Smplr.createInfoMipLodBias = 0,
		Vk.Smplr.createInfoAnisotropyEnable = False,
		Vk.Smplr.createInfoMaxAnisotropy = 0,
		Vk.Smplr.createInfoCompareEnable = False,
		Vk.Smplr.createInfoCompareOp = Vk.CompareOpAlways,
		Vk.Smplr.createInfoMinLod = 0,
		Vk.Smplr.createInfoMaxLod = 0,
		Vk.Smplr.createInfoBorderColor = Vk.BorderColorIntOpaqueBlack,
		Vk.Smplr.createInfoUnnormalizedCoordinates = False }

dscWriteTx ::
	Vk.DscSt.D sds slbts -> Vk.ImgVw.I nm TxFormat si -> Vk.Smplr.S ss ->
	Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgImage '[ '(ss, nm, TxFormat, si)]) 0
dscWriteTx dss tiv tsmp = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = dss,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeCombinedImageSampler,
	Vk.DscSt.writeSources =
		Vk.DscSt.ImageInfos . HPList.Singleton $ U4 info }
	where info = Vk.Dsc.ImageInfo {
		Vk.Dsc.imageInfoImageLayout =
			Vk.Img.LayoutShaderReadOnlyOptimal,
		Vk.Dsc.imageInfoImageView = tiv,
		Vk.Dsc.imageInfoSampler = tsmp }

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
	HPList.HomoList '(sdslo, DscStLytArgOd als) dlaso,
	HPList.HomoList '() mff, KnownNat mffn,
	KnownNat alu, KnownNat als, KnownNat alvmk, KnownNat alvtr ) =>
	Natural -> FramebufferResized -> GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I inm scfmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[	'(sdsl, DscStLytArg alu mffn),
			'(sdslo, DscStLytArgOd als),
			'(sdslt, DscStLytArgTx) ] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[	'(sdsl, DscStLytArg alu mffn),
				'(sdslo, DscStLytArgOd als),
				'(sdslt, DscStLytArgTx) ],
			'[WMeshPushConsts]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdiv ->
	VtxBffr smvmk sbvmk bnmvmk alvmk nmvmk ->
	VtxBffr smvtr sbvtr bnmvtr alvtr nmvtr ->
	HPList.PL (MemVp alu bnmvp) sbsms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg bnmsn '[AtmScene alu mffn])] ->
	HPList.PL (Vk.DscSt.D sds) dlas ->
	HPList.PL (MemOd als bnmod) sods -> HPList.PL (Vk.DscSt.D sdso) dlaso ->
	Vk.DscSt.D sdst '(sdslt, DscStLytArgTx) ->
	HPList.LL (Vk.CBffr.C scb) mff -> SyncObjs ssoss -> IO ()
mainloop (fromIntegral -> mff) fr w sfc pd qfis dv gq pq cp
	sc ex0 vs rp pl gp fbs drs vbmk vbtr vpms snm dss odms dsso dst
	cbs sos = do
	($ 0) . ($ Inf.cycle $ NE.fromList [0 .. mff - 1]) . ($ ex0) $ fix
		\go ex (cf :~ cfs) fn -> do
		GlfwG.pollEvents
		run fr w sfc pd qfis dv gq pq cp
			sc ex vs rp pl gp fbs drs vbmk vbtr vpms snm
			dss odms dsso dst cbs sos cf fn
			(\e -> go e cfs ((fn + 1) `mod` (3600 * frashRate)))
	Vk.Dvc.waitIdle dv

frashRate :: Num n => n
frashRate = 2

type VtxBffr smv sbv bnmv alv nmv = (
	Vk.Bffr.Binded smv sbv bnmv '[Obj.List alv WVertex nmv],
	Vk.Cmd.VertexCount )

run :: (
	HPList.HomoList '() mff, RecreateFrmbffrs svs sfs,
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	HPList.HomoList '(sdsl, DscStLytArg alu mffn) dlas,
	HPList.HomoList '(sdslo, DscStLytArgOd als) dlaso,
	KnownNat mffn,
	KnownNat alu, KnownNat als, KnownNat alvmk, KnownNat alvtr ) =>
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I inm scfmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[	'(sdsl, DscStLytArg alu mffn),
			'(sdslo, DscStLytArgOd als), '(sdslt, DscStLytArgTx) ]
		'[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[	'(sdsl, DscStLytArg alu mffn),
				'(sdslo, DscStLytArgOd als),
				'(sdslt, DscStLytArgTx) ],
			'[WMeshPushConsts]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdiv ->
	VtxBffr smvmk sbvmk bnmvmk alvmk nmvmk ->
	VtxBffr smvtr sbvtr bnmvtr alvtr nmvtr ->
	HPList.PL (MemVp alu bnmvp) sbsms ->
	Vk.Mm.M ssm '[ '(ssb, 'Vk.Mm.BufferArg bnmsn '[AtmScene alu mffn])] ->
	HPList.PL (Vk.DscSt.D sds) dlas ->
	HPList.PL (MemOd als bnmod) sods -> HPList.PL (Vk.DscSt.D sdso) dlaso ->
	Vk.DscSt.D sdst '(sdslt, DscStLytArgTx) ->
	HPList.LL (Vk.CBffr.C scb) mff -> SyncObjs ssoss ->
	Int -> Int -> (Vk.Extent2d -> IO ()) -> IO ()
run fr w sfc pd qfis dv gq pq cp
	sc ex vs rp pl gp fbs drs vbmk vbtr vpms snm dss odms dsso dst cbs soss
	cf fn go = do
	catchAndRecreate w sfc pd qfis dv gq cp sc vs rp pl gp drs fbs go
		$ draw dv gq pq sc ex rp pl
			gp fbs vbmk vbtr vpms snm dss odms dsso dst
			cbs soss cf fn
	(,) <$> GlfwG.Win.shouldClose w <*> checkFlag fr >>= \case
		(True, _) -> pure (); (_, False) -> go ex
		(_, _) -> go =<< recreateAll
			w sfc pd qfis dv gq cp sc vs rp pl gp drs fbs

draw :: forall
	sd fmt ssc sr sl sdsl sdslo sdslt alu als mffn sg sfs
	smvmk sbvmk bnmvmk alvmk nmvmk smvtr sbvtr bnmvtr alvtr nmvtr
	bnmvp smsbs smsn sbsn bnmsn sds dlas bnmod smsbso sdso dlaso sdst
	scb mff ssoss .  (
	HPList.HomoList '(sdsl, DscStLytArg alu mffn) dlas,
	HPList.HomoList '(sdslo, DscStLytArgOd als) dlaso,
	HPList.HomoList '() mff,
	KnownNat mffn,
	KnownNat alu, KnownNat als, KnownNat alvmk, KnownNat alvtr ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S fmt ssc ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[	'(sdsl, DscStLytArg alu mffn),
			'(sdslo, DscStLytArgOd als), '(sdslt, DscStLytArgTx) ]
		'[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[	'(sdsl, DscStLytArg alu mffn),
				'(sdslo, DscStLytArgOd als),
				'(sdslt, DscStLytArgTx) ],
			'[WMeshPushConsts]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	VtxBffr smvmk sbvmk bnmvmk alvmk nmvmk ->
	VtxBffr smvtr sbvtr bnmvtr alvtr nmvtr ->
	HPList.PL (MemVp alu bnmvp) smsbs ->
	Vk.Mm.M smsn '[ '(sbsn, 'Vk.Mm.BufferArg bnmsn '[AtmScene alu mffn])] ->
	HPList.PL (Vk.DscSt.D sds) dlas ->
	HPList.PL (MemOd als bnmod) smsbso ->
	HPList.PL (Vk.DscSt.D sdso) dlaso ->
	Vk.DscSt.D sdst '(sdslt, DscStLytArgTx) ->
	HPList.LL (Vk.CBffr.C scb) mff -> SyncObjs ssoss -> Int -> Int -> IO ()
draw dv gq pq sc ex rp pl gp fbs
	vbmk vbtr vpms snm dss odms dsso dst
	cbs (SyncObjs iass rfss iffs) cf fn =
	HPList.index iass cf \ias -> HPList.index rfss cf \rfs ->
	HPList.index iffs cf \(id &&& HPList.Singleton -> (iff, siff)) ->
	HPList.index vpms cf \(MemVp vpm) ->
	HPList.index odms cf \(MemOd odm) ->
	($ HPList.homoListIndex dss cf) \ds ->
	($ HPList.homoListIndex dsso cf) \dso -> do
	Vk.Fnc.waitForFs dv siff True Nothing >> Vk.Fnc.resetFs dv siff
	Vk.Mm.write @bnmvp @(AtmViewProj alu) @0 dv vpm zeroBits (viewProjData ex)
	Vk.Mm.write @bnmsn @(AtmScene alu mffn) @0 dv snm zeroBits . (!! cf)
		$ iterate (Nothing :) [Just $ sceneData fn]
	Vk.Mm.write @bnmod @(ListObjData als) @0 dv odm zeroBits
		. map (GStorable.W . ObjData)
		$ model (fromIntegral fn) :
			[ objMtx x y | x <- [- 20 .. 20], y <- [- 20 .. 20] ]
	ii <- Vk.Khr.acquireNextImageResult
		[Vk.Success, Vk.SuboptimalKhr] dv sc maxBound (Just ias) Nothing
	Vk.CBffr.reset cb zeroBits
	HPList.index fbs ii \fb ->
		recordCmdBffr cb ex rp pl gp fb vbmk vbtr cf fn ds dso dst
	Vk.Q.submit gq (HPList.Singleton . U4 $ sinfo ias rfs) $ Just iff
	catchAndSerialize . Vk.Khr.queuePresent pq $ pinfo rfs ii
	where
	HPList.Dummy cb = HPList.homoListIndex @'() cbs cf
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
	objMtx x y = trans x y `Glm.mat4Mul` scale
	catchAndSerialize = (`catch`
		\(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

recordCmdBffr :: forall
	scb sr sl sdsl sdslo sdslt sds sdso sdst alu als sg sf
	smvmk sbvmk bnmvmk alvmk nmvmk smvtr sbvtr bnmvtr alvtr nmvtr mffn .
	(KnownNat alu, KnownNat alvmk, KnownNat alvtr) =>
	Vk.CBffr.C scb -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[	'(sdsl, DscStLytArg alu mffn),
			'(sdslo, DscStLytArgOd als), '(sdslt, DscStLytArgTx) ]
		'[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[	'(sdsl, DscStLytArg alu mffn),
				'(sdslo, DscStLytArgOd als),
				'(sdslt, DscStLytArgTx) ],
			'[WMeshPushConsts]) ->
	Vk.Frmbffr.F sf ->
	VtxBffr smvmk sbvmk bnmvmk alvmk nmvmk ->
	VtxBffr smvtr sbvtr bnmvtr alvtr nmvtr -> Int -> Int ->
	Vk.DscSt.D sds '(sdsl, DscStLytArg alu mffn) ->
	Vk.DscSt.D sdso '(sdslo, DscStLytArgOd als) ->
	Vk.DscSt.D sdst '(sdslt, DscStLytArgTx) -> IO ()
recordCmdBffr cb ex rp pl gp fb (vbmk, vnmk) (vbtr, vntr)
	(fromIntegral -> cf) (fromIntegral -> fn) ds dso dst =
	Vk.CBffr.begin cb binfo $
	Vk.Cmd.beginRenderPass cb rpinfo Vk.Subpass.ContentsInline do
	ovmk <- newIORef Nothing
	drawObj ovmk cb ds dso dst RenderObj {
		renderObjPipelineLayout = pl, renderObjPipeline = gp,
		renderObjMesh = vbmk, renderObjMeshSize = vnmk,
		renderObjTransMtx = model fn } cf 0
	ovtr <- newIORef Nothing
	for_ [- 20 .. 20] \x -> for_ [- 20 .. 20] \y ->
		drawObj ovtr cb ds dso dst RenderObj {
			renderObjPipelineLayout = pl, renderObjPipeline = gp,
			renderObjMesh = vbtr, renderObjMeshSize = vntr,
			renderObjTransMtx = trans x y `Glm.mat4Mul` scale } cf
				((round x + 20) * 41 + (round y + 20) + 1)
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
			Vk.rect2dExtent = ex },
		Vk.RndrPss.beginInfoClearValues =
			Vk.ClearValueColor (fromJust $ rgbaDouble 0 0 0 1) :**
			Vk.ClearValueDepthStencil
				(Vk.ClearDepthStencilValue 1 0) :** HPList.Nil }

model :: Float -> Glm.Mat4
model fn = Glm.rotate
	Glm.mat4Identity (fn / 10 * Glm.rad 1) (Glm.Vec3 $ 0 :. 1 :. 0 :. NilL)

trans :: Float -> Float -> Glm.Mat4
trans x y = Glm.translate Glm.mat4Identity (Glm.Vec3 $ x :. 0 :. y :. NilL)

scale :: Glm.Mat4
scale = Glm.scale Glm.mat4Identity (Glm.Vec3 $ 0.2 :. 0.2 :. 0.2 :. NilL)

drawObj :: forall
	scb sl sg sds sdsl alu mffn sdso sdslo als alv smv sbv bnmv nmv
	sdst sdslt .
	(KnownNat alu, KnownNat alv) =>
	IORef (Maybe
		(Vk.Bffr.Binded smv sbv bnmv '[Obj.List alv WVertex nmv])) ->
	Vk.CBffr.C scb -> Vk.DscSt.D sds '(sdsl, DscStLytArg alu mffn) ->
	Vk.DscSt.D sdso '(sdslo, DscStLytArgOd als) ->
	Vk.DscSt.D sdst '(sdslt, DscStLytArgTx) ->
	RenderObj sl sdsl alu mffn sdslo als sg alv smv sbv bnmv nmv sdslt ->
	Word32 -> Word32 -> IO ()
drawObj ovb cb ds dso dst RenderObj {
	renderObjPipelineLayout = pl, renderObjPipeline = gp,
	renderObjMesh = vb, renderObjMeshSize = vn,
	renderObjTransMtx = mdl } cf i =
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gp \cbb -> do
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics pl
		(U2 ds :** U2 dso :** U2 dst :** HPList.Nil) $
		(	HPList.Nil :**
			(Vk.Cmd.DynamicIndex cf :** HPList.Nil) :**
			HPList.Nil ) :**
		(HPList.Nil :** HPList.Nil) :**
		(HPList.Nil :** HPList.Nil) :** HPList.Nil
	readIORef ovb >>= \case
		Just o | vb == o -> pure ()
		_ -> do	Vk.Cmd.bindVertexBuffers cbb . HPList.Singleton . U5
				$ Vk.Bffr.IndexedForList
					@_ @_ @_ @WVertex @nmv vb
			writeIORef ovb $ Just vb
	Vk.Cmd.pushConstantsGraphics @'[ 'Vk.T.ShaderStageVertexBit] cbb pl
		$ HPList.Id (GStorable.W MeshPushConsts {
			meshPushConstsDt = Glm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
			meshPushConstsRenderMtx = mdl }) :** HPList.Nil
	Vk.Cmd.draw cbb vn 1 0 i

data RenderObj
	sl sdsl alu mffn sdslo als sg alv smv sbv bnmv nmv sdslt = RenderObj {
	renderObjPipelineLayout :: Vk.PplLyt.P sl
		'[	'(sdsl, DscStLytArg alu mffn),
			'(sdslo, DscStLytArgOd als),
			'(sdslt, DscStLytArgTx) ] '[WMeshPushConsts],
	renderObjPipeline :: Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[	'(sdsl, DscStLytArg alu mffn),
				'(sdslo, DscStLytArgOd als),
				'(sdslt, DscStLytArgTx) ], '[WMeshPushConsts]),
	renderObjMesh ::
		Vk.Bffr.Binded smv sbv bnmv '[Obj.List alv WVertex nmv],
	renderObjMeshSize :: Word32, renderObjTransMtx :: Glm.Mat4 }

catchAndRecreate :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFrmbffrs svs sfs ) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QFamIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Khr.Swpch.S scfmt ssc -> HPList.PL (Vk.ImgVw.I nm scfmt) svs ->
	Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[	'(sdsl, DscStLytArg alu mffn),
			'(sdslo, DscStLytArgOd als),
			'(sdslt, DscStLytArgTx) ] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[	'(sdsl, DscStLytArg alu mffn),
				'(sdslo, DscStLytArgOd als),
				'(sdslt, DscStLytArgTx)], '[WMeshPushConsts]) ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdvs ->
	HPList.PL Vk.Frmbffr.F sfs -> (Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate w sfc pd qfis dv gq cp sc vs rp pl gp drs fbs go act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just (); _ -> Nothing) act \_ ->
	go =<< recreateAll w sfc pd qfis dv gq cp sc vs rp pl gp drs fbs

recreateAll :: (
	Vk.T.FormatToValue fmt, Vk.T.FormatToValue dptfmt,
	RecreateFrmbffrs svs sfs ) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QFamIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> Vk.Khr.Swpch.S fmt ssc ->
	HPList.PL (Vk.ImgVw.I nm fmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl
		'[	'(sdsl, DscStLytArg alu mffn),
			'(sdslo, DscStLytArgOd als),
			'(sdslt, DscStLytArgTx) ] '[WMeshPushConsts] ->
	Vk.Ppl.Grph.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Position), '(1, Normal), '(2, Color), '(3, Uv)]
		'(sl,	'[	'(sdsl, DscStLytArg alu mffn),
				'(sdslo, DscStLytArgOd als),
				'(sdslt, DscStLytArgTx)], '[WMeshPushConsts]) ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdvs ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateAll w sfc pd qfis dv gq cp sc vs rp pl gp drs@(_, _, dvw) fbs =
	waitFramebufferSize w >> Vk.Dvc.waitIdle dv >>
	recreateSwpch w sfc pd qfis dv sc >>= \ex -> ex <$ do
	Vk.Khr.Swpch.getImages dv sc >>= \i -> recreateImgVws dv i vs
	recreateDptRsrcs pd dv gq cp ex drs
	recreateGrPpl dv ex rp pl gp
	recreateFrmbffrs dv ex rp vs dvw fbs

waitFramebufferSize :: GlfwG.Win.W sw -> IO ()
waitFramebufferSize w = GlfwG.Win.getFramebufferSize w >>= \sz ->
	when (zero sz) $ fix \go -> (`when` go) . zero =<<
		GlfwG.waitEvents *> GlfwG.Win.getFramebufferSize w
	where zero = uncurry (||) . ((== 0) *** (== 0))

-- VERTEX

triangle :: V.Vector WVertex
triangle = V.fromList $ GStorable.W <$> [
	Vtx {	vtxPos = Position . Glm.Vec3 $ 1 :. 1 :. 0.5 :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vtxColor = Color . Glm.Vec3 $ 0 :. 1 :. 0 :. NilL,
		vtxUv = Uv . Glm.Vec2 $ 0 :. 0 :. NilL },
	Vtx {	vtxPos = Position . Glm.Vec3 $ (- 1) :. 1 :. 0.5 :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vtxColor = Color . Glm.Vec3 $ 0 :. 1 :. 0 :. NilL,
		vtxUv = Uv . Glm.Vec2 $ 0 :. 0 :. NilL },
	Vtx {	vtxPos = Position . Glm.Vec3 $ 0 :. (- 1) :. 0.5 :. NilL,
		vtxNormal = Normal . Glm.Vec3 $ 1 :. 0 :. 0 :. NilL,
		vtxColor = Color . Glm.Vec3 $ 0 :. 1 :. 0 :. NilL,
		vtxUv = Uv . Glm.Vec2 $ 0 :. 0 :. NilL } ]

type WVertex = GStorable.W Vertex

data Vertex = Vtx {
	vtxPos :: Position, vtxNormal :: Normal, vtxColor :: Color,
	vtxUv :: Uv }
	deriving (Show, Generic)

newtype Position = Position Glm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Normal = Normal Glm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Color = Color Glm.Vec3
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

newtype Uv = Uv Glm.Vec2
	deriving (Show, Storable, Vk.Ppl.VtxIptSt.Formattable)

instance GStorable.G Vertex

-- MESH PUSH CONSTANTS

type WMeshPushConsts = GStorable.W MeshPushConsts

data MeshPushConsts = MeshPushConsts {
	meshPushConstsDt :: Glm.Vec4,
	meshPushConstsRenderMtx :: Glm.Mat4 } deriving (Show, Generic)

instance GStorable.G MeshPushConsts

-- CAMERA DATA

type WViewProj = GStorable.W ViewProjData

data ViewProjData = ViewProjData {
	viewProjView :: View, viewProjProj :: Proj,
	viewProjViewProj :: ViewProj } deriving (Show, Generic)

newtype View = View Glm.Mat4 deriving (Show, Storable)
newtype Proj = Proj Glm.Mat4 deriving (Show, Storable)
newtype ViewProj = ViewProj Glm.Mat4 deriving (Show, Storable)

instance GStorable.G ViewProjData

viewProjData :: Vk.Extent2d -> WViewProj
viewProjData ex = GStorable.W
	$ ViewProjData (View view) (Proj prj) (ViewProj $ Glm.mat4Mul prj view)
	where prj = projection ex

view :: Glm.Mat4
view = Glm.lookat
	(Glm.Vec3 $ 0 :. 20 :. 20 :. NilL)
	(Glm.Vec3 $ 0 :. 10 :. 0 :. NilL)
	(Glm.Vec3 $ 0 :. 1 :. 0 :. NilL)

projection :: Vk.Extent2d -> Glm.Mat4
projection Vk.Extent2d {
	Vk.extent2dWidth = fromIntegral -> w,
	Vk.extent2dHeight = fromIntegral -> h } =
	Glm.modifyMat4 1 1 negate $ Glm.perspective (Glm.rad 70) (w / h) 0.1 200

-- SCENE DATA

type WScene = GStorable.W Scene

data Scene = Scene {
	sceneFogColor :: FogColor, sceneFogDists :: FogDists,
	sceneAmbColor :: AmbColor,
	sceneSunDir :: SunDir, sceneSunColor :: SunColor }
	deriving (Show, Generic)

newtype FogColor = FogColor Glm.Vec4 deriving (Show, Storable)
newtype FogDists = FogDists Glm.Vec4 deriving (Show, Storable)
newtype AmbColor = AmbColor Glm.Vec4 deriving (Show, Storable)
newtype SunDir = SunDir Glm.Vec4 deriving (Show, Storable)
newtype SunColor = SunColor Glm.Vec4 deriving (Show, Storable)

instance GStorable.G Scene

sceneData :: Int -> WScene
sceneData (fromIntegral -> fn) = GStorable.W Scene {
	sceneFogColor = FogColor . Glm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	sceneFogDists = FogDists . Glm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	sceneAmbColor = AmbColor . Glm.Vec4 $ r :. 0 :. b :. 0 :. NilL,
	sceneSunDir = SunDir . Glm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL,
	sceneSunColor = SunColor . Glm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL }
	where
	r = sin $ fn / (180 * frashRate) * pi
	b = cos $ fn / (180 * frashRate) * pi

-- OBJECT DATA

type WObjData = GStorable.W ObjData

newtype ObjData =
	ObjData { objectDataModelMatrix :: Glm.Mat4 }
	deriving (Show, Generic)

instance GStorable.G ObjData
	
-- OTHER TYPES

-- TEXTURE

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
	type ImageFormat ImageRgba8 = TxFormat
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

-- SHADER

[glslVertexShader|

#version 460

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec3 inColor;
layout(location = 3) in vec2 vTexCoord;

layout(location = 0) out vec3 outColor;
layout(location = 1) out vec2 texCoord;

layout (set = 0, binding = 0) uniform
	ViewProj { mat4 view; mat4 proj; mat4 viewproj; } viewProj;

layout(push_constant) uniform
	Constants { vec4 data; mat4 render_matrix; } pushConsts;

struct ObjectData { mat4 model; };

layout (std140, set = 1, binding = 0) readonly buffer
	ObjectBuffer { ObjectData objects[]; } objectBuffer;

void
main()
{
	mat4 modelMatrix = objectBuffer.objects[gl_BaseInstance].model;
	mat4 transformMatrix = viewProj.viewproj * modelMatrix;
	gl_Position = transformMatrix * vec4(inPosition, 1.0);
	outColor = inColor;
	texCoord = vTexCoord;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec3 inColor;
layout(location = 1) in vec2 texCoord;

layout(location = 0) out vec4 outColor;

layout (set = 0, binding = 1) uniform SceneData {
	vec4 fogColor; vec4 fogDists;
	vec4 ambientColor;
	vec4 sunlightDir; vec4 sunlightColor; } sceneData;

layout (set = 2, binding = 0) uniform sampler2D tex1;

void
main()
{
	vec3 color = texture(tex1, texCoord).xyz;
//	outColor = vec4(texCoord.x, texCoord.y, 0.5f, 1.0f);
	outColor = vec4(color, 1.0f);
}

|]
