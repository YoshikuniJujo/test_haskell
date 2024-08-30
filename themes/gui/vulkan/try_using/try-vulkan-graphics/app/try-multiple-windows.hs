{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.Generics
import GHC.TypeLits (Symbol)
import GHC.TypeNats
import Foreign.Storable
import Foreign.Storable.Generic qualified
import Foreign.Storable.PeekPoke
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent.STM
import Control.Concurrent.STM.ToolsYj
import Control.Exception
import Data.Kind
import Data.Proxy
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.Tuple.Uncurry
import Data.Default
import Data.Foldable
import Data.Traversable
import Data.Ord.ToolsYj
import Data.Bits
import Data.Bits.ToolsYj
import Data.Tuple.ToolsYj
import Data.Maybe
import Data.Maybe.ToolsYj
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.List.Length
import Data.List.ToolsYj
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.Bool
import Data.Bool.ToolsYj
import Data.Word
import Data.Text.IO qualified as Txt
import Data.Color

import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH
import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Exception qualified as Vk
import Gpu.Vulkan.AllocationCallbacks qualified as AllocationCallbacks
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
import Gpu.Vulkan.Pipeline.Graphics qualified as Vk.Ppl.Gr
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.InputAssemblyState qualified as Vk.Ppl.InpAsmbSt
import Gpu.Vulkan.Pipeline.ViewportState qualified as Vk.Ppl.ViewportSt
import Gpu.Vulkan.Pipeline.RasterizationState qualified as Vk.Ppl.RstSt
import Gpu.Vulkan.Pipeline.MultisampleState qualified as Vk.Ppl.MltSmplSt
import Gpu.Vulkan.Pipeline.ColorBlendAttachment qualified as Vk.Ppl.ClrBlndAtt
import Gpu.Vulkan.Pipeline.ColorBlendState qualified as Vk.Ppl.ClrBlndSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.PplLyt
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderModule
import Gpu.Vulkan.VertexInput qualified as Vk.VtxInp
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.ColorComponent qualified as Vk.ClrCmp
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPss
import Gpu.Vulkan.Attachment qualified as Vk.Att
import Gpu.Vulkan.Subpass qualified as Vk.Sbp

import Gpu.Vulkan.Cglm qualified as Cglm
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch
import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.DbgUtls.Msngr

import Debug

main :: IO ()
main = GlfwG.init error $ createIst \ist -> bool id (dbgm ist) debug $
	withDvc ist \(_ :: Proxy scfmt) (_ :: Proxy scin) pd qfis dv gq pq ->
	body @scfmt @scin ist pd qfis dv gq pq
	where dbgm i = Vk.DbgUtls.Msngr.create i dbgMsngrInfo nil

type FramebufferResized = TVar Bool

initWindow :: Ord k => Bool -> GlfwG.Win.Group sw k -> k -> IO (GlfwG.Win.W sw)
initWindow v wg k = do
	Right w <- do
		GlfwG.Win.hint noApi
		bool nov id v $ uncurryDup
			(GlfwG.Win.create' wg k) sizeName Nothing Nothing
	pure w
	where
	noApi = GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI
	nov a = do
		GlfwG.Win.hint $ GlfwG.Win.WindowHint'Visible False
		a <* GlfwG.Win.hint (GlfwG.Win.WindowHint'Visible True)
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

withDvc :: Vk.Ist.I si -> (forall (scfmt :: Vk.T.Format) sd (n :: [()]) .
	(Vk.T.FormatToValue scfmt, HPList.HomoListN n) =>
	Proxy scfmt -> Proxy n -> Vk.Phd.P -> QFamIndices ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> IO a) -> IO a
withDvc ist a =
	GlfwG.Win.group \dwg -> initWindow False dwg () >>= \dw ->
	Vk.Khr.Sfc.group ist nil \dsfcg ->
	Vk.Khr.Sfc.Glfw.Win.create' dsfcg () dw >>= \(fromRight -> dsfc) ->
	pickPhd ist dsfc >>= \(pd, qfis) ->
	querySwpchSupport pd dsfc \ss ->
	swapExtent dw (capabilities ss) >>= \ex ->
	createLgDvc pd qfis \dv gq pq ->
	chooseSwpSfcFmt (formats ss)
		\(Vk.Khr.Sfc.Format cs :: Vk.Khr.Sfc.Format fmt) ->
	let	pm = findDefault Vk.Khr.Sfc.PresentModeFifo
			(== Vk.Khr.Sfc.PresentModeMailbox) $ presentModes ss in
	Vk.Khr.Swpch.group dv nil \scg ->
	createSwpch @fmt scg () dsfc qfis (capabilities ss) cs pm ex >>= \sc ->
	Vk.Khr.Swpch.getImages dv sc >>= \n -> HPList.tnum n \pn ->
	Vk.Khr.Sfc.unsafeDestroy dsfcg () >> GlfwG.Win.unsafeDestroy dwg () >>
	a (Proxy :: Proxy fmt) pn pd qfis dv gq pq

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

body :: forall scfmt (scin :: [()]) si sd .
	(Vk.T.FormatToValue scfmt, HPList.HomoListN scin) =>
	Vk.Ist.I si -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> IO ()
body ist pd qfis dv gq pq = atomically (newTVar @Int 0) >>= \wn ->
	createCmdPl qfis dv \cp -> createCmdBffr dv cp \cb ->
	createPplLyt dv \pl ->
	bffrLstAlgn @WVertex dv (fromIntegral $ length vertices)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		\(_ :: Proxy alv) ->
	Vk.Bffr.group dv nil \bfg -> Vk.Mm.group dv nil \mmg ->
	(for [(0, vertices), (1, vertices2)] \(i, v) ->
		createVtxBffr @Int @alv pd dv gq cp bfg mmg i v) >>= \vbs ->
	winGroups @_ @_ @_ @scfmt ist dv \(wgs :: WinGrs
		sw si sd ssfc sr sg sl sias srfs siff scfmt ssc sv nm sf k) ->
	mainloop @scin @sv @sf pd qfis dv gq pq cb pl (cycle vbs)
		$ createWinRsrcs @_ @scin pd dv qfis pl wgs
			=<< atomically (readModifyTVar wn (+ 1))

winGroups :: Vk.Ist.I si -> Vk.Dvc.D sd -> (
	forall sw ssfc sr sg sias srfs siff ssc siv sf .
	WinGrs sw si sd ssfc sr sg sl sias srfs siff scifmt ssc siv nm sf k ->
	IO a) -> IO a
winGroups ist dv f =
	GlfwG.Win.group \wg -> Vk.Khr.Sfc.group ist nil \sfcg ->
	Vk.RndrPss.group dv nil \rpg -> Vk.Ppl.Gr.group dv nil \gpg ->
	Vk.Semaphore.group dv nil \iasg -> Vk.Semaphore.group dv nil \rfsg ->
	Vk.Fence.group dv nil \iffg ->
	Vk.Khr.Swpch.group dv nil \scg ->
	Vk.ImgVw.group dv nil \ivg -> Vk.Frmbffr.group dv nil \fbg ->
	f $ WinGrs wg sfcg rpg gpg iasg rfsg iffg scg ivg fbg

data WinGrs sw si sd ssfc sr sg sl sias srfs siff scfmt ssc siv nm sf k = WinGrs
	(GlfwG.Win.Group sw k) (Vk.Khr.Sfc.Group si 'Nothing ssfc k)
	(Vk.RndrPss.Group sd 'Nothing sr k)
	(Vk.Ppl.Gr.Group sd 'Nothing sg k '[ '(
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)],
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)], '(sl, '[], '[]) )] )
	(Vk.Semaphore.Group sd 'Nothing sias k)
	(Vk.Semaphore.Group sd 'Nothing srfs k)
	(Vk.Fence.Group sd 'Nothing siff k)
	(Vk.Khr.Swpch.Group sd 'Nothing scfmt ssc k)
	(Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm scfmt)
	(Vk.Frmbffr.Group sd 'Nothing sf (k, Int))

createWinRsrcs :: forall
	scfmt scin si siv sd sl sw nm ssfc sr sg sias srfs siff ssc sf k .
	(Vk.T.FormatToValue scfmt, HPList.HomoListN scin, Ord k) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> QFamIndices -> Vk.PplLyt.P sl '[] '[] ->
	WinGrs sw si sd ssfc sr sg sl sias srfs siff scfmt ssc siv nm sf k ->
	k -> IO (WinParams
		sw sl nm ssfc sr sg sias srfs siff scfmt ssc
		(HPList.Replicate scin siv) (HPList.Replicate scin sf))
createWinRsrcs
	pd dv qfis pl (WinGrs wg sfcg rpg gpg iasg rfsg iffg scg ivg fbg) k =
	atomically (newTVar False) >>= \fr ->
	initWindow True wg k >>= \w ->
	GlfwG.Win.setFramebufferSizeCallback w
		(Just \_ _ _ -> atomically $ writeTVar fr True) >>
	Vk.Khr.Sfc.Glfw.Win.create' sfcg k w >>= \(fromRight -> sfc) ->
	querySwpchSupportFmt @scfmt pd sfc >>= \ss ->
	swapExtent w (capabilitiesFmt ss) >>= \ex ->
	let	Vk.Khr.Sfc.Format cs = fromMaybe
			(error "no available swap surface formats")
			. listToMaybe $ formatsFmt ss
		pm = findDefault Vk.Khr.Sfc.PresentModeFifo
			(== Vk.Khr.Sfc.PresentModeMailbox) $ presentModesFmt ss
		in
	createSwpch scg k sfc qfis (capabilitiesFmt ss) cs pm ex >>= \sc ->
	createRndrPss @scfmt rpg k >>= \rp ->
	createGrPpl gpg k ex rp pl >>= \gpl ->
	Vk.Khr.Swpch.getImages dv sc >>= \scis ->
	createImgVws @scin ivg k scis >>= \scivs ->
	createFrmbffrs @scin @siv fbg k ex rp scivs >>= \fbs ->
	createSyncObjs iasg rfsg iffg k >>= \sos ->
	atomically (newTVar ex) >>= \vex ->
	pure $ WinParams w fr sfc vex rp gpl sos sc scivs fbs

data WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc ss sfs = WinParams
	(GlfwG.Win.W sw) FramebufferResized (Vk.Khr.Sfc.S ssfc)
	(TVar Vk.Extent2d) (Vk.RndrPss.R sr)
	(Vk.Ppl.Gr.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]))
	(SyncObjs '(sias, srfs, siff))
	(Vk.Khr.Swpch.S fmt ssc)
	(HPList.PL (Vk.ImgVw.I nm fmt) ss)
	(HPList.PL Vk.Frmbffr.F sfs)

winParamsToWin ::
	WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc svs sfs ->
	GlfwG.Win.W sw
winParamsToWin (WinParams w _ _ _ _ _ _ _ _ _) = w

checkFlagWinParams ::
	WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc svs sfs -> IO Bool
checkFlagWinParams (WinParams _ fr _ _ _ _ _ _ _ _) = atomically $ checkFlag fr

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
	info qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames = bool [] vldLayers debug,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures = Just def }
	qinfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }

createSwpch :: forall fmt ssfc sd ma ss k .
	(Ord k, Vk.T.FormatToValue fmt, AllocationCallbacks.ToMiddle ma) =>
	Vk.Khr.Swpch.Group sd ma fmt ss k -> k -> Vk.Khr.Sfc.S ssfc ->
	QFamIndices ->
	Vk.Khr.Sfc.Capabilities -> Vk.Khr.Sfc.ColorSpace ->
	Vk.Khr.Sfc.PresentMode -> Vk.Extent2d -> IO (Vk.Khr.Swpch.S fmt ss)
createSwpch scg k sfc qfis cps cs pm ex =
	fromRight <$> Vk.Khr.Swpch.create' @fmt scg k info
	where info = swpchInfo sfc qfis cps cs pm ex

data SwpchSupportDetails fmts = SwpchSupportDetails {
	capabilities :: Vk.Khr.Sfc.Capabilities,
	formats :: (
		[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
		HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ),
	presentModes :: [Vk.Khr.Sfc.PresentMode] }

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

data SwpchSupportDetailsFmt fmt = SwpchSupportDetailsFmt {
	capabilitiesFmt :: Vk.Khr.Sfc.Capabilities,
	formatsFmt :: [Vk.Khr.Sfc.Format fmt],
	presentModesFmt :: [Vk.Khr.Sfc.PresentMode] } deriving Show

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
	Vk.Khr.Swpch.createInfoCompositeAlpha =
		Vk.Khr.Sfc.CompositeAlphaOpaqueBit,
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

createImgVws :: forall scin sd k sm si ifmt nm vfmt sv .
	(HPList.HomoListN scin, Ord k, Vk.T.FormatToValue vfmt) =>
	Vk.ImgVw.Group sd 'Nothing sv (k, Int) nm vfmt ->
	k -> [Vk.Img.Binded sm si nm ifmt] ->
	IO (HPList.PL (Vk.ImgVw.I nm vfmt) (HPList.Replicate scin sv))
createImgVws vg k is = HPList.homoListNFromList @_ @scin <$>
	for (zip [0 ..] is) \(c, i) ->
		fromRight <$> Vk.ImgVw.create' vg (k, c) (imgVwInfo i)

recreateImgVws :: Vk.T.FormatToValue fmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss inm fmt] ->
	HPList.PL (Vk.ImgVw.I inm fmt) sis -> IO ()
recreateImgVws _dv [] HPList.Nil = pure ()
recreateImgVws dv (i : is) (v :** vs) =
	Vk.ImgVw.unsafeRecreate dv (imgVwInfo i) nil v >>
	recreateImgVws dv is vs
recreateImgVws _ _ _ =
	error "number of Vk.Img.I and Vk.ImageView.I should be same"

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

createRndrPss :: forall fmt sd ma sr k .
	(Vk.T.FormatToValue fmt, AllocationCallbacks.ToMiddle ma, Ord k) =>
	Vk.RndrPss.Group sd ma sr k -> k -> IO (Vk.RndrPss.R sr)
createRndrPss rpg k = fromRight <$> Vk.RndrPss.create' @_ @_ @'[fmt] rpg k info
	where
	info = Vk.RndrPss.CreateInfo {
		Vk.RndrPss.createInfoNext = TMaybe.N,
		Vk.RndrPss.createInfoFlags = zeroBits,
		Vk.RndrPss.createInfoAttachments = ca :** HPList.Nil,
		Vk.RndrPss.createInfoSubpasses = [subpass],
		Vk.RndrPss.createInfoDependencies = [dependency] }
	ca = Vk.Att.Description {
		Vk.Att.descriptionFlags = zeroBits,
		Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
		Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
		Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
		Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
		Vk.Att.descriptionStencilStoreOp = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Att.descriptionFinalLayout = Vk.Img.LayoutPresentSrcKhr }
	subpass = Vk.Sbp.Description {
		Vk.Sbp.descriptionFlags = zeroBits,
		Vk.Sbp.descriptionPipelineBindPoint =
			Vk.Ppl.BindPointGraphics,
		Vk.Sbp.descriptionInputAttachments = [],
		Vk.Sbp.descriptionColorAndResolveAttachments = Left [car],
		Vk.Sbp.descriptionDepthStencilAttachment = Nothing,
		Vk.Sbp.descriptionPreserveAttachments = [] }
	car = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 0,
		Vk.Att.referenceLayout = Vk.Img.LayoutColorAttachmentOptimal }
	dependency = Vk.Sbp.Dependency {
		Vk.Sbp.dependencySrcSubpass = Vk.Sbp.SExternal,
		Vk.Sbp.dependencyDstSubpass = 0,
		Vk.Sbp.dependencySrcStageMask =
			Vk.Ppl.StageColorAttachmentOutputBit .|.
			Vk.Ppl.StageEarlyFragmentTestsBit,
		Vk.Sbp.dependencySrcAccessMask = zeroBits,
		Vk.Sbp.dependencyDstStageMask =
			Vk.Ppl.StageColorAttachmentOutputBit .|.
			Vk.Ppl.StageEarlyFragmentTestsBit,
		Vk.Sbp.dependencyDstAccessMask =
			Vk.AccessColorAttachmentWriteBit .|.
			Vk.AccessDepthStencilAttachmentWriteBit,
		Vk.Sbp.dependencyDependencyFlags = zeroBits }

createPplLyt ::
	Vk.Dvc.D sd -> (forall sl . Vk.PplLyt.P sl '[] '[] -> IO a) -> IO a
createPplLyt dv = Vk.PplLyt.create @'Nothing @_ @_ @'[] dv info nil
	where info = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Nil }

createGrPpl :: (AllocationCallbacks.ToMiddle ma, Ord k) =>
	Vk.Ppl.Gr.Group sd ma sg k '[ '(
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)],
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)],
		'(sl, '[], '[]) )] -> k ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[] '[] ->
	IO (Vk.Ppl.Gr.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)]
		'(sl, '[], '[]))
createGrPpl gpg k ex rp pl = (\(U3 gpl :** HPList.Nil) -> gpl) . fromRight
	<$> Vk.Ppl.Gr.createGs' gpg k Nothing
		(HPList.Singleton . U14 $ grPplInfo ex rp pl)

recreateGrPpl :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[] '[] ->
	Vk.Ppl.Gr.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] '(sl, '[], '[]) -> IO ()
recreateGrPpl dv ex rp pl p = Vk.Ppl.Gr.unsafeRecreateGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	(HPList.Singleton $ U3 p)

grPplInfo :: Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[] '[] ->
	Vk.Ppl.Gr.CreateInfo 'Nothing
		'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
		'(	'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'Nothing '(sl, '[], '[]) sr '(sb, vs, ts, plas)
grPplInfo ex rp pl = Vk.Ppl.Gr.CreateInfo {
	Vk.Ppl.Gr.createInfoNext = TMaybe.N,
	Vk.Ppl.Gr.createInfoFlags = zeroBits,
	Vk.Ppl.Gr.createInfoStages = shaderStages,
	Vk.Ppl.Gr.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Gr.createInfoInputAssemblyState = Just ia,
	Vk.Ppl.Gr.createInfoViewportState = Just $ vwpSt ex,
	Vk.Ppl.Gr.createInfoRasterizationState = Just rst,
	Vk.Ppl.Gr.createInfoMultisampleState = Just ms,
	Vk.Ppl.Gr.createInfoDepthStencilState = Nothing,
	Vk.Ppl.Gr.createInfoColorBlendState = Just clrBlnd,
	Vk.Ppl.Gr.createInfoDynamicState = Nothing,
	Vk.Ppl.Gr.createInfoLayout = U3 pl,
	Vk.Ppl.Gr.createInfoRenderPass = rp,
	Vk.Ppl.Gr.createInfoSubpass = 0,
	Vk.Ppl.Gr.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Gr.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Gr.createInfoTessellationState = Nothing }
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
		Vk.Ppl.RstSt.createInfoFrontFace = Vk.FrontFaceClockwise,
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

createFrmbffrs :: forall scin sv k sd sf sr inm fmt . (HPList.HomoListN scin, Ord k) =>
	Vk.Frmbffr.Group sd 'Nothing sf (k, Int) -> k -> Vk.Extent2d ->
	Vk.RndrPss.R sr -> HPList.PL (Vk.ImgVw.I inm fmt) (HPList.Replicate scin sv) ->
	IO (HPList.PL Vk.Frmbffr.F (HPList.Replicate scin sf))
createFrmbffrs fbg k ex rp = HPList.mapHomoListNMWithI @_ @scin @_ @_ @sv 0 \i v ->
	fromRight <$> Vk.Frmbffr.create' fbg (k, i) (frmbffrInfo ex rp v)

recreateFrmbffrs :: forall scin sv sf sd sr inm fmt . HPList.HomoListN scin =>
	Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	HPList.PL (Vk.ImgVw.I inm fmt) (HPList.Replicate scin sv) ->
	HPList.PL Vk.Frmbffr.F (HPList.Replicate scin sf) -> IO ()
recreateFrmbffrs dv ex rp = HPList.zipWithHomoListNM_ @_ @scin @_ @_ @sv @_ @sf \v fb ->
	Vk.Frmbffr.unsafeRecreate dv (frmbffrInfo ex rp v) nil fb

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

createVtxBffr :: forall k alv sd sc sm sb bnmv nmv . (Ord k, KnownNat alv) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Group sd 'Nothing sb k bnmv '[VObj.List alv WVertex nmv] ->
	Vk.Mm.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mm.BufferArg bnmv '[VObj.List alv WVertex nmv])] ->
	k -> [WVertex] ->
	IO (Vk.Bffr.Binded sm sb bnmv '[VObj.List alv WVertex nmv])
createVtxBffr pd dv gq cp bfg mmg k vs = do
	(b, _) <- createBffrLst' pd dv bfg mmg k (fromIntegral $ length vs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit
	b <$ createBffrLst pd dv (fromIntegral $ length vs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) \
		(b' :: Vk.Bffr.Binded sm' sb' bnm' '[VObj.List alv t lnm'])
		bm' -> do
		Vk.Mm.write @bnm' @(VObj.List alv t lnm') @0 dv bm' zeroBits vs
		copyBffr dv gq cp b' b

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

createBffrLst :: forall al sd bnm lnm t a . (KnownNat al, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[VObj.List al t lnm] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm '[VObj.List al t lnm])] ->
		IO a) -> IO a
createBffrLst p dv ln = createBffr p dv $ VObj.LengthList ln

createBffrLst' :: forall al sd sm sb k bnm t lnm .
	(KnownNat al, Ord k, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k bnm '[VObj.List al t lnm] ->
	Vk.Mm.Group sd 'Nothing sm k
		'[ '(sb, Vk.Mm.BufferArg bnm '[VObj.List al t lnm])] -> k ->
	Vk.Dvc.Size -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb bnm '[VObj.List al t lnm],
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm '[VObj.List al t lnm])] )
createBffrLst' p dv bfg mmg k = createBffr' p dv bfg mmg k . VObj.LengthList

createBffr :: forall sd bnm o a . VObj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[o] -> Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm '[o])] -> IO a) -> IO a
createBffr p dv ln us prs f =
	Vk.Bffr.group dv nil \bfg -> Vk.Mm.group dv nil \mmg ->
	uncurry f =<< createBffr' p dv bfg mmg () ln us prs

createBffr' :: forall sd sm sb k bnm o . (Ord k, VObj.SizeAlignment o) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.Group sd 'Nothing sb k bnm '[o] ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(sb, Vk.Mm.BufferArg bnm '[o])]  ->
	k -> VObj.Length o -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb bnm '[o],
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg bnm '[o])] )
createBffr' p dv bfg mmg k ln us prs =
	Vk.Bffr.create' bfg k (bffrInfo ln us) >>= \(fromRight -> b) -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits reqs) prs
	Vk.Mm.allocateBind' mmg k (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(ainfo mt) >>= \(fromRight ->
			(HPList.Singleton (U2 (Vk.Mm.BufferBinded bd)), mm)) ->
		pure (bd, mm)
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

copyBffr :: forall sd sc sm sb sm' sb' bnm bnm' al t lnm .
	(KnownNat al, Sizable t, Storable t) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb bnm '[VObj.List al t lnm] ->
	Vk.Bffr.Binded sm' sb' bnm' '[VObj.List al t lnm] -> IO ()
copyBffr dv gq cp s d = createCmdBffr dv cp \cb -> do
	Vk.CBffr.begin @'Nothing @'Nothing cb binfo $
		Vk.Cmd.copyBuffer @'[ '( '[VObj.List al t lnm], 0, 0)] cb s d
	Vk.Q.submit gq (HPList.Singleton . U4 $ sinfo cb) Nothing
	Vk.Q.waitIdle gq
	where
	binfo = Vk.CBffr.BeginInfo {
		Vk.CBffr.beginInfoNext = TMaybe.N,
		Vk.CBffr.beginInfoFlags = Vk.CBffr.UsageOneTimeSubmitBit,
		Vk.CBffr.beginInfoInheritanceInfo = Nothing }
	sinfo cb = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

createCmdBffr :: forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb . Vk.CBffr.C scb -> IO a) -> IO a
createCmdBffr dv cp f =
	Vk.CBffr.allocate dv info $ f . \(cb :*. HPList.Nil) -> cb
	where
	info :: Vk.CBffr.AllocateInfo 'Nothing scp '[ '()]
	info = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cp,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

data SyncObjs (ssos :: (Type, Type, Type)) where
	SyncObjs :: {
		_imageAvailableSemaphores :: Vk.Semaphore.S sias,
		_renderFinishedSemaphores :: Vk.Semaphore.S srfs,
		_inFlightFences :: Vk.Fence.F sfs } ->
		SyncObjs '(sias, srfs, sfs)

createSyncObjs :: (Ord k, AllocationCallbacks.ToMiddle ma) =>
	Vk.Semaphore.Group sd ma sias k -> Vk.Semaphore.Group sd ma srfs k ->
	Vk.Fence.Group sd ma siff k -> k -> IO (SyncObjs '(sias, srfs, siff))
createSyncObjs iasg rfsg iffg k = SyncObjs
	<$> (fromRight <$> Vk.Semaphore.create' @_ @Nothing iasg k def)
	<*> (fromRight <$> Vk.Semaphore.create' @_ @Nothing rfsg k def)
	<*> (fromRight <$> Vk.Fence.create' @_ @'Nothing iffg k finfo)
	where
	finfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

mainloop :: forall
	scin sv sf sd scb sl sm sb bnmv alv nmv
	sw nmi ssfc sr sg sias srfs siff fmt ssc .
	(HPList.HomoListN scin, KnownNat alv, Vk.T.FormatToValue fmt) =>
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.CBffr.C scb -> Vk.PplLyt.P sl '[] '[] ->
	[Vk.Bffr.Binded sm sb bnmv '[VObj.List alv WVertex nmv]] ->
	IO (WinParams sw sl nmi ssfc sr sg sias srfs siff fmt ssc
		(HPList.Replicate scin sv) (HPList.Replicate scin sf)) -> IO ()
mainloop pd qfis dv gq pq cb pl vbs crw = do
	wpss0 <- replicateM 3 crw
	($ wpss0) $ fix \go wpss ->
		GlfwG.pollEvents >>
		run @scin @sv @sf pd qfis dv gq pq cb pl vbs wpss (go wpss)
	Vk.Dvc.waitIdle dv

run :: forall
	scin sv sf sd scb sl sm sb bnmv alv nmv
	sw nmi ssfc sr sg sias srfs siff fmt ssc .
	(HPList.HomoListN scin, KnownNat alv, Vk.T.FormatToValue fmt) =>
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.CBffr.C scb -> Vk.PplLyt.P sl '[] '[] ->
	[Vk.Bffr.Binded sm sb bnmv '[VObj.List alv WVertex nmv]] ->
	[WinParams sw sl nmi ssfc sr sg sias srfs siff fmt ssc
		(HPList.Replicate scin sv) (HPList.Replicate scin sf)] -> IO () -> IO ()
run pd qfis dv gq pq cb pl vbs wpss go = do
	for_ (zip vbs wpss) \(v, w) ->
		drawCatch @scin @sv @sf pd qfis dv gq pq cb pl v w go
	(not . or <$> (GlfwG.Win.shouldClose . winParamsToWin) `mapM` wpss >>=)
		. bool (pure ()) . (>> go) $ for_ wpss \w ->
			(checkFlagWinParams w >>=) . bool (pure ()) $
			recreateAll @scin @sv @sf
				pd qfis dv pl (winParamsToRecreates w)

drawCatch :: forall
	scin sv sf sd scb sl sm sb bnmv alv nmv
	sw nmi ssfc sr sg sias srfs siff fmt ssc .
	(HPList.HomoListN scin, KnownNat alv, Vk.T.FormatToValue fmt) =>
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.CBffr.C scb -> Vk.PplLyt.P sl '[] '[]  ->
	Vk.Bffr.Binded sm sb bnmv '[VObj.List alv WVertex nmv] ->
	WinParams sw sl nmi ssfc sr sg sias srfs siff fmt ssc
		(HPList.Replicate scin sv) (HPList.Replicate scin sf) -> IO () -> IO ()
drawCatch pd qfis dv gq pq cb pl vb wps go = do
	catchRecreate @scin @sv @sf pd qfis dv pl (winParamsToRecreates wps) go
		. draw dv gq pq cb vb $ winParamsToDraws wps
	Vk.Dvc.waitIdle dv

catchRecreate :: forall scin sv sf sd sl sw nm ssfc sr sg fmt ssc .
	(HPList.HomoListN scin, Vk.T.FormatToValue fmt) =>
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.PplLyt.P sl '[] '[] ->
	Recreates sw sl nm ssfc sr sg fmt ssc
		(HPList.Replicate scin sv) (HPList.Replicate scin sf) -> IO () -> IO () ->
	IO ()
catchRecreate pd qfis dv pl rcs go act = catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just (Left "VK_ERROR_OUT_OF_DATE_KHR")
		Vk.SuboptimalKhr -> Just (Left "VK_SUBOPTIMAL_KHR")
		_ -> Nothing) act
	\mm -> either putStrLn pure mm >> recreateAll @scin @sv @sf pd qfis dv pl rcs >> go

recreateAll :: forall scin sv sf sd sl sw nm ssfc sr sg fmt ssc .
	(HPList.HomoListN scin, Vk.T.FormatToValue fmt) =>
	Vk.Phd.P -> QFamIndices -> Vk.Dvc.D sd -> Vk.PplLyt.P sl '[] '[] ->
	Recreates sw sl nm ssfc sr sg fmt ssc
		(HPList.Replicate scin sv) (HPList.Replicate scin sf) -> IO ()
recreateAll pd qfis dv pl (w, sfc, vex, rp, gp, sc, vs, fbs) = do
	waitFramebufferSize w >> Vk.Dvc.waitIdle dv
	ex <- recreateSwpch w sfc pd qfis dv sc
	atomically $ writeTVar vex ex
	Vk.Khr.Swpch.getImages dv sc >>= \is -> recreateImgVws dv is vs
	recreateGrPpl dv ex rp pl gp
	recreateFrmbffrs @scin @sv @sf dv ex rp vs fbs

waitFramebufferSize :: GlfwG.Win.W sw -> IO ()
waitFramebufferSize w = GlfwG.Win.getFramebufferSize w >>= \sz ->
	when (zero sz) $ fix \go -> (`when` go) . zero =<<
		GlfwG.waitEvents *> GlfwG.Win.getFramebufferSize w
	where zero = uncurry (||) . ((== 0) *** (== 0))

draw :: forall sd scb sm sb bnmv alv nmv sl sr sg fmt sias srfs siff ssc sfs .
	KnownNat alv =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.CBffr.C scb ->
	Vk.Bffr.Binded sm sb bnmv '[VObj.List alv WVertex nmv] ->
	Draws sl sr sg sias srfs siff fmt ssc sfs -> IO ()
draw dv gq pq cb vb (vex, rp, gp, (SyncObjs ias rfs iff), sc, fbs) = do
	Vk.Fence.waitForFs dv siff True Nothing >> Vk.Fence.resetFs dv siff
	ex <-atomically (readTVar vex)
	ii <- Vk.Khr.Swpch.acquireNextImageResult
		[Vk.Success, Vk.SuboptimalKhr] dv sc maxBound (Just ias) Nothing
	Vk.CBffr.reset cb def
	HPList.index fbs ii \fb -> recordCmdBffr cb ex rp gp fb vb
	Vk.Q.submit gq (HPList.Singleton $ U4 sinfo) $ Just iff
	catchAndSerialize . Vk.Khr.Swpch.queuePresent @'Nothing pq $ pinfo ii
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
		Vk.Khr.Swpch.presentInfoSwapchainImageIndices = HPList.Singleton
			$ Vk.Khr.Swpch.SwapchainImageIndex sc ii }

type Recreates sw sl nm ssfc sr sg fmt ssc svs sfs = (
	GlfwG.Win.W sw, Vk.Khr.Sfc.S ssfc, TVar Vk.Extent2d, Vk.RndrPss.R sr,
	Vk.Ppl.Gr.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] '(sl, '[], '[]),
	Vk.Khr.Swpch.S fmt ssc,
	HPList.PL (Vk.ImgVw.I nm fmt) svs, HPList.PL Vk.Frmbffr.F sfs )

winParamsToRecreates ::
	WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc svs sfs ->
	Recreates sw sl nm ssfc sr sg fmt ssc svs sfs
winParamsToRecreates (WinParams w _fr sfc ex rp gp _sos sc vs fb) =
	(w, sfc, ex, rp, gp, sc, vs, fb)

type Draws sl sr sg sias srfs siff fmt ssc sfs = (
	TVar Vk.Extent2d, Vk.RndrPss.R sr,
	Vk.Ppl.Gr.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] '(sl, '[], '[]),
	SyncObjs '(sias, srfs, siff),
	Vk.Khr.Swpch.S fmt ssc, HPList.PL Vk.Frmbffr.F sfs )

winParamsToDraws ::
	WinParams sw sl nm ssfc sr sg sias srfs siff fmt ssc svs sfs ->
	Draws sl sr sg sias srfs siff fmt ssc sfs
winParamsToDraws (WinParams _w _fr _sfc ex rp gp sos sc _vs fb) =
	(ex, rp, gp, sos, sc, fb)

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

recordCmdBffr :: forall scb sr sg sl sf smv sbv bnmv alv nmv . KnownNat alv =>
	Vk.CBffr.C scb -> Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.Ppl.Gr.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3)] '(sl, '[], '[]) ->
	Vk.Frmbffr.F sf ->
	Vk.Bffr.Binded smv sbv bnmv '[VObj.List alv WVertex nmv] -> IO ()
recordCmdBffr cb ex rp gp fb vb = Vk.CBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb info Vk.Sbp.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gp \cbb -> do
	Vk.Cmd.bindVertexBuffers cbb . HPList.Singleton
		. U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @nmv vb
	Vk.Cmd.draw cbb (bffrLn vb) 1 0 0
	where
	info :: Vk.RndrPss.BeginInfo 'Nothing sr sf
		'[ 'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32]
	info = Vk.RndrPss.BeginInfo {
		Vk.RndrPss.beginInfoNext = TMaybe.N,
		Vk.RndrPss.beginInfoRenderPass = rp,
		Vk.RndrPss.beginInfoFramebuffer = fb,
		Vk.RndrPss.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = ex },
		Vk.RndrPss.beginInfoClearValues = HPList.Singleton
			. Vk.ClearValueColor . fromJust $ rgbaDouble 0 0 0 1 }

bffrLn :: Vk.Bffr.Binded sm' sb' nm' '[ VObj.List ali t lnmi] -> Word32
bffrLn b = fromIntegral sz
	where HPList.Singleton (VObj.LengthList' sz) = Vk.Bffr.lengthBinded b

type WVertex = Foreign.Storable.Generic.W Vertex

data Vertex = Vertex { vertexPos :: Cglm.Vec2, vertexColor :: Cglm.Vec3 }
	deriving (Show, Generic)

instance Foreign.Storable.Generic.G Vertex

vertices :: [WVertex]
vertices = Foreign.Storable.Generic.W <$> [
	Vertex (Cglm.Vec2 $ 0.0 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 1.0 :. NilL),
	Vertex (Cglm.Vec2 $ 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL) ]

vertices2 :: [WVertex]
vertices2 = Foreign.Storable.Generic.W <$> [
	Vertex (Cglm.Vec2 $ 0.0 :. (- 0.5) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ 0.5 :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ (- 0.5) :. 0.5 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL) ]

fromRight :: Either String r -> r
fromRight = \case Left m -> error $ "fromRight: not Right: " ++ m; Right r -> r

[glslVertexShader|

#version 450

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec3 inColor;
layout(location = 0) out vec3 fragColor;

void
main()
{
	gl_Position = vec4(inPosition, 0.0, 1.0);
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
	outColor = vec4(fragColor, 1.0);
}

|]
