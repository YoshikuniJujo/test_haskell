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
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Concurrent.STM

import GHC.Generics
import GHC.TypeNats
import Foreign.Storable
import Foreign.Storable.Generic qualified as GStorable
import Foreign.Storable.PeekPoke
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Foldable
import Data.MonoTraversable (Element, olength)
import Data.Sequences (IsSequence)
import Data.Default
import Data.Ord.ToolsYj
import Data.Bits
import Data.Bits.ToolsYj
import Data.Function.ToolsYj
import Data.Bool
import Data.Bool.ToolsYj
import Data.Maybe
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.List.Infinite qualified as Inf
import Data.List.Infinite (pattern (:~))
import Data.List.Length
import Data.List.ToolsYj
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.Word
import Data.Text.IO qualified as Txt
import Data.Time
import Data.Color
import Data.IORef
import Data.IORef.ToolsYj

import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH
import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object qualified as Vk.Obj
import Gpu.Vulkan.Object.NoAlignment qualified as Vk.ObjNA
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
import Gpu.Vulkan.Framebuffer qualified as Vk.Frmbffr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.Semaphore qualified as Vk.Semaphore
import Gpu.Vulkan.Fence qualified as Vk.Fence

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Pipeline.Graphics qualified as Vk.Ppl.Gr
import Gpu.Vulkan.Pipeline.Compute qualified as Vk.Ppl.Cp
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.InputAssemblyState qualified as Vk.Ppl.InpAsmbSt
import Gpu.Vulkan.Pipeline.ViewportState qualified as Vk.Ppl.ViewportSt
import Gpu.Vulkan.Pipeline.RasterizationState qualified as Vk.Ppl.RstSt
import Gpu.Vulkan.Pipeline.MultisampleState qualified as Vk.Ppl.MltSmplSt
import Gpu.Vulkan.Pipeline.ColorBlendAttachment qualified as Vk.Ppl.ClrBlndAtt
import Gpu.Vulkan.Pipeline.ColorBlendState qualified as Vk.Ppl.ClrBlndSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.PplLyt
import Gpu.Vulkan.PushConstant qualified as Vk.PshCnst
import Gpu.Vulkan.ShaderModule qualified as Vk.ShdrMd
import Gpu.Vulkan.VertexInput qualified as Vk.VtxInp
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.ColorComponent qualified as Vk.ClrCmp
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPss
import Gpu.Vulkan.Attachment qualified as Vk.Att
import Gpu.Vulkan.Subpass qualified as Vk.Sbp
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt

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

import System.Random
import Gpu.Vulkan.Pipeline.VertexInputState qualified as Vk.Ppl.VertexInputSt

import Language.SpirV qualified as SpirV

---------------------------------------------------------------------------
--
-- * PARAMETERS
-- * MAIN
-- * BODY
-- * CREATE SWAP CHAIN
-- * BUFFERS
-- * COMPUTE DESCRIPTOR SET
-- * COMPUTE PIPELINE
-- * GRAPHICS PIPELINE
-- * MAINLOOP
-- * RECREATE
-- * SHADERS
--
---------------------------------------------------------------------------

-- PARAMETERS

type MaxFramesInFlight = TNum 2 '()

type family TNum i a where TNum 0 a = '[]; TNum n a = a ': TNum (n - 1) a

maxFramesInFlight :: Integral n => n
maxFramesInFlight = toNum @_ @MaxFramesInFlight

class ToNum (n :: [k]) where toNum :: Integral i => i
instance ToNum '[] where toNum = 0
instance ToNum n => ToNum (t ': n) where toNum = 1 + toNum @_ @n

particleCount :: Integral n => n
particleCount = 8192

-- MAIN

main :: IO ()
main = newIORef False >>= \fr ->
	withWin fr \w -> createIst \i -> bool id (dbgm i) debug $ body fr w i
	where dbgm i = Vk.DbgUtls.Msngr.create i dbgMsngrInfo nil

type FramebufferResized = IORef Bool

withWin :: FramebufferResized -> (forall sw . GlfwG.Win.W sw -> IO a) -> IO a
withWin fr a = GlfwG.init error $ GlfwG.Win.group $ (a =<<) . initw
	where
	initw :: GlfwG.Win.Group s () -> IO (GlfwG.Win.W s)
	initw g = do
		Right w <- GlfwG.Win.hint noapi >> GlfwG.Win.create'
			g () 800 600 "Particle" Nothing Nothing
		w <$ GlfwG.Win.setFramebufferSizeCallback w
			(Just . const3 $ writeIORef fr True)
	noapi = GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI

createIst :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createIst f = do
	errorIf emsg . (debug &&) . elemNotAll vldLayers
		. (Vk.layerPropertiesLayerName <$>)
		=<< Vk.Ist.enumerateLayerProperties
	exts <- bool id (Vk.DbgUtls.extensionName :) debug
		. (Vk.Ist.ExtensionName <$>)
		<$> GlfwG.getRequiredInstanceExtensions
	bool	(Vk.Ist.create (info exts) nil f)
		(Vk.Ist.create (infodbg exts) nil f) debug
	where
	emsg = "validation layers requested, but not available!"
	info exts = Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.N,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo = Just ainfo,
		Vk.Ist.createInfoEnabledLayerNames = [],
		Vk.Ist.createInfoEnabledExtensionNames = exts }
	infodbg exts = Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.J dbgMsngrInfo,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo = Just ainfo,
		Vk.Ist.createInfoEnabledLayerNames = vldLayers,
		Vk.Ist.createInfoEnabledExtensionNames = exts }
	ainfo = Vk.ApplicationInfo {
		Vk.applicationInfoNext = TMaybe.N,
		Vk.applicationInfoApplicationName = "Hello Particle",
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
	Vk.DbgUtls.Msngr.createInfoFnUserCallback = dbgck,
	Vk.DbgUtls.Msngr.createInfoUserData = Nothing }
	where dbgck _svr _tp cbdt _ud = False <$ Txt.putStrLn (
		"validation layer: " <>
		Vk.DbgUtls.Msngr.callbackDataMessage cbdt )

-- BODY

body :: FramebufferResized -> GlfwG.Win.W sw -> Vk.Ist.I si -> IO ()
body fr w ist =
	Vk.Khr.Sfc.Glfw.Win.create ist w nil \sfc ->
	pickPhd ist sfc \pd ssd qfis -> swpExt w (capabilities ssd) >>= \ex ->
	createLgDvc pd qfis \d gq cq pq -> createCmdPl qfis d \cp ->
	createDscPl d \dp ->
	createSyncObjs @MaxFramesInFlight d \sos@(SyncObjs _ _ _ cfss ciffs) ->
	createVtxBffrs pd d gq cp (vertices ex $ mkStdGen 8) \vbs ->

	Vk.CBffr.allocate @_ @MaxFramesInFlight d (cmdBffrInfo cp) \ccbs ->
	createCmpPpl d \cdsl cpl cppl ->
	createDeltaTimeBffr pd d \bdt mdt ->
	createCmpDscSts
		@MaxFramesInFlight d dp cdsl \(HPList.homoListIndex -> cdss) ->
	replicateMWithI maxFramesInFlight (updateCmpDscSt d cdss bdt vbs) >>
	getCurrentTime >>= \ft0 -> atomically (newTVar ft0) >>= \vft ->
	let	cruns = zipWith ($) (rs particleCount) [0 :: Int ..]
			where rs pc = HPList.zip3ToList
				(\(HPList.Dummy ccb) cif cf i -> cmpRun vft d
					cq ccb cpl cppl mdt (cdss i) pc cif cf)
				ccbs ciffs cfss in

	Vk.CBffr.allocate @_ @MaxFramesInFlight d (cmdBffrInfo cp) \cbs ->
	createSwpch sfc ssd ex qfis d \(sc :: Vk.Khr.Swpch.S scifmt ssc) ->
	Vk.Khr.Swpch.getImages d sc >>= \scis -> createImgVws d scis \scvs ->
	createRndrPss @scifmt d \rp -> createFrmbffrs d ex rp scvs \fbs ->
	createPplLyt d \pl -> createGrPpl d ex rp pl \gp ->

	mainloop fr w sfc pd qfis d sos
		cruns gq pq sc ex scvs rp pl gp fbs vbs cbs

pickPhd :: Vk.Ist.I si -> Vk.Khr.Sfc.S ss -> (forall fmts .
	Vk.Phd.P -> SwpchSupportDetails fmts -> QFamIdcs -> IO a) -> IO a
pickPhd ist sfc f = Vk.Phd.enumerate ist >>= \case
	[] -> error "failed to find GPUs with Gpu.Vulkan support!"
	pds -> chk pds suit $ maybe
		(error "failed to find a suitable GPU!")
		\(pd, ss, qfi) -> f pd ss qfi
	where
	chk :: Monad m => [a] ->
		(forall c .
			a -> (forall p . Maybe (x, y p, z) -> m c) -> m c) ->
		(forall p . Maybe (x, y p, z) -> m d) -> m d
	chk [] _ g = g Nothing
	chk (x : xs) p g = p x $ maybe (chk xs p g) \y -> g $ Just y
	suit :: Vk.Phd.P -> (forall fmts .
		Maybe (Vk.Phd.P, SwpchSupportDetails fmts, QFamIdcs) ->
		IO c) -> IO c
	suit pd g = espt pd >>= bool (g Nothing) do
		qfis <- findQFams pd sfc
		querySwpchSupport pd sfc \ss -> bool (g $ (pd, ss ,) <$> qfis) (g Nothing)
			$	HPListC.null (snd $ formats ss) ||
				null (presentModes ss)
	espt pd = elemAll dvcExtensions
		. (Vk.Phd.extensionPropertiesExtensionName <$>)
		<$> Vk.Phd.enumerateExtensionProperties pd Nothing

dvcExtensions :: [Vk.Phd.ExtensionName]
dvcExtensions = [Vk.Khr.Swpch.extensionName]

findQFams :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (Maybe QFamIdcs)
findQFams pd sfc = do
	prps@((fst <$>) -> is) <- Vk.Phd.getQueueFamilyProperties pd
	mp <- listToMaybe
		<$> filterM (flip (Vk.Khr.Sfc.Phd.getSupport pd) sfc) is
	pure $ QFamIdcs <$> (fst <$> find (gcbit . snd) prps) <*> mp
	where gcbit = checkBits (Vk.Q.GraphicsBit .|. Vk.Q.ComputeBit)
		. Vk.QFam.propertiesQueueFlags

data QFamIdcs = QFamIdcs { grFam :: Vk.QFam.Index, prFam :: Vk.QFam.Index }

querySwpchSupport :: Vk.Phd.P -> Vk.Khr.Sfc.S ss ->
	(forall fmts . SwpchSupportDetails fmts -> IO a) -> IO a
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

createLgDvc :: Vk.Phd.P -> QFamIdcs ->
	(forall sd . Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Q.Q -> IO a) -> IO a
createLgDvc pd qfis f = hetero qinfo uniqueQFams \qs ->
	Vk.Dvc.create pd (info qs) nil \dv -> join $ f dv
		<$> Vk.Dvc.getQueue dv (grFam qfis) 0
		<*> Vk.Dvc.getQueue dv (grFam qfis) 0
		<*> Vk.Dvc.getQueue dv (prFam qfis) 0
	where
	hetero :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] -> (forall ss .
		HPList.ToListWithCM' WithPoked TMaybe.M ss =>
		HPList.PL t ss -> b) -> b
	hetero _k [] g = g HPList.Nil
	hetero k (x : xs) g = hetero k xs \xs' -> g (k x :** xs')
	qinfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }
	uniqueQFams = L.nub [grFam qfis, prFam qfis]
	info qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames = bool [] vldLayers debug,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures = Just def {
			Vk.Phd.featuresSamplerAnisotropy = True,
			Vk.Phd.featuresSampleRateShading = True } }

createCmdPl ::
	QFamIdcs -> Vk.Dvc.D sd -> (forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfis dv = Vk.CmdPl.create dv Vk.CmdPl.CreateInfo {
	Vk.CmdPl.createInfoNext = TMaybe.N,
	Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
	Vk.CmdPl.createInfoQueueFamilyIndex = grFam qfis } nil

createDscPl :: Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDscPl dv = Vk.DscPl.create dv info nil
	where
	info = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = maxFramesInFlight,
		Vk.DscPl.createInfoPoolSizes = [szdt, szv] }
	szdt = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPl.sizeDescriptorCount = maxFramesInFlight }
	szv = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPl.sizeDescriptorCount = maxFramesInFlight * 2 }

createSyncObjs :: forall n sd a . HPList.RepM n =>
	Vk.Dvc.D sd -> (forall ssos . SyncObjs ssos -> IO a) -> IO a
createSyncObjs dv f =
	HPList.repM @n (Vk.Semaphore.create @'Nothing dv def nil) \iass ->
	HPList.repM @n (Vk.Semaphore.create @'Nothing dv def nil) \rfss ->
	HPList.repM @n (Vk.Fence.create @'Nothing dv finfo nil) \iffs ->
	HPList.repM @n (Vk.Semaphore.create @'Nothing dv def nil) \cfss ->
	HPList.repM @n (Vk.Fence.create @'Nothing dv finfo nil) \ciffs ->
	f $ SyncObjs iass rfss iffs cfss ciffs
	where
	finfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

data SyncObjs (ssos :: ([Type], [Type], [Type], [Type], [Type])) where
	SyncObjs :: {
		_imageAvailableSemaphores :: HPList.PL Vk.Semaphore.S siass,
		_renderFinishedSemaphores :: HPList.PL Vk.Semaphore.S srfss,
		_inFlightFences :: HPList.PL Vk.Fence.F siffs,
		_computeFinishedSemaphores :: HPList.PL Vk.Semaphore.S scfss,
		_computeInFlightFences :: HPList.PL Vk.Fence.F sciffs } ->
		SyncObjs '(siass, srfss, siffs, scifss, sciffs)

cmdBffrInfo :: forall n scp .
	Vk.CmdPl.C scp -> Vk.CBffr.AllocateInfo 'Nothing scp n
cmdBffrInfo cp = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cp,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

createRndrPss :: forall fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPss.R sr -> IO a) -> IO a
createRndrPss dv = Vk.RndrPss.create @_ @'[fmt] dv info nil
	where
	info = Vk.RndrPss.CreateInfo {
		Vk.RndrPss.createInfoNext = TMaybe.N,
		Vk.RndrPss.createInfoFlags = zeroBits,
		Vk.RndrPss.createInfoAttachments = HPList.Singleton ca,
		Vk.RndrPss.createInfoSubpasses = [sbp],
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
	sbp = Vk.Sbp.Description {
		Vk.Sbp.descriptionFlags = zeroBits,
		Vk.Sbp.descriptionPipelineBindPoint = Vk.Ppl.BindPointGraphics,
		Vk.Sbp.descriptionInputAttachments = [],
		Vk.Sbp.descriptionColorAndResolveAttachments = Left [car],
		Vk.Sbp.descriptionDepthStencilAttachment = Nothing,
		Vk.Sbp.descriptionPreserveAttachments = [] }
	car = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 0,
		Vk.Att.referenceLayout = Vk.Img.LayoutColorAttachmentOptimal }
	dpnd = Vk.Sbp.Dependency {
		Vk.Sbp.dependencySrcSubpass = Vk.Sbp.SExternal,
		Vk.Sbp.dependencyDstSubpass = 0,
		Vk.Sbp.dependencySrcStageMask =
			Vk.Ppl.StageColorAttachmentOutputBit,
		Vk.Sbp.dependencySrcAccessMask = zeroBits,
		Vk.Sbp.dependencyDstStageMask =
			Vk.Ppl.StageColorAttachmentOutputBit,
		Vk.Sbp.dependencyDstAccessMask =
			Vk.AccessColorAttachmentWriteBit,
		Vk.Sbp.dependencyDependencyFlags = zeroBits }

createFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	HPList.PL (Vk.ImgVw.I inm fmt) svs -> (forall sfs .
		RecreateFrmbffrs svs sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HPList.Nil f = f HPList.Nil
createFrmbffrs dv ex rp (v :** vs) f =
	Vk.Frmbffr.create dv (frmbffrInfo ex rp v) nil \fb ->
	createFrmbffrs dv ex rp vs \fbs -> f (fb :** fbs)

class RecreateFrmbffrs (svs :: [Type]) (sfs :: [Type]) where
	recreateFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
		HPList.PL (Vk.ImgVw.I inm fmt) svs ->
		HPList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFrmbffrs '[] '[] where
	recreateFrmbffrs _ _ _ HPList.Nil HPList.Nil = pure ()

instance RecreateFrmbffrs svs sfs =>
	RecreateFrmbffrs (sv ': svs) (sf ': sfs) where
	recreateFrmbffrs d ex rp (v :** vs) (fb :** fbs) =
		Vk.Frmbffr.unsafeRecreate d (frmbffrInfo ex rp v) nil fb >>
		recreateFrmbffrs d ex rp vs fbs

frmbffrInfo :: Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I inm fmt si ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(inm, fmt, si)]
frmbffrInfo Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } rp att =
	Vk.Frmbffr.CreateInfo {
		Vk.Frmbffr.createInfoNext = TMaybe.N,
		Vk.Frmbffr.createInfoFlags = zeroBits,
		Vk.Frmbffr.createInfoRenderPass = rp,
		Vk.Frmbffr.createInfoAttachments = HPList.Singleton $ U3 att,
		Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
		Vk.Frmbffr.createInfoLayers = 1 }

-- CREATE SWAP CHAIN

createSwpch :: Vk.Khr.Sfc.S ssfc -> SwpchSupportDetails fmts ->
	Vk.Extent2d -> QFamIdcs -> Vk.Dvc.D sd -> (forall ss scfmt .
		Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swpch.S scfmt ss -> IO a) -> IO a
createSwpch sfc ssd ex qfis dv f = chooseSwpSfcFmt (formats ssd)
	\(Vk.Khr.Sfc.Format cs :: Vk.Khr.Sfc.Format fmt) ->
	Vk.Khr.Swpch.create @_ @fmt dv (swpchInfo sfc qfis cps cs pm ex) nil f
	where
	(cps, pm) = (capabilities ssd, choosePresentMode $ presentModes ssd)

chooseSwpSfcFmt :: (
	[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
	HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ) ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Khr.Sfc.Format fmt -> a) -> a
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`find` fmts)
	$ (== Vk.Khr.ColorSpaceSrgbNonlinear) . Vk.Khr.Sfc.formatColorSpace
chooseSwpSfcFmt (_, HPListC.Nil) _ = error "no available swap surface formats"

recreateSwpch :: forall sw ssfc sd fmt ssc . Vk.T.FormatToValue fmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIdcs -> Vk.Dvc.D sd -> Vk.Khr.Swpch.S fmt ssc -> IO Vk.Extent2d
recreateSwpch w sfc pd qfis dv sc = do
	ssd <- querySwpchSupportFmt @fmt pd sfc
	let	cps = capabilitiesFmt ssd
		Vk.Khr.Sfc.Format cs = fromMaybe
			(error "no available swap surface formats")
			. listToMaybe $ formatsFmt ssd
		pm = choosePresentMode $ presentModesFmt ssd
	ex <- swpExt w cps
	ex <$ Vk.Khr.Swpch.unsafeRecreate dv
		(swpchInfo @fmt sfc qfis cps cs pm ex) nil sc

querySwpchSupportFmt :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (SwpchSupportDetailsFmt fmt)
querySwpchSupportFmt dv sfc = SwpchSupportDetailsFmt
	<$> Vk.Khr.Sfc.Phd.getCapabilities dv sfc
	<*> Vk.Khr.Sfc.Phd.getFormatsFiltered dv sfc
	<*> Vk.Khr.Sfc.Phd.getPresentModes dv sfc

data SwpchSupportDetailsFmt fmt = SwpchSupportDetailsFmt {
	capabilitiesFmt :: Vk.Khr.Sfc.Capabilities,
	formatsFmt :: [Vk.Khr.Sfc.Format fmt],
	presentModesFmt :: [Vk.Khr.PresentMode] } deriving Show

swpExt :: GlfwG.Win.W sw -> Vk.Khr.Sfc.Capabilities -> IO Vk.Extent2d
swpExt win cps
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

choosePresentMode :: [Vk.Khr.Sfc.PresentMode] -> Vk.Khr.Sfc.PresentMode
choosePresentMode =
	findDefault Vk.Khr.PresentModeFifo (== Vk.Khr.PresentModeMailbox)

swpchInfo :: forall fmt ss .
	Vk.Khr.Sfc.S ss -> QFamIdcs -> Vk.Khr.Sfc.Capabilities ->
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

createImgVws :: Vk.T.FormatToValue fmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss inm fmt] ->
	(forall si . HPList.PL (Vk.ImgVw.I inm fmt) si -> IO a) -> IO a
createImgVws _ [] f = f HPList.Nil
createImgVws dv (i : is) f =
	Vk.ImgVw.create dv (imgVwInfo i) nil \v ->
	createImgVws dv is \vs -> f $ v :** vs

recreateImgVws :: Vk.T.FormatToValue fmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss inm fmt] ->
	HPList.PL (Vk.ImgVw.I inm fmt) svs -> IO ()
recreateImgVws _ [] HPList.Nil = pure ()
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

-- BUFFERS

createVtxBffrs :: (IsSequence lst, Element lst ~ WVertex) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> lst ->
	(forall smsbbnms .
		HPList.PL (LstBffr WVertex nm) smsbbnms -> IO a) -> IO a
createVtxBffrs pd d gq cp vtcs@(fromIntegral . olength -> ln) f =
	createBffrLsts pd d ln
		(	Vk.Bffr.UsageVertexBufferBit .|.
			Vk.Bffr.UsageStorageBufferBit .|.
			Vk.Bffr.UsageTransferDstBit )
		Vk.Mm.PropertyDeviceLocalBit maxFramesInFlight \bs ->
	writeBffrLsts pd d gq cp bs vtcs >> f bs

createDeltaTimeBffr :: Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb bnm '[AtomDiffTime nm] ->
	Vk.Mm.M sm '[ '(sb, Vk.Mm.BufferArg bnm '[AtomDiffTime nm])] ->
	IO a) -> IO a
createDeltaTimeBffr = createBffrAtm @1 @_ @_ @_ @Float
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

type AtomDiffTime nm = Vk.ObjNA.Atom Float nm

createBffrAtm :: forall al sd bnm mnm a b . (KnownNat al, Storable a) =>
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.Obj.Atom al a mnm] ->
		Vk.Mm.M sm '[ '(
			sb, 'Vk.Mm.BufferArg bnm '[Vk.Obj.Atom al a mnm] )] ->
		IO b) -> IO b
createBffrAtm us prs p dv = createBffr p dv Vk.Obj.LengthAtom us prs

createBffrLst :: forall a sd bnm nm t b . (KnownNat a, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.Obj.List a t nm] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm '[Vk.Obj.List a t nm])] ->
		IO b) -> IO b
createBffrLst p dv ln = createBffr p dv $ Vk.Obj.LengthList ln

createBffrLsts :: Storable t =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> Int ->
	(forall smsbbnmvs . HPList.PL (LstBffr t nm) smsbbnmvs -> IO a) -> IO a
createBffrLsts pd d ln us prs n = HPList.replicateM n \f ->
	createBffrLst pd d ln us prs \b _ -> f . U3 $ LstBffr' b

type LstBffr t nm = U3 (LstBffr' t nm)

newtype LstBffr' t nm sm sb bnm =
	LstBffr' (Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List t nm])

createBffr :: forall sd bnm o a . Vk.Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[o] ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg bnm '[o])] -> IO a) -> IO a
createBffr p dv ln us prs f = Vk.Bffr.create dv (bffrInfo ln us) nil \b -> do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(ainfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bd))) -> bd
	where ainfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

findMmType ::
	Vk.Phd.P -> Vk.Mm.TypeBits -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmType pd flt prs =
	fromMaybe (error msg) . suit <$> Vk.Phd.getMemoryProperties pd
	where
	msg = "failed to find suitable memory type!"
	suit prs1 = fst <$> find
		((&&)	<$> (`Vk.Mm.elemTypeIndex` flt) . fst
			<*> checkBits prs . Vk.Mm.mTypePropertyFlags . snd)
		(Vk.Phd.memoryPropertiesMemoryTypes prs1)

bffrInfo :: Vk.Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo 'Nothing '[o]
bffrInfo ln us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N, Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton ln,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

writeBffrLsts :: forall sd sc lst nm smsbbnms .
	(IsSequence lst, Storable' (Element lst)) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	HPList.PL (LstBffr (Element lst) nm) smsbbnms -> lst -> IO ()
writeBffrLsts pd dv gq cp bs xs@(fromIntegral . olength -> ln) =
	createBffrLst pd dv ln Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
		\(b :: Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List t nm]) m ->
		Vk.Mm.write @bnm @(Vk.ObjNA.List t nm) @0 dv m zeroBits xs >>
		HPList.mapM_ (copy b) bs
	where
	copy :: forall sm sb bnm s .
		Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List (Element lst) nm] ->
		LstBffr (Element lst) nm s -> IO ()
	copy s (U3 (LstBffr' d)) = singleTimeCmds dv gq cp \cb ->
		Vk.Cmd.copyBuffer
			@'[ '( '[Vk.ObjNA.List (Element lst) nm], 0, 0)] cb s d

singleTimeCmds :: forall sd scp a . Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C scp -> (forall s . Vk.CBffr.C s -> IO a) -> IO a
singleTimeCmds dv gq cp cmd =
	Vk.CBffr.allocate dv (cmdBffrInfo @'[ '()] cp) \(cb :*. HPList.Nil) ->
	Vk.CBffr.begin @_ @'Nothing cb binfo (cmd cb) <* do
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

-- COMPUTE DESCRIPTOR SET

createCmpDscSts :: forall n sd sp sdsl nmdt nmh a . (
	HPList.HomoListN n,
	Vk.DscSt.DListFromMiddle
		(HPList.Replicate n '(sdsl, CmpDscStLytArg nmdt nmh)),
	HPList.HomoList '(sdsl, CmpDscStLytArg nmdt nmh)
		(HPList.Replicate n '(sdsl, CmpDscStLytArg nmdt nmh)) ) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.DscStLyt.D sdsl (CmpDscStLytArg nmdt nmh) -> (forall sds .
		HPList.HomoList '(sdsl, CmpDscStLytArg nmdt nmh)
			(HPList.Replicate n '(sdsl, CmpDscStLytArg nmdt nmh)) =>
		HPList.PL (Vk.DscSt.D sds)
			(HPList.Replicate n '(sdsl, CmpDscStLytArg nmdt nmh)) ->
		IO a) -> IO a
createCmpDscSts dv dp dsl = Vk.DscSt.allocateDs dv $ cmpDscStInfo @n dp dsl

cmpDscStInfo :: forall n sp sl bts . HPList.HomoListN n =>
	Vk.DscPl.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.AllocateInfo 'Nothing sp (HPList.Replicate n '(sl, bts))
cmpDscStInfo dp dsl = Vk.DscSt.AllocateInfo {
	Vk.DscSt.allocateInfoNext = TMaybe.N,
	Vk.DscSt.allocateInfoDescriptorPool = dp,
	Vk.DscSt.allocateInfoSetLayouts =
		HPList.homoListNFromList @_ @n $ replicate 2 (U2 dsl) }

updateCmpDscSt :: forall sd sdsl nmdt nmh smdt sbdt bnmdt smsbbnms sds .
	Vk.Dvc.D sd ->
	(Int -> Vk.DscSt.D sds '(sdsl, CmpDscStLytArg nmdt nmh)) ->
	Vk.Bffr.Binded smdt sbdt bnmdt '[AtomDiffTime nmdt] ->
	HPList.PL (LstBffr WVertex nmh) smsbbnms -> Int -> IO ()
updateCmpDscSt dv dss dtbs vbs i =
	HPList.index vbs ((i - 1) `mod` maxFramesInFlight) \(U3 (LstBffr' lvb)) ->
	HPList.index vbs i \(U3 (LstBffr' cvb)) ->
	Vk.DscSt.updateDs dv (
		U5 (cmpWriteDscStUniform @nmdt ds dtbs) :**
		U5 (cmpWriteDscStStorageN @0 @nmh ds lvb) :**
		U5 (cmpWriteDscStStorageN @1 @nmh ds cvb) :** HPList.Nil)
		HPList.Nil
	where ds = dss i

cmpWriteDscStUniform :: forall nm sds slbts sm sb bnm os . (
	Show (HPList.PL Vk.Obj.Length os),
	Vk.Obj.OffsetRange (AtomDiffTime nm) os 0 ) =>
	Vk.DscSt.D sds slbts -> Vk.Bffr.Binded sm sb bnm os ->
	Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgBuffer
			'[ '(sm, sb, bnm, AtomDiffTime nm, 0)]) 0
cmpWriteDscStUniform ds bf = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos
		. HPList.Singleton . U5 $ Vk.Dsc.BufferInfo bf }

cmpWriteDscStStorageN :: forall n nm sds slbts sm sb bnm os . (
	Show (HPList.PL Vk.Obj.Length os),
	Vk.Obj.OffsetRange (ListVertex nm) os 0 ) =>
	Vk.DscSt.D sds slbts -> Vk.Bffr.Binded sm sb bnm os ->
	Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgBuffer
			'[ '(sm, sb, bnm, ListVertex nm, 0)]) n
cmpWriteDscStStorageN ds bf = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos
		. HPList.Singleton . U5 $ Vk.Dsc.BufferInfo bf }

type ListVertex nm = Vk.ObjNA.List WVertex nm

-- COMPUTE PIPELINE

createCmpPpl :: Vk.Dvc.D sd -> (forall sds scppl spl .
	Vk.DscStLyt.D sds (CmpDscStLytArg nmdt nmv) ->
	Vk.PplLyt.P spl '[ '(sds, CmpDscStLytArg nmdt nmv)] '[] ->
	Vk.Ppl.Cp.C scppl '(spl, '[ '( sds, CmpDscStLytArg nmdt nmv)], '[]) ->
	IO a) -> IO a
createCmpPpl d f = createCmpPplLyt d \dsl pl ->
	Vk.Ppl.Cp.createCs d Nothing (HPList.Singleton . U4 $ info pl) nil
		\(HPList.Singleton p) -> f dsl pl p
	where
	info pl = Vk.Ppl.Cp.CreateInfo {
		Vk.Ppl.Cp.createInfoNext = TMaybe.N,
		Vk.Ppl.Cp.createInfoFlags = zeroBits,
		Vk.Ppl.Cp.createInfoStage = U5 shdrst,
		Vk.Ppl.Cp.createInfoLayout = U3 pl,
		Vk.Ppl.Cp.createInfoBasePipelineHandleOrIndex = Nothing }
	shdrst :: Vk.Ppl.ShdrSt.CreateInfo
		'Nothing 'Nothing 'GlslComputeShader 'Nothing '[]
	shdrst = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = zeroBits,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageComputeBit,
		Vk.Ppl.ShdrSt.createInfoModule =
			(shdrMdInfo glslComputeShaderMain, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }

shdrMdInfo :: SpirV.S sknd -> Vk.ShdrMd.CreateInfo 'Nothing sknd
shdrMdInfo cd = Vk.ShdrMd.CreateInfo {
	Vk.ShdrMd.createInfoNext = TMaybe.N,
	Vk.ShdrMd.createInfoFlags = zeroBits, Vk.ShdrMd.createInfoCode = cd }

createCmpPplLyt :: Vk.Dvc.D sd -> (forall sds spl .
	Vk.DscStLyt.D sds (CmpDscStLytArg nmdt nmv) ->
	Vk.PplLyt.P spl '[ '(sds, CmpDscStLytArg nmdt nmv)] '[] -> IO a) -> IO a
createCmpPplLyt dv f = createCmpDscStLyt dv \dsl ->
	Vk.PplLyt.create dv (info dsl) nil $ f dsl
	where
	info :: Vk.DscStLyt.D sl bts -> Vk.PplLyt.CreateInfo
		'Nothing '[ '(sl, bts)] ('Vk.PshCnst.Layout '[] '[])
	info dsl = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

createCmpDscStLyt :: Vk.Dvc.D sd -> (forall (sds :: Type) .
	Vk.DscStLyt.D sds (CmpDscStLytArg nmdt nmh) -> IO a) -> IO a
createCmpDscStLyt dv = Vk.DscStLyt.create dv info nil
	where
	info = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings =
			bu :** bs :** bs :** HPList.Nil }
	bu = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }
	bs = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

type CmpDscStLytArg nmdt nmv = '[
	'Vk.DscStLyt.Buffer '[AtomDiffTime nmdt],
	'Vk.DscStLyt.Buffer '[ListVertex nmv],
	'Vk.DscStLyt.Buffer '[ListVertex nmv] ]

-- GRAPHICS PIPELINE

createPplLyt :: forall sd a . Vk.Dvc.D sd ->
	(forall sl . Vk.PplLyt.P sl '[] '[] -> IO a) -> IO a
createPplLyt dv = Vk.PplLyt.create @_ @_ @_ @'[] dv info nil
	where info = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Nil }

createGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[] '[] -> (forall sg .
		Vk.Ppl.Gr.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
			'[ '(0, Pos), '(1, Color)] '(sl, '[], '[]) ->
		IO a) -> IO a
createGrPpl dv ex rp pl f = Vk.Ppl.Gr.createGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	\(HPList.Singleton (U3 p)) -> f p

recreateGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[] '[] ->
	Vk.Ppl.Gr.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)] '(sl, '[], '[]) -> IO ()
recreateGrPpl dv ex rp pl p = Vk.Ppl.Gr.unsafeRecreateGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	(HPList.Singleton $ U3 p)

grPplInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[] '[] ->
	Vk.Ppl.Gr.CreateInfo 'Nothing
		'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
		'(	'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Pos), '(1, Color)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'Nothing '(sl, '[], '[]) sr '(sb, vs, ts, plas)
grPplInfo ex rp pl = Vk.Ppl.Gr.CreateInfo {
	Vk.Ppl.Gr.createInfoNext = TMaybe.N,
	Vk.Ppl.Gr.createInfoFlags = zeroBits,
	Vk.Ppl.Gr.createInfoStages = shdrSt,
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
			Vk.PrimitiveTopologyPointList,
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

shdrSt :: HPList.PL (U5 Vk.Ppl.ShdrSt.CreateInfo)
	'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
shdrSt = U5 vinfo :** U5 finfo :** HPList.Nil
	where
	vinfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = zeroBits,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModule =
			(shdrMdInfo glslVertexShaderMain, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	finfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = zeroBits,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModule =
			(shdrMdInfo glslFragmentShaderMain, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }

type GlslVertexShaderArgs = '(
	'Nothing, 'Nothing,
	'GlslVertexShader, 'Nothing :: Maybe (Type, Type), '[] )

type GlslFragmentShaderArgs = '(
	'Nothing, 'Nothing,
	'GlslFragmentShader, 'Nothing :: Maybe (Type, Type), '[] )

vwpSt :: Vk.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo 'Nothing
vwpSt ex@Vk.Extent2d {
	Vk.extent2dWidth = fromIntegral -> w,
	Vk.extent2dHeight = fromIntegral -> h } = Vk.Ppl.ViewportSt.CreateInfo {
	Vk.Ppl.ViewportSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ViewportSt.createInfoFlags = zeroBits,
	Vk.Ppl.ViewportSt.createInfoViewports = [vp],
	Vk.Ppl.ViewportSt.createInfoScissors = [scssr] }
	where
	vp = Vk.Viewport {
		Vk.viewportX = 0, Vk.viewportY = 0,
		Vk.viewportWidth = w, Vk.viewportHeight = h,
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
		Vk.Ppl.ClrBlndAtt.stateBlendEnable = True,
		Vk.Ppl.ClrBlndAtt.stateSrcColorBlendFactor =
			Vk.BlendFactorSrcAlpha,
		Vk.Ppl.ClrBlndAtt.stateDstColorBlendFactor =
			Vk.BlendFactorOneMinusSrcAlpha,
		Vk.Ppl.ClrBlndAtt.stateColorBlendOp = Vk.BlendOpAdd,
		Vk.Ppl.ClrBlndAtt.stateSrcAlphaBlendFactor = Vk.BlendFactorOne,
		Vk.Ppl.ClrBlndAtt.stateDstAlphaBlendFactor = Vk.BlendFactorZero,
		Vk.Ppl.ClrBlndAtt.stateAlphaBlendOp = Vk.BlendOpAdd }

-- MAINLOOP

mainloop :: (
	Vk.T.FormatToValue scfmt,
	RecreateFrmbffrs svs sfs, HPList.HomoList '() mff ) =>
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIdcs -> Vk.Dvc.D sd -> SyncObjs ssoss -> [IO ()] ->
	Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[] '[] ->
	Vk.Ppl.Gr.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)] '(sl, '[], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	HPList.PL (LstBffr WVertex nmv) smsbbnmvs ->
	HPList.LL (Vk.CBffr.C scb) mff -> IO ()
mainloop fr w sfc pd qfis d soss cruns gq pq sc ex0 vs rp pl gp fbs vbs cbs = do
	($ Inf.cycle $ NE.fromList [0 .. maxFramesInFlight - 1]) . ($ ex0)
		$ fix \go ex (cf :~ cfs) -> GlfwG.pollEvents >> cruns !! cf >>
		run fr w sfc pd qfis d soss
			gq pq sc ex vs rp pl gp fbs vbs cbs cf (`go` cfs)
	Vk.Dvc.waitIdle d

cmpRun :: forall sd sc spl slbts sg smdt sbdt bnmdt nmdt sds sciff scfs .
	Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[], '[], '[]]] =>
	TVar UTCTime -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CBffr.C sc -> Vk.PplLyt.P spl '[slbts] '[] ->
	Vk.Ppl.Cp.C sg '(spl, '[slbts], '[]) ->
	Vk.Mm.M smdt '[ '(sbdt, Vk.Mm.BufferArg bnmdt '[AtomDiffTime nmdt])] ->
	Vk.DscSt.D sds slbts -> Word32 ->
	Vk.Fence.F sciff -> Vk.Semaphore.S scfs -> IO ()
cmpRun vft dv q cb pl ppl mdt dss pc ciff@(HPList.Singleton -> sciff) cfs = do
	Vk.Fence.waitForFs dv sciff True Nothing
	cft <- getCurrentTime
	lft <- atomically $ readTVar vft <* writeTVar vft cft
	Vk.Mm.write @bnmdt @(AtomDiffTime nmdt) @0 dv mdt zeroBits
		(realToFrac $ cft `diffUTCTime` lft * 1000 :: Float)
	Vk.Fence.resetFs dv sciff
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute ppl \ccb ->
		Vk.Cmd.bindDescriptorSetsCompute
			ccb pl (HPList.Singleton $ U2 dss) def >>
		Vk.Cmd.dispatch ccb (pc `div` 256) 1 1
	Vk.Q.submit q (HPList.Singleton $ U4 sinfo) $ Just ciff
	Vk.Q.waitIdle q
	where sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Singleton cfs }

run :: (Vk.T.FormatToValue scfmt,
	RecreateFrmbffrs svs sfs, HPList.HomoList '() mff) =>
	FramebufferResized -> GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIdcs -> Vk.Dvc.D sd -> SyncObjs ssoss ->
	Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I inm scfmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[] '[] ->
	Vk.Ppl.Gr.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)] '(sl, '[], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	HPList.PL (LstBffr WVertex nmv) smsbbnmvs ->
	HPList.LL (Vk.CBffr.C scb) mff -> Int -> (Vk.Extent2d -> IO ()) -> IO ()
run fr w sfc pd qfis dv soss gq pq sc ex vs rp pl gp fbs vbs cbs cf go = do
	catchAndRecreate w sfc pd qfis dv sc vs rp pl gp fbs go
		$ draw dv gq pq sc ex rp gp fbs vbs cbs soss cf
	(,) <$> GlfwG.Win.shouldClose w <*> checkFlag fr >>= \case
		(True, _) -> pure (); (_, False) -> go ex
		(_, _) -> go =<< recreateAll w sfc pd qfis dv sc vs rp pl gp fbs

draw :: forall sd fmt ssc sr sg sl sfs nmv smsbbnmvs scb mff ssos .
	HPList.HomoList '() mff =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S fmt ssc ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Gr.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)] '(sl, '[], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	HPList.PL (LstBffr WVertex nmv) smsbbnmvs ->
	HPList.LL (Vk.CBffr.C scb) mff -> SyncObjs ssos -> Int -> IO ()
draw dv gq pq sc ex rp gp fbs vbs cbs (SyncObjs iass rfss iffs cfss _) cf =
	HPList.index vbs cf \(U3 (LstBffr' vb)) ->
	HPList.index iass cf \ias -> HPList.index rfss cf \rfs ->
	HPList.index iffs cf \(id &&& HPList.Singleton -> (iff, siff)) ->
	HPList.index cfss cf \cfs -> do
	Vk.Fence.waitForFs dv siff True Nothing
	ii <- Vk.Khr.acquireNextImageResult
		[Vk.Success, Vk.SuboptimalKhr] dv sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dv siff
	Vk.CBffr.reset cb def
	HPList.index fbs ii \fb -> recordCmdBffr cb ex rp gp fb vb
	Vk.Q.submit gq (HPList.Singleton . U4 $ sinfo ias rfs cfs) $ Just iff
	catchAndSerialize . Vk.Khr.queuePresent pq $ pinfo rfs ii
	where
	HPList.Dummy cb = cbs `HPList.homoListIndex` cf ::
		HPList.Dummy (Vk.CBffr.C scb) '()
	sinfo :: Vk.Semaphore.S sias -> Vk.Semaphore.S srfs ->
		Vk.Semaphore.S scfs ->
		Vk.SubmitInfo 'Nothing '[scfs, sias] '[scb] '[srfs]
	sinfo ias rfs cfs = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks =
			Vk.SemaphorePipelineStageFlags
				cfs Vk.Ppl.StageVertexInputBit :**
			Vk.SemaphorePipelineStageFlags
				ias Vk.Ppl.StageColorAttachmentOutputBit :**
			HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
	pinfo :: Vk.Semaphore.S srfs -> Word32 ->
		Vk.Khr.PresentInfo 'Nothing '[srfs] fmt '[ssc]
	pinfo rfs ii = Vk.Khr.PresentInfo {
		Vk.Khr.presentInfoNext = TMaybe.N,
		Vk.Khr.presentInfoWaitSemaphores = HPList.Singleton rfs,
		Vk.Khr.presentInfoSwapchainImageIndices =
			HPList.Singleton $ Vk.Khr.SwapchainImageIndex sc ii }

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

recordCmdBffr :: forall scb sr sg sl sf smv sbv bnmv nmv .
	Vk.CBffr.C scb -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Gr.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)] '(sl, '[], '[]) ->
	Vk.Frmbffr.F sf -> Vk.Bffr.Binded smv sbv bnmv '[ListVertex nmv] ->
	IO ()
recordCmdBffr cb ex rp gp fb vb =
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb info Vk.Sbp.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gp \cbb -> do
	Vk.Cmd.bindVertexBuffers cbb . HPList.Singleton
		. U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @nmv vb
	Vk.Cmd.draw cbb (bffrLstLn vb) 1 0 0
	where
	info :: Vk.RndrPss.BeginInfo 'Nothing sr sf '[
		'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32 ]
	info = Vk.RndrPss.BeginInfo {
		Vk.RndrPss.beginInfoNext = TMaybe.N,
		Vk.RndrPss.beginInfoRenderPass = rp,
		Vk.RndrPss.beginInfoFramebuffer = fb,
		Vk.RndrPss.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = ex },
		Vk.RndrPss.beginInfoClearValues =
			Vk.ClearValueColor (fromJust $ rgbaDouble 0 0 0 1) :**
			HPList.Nil }

bffrLstLn :: Num n =>
	Vk.Bffr.Binded sm sb bnm '[Vk.Obj.ListMaybeName al v mnm] -> n
bffrLstLn b = fromIntegral sz
	where HPList.Singleton (Vk.Obj.LengthList' sz) = Vk.Bffr.lengthBinded b

-- RECREATE

catchAndRecreate :: (Vk.T.FormatToValue scfmt, RecreateFrmbffrs svs sfs) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QFamIdcs ->
	Vk.Dvc.D sd -> Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[] '[] ->
	Vk.Ppl.Gr.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)] '(sl, '[], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs -> (Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate w sfc pd qfis dv sc vs rp pl gp fbs go act = catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just (); _ -> Nothing) act \_ ->
	go =<< recreateAll w sfc pd qfis dv sc vs rp pl gp fbs

recreateAll :: (Vk.T.FormatToValue fmt, RecreateFrmbffrs svs sfs) =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P -> QFamIdcs ->
	Vk.Dvc.D sd -> Vk.Khr.Swpch.S fmt ssc ->
	HPList.PL (Vk.ImgVw.I nm fmt) svs -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[] '[] ->
	Vk.Ppl.Gr.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color)] '(sl, '[], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs -> IO Vk.Extent2d
recreateAll w sfc pd qfis dv sc vs rp pl gp fbs = do
	waitFramebufferSize >> Vk.Dvc.waitIdle dv
	ex <- recreateSwpch w sfc pd qfis dv sc
	ex <$ do
		Vk.Khr.Swpch.getImages dv sc >>= \is -> recreateImgVws dv is vs
		recreateGrPpl dv ex rp pl gp
		recreateFrmbffrs dv ex rp vs fbs
	where
	waitFramebufferSize = GlfwG.Win.getFramebufferSize w >>= \sz ->
		when (zero sz) $ fix \go -> (`when` go) . zero =<<
			GlfwG.waitEvents *> GlfwG.Win.getFramebufferSize w
	zero = uncurry (||) . ((== 0) *** (== 0))

-- DATA TYPES

type WVertex = GStorable.W Vertex

data Vertex =
	Vertex { vertexPos :: Pos, vertexVel :: Glm.Vec2, vertexColor :: Color }
	deriving (Show, Eq, Ord, Generic)

instance GStorable.G Vertex

newtype Pos = Pos Glm.Vec2
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype Color = Color Glm.Vec4
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

vertices :: Vk.Extent2d -> StdGen -> [WVertex]
vertices Vk.Extent2d {
	Vk.extent2dWidth = fromIntegral -> w,
	Vk.extent2dHeight = fromIntegral -> h } g = GStorable.W
	<$> take particleCount (L.unfoldr (Just . randomVertex w h) g)

randomVertex :: Float -> Float -> StdGen -> (Vertex, StdGen)
randomVertex w h g0 = (
	Vertex	(Pos . Glm.Vec2 $ x' :. y :. NilL)
		(Glm.Vec2 $ vx :. vy :. NilL)
		(Color . Glm.Vec4 $ rd :. g :. b :. 1 :. NilL), g5 )
	where
	(r, g1) = ((0.25 *) . sqrt) `first` randomR (0.0, 1.0) g0
	(theta, g2) = randomR (0.0, 2 * pi) g1
	(rd, g3) = randomR (0, 1.0) g2
	(g, g4) = randomR (0, 1.0) g3
	(b, g5) = randomR (0, 1.0) g4
	x = r * cos theta; x' = x * h / w
	y = r * sin theta
	vx = x' / d * 0.00025
	vy = y / d * 0.00025
	d = sqrt $ x ^ (2 :: Int) + y ^ (2 :: Int)

-- SHADERS

[glslVertexShader|

#version 450

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec4 inColor;
layout(location = 0) out vec3 fragColor;

void
main()
{
	gl_PointSize = 14.0;
	gl_Position = vec4(inPosition, 1.0, 1.0);
	fragColor = inColor.rgb;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec3 fragColor;

layout(location = 0) out vec4 outColor;

void
main()
{

	vec2 coord = gl_PointCoord - vec2(0.5);
	outColor = vec4(fragColor, 0.5 - length(coord));

}

|]

[glslComputeShader|

#version 450

struct Particle {
	vec2 position;
	vec2 velocity;
	vec4 color;
};

layout (binding = 0) uniform ParameterUBO {
	float deltaTime;
} ubo;

layout(std140, binding = 1) readonly buffer ParticleSSBOIn {
	Particle particlesIn[ ];
};

layout(std140, binding = 2) buffer ParticleSSBOOut {
	Particle particlesOut[ ];
};

layout (local_size_x = 256, local_size_y = 1, local_size_z = 1) in;

void main()
{
	uint index = gl_GlobalInvocationID.x;

	Particle particleIn = particlesIn[index];

	particlesOut[index].position = particleIn.position + particleIn.velocity.xy * ubo.deltaTime;

	float x = particlesOut[index].position.x;
	float y = particlesOut[index].position.y;
	float vx = particlesIn[index].velocity.x;
	float vy = particlesIn[index].velocity.y;

	if (x <= -1.0) {	particlesOut[index].velocity.x = abs(vx); }
	else if (x < 1.0) {	particlesOut[index].velocity.x = vx; }
	else {			particlesOut[index].velocity.x = - abs(vx); }

	if (y <= -1.0) {	particlesOut[index].velocity.y = abs(vy); }
	else if (y < 1.0) {	particlesOut[index].velocity.y = vy; }
	else {		particlesOut[index].velocity.y = - abs(vy); }
}

|]
