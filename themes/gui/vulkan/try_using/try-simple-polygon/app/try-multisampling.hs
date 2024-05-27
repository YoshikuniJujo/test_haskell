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

import Gpu.Vulkan.Memory qualified as Vk.Mm

import GHC.Generics
import GHC.TypeNats
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import Data.Kind
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.Object qualified as Obj
import Data.Foldable
import Data.Default
import Data.Bits
import Data.Array hiding (indices)
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Proxy
import Data.Bool
import Data.Maybe
import Data.List (nub)
import Data.IORef
import Data.List.Length
import Data.Word
import Data.Int
import Data.Color
import Data.Time
import Codec.Picture

import Data.List.NonEmpty qualified as NE
import GHC.Data.List.Infinite qualified as Inf
import Data.Vector.Storable qualified as V
import Graphics.UI.GLFW qualified as Glfw hiding (createWindowSurface)
import Gpu.Vulkan.Cglm qualified as Cglm
import Foreign.Storable.Generic qualified as GStorable

import Language.SpirV qualified as SpirV
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH


import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Exception qualified as Vk
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam

import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Device qualified as Device.M
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.Image qualified as Vk.Img.M
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Component qualified as Vk.Component
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderModule
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.InputAssemblyState qualified as Vk.Ppl.InpAsmbSt
import Gpu.Vulkan.Pipeline.ViewportState qualified as Vk.Ppl.ViewportSt
import Gpu.Vulkan.Pipeline.RasterizationState qualified as Vk.Ppl.RstSt
import Gpu.Vulkan.Pipeline.MultisampleState qualified as Vk.Ppl.MltSmplSt
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.Pipeline.ColorBlendAttachment qualified as Vk.Ppl.ClrBlndAtt
import Gpu.Vulkan.ColorComponent qualified as Vk.ClrCmp
import Gpu.Vulkan.Pipeline.ColorBlendState qualified as Vk.Ppl.ClrBlndSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.Ppl.Layout
import Gpu.Vulkan.Attachment qualified as Vk.Att
import Gpu.Vulkan.Subpass qualified as Vk.Subpass
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPass
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPass.M
import Gpu.Vulkan.Pipeline.Graphics qualified as Vk.Ppl.Graphics
import Gpu.Vulkan.Framebuffer qualified as Vk.Frmbffr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr.M
import Gpu.Vulkan.Semaphore qualified as Vk.Semaphore
import Gpu.Vulkan.Fence qualified as Vk.Fence
import Gpu.Vulkan.VertexInput qualified as Vk.VtxInp
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Memory qualified as Vk.Mm.M
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.Cmd qualified as Vk.Cmd

import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscSetLyt
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSet

import Gpu.Vulkan.Sampler qualified as Vk.Smplr
import Gpu.Vulkan.Sampler qualified as Vk.Smplr.M
import Gpu.Vulkan.Pipeline.DepthStencilState qualified as Vk.Ppl.DptStnSt

import Vertex
import Vertex.Wavefront qualified as Wf

import Graphics.SimplePolygon.DebugMessenger qualified as DbgMsngr
import Graphics.SimplePolygon.Instance qualified as Ist
import Graphics.SimplePolygon.Window qualified as Win
import Graphics.SimplePolygon.Surface qualified as Sfc
import Graphics.SimplePolygon.PhysicalDevice (QFamIndices(..))
import Graphics.SimplePolygon.PhysicalDevice qualified as PhDvc
import Options.Declarative hiding (run)
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM

import Debug

import Data.Ord.ToolsYj
import Data.Bits.ToolsYj
import Data.Sequences.ToolsYj
import Data.Bool.ToolsYj
import Data.IORef.ToolsYj
import Data.Maybe.ToolsYj

import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Data.List.ToolsYj
import Data.TypeLevel.List qualified as TList

main :: IO ()
main = run_ realMain

realMain ::
	Flag "t" '["texture"] "FILEPATH" "texture image file path" [FilePath] ->
	Flag "m" '["model"] "FILEPATH" "model filepath"
		(Def "../../../../../files/models/viking_room.obj" String) ->
	Cmd "Try multisampling" ()
realMain txfp mdfp = liftIO $
	supplyTxImg (NE.fromList $ get txfp) >>= \(lb, txi, tctxi) ->
	Win.create windowSize windowName \(Win.W w fr) ->
	Ist.create debug \ist ->
	Sfc.create ist w \sfc -> phdvc ist sfc >>= \pd ->
	indexing <$> Wf.readFile (get mdfp) >>= \mdl ->
	bool id (DbgMsngr.setup ist) debug
		$ body lb txi tctxi mdl 0 fr w sfc pd
	where phdvc ist sfc = PhDvc.enumerate ist sfc >>= \case
		[] -> error "failed to find a suitable GPU"; pd : _ -> pure pd

windowName :: String
windowName = "TRY MULTISAMPLING"

windowSize :: (Int, Int)
windowSize = (width, height) where width = 800; height = 600

supplyTxImg :: NE.NonEmpty FilePath -> IO (TChan a, ImageRgba8, TChan ImageRgba8)
supplyTxImg txfps =
	atomically newTChan >>= \lb ->
	rdimg `mapM` txfps >>= \txis@(txi NE.:| _) ->
	atomically newTChan >>= \ctxi ->
	((lb, txi, ctxi) <$) . forkIO $ for_ (Inf.tail $ cycle' txis) \ti ->
		atomically $ readTChan lb >> writeTChan ctxi ti
	where rdimg fp = ImageRgba8 . either error convertRGBA8 <$> readImage fp

type FramebufferResized = IORef Bool

vldLayers :: [Vk.LayerName]
vldLayers = [Vk.layerKhronosValidation]

body :: BObj.IsImage tximg => TChan () -> tximg -> TChan tximg ->
	(V.Vector WVertex, V.Vector Word32) ->
	Float -> FramebufferResized -> GlfwG.Win.W sw -> Sfc.S ss -> PhDvc.P ->
	IO ()
body lb txi tctxi (vtcs, idcs) mnld fr w sfc (PhDvc.P pd qfis) =
	getMaxUsableSampleCount pd >>= \spcnt ->
	createLgDvc pd qfis \d gq pq ->
	createCmdPl qfis d \cp ->
	createSwpch w sfc pd qfis d \(sc :: Vk.Khr.Swpch.S scifmt ss) ex ->
	Vk.Khr.Swpch.getImages d sc >>= \scis -> createImgVws d scis \scvs ->
	dptFmt pd Vk.Img.TilingOptimal \(_ :: Proxy dfmt) ->
	createClrRsrcs @scifmt pd d ex spcnt \crs@(_, _, cv, _) ->
	createDptRsrcs @dfmt pd d gq cp ex spcnt \drs@(_, _, dv) ->
	createRndrPss @scifmt @dfmt d spcnt \rp ->
	unfrmBffrOstAlgn pd \(_ :: Proxy alu) ->
	createPplLyt @alu d \dsl pl -> createGrPpl d ex rp pl spcnt \gp ->
	createFrmbffrs d ex rp scvs dv cv \fbs ->
	let	mls@(mls', _, _) = calcMipLevels txi in
	createTxImg pd d gq cp txi mls \tx txm ->
	generateMipmaps pd d gq cp tx mls >>
	Vk.ImgVw.create d (imgVwInfo tx Vk.Img.AspectColorBit mls') nil \tv ->
	createTxSmplr pd d mls mnld \txsp ->
	createVtxBffr pd d gq cp vtcs \vb ->
	createIdxBffr pd d gq cp idcs \ib ->
	tnum maxFramesInFlight \(_ :: Proxy mff) ->
	createMvpBffrs @mff pd d dsl \dsls mbs mbms ->
	createDscPl d \dp -> createDscSts d dp mbs dsls tv txsp \dss ->
	Vk.CBffr.allocate @_ @mff d (cmdBffrInfo cp) \cbs ->
	createSyncObjs @mff d \sos ->
	getCurrentTime >>=
	mainLoop lb tctxi fr w sfc pd qfis d gq pq cp
		sc ex scvs rp pl gp fbs crs drs vb ib mbms dss cbs sos
		tx txm tv txsp
	where
	tnum :: Int -> (forall (n :: [()]) . (
		TList.Length n, HPList.FromList n,
		HPList.HomoList '() n, HPList.RepM n,
		CreateMvpBffrs n ) => Proxy n -> a) -> a
	tnum 0 f = f (Proxy @'[])
	tnum n f = tnum (n - 1) \p -> f $ plus1 p
		where plus1 :: Proxy n -> Proxy ('() ': n); plus1 Proxy = Proxy

maxFramesInFlight :: Integral n => n
maxFramesInFlight = 2

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

createDptRsrcs :: forall fmt sd sc nm a . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> Vk.Extent2d ->
	Vk.Sample.CountFlags ->
	(forall sm si sv . DptRsrcs si sm nm fmt sv -> IO a) -> IO a
createDptRsrcs pd dv gq cp (Vk.Extent2d w h) spcnt a =
	prepareImg pd dv
		Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mm.PropertyDeviceLocalBit w h 1 spcnt \di dm ->
	Vk.ImgVw.create dv (imgVwInfo di Vk.Img.AspectDepthBit 1) nil \dvw ->
	transitionImgLyt dv gq cp di Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal 1 >>
	a (di, dm, dvw)

recreateDepthResources :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Extent2d -> Vk.Sample.CountFlags ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mm.M
		sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)] ->
	Vk.ImgVw.I nm fmt sdiv -> IO ()
recreateDepthResources phdvc dvc gq cp ext mss dptImg dptImgMem dptImgVw = do
	print ext
	recreateImage phdvc dvc
		(Vk.extent2dWidth ext) (Vk.extent2dHeight ext) 1 mss
		Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mm.PropertyDeviceLocalBit dptImg dptImgMem
	recreateImageView dvc dptImg Vk.Img.AspectDepthBit dptImgVw 1
	transitionImageLayout dvc gq cp dptImg Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal 1

type DptRsrcs sb sm nm fmt svw = (
	Vk.Img.Binded sm sb nm fmt,
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)], Vk.ImgVw.I nm fmt svw )

class CreateMvpBffrs (mff :: [()]) where
	createMvpBffrs :: KnownNat al => Vk.Phd.P -> Vk.Dvc.D sd ->
		Vk.DscSetLyt.D sdsl (DscStLytArg al) ->
		(forall sls smsbs . (
			HPList.FromList sls, Vk.DscSet.DListFromMiddle sls,
			HPList.HomoList '(sdsl, DscStLytArg al) sls,
			UpdateTexture al sls (AtomUbo sdsl al),
			Update al smsbs sls ) =>
			HPList.PL (U2 Vk.DscSetLyt.D) sls ->
			HPList.PL (BindedModelViewProj al nm) smsbs ->
			HPList.PL (MemoryModelViewProj al nm) smsbs -> IO a) ->
		IO a

instance CreateMvpBffrs '[] where
	createMvpBffrs _ _ _ f = f HPList.Nil HPList.Nil HPList.Nil

instance CreateMvpBffrs mff => CreateMvpBffrs ('() ': mff) where
	createMvpBffrs pd dv dl f = createMvpBffr pd dv \b m ->
		createMvpBffrs @mff pd dv dl \dls bs ms -> f (U2 dl :** dls)
			(BindedModelViewProj b :** bs)
			(MemoryModelViewProj m :** ms)

createMvpBffr :: KnownNat alm => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb mnm '[AtomModelViewProj alm] ->
	ModelViewProjMemory sm sb mnm alm -> IO b) -> IO b
createMvpBffr = createBffrAtm
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

createBffrAtm :: forall al sd nm a b . (KnownNat al, Storable a) =>
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Obj.Atom al a 'Nothing] ->
		Vk.Mm.M sm '[ '(
			sb, 'Vk.Mm.BufferArg nm '[Obj.Atom al a 'Nothing] )] ->
		IO b) -> IO b
createBffrAtm us prs p dv = createBffr p dv Obj.LengthAtom us prs

type ModelViewProjMemory sm sb mnm alm =
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg mnm '[AtomModelViewProj alm])]

type DscStLytArg alm = '[BufferModelViewProj alm, TxImg]
type BufferModelViewProj alm = 'Vk.DscSetLyt.Buffer '[AtomModelViewProj alm]
type AtomModelViewProj alm = Obj.Atom alm WModelViewProj 'Nothing
type TxImg = 'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)]

calcMipLevels :: BObj.IsImage img => img -> MipLevels
calcMipLevels img = (mls, w, h) where
	w, h :: Num n => n
	w = fromIntegral $ BObj.imageWidth img; h = fromIntegral $ BObj.imageHeight img
	mls = floor @Double (logBase 2 $ max w h) + 1

type MipLevels = (Word32, Int32, Int32)

createTxImg :: forall sd scp img inm a .  BObj.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp -> img ->
	MipLevels -> (forall si sm .
		Vk.Img.Binded sm si inm (BObj.ImageFormat img) ->
		Vk.Mm.M sm '[ '(si, Vk.Mm.ImageArg inm (BObj.ImageFormat img))] ->
		IO a) -> IO a
createTxImg pd d gq cp img (mls, _, _) a = prepareImg pd d Vk.Img.TilingOptimal
	(	Vk.Img.UsageTransferSrcBit .|.
		Vk.Img.UsageTransferDstBit .|.
		Vk.Img.UsageSampledBit )
	Vk.Mm.PropertyDeviceLocalBit w h mls Vk.Sample.Count1Bit \i m -> do
	createBffrImg pd d
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) img
		\(b :: Vk.Bffr.Binded sm sb inm '[bimg]) bm -> do
		Vk.Mm.write @inm @bimg @0 d bm zeroBits img
		transitionImgLyt d gq cp i
			Vk.Img.LayoutUndefined Vk.Img.LayoutTransferDstOptimal mls
		copyBffrToImg d gq cp b i
	a i m
	where
	w = fromIntegral $ BObj.imageWidth img
	h = fromIntegral $ BObj.imageHeight img

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

createBffrImg :: forall sd img nm inm a . BObj.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	img -> (forall sm sb al . KnownNat al =>
		Vk.Bffr.Binded sm sb nm '[Obj.Image al img inm] ->
		Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.BufferArg nm '[Obj.Image al img inm]) ] ->
		IO a) -> IO a
createBffrImg p dv us prs img a =
	bffrAlgn @(Obj.Image 256 img inm) dv ln us \(_ :: Proxy al) ->
	createBffr @_ @_ @(Obj.Image al img inm) p dv ln us prs a
	where
	ln :: Obj.Length (Obj.Image al img inm)
	ln = Obj.LengthImage
		(BObj.imageRow img) (BObj.imageWidth img)
		(BObj.imageHeight img) (BObj.imageDepth img)

createBffr :: forall sd bnm o a . Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Obj.Length o ->
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
	Obj.Length o -> Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo 'Nothing '[o]
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
	suit prs1 = fst <$> find ((&&)
		<$> (`Vk.Mm.elemTypeIndex` flt) . fst
		<*> checkBits prs . Vk.Mm.mTypePropertyFlags . snd)
			(Vk.Phd.memoryPropertiesMemoryTypes prs1)

bffrAlgn :: forall o sd a . Obj.SizeAlignment o =>
	Vk.Dvc.D sd -> Obj.Length o -> Vk.Bffr.UsageFlags ->
	(forall al . KnownNat al => Proxy al -> IO a) -> IO a
bffrAlgn dv ln us f = Vk.Bffr.create dv (bffrInfo ln us) nil \b ->
	(\(SomeNat p) -> f p) . someNatVal . fromIntegral =<<
	Vk.Mm.requirementsAlignment <$> Vk.Bffr.getMemoryRequirements dv b

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

memInfo :: Vk.Mm.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
memInfo mt = Vk.Mm.AllocateInfo {
	Vk.Mm.allocateInfoNext = TMaybe.N,
	Vk.Mm.allocateInfoMemoryTypeIndex = mt }

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

createTxSmplr :: Vk.Phd.P -> Vk.Dvc.D sd -> MipLevels -> Float ->
	(forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTxSmplr pd dv (mls, _, _) mnld a = Vk.Phd.getProperties pd >>= \pr ->
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
		Vk.Smplr.createInfoMinLod = mnld,
		Vk.Smplr.createInfoMaxLod = fromIntegral mls,
		Vk.Smplr.createInfoBorderColor = Vk.BorderColorIntOpaqueBlack,
		Vk.Smplr.createInfoUnnormalizedCoordinates = False }

recreateTexture :: forall img sd sc siv si3 sm2 a . BObj.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> img ->
	Vk.Img.M.Binded sm2 si3 "texture" (BObj.ImageFormat img) ->
	Vk.Mm.M.M sm2 '[ '(si3,
		'Vk.Mm.M.ImageArg "texture" (BObj.ImageFormat img))] ->
	Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv -> IO a -> IO ()
recreateTexture phdv dv gq cp tximg tx txmem txiv act =
	putStrLn "RECREATE TEXTURE" >>
	recreateTextureImage phdv dv gq cp tximg tx txmem \mplvs ->
	recreateImageView' @'Vk.T.FormatR8g8b8a8Srgb dv tx Vk.Img.AspectColorBit txiv mplvs act -- mplvs

getMaxUsableSampleCount :: Vk.Phd.P -> IO Vk.Sample.CountFlags
getMaxUsableSampleCount phdvc = do
	cnts <- Vk.Phd.limitsFramebufferDepthSampleCounts . Vk.Phd.propertiesLimits
		<$> Vk.Phd.getProperties phdvc
	print cnts
	pure . fromMaybe Vk.Sample.Count1Bit $ findBit [
		Vk.Sample.Count64Bit, Vk.Sample.Count32Bit,
		Vk.Sample.Count16Bit, Vk.Sample.Count8Bit,
		Vk.Sample.Count4Bit, Vk.Sample.Count2Bit ] cnts

findBit :: Bits b => [b] -> b -> Maybe b
findBit bl bs = find ((/= zeroBits) . (.&. bs)) bl

dvcExtensions :: [Vk.Phd.ExtensionName]
dvcExtensions = [Vk.Khr.Swpch.extensionName]

createImgVws :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Img.Binded ss ss nm fmt] ->
	(forall si . HPList.PL (Vk.ImgVw.I nm fmt) si -> IO a) -> IO a
createImgVws _dvc [] f = f HPList.Nil
createImgVws dvc (sci : scis) f =
	createImageView dvc sci Vk.Img.AspectColorBit 1 \sciv ->
	createImgVws dvc scis \scivs -> f $ sciv :** scivs

recreateImageViews :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss nm scfmt] -> HPList.PL (Vk.ImgVw.I nm scfmt) sis -> IO ()
recreateImageViews _dvc [] HPList.Nil = pure ()
recreateImageViews dvc (sci : scis) (iv :** ivs) =
	Vk.ImgVw.unsafeRecreate dvc (mkImageViewCreateInfo sci Vk.Img.AspectColorBit 1) nil iv >>
	recreateImageViews dvc scis ivs
recreateImageViews _ _ _ =
	error "number of Vk.Image.M.I and Vk.ImageView.M.I should be same"

createImageView :: forall ivfmt sd si sm nm ifmt a .
	Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	Vk.Img.AspectFlags -> Word32 ->
	(forall siv . Vk.ImgVw.I nm ivfmt siv -> IO a) -> IO a
createImageView dvc timg asps mplvs f =
	Vk.ImgVw.create dvc (mkImageViewCreateInfo timg asps mplvs) nil f

recreateImageView :: Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	Vk.Img.AspectFlags ->
	Vk.ImgVw.I nm ivfmt s -> Word32 -> IO ()
recreateImageView dvc timg asps iv mplvs =
	Vk.ImgVw.unsafeRecreate dvc (mkImageViewCreateInfo timg asps mplvs) nil iv

recreateImageView' :: Vk.T.FormatToValue ivfmt =>
	Vk.Dvc.D sd -> Vk.Img.Binded sm si nm ifmt ->
	Vk.Img.AspectFlags ->
	Vk.ImgVw.I nm ivfmt s -> Word32 -> IO a -> IO ()
recreateImageView' dvc timg asps iv mplvs =
	Vk.ImgVw.unsafeRecreate' dvc (mkImageViewCreateInfo timg asps mplvs) nil iv

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

createRndrPss ::
	forall (scifmt :: Vk.T.Format) (dptfmt :: Vk.T.Format) sd a . (
	Vk.T.FormatToValue scifmt, Vk.T.FormatToValue dptfmt ) =>
	Vk.Dvc.D sd -> Vk.Sample.CountFlags ->
	(forall sr . Vk.RndrPass.R sr -> IO a) -> IO a
createRndrPss dvc mss f = do
	let	colorAttachment :: Vk.Att.Description scifmt
		colorAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = zeroBits,
			Vk.Att.descriptionSamples = mss,
			Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
			Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
			Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOp =
				Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayout =
				Vk.Img.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
		colorAttachmentRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 0,
			Vk.Att.referenceLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
		depthAttachment :: Vk.Att.Description dptfmt
		depthAttachment = Vk.Att.Description {
			Vk.Att.descriptionFlags = zeroBits,
			Vk.Att.descriptionSamples = mss,
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
		colorAttachmentResolve = Vk.Att.Description {
			Vk.Att.descriptionFlags = zeroBits,
			Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
			Vk.Att.descriptionLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
			Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
			Vk.Att.descriptionStencilStoreOp =
				Vk.Att.StoreOpDontCare,
			Vk.Att.descriptionInitialLayout =
				Vk.Img.LayoutUndefined,
			Vk.Att.descriptionFinalLayout =
				Vk.Img.LayoutPresentSrcKhr }
		colorAttachmentResolveRef = Vk.Att.Reference {
			Vk.Att.referenceAttachment = 2,
			Vk.Att.referenceLayout =
				Vk.Img.LayoutColorAttachmentOptimal }
		subpass = Vk.Subpass.Description {
			Vk.Subpass.descriptionFlags = zeroBits,
			Vk.Subpass.descriptionPipelineBindPoint =
				Vk.Ppl.BindPointGraphics,
			Vk.Subpass.descriptionInputAttachments = [],
			Vk.Subpass.descriptionColorAndResolveAttachments =
				Right [(colorAttachmentRef, colorAttachmentResolveRef)],
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
				colorAttachment :** depthAttachment :**
				colorAttachmentResolve :** HPList.Nil,
			Vk.RndrPass.M.createInfoSubpasses = [subpass],
			Vk.RndrPass.M.createInfoDependencies = [dependency] }
	Vk.RndrPass.create @'Nothing @'[scifmt, dptfmt, scifmt] dvc renderPassInfo nil \rp -> f rp

type AtomUbo s alu = '(s, '[
	'Vk.DscSetLyt.Buffer '[Obj.Atom alu WModelViewProj 'Nothing],
	'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ])

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscSetLyt.D s '[
		'Vk.DscSetLyt.Buffer '[Obj.Atom alu WModelViewProj 'Nothing],
		'Vk.DscSetLyt.Image
			'[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ] -> IO a) ->
	IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
		'Vk.DscSetLyt.Buffer '[Obj.Atom alu WModelViewProj 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings =
			uboLayoutBinding :**
			samplerLayoutBinding :** HPList.Nil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[Obj.Atom alu WModelViewProj 'Nothing])
	uboLayoutBinding = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	samplerLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)])
	samplerLayoutBinding = Vk.DscSetLyt.BindingImage {
		Vk.DscSetLyt.bindingImageDescriptorType =
			Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscSetLyt.bindingImageStageFlags =
			Vk.ShaderStageFragmentBit }

unfrmBffrOstAlgn ::
	Vk.Phd.P -> (forall a . KnownNat a => Proxy a -> IO b) -> IO b
unfrmBffrOstAlgn pd f = (\(SomeNat p) -> f p) . someNatVal . fromIntegral
	. Vk.Phd.limitsMinUniformBufferOffsetAlignment . Vk.Phd.propertiesLimits
	=<< Vk.Phd.getProperties pd

createPplLyt :: forall alu sd b .
	Vk.Dvc.D sd -> (forall sdsl sl .
		Vk.DscSetLyt.D sdsl '[
			'Vk.DscSetLyt.Buffer '[Obj.Atom alu WModelViewProj 'Nothing],
			'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ] ->
		Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] -> IO b) -> IO b
createPplLyt dvc f =
	createDescriptorSetLayout dvc \dsl ->
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = TMaybe.N,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts =
				U2 dsl :** HPList.Nil } in
	Vk.Ppl.Layout.create @'Nothing @_ @_ @'[] dvc pipelineLayoutInfo nil $ f dsl

createGrPpl :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Sample.CountFlags ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl alu], '[]) -> IO a) -> IO a
createGrPpl dvc sce rp ppllyt mss f =
	Vk.Ppl.Graphics.createGs dvc Nothing (U14 pplInfo :** HPList.Nil)
			nil \(U3 gpl :** HPList.Nil) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo LeaveFrontFaceCounterClockwise sce rp ppllyt mss

recreateGraphicsPipeline :: Culling -> Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Sample.CountFlags ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl alu], '[]) -> IO ()
recreateGraphicsPipeline cll dvc sce rp ppllyt mss gpls = Vk.Ppl.Graphics.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HPList.Nil) nil (U3 gpls :** HPList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo cll sce rp ppllyt mss

mkGraphicsPipelineCreateInfo :: Culling ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Sample.CountFlags ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Pos), '(1, Color), '(2, TexCoord)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'Nothing '(sl, '[AtomUbo sdsl alu], '[]) sr '(sb, vs', ts', sbtss')
mkGraphicsPipelineCreateInfo cll sce rp ppllyt mss = Vk.Ppl.Graphics.CreateInfo {
	Vk.Ppl.Graphics.createInfoNext = TMaybe.N,
	Vk.Ppl.Graphics.createInfoFlags = Vk.Ppl.CreateFlagsZero,
	Vk.Ppl.Graphics.createInfoStages = shaderStages,
	Vk.Ppl.Graphics.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Graphics.createInfoInputAssemblyState = Just inputAssembly,
	Vk.Ppl.Graphics.createInfoViewportState = Just $ mkViewportState sce,
	Vk.Ppl.Graphics.createInfoRasterizationState = Just $ rasterizer cll,
	Vk.Ppl.Graphics.createInfoMultisampleState = Just $ multisampling mss,
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

shaderStages :: HPList.PL (U5 Vk.Ppl.ShdrSt.CreateInfo) '[
	'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
	'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
shaderStages = U5 vertShaderStageInfo :** U5 fragShaderStageInfo :** HPList.Nil
	where
	vertShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModule = (
			shaderModuleCreateInfo glslVertexShaderMain, nil ),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	fragShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModule = (
			shaderModuleCreateInfo glslFragmentShaderMain, nil ),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }

inputAssembly :: Vk.Ppl.InpAsmbSt.CreateInfo 'Nothing
inputAssembly = Vk.Ppl.InpAsmbSt.CreateInfo {
	Vk.Ppl.InpAsmbSt.createInfoNext = TMaybe.N,
	Vk.Ppl.InpAsmbSt.createInfoFlags = zeroBits,
	Vk.Ppl.InpAsmbSt.createInfoTopology = Vk.PrimitiveTopologyTriangleList,
	Vk.Ppl.InpAsmbSt.createInfoPrimitiveRestartEnable = False }

mkViewportState :: Vk.Extent2d -> Vk.Ppl.ViewportSt.CreateInfo 'Nothing
mkViewportState sce = Vk.Ppl.ViewportSt.CreateInfo {
	Vk.Ppl.ViewportSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ViewportSt.createInfoFlags = zeroBits,
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

rasterizer :: Culling -> Vk.Ppl.RstSt.CreateInfo 'Nothing
rasterizer cll = Vk.Ppl.RstSt.CreateInfo {
	Vk.Ppl.RstSt.createInfoNext = TMaybe.N,
	Vk.Ppl.RstSt.createInfoFlags = zeroBits,
	Vk.Ppl.RstSt.createInfoDepthClampEnable = False,
	Vk.Ppl.RstSt.createInfoRasterizerDiscardEnable = False,
	Vk.Ppl.RstSt.createInfoPolygonMode = Vk.PolygonModeFill,
	Vk.Ppl.RstSt.createInfoLineWidth = 1,
	Vk.Ppl.RstSt.createInfoCullMode = cm,
	Vk.Ppl.RstSt.createInfoFrontFace = ff,
	Vk.Ppl.RstSt.createInfoDepthBiasEnable = False,
	Vk.Ppl.RstSt.createInfoDepthBiasConstantFactor = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasClamp = 0,
	Vk.Ppl.RstSt.createInfoDepthBiasSlopeFactor = 0 }
	where (cm, ff) = cullingToCullModes cll

data Culling =
	NoCulling | LeaveFrontFaceCounterClockwise | LeaveFrontFaceClockwise
	deriving Show

cullingToCullModes :: Culling -> (Vk.CullModeFlags, Vk.FrontFace)
cullingToCullModes = \case
	NoCulling -> (Vk.CullModeNone, Vk.FrontFaceCounterClockwise)
	LeaveFrontFaceCounterClockwise ->
		(Vk.CullModeBackBit, Vk.FrontFaceCounterClockwise)
	LeaveFrontFaceClockwise -> (Vk.CullModeBackBit, Vk.FrontFaceClockwise)

multisampling :: Vk.Sample.CountFlags -> Vk.Ppl.MltSmplSt.CreateInfo 'Nothing
multisampling mss = Vk.Ppl.MltSmplSt.CreateInfo {
	Vk.Ppl.MltSmplSt.createInfoNext = TMaybe.N,
	Vk.Ppl.MltSmplSt.createInfoFlags = zeroBits,
	Vk.Ppl.MltSmplSt.createInfoSampleShadingEnable = False,
	Vk.Ppl.MltSmplSt.createInfoRasterizationSamplesAndMask =
		Vk.Sample.CountAndMask mss Nothing,
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

createFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d ->
	Vk.RndrPass.R sr -> HPList.PL (Vk.ImgVw.I nm fmt) sis ->
	Vk.ImgVw.I dptnm dptfmt siv ->
	Vk.ImgVw.I clrnm fmt slrsiv ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HPList.Nil _ _ f = f HPList.Nil
createFrmbffrs dvc sce rp (iv :** ivs) dptiv clriv f =
	Vk.Frmbffr.create dvc (mkFramebufferCreateInfo sce rp iv clriv dptiv) nil \fb ->
	createFrmbffrs dvc sce rp ivs dptiv clriv \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPass.R sr -> HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
		Vk.ImgVw.I clrnm scfmt clrsdiv ->
		Vk.ImgVw.I dptfmt dptnm sdiv ->
		HPList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HPList.Nil _ _ HPList.Nil = pure ()

instance RecreateFramebuffers sis sfs =>
	RecreateFramebuffers (si ': sis) (sf ': sfs) where
	recreateFramebuffers dvc sce rp (sciv :** scivs) clriv dptiv (fb :** fbs) =
		Vk.Frmbffr.unsafeRecreate dvc
			(mkFramebufferCreateInfo sce rp sciv clriv dptiv) nil fb >>
		recreateFramebuffers dvc sce rp scivs clriv dptiv fbs

mkFramebufferCreateInfo ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.ImgVw.I nm fmt si ->
	Vk.ImgVw.I clrnm fmt clrsdiv ->
	Vk.ImgVw.I dptnm dptfmt sdiv ->
	Vk.Frmbffr.CreateInfo 'Nothing sr
		'[ '(clrnm, fmt, clrsdiv), '(dptnm, dptfmt, sdiv), '(nm, fmt, si)]
mkFramebufferCreateInfo sce rp attch clr dpt = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = U3 clr :** U3 dpt :** U3 attch :** HPList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where
	Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = sce

createCmdPl :: PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfis dvc f =
	Vk.CmdPl.create dvc poolInfo nil \cp -> f cp
	where poolInfo = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags =
			Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = PhDvc.grFam qfis }

type ColorResources nm fmt si sm siv = (
	Vk.Img.Binded sm si nm fmt,
	Vk.Mm.M sm
		'[ '(si, 'Vk.Mm.ImageArg nm fmt)],
	Vk.ImgVw.I nm fmt siv,
	Vk.Sample.CountFlags )

recreateTextureImage :: forall img sd sc nm a sm si . BObj.IsImage img =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> img ->
		Vk.Img.Binded sm si nm (BObj.ImageFormat img) ->
		Vk.Mm.M.M sm '[ '(si, 'Vk.Mm.M.ImageArg nm (BObj.ImageFormat img))] -> (Word32 -> IO a) -> IO ()
recreateTextureImage phdvc dvc gq cp img tx txmem act = do
	let	wdt_ = BObj.imageWidth img
		hgt_ = BObj.imageHeight img
		wdt, hgt :: Num i => i
		wdt = fromIntegral wdt_
		hgt = fromIntegral hgt_
		mipLevels :: Word32 = floor @Double . logBase 2 $ max wdt hgt
	recreateImage' @(BObj.ImageFormat img)
		phdvc dvc wdt hgt mipLevels Vk.Sample.Count1Bit Vk.Img.TilingOptimal
		(	Vk.Img.UsageTransferSrcBit .|.
			Vk.Img.UsageTransferDstBit .|.
			Vk.Img.UsageSampledBit) Vk.Mm.PropertyDeviceLocalBit tx txmem do
		createBufferImage @img @_ phdvc dvc
			(wdt, wdt, hgt, 1)
			Vk.Bffr.UsageTransferSrcBit
			(	Vk.Mm.PropertyHostVisibleBit .|.
				Vk.Mm.PropertyHostCoherentBit )
			\(sb :: Vk.Bffr.Binded
				sm2 sb "texture-buffer" '[ Obj.Image 1 img inm]) sbm -> do
			Vk.Mm.write @"texture-buffer"
				@(Obj.Image 1 img inm) @0 dvc sbm zeroBits img
			print sb
			transitionImageLayout dvc gq cp tx
				Vk.Img.LayoutUndefined
				Vk.Img.LayoutTransferDstOptimal mipLevels
			copyBufferToImage dvc gq cp sb tx wdt hgt
			generateMipmapsOld phdvc dvc gq cp tx mipLevels wdt hgt
		act mipLevels

newtype ImageRgba8 = ImageRgba8 (Image PixelRGBA8)

instance BObj.IsImage ImageRgba8 where
	type ImagePixel ImageRgba8 = MyRgba8
	type ImageFormat ImageRgba8 = 'Vk.T.FormatR8g8b8a8Srgb
	imageRow = BObj.imageWidth
	imageWidth (ImageRgba8 img) = fromIntegral $ imageWidth img
	imageHeight (ImageRgba8 img) = fromIntegral $ imageHeight img
	imageDepth _ = 1
	imageBody (ImageRgba8 img) = (<$> [0 .. imageHeight img - 1]) \y ->
		(<$> [0 .. imageWidth img - 1]) \x -> MyRgba8 $ pixelAt img x y
	imageMake w h _d pss = ImageRgba8 $ generateImage
		(\x y -> let MyRgba8 p = (pss' ! y) ! x in p)
		(fromIntegral w) (fromIntegral h)
		where pss' = listArray
			(0, fromIntegral h - 1)
			(listArray (0, fromIntegral w - 1) <$> pss)

newtype MyRgba8 = MyRgba8 { _unMyRgba8 :: PixelRGBA8 }

instance Storable MyRgba8 where
	sizeOf _ = 4 * sizeOf @Pixel8 undefined
	alignment _ = alignment @Pixel8 undefined
	peek p = MyRgba8 . (\(r, g, b, a) -> PixelRGBA8 r g b a) . listToTuple4
		<$> peekArray 4 (castPtr p)
	poke p (MyRgba8 (PixelRGBA8 r g b a)) =
		pokeArray (castPtr p) [r, g, b, a]

checkImageFilterLinearBit ::
	forall fmt . Vk.T.FormatToValue fmt => Vk.Phd.P -> IO ()
checkImageFilterLinearBit phdvc = do
	let	fmt = Vk.T.formatToValue @fmt
	formatProperties <- Vk.Phd.getFormatProperties phdvc fmt
	let	bt = Vk.formatPropertiesOptimalTilingFeatures
				formatProperties .&.
			Vk.FormatFeatureSampledImageFilterLinearBit
	when (bt == zeroBits) $ error
		"texture image format does not support linear blitting!"

generateMipmapsOld :: forall sd scp si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp ->
	Vk.Img.Binded sm si nm fmt -> Word32 -> Int32 -> Int32 -> IO ()
generateMipmapsOld phdvc dvc gq cp img mlvs wdt hgt = do
	putStrLn "GENERATE MIPMAPS"
	checkImageFilterLinearBit @fmt phdvc
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

generateMipmaps :: forall sd scp sm si nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp ->
	Vk.Img.Binded sm si nm fmt -> MipLevels -> IO ()
generateMipmaps pd dv gq cp img (mls, w0, h0) = do
	checkImgFilterLinearBit @fmt pd
	singleTimeCmds dv gq cp \cb -> do
		for_ whs \(i, (w, h)) -> generateMipmap1 cb img i w h
		Vk.Cmd.pipelineBarrier cb
			Vk.Ppl.StageTransferBit Vk.Ppl.StageFragmentShaderBit
			zeroBits HPList.Nil HPList.Nil
			. HPList.Singleton . U5 $ mipmapBarrier
				Vk.AccessTransferWriteBit Vk.AccessShaderReadBit
				Vk.Img.LayoutTransferDstOptimal
				Vk.Img.LayoutShaderReadOnlyOptimal img mls
	where whs = [1 .. mls - 1] `zip` (iterate half w0 `zip` iterate half h0)

checkImgFilterLinearBit ::
	forall fmt . Vk.T.FormatToValue fmt => Vk.Phd.P -> IO ()
checkImgFilterLinearBit pd = Vk.Phd.getFormatProperties pd fmt >>= \prs ->
	when (not . checkBits Vk.FormatFeatureSampledImageFilterLinearBit
			$ Vk.formatPropertiesOptimalTilingFeatures prs)
		$ error "texture image format does not support linear blitting!"
	where fmt = Vk.T.formatToValue @fmt

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

copyBufferToImage :: forall sd sc sm sb nm img inm si sm' nm' .
	Storable (BObj.ImagePixel img) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[ Obj.Image 1 img inm]  ->
	Vk.Img.Binded sm' si nm' (BObj.ImageFormat img) ->
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

transitionImageLayout :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Img.Binded sm si nm fmt -> Vk.Img.Layout -> Vk.Img.Layout -> Word32 ->
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

createBufferImage :: Storable (BObj.ImagePixel t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> (Device.M.Size, Device.M.Size, Device.M.Size, Device.M.Size) ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ Obj.Image 1 t inm] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[ Obj.Image 1 t inm])] ->
		IO a) -> IO a
createBufferImage p dv (r, w, h, d) usg props =
	createBuffer' p dv (Obj.LengthImage r w h d) usg props

createBufferList :: forall sd nm t a . Storable t =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Device.M.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Obj.List 256 t ""] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[Obj.List 256 t ""] ) ] ->
		IO a) ->
	IO a
createBufferList p dv ln usg props =
	createBuffer' p dv (Obj.LengthList ln) usg props

createBuffer' :: forall sd nm o a . Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer' p dv ln usg props f = Vk.Bffr.create dv binfo nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mm.M.requirementsMemoryTypeBits reqs) props
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(allcInfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) -> bnd
	where
	binfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	binfo = Vk.Bffr.CreateInfo {
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

recreateImage :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mm.PropertyFlags ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mm.M
		sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)] -> IO ()
recreateImage pd dvc wdt hgt mplvs mss tlng usg prps img mem = do
	Vk.Img.unsafeRecreate @'Nothing dvc
		(imageInfo wdt hgt mplvs mss tlng usg) nil img
	minfo <- imageMemoryInfoBinded pd dvc prps img
	imageReallocateBind dvc img minfo mem

recreateImage' :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mm.PropertyFlags ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mm.M
		sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)] -> IO a -> IO ()
recreateImage' pd dvc wdt hgt mplvs mss tlng usg prps img mem act = do
	Vk.Img.unsafeRecreate' @'Nothing dvc
		(imageInfo wdt hgt mplvs mss tlng usg) nil img do
		minfo <- imageMemoryInfoBinded pd dvc prps img
		imageReallocateBind' dvc img minfo mem act

imageInfo ::
	Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags -> Vk.Img.Tiling -> Vk.Img.UsageFlags ->
	Vk.Img.CreateInfo 'Nothing fmt
imageInfo wdt hgt mplvs mss tlng usg = Vk.Img.CreateInfo {
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
		Vk.Img.createInfoSamples = mss,
		Vk.Img.createInfoFlags = zeroBits,
		Vk.Img.createInfoQueueFamilyIndices = [] }

imageReallocateBind ::
	Vk.Dvc.D sd -> Vk.Img.Binded sm sb nm fmt ->
	Vk.Mm.AllocateInfo 'Nothing ->
	Vk.Mm.M
		sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)] -> IO ()
imageReallocateBind dvc img minfo m =
	Vk.Mm.unsafeReallocateBind @'Nothing dvc
		(HPList.Singleton . U2 $ Vk.Mm.ImageBinded img) minfo
		nil m

imageReallocateBind' ::
	Vk.Dvc.D sd -> Vk.Img.Binded sm sb nm fmt ->
	Vk.Mm.AllocateInfo 'Nothing ->
	Vk.Mm.M
		sm '[ '(sb, 'Vk.Mm.ImageArg nm fmt)] -> IO a -> IO ()
imageReallocateBind' dvc img minfo m =
	Vk.Mm.unsafeReallocateBind' @'Nothing dvc
		(HPList.Singleton . U2 $ Vk.Mm.ImageBinded img) minfo
		nil m

imageMemoryInfoBinded ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Mm.PropertyFlags ->
	Vk.Img.Binded sm si nm fmt -> IO (Vk.Mm.AllocateInfo 'Nothing)
imageMemoryInfoBinded pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirementsBinded dvc img
	mt <- findMemoryType pd (Vk.Mm.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

createVtxBffr :: Vk.Phd.P ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> V.Vector WVertex -> (forall sm sb .
		Vk.Bffr.Binded sm sb "vertex-buffer" '[Obj.List 256 WVertex ""] -> IO a) -> IO a
createVtxBffr phdvc dvc gq cp vtcs f =
	createBufferList phdvc dvc (fromIntegral $ V.length vtcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit \b _ ->
	createBufferList phdvc dvc (fromIntegral $ V.length vtcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
		\(b' :: Vk.Bffr.Binded sm sb "vertex-buffer" '[Obj.List 256 t ""])
			(bm' :: Vk.Mm.M sm '[ '(
				sb,
				'Vk.Mm.BufferArg
					"vertex-buffer" '[Obj.List 256 WVertex ""] )]) -> do
	Vk.Mm.write @"vertex-buffer" @(Obj.List 256 WVertex "") @0 dvc bm' zeroBits vtcs
	copyBuffer dvc gq cp b' b
	f b

createIdxBffr :: Vk.Phd.P ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> V.Vector Word32 -> (forall sm sb .
		Vk.Bffr.Binded sm sb "index-buffer" '[Obj.List 256 Word32 ""] -> IO a) -> IO a
createIdxBffr phdvc dvc gq cp idcs f =
	createBufferList phdvc dvc (fromIntegral $ V.length idcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageIndexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit \b _ ->
	createBufferList phdvc dvc (fromIntegral $ V.length idcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
		\(b' :: Vk.Bffr.Binded sm sb "index-buffer" '[Obj.List 256 t ""])
			(bm' :: Vk.Mm.M sm '[ '(
				sb,
				'Vk.Mm.BufferArg "index-buffer"
					'[Obj.List 256 Word32 ""] )]) -> do
	Vk.Mm.write @"index-buffer" @(Obj.List 256 Word32 "") @0 dvc bm' zeroBits idcs
	copyBuffer dvc gq cp b' b
	f b

createDscPl :: Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDscPl dv = Vk.DscPl.create dv info nil
	where
	info = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = maxFramesInFlight,
		Vk.DscPl.createInfoPoolSizes = [sz0, sz1] }
	sz0 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPl.sizeDescriptorCount = maxFramesInFlight }
	sz1 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscPl.sizeDescriptorCount = maxFramesInFlight }

createDscSts :: (
	Vk.DscSet.DListFromMiddle sls,
	Update al smsbs sls ) =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	HPList.PL (BindedModelViewProj al nm) smsbs ->
	HPList.PL (U2 Vk.DscSetLyt.D) sls ->
	Vk.ImgVw.I Tx TxFmt siv -> Vk.Smplr.S ssmp ->
	(forall sds . HPList.PL (Vk.DscSet.D sds) sls -> IO a) -> IO a
createDscSts dv dp mbs dls tv tsp f =
	Vk.DscSet.allocateDs dv info $ (>>) <$> update dv mbs tv tsp <*> f
	where info = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dp,
		Vk.DscSet.allocateInfoSetLayouts = dls }

class Update al smsbs slbtss where
	update :: Vk.Dvc.D sd -> HPList.PL (BindedModelViewProj al nm) smsbs ->
		Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv ->
		Vk.Smplr.S ssmp -> HPList.PL (Vk.DscSet.D sds) slbtss -> IO ()

instance Update _al '[] '[] where update _ HPList.Nil _ _ HPList.Nil = pure ()

instance (
	KnownNat al,
	Vk.DscSet.BindingAndArrayElemBuffer
		cs '[Obj.Atom al WModelViewProj 'Nothing] 0,
	Vk.DscSet.UpdateDynamicLength
		cs '[Obj.Atom al WModelViewProj 'Nothing],
	Vk.DscSet.BindingAndArrayElemImage cs '[ '(Tx, TxFmt)] 0,
	Update al smsbs slbtss ) =>
	Update al (smsb ': smsbs) ('(ds, cs) ': slbtss) where
	update dv (BindedModelViewProj mb :** mbs) tv tsp (ds :** dss) =
		Vk.DscSet.updateDs dv (
			U5 (dscWrite0 ds mb) :**
			U5 (dscWrite1 ds tv tsp) :** HPList.Nil ) HPList.Nil >>
		update dv mbs tv tsp dss

dscWrite0 :: KnownNat alm => Vk.DscSet.D sds slbts ->
	Vk.Bffr.Binded sm sb bnm '[AtomModelViewProj alm] ->
	Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgBuffer '[ '(
			sm, sb, bnm, Obj.Atom alm WModelViewProj 'Nothing, 0 )]) 0
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

type Tx = "texture"
type TxFmt = Vk.T.FormatR8g8b8a8Srgb

data BindedModelViewProj al nm smsb where
	BindedModelViewProj ::
		Vk.Bffr.Binded sm sb nm '[AtomModelViewProj al] ->
		BindedModelViewProj al nm '(sm, sb)

data MemoryModelViewProj al nm smsb where
	MemoryModelViewProj ::
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg nm '[AtomModelViewProj al])] ->
		MemoryModelViewProj al nm '(sm, sb)

type BindedUbo = BindedModelViewProj

type MemoryUbo = MemoryModelViewProj

type family MapFst abs where
	MapFst '[] = '[]
	MapFst ( '(a, b, c :: k) ': abs) = a ': MapFst abs

descriptorWrite0 :: KnownNat alu =>
	Vk.Bffr.Binded sm sb nm '[Obj.Atom alu WModelViewProj 'Nothing] ->
	Vk.DscSet.D sds slbts ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, Obj.Atom alu WModelViewProj 'Nothing, 0 )]) 0
descriptorWrite0 ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HPList.Singleton bufferInfo
	}
	where bufferInfo = U5 $ Vk.Dsc.BufferInfo ub

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
			Vk.Dsc.imageInfoSampler = tsmp } }

class UpdateOld alu smsbs slbtss ssmp siv where
	updateOld ::
		Vk.Dvc.D sd ->
		HPList.PL (BindedUbo alu "uniform-buffer") smsbs ->
		HPList.PL (Vk.DscSet.D sds) slbtss ->
		Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv ->
		Vk.Smplr.S ssmp ->
		IO ()

instance UpdateOld alu '[] '[] ssmp siv where updateOld _ HPList.Nil HPList.Nil _ _ = pure ()

instance (
	KnownNat alu,
	Vk.DscSet.BindingAndArrayElemBuffer (TIndex.I1_2 '(ds, cs)) '[Obj.Atom alu WModelViewProj 'Nothing] 0,
	Vk.DscSet.UpdateDynamicLength (TIndex.I1_2 '(ds, cs)) '[Obj.Atom alu WModelViewProj 'Nothing],
	UpdateOld alu ubs dscss ssmp siv,
	Vk.DscSet.WriteSourcesToMiddle cs ('Vk.DscSet.WriteSourcesArgImage
			'[ '(ssmp, "texture", 'Vk.T.FormatR8g8b8a8Srgb, siv)]) 0
	) =>
	UpdateOld alu (ub ': ubs) ('(ds, cs) ': dscss) ssmp siv where
	updateOld dvc (BindedModelViewProj ub :** ubs) (dscs :** dscss) tximgvw txsmp = do
		Vk.DscSet.updateDs dvc (
			U5 (descriptorWrite0 ub dscs) :**
			U5 (descriptorWrite1 dscs tximgvw txsmp) :**
			HPList.Nil )
			HPList.Nil
		updateOld dvc ubs dscss tximgvw txsmp

class UpdateTexture alu slbtss dscs where
	updateTexture :: (
		Vk.DscSet.BindingAndArrayElemBuffer (TIndex.I1_2 dscs) '[Obj.Atom alu WModelViewProj 'Nothing] 0,
		Vk.DscSet.UpdateDynamicLength (TIndex.I1_2 dscs) '[Obj.Atom alu WModelViewProj 'Nothing],
		Vk.DscSet.WriteSourcesToMiddle (TIndex.I1_2 dscs) ('Vk.DscSet.WriteSourcesArgImage
			'[ '(ssmp, "texture", 'Vk.T.FormatR8g8b8a8Srgb, siv)]) 0,
		Vk.DscSet.WriteListUpdateDynamicLengths
			'[ '( 'Nothing, sds, dscs, 'Vk.DscSet.WriteSourcesArgImage
				'[ '(ssmp, "texture", 'Vk.T.FormatR8g8b8a8Srgb, siv)], 0)]
			) =>
		Vk.Dvc.D sd ->
		HPList.PL (Vk.DscSet.D sds) slbtss ->
		Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv ->
		Vk.Smplr.S ssmp -> IO ()

instance UpdateTexture alu '[] dscs where updateTexture _ HPList.Nil _ _ = pure ()

instance UpdateTexture alu dscss dscs =>
	UpdateTexture alu (dscs ': dscss) dscs where
	updateTexture dvc (dscs :** dscss) tximgvw txsmp = do
		Vk.DscSet.updateDs dvc (
			U5 (descriptorWrite1 dscs tximgvw txsmp) :**
			HPList.Nil )
			HPList.Nil
		updateTexture @alu @dscss @dscs dvc dscss tximgvw txsmp

findMemoryType :: Vk.Phd.P -> Vk.Mm.M.TypeBits -> Vk.Mm.PropertyFlags ->
	IO Vk.Mm.M.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.Phd.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> find ((&&)
		<$> (`Vk.Mm.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mm.M.mTypePropertyFlags . snd) tps
		where tps = Vk.Phd.memoryPropertiesMemoryTypes props1

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a . Storable' a =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 a ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List 256 a ""] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CBffr.allocate
		dvc allocInfo \((cb :: Vk.CBffr.C s) :*. HPList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
				Vk.submitInfoCommandBuffers = HPList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HPList.Nil }
		Vk.CBffr.begin @'Nothing @'Nothing cb beginInfo do
			Vk.Cmd.copyBuffer @'[ '( '[Obj.List 256 a ""], 0, 0)] cb src dst
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

createSyncObjs :: forall n sd a . HPList.RepM n =>
	Vk.Dvc.D sd -> (forall ssos . SyncObjs ssos -> IO a) -> IO a
createSyncObjs dv f =
	HPList.repM @n (Vk.Semaphore.create @'Nothing dv def nil) \iass ->
	HPList.repM @n (Vk.Semaphore.create @'Nothing dv def nil) \rfss ->
	HPList.repM @n (Vk.Fence.create @'Nothing dv finfo nil) \iffs ->
	f $ SyncObjs iass rfss iffs
	where
	finfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

data SyncObjs (ssos :: ([Type], [Type], [Type])) where
	SyncObjs :: {
		_imageAvailableSemaphores :: HPList.PL Vk.Semaphore.S siass,
		_renderFinishedSemaphores :: HPList.PL Vk.Semaphore.S srfss,
		_inFlightFences :: HPList.PL Vk.Fence.F sfss } ->
		SyncObjs '(siass, srfss, sfss)

type SyncObjects = SyncObjs

recordCommandBuffer :: forall scb sr sf sl sg sm sb nm sm' sb' nm' sdsl sds alu .
	Vk.CBffr.C scb ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl alu], '[]) ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List 256 Word32 ""] ->
	Vk.DscSet.D sds (AtomUbo sdsl alu) ->
	IO ()
recordCommandBuffer cb rp fb sce ppllyt gpl vb ib ubds =
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->

	Vk.Cmd.bindVertexBuffers cbb
		(HPList.Singleton . U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @"" vb) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word32 @"" ib) >>

	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics ppllyt
		(U2 ubds :** HPList.Nil)
		(	(HPList.Nil :** HPList.Nil :** HPList.Nil) :** HPList.Nil ) >>

	let Obj.LengthList ln = Obj.lengthOf @(Obj.List 256 Word32 "") $ Vk.Bffr.lengthBinded ib in
	Vk.Cmd.drawIndexed cbb (fromIntegral ln) 1 0 0 0

	where
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
			Vk.ClearValueColor (fromJust $ rgbaDouble 0 0 0 1) :**
			Vk.ClearValueDepthStencil (Vk.ClearDepthStencilValue 1 0) :**
			HPList.Nil }

mainLoop :: (
	KnownNat alu,
	UpdateTexture alu slyts (AtomUbo sdsc alu),
	BObj.IsImage tximg,
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers ss sfs,
	HPList.HomoList (AtomUbo sdsc alu) slyts,
	HPList.HomoList '() vss ) =>
	TChan () -> TChan tximg ->
	FramebufferResized ->
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.Q.Q ->
	Vk.CmdPl.C sc ->
	Vk.Khr.Swpch.S scfmt ssc -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) ss ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsc alu] '[] -> Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsc alu], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	ColorResources clrnm scfmt clrsi clrsm clrsiv ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdiv ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List 256 Word32 ""] ->

	HPList.PL (MemoryUbo alu "uniform-buffer") smsbs ->
	HPList.PL (Vk.DscSet.D sds) slyts ->

	HPList.LL (Vk.CBffr.C scb) vss ->
	SyncObjects siassrfssfs ->

	Vk.Img.M.Binded sm2 si3 "texture" (BObj.ImageFormat tximg) ->
	Vk.Mm.M.M sm2 '[ '(si3,
		'Vk.Mm.M.ImageArg "texture" (BObj.ImageFormat tximg))] ->
	Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv2 -> Vk.Smplr.M.S ss2 ->
	UTCTime ->
	IO ()
mainLoop lb tctximg g w sfc phdvc qfis dvc gq pq cp sc ext0 scivs rp ppllyt gpl fbs
	crsrcs drsrcs vb ib
	ums dscss
	cbs iasrfsifs
	tx txmem txiv txsmplr tm0 = do
	lbst <- atomically $ newTVar Glfw.MouseButtonState'Released
	($ cycle' (NE.fromList [0 .. maxFramesInFlight - 1])) . ($ ext0) $ fix \loop ext (Inf.Inf cf cfs) -> do
		Glfw.pollEvents
		tm <- getCurrentTime
		runLoop lbst lb LeaveFrontFaceCounterClockwise w sfc phdvc qfis dvc gq pq
			sc g ext scivs rp ppllyt gpl fbs cp tctximg crsrcs drsrcs vb ib cbs iasrfsifs
			ums dscss
			(realToFrac $ tm `diffUTCTime` tm0)
			cf tx txmem txiv txsmplr (`loop` cfs)
	Vk.Dvc.waitIdle dvc

runLoop :: forall
	tximg scfmt dptfmt sfs slyts vss ssfc sd ssc sis sr sl sg sdi sdm
	sdiv sm sb nm sm' sb' nm' sdsc sc clrnm clrsm clrsi clrsiv scb
	siassrfssfs
	smsbs sds ss2 siv2 sm2 si3 sw alu .
	(
	KnownNat alu,
	BObj.IsImage tximg,
	UpdateTexture alu slyts (AtomUbo sdsc alu),
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs,
	HPList.HomoList (AtomUbo sdsc alu) slyts,
	HPList.HomoList '() vss) =>
	TVar Glfw.MouseButtonState -> TChan () ->
	Culling -> GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	PhDvc.QFamIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc -> FramebufferResized -> Vk.Extent2d ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsc alu] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsc alu], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPl.C sc ->
	TChan tximg ->
	ColorResources clrnm scfmt clrsi clrsm clrsiv ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdiv ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List 256 Word32 ""] ->
	HPList.LL (Vk.CBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	HPList.PL (MemoryUbo alu "uniform-buffer") smsbs ->
	HPList.PL (Vk.DscSet.D sds) slyts ->
	Float ->
	Int ->
	Vk.Img.M.Binded sm2 si3 "texture" (BObj.ImageFormat tximg) ->
	Vk.Mm.M.M sm2 '[ '(si3,
		'Vk.Mm.M.ImageArg "texture" (BObj.ImageFormat tximg))] ->
	Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv2 -> Vk.Smplr.M.S ss2 ->
	(Vk.Extent2d -> IO ()) -> IO ()
runLoop lbst lb cll w@(GlfwG.Win.W win) sfc phdvc qfis dvc gq pq sc frszd ext
	scivs rp ppllyt gpl fbs cp tctximg crsrcs drsrcs vb ib cbs iasrfsifs
	ums dscss
	tm cf tx txmem txiv txsmplr loop = do
	mtximg <- atomically (do
		b <- isEmptyTChan tctximg
		case b of
			True -> pure Nothing
			False -> Just <$> readTChan tctximg)
	case mtximg of
		Just tximg -> recreateTexture phdvc dvc gq cp tximg tx txmem txiv
				(updateTexture @alu @slyts @(AtomUbo sdsc alu) dvc dscss txiv txsmplr) >> do
--			(txvw :: Vk.ImgVw.I "texture" txfmt siv)
--			(_txsmplr :: Vk.Smplr.S ssmp) -> do
--				updateTexture @slyts @(AtomUbo sdsc 256) dvc dscss txiv txsmplr
				catchAndRecreate cll w sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp crsrcs drsrcs loop
					$ drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib cbs iasrfsifs
						ums dscss tm cf
				cls <- Glfw.windowShouldClose win
				mouseButtonDown lbst win Glfw.MouseButton'1 >>= \case
					True -> atomically $ writeTChan lb ()
					_ -> pure ()
				if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
					(loop =<< recreateAll cll
						w sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp crsrcs drsrcs)
		Nothing -> do
			catchAndRecreate cll w sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp crsrcs drsrcs loop
				$ drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib cbs iasrfsifs
					ums dscss
					tm cf
			cls <- Glfw.windowShouldClose win
			mouseButtonDown lbst win Glfw.MouseButton'1 >>= \case
				True -> atomically $ writeTChan lb ()
				_ -> pure ()
			if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
				(loop =<< recreateAll cll
					w sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp crsrcs drsrcs)

mouseButtonDown ::
	TVar Glfw.MouseButtonState -> Glfw.Window -> Glfw.MouseButton -> IO Bool
mouseButtonDown st w b = do
	now <- Glfw.getMouseButton w b
	pre <- atomically $ readTVar st <* writeTVar st now
	case (pre, now) of
		(	Glfw.MouseButtonState'Released,
			Glfw.MouseButtonState'Pressed ) -> pure True
		_ -> pure False

drawFrame :: forall sfs sd ssc scfmt sr sl sdsc sg sm sb nm sm' sb' nm' scb ssos vss smsbs
		slyts sds alu . (
	KnownNat alu,
	HPList.HomoList (AtomUbo sdsc alu) slyts,
	HPList.HomoList '() vss) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Vk.Khr.Swpch.S scfmt ssc ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsc alu] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsc alu], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List 256 Word32 ""] ->
	HPList.LL (Vk.CBffr.C scb) vss -> SyncObjects ssos ->
	HPList.PL (MemoryUbo alu "uniform-buffer") smsbs ->
	HPList.PL (Vk.DscSet.D sds) slyts ->
	Float ->
	Int -> IO ()
drawFrame dvc gq pq sc ext rp ppllyt gpl fbs vb ib cbs
	(SyncObjs iass rfss iffs) ums dscss tm cf =
	HPList.index iass cf \(ias :: Vk.Semaphore.S sias) ->
	HPList.index rfss cf \(rfs :: Vk.Semaphore.S srfs) ->
	HPList.index iffs cf \(id &&& HPList.Singleton -> (iff, siff)) ->
	HPList.index ums cf \um ->
	($ HPList.homoListIndex dscss cf) \dscs -> do
	Vk.Fence.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CBffr.reset cb def
	HPList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext ppllyt gpl vb ib dscs
	updateUniformBuffer dvc um ext tm
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers = HPList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
		presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphores = HPList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = HPList.Singleton
				$ Vk.Khr.SwapchainImageIndex sc imgIdx }
	Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfo
	where	HPList.Dummy cb = cbs `HPList.homoListIndex` cf ::
			HPList.Dummy (Vk.CBffr.C scb) '()

updateUniformBuffer :: forall sd alu sm . KnownNat alu =>
	Vk.Dvc.D sd -> MemoryUbo alu "uniform-buffer" sm -> Vk.Extent2d -> Float -> IO ()
updateUniformBuffer dvc (MemoryModelViewProj um) sce tm =
	Vk.Mm.write @"uniform-buffer" @(Obj.Atom alu WModelViewProj 'Nothing) @0 dvc um zeroBits ubo
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
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs ) => Culling ->
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsc alu] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsc alu], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPl.C sc ->
	ColorResources clrnm scfmt clrsi clrsm clrsiv ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdiv ->
	(Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate cll w sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp crsrcs drsrcs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateAll cll
		w sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp crsrcs drsrcs

recreateAll :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs ) => Culling ->
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> PhDvc.QFamIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q ->
	Vk.Khr.Swpch.S scfmt ssc ->
	HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsc alu] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsc alu], '[]) ->
	HPList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPl.C sc ->
	ColorResources clnm scfmt clrsi clrsm clrsiv ->
	DptRsrcs sdi sdm "depth-buffer" dptfmt sdiv ->
	IO Vk.Extent2d
recreateAll cll w@(GlfwG.Win.W win) sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp
	(clrimg, clrimgm, clrimgvw, mss) (dptImg, dptImgMem, dptImgVw) = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwpch w sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swpch.getImages dvc sc >>= \imgs ->
			recreateImageViews dvc imgs scivs
		recreateClrRsrcs phdvc dvc ext (clrimg, clrimgm, clrimgvw, mss)
		recreateDepthResources phdvc dvc gq cp ext mss dptImg dptImgMem dptImgVw
		recreateGraphicsPipeline cll dvc ext rp ppllyt mss gpl
		recreateFramebuffers dvc ext rp scivs clrimgvw dptImgVw fbs

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

instance Storable UniformBufferObject where
	sizeOf = GStorable.gSizeOf
	alignment = GStorable.gAlignment
	peek = GStorable.gPeek
	poke = GStorable.gPoke

instance GStorable.G UniformBufferObject

shaderModuleCreateInfo :: SpirV.S sknd -> Vk.ShaderModule.CreateInfo 'Nothing sknd
shaderModuleCreateInfo code = Vk.ShaderModule.CreateInfo {
	Vk.ShaderModule.createInfoNext = TMaybe.N,
	Vk.ShaderModule.createInfoFlags = def,
	Vk.ShaderModule.createInfoCode = code }

cycle' :: NE.NonEmpty a -> Inf.Infinite a
cycle' (x NE.:| xs) = x `Inf.Inf` cycle' (xs `snoc` x)

snoc :: [a] -> a -> NE.NonEmpty a
snoc [] x = x NE.:| []
snoc (x : xs) y = x NE.:| (xs ++ [y])

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

layout(location = 0) out vec4 outColor;

layout(binding = 1) uniform sampler2D texSampler;

void
main()
{
//	outColor = texture(texSampler, fragTexCoord * 2.0);
	outColor = vec4(fragColor * texture(texSampler, fragTexCoord).rgb, 1.0);
}

|]
