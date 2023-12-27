{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import qualified Gpu.Vulkan.Memory as Vk.Mem

import GHC.Generics
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
import Data.TypeLevel.List qualified as TLength
import Data.TypeLevel.List qualified as TpLvlLst
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Proxy
import Data.Bool
import Data.Maybe
import Data.List qualified as L
import Data.IORef
import Data.List.Length
import Data.Word
import Data.Int
import Data.Color
import Data.Time
import Codec.Picture

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Storable as V
import qualified Graphics.UI.GLFW as Glfw hiding (createWindowSurface)
import qualified Gpu.Vulkan.Cglm as Cglm
import qualified Foreign.Storable.Generic as GStorable

import ThEnv
import qualified Language.SpirV as SpirV
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH


import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.TypeEnum as Vk.T
import qualified Gpu.Vulkan.Exception as Vk
import qualified Gpu.Vulkan.Instance as Vk.Ist
import qualified Gpu.Vulkan.Khr as Vk.Khr
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.QueueFamily as Vk.QueueFamily

import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.Device as Vk.Dvc.M
import qualified Gpu.Vulkan.Device as Device.M
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Surface
import qualified Gpu.Vulkan.Khr.Surface as Vk.Khr.Surface.M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice as
	Vk.Khr.Surface.PhysicalDevice
import qualified Gpu.Vulkan.Khr.Swapchain as Vk.Khr.Swapchain
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
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Layout
import qualified Gpu.Vulkan.Attachment as Vk.Att
import qualified Gpu.Vulkan.Subpass as Vk.Subpass
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPass
import qualified Gpu.Vulkan.RenderPass as Vk.RndrPass.M
import qualified Gpu.Vulkan.Pipeline.Graphics as Vk.Ppl.Graphics
import qualified Gpu.Vulkan.Framebuffer as Vk.Frmbffr
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPool
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBffr
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBffr.M
import qualified Gpu.Vulkan.Semaphore as Vk.Semaphore
import qualified Gpu.Vulkan.Fence as Vk.Fence
import qualified Gpu.Vulkan.VertexInput as Vk.VtxInp
import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Mem.M
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet

import qualified Gpu.Vulkan.Sampler as Vk.Smplr
import qualified Gpu.Vulkan.Sampler as Vk.Smplr.M
import qualified Gpu.Vulkan.PhysicalDevice.Struct as Vk.PhDvc
import qualified Gpu.Vulkan.Pipeline.DepthStencilState as Vk.Ppl.DptStnSt

import Tools
import Vertex
import Vertex.Wavefront

import Graphics.SimplePolygon.DebugMessenger qualified as DbgMsngr
import Graphics.SimplePolygon.Instance qualified as Ist
import Graphics.SimplePolygon.Window qualified as Win
import Graphics.SimplePolygon.Surface qualified as Sfc
import Graphics.SimplePolygon.PhysicalDevice qualified as PhDvc

import Options.Declarative hiding (run)
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM

type WVertex = GStorable.W Vertex
type FramebufferResized = IORef Bool

windowName :: String
windowName = "TRY MULTISAMPLING"

windowSize :: (Int, Int)
windowSize = (width, height) where width = 800; height = 600

enableValidationLayers :: Bool
enableValidationLayers = maybe True (const False) $(lookupCompileEnv "NDEBUG")

maxFramesInFlight :: Integral n => n
maxFramesInFlight = 2

main :: IO ()
main = run_ command

command ::
	Flag "t" '["texture"] "FILEPATH" "texture image file path" [FilePath] ->
	Flag "m" '["model"] "FILEPATH" "model file path" [FilePath] ->
	Cmd "Try multisampling" ()
command txfp mdfp = liftIO $
	atomically newTChan >>= \lb ->
	Win.create windowSize windowName \w ->
	Ist.create enableValidationLayers \inst ->
	Sfc.create inst w \sfc ->
	pickPhysicalDevice inst sfc >>= \pd ->
	(MyImage <$>) <$> readRgba8 `mapM` (get txfp) >>= \tximgs@(tximg : _) ->
	atomically newTChan >>= \tctximg -> forkIO (for_ (tail $ cycle tximgs) \ti -> do
		_ <- atomically $ readTChan lb
		putStrLn "Left Button Down"
		atomically $ writeTChan tctximg ti) >>
	loadModel `mapM` get mdfp >>= \mdls@(mdl0 : mdl1 : _) ->
	displayTex enableValidationLayers lb LeaveFrontFaceCounterClockwise
		w inst sfc pd tximg tctximg mdl0 mdl1

displayTex :: BObj.IsImage img => Bool -> TChan () -> Culling ->
	Win.W -> Vk.Ist.I si -> Sfc.S ss -> PhDvc.P ->
	img -> TChan img -> Model -> Model -> IO ()
displayTex vld lb cll (Win.W w g) inst sfc pd img tctximg mdl0 mdl1 =
	bool id (DbgMsngr.setup inst) vld $ run vld lb cll img tctximg mdl0 mdl1 0 w g sfc pd

type Model = (V.Vector WVertex, V.Vector Word32)

run :: BObj.IsImage tximg => Bool -> TChan () -> Culling -> tximg -> TChan tximg ->
	Model -> Model -> Float ->
	Glfw.Window -> FramebufferResized -> Sfc.S ss -> PhDvc.P -> IO ()
run vld lb cll tximg tctximg mdl0 mdl1 mnld w g sfc (PhDvc.P phdv qfis) =
	getMaxUsableSampleCount phdv >>= \mss ->
	createLogicalDevice vld phdv qfis \dv gq pq ->
	createSwapChain w sfc phdv qfis dv
		\(sc :: Vk.Khr.Swapchain.S scifmt ss) ext ->
	Vk.Khr.Swapchain.getImages dv sc >>= \imgs ->
	createImageViews dv imgs \scivs ->
	findDepthFormat phdv >>= \dptfmt ->
	Vk.T.formatToType dptfmt \(_ :: Proxy dptfmt) ->
	createColorResources @scifmt phdv dv ext mss \clrimg clrimgm clrimgvw ->
	createRenderPass @scifmt @dptfmt dv mss \rp ->
	createPipelineLayout' dv \dscslyt dscslytNew ppllyt ->
	createGraphicsPipeline cll dv ext rp ppllyt mss \gpl ->
	createCommandPool qfis dv \cp ->
	createDepthResources phdv dv gq cp ext mss \dptImg dptImgMem dptImgVw ->
	createFramebuffers dv ext rp scivs clrimgvw dptImgVw \fbs ->

	createTexture phdv dv gq cp tximg mnld \
		tx
		txmem
		(txvw :: Vk.ImgVw.I "texture" txfmt siv)
		(txsmplr :: Vk.Smplr.S ssmp) ->

	createDescriptorPool dv \dscp ->
	createDescriptorPool dv \dscpNew ->
	createUniformBuffers @ssmp @siv phdv dv dscslyt maxFramesInFlight \dscslyts ubs ums ->
	createUniformBuffersNew @ssmp @siv phdv dv dscslytNew maxFramesInFlight \dscslytsNew ubsNew umsNew ->

	createDescriptorSets @_ @_ @ssmp @siv dv dscp ubs dscslyts txvw txsmplr \dscss ->
	createDescriptorSetsNew @_ @_ @ssmp @siv dv dscpNew ubsNew dscslytsNew txvw txsmplr \dscssNew ->

	createCommandBuffers dv cp \cbs ->
	createSyncObjects dv \sos ->
	getCurrentTime >>= \tm ->

	createModel phdv dv gq cp mdl0 \vbib ->

	mainLoop lb cll g w sfc phdv qfis dv gq pq sc ext scivs rp ppllyt gpl fbs cp
		tctximg
		(clrimg, clrimgm, clrimgvw, mss)
		(dptImg, dptImgMem, dptImgVw) (snd mdl0) vbib cbs sos ubs ums dscss ubsNew umsNew dscssNew tm tx txmem txvw txsmplr

createTexture :: forall slyts ds cs img sd sc a . BObj.IsImage img =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> img -> Float -> (
		forall sm si siv ss .
		Vk.Img.M.Binded sm si "texture" (BObj.ImageFormat img) ->
		Vk.Mem.M.M sm '[ '(si,
			'Vk.Mem.M.ImageArg "texture" (BObj.ImageFormat img))] ->
		Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv  ->
		Vk.Smplr.S ss -> IO a ) -> IO a
createTexture phdv dv gq cp tximg mnld f =
	createTextureImage phdv dv gq cp tximg \tx txmem mplvs ->
	createImageView @'Vk.T.FormatR8g8b8a8Srgb dv tx Vk.Img.AspectColorBit mplvs
		\(txvw :: Vk.ImgVw.I "texture" txfmt siv) ->
	createTextureSampler phdv dv mplvs mnld \(txsmplr :: Vk.Smplr.S ssmp) ->
	f @_ @_ @siv @ssmp tx txmem txvw txsmplr

recreateTexture :: forall slyts ds cs img sd sc siv si3 sm2 a . BObj.IsImage img =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> img ->
	Vk.Img.M.Binded sm2 si3 "texture" (BObj.ImageFormat img) ->
	Vk.Mem.M.M sm2 '[ '(si3,
		'Vk.Mem.M.ImageArg "texture" (BObj.ImageFormat img))] ->
	Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv -> Float -> IO a -> IO ()
recreateTexture phdv dv gq cp tximg tx txmem txiv mnld act =
	recreateTextureImage phdv dv gq cp tximg tx txmem \mplvs ->
	recreateImageView' @'Vk.T.FormatR8g8b8a8Srgb dv tx Vk.Img.AspectColorBit txiv mplvs act -- mplvs

-- createModels ::
--	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C scp -> [Model] ->

createModel ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C scp -> Model ->
	(forall sm sb sm1 sb1 . (
		Vk.Bffr.Binded
			sm sb "vertex-buffer" '[Obj.List 256 WVertex ""],
		Vk.Bffr.Binded
			sm1 sb1 "index-buffer" '[Obj.List 256 Word32 ""] ) ->
		IO a) -> IO a
createModel phdv dv gq cp (vtcs, idcs) f =
	createVertexBuffer phdv dv gq cp vtcs \vb ->
	createIndexBuffer phdv dv gq cp idcs \ib -> f (vb, ib)

pickPhysicalDevice :: Vk.Ist.I si -> Vk.Khr.Surface.S ss -> IO PhDvc.P
pickPhysicalDevice ist sfc = PhDvc.enumerate ist sfc >>= \case
	[] -> error "failed to find a suitable GPU"
	pd : _ -> pure pd

getMaxUsableSampleCount :: Vk.PhDvc.P -> IO Vk.Sample.CountFlags
getMaxUsableSampleCount phdvc = do
	cnts <- Vk.PhDvc.limitsFramebufferDepthSampleCounts . Vk.PhDvc.propertiesLimits
		<$> Vk.PhDvc.getProperties phdvc
	print cnts
	pure . fromMaybe Vk.Sample.Count1Bit $ findBit [
		Vk.Sample.Count64Bit, Vk.Sample.Count32Bit,
		Vk.Sample.Count16Bit, Vk.Sample.Count8Bit,
		Vk.Sample.Count4Bit, Vk.Sample.Count2Bit ] cnts

findBit :: Bits b => [b] -> b -> Maybe b
findBit bl bs = find ((/= zeroBits) . (.&. bs)) bl

deviceExtensions :: [Vk.PhDvc.ExtensionName]
deviceExtensions = [Vk.Khr.Swapchain.extensionName]

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.Khr.Surface.M.Capabilities,
	formats :: [Vk.Khr.Surface.M.Format],
	presentModes :: [Vk.Khr.PresentMode] }

querySwapChainSupport ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO SwapChainSupportDetails
querySwapChainSupport dvc sfc = SwapChainSupportDetails
	<$> Vk.Khr.Surface.PhysicalDevice.getCapabilities dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getFormats dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getPresentModes dvc sfc

createLogicalDevice :: Bool -> Vk.PhDvc.P -> PhDvc.QueueFamilyIndices -> (forall sd .
		Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> IO a) -> IO a
createLogicalDevice vld phdvc qfis f =
	mkHeteroParList queueCreateInfos uniqueQueueFamilies \qs ->
	let	createInfo = Vk.Dvc.M.CreateInfo {
			Vk.Dvc.M.createInfoNext = TMaybe.N,
			Vk.Dvc.M.createInfoFlags = def,
			Vk.Dvc.M.createInfoQueueCreateInfos = qs,
			Vk.Dvc.M.createInfoEnabledLayerNames =
				bool [] validationLayers vld,
			Vk.Dvc.M.createInfoEnabledExtensionNames =
				deviceExtensions,
			Vk.Dvc.M.createInfoEnabledFeatures = Just def {
				Vk.PhDvc.featuresSamplerAnisotropy = True } } in
	Vk.Dvc.create @'Nothing phdvc createInfo nil \dvc -> do
		gq <- Vk.Dvc.getQueue dvc (PhDvc.graphicsFamily qfis) 0
		pq <- Vk.Dvc.getQueue dvc (PhDvc.presentFamily qfis) 0
		f dvc gq pq
	where
	uniqueQueueFamilies = L.nub [PhDvc.graphicsFamily qfis, PhDvc.presentFamily qfis]
	queueCreateInfos qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }

validationLayers :: [Vk.LayerName]
validationLayers = [Vk.layerKhronosValidation]

mkHeteroParList :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] ->
	(forall ss . HeteroParList.ToListWithCM' WithPoked TMaybe.M ss => HeteroParList.PL t ss -> b) -> b
mkHeteroParList _k [] f = f HeteroParList.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

createSwapChain :: Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	PhDvc.QueueFamilyIndices -> Vk.Dvc.D sd ->
	(forall ss scfmt . Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swapchain.S scfmt ss -> Vk.Extent2d -> IO a) ->
	IO a
createSwapChain win sfc phdvc qfis dvc f = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	fmt = Vk.Khr.Surface.M.formatFormat
			. chooseSwapSurfaceFormat $ formats spp
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		let	crInfo = mkSwapchainCreateInfo sfc qfis spp ext
		Vk.Khr.Swapchain.create @'Nothing @fmt dvc crInfo nil
			\sc -> f sc ext

recreateSwapChain :: Vk.T.FormatToValue scfmt =>
	Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	PhDvc.QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Khr.Swapchain.S scfmt ssc ->
	IO Vk.Extent2d
recreateSwapChain win sfc phdvc qfis0 dvc sc = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	crInfo = mkSwapchainCreateInfo sfc qfis0 spp ext
	ext <$ Vk.Khr.Swapchain.unsafeRecreate @'Nothing dvc crInfo nil sc

mkSwapchainCreateInfo :: Vk.Khr.Surface.S ss -> PhDvc.QueueFamilyIndices ->
	SwapChainSupportDetails -> Vk.Extent2d ->
	Vk.Khr.Swapchain.CreateInfo 'Nothing ss fmt
mkSwapchainCreateInfo sfc qfis0 spp ext =
	Vk.Khr.Swapchain.CreateInfo {
		Vk.Khr.Swapchain.createInfoNext = TMaybe.N,
		Vk.Khr.Swapchain.createInfoFlags = def,
		Vk.Khr.Swapchain.createInfoSurface = sfc,
		Vk.Khr.Swapchain.createInfoMinImageCount = imgc,
		Vk.Khr.Swapchain.createInfoImageColorSpace =
			Vk.Khr.Surface.M.formatColorSpace fmt,
		Vk.Khr.Swapchain.createInfoImageExtent = ext,
		Vk.Khr.Swapchain.createInfoImageArrayLayers = 1,
		Vk.Khr.Swapchain.createInfoImageUsage =
			Vk.Img.UsageColorAttachmentBit,
		Vk.Khr.Swapchain.createInfoImageSharingMode = ism,
		Vk.Khr.Swapchain.createInfoQueueFamilyIndices = qfis,
		Vk.Khr.Swapchain.createInfoPreTransform =
			Vk.Khr.Surface.M.capabilitiesCurrentTransform caps,
		Vk.Khr.Swapchain.createInfoCompositeAlpha =
			Vk.Khr.CompositeAlphaOpaqueBit,
		Vk.Khr.Swapchain.createInfoPresentMode = presentMode,
		Vk.Khr.Swapchain.createInfoClipped = True,
		Vk.Khr.Swapchain.createInfoOldSwapchain = Nothing }
	where
	fmt = chooseSwapSurfaceFormat $ formats spp
	presentMode = chooseSwapPresentMode $ presentModes spp
	caps = capabilities spp
	maxImgc = fromMaybe maxBound . onlyIf (> 0)
		$ Vk.Khr.Surface.M.capabilitiesMaxImageCount caps
	imgc = clamp
		(Vk.Khr.Surface.M.capabilitiesMinImageCount caps + 1) 0 maxImgc
	(ism, qfis) = bool
		(Vk.SharingModeConcurrent,
			[PhDvc.graphicsFamily qfis0, PhDvc.presentFamily qfis0])
		(Vk.SharingModeExclusive, [])
		(PhDvc.graphicsFamily qfis0 == PhDvc.presentFamily qfis0)

chooseSwapSurfaceFormat  :: [Vk.Khr.Surface.M.Format] -> Vk.Khr.Surface.M.Format
chooseSwapSurfaceFormat = \case
	availableFormats@(af0 : _) -> fromMaybe af0
		$ find preferredSwapSurfaceFormat availableFormats
	_ -> error "no available swap surface formats"

preferredSwapSurfaceFormat :: Vk.Khr.Surface.M.Format -> Bool
preferredSwapSurfaceFormat f =
	Vk.Khr.Surface.M.formatFormat f == Vk.FormatB8g8r8a8Srgb &&
	Vk.Khr.Surface.M.formatColorSpace f == Vk.Khr.ColorSpaceSrgbNonlinear

chooseSwapPresentMode :: [Vk.Khr.PresentMode] -> Vk.Khr.PresentMode
chooseSwapPresentMode =
	fromMaybe Vk.Khr.PresentModeFifo . find (== Vk.Khr.PresentModeMailbox)

chooseSwapExtent :: Glfw.Window -> Vk.Khr.Surface.M.Capabilities -> IO Vk.Extent2d
chooseSwapExtent win caps
	| Vk.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> w, fromIntegral -> h) <-
			Glfw.getFramebufferSize win
		pure $ Vk.Extent2d
			(clamp w (Vk.extent2dWidth n) (Vk.extent2dHeight n))
			(clamp h (Vk.extent2dWidth x) (Vk.extent2dHeight x))
	where
	curExt = Vk.Khr.Surface.M.capabilitiesCurrentExtent caps
	n = Vk.Khr.Surface.M.capabilitiesMinImageExtent caps
	x = Vk.Khr.Surface.M.capabilitiesMaxImageExtent caps

createImageViews :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Img.Binded ss ss nm fmt] ->
	(forall si . HeteroParList.PL (Vk.ImgVw.I nm fmt) si -> IO a) -> IO a
createImageViews _dvc [] f = f HeteroParList.Nil
createImageViews dvc (sci : scis) f =
	createImageView dvc sci Vk.Img.AspectColorBit 1 \sciv ->
	createImageViews dvc scis \scivs -> f $ sciv :** scivs

recreateImageViews :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss nm scfmt] -> HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis -> IO ()
recreateImageViews _dvc [] HeteroParList.Nil = pure ()
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

createRenderPass ::
	forall (scifmt :: Vk.T.Format) (dptfmt :: Vk.T.Format) sd a . (
	Vk.T.FormatToValue scifmt, Vk.T.FormatToValue dptfmt ) =>
	Vk.Dvc.D sd -> Vk.Sample.CountFlags ->
	(forall sr . Vk.RndrPass.R sr -> IO a) -> IO a
createRenderPass dvc mss f = do
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
				colorAttachmentResolve :** HeteroParList.Nil,
			Vk.RndrPass.M.createInfoSubpasses = [subpass],
			Vk.RndrPass.M.createInfoDependencies = [dependency] }
	Vk.RndrPass.create @'Nothing @'[scifmt, dptfmt, scifmt] dvc renderPassInfo nil \rp -> f rp

type AtomUbo s = '(s, '[
	'Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing],
	'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ])

type AtomModel s = '(s, '[
	'Vk.DscSetLyt.Buffer '[Obj.Atom 256 WModelObject 'Nothing],
	'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ])

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscSetLyt.D s '[
		'Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing],
		'Vk.DscSetLyt.Image
			'[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ] -> IO a) ->
	IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
		'Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings =
			uboLayoutBinding :**
			samplerLayoutBinding :** HeteroParList.Nil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing])
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

createDescriptorSetLayout1 :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscSetLyt.D s '[
		'Vk.DscSetLyt.Buffer '[Obj.Atom 256 WModelObject 'Nothing],
		'Vk.DscSetLyt.Image
			'[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ] -> IO a) ->
	IO a
createDescriptorSetLayout1 dvc = Vk.DscSetLyt.create dvc layoutInfo nil
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
		'Vk.DscSetLyt.Buffer '[Obj.Atom 256 WModelObject 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings =
			uboLayoutBinding :**
			samplerLayoutBinding :** HeteroParList.Nil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[Obj.Atom 256 WModelObject 'Nothing])
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

createPipelineLayout' ::
	Vk.Dvc.D sd -> (forall sdsl sdsl' sl .
		Vk.DscSetLyt.D sdsl '[
			'Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing],
			'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ] ->
		Vk.DscSetLyt.D sdsl' '[
			'Vk.DscSetLyt.Buffer '[Obj.Atom 256 WModelObject 'Nothing],
			'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)] ] ->
		Vk.Ppl.Layout.P sl '[AtomUbo sdsl, AtomModel sdsl'] '[] -> IO b) -> IO b
createPipelineLayout' dvc f =
	createDescriptorSetLayout dvc \dsl ->
	createDescriptorSetLayout1 dvc \dsl1 ->
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = TMaybe.N,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts =
				U2 dsl :** U2 dsl1 :** HeteroParList.Nil } in
	Vk.Ppl.Layout.create @'Nothing @_ @_ @'[] dvc pipelineLayoutInfo nil $ f dsl dsl1

createGraphicsPipeline :: Culling -> Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl, AtomModel sdsl'] '[] ->
	Vk.Sample.CountFlags ->
	(forall sg . Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl, AtomModel sdsl'], '[]) -> IO a) -> IO a
createGraphicsPipeline cll dvc sce rp ppllyt mss f =
	Vk.Ppl.Graphics.createGs dvc Nothing (U14 pplInfo :** HeteroParList.Nil)
			nil \(U3 gpl :** HeteroParList.Nil) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo cll sce rp ppllyt mss

recreateGraphicsPipeline :: Culling -> Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl, AtomModel sdsl'] '[] ->
	Vk.Sample.CountFlags ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl, AtomModel sdsl'], '[]) -> IO ()
recreateGraphicsPipeline cll dvc sce rp ppllyt mss gpls = Vk.Ppl.Graphics.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HeteroParList.Nil) nil (U3 gpls :** HeteroParList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo cll sce rp ppllyt mss

mkGraphicsPipelineCreateInfo :: Culling ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl, AtomModel sdsl'] '[] ->
	Vk.Sample.CountFlags ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing, '[ '(WVertex, 'Vk.VtxInp.RateVertex)],
			'[ '(0, Pos), '(1, Color), '(2, TexCoord)] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'Nothing '(sl, '[AtomUbo sdsl, AtomModel sdsl'], '[]) sr '(sb, vs', ts', sbtss')
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

shaderStages :: HeteroParList.PL (U5 Vk.Ppl.ShdrSt.CreateInfo) '[
	'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
	'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
shaderStages = U5 vertShaderStageInfo :** U5 fragShaderStageInfo :** HeteroParList.Nil
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

createFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
	Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.I nm fmt) sis ->
	Vk.ImgVw.I clrnm fmt slrsiv ->
	Vk.ImgVw.I dptnm dptfmt siv ->
	(forall sfs . RecreateFramebuffers sis sfs =>
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFramebuffers _ _ _ HeteroParList.Nil _ _ f = f HeteroParList.Nil
createFramebuffers dvc sce rp (iv :** ivs) clriv dptiv f =
	Vk.Frmbffr.create dvc (mkFramebufferCreateInfo sce rp iv clriv dptiv) nil \fb ->
	createFramebuffers dvc sce rp ivs clriv dptiv \fbs -> f (fb :** fbs)

class RecreateFramebuffers (sis :: [Type]) (sfs :: [Type]) where
	recreateFramebuffers :: Vk.Dvc.D sd -> Vk.Extent2d ->
		Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
		Vk.ImgVw.I clrnm scfmt clrsdiv ->
		Vk.ImgVw.I dptfmt dptnm sdiv ->
		HeteroParList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFramebuffers '[] '[] where
	recreateFramebuffers _dvc _sce _rp HeteroParList.Nil _ _ HeteroParList.Nil = pure ()

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
	Vk.Frmbffr.createInfoAttachments = U3 clr :** U3 dpt :** U3 attch :** HeteroParList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where
	Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = sce

createCommandPool :: PhDvc.QueueFamilyIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPool.C sc -> IO a) -> IO a
createCommandPool qfis dvc f =
	Vk.CmdPool.create dvc poolInfo nil \cp -> f cp
	where poolInfo = Vk.CmdPool.CreateInfo {
		Vk.CmdPool.createInfoNext = TMaybe.N,
		Vk.CmdPool.createInfoFlags =
			Vk.CmdPool.CreateResetCommandBufferBit,
		Vk.CmdPool.createInfoQueueFamilyIndex = PhDvc.graphicsFamily qfis }

type ColorResources nm fmt si sm siv = (
	Vk.Img.Binded sm si nm fmt,
	Vk.Mem.M sm
		'[ '(si, 'Vk.Mem.ImageArg nm fmt)],
	Vk.ImgVw.I nm fmt siv,
	Vk.Sample.CountFlags )

createColorResources :: forall fmt sd nm a . Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Extent2d -> Vk.Sample.CountFlags ->
	(forall si sm siv .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.ImageArg nm fmt)] ->
		Vk.ImgVw.I nm fmt siv ->
		IO a) -> IO a
createColorResources phdvc dvc ext msmpls f =
	createImage @_ @fmt phdvc dvc
		(Vk.extent2dWidth ext) (Vk.extent2dHeight ext) 1 msmpls
		Vk.Img.TilingOptimal
		(	Vk.Img.UsageTransientAttachmentBit .|.
			Vk.Img.UsageColorAttachmentBit )
		Vk.Mem.PropertyDeviceLocalBit \img imgmem ->
	createImageView @fmt dvc img Vk.Img.AspectColorBit 1 \imgvw ->
	f img imgmem imgvw

recreateColorResources :: forall fmt sd nm si sm siv . Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Extent2d -> Vk.Sample.CountFlags ->
	Vk.Img.Binded sm si nm fmt ->
	Vk.Mem.M sm
		'[ '(si, 'Vk.Mem.ImageArg nm fmt)] ->
	Vk.ImgVw.I nm fmt siv -> IO ()
recreateColorResources phdvc dvc ext msmpls img imgmem imgvw = do
	recreateImage phdvc dvc
		(Vk.extent2dWidth ext) (Vk.extent2dHeight ext) 1 msmpls
		Vk.Img.TilingOptimal
		(	Vk.Img.UsageTransientAttachmentBit .|.
			Vk.Img.UsageColorAttachmentBit )
		Vk.Mem.PropertyDeviceLocalBit img imgmem
	recreateImageView dvc img Vk.Img.AspectColorBit imgvw 1

createDepthResources ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Extent2d -> Vk.Sample.CountFlags ->
	(forall si sm fmt siv . Vk.T.FormatToValue fmt =>
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.ImageArg nm fmt) ] ->
		Vk.ImgVw.I nm fmt siv ->
		IO a) -> IO a
createDepthResources phdvc dvc gq cp ext mss f = do
	fmt <- findDepthFormat phdvc
	print fmt
	print ext
	Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
		createImage @_ @fmt phdvc dvc
			(Vk.extent2dWidth ext) (Vk.extent2dHeight ext) 1 mss
			Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
			Vk.Mem.PropertyDeviceLocalBit \dptImg dptImgMem ->
			createImageView @fmt
				dvc dptImg Vk.Img.AspectDepthBit 1 \dptImgVw -> do
			transitionImageLayout dvc gq cp dptImg Vk.Img.LayoutUndefined
				Vk.Img.LayoutDepthStencilAttachmentOptimal 1
			f dptImg dptImgMem dptImgVw

recreateDepthResources :: Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Extent2d -> Vk.Sample.CountFlags ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.ImageArg nm fmt)] ->
	Vk.ImgVw.I nm fmt sdiv -> IO ()
recreateDepthResources phdvc dvc gq cp ext mss dptImg dptImgMem dptImgVw = do
	print ext
	recreateImage phdvc dvc
		(Vk.extent2dWidth ext) (Vk.extent2dHeight ext) 1 mss
		Vk.Img.TilingOptimal Vk.Img.UsageDepthStencilAttachmentBit
		Vk.Mem.PropertyDeviceLocalBit dptImg dptImgMem
	recreateImageView dvc dptImg Vk.Img.AspectDepthBit dptImgVw 1
	transitionImageLayout dvc gq cp dptImg Vk.Img.LayoutUndefined
		Vk.Img.LayoutDepthStencilAttachmentOptimal 1

type DepthResources sb sm nm fmt sdiv = (
	Vk.Img.Binded sm sb nm fmt,
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.ImageArg nm fmt)],
	Vk.ImgVw.I nm fmt sdiv )

findDepthFormat :: Vk.PhDvc.P -> IO Vk.Format
findDepthFormat phdvc = findSupportedFormat phdvc
	[Vk.FormatD32Sfloat, Vk.FormatD32SfloatS8Uint, Vk.FormatD24UnormS8Uint]
	Vk.Img.TilingOptimal
	Vk.FormatFeatureDepthStencilAttachmentBit

findSupportedFormat ::
	Vk.PhDvc.P -> [Vk.Format] -> Vk.Img.Tiling -> Vk.FormatFeatureFlags -> IO Vk.Format
findSupportedFormat phdvc fs tlng fffs = do
	props <- Vk.PhDvc.getFormatProperties phdvc `mapM` fs
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

createTextureImage :: forall img sd sc nm a . BObj.IsImage img =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> img -> (
		forall si sm .
		Vk.Img.Binded sm si nm (BObj.ImageFormat img) ->
		Vk.Mem.M.M sm '[ '(si, 'Vk.Mem.M.ImageArg nm (BObj.ImageFormat img))] ->
		Word32 ->
		IO a ) -> IO a
createTextureImage phdvc dvc gq cp img f = do
	let	wdt_ = BObj.imageWidth img
		hgt_ = BObj.imageHeight img
		wdt, hgt :: Num i => i
		wdt = fromIntegral wdt_
		hgt = fromIntegral hgt_
		mipLevels :: Word32 = floor @Double . logBase 2 $ max wdt hgt
	createImage @_ @(BObj.ImageFormat img)
		phdvc dvc wdt hgt mipLevels Vk.Sample.Count1Bit Vk.Img.TilingOptimal
		(	Vk.Img.UsageTransferSrcBit .|.
			Vk.Img.UsageTransferDstBit .|.
			Vk.Img.UsageSampledBit)
		Vk.Mem.PropertyDeviceLocalBit \tximg txmem -> do
		createBufferImage @img @_ phdvc dvc
			(wdt, wdt, hgt, 1)
			Vk.Bffr.UsageTransferSrcBit
			(	Vk.Mem.PropertyHostVisibleBit .|.
				Vk.Mem.PropertyHostCoherentBit )
			\(sb :: Vk.Bffr.Binded
				sm sb "texture-buffer" '[ Obj.Image 1 img inm]) sbm -> do
			Vk.Mem.write @"texture-buffer"
				@(Obj.Image 1 img inm) dvc sbm zeroBits img
			print sb
			transitionImageLayout dvc gq cp tximg
				Vk.Img.LayoutUndefined
				Vk.Img.LayoutTransferDstOptimal mipLevels
			copyBufferToImage dvc gq cp sb tximg wdt hgt
			generateMipmaps phdvc dvc gq cp tximg mipLevels wdt hgt
			f tximg txmem mipLevels

recreateTextureImage :: forall img sd sc nm a sm si . BObj.IsImage img =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> img ->
		Vk.Img.Binded sm si nm (BObj.ImageFormat img) ->
		Vk.Mem.M.M sm '[ '(si, 'Vk.Mem.M.ImageArg nm (BObj.ImageFormat img))] -> (Word32 -> IO a) -> IO ()
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
			Vk.Img.UsageSampledBit) Vk.Mem.PropertyDeviceLocalBit tx txmem do
		createBufferImage @img @_ phdvc dvc
			(wdt, wdt, hgt, 1)
			Vk.Bffr.UsageTransferSrcBit
			(	Vk.Mem.PropertyHostVisibleBit .|.
				Vk.Mem.PropertyHostCoherentBit )
			\(sb :: Vk.Bffr.Binded
				sm2 sb "texture-buffer" '[ Obj.Image 1 img inm]) sbm -> do
			Vk.Mem.write @"texture-buffer"
				@(Obj.Image 1 img inm) dvc sbm zeroBits img
			print sb
			transitionImageLayout dvc gq cp tx
				Vk.Img.LayoutUndefined
				Vk.Img.LayoutTransferDstOptimal mipLevels
			copyBufferToImage dvc gq cp sb tx wdt hgt
			generateMipmaps phdvc dvc gq cp tx mipLevels wdt hgt
		act mipLevels

newtype MyImage = MyImage (Image PixelRGBA8)

instance BObj.IsImage MyImage where
	type ImagePixel MyImage = MyRgba8
	type ImageFormat MyImage = 'Vk.T.FormatR8g8b8a8Srgb
	imageRow = BObj.imageWidth
	imageWidth (MyImage img) = fromIntegral $ imageWidth img
	imageHeight (MyImage img) = fromIntegral $ imageHeight img
	imageDepth _ = 1
	imageBody (MyImage img) = (<$> [0 .. imageHeight img - 1]) \y ->
		(<$> [0 .. imageWidth img - 1]) \x -> MyRgba8 $ pixelAt img x y
	imageMake w h _d pss = MyImage $ generateImage
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
		where
		listToTuple4 :: [a] -> (a, a, a, a)
		listToTuple4 = \case
			[r, g, b, a] -> (r, g, b, a)
			_ -> error "The length of the list is not 4"
	poke p (MyRgba8 (PixelRGBA8 r g b a)) =
		pokeArray (castPtr p) [r, g, b, a]

checkImageFilterLinearBit ::
	forall fmt . Vk.T.FormatToValue fmt => Vk.PhDvc.P -> IO ()
checkImageFilterLinearBit phdvc = do
	let	fmt = Vk.T.formatToValue @fmt
	formatProperties <- Vk.PhDvc.getFormatProperties phdvc fmt
	let	bt = Vk.formatPropertiesOptimalTilingFeatures
				formatProperties .&.
			Vk.FormatFeatureSampledImageFilterLinearBit
	when (bt == zeroBits) $ error
		"texture image format does not support linear blitting!"

generateMipmaps :: forall sd scp si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C scp ->
	Vk.Img.Binded sm si nm fmt -> Word32 -> Int32 -> Int32 -> IO ()
generateMipmaps phdvc dvc gq cp img mlvs wdt hgt = do
	putStrLn "GENERATE MIPMAPS"
	checkImageFilterLinearBit @fmt phdvc
	beginSingleTimeCommands dvc gq cp \cb -> do
		for_ iwhs \(i, (w, h)) -> generateMipmap1 cb img i w h
		Vk.Cmd.pipelineBarrier cb
			Vk.Ppl.StageTransferBit Vk.Ppl.StageFragmentShaderBit zeroBits
			HeteroParList.Nil HeteroParList.Nil (HeteroParList.Singleton $ U5 barrier)
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
	Vk.Img.memoryBarrierSrcQueueFamilyIndex = Vk.QueueFamily.Ignored,
	Vk.Img.memoryBarrierDstQueueFamilyIndex = Vk.QueueFamily.Ignored,
	Vk.Img.memoryBarrierImage = img,
	Vk.Img.memoryBarrierSubresourceRange = srr }
	where
	srr = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = i - 1,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 }

generateMipmap1 :: forall scb si sm nm fmt . Vk.CmdBffr.C scb ->
	Vk.Img.Binded sm si nm fmt -> Word32 -> Int32 -> Int32 -> IO ()
generateMipmap1 cb img i w h = do
	Vk.Cmd.pipelineBarrier cb
		Vk.Ppl.StageTransferBit Vk.Ppl.StageTransferBit zeroBits
		HeteroParList.Nil HeteroParList.Nil . HeteroParList.Singleton $ U5 barrier'
	Vk.Cmd.blitImage cb
		img Vk.Img.LayoutTransferSrcOptimal
		img Vk.Img.LayoutTransferDstOptimal
		[blit] Vk.FilterLinear
	Vk.Cmd.pipelineBarrier cb
		Vk.Ppl.StageTransferBit Vk.Ppl.StageFragmentShaderBit zeroBits
		HeteroParList.Nil HeteroParList.Nil . HeteroParList.Singleton $ U5 barrier''
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
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
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
		cb bf img Vk.Img.LayoutTransferDstOptimal (HeteroParList.Singleton region)

transitionImageLayout :: forall sd sc si sm nm fmt . Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
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
				Vk.QueueFamily.Ignored,
			Vk.Img.memoryBarrierDstQueueFamilyIndex =
				Vk.QueueFamily.Ignored,
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
		sstg dstg zeroBits HeteroParList.Nil HeteroParList.Nil (HeteroParList.Singleton $ U5 barrier)
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
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	(forall s . Vk.CmdBffr.C s -> IO a) -> IO a
beginSingleTimeCommands dvc gq cp cmd = do
	Vk.CmdBffr.allocate
		dvc allocInfo \((cb :: Vk.CmdBffr.C s) :*. HeteroParList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
				Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
		Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo (cmd cb) <* do
			Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) Nothing
			Vk.Queue.waitIdle gq
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

createBufferImage :: Storable (BObj.ImagePixel t) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> (Device.M.Size, Device.M.Size, Device.M.Size, Device.M.Size) ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[ Obj.Image 1 t inm] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[ Obj.Image 1 t inm])] ->
		IO a) -> IO a
createBufferImage p dv (r, w, h, d) usg props =
	createBuffer' p dv (Obj.LengthImage r w h d) usg props

createBufferAtom :: forall sd nm a b . Storable a => Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Obj.Atom 256 a 'Nothing] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[Obj.Atom 256 a 'Nothing] )] ->
			IO b) -> IO b
createBufferAtom p dv usg props = createBuffer' p dv Obj.LengthAtom usg props

createBufferList :: forall sd nm t a . Storable t =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Device.M.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Obj.List 256 t ""] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[Obj.List 256 t ""] ) ] ->
		IO a) ->
	IO a
createBufferList p dv ln usg props =
	createBuffer' p dv (Obj.LengthList ln) usg props

createBuffer' :: forall sd nm o a . Obj.SizeAlignment o =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mem.M sm
			'[ '(sb, 'Vk.Mem.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer' p dv ln usg props f = Vk.Bffr.create dv bffrInfo nil \b -> do
	reqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMemoryType p (Vk.Mem.M.requirementsMemoryTypeBits reqs) props
	Vk.Mem.allocateBind dv (HeteroParList.Singleton . U2 $ Vk.Mem.Buffer b)
		(allcInfo mt) nil
		$ f . \(HeteroParList.Singleton (U2 (Vk.Mem.BufferBinded bnd))) -> bnd
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HeteroParList.Singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.M.TypeIndex -> Vk.Mem.AllocateInfo 'Nothing
	allcInfo mt = Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

createImage :: forall nm fmt sd a . Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags -> Vk.Img.Tiling ->
	Vk.Img.UsageFlagBits -> Vk.Mem.PropertyFlagBits -> (forall si sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.ImageArg nm fmt) ] ->
		IO a) -> IO a
createImage pd dvc wdt hgt mplvs mss tlng usg prps f = Vk.Img.create @'Nothing dvc
		(imageInfo wdt hgt mplvs mss tlng usg) nil \img -> do
	memInfo <- imageMemoryInfo pd dvc prps img
	imageAllocateBind dvc img memInfo f

recreateImage :: Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mem.PropertyFlags ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.ImageArg nm fmt)] -> IO ()
recreateImage pd dvc wdt hgt mplvs mss tlng usg prps img mem = do
	Vk.Img.unsafeRecreate @'Nothing dvc
		(imageInfo wdt hgt mplvs mss tlng usg) nil img
	memInfo <- imageMemoryInfoBinded pd dvc prps img
	imageReallocateBind dvc img memInfo mem

recreateImage' :: Vk.T.FormatToValue fmt =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Word32 -> Word32 -> Word32 -> Vk.Sample.CountFlags -> Vk.Img.Tiling ->
	Vk.Img.UsageFlags -> Vk.Mem.PropertyFlags ->
	Vk.Img.Binded sm sb nm fmt ->
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.ImageArg nm fmt)] -> IO a -> IO ()
recreateImage' pd dvc wdt hgt mplvs mss tlng usg prps img mem act = do
	Vk.Img.unsafeRecreate' @'Nothing dvc
		(imageInfo wdt hgt mplvs mss tlng usg) nil img do
		memInfo <- imageMemoryInfoBinded pd dvc prps img
		imageReallocateBind' dvc img memInfo mem act

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

imageAllocateBind :: Vk.Dvc.D sd -> Vk.Img.I si nm fmt ->
	Vk.Mem.AllocateInfo 'Nothing -> (forall sm .
		Vk.Img.Binded sm si nm fmt ->
		Vk.Mem.M sm
			'[ '(si, 'Vk.Mem.ImageArg nm fmt) ] ->
		IO a) -> IO a
imageAllocateBind dvc img memInfo f =
	Vk.Mem.allocateBind @'Nothing dvc
		(HeteroParList.Singleton . U2 $ Vk.Mem.Image img) memInfo
		nil \(HeteroParList.Singleton (U2 (Vk.Mem.ImageBinded bnd))) m -> do
		f bnd m

imageReallocateBind ::
	Vk.Dvc.D sd -> Vk.Img.Binded sm sb nm fmt ->
	Vk.Mem.AllocateInfo 'Nothing ->
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.ImageArg nm fmt)] -> IO ()
imageReallocateBind dvc img memInfo m =
	Vk.Mem.unsafeReallocateBind @'Nothing dvc
		(HeteroParList.Singleton . U2 $ Vk.Mem.ImageBinded img) memInfo
		nil m

imageReallocateBind' ::
	Vk.Dvc.D sd -> Vk.Img.Binded sm sb nm fmt ->
	Vk.Mem.AllocateInfo 'Nothing ->
	Vk.Mem.M
		sm '[ '(sb, 'Vk.Mem.ImageArg nm fmt)] -> IO a -> IO ()
imageReallocateBind' dvc img memInfo m =
	Vk.Mem.unsafeReallocateBind' @'Nothing dvc
		(HeteroParList.Singleton . U2 $ Vk.Mem.ImageBinded img) memInfo
		nil m

imageMemoryInfo ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Mem.PropertyFlags ->
	Vk.Img.I s nm fmt -> IO (Vk.Mem.AllocateInfo 'Nothing)
imageMemoryInfo pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirements dvc img
	mt <- findMemoryType pd (Vk.Mem.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

imageMemoryInfoBinded ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Mem.PropertyFlags ->
	Vk.Img.Binded sm si nm fmt -> IO (Vk.Mem.AllocateInfo 'Nothing)
imageMemoryInfoBinded pd dvc prps img = do
	reqs <- Vk.Img.getMemoryRequirementsBinded dvc img
	mt <- findMemoryType pd (Vk.Mem.M.requirementsMemoryTypeBits reqs) prps
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

createTextureSampler ::
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Word32 -> Float ->
	(forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTextureSampler phdv dvc mplvs mnld f = do
	prp <- Vk.PhDvc.getProperties phdv
	print . Vk.PhDvc.limitsMaxSamplerAnisotropy $ Vk.PhDvc.propertiesLimits prp
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
				Vk.PhDvc.limitsMaxSamplerAnisotropy
					$ Vk.PhDvc.propertiesLimits prp,
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
	(vtcs, idcs) <- verticesIndices fp
	let	(vtcs', idcs') = indexingVector vtcs
	putStrLn "LOAD MODEL"
	putStrLn $ "vtcs : " ++ show (V.length (vtcs :: V.Vector WVertex))
	putStrLn $ "vtcs': " ++ show (V.length (vtcs' :: V.Vector WVertex))
	putStrLn $ "idcs : " ++ show (V.length (idcs :: V.Vector Word32))
	putStrLn $ "idcs': " ++ show (V.length (idcs':: V.Vector Word32))
	pure (vtcs', idcs')

createVertexBuffer :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> V.Vector WVertex -> (forall sm sb .
		Vk.Bffr.Binded sm sb "vertex-buffer" '[Obj.List 256 WVertex ""] -> IO a) -> IO a
createVertexBuffer phdvc dvc gq cp vtcs f =
	createBufferList phdvc dvc (fromIntegral $ V.length vtcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList phdvc dvc (fromIntegral $ V.length vtcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
		\(b' :: Vk.Bffr.Binded sm sb "vertex-buffer" '[Obj.List 256 t ""])
			(bm' :: Vk.Mem.M sm '[ '(
				sb,
				'Vk.Mem.BufferArg
					"vertex-buffer" '[Obj.List 256 WVertex ""] )]) -> do
	Vk.Mem.write @"vertex-buffer" @(Obj.List 256 WVertex "") dvc bm' zeroBits vtcs
	copyBuffer dvc gq cp b' b
	f b

createIndexBuffer :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> V.Vector Word32 -> (forall sm sb .
		Vk.Bffr.Binded sm sb "index-buffer" '[Obj.List 256 Word32 ""] -> IO a) -> IO a
createIndexBuffer phdvc dvc gq cp idcs f =
	createBufferList phdvc dvc (fromIntegral $ V.length idcs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageIndexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ ->
	createBufferList phdvc dvc (fromIntegral $ V.length idcs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
		\(b' :: Vk.Bffr.Binded sm sb "index-buffer" '[Obj.List 256 t ""])
			(bm' :: Vk.Mem.M sm '[ '(
				sb,
				'Vk.Mem.BufferArg "index-buffer"
					'[Obj.List 256 Word32 ""] )]) -> do
	Vk.Mem.write @"index-buffer" @(Obj.List 256 Word32 "") dvc bm' zeroBits idcs
	copyBuffer dvc gq cp b' b
	f b

createUniformBuffers :: forall ssmp siv sd sdsc a .
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.DscSetLyt.D sdsc '[
		'Vk.DscSetLyt.Buffer '[Obj.Atom 256 UniformBufferObject 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)]] ->
	Int -> (forall slyts smsbs . (
		Vk.DscSet.DListFromMiddle slyts,
		HeteroParList.FromList slyts,
		Update smsbs slyts ssmp siv,
		UpdateTexture slyts (AtomUbo sdsc),
		HeteroParList.HomoList (AtomUbo sdsc) slyts
		) =>
		HeteroParList.PL (U2 Vk.DscSetLyt.D) slyts ->
		HeteroParList.PL BindedUbo smsbs ->
		HeteroParList.PL MemoryUbo smsbs -> IO a) -> IO a
createUniformBuffers _ _ _ 0 f = f HeteroParList.Nil HeteroParList.Nil HeteroParList.Nil
createUniformBuffers ph dvc dscslyt n f =
	createUniformBuffer1 ph dvc \(b :: BindedUbo smsb) m ->
		createUniformBuffers @ssmp @siv ph dvc dscslyt (n - 1) \ls (bs :: HeteroParList.PL BindedUbo smsbs) ms -> f
			(U2 dscslyt :** ls)
			(b :** bs :: HeteroParList.PL BindedUbo (smsb ': smsbs))
			(m :** ms)

createUniformBuffersNew :: forall ssmp siv sd sdsc a .
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.DscSetLyt.D sdsc '[
		'Vk.DscSetLyt.Buffer '[Obj.Atom 256 WModelObject 'Nothing],
		'Vk.DscSetLyt.Image '[ '("texture", 'Vk.T.FormatR8g8b8a8Srgb)]] ->
	Int -> (forall slyts smsbs . (
		Vk.DscSet.DListFromMiddle slyts,
		HeteroParList.FromList slyts,
		UpdateNew smsbs slyts ssmp siv,
--		UpdateTextureNew slyts (AtomModel sdsc),
		HeteroParList.HomoList (AtomModel sdsc) slyts
		) =>
		HeteroParList.PL (U2 Vk.DscSetLyt.D) slyts ->
		HeteroParList.PL BindedUboNew smsbs ->
		HeteroParList.PL MemoryUboNew smsbs -> IO a) -> IO a
createUniformBuffersNew _ _ _ 0 f = f HeteroParList.Nil HeteroParList.Nil HeteroParList.Nil
createUniformBuffersNew ph dvc dscslyt n f =
	createUniformBuffer1New ph dvc \(b :: BindedUboNew smsb) m ->
		createUniformBuffersNew @ssmp @siv ph dvc dscslyt (n - 1) \ls (bs :: HeteroParList.PL BindedUboNew smsbs) ms -> f
			(U2 dscslyt :** ls)
			(b :** bs :: HeteroParList.PL BindedUboNew (smsb ': smsbs))
			(m :** ms)

type family MapFst abs where
	MapFst '[] = '[]
	MapFst ( '(a, b, c :: k) ': abs) = a ': MapFst abs

data BindedUbo smsb where
	BindedUbo :: Vk.Bffr.Binded sm sb "uniform-buffer" '[Obj.Atom 256 UniformBufferObject 'Nothing] ->
		BindedUbo '(sm, sb)

data MemoryUbo smsb where
	MemoryUbo :: Vk.Mem.M sm '[ '(
		sb,
		'Vk.Mem.BufferArg "uniform-buffer"
			'[Obj.Atom 256 UniformBufferObject 'Nothing] )] ->
		MemoryUbo '(sm, sb)

data BindedUboNew smsb where
	BindedUboNew :: Vk.Bffr.Binded sm sb "uniform-buffer" '[Obj.Atom 256 WModelObject 'Nothing] ->
		BindedUboNew '(sm, sb)

data MemoryUboNew smsb where
	MemoryUboNew :: Vk.Mem.M sm '[ '(
		sb,
		'Vk.Mem.BufferArg "uniform-buffer"
			'[Obj.Atom 256 WModelObject 'Nothing] )] ->
		MemoryUboNew '(sm, sb)

createUniformBuffer1 :: Vk.PhDvc.P -> Vk.Dvc.D sd -> (forall sm sb .
		BindedUbo '(sm, sb) -> MemoryUbo '(sm, sb) -> IO b) -> IO b
createUniformBuffer1 phdvc dvc f = createBufferAtom phdvc dvc
		Vk.Bffr.UsageUniformBufferBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit ) \b m ->
	f (BindedUbo b) (MemoryUbo m)

createUniformBuffer1New :: Vk.PhDvc.P -> Vk.Dvc.D sd -> (forall sm sb .
		BindedUboNew '(sm, sb) -> MemoryUboNew '(sm, sb) -> IO b) -> IO b
createUniformBuffer1New phdvc dvc f = createBufferAtom phdvc dvc
		Vk.Bffr.UsageUniformBufferBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit ) \b m ->
	f (BindedUboNew b) (MemoryUboNew m)

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDescriptorPool dvc = Vk.DscPool.create dvc poolInfo nil
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = TMaybe.N,
		Vk.DscPool.createInfoFlags =
			Vk.DscPool.CreateFreeDescriptorSetBit,
		Vk.DscPool.createInfoMaxSets = maxFramesInFlight,
		Vk.DscPool.createInfoPoolSizes = [poolSize0, poolSize1] }
	poolSize0 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = maxFramesInFlight }
	poolSize1 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscPool.sizeDescriptorCount = maxFramesInFlight }

createDescriptorSets :: (
	Vk.DscSet.DListFromMiddle ss,
	Update smsbs ss ssmp siv ) =>
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> HeteroParList.PL BindedUbo smsbs ->
	HeteroParList.PL (U2 Vk.DscSetLyt.D) ss ->
	Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv -> Vk.Smplr.S ssmp ->
	(forall sds . HeteroParList.PL (Vk.DscSet.D sds) ss -> IO a) -> IO a
createDescriptorSets dvc dscp ubs dscslyts tximgvw txsmp f =
	Vk.DscSet.allocateDs dvc allocInfo \dscss -> do
	update dvc ubs dscss tximgvw txsmp
	f dscss
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = dscslyts }

createDescriptorSetsNew :: (
	Vk.DscSet.DListFromMiddle ss,
	UpdateNew smsbs ss ssmp siv ) =>
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> HeteroParList.PL BindedUboNew smsbs ->
	HeteroParList.PL (U2 Vk.DscSetLyt.D) ss ->
	Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv -> Vk.Smplr.S ssmp ->
	(forall sds . HeteroParList.PL (Vk.DscSet.D sds) ss -> IO a) -> IO a
createDescriptorSetsNew dvc dscp ubs dscslyts tximgvw txsmp f =
	Vk.DscSet.allocateDs dvc allocInfo \dscss -> do
	updateNew dvc ubs dscss tximgvw txsmp
	f dscss
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts = dscslyts }

descriptorWrite0 ::
	Vk.Bffr.Binded sm sb nm '[Obj.Atom 256 UniformBufferObject 'Nothing] ->
	Vk.DscSet.D sds slbts ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, Obj.Atom 256 UniformBufferObject 'Nothing )]) 0
descriptorWrite0 ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HeteroParList.Singleton bufferInfo
	}
	where bufferInfo = U4 $ Vk.Dsc.BufferInfo ub

descriptorWrite0New ::
	Vk.Bffr.Binded sm sb nm '[Obj.Atom 256 WModelObject 'Nothing] ->
	Vk.DscSet.D sds slbts ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, Obj.Atom 256 WModelObject 'Nothing )]) 0
descriptorWrite0New ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HeteroParList.Singleton bufferInfo
	}
	where bufferInfo = U4 $ Vk.Dsc.BufferInfo ub

descriptorWrite1 ::
	Vk.DscSet.D sds slbts -> Vk.ImgVw.I nm fmt si -> Vk.Smplr.S ss ->
	Vk.DscSet.Write 'Nothing sds slbts
		('Vk.DscSet.WriteSourcesArgImage '[ '(ss, nm, fmt, si) ]) 0
descriptorWrite1 dscs tiv tsmp = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeCombinedImageSampler,
	Vk.DscSet.writeSources = Vk.DscSet.ImageInfos . HeteroParList.Singleton
		$ U4 Vk.Dsc.ImageInfo {
			Vk.Dsc.imageInfoImageLayout =
				Vk.Img.LayoutShaderReadOnlyOptimal,
			Vk.Dsc.imageInfoImageView = tiv,
			Vk.Dsc.imageInfoSampler = tsmp } }

class Update smsbs slbtss ssmp siv where
	update ::
		Vk.Dvc.D sd ->
		HeteroParList.PL BindedUbo smsbs ->
		HeteroParList.PL (Vk.DscSet.D sds) slbtss ->
		Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv ->
		Vk.Smplr.S ssmp ->
		IO ()

instance Update '[] '[] ssmp siv where update _ HeteroParList.Nil HeteroParList.Nil _ _ = pure ()

instance (
	Vk.DscSet.BindingAndArrayElemBuffer (TIndex.I1_2 '(ds, cs)) '[Obj.Atom 256 UniformBufferObject 'Nothing] 0,
	Vk.DscSet.UpdateDynamicLength (TIndex.I1_2 '(ds, cs)) '[Obj.Atom 256 UniformBufferObject 'Nothing],
	Update ubs dscss ssmp siv,
	Vk.DscSet.WriteSourcesToMiddle cs ('Vk.DscSet.WriteSourcesArgImage
			'[ '(ssmp, "texture", 'Vk.T.FormatR8g8b8a8Srgb, siv)]) 0
	) =>
	Update (ub ': ubs) ('(ds, cs) ': dscss) ssmp siv where
	update dvc (BindedUbo ub :** ubs) (dscs :** dscss) tximgvw txsmp = do
		Vk.DscSet.updateDs dvc (
			U5 (descriptorWrite0 ub dscs) :**
			U5 (descriptorWrite1 dscs tximgvw txsmp) :**
			HeteroParList.Nil )
			HeteroParList.Nil
		update dvc ubs dscss tximgvw txsmp

class UpdateTexture slbtss dscs where
	updateTexture :: (
		Vk.DscSet.BindingAndArrayElemBuffer (TIndex.I1_2 dscs) '[Obj.Atom 256 UniformBufferObject 'Nothing] 0,
		Vk.DscSet.UpdateDynamicLength (TIndex.I1_2 dscs) '[Obj.Atom 256 UniformBufferObject 'Nothing],
		Vk.DscSet.WriteSourcesToMiddle (TIndex.I1_2 dscs) ('Vk.DscSet.WriteSourcesArgImage
			'[ '(ssmp, "texture", 'Vk.T.FormatR8g8b8a8Srgb, siv)]) 0,
		Vk.DscSet.WriteListUpdateDynamicLengths
			'[ '( 'Nothing, sds, dscs, 'Vk.DscSet.WriteSourcesArgImage
				'[ '(ssmp, "texture", 'Vk.T.FormatR8g8b8a8Srgb, siv)], 0)]
			) =>
		Vk.Dvc.D sd ->
		HeteroParList.PL (Vk.DscSet.D sds) slbtss ->
		Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv ->
		Vk.Smplr.S ssmp -> IO ()

instance UpdateTexture '[] dscs where updateTexture _ HeteroParList.Nil _ _ = pure ()

instance UpdateTexture dscss dscs =>
	UpdateTexture (dscs ': dscss) dscs where
	updateTexture dvc (dscs :** dscss) tximgvw txsmp = do
		Vk.DscSet.updateDs dvc (
			U5 (descriptorWrite1 dscs tximgvw txsmp) :**
			HeteroParList.Nil )
			HeteroParList.Nil
		updateTexture @dscss @dscs dvc dscss tximgvw txsmp

class UpdateNew smsbs slbtss ssmp siv where
	updateNew ::
		Vk.Dvc.D sd ->
		HeteroParList.PL BindedUboNew smsbs ->
		HeteroParList.PL (Vk.DscSet.D sds) slbtss ->
		Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv ->
		Vk.Smplr.S ssmp ->
		IO ()

instance UpdateNew '[] '[] ssmp siv where updateNew _ HeteroParList.Nil HeteroParList.Nil _ _ = pure ()

instance (
	Vk.DscSet.BindingAndArrayElemBuffer (TIndex.I1_2 '(ds, cs)) '[Obj.Atom 256 WModelObject 'Nothing] 0,
	Vk.DscSet.UpdateDynamicLength (TIndex.I1_2 '(ds, cs)) '[Obj.Atom 256 WModelObject 'Nothing],
	UpdateNew ubs dscss ssmp siv,
	Vk.DscSet.WriteSourcesToMiddle cs ('Vk.DscSet.WriteSourcesArgImage
			'[ '(ssmp, "texture", 'Vk.T.FormatR8g8b8a8Srgb, siv)]) 0
	) =>
	UpdateNew (ub ': ubs) ('(ds, cs) ': dscss) ssmp siv where
	updateNew dvc (BindedUboNew ub :** ubs) (dscs :** dscss) tximgvw txsmp = do
		Vk.DscSet.updateDs dvc (
			U5 (descriptorWrite0New ub dscs) :**
			U5 (descriptorWrite1 dscs tximgvw txsmp) :**
			HeteroParList.Nil )
			HeteroParList.Nil
		updateNew dvc ubs dscss tximgvw txsmp

findMemoryType :: Vk.PhDvc.P -> Vk.Mem.M.TypeBits -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.M.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.PhDvc.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> find ((&&)
		<$> (`Vk.Mem.M.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mem.M.mTypePropertyFlags . snd) tps
		where tps = Vk.PhDvc.memoryPropertiesMemoryTypes props1

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a . Storable' a =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 a ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List 256 a ""] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CmdBffr.allocate
		dvc allocInfo \((cb :: Vk.CmdBffr.C s) :*. HeteroParList.Nil) -> do
		let	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[s] '[]
			submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
				Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
		Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo do
			Vk.Cmd.copyBuffer @'[ '[Obj.List 256 a ""]] cb src dst
		Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) Nothing
		Vk.Queue.waitIdle gq
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
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPool.C scp ->
	(forall scb vss . (TLength.Length vss, HeteroParList.HomoList '() vss) =>
		HeteroParList.LL (Vk.CmdBffr.C scb) (vss :: [()]) -> IO a) ->
	IO a
createCommandBuffers dvc cp f = mkVss maxFramesInFlight \(_p :: Proxy vss1) ->
	Vk.CmdBffr.allocate @_ @vss1 dvc (allocInfo @vss1) (f @_ @vss1)
	where
	allocInfo :: forall vss . Vk.CmdBffr.AllocateInfo 'Nothing scp vss
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }

mkVss :: Int -> (forall (vss :: [()]) .
	(TpLvlLst.Length vss, TLength.Length vss, HeteroParList.FromList vss, HeteroParList.HomoList '() vss) =>
	Proxy vss -> a) -> a
mkVss 0 f = f (Proxy @'[])
mkVss n f = mkVss (n - 1) \p -> f $ addTypeToProxy p
	where
	addTypeToProxy :: Proxy vss -> Proxy ('() ': vss)
	addTypeToProxy Proxy = Proxy

data SyncObjects (ssos :: ([Type], [Type], [Type])) where
	SyncObjects :: {
		_imageAvailableSemaphores :: HeteroParList.PL Vk.Semaphore.S siass,
		_renderFinishedSemaphores :: HeteroParList.PL Vk.Semaphore.S srfss,
		_inFlightFences :: HeteroParList.PL Vk.Fence.F sfss } ->
		SyncObjects '(siass, srfss, sfss)

createSyncObjects ::
	Vk.Dvc.D sd -> (forall ssos . SyncObjects ssos -> IO a ) -> IO a
createSyncObjects dvc f =
	HeteroParList.replicateM maxFramesInFlight
		(Vk.Semaphore.create @'Nothing dvc def nil) \iass ->
	HeteroParList.replicateM maxFramesInFlight
		(Vk.Semaphore.create @'Nothing dvc def nil) \rfss ->
	HeteroParList.replicateM maxFramesInFlight
		(Vk.Fence.create @'Nothing dvc fncInfo nil) \iffs ->
	f $ SyncObjects iass rfss iffs
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb tximg sr sf sl sg sm sb nm sm' sb' nm' sdsl sdsl' sds sdsNew .
	Vk.CmdBffr.C scb ->
	TChan tximg ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl, AtomModel sdsl'] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsl, AtomModel sdsl'], '[]) ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List 256 Word32 ""] ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	Vk.DscSet.D sdsNew (AtomModel sdsl') ->
	IO ()
recordCommandBuffer cb tctximg rp fb sce ppllyt gpl idcs vb ib ubds ubdsNew =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->

	Vk.Cmd.bindVertexBuffers cbb
		(HeteroParList.Singleton . U5 $ Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @"" vb) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word32 @"" ib) >>

	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics ppllyt
		(U2 ubds :** U2 ubdsNew :** HeteroParList.Nil)
		(	(HeteroParList.Nil :** HeteroParList.Nil :** HeteroParList.Nil) :**
			(HeteroParList.Nil :** HeteroParList.Nil :** HeteroParList.Nil) :** HeteroParList.Nil ) >>

	Vk.Cmd.drawIndexed cbb (fromIntegral $ V.length idcs) 1 0 0 0

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
			HeteroParList.Nil }

mainLoop :: (
	UpdateTexture slyts (AtomUbo sdsc),
	BObj.IsImage tximg,
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers ss sfs,
	HeteroParList.HomoList (AtomUbo sdsc) slyts,
	HeteroParList.HomoList (AtomModel sdsc') slytsNew,
	HeteroParList.HomoList '() vss ) =>
	TChan () -> Culling -> FramebufferResized ->
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> PhDvc.QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.S scfmt ssc -> Vk.Extent2d ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) ss ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsc, AtomModel sdsc'] '[] -> Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsc, AtomModel sdsc'], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPool.C sc ->
	TChan tximg ->
	ColorResources clrnm scfmt clrsi clrsm clrsiv ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	V.Vector Word32 ->
	(	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""],
		Vk.Bffr.Binded sm' sb' nm' '[Obj.List 256 Word32 ""] ) ->
	HeteroParList.LL (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	HeteroParList.PL BindedUbo smsbs ->
	HeteroParList.PL MemoryUbo smsbs ->
	HeteroParList.PL (Vk.DscSet.D sds) slyts ->
	HeteroParList.PL BindedUboNew smsbsNew ->
	HeteroParList.PL MemoryUboNew smsbsNew ->
	HeteroParList.PL (Vk.DscSet.D sdsNew) slytsNew ->
	UTCTime ->
	Vk.Img.M.Binded sm2 si3 "texture" (BObj.ImageFormat tximg) ->
	Vk.Mem.M.M sm2 '[ '(si3,
		'Vk.Mem.M.ImageArg "texture" (BObj.ImageFormat tximg))] ->
	Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv2 -> Vk.Smplr.M.S ss2 ->
	IO ()
mainLoop lb cll g w sfc phdvc qfis dvc gq pq sc ext0 scivs rp ppllyt gpl fbs cp
	tctximg crsrcs drsrcs idcs (vb, ib) cbs iasrfsifs ubs ums dscss ubsNew umsNew dscssNew tm0 tx txmem txiv txsmplr = do
	lbst <- atomically $ newTVar Glfw.MouseButtonState'Released
	($ cycle [0 .. maxFramesInFlight - 1]) . ($ ext0) $ fix \loop ext (cf : cfs) -> do
		Glfw.pollEvents
		tm <- getCurrentTime
		runLoop lbst lb cll w sfc phdvc qfis dvc gq pq
			sc g ext scivs rp ppllyt gpl fbs cp tctximg crsrcs drsrcs idcs vb ib cbs iasrfsifs
			ubs ums dscss
			ubsNew umsNew dscssNew
			(realToFrac $ tm `diffUTCTime` tm0)
			cf tx txmem txiv txsmplr (`loop` cfs)
	Vk.Dvc.waitIdle dvc

runLoop :: forall
	tximg scfmt dptfmt sfs slyts slytsNew vss ssfc sd ssc sis sr sl sg sdi sdm
	sdiv sm sb nm sm' sb' nm' sdsc sdsc' sc clrnm clrsm clrsi clrsiv scb
	siassrfssfs
	smsbs smsbsNew sds sdsNew ss2 siv2 sm2 si3 .
	(
	BObj.IsImage tximg,
	UpdateTexture slyts (AtomUbo sdsc),
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs,
	HeteroParList.HomoList (AtomUbo sdsc) slyts,
	HeteroParList.HomoList (AtomModel sdsc') slytsNew,
	HeteroParList.HomoList '() vss) =>
	TVar Glfw.MouseButtonState -> TChan () ->
	Culling -> Glfw.Window -> Vk.Khr.Surface.S ssfc -> Vk.PhDvc.P ->
	PhDvc.QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Khr.Swapchain.S scfmt ssc -> FramebufferResized -> Vk.Extent2d ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsc, AtomModel sdsc'] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsc, AtomModel sdsc'], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPool.C sc ->
	TChan tximg ->
	ColorResources clrnm scfmt clrsi clrsm clrsiv ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List 256 Word32 ""] ->
	HeteroParList.LL (Vk.CmdBffr.C scb) vss ->
	SyncObjects siassrfssfs ->
	HeteroParList.PL BindedUbo smsbs ->
	HeteroParList.PL MemoryUbo smsbs ->
	HeteroParList.PL (Vk.DscSet.D sds) slyts ->
	HeteroParList.PL BindedUboNew smsbsNew ->
	HeteroParList.PL MemoryUboNew smsbsNew ->
	HeteroParList.PL (Vk.DscSet.D sdsNew) slytsNew ->
	Float ->
	Int ->
	Vk.Img.M.Binded sm2 si3 "texture" (BObj.ImageFormat tximg) ->
	Vk.Mem.M.M sm2 '[ '(si3,
		'Vk.Mem.M.ImageArg "texture" (BObj.ImageFormat tximg))] ->
	Vk.ImgVw.I "texture" 'Vk.T.FormatR8g8b8a8Srgb siv2 -> Vk.Smplr.M.S ss2 ->
	(Vk.Extent2d -> IO ()) -> IO ()
runLoop lbst lb cll win sfc phdvc qfis dvc gq pq sc frszd ext
	scivs rp ppllyt gpl fbs cp tctximg crsrcs drsrcs idcs vb ib cbs iasrfsifs
	ubs ums dscss
	ubsNew umsNew dscssNew
	tm cf tx txmem txiv txsmplr loop = do
	mtximg <- atomically (do
		b <- isEmptyTChan tctximg
		case b of
			True -> pure Nothing
			False -> Just <$> readTChan tctximg)
	case mtximg of
		Just tximg -> recreateTexture phdvc dvc gq cp tximg tx txmem txiv 0
				(updateTexture @slyts @(AtomUbo sdsc) dvc dscss txiv txsmplr) >> do
--			(txvw :: Vk.ImgVw.I "texture" txfmt siv)
--			(_txsmplr :: Vk.Smplr.S ssmp) -> do
--				updateTexture @slyts @(AtomUbo sdsc) dvc dscss txiv txsmplr
				catchAndRecreate cll win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp crsrcs drsrcs loop
					$ drawFrame dvc gq pq tctximg sc ext rp ppllyt gpl fbs idcs vb ib cbs iasrfsifs
						ubs ums dscss ubsNew umsNew dscssNew tm cf
				cls <- Glfw.windowShouldClose win
				mouseButtonDown lbst win Glfw.MouseButton'1 >>= \case
					True -> atomically $ writeTChan lb ()
					_ -> pure ()
				if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
					(loop =<< recreateSwapChainEtc cll
						win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp crsrcs drsrcs)
		Nothing -> do
			catchAndRecreate cll win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp crsrcs drsrcs loop
				$ drawFrame dvc gq pq tctximg sc ext rp ppllyt gpl fbs idcs vb ib cbs iasrfsifs
					ubs ums dscss
					ubsNew umsNew dscssNew tm cf
			cls <- Glfw.windowShouldClose win
			mouseButtonDown lbst win Glfw.MouseButton'1 >>= \case
				True -> atomically $ writeTChan lb ()
				_ -> pure ()
			if cls then (pure ()) else checkFlag frszd >>= bool (loop ext)
				(loop =<< recreateSwapChainEtc cll
					win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp crsrcs drsrcs)

mouseButtonDown ::
	TVar Glfw.MouseButtonState -> Glfw.Window -> Glfw.MouseButton -> IO Bool
mouseButtonDown st w b = do
	now <- Glfw.getMouseButton w b
	pre <- atomically $ readTVar st <* writeTVar st now
	case (pre, now) of
		(	Glfw.MouseButtonState'Released,
			Glfw.MouseButtonState'Pressed ) -> pure True
		_ -> pure False

drawFrame :: forall sfs sd tximg ssc scfmt sr sl sdsc sdsc' sg sm sb nm sm' sb' nm' scb ssos vss smsbs smsbsNew
		slyts slytsNew sds sdsNew . (
	HeteroParList.HomoList (AtomUbo sdsc) slyts,
	HeteroParList.HomoList (AtomModel sdsc') slytsNew,
	HeteroParList.HomoList '() vss) =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q -> TChan tximg -> Vk.Khr.Swapchain.S scfmt ssc ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsc, AtomModel sdsc'] '[] ->
	Vk.Ppl.Graphics.G sg '[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsc, AtomModel sdsc'], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	V.Vector Word32 ->
	Vk.Bffr.Binded sm sb nm '[Obj.List 256 WVertex ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[Obj.List 256 Word32 ""] ->
	HeteroParList.LL (Vk.CmdBffr.C scb) vss -> SyncObjects ssos ->
	HeteroParList.PL BindedUbo smsbs ->
	HeteroParList.PL MemoryUbo smsbs ->
	HeteroParList.PL (Vk.DscSet.D sds) slyts ->
	HeteroParList.PL BindedUboNew smsbsNew ->
	HeteroParList.PL MemoryUboNew smsbsNew ->
	HeteroParList.PL (Vk.DscSet.D sdsNew) slytsNew ->
	Float ->
	Int -> IO ()
drawFrame dvc gq pq tctximg sc ext rp ppllyt gpl fbs idcs vb ib cbs
	(SyncObjects iass rfss iffs) ubs ums dscss ubsNew umsNew dscssNew tm cf =
	HeteroParList.index iass cf \(ias :: Vk.Semaphore.S sias) ->
	HeteroParList.index rfss cf \(rfs :: Vk.Semaphore.S srfs) ->
	HeteroParList.index iffs cf \(id &&& HeteroParList.Singleton -> (iff, siff)) ->
	HeteroParList.index ubs cf \ub -> HeteroParList.index ums cf \um ->
	HeteroParList.index ubsNew cf \ubNew -> HeteroParList.index umsNew cf \umNew ->
	($ HeteroParList.homoListIndex dscss cf) \dscs ->
	($ HeteroParList.homoListIndex dscssNew cf) \dscsNew -> do
	Vk.Fence.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HeteroParList.index fbs imgIdx \fb ->
		recordCommandBuffer cb tctximg rp fb ext ppllyt gpl idcs vb ib dscs dscsNew
	updateUniformBuffer dvc um ext tm
	updateUniformBufferNew dvc umNew ext tm
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Singleton
				$ Vk.SemaphorePipelineStageFlags ias
					Vk.Ppl.StageColorAttachmentOutputBit,
			Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HeteroParList.Singleton rfs }
		presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphores = HeteroParList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = HeteroParList.Singleton
				$ Vk.Khr.SwapchainImageIndex sc imgIdx }
	Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfo
	where	HeteroParList.Dummy cb = cbs `HeteroParList.homoListIndex` cf ::
			HeteroParList.Dummy (Vk.CmdBffr.C scb) '()

updateUniformBuffer :: Vk.Dvc.D sd -> MemoryUbo sm -> Vk.Extent2d -> Float -> IO ()
updateUniformBuffer dvc (MemoryUbo um) sce tm =
	Vk.Mem.write @"uniform-buffer" @(Obj.Atom 256 UniformBufferObject 'Nothing) dvc um zeroBits ubo
	where ubo = UniformBufferObject {
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

updateUniformBufferNew :: Vk.Dvc.D sd -> MemoryUboNew sm -> Vk.Extent2d -> Float -> IO ()
updateUniformBufferNew dvc (MemoryUboNew um) sce tm =
	Vk.Mem.write @"uniform-buffer" @(Obj.Atom 256 WModelObject 'Nothing) dvc um zeroBits ubo
	where ubo = GStorable.W ModelObject {
		modelObject = Cglm.rotate
			Cglm.mat4Identity
			(tm * Cglm.rad 90)
			(Cglm.Vec3 $ 0 :. 0 :. 1 :. NilL) }

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs ) => Culling ->
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> PhDvc.QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q ->
	Vk.Khr.Swapchain.S scfmt ssc ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsc, AtomModel sdsc'] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsc, AtomModel sdsc'], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPool.C sc ->
	ColorResources clrnm scfmt clrsi clrsm clrsiv ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	(Vk.Extent2d -> IO ()) -> IO () -> IO ()
catchAndRecreate cll win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp crsrcs drsrcs loop act =
	catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> loop =<< recreateSwapChainEtc cll
		win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp crsrcs drsrcs

recreateSwapChainEtc :: (
	Vk.T.FormatToValue scfmt, Vk.T.FormatToValue dptfmt,
	RecreateFramebuffers sis sfs ) => Culling ->
	Glfw.Window -> Vk.Khr.Surface.S ssfc ->
	Vk.PhDvc.P -> PhDvc.QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q ->
	Vk.Khr.Swapchain.S scfmt ssc ->
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis ->
	Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsc, AtomModel sdsc'] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex)]
		'[ '(0, Pos), '(1, Color), '(2, TexCoord)]
		'(sl, '[AtomUbo sdsc, AtomModel sdsc'], '[]) ->
	HeteroParList.PL Vk.Frmbffr.F sfs ->
	Vk.CmdPool.C sc ->
	ColorResources clnm scfmt clrsi clrsm clrsiv ->
	DepthResources sdi sdm "depth-buffer" dptfmt sdiv ->
	IO Vk.Extent2d
recreateSwapChainEtc cll win sfc phdvc qfis dvc gq sc scivs rp ppllyt gpl fbs cp
	(clrimg, clrimgm, clrimgvw, mss) (dptImg, dptImgMem, dptImgVw) = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwapChain win sfc phdvc qfis dvc sc
	ext <$ do
		Vk.Khr.Swapchain.getImages dvc sc >>= \imgs ->
			recreateImageViews dvc imgs scivs
		recreateColorResources phdvc dvc ext mss clrimg clrimgm clrimgvw
		recreateDepthResources phdvc dvc gq cp ext mss dptImg dptImgMem dptImgVw
		recreateGraphicsPipeline cll dvc ext rp ppllyt mss gpl
		recreateFramebuffers dvc ext rp scivs clrimgvw dptImgVw fbs

waitFramebufferSize :: Glfw.Window -> IO ()
waitFramebufferSize win = Glfw.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		Glfw.waitEvents *> Glfw.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

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

type WModelObject = GStorable.W ModelObject
data ModelObject = ModelObject { modelObject :: Cglm.Mat4 } deriving (Show, Generic)

instance GStorable.G ModelObject

shaderModuleCreateInfo :: SpirV.S sknd -> Vk.ShaderModule.CreateInfo 'Nothing sknd
shaderModuleCreateInfo code = Vk.ShaderModule.CreateInfo {
	Vk.ShaderModule.createInfoNext = TMaybe.N,
	Vk.ShaderModule.createInfoFlags = def,
	Vk.ShaderModule.createInfoCode = code }

[glslVertexShader|

#version 450

layout(binding = 0) uniform UniformBufferObject {
	mat4 model;
	mat4 view;
	mat4 proj;
	} ubo;

layout(set = 1, binding = 0) uniform UniformBufferObject1 {
	mat4 model2;
	} ubo1;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inColor;
layout(location = 2) in vec2 inTexCoord;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void
main()
{
	gl_Position = ubo.proj * ubo.view * ubo1.model2 * vec4(inPosition, 1.0);
//	gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 1.0);
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

layout(set = 1, binding = 1) uniform sampler2D texSampler1;

void
main()
{
//	outColor = texture(texSampler, fragTexCoord * 2.0);
	outColor = vec4(fragColor * texture(texSampler, fragTexCoord).rgb, 1.0);
//	outColor = vec4(fragColor * texture(texSampler1, fragTexCoord).rgb, 1.0);
}

|]
