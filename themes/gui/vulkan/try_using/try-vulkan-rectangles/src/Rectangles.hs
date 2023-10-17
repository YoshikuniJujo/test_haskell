{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Rectangles (

	-- * RECTANGLES

	rectangles,

	-- * COMMAND

	Command(..), Succable(..),

	-- ** VIEW PROJECTION

	ViewProjection(..),

	-- ** RECTANGLE

	Rectangle(..),
	RectPos(..), RectSize(..), RectColor(..),
	RectModel0(..), RectModel1(..), RectModel2(..), RectModel3(..),

	-- * EVENT

	Event(..)

	) where

import GHC.Generics
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.Storable.Generic qualified as StrG
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Foldable
import Data.Proxy
import Data.Default
import Data.Bits
import Data.Maybe
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.List.Length
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Map qualified as M
import Data.Bool
import Data.Word
import Data.Text.IO qualified as Txt
import Data.Color
import Language.SpirV qualified as SpirV
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Enum qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Exception qualified as Vk
import Gpu.Vulkan.Exception.Enum qualified as Vk
import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.AllocationCallbacks qualified as Vk.AllocationCallbacks
import Gpu.Vulkan.Instance.Internal qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.PhDvc
import Gpu.Vulkan.QueueFamily qualified as Vk.QueueFamily
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPool
import Gpu.Vulkan.CommandPool.Enum qualified as Vk.CmdPool
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.CommandBuffer.Enum qualified as Vk.CmdBffr
import Gpu.Vulkan.Queue qualified as Vk.Queue
import Gpu.Vulkan.Queue.Enum qualified as Vk.Queue
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.Descriptor.Enum qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPool
import Gpu.Vulkan.DescriptorPool.Enum qualified as Vk.DscPool
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscSetLyt
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSet
import Gpu.Vulkan.Memory qualified as Vk.Mem
import Gpu.Vulkan.Memory.Enum qualified as Vk.Mem
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Buffer.Enum qualified as Vk.Bffr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.Image.Enum qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.ImageView.Enum qualified as Vk.ImgVw
import Gpu.Vulkan.Semaphore qualified as Vk.Semaphore
import Gpu.Vulkan.Fence qualified as Vk.Fence
import Gpu.Vulkan.Fence.Enum qualified as Vk.Fence
import Gpu.Vulkan.Pipeline.Enum qualified as Vk.Ppl
import Gpu.Vulkan.Pipeline.Graphics qualified as Vk.Ppl.Graphics
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.VertexInputState qualified as Vk.Ppl.VertexInputSt
import Gpu.Vulkan.Pipeline.InputAssemblyState qualified as Vk.Ppl.InpAsmbSt
import Gpu.Vulkan.Pipeline.RasterizationState qualified as Vk.Ppl.RstSt
import Gpu.Vulkan.Pipeline.ColorBlendState qualified as Vk.Ppl.ClrBlndSt
import Gpu.Vulkan.Pipeline.ColorBlendAttachment qualified as Vk.Ppl.ClrBlndAtt
import Gpu.Vulkan.Pipeline.ViewportState qualified as Vk.Ppl.ViewportSt
import Gpu.Vulkan.Pipeline.MultisampleState qualified as Vk.Ppl.MltSmplSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.Ppl.Layout
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderModule
import Gpu.Vulkan.VertexInput qualified as Vk.VtxInp
import Gpu.Vulkan.Framebuffer qualified as Vk.Frmbffr
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPass
import Gpu.Vulkan.Subpass qualified as Vk.Subpass
import Gpu.Vulkan.Subpass.Enum qualified as Vk.Subpass
import Gpu.Vulkan.Attachment qualified as Vk.Att
import Gpu.Vulkan.Attachment.Enum qualified as Vk.Att
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.Sample.Enum qualified as Vk.Sample
import Gpu.Vulkan.Component qualified as Vk.Component
import Gpu.Vulkan.ColorComponent.Enum qualified as Vk.ClrCmp
import Gpu.Vulkan.Misc
import Gpu.Vulkan.Data

import Gpu.Vulkan.Khr qualified as Vk.Khr
import Gpu.Vulkan.Khr.Enum qualified as Vk.Khr
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swapchain
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.Ext.DUtls
import Gpu.Vulkan.Ext.DebugUtils.Enum qualified as Vk.Ext.DUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.Ex.DUtls.Msgr
import Gpu.Vulkan.Cglm qualified as Cglm

import Tools
import ThEnv

import Graphics.UI.GlfwG as GlfwG
import Graphics.UI.GlfwG.Window as GlfwG.Win
import Graphics.UI.GlfwG.Mouse as GlfwG.Ms

rectangles :: forall k . (Ord k, Succable k) =>
	IO ((Command k -> STM (), (STM Bool, STM (Event k))), k -> STM Vk.Extent2d)
rectangles = do
	(inp, outp) <- atomically $ (,) <$> newTChan <*> newTChan
	vext <- atomically $ newTVar M.empty
	_ <- forkIO . GlfwG.init error $ do
		createInstance \ist ->
			Vk.Dvc.group nil' \dvcgrp -> bool id (setupDebugMessenger ist)
				enableValidationLayers do
			(phd', qfis', fmt', dv', gq', pq', n') <-
				withWindow False \dw ->
				createSurface dw ist \dsfc -> do
				(phd, qfis) <- pickPhysicalDevice ist dsfc
				(dv, gq, pq) <-
					createLogicalDevice phd dvcgrp () qfis
				spp <- querySwapChainSupport phd dsfc
				ext <- chooseSwapExtent dw $ capabilities spp
				let	fmt = Vk.Khr.Sfc.formatFormat
						. chooseSwapSurfaceFormat
						$ formats spp
				Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
					n <- getSwapchainImageNum
						@fmt dv dsfc spp ext qfis
					pure (	phd, qfis,
						Vk.Khr.Sfc.formatFormat
							. chooseSwapSurfaceFormat
							$ formats spp, dv, gq, pq, n )
			getNum n' \(_ :: Proxy n) ->
				Vk.T.formatToType fmt' \(_ :: Proxy fmt) ->
					run' @n @fmt @_ @_ @k inp outp vext
						ist phd' qfis' dv' gq' pq'
		atomically $ writeTChan outp EventEnd
	pure (	(writeTChan inp, (isEmptyTChan outp, readTChan outp)),
		readTVarOr (Vk.Extent2d 0 0) vext )
	where setupDebugMessenger ist f =
		Vk.Ex.DUtls.Msgr.create ist debugMessengerCreateInfo nil' f

readTVarOr :: Ord k => a -> TVar (M.Map k (TVar a)) -> k -> STM a
readTVarOr d mp k = do
	mv <- (M.lookup k) <$> readTVar mp
	case mv of
		Nothing -> pure d
		Just v -> readTVar v

waitAndRead :: Ord k => TVar (M.Map k (TVar a)) -> k -> STM a
waitAndRead vmv k = do
	mv <- (M.lookup k) <$> readTVar vmv
	case mv of
		Nothing -> retry
		Just v -> readTVar v

getSwapchainImageNum :: forall (fmt :: Vk.T.Format) sd ssfc .
	Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Khr.Sfc.S ssfc -> SwapChainSupportDetails ->
	Vk.Extent2d -> QueueFamilyIndices -> IO [()]
getSwapchainImageNum dv sfc spp ext qfis =
	withSwapchain @fmt dv sfc spp ext qfis \sc _ ->
	sameNum () <$> Vk.Khr.Swapchain.getImages dv sc

sameNum :: b -> [a] -> [b]
sameNum x = \case [] -> []; _ : ys -> x : sameNum x ys

getNum :: [a] -> (forall (n :: [()]) . Mappable n => Proxy n -> b) -> b
getNum [] f = f (Proxy :: Proxy '[])
getNum (_ : xs) f =
	getNum xs \(Proxy :: Proxy n) -> f (Proxy :: Proxy ('() ': n))

data Command k
	= Draw (M.Map k (ViewProjection, [Rectangle]))
	| OpenWindow
	deriving Show

data Event k
	= EventEnd
	| EventMouseButtonDown k GlfwG.Ms.MouseButton
	| EventMouseButtonUp k GlfwG.Ms.MouseButton
	| EventCursorPosition k Double Double
	| EventOpenWindow k
	deriving Show

enableValidationLayers :: Bool
enableValidationLayers = maybe True (const False) $(lookupCompileEnv "NDEBUG")

type MouseButtonStateDict = M.Map GlfwG.Ms.MouseButton GlfwG.Ms.MouseButtonState

getMouseButtons :: GlfwG.Win.W sw -> IO MouseButtonStateDict
getMouseButtons w = foldr (uncurry M.insert) M.empty . zip bs
	<$> GlfwG.Ms.getButton w `mapM` bs
	where bs = [GlfwG.Ms.MouseButton'1 .. GlfwG.Ms.MouseButton'8]

mAny :: (a -> Bool) -> M.Map k a -> Bool
mAny p = M.foldr (\x b -> p x || b) False

glfwEvents :: k -> GlfwG.Win.W sw -> TChan (Event k) -> MouseButtonStateDict -> IO ()
glfwEvents k w outp = fix \loop mb1p -> do
	threadDelay 10000
	mb1 <- getMouseButtons w
	sendMouseButtonDown k mb1p mb1 outp `mapM_` mouseButtonAll
	sendMouseButtonUp k mb1p mb1 outp `mapM_` mouseButtonAll
	if mAny (== GlfwG.Ms.MouseButtonState'Pressed) mb1
	then atomically . writeTChan outp . uncurry (EventCursorPosition k)
		=<< GlfwG.Ms.getCursorPos w
	else pure ()
	loop mb1

mouseButtonAll :: [GlfwG.Ms.MouseButton]
mouseButtonAll = [GlfwG.Ms.MouseButton'1 .. GlfwG.Ms.MouseButton'8]

sendMouseButtonDown, sendMouseButtonUp ::
	k -> MouseButtonStateDict -> MouseButtonStateDict -> TChan (Event k) ->
	GlfwG.Ms.MouseButton -> IO ()
sendMouseButtonDown k = sendMouseButton k EventMouseButtonDown
	GlfwG.Ms.MouseButtonState'Released GlfwG.Ms.MouseButtonState'Pressed

sendMouseButtonUp k = sendMouseButton k EventMouseButtonUp
	GlfwG.Ms.MouseButtonState'Pressed GlfwG.Ms.MouseButtonState'Released

sendMouseButton ::
	k ->
	(k -> GlfwG.Ms.MouseButton -> Event k) ->
	GlfwG.Ms.MouseButtonState -> GlfwG.Ms.MouseButtonState ->
	MouseButtonStateDict -> MouseButtonStateDict -> TChan (Event k) ->
	GlfwG.Ms.MouseButton -> IO ()
sendMouseButton k ev pst st pbss bss outp b =
	case (pbss M.! b == pst, bss M.! b == st) of
		(True, True) -> do
--			print $ ev k b
			atomically . writeTChan outp $ ev k b
		_ -> pure ()

withWindow :: Bool -> (forall sw . GlfwG.Win.W sw -> IO a) -> IO a
withWindow v f = GlfwG.Win.group \wgrp -> initWindow v wgrp () >>= f

initWindow :: Ord k => Bool -> GlfwG.Win.Group sw k -> k -> IO (GlfwG.Win.W sw)
initWindow v wgrp k = do
	GlfwG.Win.hint
		$ GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI
	GlfwG.Win.hint $ GlfwG.Win.WindowHint'Visible v
	(fromRight -> w) <- uncurry
		(GlfwG.Win.create' wgrp k) wSize wName Nothing Nothing
	pure w
	where wName = "Triangle"; wSize = (800, 600)

fromRight :: Either String a -> a
fromRight (Left emsg) = error emsg
fromRight (Right x) = x

createInstance :: (forall si . Vk.Ist.I si -> IO a) -> IO a
createInstance f = do
	when enableValidationLayers $ bool
		(error "validation layers requested, but not available!")
		(pure ())
		=<< null . (validationLayers L.\\)
				. (Vk.layerPropertiesLayerName <$>)
			<$> Vk.Ist.enumerateLayerProperties
	exts <- bool id (Vk.Ext.DUtls.extensionName :)
			enableValidationLayers . (Vk.Ist.ExtensionName <$>)
		<$> GlfwG.getRequiredInstanceExtensions
	print exts
	Vk.Ist.create (crinfo exts) nil' f
	where
	crinfo :: [Vk.Ist.ExtensionName] -> Vk.Ist.CreateInfo
		('Just (Vk.Ex.DUtls.Msgr.CreateInfo 'Nothing '[] ())) 'Nothing
	crinfo es = Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.J debugMessengerCreateInfo,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo = Just appinfo,
		Vk.Ist.createInfoEnabledLayerNames =
			bool [] validationLayers enableValidationLayers,
		Vk.Ist.createInfoEnabledExtensionNames = es }
	appinfo = Vk.ApplicationInfo {
		Vk.applicationInfoNext = TMaybe.N,
		Vk.applicationInfoApplicationName = "Hello Triangle",
		Vk.applicationInfoApplicationVersion =
			Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoEngineName = "No Engine",
		Vk.applicationInfoEngineVersion = Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoApiVersion = Vk.apiVersion_1_0 }

validationLayers :: [Vk.LayerName]
validationLayers = [Vk.layerKhronosValidation]

debugMessengerCreateInfo :: Vk.Ex.DUtls.Msgr.CreateInfo 'Nothing '[] ()
debugMessengerCreateInfo = Vk.Ex.DUtls.Msgr.CreateInfo {
	Vk.Ex.DUtls.Msgr.createInfoNext = TMaybe.N,
	Vk.Ex.DUtls.Msgr.createInfoFlags = zeroBits,
	Vk.Ex.DUtls.Msgr.createInfoMessageSeverity =
		Vk.Ext.DUtls.MessageSeverityVerboseBit .|.
		Vk.Ext.DUtls.MessageSeverityWarningBit .|.
		Vk.Ext.DUtls.MessageSeverityErrorBit,
	Vk.Ex.DUtls.Msgr.createInfoMessageType =
		Vk.Ext.DUtls.MessageTypeGeneralBit .|.
		Vk.Ext.DUtls.MessageTypeValidationBit .|.
		Vk.Ext.DUtls.MessageTypePerformanceBit,
	Vk.Ex.DUtls.Msgr.createInfoFnUserCallback = debugCallback,
	Vk.Ex.DUtls.Msgr.createInfoUserData = Nothing }
	where debugCallback _msgsvr _msgtp d _udata = False <$ Txt.putStrLn
		("validation layer: " <> Vk.Ex.DUtls.Msgr.callbackDataMessage d)

run' :: forall (n :: [()]) (scfmt :: Vk.T.Format) si sd k . (
	Mappable n, Vk.T.FormatToValue scfmt, Ord k, Succable k ) =>
	TChan (Command k) -> TChan (Event k) -> TVar (M.Map k (TVar Vk.Extent2d)) ->
	Vk.Ist.I si -> Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q -> IO ()
run' inp outp vext_ ist phd qfis dv gq pq =
	createCommandPool qfis dv \cp ->
	createCommandBuffer dv cp \cb ->
	let	dvs = (phd, qfis, dv, gq, pq, cp, cb) in
	createPipelineLayout dv \dslyt pllyt ->

	createVertexBuffer phd dv gq cp \vb ->
	createIndexBuffer phd dv gq cp \ib ->
	let	vbs = (vb, ib) in

	createUniformBuffer phd dv \ub ubm ->
	createDescriptorPool dv \dp ->
	createDescriptorSet dv dp ub dslyt \ubds ->
	let	ubs = (ubds, ubm) in

	GlfwG.Win.group \wgrp ->
	Vk.Khr.Sfc.group ist nil' \sfcgrp ->
	Vk.RndrPass.group dv nil' \rpgrp ->
	Vk.Ppl.Graphics.group dv nil' \gpgrp ->
	Vk.Semaphore.group dv nil' \iasgrp ->
	Vk.Semaphore.group dv nil' \rfsgrp ->
	Vk.Fence.group dv nil' \iffgrp ->
	Vk.Khr.Swapchain.group dv nil' \scgrp ->
	Vk.ImgVw.group dv nil'
		\(ivgrp :: Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm ivfmt) ->
	Vk.Frmbffr.group dv nil'
		\(fbgrp :: Vk.Frmbffr.Group sd 'Nothing sf (k, Int)) ->
	Vk.Bffr.group dv nil' \rbgrp -> Vk.Mem.group dv nil' \rmgrp ->
	let	rgrps = (rbgrp, rmgrp) in

	let	crwos = winObjs @n @scfmt outp ist phd dv gq cp qfis pllyt vext_ wgrp sfcgrp rpgrp gpgrp
			rgrps iasgrp rfsgrp iffgrp scgrp ivgrp fbgrp in

	atomically (newTVar zero') >>= \vwid ->
	atomically (newTVar M.empty) >>= \vws ->

	mainLoop' @n @siv @sf inp outp dvs pllyt crwos vbs rgrps ubs vwid vws

winObjs :: forall (n :: [()]) (scfmt :: Vk.T.Format) k
	si sd sc sw ssfc sg sl sdsl sias srfs siff ssc nm siv sr sf
	smrct sbrct nmrct . (
	Mappable n, Vk.T.FormatToValue scfmt, Ord k ) =>
	TChan (Event k) -> Vk.Ist.I si -> Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.CmdPool.C sc ->
	QueueFamilyIndices -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	TVar (M.Map k (TVar Vk.Extent2d)) -> Group sw k ->
	Vk.Khr.Sfc.Group 'Nothing ssfc k ->
	Vk.RndrPass.Group sd 'Nothing sr k ->
	Vk.Ppl.Graphics.Group sd 'Nothing sg k '[ '(
		'[	'(Vertex, 'Vk.VtxInp.RateVertex),
			'(Rectangle, 'Vk.VtxInp.RateInstance) ],
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3) ],
		'(sl, '[AtomUbo sdsl], '[]) )] ->
	(	Vk.Bffr.Group sd 'Nothing sbrct k nmrct '[VObj.List 256 Rectangle ""],
		Vk.Mem.Group sd 'Nothing smrct k '[ '(sbrct, Vk.Mem.BufferArg nmrct '[VObj.List 256 Rectangle ""])]
		) ->
	Vk.Semaphore.Group sd 'Nothing sias k ->
	Vk.Semaphore.Group sd 'Nothing srfs k ->
	Vk.Fence.Group sd 'Nothing siff k ->
	Vk.Khr.Swapchain.Group sd 'Nothing scfmt ssc k ->
	Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm scfmt ->
	Vk.Frmbffr.Group sd 'Nothing sf (k, Int) -> k ->
	IO (WinObjs
		sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm
		(Replicate n siv) sr (Replicate n sf))
winObjs outp ist phd dv gq cp qfis pllyt vext_
	wgrp sfcgrp rpgrp gpgrp rgrps iasgrp rfsgrp iffgrp scgrp ivgrp fbgrp k =
	initWindow True wgrp k >>= \w ->
	forkIO (glfwEvents k w outp . foldr (uncurry M.insert) M.empty
		$ ((, GlfwG.Ms.MouseButtonState'Released)
		<$>) [GlfwG.Ms.MouseButton'1 .. GlfwG.Ms.MouseButton'8]) >>
	atomically (newTVar False) >>= \fbrszd ->
	GlfwG.Win.setFramebufferSizeCallback w
		(Just \_ _ _ -> atomically $ writeTVar fbrszd True) >>

	Vk.Khr.Sfc.Glfw.Win.create' ist sfcgrp k w >>= \(fromRight -> sfc) ->
	createRenderPass @scfmt rpgrp k >>= \rp ->
	prepareSwapchain @scfmt w sfc phd >>= \(spp, ext) ->

	createRectangleBuffer' phd dv gq cp rgrps k dummy >>

	atomically (
		newTVar (Vk.Extent2d 0 0) >>= \v ->
		writeTVar v ext >>
		v <$ modifyTVar vext_ (M.insert k v) ) >>= \vext ->

	createGraphicsPipeline gpgrp k ext rp pllyt >>= \gpl ->
	createSyncObjects iasgrp rfsgrp iffgrp k >>= \sos ->
	createSwapchain @scfmt scgrp k sfc spp ext qfis >>= \(sc, _) ->

	Vk.Khr.Swapchain.getImages dv sc >>= \scis ->
	createImageViews' @n ivgrp k scis >>= \scivs ->
	createFramebuffers' @n @siv fbgrp k ext rp scivs >>= \fbs ->

	let	wos = WinObjs
			(w, fbrszd) sfc vext gpl sos (sc, scivs, rp, fbs) in
	pure wos

createSurface :: GlfwG.Win.W sw -> Vk.Ist.I si ->
	(forall ss . Vk.Khr.Sfc.S ss -> IO a) -> IO a
createSurface win ist f =
	Vk.Khr.Sfc.group ist nil' \sfcgrp ->
	Vk.Khr.Sfc.Glfw.Win.create' ist sfcgrp () win >>= f . fromRight

pickPhysicalDevice :: Vk.Ist.I si ->
	Vk.Khr.Sfc.S ss -> IO (Vk.PhDvc.P, QueueFamilyIndices)
pickPhysicalDevice ist sfc = do
	dvcs <- Vk.PhDvc.enumerate ist
	when (null dvcs) $ error "failed to find GPUs with Gpu.Vulkan support!"
	findDevice (`isDeviceSuitable` sfc) dvcs >>= \case
		Just pdvc -> pure pdvc
		Nothing -> error "failed to find a suitable GPU!"

findDevice :: Monad m =>
	(Vk.PhDvc.P -> m (Maybe a)) -> [Vk.PhDvc.P] ->
	m (Maybe (Vk.PhDvc.P, a))
findDevice prd = \case
	[] -> pure Nothing
	p : ps -> prd p >>= \case
		Nothing -> findDevice prd ps; Just x -> pure $ Just (p, x)

isDeviceSuitable ::
	Vk.PhDvc.P -> Vk.Khr.Sfc.S ss -> IO (Maybe QueueFamilyIndices)
isDeviceSuitable phdvc sfc = do
	_deviceProperties <- Vk.PhDvc.getProperties phdvc
	_deviceFeatures <- Vk.PhDvc.getFeatures phdvc
	is <- findQueueFamilies phdvc sfc
	extensionSupported <- checkDeviceExtensionSupport phdvc
	if extensionSupported
	then (<$> querySwapChainSupport phdvc sfc) \spp ->
		bool (completeQueueFamilies is) Nothing
			$ null (formats spp) || null (presentModes spp)
	else pure Nothing

data QueueFamilyIndices = QueueFamilyIndices {
	graphicsFamily :: Vk.QueueFamily.Index,
	presentFamily :: Vk.QueueFamily.Index }

data QueueFamilyIndicesMaybe = QueueFamilyIndicesMaybe {
	graphicsFamilyMaybe :: Maybe Vk.QueueFamily.Index,
	presentFamilyMaybe :: Maybe Vk.QueueFamily.Index }

completeQueueFamilies :: QueueFamilyIndicesMaybe -> Maybe QueueFamilyIndices
completeQueueFamilies = \case
	QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = Just gf, presentFamilyMaybe = Just pf } ->
		Just QueueFamilyIndices {
			graphicsFamily = gf, presentFamily = pf }
	_ -> Nothing

findQueueFamilies ::
	Vk.PhDvc.P -> Vk.Khr.Sfc.S ss -> IO QueueFamilyIndicesMaybe
findQueueFamilies device sfc = do
	queueFamilies <- Vk.PhDvc.getQueueFamilyProperties device
	pfis <- filterM
		(\i -> Vk.Khr.Sfc.Phd.getSupport device i sfc)
		(fst <$> queueFamilies)
	pure QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = fst <$> L.find
			(checkBits Vk.Queue.GraphicsBit
				. Vk.QueueFamily.propertiesQueueFlags . snd)
			queueFamilies,
		presentFamilyMaybe = listToMaybe pfis }

checkDeviceExtensionSupport :: Vk.PhDvc.P -> IO Bool
checkDeviceExtensionSupport dvc =
	null . (deviceExtensions L.\\) . (Vk.PhDvc.extensionPropertiesExtensionName <$>)
		<$> Vk.PhDvc.enumerateExtensionProperties dvc Nothing

deviceExtensions :: [Vk.PhDvc.ExtensionName]
deviceExtensions = [Vk.Khr.Swapchain.extensionName]

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.Khr.Sfc.Capabilities,
	formats :: [Vk.Khr.Sfc.Format],
	presentModes :: [Vk.Khr.PresentMode] }

querySwapChainSupport ::
	Vk.PhDvc.P -> Vk.Khr.Sfc.S ss -> IO SwapChainSupportDetails
querySwapChainSupport dvc sfc = SwapChainSupportDetails
	<$> Vk.Khr.Sfc.Phd.getCapabilities dvc sfc
	<*> Vk.Khr.Sfc.Phd.getFormats dvc sfc
	<*> Vk.Khr.Sfc.Phd.getPresentModes dvc sfc

createLogicalDevice :: (Ord k, Vk.AllocationCallbacks.ToMiddle ma) =>
	Vk.PhDvc.P -> Vk.Dvc.Group ma sd k -> k ->
	QueueFamilyIndices -> IO (Vk.Dvc.D sd, Vk.Queue.Q, Vk.Queue.Q)
createLogicalDevice phdvc dvcgrp k qfis =
	mkHeteroParList queueCreateInfos uniqueQueueFamilies \qs ->
	Vk.Dvc.create' phdvc dvcgrp k (createInfo qs) >>= \(fromRight -> dvc) -> do
		gq <- Vk.Dvc.getQueue dvc (graphicsFamily qfis) 0
		pq <- Vk.Dvc.getQueue dvc (presentFamily qfis) 0
		pure (dvc, gq, pq)
	where
	createInfo qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = def,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames =
			bool [] validationLayers enableValidationLayers,
		Vk.Dvc.createInfoEnabledExtensionNames = deviceExtensions,
		Vk.Dvc.createInfoEnabledFeatures = Just def }
	uniqueQueueFamilies = L.nub [graphicsFamily qfis, presentFamily qfis]
	queueCreateInfos qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }

mkHeteroParList :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] ->
	(forall ss . HeteroParList.ToListWithCM' WithPoked TMaybe.M ss => HeteroParList.PL t ss -> b) -> b
mkHeteroParList _k [] f = f HeteroParList.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

prepareSwapchain :: forall (scfmt :: Vk.T.Format) sw ssfc .
	Vk.T.FormatToValue scfmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.PhDvc.P ->
	IO (SwapChainSupportDetails, Vk.Extent2d)
prepareSwapchain win sfc phdvc = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	fmt0 = Vk.T.formatToValue @scfmt
		fmt = Vk.Khr.Sfc.formatFormat
			. chooseSwapSurfaceFormat $ formats spp
	when (fmt0 /= fmt) $ error
		"Rectangles: prepareSwapchain format not match"
	pure (spp, ext)

withSwapchain ::
	Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Khr.Sfc.S ssfc -> SwapChainSupportDetails ->
	Vk.Extent2d -> QueueFamilyIndices ->
	(forall ssc . Vk.Khr.Swapchain.S fmt ssc -> Vk.Extent2d -> IO a) -> IO a
withSwapchain dvc sfc spp ext qfis f =
	Vk.Khr.Swapchain.group dvc nil' \scgrp ->
	uncurry f =<< createSwapchain scgrp () sfc spp ext qfis

createSwapchain ::
	forall (scfmt :: Vk.T.Format) ssfc sd ma ssc k . (
	Ord k, Vk.AllocationCallbacks.ToMiddle ma ) =>
	Vk.T.FormatToValue scfmt =>
	Vk.Khr.Swapchain.Group sd ma scfmt ssc k -> k ->
	Vk.Khr.Sfc.S ssfc -> SwapChainSupportDetails -> Vk.Extent2d ->
	QueueFamilyIndices -> IO (Vk.Khr.Swapchain.S scfmt ssc, Vk.Extent2d)
createSwapchain scgrp k sfc spp ext qfis =
	Vk.Khr.Swapchain.create' @scfmt scgrp k crInfo
		>>= \(fromRight -> sc) -> pure (sc, ext)
	where crInfo = mkSwapchainCreateInfoNew sfc qfis spp ext

mkSwapchainCreateInfoNew :: Vk.Khr.Sfc.S ss -> QueueFamilyIndices ->
	SwapChainSupportDetails -> Vk.Extent2d ->
	Vk.Khr.Swapchain.CreateInfo 'Nothing ss fmt
mkSwapchainCreateInfoNew sfc qfis0 spp ext =
	Vk.Khr.Swapchain.CreateInfo {
		Vk.Khr.Swapchain.createInfoNext = TMaybe.N,
		Vk.Khr.Swapchain.createInfoFlags = def,
		Vk.Khr.Swapchain.createInfoSurface = sfc,
		Vk.Khr.Swapchain.createInfoMinImageCount = imgc,
		Vk.Khr.Swapchain.createInfoImageColorSpace =
			Vk.Khr.Sfc.formatColorSpace fmt,
		Vk.Khr.Swapchain.createInfoImageExtent = ext,
		Vk.Khr.Swapchain.createInfoImageArrayLayers = 1,
		Vk.Khr.Swapchain.createInfoImageUsage =
			Vk.Img.UsageColorAttachmentBit,
		Vk.Khr.Swapchain.createInfoImageSharingMode = ism,
		Vk.Khr.Swapchain.createInfoQueueFamilyIndices = qfis,
		Vk.Khr.Swapchain.createInfoPreTransform =
			Vk.Khr.Sfc.capabilitiesCurrentTransform caps,
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
		$ Vk.Khr.Sfc.capabilitiesMaxImageCount caps
	imgc = clamp
		(Vk.Khr.Sfc.capabilitiesMinImageCount caps + 1) 0 maxImgc
	(ism, qfis) = bool
		(Vk.SharingModeConcurrent,
			[graphicsFamily qfis0, presentFamily qfis0])
		(Vk.SharingModeExclusive, [])
		(graphicsFamily qfis0 == presentFamily qfis0)

recreateSwapchain :: Vk.T.FormatToValue scfmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.PhDvc.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Khr.Swapchain.S scfmt ssc ->
	IO Vk.Extent2d
recreateSwapchain win sfc phdvc qfis0 dvc sc = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	crInfo = mkSwapchainCreateInfoNew sfc qfis0 spp ext
	ext <$ Vk.Khr.Swapchain.recreate @'Nothing dvc crInfo nil' sc

chooseSwapSurfaceFormat  :: [Vk.Khr.Sfc.Format] -> Vk.Khr.Sfc.Format
chooseSwapSurfaceFormat = \case
	availableFormats@(af0 : _) -> fromMaybe af0
		$ L.find preferredSwapSurfaceFormat availableFormats
	_ -> error "no available swap surface formats"

preferredSwapSurfaceFormat :: Vk.Khr.Sfc.Format -> Bool
preferredSwapSurfaceFormat f =
	Vk.Khr.Sfc.formatFormat f == Vk.FormatB8g8r8a8Srgb &&
	Vk.Khr.Sfc.formatColorSpace f == Vk.Khr.ColorSpaceSrgbNonlinear

chooseSwapPresentMode :: [Vk.Khr.PresentMode] -> Vk.Khr.PresentMode
chooseSwapPresentMode =
	fromMaybe Vk.Khr.PresentModeFifo . L.find (== Vk.Khr.PresentModeMailbox)

chooseSwapExtent :: GlfwG.Win.W sw -> Vk.Khr.Sfc.Capabilities -> IO Vk.Extent2d
chooseSwapExtent win caps
	| Vk.extent2dWidth curExt /= maxBound = pure curExt
	| otherwise = do
		(fromIntegral -> w, fromIntegral -> h) <-
			GlfwG.Win.getFramebufferSize win
		pure $ Vk.Extent2d
			(clamp w (Vk.extent2dWidth n) (Vk.extent2dHeight n))
			(clamp h (Vk.extent2dWidth x) (Vk.extent2dHeight x))
	where
	curExt = Vk.Khr.Sfc.capabilitiesCurrentExtent caps
	n = Vk.Khr.Sfc.capabilitiesMinImageExtent caps
	x = Vk.Khr.Sfc.capabilitiesMaxImageExtent caps

createImageViews' :: forall n ivfmt sd si siv k sm nm ifmt . (
	Mappable n, Ord k, Vk.T.FormatToValue ivfmt ) =>
	Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm ivfmt ->
	k -> [Vk.Img.Binded sm si nm ifmt] ->
	IO (HeteroParList.PL (Vk.ImgVw.I nm ivfmt) (Replicate n siv))
createImageViews' ivgrp k imgs = do
	ivs <- createImageViewList ivgrp k imgs
	pure $ homoListFromList @_ @n ivs

createImageViewList :: forall ivfmt sd si siv k sm nm ifmt . (
	Ord k, Vk.T.FormatToValue ivfmt ) =>
	Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm ivfmt ->
	k -> [Vk.Img.Binded sm si nm ifmt] -> IO [Vk.ImgVw.I nm ivfmt siv]
createImageViewList ivgrp k =
	mapM (\(i, img) -> createImageView' ivgrp (k, i) img) . zip [0 ..]

createImageView' :: forall ivfmt sd si siv k sm nm ifmt .
	Ord k =>
	Vk.T.FormatToValue ivfmt =>
	Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm ivfmt ->
	(k, Int) -> Vk.Img.Binded sm si nm ifmt -> IO (Vk.ImgVw.I nm ivfmt siv)
createImageView' ivgrp k timg = fromRight <$>
	Vk.ImgVw.create' ivgrp k (mkImageViewCreateInfoNew timg)

recreateImageViews :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss nm scfmt] -> HeteroParList.PL (Vk.ImgVw.I nm scfmt) sis -> IO ()
recreateImageViews _dvc [] HeteroParList.Nil = pure ()
recreateImageViews dvc (sci : scis) (iv :** ivs) =
	Vk.ImgVw.recreate dvc (mkImageViewCreateInfoNew sci) nil' iv >>
	recreateImageViews dvc scis ivs
recreateImageViews _ _ _ =
	error "number of Vk.Img.I and Vk.ImageView.I should be same"

mkImageViewCreateInfoNew ::
	Vk.Img.Binded sm si nm ifmt ->
	Vk.ImgVw.CreateInfo 'Nothing sm si nm ifmt ivfmt
mkImageViewCreateInfoNew sci = Vk.ImgVw.CreateInfo {
	Vk.ImgVw.createInfoNext = TMaybe.N,
	Vk.ImgVw.createInfoFlags = Vk.ImgVw.CreateFlagsZero,
	Vk.ImgVw.createInfoImage = sci,
	Vk.ImgVw.createInfoViewType = Vk.ImgVw.Type2d,
	Vk.ImgVw.createInfoComponents = components,
	Vk.ImgVw.createInfoSubresourceRange = subresourceRange }
	where
	components = Vk.Component.Mapping {
		Vk.Component.mappingR = def, Vk.Component.mappingG = def,
		Vk.Component.mappingB = def, Vk.Component.mappingA = def }
	subresourceRange = Vk.Img.SubresourceRange {
		Vk.Img.subresourceRangeAspectMask = Vk.Img.AspectColorBit,
		Vk.Img.subresourceRangeBaseMipLevel = 0,
		Vk.Img.subresourceRangeLevelCount = 1,
		Vk.Img.subresourceRangeBaseArrayLayer = 0,
		Vk.Img.subresourceRangeLayerCount = 1 }

createRenderPass ::
	forall (scfmt :: Vk.T.Format) sd ma sr k . (
	Vk.T.FormatToValue scfmt, Ord k,
	Vk.AllocationCallbacks.ToMiddle ma ) =>
	Vk.RndrPass.Group sd ma sr k -> k -> IO (Vk.RndrPass.R sr)
createRenderPass rpgrp k =
	fromRight <$> Vk.RndrPass.create' @_ @_ @'[scfmt] rpgrp k renderPassInfo
	where
	colorAttachment :: Vk.Att.Description scfmt
	colorAttachment = Vk.Att.Description {
		Vk.Att.descriptionFlags = zeroBits,
		Vk.Att.descriptionSamples = Vk.Sample.Count1Bit,
		Vk.Att.descriptionLoadOp = Vk.Att.LoadOpClear,
		Vk.Att.descriptionStoreOp = Vk.Att.StoreOpStore,
		Vk.Att.descriptionStencilLoadOp = Vk.Att.LoadOpDontCare,
		Vk.Att.descriptionStencilStoreOp = Vk.Att.StoreOpDontCare,
		Vk.Att.descriptionInitialLayout = Vk.Img.LayoutUndefined,
		Vk.Att.descriptionFinalLayout = Vk.Img.LayoutPresentSrcKhr }
	colorAttachmentRef = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 0,
		Vk.Att.referenceLayout = Vk.Img.LayoutColorAttachmentOptimal }
	subpass = Vk.Subpass.Description {
		Vk.Subpass.descriptionFlags = zeroBits,
		Vk.Subpass.descriptionPipelineBindPoint =
			Vk.Ppl.BindPointGraphics,
		Vk.Subpass.descriptionInputAttachments = [],
		Vk.Subpass.descriptionColorAndResolveAttachments =
			Left [colorAttachmentRef],
		Vk.Subpass.descriptionDepthStencilAttachment = Nothing,
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
	renderPassInfo = Vk.RndrPass.CreateInfo {
		Vk.RndrPass.createInfoNext = TMaybe.N,
		Vk.RndrPass.createInfoFlags = zeroBits,
		Vk.RndrPass.createInfoAttachments =
			colorAttachment :** HeteroParList.Nil,
		Vk.RndrPass.createInfoSubpasses = [subpass],
		Vk.RndrPass.createInfoDependencies = [dependency] }

type AtomUbo s = '(s, '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing]])

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscSetLyt.D s '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing]]
	-> IO a) -> IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil'
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing
		'[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing] ]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings = uboLayoutBinding :** HeteroParList.Nil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing])
	uboLayoutBinding = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

createPipelineLayout ::
	Vk.Dvc.D sd -> (forall sdsl sl .
		Vk.DscSetLyt.D sdsl
			'[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing]] ->
		Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] -> IO b) -> IO b
createPipelineLayout dvc f =
	createDescriptorSetLayout dvc \dsl ->
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = TMaybe.N,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts =
				HeteroParList.Singleton $ U2 dsl } in
	Vk.Ppl.Layout.create @'Nothing @_ @_ @'[] dvc pipelineLayoutInfo nil' $ f dsl

createGraphicsPipeline :: (Ord k, Vk.AllocationCallbacks.ToMiddle mac) =>
	Vk.Ppl.Graphics.Group sd mac sg k '[ '(
		'[ '(Vertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)],
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3) ],
			'(sl, '[AtomUbo sdsl], '[]) )] -> k ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] -> IO (
		Vk.Ppl.Graphics.G sg
			'[ '(Vertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)]
			'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
				'(2, RectPos), '(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3) ]
			'(sl, '[AtomUbo sdsl], '[]))
createGraphicsPipeline gpgrp k sce rp pllyt =
	Vk.Ppl.Graphics.createGs' gpgrp k Nothing (U14 pplInfo :** HeteroParList.Nil)
			>>= \(fromRight -> (U3 gpl :** HeteroParList.Nil)) -> pure gpl
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp pllyt

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[AtomUbo sdsl], '[]) -> IO ()
recreateGraphicsPipeline dvc sce rp pllyt gpls = Vk.Ppl.Graphics.recreateGs
	dvc Nothing (U14 pplInfo :** HeteroParList.Nil) nil' (U3 gpls :** HeteroParList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp pllyt

mkGraphicsPipelineCreateInfo' ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing,
			'[ '(Vertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
				'(2, RectPos), '(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3) ] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing '(sl, '[AtomUbo sdsl], '[]) sr '(sb, vs', ts', foo)
mkGraphicsPipelineCreateInfo' sce rp pllyt = Vk.Ppl.Graphics.CreateInfo {
	Vk.Ppl.Graphics.createInfoNext = TMaybe.N,
	Vk.Ppl.Graphics.createInfoFlags = Vk.Ppl.CreateFlagsZero,
	Vk.Ppl.Graphics.createInfoStages = shaderStages,
	Vk.Ppl.Graphics.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Graphics.createInfoInputAssemblyState = Just inputAssembly,
	Vk.Ppl.Graphics.createInfoViewportState = Just $ mkViewportState sce,
	Vk.Ppl.Graphics.createInfoRasterizationState = Just rasterizer,
	Vk.Ppl.Graphics.createInfoMultisampleState = Just multisampling,
	Vk.Ppl.Graphics.createInfoDepthStencilState = Nothing,
	Vk.Ppl.Graphics.createInfoColorBlendState = Just colorBlending,
	Vk.Ppl.Graphics.createInfoDynamicState = Nothing,
	Vk.Ppl.Graphics.createInfoLayout = U3 pllyt,
	Vk.Ppl.Graphics.createInfoRenderPass = rp,
	Vk.Ppl.Graphics.createInfoSubpass = 0,
	Vk.Ppl.Graphics.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Graphics.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Graphics.createInfoTessellationState = Nothing }

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
			shaderModuleCreateInfo glslVertexShaderMain, nil' ),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	fragShaderStageInfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModule = (
			shaderModuleCreateInfo glslFragmentShaderMain, nil' ),
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

rasterizer :: Vk.Ppl.RstSt.CreateInfo 'Nothing
rasterizer = Vk.Ppl.RstSt.CreateInfo {
	Vk.Ppl.RstSt.createInfoNext = TMaybe.N,
	Vk.Ppl.RstSt.createInfoFlags = zeroBits,
	Vk.Ppl.RstSt.createInfoDepthClampEnable = False,
	Vk.Ppl.RstSt.createInfoRasterizerDiscardEnable = False,
	Vk.Ppl.RstSt.createInfoPolygonMode = Vk.PolygonModeFill,
	Vk.Ppl.RstSt.createInfoLineWidth = 1,
--	Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeBackBit,
	Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeNone,
	Vk.Ppl.RstSt.createInfoFrontFace = Vk.FrontFaceCounterClockwise,
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

createFramebuffers' ::
	forall ts siv k sd sf sr nm fmt .
	(Mappable ts, Ord k) =>
	Vk.Frmbffr.Group sd 'Nothing sf (k, Int) -> k ->
	Vk.Extent2d -> Vk.RndrPass.R sr ->
	HeteroParList.PL (Vk.ImgVw.I nm fmt) (Replicate ts siv) ->
	IO (HeteroParList.PL Vk.Frmbffr.F (Replicate ts sf))
createFramebuffers' fbgrp k sce rp =
	mapHomoListMWithI @_ @ts @_ @_ @siv 0 \i sciv ->
	fromRight <$> Vk.Frmbffr.create'
		fbgrp (k, i) (mkFramebufferCreateInfo sce rp sciv)

recreateFramebuffers' :: forall ts sd sr nm fmt siv sf .
	Mappable ts =>
	Vk.Dvc.D sd -> Vk.Extent2d ->
	Vk.RndrPass.R sr -> HeteroParList.PL (Vk.ImgVw.I nm fmt) (Replicate ts siv) ->
	HeteroParList.PL Vk.Frmbffr.F (Replicate ts sf) -> IO ()
recreateFramebuffers' dvc sce rp =
	zipWithHomoListM_ @_ @ts @_ @_ @siv @_ @sf \sciv fb ->
	Vk.Frmbffr.recreate dvc (mkFramebufferCreateInfo sce rp sciv) nil' fb

class Mappable (ts :: [knd]) where
	type Replicate ts s :: [Type]
	homoListFromList :: [t s] -> HeteroParList.PL t (Replicate ts s)
	mapHomoListMWithI :: Monad m => Int -> (Int -> t a -> m (t' b)) ->
		HeteroParList.PL t (Replicate ts a) ->
		m (HeteroParList.PL t' (Replicate ts b))
	zipWithHomoListM_ :: Monad m => (t a -> t' b -> m c) ->
		HeteroParList.PL t (Replicate ts a) ->
		HeteroParList.PL t' (Replicate ts b) -> m ()

instance Mappable '[] where
	type Replicate '[] s = '[]
	homoListFromList [] = HeteroParList.Nil
	homoListFromList _ = error "bad"
	mapHomoListMWithI _ _ HeteroParList.Nil = pure HeteroParList.Nil
	zipWithHomoListM_ _ HeteroParList.Nil HeteroParList.Nil = pure ()

instance Mappable ts => Mappable (t ': ts) where
	type Replicate (t ': ts) s = s ': Replicate ts s
	homoListFromList (x : xs) = x :** (homoListFromList @_ @ts xs)
	homoListFromList _ = error "bad"
	mapHomoListMWithI :: forall t' a t'' b m .
		Monad m => Int -> (Int -> t' a -> m (t'' b)) ->
		HeteroParList.PL t' (Replicate (t ': ts) a) ->
		m (HeteroParList.PL t'' (Replicate (t ': ts) b))
	mapHomoListMWithI i f (x :** xs) = (:**) <$> f i x
		<*> mapHomoListMWithI @_ @ts @_ @_ @a @_ @b (i + 1) f xs
	zipWithHomoListM_ f (x :** xs) (y :** ys) =
		f x y >> zipWithHomoListM_ @_ @ts f xs ys


mkFramebufferCreateInfo ::
	Vk.Extent2d -> Vk.RndrPass.R sr -> Vk.ImgVw.I nm fmt si ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(nm, fmt, si)]
mkFramebufferCreateInfo sce rp attch = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = U3 attch :** HeteroParList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where
	Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = sce

createCommandPool :: QueueFamilyIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPool.C sc -> IO a) -> IO a
createCommandPool qfis dvc f =
	Vk.CmdPool.create dvc poolInfo nil' \cp -> f cp
	where poolInfo = Vk.CmdPool.CreateInfo {
		Vk.CmdPool.createInfoNext = TMaybe.N,
		Vk.CmdPool.createInfoFlags =
			Vk.CmdPool.CreateResetCommandBufferBit,
		Vk.CmdPool.createInfoQueueFamilyIndex = graphicsFamily qfis }

createVertexBuffer :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] -> IO a ) -> IO a
createVertexBuffer phdvc dvc gq cp f =
	createBufferList phdvc dvc (fromIntegral $ length vertices)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ -> do
	createBufferList phdvc dvc (fromIntegral $ length vertices)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
		\(b' :: Vk.Bffr.Binded sm sb "vertex-buffer" '[VObj.List 256 t ""]) bm' -> do
		Vk.Mem.write @"vertex-buffer" @(VObj.List 256 Vertex "") dvc bm' zeroBits vertices
		copyBuffer dvc gq cp b' b
	f b

createIndexBuffer :: Vk.PhDvc.P ->
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 Word16 ""] -> IO a) -> IO a
createIndexBuffer phdvc dvc gq cp f =
	createBufferList phdvc dvc (fromIntegral $ length indices)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageIndexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit \b _ -> do
	createBufferList phdvc dvc (fromIntegral $ length indices)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
			\(b' :: Vk.Bffr.Binded sm sb "vertex-buffer" '[VObj.List 256 t ""]) bm' -> do
		Vk.Mem.write @"vertex-buffer" @(VObj.List 256 Word16 "") dvc bm' zeroBits indices
		copyBuffer dvc gq cp b' b
	f b

createRectangleBuffer :: Ord k =>
	Devices sd sc scb -> RectGroups sd sm sb nm k -> k -> [Rectangle] ->
	IO (Vk.Bffr.Binded sm sb nm '[VObj.List 256 Rectangle ""])
createRectangleBuffer (phdvc, _qfis, dvc, gq, _pq, cp, _cb) (bgrp, mgrp) k rs =
	createBufferList' phdvc dvc bgrp mgrp k (fromIntegral $ length rs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit >>= \(b, _) -> do
	createBufferList phdvc dvc (fromIntegral $ length rs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
			\(b' :: Vk.Bffr.Binded sm sb "rectangle-buffer" '[VObj.List 256 t ""]) bm' -> do
		Vk.Mem.write @"rectangle-buffer" @(VObj.List 256 Rectangle "") dvc bm' zeroBits rs
		copyBuffer dvc gq cp b' b
	pure b

createRectangleBuffer' :: Ord k =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	RectGroups sd sm sb nm k -> k -> [Rectangle] ->
	IO (Vk.Bffr.Binded sm sb nm '[VObj.List 256 Rectangle ""])
createRectangleBuffer' phdvc dvc gq cp (bgrp, mgrp) k rs =
	createBufferList' phdvc dvc bgrp mgrp k (fromIntegral $ length rs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mem.PropertyDeviceLocalBit >>= \(b, _) -> do
	createBufferList phdvc dvc (fromIntegral $ length rs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mem.PropertyHostVisibleBit .|.
			Vk.Mem.PropertyHostCoherentBit )
			\(b' :: Vk.Bffr.Binded sm sb "rectangle-buffer" '[VObj.List 256 t ""]) bm' -> do
		Vk.Mem.write @"rectangle-buffer" @(VObj.List 256 Rectangle "") dvc bm' zeroBits rs
		copyBuffer dvc gq cp b' b
	pure b

destroyRectangleBuffer :: Ord k => RectGroups sd sm sb nm k -> k -> IO ()
destroyRectangleBuffer (bgrp, mgrp) k = do
	r1 <- Vk.Mem.free mgrp k
	r2 <- Vk.Bffr.destroy bgrp k
	case (r1, r2) of
		(Left msg, _) -> error msg
		(_, Left msg) -> error msg
		_ -> pure ()

type RectGroups sd sm sb nm k = (
	Vk.Bffr.Group sd 'Nothing sb k nm '[VObj.List 256 Rectangle ""],
	Vk.Mem.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 Rectangle ""])] )

createUniformBuffer :: Vk.PhDvc.P -> Vk.Dvc.D sd -> (forall sm sb .
		Vk.Bffr.Binded sm sb "uniform-buffer" '[VObj.Atom 256 ViewProjection 'Nothing]  ->
		UniformBufferMemory sm sb ->
		IO b) -> IO b
createUniformBuffer phdvc dvc = createBufferAtom phdvc dvc
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mem.PropertyHostVisibleBit .|. Vk.Mem.PropertyHostCoherentBit)

type UniformBufferMemory sm sb = Vk.Mem.M sm '[ '(
	sb,
	'Vk.Mem.BufferArg "uniform-buffer" '[VObj.Atom 256 ViewProjection 'Nothing]
	)]

createDescriptorPool ::
	Vk.Dvc.D sd -> (forall sp . Vk.DscPool.P sp -> IO a) -> IO a
createDescriptorPool dvc = Vk.DscPool.create dvc poolInfo nil'
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = TMaybe.N,
		Vk.DscPool.createInfoFlags =
			Vk.DscPool.CreateFreeDescriptorSetBit,
		Vk.DscPool.createInfoMaxSets = 1,
		Vk.DscPool.createInfoPoolSizes = [poolSize] }
	poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = 1 }

createDescriptorSet ::
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 ViewProjection 'Nothing] ->
	Vk.DscSetLyt.D sdsc '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing]] ->
	(forall sds .
		Vk.DscSet.D sds '(sdsc, '[ 'Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing]]) -> IO a) -> IO a
createDescriptorSet dvc dscp ub dscslyt f =
	Vk.DscSet.allocateDs dvc allocInfo \(HeteroParList.Singleton dscs) -> do
	Vk.DscSet.updateDs dvc
		(HeteroParList.Singleton . U5 $ descriptorWrite ub dscs) HeteroParList.Nil
	f dscs
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts =
			HeteroParList.Singleton $ U2 dscslyt }

descriptorWrite ::
	Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 ViewProjection 'Nothing] ->
	Vk.DscSet.D sds slbts ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, VObj.Atom 256 ViewProjection 'Nothing)]) 0
descriptorWrite ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HeteroParList.Singleton bufferInfo }
	where bufferInfo = U4 $ Vk.Dsc.BufferInfo ub

createBufferAtom :: forall sd nm a b . Storable a => Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 a 'Nothing] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[VObj.Atom 256 a 'Nothing] )] ->
			IO b) -> IO b
createBufferAtom p dv usg props = createBuffer p dv VObj.LengthAtom usg props

createBufferList :: forall sd nm t a . Storable t =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 t ""] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[VObj.List 256 t ""] ) ] ->
		IO a) -> IO a
createBufferList p dv ln usg props =
	createBuffer p dv (VObj.LengthList ln) usg props

createBufferList' :: forall sd nm t sm sb k . (Ord k, Storable t) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[VObj.List 256 t ""]  ->
	Vk.Mem.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 t ""])] ->
	k ->
	Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mem.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 t ""],
		Vk.Mem.M sm '[ '(
			sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 t ""] ) ] )
createBufferList' p dv bgrp mgrp k ln usg props =
	createBuffer' p dv bgrp mgrp k (VObj.LengthList ln) usg props

createBuffer :: forall sd nm o a . VObj.SizeAlignment o =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mem.M sm
			'[ '(sb, 'Vk.Mem.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer p dv ln usg props f =
	Vk.Bffr.group dv nil' \bgrp -> Vk.Mem.group dv nil' \mgrp ->
	uncurry f =<< createBuffer' p dv bgrp mgrp () ln usg props

createBuffer' :: forall sd sb nm o sm k .
	(Ord k, VObj.SizeAlignment o) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[o] ->
	Vk.Mem.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mem.BufferArg nm '[o])] -> k ->
	VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[o],
		Vk.Mem.M sm '[ '(sb, 'Vk.Mem.BufferArg nm '[o])] )
createBuffer' p dv bgrp mgrp k ln usg props =
	Vk.Bffr.create' bgrp k bffrInfo >>= \(AlwaysRight b) -> do
		reqs <- Vk.Bffr.getMemoryRequirements dv b
		mt <- findMemoryType p (Vk.Mem.requirementsMemoryTypeBits reqs) props
		Vk.Mem.allocateBind' mgrp k (HeteroParList.Singleton . U2 $ Vk.Mem.Buffer b)
			(allcInfo mt) >>=
			\(AlwaysRight (HeteroParList.Singleton (U2 (Vk.Mem.BufferBinded bnd)), mem)) -> pure (bnd, mem)
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HeteroParList.Singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mem.TypeIndex -> Vk.Mem.AllocateInfo 'Nothing
	allcInfo mt = Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mt }

{-# COMPLETE AlwaysRight #-}

pattern AlwaysRight :: r -> Either l r
pattern AlwaysRight x <- Right x where
	AlwaysRight x = Right x

findMemoryType :: Vk.PhDvc.P -> Vk.Mem.TypeBits -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.PhDvc.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> L.find ((&&)
		<$> (`Vk.Mem.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mem.mTypePropertyFlags . snd) tps
		where tps = Vk.PhDvc.memoryPropertiesMemoryTypes props1

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a . Storable' a =>
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 a ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 a ""] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CmdBffr.allocate
		dvc allocInfo \(cb :*. HeteroParList.Nil) -> do
		let	submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks =
					HeteroParList.Nil,
				Vk.submitInfoCommandBuffers =
					HeteroParList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HeteroParList.Nil }
		Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo do
			Vk.Cmd.copyBuffer @'[ '[VObj.List 256 a ""]] cb src dst
		Vk.Queue.submit gq (HeteroParList.Singleton $ U4 submitInfo) Nothing
		Vk.Queue.waitIdle gq
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo 'Nothing sc '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }
	beginInfo = Vk.CmdBffr.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }

createCommandBuffer ::
	forall sd scp a . Vk.Dvc.D sd -> Vk.CmdPool.C scp ->
	(forall scb . Vk.CmdBffr.C scb -> IO a) ->
	IO a
createCommandBuffer dvc cp f = Vk.CmdBffr.allocate dvc allocInfo $ f . \(cb :*. HeteroParList.Nil) -> cb
	where
	allocInfo :: Vk.CmdBffr.AllocateInfo 'Nothing scp '[ '()]
	allocInfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }

data SyncObjects (ssos :: (Type, Type, Type)) where
	SyncObjects :: {
		_imageAvailableSemaphores :: Vk.Semaphore.S sias,
		_renderFinishedSemaphores :: Vk.Semaphore.S srfs,
		_inFlightFences :: Vk.Fence.F sfs } ->
		SyncObjects '(sias, srfs, sfs)

createSyncObjects :: (Ord k, Vk.AllocationCallbacks.ToMiddle ma) =>
	Vk.Semaphore.Group sd ma sias k -> Vk.Semaphore.Group sd ma srfs k ->
	Vk.Fence.Group sd ma siff k -> k -> IO (SyncObjects '(sias, srfs, siff))
createSyncObjects iasgrp rfsgrp iffgrp k =
	Vk.Semaphore.create' @_ @'Nothing iasgrp k def >>= \(fromRight -> ias) ->
	Vk.Semaphore.create' @_ @'Nothing rfsgrp k def >>= \(fromRight -> rfs) ->
	Vk.Fence.create' @_ @'Nothing iffgrp k fncInfo >>= \(fromRight -> iff) ->
	pure $ SyncObjects ias rfs iff
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sl sg sm sb smr sbr nm sm' sb' nm' sdsl sds .
	Vk.CmdBffr.C scb ->
	Vk.RndrPass.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[AtomUbo sdsl], '[]) ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[VObj.List 256 Rectangle ""], Vk.Cmd.InstanceCount) ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	IO ()
recordCommandBuffer cb rp fb sce pllyt gpl vb (rb, ic) ib ubds =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb (
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @Vertex @"" vb) :**
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @Rectangle @"" rb) :**
		HeteroParList.Nil
		) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word16 @"" ib) >>
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics pllyt
		(HeteroParList.Singleton $ U2 ubds)
		(HeteroParList.Singleton (
			HeteroParList.Nil :**
			HeteroParList.Nil )) >>
	Vk.Cmd.drawIndexed cbb (fromIntegral $ length indices) ic 0 0 0
	where
	rpInfo :: Vk.RndrPass.BeginInfo 'Nothing sr sf
		'[ 'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32]
	rpInfo = Vk.RndrPass.BeginInfo {
		Vk.RndrPass.beginInfoNext = TMaybe.N,
		Vk.RndrPass.beginInfoRenderPass = rp,
		Vk.RndrPass.beginInfoFramebuffer = fb,
		Vk.RndrPass.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = sce },
		Vk.RndrPass.beginInfoClearValues = HeteroParList.Singleton
			. Vk.ClearValueColor . fromJust $ rgbaDouble 0 0 0 1 }

class Succable n where
	zero' :: n
	succ' :: n -> n

instance Succable Bool where
	zero' = False
	succ' = \case False -> True; True -> error "no more"

instance Succable Int where
	zero' = 0
	succ' = succ

mainLoop' ::
	forall n siv sf scfmt sw ssfc sd sc scb sias srfs siff ssc nm sr sg sl
		sdsl sm sb sm' sb' nm' srm srb sds sm2 sb2 k .
	(Mappable n, Vk.T.FormatToValue scfmt, Ord k, Succable k) =>
	TChan (Command k) -> TChan (Event k) -> Devices sd sc scb -> PipelineLayout sl sdsl ->

	(k -> IO (WinObjs sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm
		(Replicate n siv) sr (Replicate n sf))) ->

	VertexBuffers sm sb nm sm' sb' nm' ->
	RectGroups sd srm srb nm k ->
	UniformBuffers sds sdsl sm2 sb2 ->
	TVar k ->
	TVar (M.Map k (WinObjs sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm
		(Replicate n siv) sr (Replicate n sf))) ->
	IO ()
mainLoop' inp outp dvs pll crwos vbs rgrps ubs vwid vws = do
	let	crwos' = do
			wi <- atomically do
				i <- readTVar vwid
				i <$ modifyTVar vwid succ'
			wi <$ (atomically . modifyTVar vws . M.insert wi =<< crwos wi)
	fix \loop -> do
		GlfwG.pollEvents
		atomically (readTChan inp) >>= \case
			Draw ds -> do
				ws <- atomically $ readTVar vws
				runLoop' @n @siv @sf dvs pll ws vbs rgrps (rectsToDummy ds) ubs loop
			OpenWindow -> crwos' >>= atomically . writeTChan outp . EventOpenWindow >> loop

rectsToDummy :: M.Map k (b, [Rectangle]) -> M.Map k (b, [Rectangle])
rectsToDummy = M.map \(tm, rects) -> (tm, bool rects dummy $ null rects)

type Devices sd scp scb = (
	Vk.PhDvc.P, QueueFamilyIndices, Vk.Dvc.D sd,
	Vk.Queue.Q, Vk.Queue.Q, Vk.CmdPool.C scp, Vk.CmdBffr.C scb )

data WinObjs sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm ss sr sfs = WinObjs
	(WinEnvs sw) (Vk.Khr.Sfc.S ssfc) (TVar Vk.Extent2d)
	(Pipeline sg sl sdsl) (SyncObjects '(sias, srfs, siff))
	(Swapchains scfmt ssc nm ss sr sfs)

type WinEnvs sw = (GlfwG.Win.W sw , FramebufferResized)
type FramebufferResized = TVar Bool

type Swapchains scfmt ssc nm ss sr sfs = (
	Vk.Khr.Swapchain.S scfmt ssc,
	HeteroParList.PL (Vk.ImgVw.I nm scfmt) ss,
	Vk.RndrPass.R sr, HeteroParList.PL Vk.Frmbffr.F sfs )

type Pipeline sg sl sdsl = Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[AtomUbo sdsl], '[])

type PipelineLayout sl sdsl = Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[]

type VertexBuffers sm sb nm sm' sb' nm' = (
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""],
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] )

type UniformBuffers sds sdsl sm2 sb2 =
	(Vk.DscSet.D sds (AtomUbo sdsl), UniformBufferMemory sm2 sb2)

data Recreates sw sl nm ssfc sr sg sdsl fmt ssc sis sfs = Recreates
	(GlfwG.Win.W sw) (Vk.Khr.Sfc.S ssfc)
	(TVar Vk.Extent2d)
	(Vk.RndrPass.R sr)
	(Vk.Ppl.Graphics.G sg
		'[	'(Vertex, 'Vk.VtxInp.RateVertex),
			'(Rectangle, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[AtomUbo sdsl], '[]))
	(Vk.Khr.Swapchain.S fmt ssc)
	(HeteroParList.PL (Vk.ImgVw.I nm fmt) sis)
	(HeteroParList.PL Vk.Frmbffr.F sfs)

winObjsToRecreates ::
	WinObjs sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm sscivs sr sfs ->
	Recreates sw sl nm ssfc sr sg sdsl scfmt ssc sscivs sfs
winObjsToRecreates (WinObjs (w, _) sfc vex gpl _iasrfsifs (sc, scivs, rp, fbs)) =
	Recreates w sfc vex rp gpl sc scivs fbs

data Draws sl sr sg sdsl sias srfs siff fmt ssc sfs = Draws
	(TVar Vk.Extent2d) (Vk.RndrPass.R sr)
	(Vk.Ppl.Graphics.G sg
		'[	'(Vertex, 'Vk.VtxInp.RateVertex),
			'(Rectangle, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[AtomUbo sdsl], '[]))
	(SyncObjects '(sias, srfs, siff))
	(Vk.Khr.Swapchain.S fmt ssc)
	(HeteroParList.PL Vk.Frmbffr.F sfs)

winObjsToDraws ::
	WinObjs sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm sscivs sr sfs ->
	Draws sl sr sg sdsl sias srfs siff scfmt ssc sfs
winObjsToDraws (WinObjs _ _sfc vex gpl iasrfsifs (sc, _scivs, rp, fbs)) =
	Draws vex rp gpl iasrfsifs sc fbs

winObjsToWin ::
	WinObjs sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm sscivs sr sfs ->
	W sw
winObjsToWin (WinObjs (win, _) _ _ _ _ _) = win

runLoop' :: forall n (siv :: Type) (sf :: Type)
	sd sc scb sl
	sw ssfc sg sias srfs siff scfmt ssc sr
	smrct sbrct nmrct sds sdsl sm sb sm' sb' sm2 sb2 nm2 k .
	(Mappable n, Vk.T.FormatToValue scfmt, Ord k) =>
	Devices sd sc scb -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	(M.Map k (WinObjs sw ssfc sg sl sdsl sias srfs siff scfmt ssc nmrct
		(Replicate n siv) sr (Replicate n sf))) ->
	(	Vk.Bffr.Binded sm' sb' nmrct '[VObj.List 256 Vertex ""],
		Vk.Bffr.Binded sm2 sb2 nm2 '[VObj.List 256 Word16 ""] ) ->
	(	Vk.Bffr.Group sd 'Nothing sbrct k nmrct '[VObj.List 256 Rectangle ""],
		Vk.Mem.Group sd 'Nothing smrct k '[
			'(sbrct, 'Vk.Mem.BufferArg nmrct '[VObj.List 256 Rectangle ""])] ) ->
	M.Map k (ViewProjection, [Rectangle]) ->
	(Vk.DscSet.D sds (AtomUbo sdsl), UniformBufferMemory sm sb) ->
	IO () -> IO ()
runLoop' dvs pll ws vbs rgrps rectss ubs loop = do
	let	(phdvc, qfis, dvc, gq, pq, _cp, cb) = dvs
		(vb, ib) = vbs
		(ubds, ubm) = ubs
	for_ (M.toList ws) \(k', wos) -> do
		let	(tm, rects') = lookupRects rectss k'
		destroyRectangleBuffer rgrps k'
		rb <- createRectangleBuffer dvs rgrps k' rects'
		let	rb' = (rb, fromIntegral $ length rects')
		catchAndDraw @n @siv @sf phdvc qfis dvc gq pq pll vb rb' ib ubm ubds cb tm wos
	cls <- or <$> GlfwG.Win.shouldClose `mapM` (winObjsToWin <$> ws)
	if cls then (pure ()) else do
		for_ ws \wos ->
			recreateSwapchainEtcIfNeed @n @siv @sf phdvc qfis dvc pll wos
		loop

lookupRects :: Ord k =>
	M.Map k (ViewProjection, [Rectangle]) -> k ->
	(ViewProjection, [Rectangle])
lookupRects rs = fromMaybe (viewProjectionIdentity, dummy) . (`M.lookup` rs)

catchAndDraw ::
	forall n siv sf
		sd sl sdsl sm sb smr sbr nm sm' sb' sm2 sb2 nm' sw ssfc sg sias srfs siff win ssc sr sds scb .
	(Mappable n, Vk.T.FormatToValue win) =>
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Queue.Q -> Vk.Queue.Q -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[VObj.List 256 Rectangle ""], Vk.Cmd.InstanceCount)  ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory sm2 sb2 -> Vk.DscSet.D sds (AtomUbo sdsl) ->
	Vk.CmdBffr.C scb ->
	ViewProjection ->
	WinObjs sw ssfc sg sl sdsl sias srfs siff win ssc nm
		(Replicate n siv) sr (Replicate n sf) ->
	IO ()
catchAndDraw phdvc qfis dvc gq pq pllyt vb rb ib ubm ubds cb ubo wos = do
	catchAndRecreate @n @_ @siv @sf phdvc qfis dvc pllyt (winObjsToRecreates wos)
		$ drawFrame dvc gq pq pllyt (winObjsToDraws wos) vb rb ib ubm ubds cb ubo
	Vk.Dvc.waitIdle dvc

recreateSwapchainEtcIfNeed ::
	forall n siv sf
		sd sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm sr .
	(Vk.T.FormatToValue scfmt, Mappable n) =>
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	WinObjs sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm
		(Replicate n siv) sr (Replicate n sf) -> IO ()
recreateSwapchainEtcIfNeed phdvc qfis dvc pllyt wos@(WinObjs (_, fbrszd) _ _ _ _ _) =
	checkFlag fbrszd >>= bool (pure ())
		(recreateSwapchainEtc @n @siv @sf phdvc qfis dvc pllyt $ winObjsToRecreates wos)
	

drawFrame :: forall sfs sd ssc sr sl sg sm sb smr sbr nm sm' sb' nm' sm2 sb2 scb sias srfs siff sdsl scfmt sds .
	Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.Queue.Q ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Draws sl sr sg sdsl sias srfs siff scfmt ssc sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[VObj.List 256 Rectangle ""], Vk.Cmd.InstanceCount) ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl) ->
	Vk.CmdBffr.C scb ->
	ViewProjection -> IO ()
drawFrame dvc gq pq
	pllyt
	(Draws vext rp gpl (SyncObjects ias rfs iff) sc fbs)
	vb rb ib ubm ubds cb
	ubo = do
	let	siff = HeteroParList.Singleton iff
	ext <- atomically $ readTVar vext
	Vk.Fence.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc uint64Max (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HeteroParList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext pllyt gpl vb rb ib ubds
	updateUniformBuffer' dvc ubm ubo
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

updateUniformBuffer' :: Vk.Dvc.D sd ->
	UniformBufferMemory sm2 sb2 -> ViewProjection -> IO ()
updateUniformBuffer' dvc um obj = do
	Vk.Mem.write @"uniform-buffer" @(VObj.Atom 256 ViewProjection 'Nothing)
		dvc um zeroBits obj

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate ::
	forall n scfmt siv sf sw ssfc sd nm sr ssc sl sdsl sg .
	(Mappable n, Vk.T.FormatToValue scfmt) =>
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Recreates sw sl nm ssfc sr sg sdsl scfmt
		ssc (Replicate n siv) (Replicate n sf) ->
	IO () -> IO ()
catchAndRecreate phdvc qfis dvc pllyt rcs act = catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> recreateSwapchainEtc @n @siv @sf phdvc qfis dvc pllyt rcs

recreateSwapchainEtc :: forall
	n siv sf scfmt sw ssfc sd ssc nm sr sl sdsl sg .
	(
	Vk.T.FormatToValue scfmt, Mappable n ) =>
	Vk.PhDvc.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl] '[] ->
	Recreates sw sl nm ssfc sr sg sdsl scfmt ssc (Replicate n siv) (Replicate n sf) ->
	IO ()
recreateSwapchainEtc
	phdvc qfis dvc pllyt
	(Recreates win sfc vex rp gpl sc scivs fbs) = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwapchain win sfc phdvc qfis dvc sc
	atomically $ writeTVar vex ext
	Vk.Khr.Swapchain.getImages dvc sc >>= \imgs ->
		recreateImageViews dvc imgs scivs
	recreateGraphicsPipeline dvc ext rp pllyt gpl
	recreateFramebuffers' @n @_ @_ @_ @_ @siv @sf dvc ext rp scivs fbs

waitFramebufferSize :: GlfwG.Win.W sw -> IO ()
waitFramebufferSize win = GlfwG.Win.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		GlfwG.waitEvents *> GlfwG.Win.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

data Vertex = Vertex { vertexPos :: Cglm.Vec2, vertexColor :: Cglm.Vec3 }
	deriving (Show, Generic)

instance Storable Vertex where
	sizeOf = StrG.gSizeOf
	alignment = StrG.gAlignment
	peek = StrG.gPeek
	poke = StrG.gPoke

instance StrG.G Vertex where

data Rectangle = Rectangle {
	rectanglePos :: RectPos,
	rectangleSize :: RectSize,
	rectagnleColor :: RectColor,
	rectangleModel0 :: RectModel0,
	rectangleModel1 :: RectModel1,
	rectangleModel2 :: RectModel2,
	rectangleModel3 :: RectModel3 }
	deriving (Show, Generic)

instance StrG.G Rectangle where

instance Storable Rectangle where
	sizeOf = StrG.gSizeOf
	alignment = StrG.gAlignment
	peek = StrG.gPeek
	poke = StrG.gPoke

instance Default Rectangle where
	def = Rectangle
		(RectPos . Cglm.Vec2 $ 0 :. 0 :. NilL)
		(RectSize . Cglm.Vec2 $ 1 :. 1 :. NilL)
		(RectColor . Cglm.Vec4 $ 0 :. 0 :. 0 :. 0 :. NilL)
		def def def def

newtype RectPos = RectPos Cglm.Vec2
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype RectSize = RectSize Cglm.Vec2
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype RectColor = RectColor Cglm.Vec4
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype RectModel0 = RectModel0 Cglm.Vec4
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype RectModel1 = RectModel1 Cglm.Vec4
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype RectModel2 = RectModel2 Cglm.Vec4
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype RectModel3 = RectModel3 Cglm.Vec4
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

defaultRectModel :: (RectModel0, RectModel1, RectModel2, RectModel3)
defaultRectModel =
	let m0 :. m1 :. m2 :. m3 :. NilL = Cglm.mat4ToVec4s Cglm.mat4Identity in
		(RectModel0 m0, RectModel1 m1, RectModel2 m2, RectModel3 m3)

instance Default RectModel0 where def = let (d, _, _, _) = defaultRectModel in d
instance Default RectModel1 where def = let (_, d, _, _) = defaultRectModel in d
instance Default RectModel2 where def = let (_, _, d, _) = defaultRectModel in d
instance Default RectModel3 where def = let (_, _, _, d) = defaultRectModel in d

vertices :: [Vertex]
vertices = [
	Vertex (Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ 1 :. (- 1) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL),
	Vertex (Cglm.Vec2 $ 1 :. 1 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL),
	Vertex (Cglm.Vec2 $ (- 1) :. 1 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 1.0 :. NilL) ]

indices :: [Word16]
indices = [0, 1, 2, 2, 3, 0]

dummy :: [Rectangle]
dummy = let m0 :. m1 :. m2 :. m3 :. NilL = Cglm.mat4ToVec4s Cglm.mat4Identity in
	[Rectangle (RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
			(RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
			(RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 0.0 :. NilL)
			(RectModel0 m0) (RectModel1 m1)
			(RectModel2 m2) (RectModel3 m3)]

data ViewProjection = ViewProjection {
	viewProjectionView :: Cglm.Mat4,
	viewProjectionProj :: Cglm.Mat4 }
	deriving (Show, Generic)

viewProjectionIdentity :: ViewProjection
viewProjectionIdentity = ViewProjection {
	viewProjectionView = Cglm.mat4Identity,
	viewProjectionProj = Cglm.mat4Identity }

instance Storable ViewProjection where
	sizeOf = StrG.gSizeOf
	alignment = StrG.gAlignment
	peek = StrG.gPeek
	poke = StrG.gPoke

instance StrG.G ViewProjection

instance Default ViewProjection where
	def = ViewProjection Cglm.mat4Identity Cglm.mat4Identity

shaderModuleCreateInfo :: SpirV.S sknd -> Vk.ShaderModule.CreateInfo 'Nothing sknd
shaderModuleCreateInfo code = Vk.ShaderModule.CreateInfo {
	Vk.ShaderModule.createInfoNext = TMaybe.N,
	Vk.ShaderModule.createInfoFlags = def,
	Vk.ShaderModule.createInfoCode = code }

[glslVertexShader|

#version 450

layout(binding = 0) uniform ViewProjection {
	mat4 view;
	mat4 proj;
} ubo;

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec3 inColor;
layout(location = 2) in vec2 rectPosition;
layout(location = 3) in vec2 rectSize;
layout(location = 4) in vec4 rectColor;
layout(location = 5) in mat4 rectModel;

layout(location = 0) out vec4 fragColor;

void
main()
{
	gl_Position =
		ubo.proj * ubo.view * rectModel *
		vec4(inPosition * rectSize, 0.0, 1.0) +
		vec4(rectPosition, 0.0, 1.0);
//	fragColor = inColor;
	fragColor = rectColor;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec4 fragColor;

layout(location = 0) out vec4 outColor;

void
main()
{
	outColor = fragColor;
	if (outColor.w < 1) { discard; }
}

|]
