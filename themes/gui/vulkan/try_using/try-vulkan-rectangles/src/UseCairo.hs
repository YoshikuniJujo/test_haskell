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
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseCairo (

	-- * RECTANGLES

	rectangles2,

	-- * COMMAND

	Command(..),

	-- ** VIEW PROJECTION

	ViewProjection(..),

	-- ** RECTANGLE

	Rectangle'(..), RectPos(..), RectSize(..), RectColor(..), RectModel(..),

	-- * EVENT

	Event(..),

	-- * OTHERS

	readTVarOr

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
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.Tuple.Uncurry
import Data.Foldable
import Data.Proxy
import Data.Default
import Data.Bits
import Data.Maybe
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.List.Length
import Data.HeteroParList qualified as HPList
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
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Exception qualified as Vk
import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.AllocationCallbacks qualified as Vk.AllocationCallbacks
import Gpu.Vulkan.Instance.Internal qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPool
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPool
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscSetLyt
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSet
import Gpu.Vulkan.Memory qualified as Vk.Mem
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Semaphore qualified as Vk.Semaphore
import Gpu.Vulkan.Fence qualified as Vk.Fence
import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
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
import Gpu.Vulkan.RenderPass qualified as Vk.RndrPss
import Gpu.Vulkan.Subpass qualified as Vk.Subpass
import Gpu.Vulkan.Attachment qualified as Vk.Att
import Gpu.Vulkan.Sample qualified as Vk.Sample
import Gpu.Vulkan.ColorComponent qualified as Vk.ClrCmp

import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.Ext.DUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.Ex.DUtls.Msgr
import Gpu.Vulkan.Cglm qualified as Cglm

import Tools
import ThEnv

import Graphics.UI.GlfwG as GlfwG
import Graphics.UI.GlfwG.Window as GlfwG.Win
import Graphics.UI.GlfwG.Key as GlfwG.Ky
import Graphics.UI.GlfwG.Mouse as GlfwG.Ms

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Data.CairoImage.Internal
import Data.CairoContext
import Control.Monad.ST

import PangoLayoutExtent

import Control.Moffy
import Control.Moffy.Event.CalcTextExtents qualified as CTE

import Texture
import Gpu.Vulkan.CairoImage

import Gpu.Vulkan.Sampler qualified as Vk.Smplr

import Trial.Followbox.ViewType as FV

import Data.Time

import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC

import Data.Maybe.ToolsYj
import Data.List.ToolsYj

import Data.Ord.ToolsYj

textureSize :: Integral n => (n, n)
textureWidth, textureHeight :: Integral n => n
textureSize@(textureWidth, textureHeight) =
	(1024 :: forall n . Num n => n, 1024 :: forall n . Num n => n)

rectangles2 :: forall k . (Ord k, Show k, Succable k) =>
	TChan (Command k) -> TChan (Event k) -> TVar (M.Map k (TVar Vk.Extent2d)) -> IO ()
rectangles2 inp outp vext = GlfwG.init error $ do
	createInstance \ist ->
		Vk.Dvc.group nil \dvcgrp -> bool id (setupDebugMessenger ist)
			enableValidationLayers do
		(phd', qfis', fmt', dv', gq', pq') <-
			withWindow False \dw ->
			createSurface dw ist \dsfc -> do
			(phd, qfis) <- pickPhd ist dsfc
			(dv, gq, pq) <-
				createLogicalDevice phd dvcgrp () qfis
			spp <- querySwapChainSupport phd dsfc
			let	fmt = Vk.Khr.Sfc.formatOldFormat
					. chooseSwapSurfaceFormat
					$ formats spp
			Vk.T.formatToType fmt \(_ :: Proxy fmt) -> do
				pure (	phd, qfis,
					Vk.Khr.Sfc.formatOldFormat
						. chooseSwapSurfaceFormat
						$ formats spp, dv, gq, pq )
		Vk.T.formatToType fmt' \(_ :: Proxy fmt) ->
			run' @fmt @_ @_ @k inp outp vext
					ist phd' qfis' dv' gq' pq'
	atomically $ writeTChan outp EventEnd
	where setupDebugMessenger ist f =
		Vk.Ex.DUtls.Msgr.create ist debugMessengerCreateInfo nil f

readTVarOr :: Ord k => a -> TVar (M.Map k (TVar a)) -> k -> STM a
readTVarOr d mp k = do
	mv <- (M.lookup k) <$> readTVar mp
	case mv of
		Nothing -> pure d
		Just v -> readTVar v

getSwapchainImageNum :: forall (fmt :: Vk.T.Format) sd ssfc .
	Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Khr.Sfc.S ssfc -> SwapChainSupportDetails ->
	Vk.Extent2d -> QueueFamilyIndices -> IO [()]
getSwapchainImageNum dv sfc spp ext qfis =
	withSwapchain @fmt dv sfc spp ext qfis \sc _ ->
	sameNum () <$> Vk.Khr.Swpch.getImages dv sc

sameNum :: b -> [a] -> [b]
sameNum x = \case [] -> []; _ : ys -> x : sameNum x ys

class NumToValue (n :: [()]) where numToValue :: Int
instance NumToValue '[] where numToValue = 0

instance NumToValue n => NumToValue ('() ': n) where
	numToValue = 1 + numToValue @n

data Command k
	= Draw (M.Map k (ViewProjection, [Rectangle']))
	| Draw2 View
	| OpenWindow
	| DestroyWindow k
	| GetEvent
	| CalcTextLayoutExtent CTE.CalcTextExtents
	| EndWorld
	deriving Show

data Event k
	= EventEnd
	| EventKeyDown k GlfwG.Ky.Key
	| EventKeyUp k GlfwG.Ky.Key
	| EventMouseButtonDown k GlfwG.Ms.MouseButton
	| EventMouseButtonUp k GlfwG.Ms.MouseButton
	| EventCursorPosition k Double Double
	| EventOpenWindow k
	| EventDeleteWindow k
	| EventTextLayoutExtentResult (Occurred CTE.CalcTextExtents)
	| EventNeedRedraw
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

glfwEvents :: k -> GlfwG.Win.W sw -> TChan (Event k) -> TVar Bool -> TVar MouseButtonStateDict -> IO ()
glfwEvents k w outp vscls vmb1p = do
--	threadDelay 10000
	GlfwG.pollEvents
	cls <- GlfwG.Win.shouldClose w
	scls <- atomically $ readTVar vscls
	atomically $ writeTVar vscls cls
--	when cls . putStrLn $ "glfwEvents: scls = " ++ show scls
	when (not scls && cls) . atomically . writeTChan outp $ EventDeleteWindow k
	mb1 <- getMouseButtons w
	mb1p <- atomically $ readTVar vmb1p
	atomically $ writeTVar vmb1p mb1
	if mAny (== GlfwG.Ms.MouseButtonState'Pressed) mb1 && not cls
	then atomically . writeTChan outp . uncurry (EventCursorPosition k)
		=<< GlfwG.Ms.getCursorPos w
	else pure ()
	sendMouseButtonDown k w mb1p mb1 outp `mapM_` mouseButtonAll
	sendMouseButtonUp k w mb1p mb1 outp `mapM_` mouseButtonAll
	cls' <- GlfwG.Win.shouldClose w
	if mAny (== GlfwG.Ms.MouseButtonState'Pressed) mb1 && not cls'
	then atomically . writeTChan outp . uncurry (EventCursorPosition k)
		=<< GlfwG.Ms.getCursorPos w
	else pure ()

mouseButtonAll :: [GlfwG.Ms.MouseButton]
mouseButtonAll = [GlfwG.Ms.MouseButton'1 .. GlfwG.Ms.MouseButton'8]

sendMouseButtonDown, sendMouseButtonUp ::
	k -> GlfwG.Win.W sw -> MouseButtonStateDict -> MouseButtonStateDict -> TChan (Event k) ->
	GlfwG.Ms.MouseButton -> IO ()
sendMouseButtonDown k w = sendMouseButton k w EventMouseButtonDown
	GlfwG.Ms.MouseButtonState'Released GlfwG.Ms.MouseButtonState'Pressed

sendMouseButtonUp k w = sendMouseButton k w EventMouseButtonUp
	GlfwG.Ms.MouseButtonState'Pressed GlfwG.Ms.MouseButtonState'Released

sendMouseButton ::
	k -> GlfwG.Win.W sw ->
	(k -> GlfwG.Ms.MouseButton -> Event k) ->
	GlfwG.Ms.MouseButtonState -> GlfwG.Ms.MouseButtonState ->
	MouseButtonStateDict -> MouseButtonStateDict -> TChan (Event k) ->
	GlfwG.Ms.MouseButton -> IO ()
sendMouseButton k w ev pst st pbss bss outp b =
	case (pbss M.! b == pst, bss M.! b == st) of
		(True, True) -> do
--			print $ ev k b
			atomically . writeTChan outp $ ev k b
			cl <- GlfwG.Win.shouldClose w
			when (not cl) $
				atomically . writeTChan outp . uncurry (EventCursorPosition k)
					=<< GlfwG.Ms.getCursorPos w
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
	Vk.Ist.create (crinfo exts) nil f
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

run' :: forall (scfmt :: Vk.T.Format) si sd k . (
	Vk.T.FormatToValue scfmt, Ord k, Show k, Succable k ) =>
	TChan (Command k) -> TChan (Event k) -> TVar (M.Map k (TVar Vk.Extent2d)) ->
	Vk.Ist.I si -> Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.Q.Q -> IO ()
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
	Vk.Khr.Sfc.group ist nil \sfcgrp ->
	Vk.RndrPss.group dv nil \rpgrp ->
	Vk.Ppl.Graphics.group dv nil \gpgrp ->
	Vk.Semaphore.group dv nil \iasgrp ->
	Vk.Semaphore.group dv nil \rfsgrp ->
	Vk.Fence.group dv nil \iffgrp ->
	Vk.Khr.Swpch.group dv nil \scgrp ->
	Vk.Bffr.group dv nil \rbgrp -> Vk.Mem.group dv nil \rmgrp ->
	let	rgrps = (rbgrp, rmgrp) in

	atomically (newTVar []) >>= \ges ->

	let	crwos = winObjs @scfmt outp phd dv gq cp qfis pllyt vext_ wgrp sfcgrp rpgrp gpgrp
			rgrps iasgrp rfsgrp iffgrp scgrp ges in

	crwos zero' \wos ->

	atomically (newTVar M.empty) >>= \vws ->

	atomically (modifyTVar vws $ M.insert zero' wos) >>

	createTextureSampler phd dv \txsmplr ->

	cairoImageSurfaceCreate CairoFormatArgb32 textureWidth textureHeight >>= \crsfc ->
	cairoCreate crsfc >>= \cr ->

	createBindImg phd dv ubds txsmplr textureSize \(tximg :: Vk.Img.Binded sm si' nmt ifmt) ->
	drawViewIO crsfc cr (View []) >>= \trs -> createBufferImageForCopy phd dv trs \ibf ibfm -> do

	let
		wwww v = do
			t0 <- getCurrentTime
			trs' <- drawViewIO crsfc cr v
			t1 <- getCurrentTime
			writeBufferImage1 dv ibfm trs'
			writeBufferImage2 dv gq cp tximg ibf textureWidth textureHeight
			t2 <- getCurrentTime
			print $ t1 `diffUTCTime` t0
			print $ t2 `diffUTCTime` t1
		wwww1 v = do
			trs' <- drawViewIO crsfc cr v
			writeBufferImage1 dv ibfm trs'
		wwww2 = writeBufferImage2 dv gq cp tximg ibf textureWidth textureHeight

	wwww $ View []

	wbw <- atomically newTChan

	mainLoop @nmt inp outp dvs pllyt vbs rgrps ubs vws ges cr
		wwww1 wwww2 wbw

winObjs :: forall (scfmt :: Vk.T.Format) k
	si sd sc sw ssfc sg sl sdsl sias srfs siff ssc nm sr
	smrct sbrct nmrct nmt a . (
	Vk.T.FormatToValue scfmt, Ord k ) =>
	TChan (Event k) -> Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPool.C sc ->
	QueueFamilyIndices -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[] ->
	TVar (M.Map k (TVar Vk.Extent2d)) -> Group sw k ->
	Vk.Khr.Sfc.Group si 'Nothing ssfc k ->
	Vk.RndrPss.Group sd 'Nothing sr k ->
	Vk.Ppl.Graphics.Group sd 'Nothing sg k '[ '(
		'[	'(Vertex, 'Vk.VtxInp.RateVertex),
			'(Rectangle, 'Vk.VtxInp.RateInstance) ],
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ],
		'(sl, '[AtomUbo sdsl nmt], '[]) )] ->
	(	Vk.Bffr.Group sd 'Nothing sbrct k nmrct '[VObj.List 256 Rectangle ""],
		Vk.Mem.Group sd 'Nothing smrct k '[ '(sbrct, Vk.Mem.BufferArg nmrct '[VObj.List 256 Rectangle ""])]
		) ->
	Vk.Semaphore.Group sd 'Nothing sias k ->
	Vk.Semaphore.Group sd 'Nothing srfs k ->
	Vk.Fence.Group sd 'Nothing siff k ->
	Vk.Khr.Swpch.Group sd 'Nothing scfmt ssc k ->
	TVar [IO ()] -> k ->
	(forall svs sfs . RecreateFrmbffrs svs sfs => WinObjs
		sw ssfc sg sl sdsl nmt sias srfs siff scfmt ssc nm
		svs sr sfs -> IO a) -> IO a
winObjs outp phd dv gq cp qfis pllyt vext_
	wgrp sfcgrp rpgrp gpgrp rgrps iasgrp rfsgrp iffgrp scgrp ges k f =
	initWindow True wgrp k >>= \w ->
	let	initMouseButtonStates = foldr (uncurry M.insert) M.empty
			$ (, GlfwG.Ms.MouseButtonState'Released) <$>
				[GlfwG.Ms.MouseButton'1 .. GlfwG.Ms.MouseButton'8] in
--	forkIO (glfwEvents k w outp False initMouseButtonStates) >>
	atomically (newTVar False) >>= \vb ->
	atomically (newTVar initMouseButtonStates) >>= \vmbs ->
	atomically (modifyTVar ges (glfwEvents k w outp vb vmbs :)) >>
	atomically (newTVar NoResized) >>= \fbrszd ->
	GlfwG.Win.setKeyCallback w
		(Just \w' ky sc act mods -> do
			putStrLn $
				show w' ++ " " ++ show ky ++ " " ++
				show sc ++ " " ++ show act ++ " " ++ show mods
			case act of
				GlfwG.Ky.KeyState'Pressed ->
					atomically $ writeTChan outp $ EventKeyDown k ky
				GlfwG.Ky.KeyState'Released ->
					atomically $ writeTChan outp $ EventKeyUp k ky
				_ -> pure ()
			) >>
	GlfwG.Win.setFramebufferSizeCallback w
		(Just \_ _ _ -> atomically $ writeTVar fbrszd Resized) >>

	Vk.Khr.Sfc.Glfw.Win.create' sfcgrp k w >>= \(fromRight -> sfc) ->
--	createSwpch @_ @scfmt w sfc phd qfis dv \sc ->
	prepareSwapchain @scfmt w sfc phd >>= \(spp, ext) ->
	createSwapchain @scfmt scgrp k sfc spp ext qfis >>= \(sc :: Vk.Khr.Swpch.S scfmt ss, _) ->
	createRenderPass @scfmt rpgrp k >>= \rp ->

	createRectangleBuffer' phd dv gq cp rgrps k dummy >>

	atomically (
		newTVar (Vk.Extent2d 0 0) >>= \v ->
		writeTVar v ext >>
		v <$ modifyTVar vext_ (M.insert k v) ) >>= \vext ->

	createGraphicsPipeline gpgrp k ext rp pllyt >>= \gpl ->
	createSyncObjects iasgrp rfsgrp iffgrp k >>= \sos ->

	Vk.Khr.Swpch.getImages dv sc >>= \scis ->
	createImgVws dv scis \scivs ->
	createFrmbffrs' dv ext rp scivs \fbs ->

	let	wos = WinObjs
			(w, fbrszd) sfc vext gpl sos (sc, scivs, rp, fbs) in
	f wos

createSurface :: GlfwG.Win.W sw -> Vk.Ist.I si ->
	(forall ss . Vk.Khr.Sfc.S ss -> IO a) -> IO a
createSurface win ist f =
	Vk.Khr.Sfc.group ist nil \sfcgrp ->
	Vk.Khr.Sfc.Glfw.Win.create' sfcgrp () win >>= f . fromRight

pickPhd :: Vk.Ist.I si -> Vk.Khr.Sfc.S ss -> IO (Vk.Phd.P, QFamIndices)
pickPhd ist sfc = Vk.Phd.enumerate ist >>= \case
	[] -> error "failed to find GPUs with Gpu.Vulkan support!"
	pds -> findMaybeM suit pds >>= \case
		Nothing -> error "failed to find a suitable GPU!"
		Just pdqfi -> pure pdqfi
	where
	suit pd = ((&&) <$> espt pd <*> sa pd) >>= bool (pure Nothing) do
		qfis <- findQFams pd sfc
		querySwpchSupport pd sfc \ss -> pure . bool qfis Nothing
			$	HPListC.null (snd $ formatsNew ss) ||
				null (presentModesNew ss)
	espt pd = elemAll dvcExtensions
		. (Vk.Phd.extensionPropertiesExtensionName <$>)
		<$> Vk.Phd.enumerateExtensionProperties pd Nothing
	sa pd = Vk.Phd.featuresSamplerAnisotropy <$> Vk.Phd.getFeatures pd

querySwpchSupport :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> (forall fmts .
	Show (HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts) =>
	SwpchSupportDetails fmts -> IO a) -> IO a
querySwpchSupport pd sfc f = Vk.Khr.Sfc.Phd.getFormats pd sfc \fmts ->
	f =<< SwpchSupportDetails
		<$> Vk.Khr.Sfc.Phd.getCapabilities pd sfc
		<*> ((, fmts) <$> Vk.Khr.Sfc.Phd.getFormatsFiltered pd sfc)
		<*> Vk.Khr.Sfc.Phd.getPresentModes pd sfc

data SwpchSupportDetails fmts = SwpchSupportDetails {
	capabilitiesNew :: Vk.Khr.Sfc.Capabilities,
	formatsNew :: (
		[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
		HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ),
	presentModesNew :: [Vk.Khr.Sfc.PresentMode] }

type QueueFamilyIndices = QFamIndices

data QFamIndices = QFamIndices {
	grFam :: Vk.QFam.Index,
	prFam :: Vk.QFam.Index }

findQFams :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (Maybe QFamIndices)
findQFams pd sfc = do
	prps@((fst <$>) -> is) <- Vk.Phd.getQueueFamilyProperties pd
	mp <- listToMaybe
		<$> filterM (flip (Vk.Khr.Sfc.Phd.getSupport pd) sfc) is
	pure $ QFamIndices <$> (fst <$> L.find (grbit . snd) prps) <*> mp
	where grbit = checkBits Vk.Q.GraphicsBit . Vk.QFam.propertiesQueueFlags

dvcExtensions :: [Vk.Phd.ExtensionName]
dvcExtensions = [Vk.Khr.Swpch.extensionName]

data SwapChainSupportDetails = SwapChainSupportDetails {
	capabilities :: Vk.Khr.Sfc.Capabilities,
	formats :: [Vk.Khr.Sfc.FormatOld],
	presentModes :: [Vk.Khr.PresentMode] }

querySwapChainSupport ::
	Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO SwapChainSupportDetails
querySwapChainSupport dvc sfc = SwapChainSupportDetails
	<$> Vk.Khr.Sfc.Phd.getCapabilities dvc sfc
	<*> Vk.Khr.Sfc.Phd.getFormatsOld dvc sfc
	<*> Vk.Khr.Sfc.Phd.getPresentModes dvc sfc

createLogicalDevice :: (Ord k, Vk.AllocationCallbacks.ToMiddle ma) =>
	Vk.Phd.P -> Vk.Dvc.Group ma sd k -> k ->
	QueueFamilyIndices -> IO (Vk.Dvc.D sd, Vk.Q.Q, Vk.Q.Q)
createLogicalDevice phdvc dvcgrp k qfis =
	mkHeteroParList queueCreateInfos uniqueQueueFamilies \qs ->
	Vk.Dvc.create' phdvc dvcgrp k (createInfo qs) >>= \(fromRight -> dvc) -> do
		gq <- Vk.Dvc.getQueue dvc (grFam qfis) 0
		pq <- Vk.Dvc.getQueue dvc (prFam qfis) 0
		pure (dvc, gq, pq)
	where
	createInfo qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = def,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames =
			bool [] validationLayers enableValidationLayers,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures = Just def {
			Vk.Phd.featuresSamplerAnisotropy = True } }
	uniqueQueueFamilies = L.nub [grFam qfis, prFam qfis] -- ++ [grFam qfis]
	queueCreateInfos qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }

mkHeteroParList :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] ->
	(forall ss . HPList.ToListWithCM' WithPoked TMaybe.M ss => HPList.PL t ss -> b) -> b
mkHeteroParList _k [] f = f HPList.Nil
mkHeteroParList k (x : xs) f = mkHeteroParList k xs \xs' -> f (k x :** xs')

prepareSwapchain :: forall (scfmt :: Vk.T.Format) sw ssfc .
	Vk.T.FormatToValue scfmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	IO (SwapChainSupportDetails, Vk.Extent2d)
prepareSwapchain win sfc phdvc = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	fmt0 = Vk.T.formatToValue @scfmt
		fmt = Vk.Khr.Sfc.formatOldFormat
			. chooseSwapSurfaceFormat $ formats spp
	when (fmt0 /= fmt) $ error
		"Rectangles: prepareSwapchain format not match"
	pure (spp, ext)

withSwapchain ::
	Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Khr.Sfc.S ssfc -> SwapChainSupportDetails ->
	Vk.Extent2d -> QueueFamilyIndices ->
	(forall ssc . Vk.Khr.Swpch.S fmt ssc -> Vk.Extent2d -> IO a) -> IO a
withSwapchain dvc sfc spp ext qfis f =
	Vk.Khr.Swpch.group dvc nil \scgrp ->
--	createSwpch sfc spp ext qfis f
	uncurry f =<< createSwapchain scgrp () sfc spp ext qfis

createSwpch :: GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIndices -> Vk.Dvc.D sd -> (forall ss scfmt .
		Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swpch.S scfmt ss -> Vk.Extent2d -> IO a) -> IO a
createSwpch w sfc pd qfis dv f = querySwpchSupport pd sfc \ss -> do
	ex <- swapExtent w $ capabilitiesNew ss
	let	cps = capabilitiesNew ss
		pm = findDefault Vk.Khr.Sfc.PresentModeFifo
			(== Vk.Khr.Sfc.PresentModeMailbox) $ presentModesNew ss
	chooseSwpSfcFmt (formatsNew ss)
		\(Vk.Khr.Sfc.Format sc :: Vk.Khr.Sfc.Format fmt) ->
		Vk.Khr.Swpch.create @_ @fmt dv
			(swpchInfo sfc qfis cps sc pm ex) nil (`f` ex)

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

chooseSwpSfcFmt :: (
	[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
	HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ) ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Khr.Sfc.Format fmt -> a) -> a
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`L.find` fmts)
	$ (== Vk.Khr.Sfc.ColorSpaceSrgbNonlinear) . Vk.Khr.Sfc.formatColorSpace
chooseSwpSfcFmt (_, HPListC.Nil) _ = error "no available swap surface formats"

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
	Vk.Khr.Swpch.createInfoCompositeAlpha = Vk.Khr.Sfc.CompositeAlphaOpaqueBit,
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

createSwapchain ::
	forall (scfmt :: Vk.T.Format) ssfc sd ma ssc k . (
	Ord k, Vk.AllocationCallbacks.ToMiddle ma ) =>
	Vk.T.FormatToValue scfmt =>
	Vk.Khr.Swpch.Group sd ma scfmt ssc k -> k ->
	Vk.Khr.Sfc.S ssfc -> SwapChainSupportDetails -> Vk.Extent2d ->
	QueueFamilyIndices -> IO (Vk.Khr.Swpch.S scfmt ssc, Vk.Extent2d)
createSwapchain scgrp k sfc spp ext qfis =
	Vk.Khr.Swpch.create' @scfmt scgrp k crInfo
		>>= \(fromRight -> sc) -> pure (sc, ext)
	where crInfo = mkSwapchainCreateInfoNew sfc qfis spp ext

mkSwapchainCreateInfoNew :: Vk.Khr.Sfc.S ss -> QueueFamilyIndices ->
	SwapChainSupportDetails -> Vk.Extent2d ->
	Vk.Khr.Swpch.CreateInfo 'Nothing ss fmt
mkSwapchainCreateInfoNew sfc qfis0 spp ext =
	Vk.Khr.Swpch.CreateInfo {
		Vk.Khr.Swpch.createInfoNext = TMaybe.N,
		Vk.Khr.Swpch.createInfoFlags = def,
		Vk.Khr.Swpch.createInfoSurface = sfc,
		Vk.Khr.Swpch.createInfoMinImageCount = imgc,
		Vk.Khr.Swpch.createInfoImageColorSpace =
			Vk.Khr.Sfc.formatOldColorSpace fmt,
		Vk.Khr.Swpch.createInfoImageExtent = ext,
		Vk.Khr.Swpch.createInfoImageArrayLayers = 1,
		Vk.Khr.Swpch.createInfoImageUsage =
			Vk.Img.UsageColorAttachmentBit,
		Vk.Khr.Swpch.createInfoImageSharingMode = ism,
		Vk.Khr.Swpch.createInfoQueueFamilyIndices = qfis,
		Vk.Khr.Swpch.createInfoPreTransform =
			Vk.Khr.Sfc.capabilitiesCurrentTransform caps,
		Vk.Khr.Swpch.createInfoCompositeAlpha =
			Vk.Khr.CompositeAlphaOpaqueBit,
		Vk.Khr.Swpch.createInfoPresentMode = presentMode,
		Vk.Khr.Swpch.createInfoClipped = True,
		Vk.Khr.Swpch.createInfoOldSwapchain = Nothing }
	where
	fmt = chooseSwapSurfaceFormat $ formats spp
	presentMode = chooseSwapPresentMode $ presentModes spp
	caps = capabilities spp
	maxImgc = fromMaybe maxBound . onlyIf (> 0)
		$ Vk.Khr.Sfc.capabilitiesMaxImageCount caps
	imgc = clampOld
		(Vk.Khr.Sfc.capabilitiesMinImageCount caps + 1) 0 maxImgc
	(ism, qfis) = bool
		(Vk.SharingModeConcurrent,
			[grFam qfis0, prFam qfis0])
		(Vk.SharingModeExclusive, [])
		(grFam qfis0 == prFam qfis0)

recreateSwapchain :: Vk.T.FormatToValue scfmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Khr.Swpch.S scfmt ssc ->
	IO Vk.Extent2d
recreateSwapchain win sfc phdvc qfis0 dvc sc = do
	spp <- querySwapChainSupport phdvc sfc
	ext <- chooseSwapExtent win $ capabilities spp
	let	crInfo = mkSwapchainCreateInfoNew sfc qfis0 spp ext
	ext <$ Vk.Khr.Swpch.unsafeRecreate @'Nothing dvc crInfo nil sc

chooseSwapSurfaceFormat  :: [Vk.Khr.Sfc.FormatOld] -> Vk.Khr.Sfc.FormatOld
chooseSwapSurfaceFormat = \case
	availableFormats@(af0 : _) -> fromMaybe af0
		$ L.find preferredSwapSurfaceFormat availableFormats
	_ -> error "no available swap surface formats"

preferredSwapSurfaceFormat :: Vk.Khr.Sfc.FormatOld -> Bool
preferredSwapSurfaceFormat f =
	Vk.Khr.Sfc.formatOldFormat f == Vk.FormatB8g8r8a8Srgb &&
	Vk.Khr.Sfc.formatOldColorSpace f == Vk.Khr.ColorSpaceSrgbNonlinear

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
			(clampOld w (Vk.extent2dWidth n) (Vk.extent2dHeight n))
			(clampOld h (Vk.extent2dWidth x) (Vk.extent2dHeight x))
	where
	curExt = Vk.Khr.Sfc.capabilitiesCurrentExtent caps
	n = Vk.Khr.Sfc.capabilitiesMinImageExtent caps
	x = Vk.Khr.Sfc.capabilitiesMaxImageExtent caps

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

createRenderPass ::
	forall (scfmt :: Vk.T.Format) sd ma sr k . (
	Vk.T.FormatToValue scfmt, Ord k,
	Vk.AllocationCallbacks.ToMiddle ma ) =>
	Vk.RndrPss.Group sd ma sr k -> k -> IO (Vk.RndrPss.R sr)
createRenderPass rpgrp k =
	fromRight <$> Vk.RndrPss.create' @_ @_ @'[scfmt] rpgrp k renderPassInfo
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
	renderPassInfo = Vk.RndrPss.CreateInfo {
		Vk.RndrPss.createInfoNext = TMaybe.N,
		Vk.RndrPss.createInfoFlags = zeroBits,
		Vk.RndrPss.createInfoAttachments =
			colorAttachment :** HPList.Nil,
		Vk.RndrPss.createInfoSubpasses = [subpass],
		Vk.RndrPss.createInfoDependencies = [dependency] }

type AtomUbo s nm = '(s, '[
	'Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing],
	'Vk.DscSetLyt.Image '[ '(nm, 'Vk.T.FormatR8g8b8a8Srgb)] ])

createDescriptorSetLayout :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscSetLyt.D s '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing],
		'Vk.DscSetLyt.Image
			'[ '(nmt, 'Vk.T.FormatR8g8b8a8Srgb)] ] -> IO a) ->
	IO a
createDescriptorSetLayout dvc = Vk.DscSetLyt.create dvc layoutInfo nil
	where
	layoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing],
		'Vk.DscSetLyt.Image '[ '(nmt, 'Vk.T.FormatR8g8b8a8Srgb)]
		]
	layoutInfo = Vk.DscSetLyt.CreateInfo {
		Vk.DscSetLyt.createInfoNext = TMaybe.N,
		Vk.DscSetLyt.createInfoFlags = zeroBits,
		Vk.DscSetLyt.createInfoBindings =
			uboLayoutBinding :**
			samplerLayoutBinding :** HPList.Nil }
	uboLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing])
	uboLayoutBinding = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	samplerLayoutBinding :: Vk.DscSetLyt.Binding
		('Vk.DscSetLyt.Image '[ '(nmt, Vk.T.FormatR8g8b8a8Srgb)])
	samplerLayoutBinding = Vk.DscSetLyt.BindingImage {
		Vk.DscSetLyt.bindingImageDescriptorType =
			Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscSetLyt.bindingImageStageFlags =
			Vk.ShaderStageFragmentBit }

createPipelineLayout ::
	Vk.Dvc.D sd -> (forall sdsl sl .
		Vk.DscSetLyt.D sdsl '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing],
			'Vk.DscSetLyt.Image '[ '(nmt, 'Vk.T.FormatR8g8b8a8Srgb)] ] ->
		Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[] -> IO b) -> IO b
createPipelineLayout dvc f =
	createDescriptorSetLayout dvc \dsl ->
	let	pipelineLayoutInfo = Vk.Ppl.Layout.CreateInfo {
			Vk.Ppl.Layout.createInfoNext = TMaybe.N,
			Vk.Ppl.Layout.createInfoFlags = zeroBits,
			Vk.Ppl.Layout.createInfoSetLayouts =
				HPList.Singleton $ U2 dsl } in
	Vk.Ppl.Layout.create @'Nothing @_ @_ @'[] dvc pipelineLayoutInfo nil $ f dsl

createGraphicsPipeline :: (Ord k, Vk.AllocationCallbacks.ToMiddle mac) =>
	Vk.Ppl.Graphics.Group sd mac sg k '[ '(
		'[ '(Vertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)],
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ],
			'(sl, '[AtomUbo sdsl nmt], '[]) )] -> k ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[] -> IO (
		Vk.Ppl.Graphics.G sg
			'[ '(Vertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)]
			'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
				'(2, RectPos), '(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
				'(9, TexCoord) ]
			'(sl, '[AtomUbo sdsl nmt], '[]))
createGraphicsPipeline gpgrp k sce rp pllyt =
	Vk.Ppl.Graphics.createGs' gpgrp k Nothing (U14 pplInfo :** HPList.Nil)
			>>= \(fromRight -> (U3 gpl :** HPList.Nil)) -> pure gpl
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp pllyt

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl nmt], '[]) -> IO ()
recreateGraphicsPipeline dvc sce rp pllyt gpls = Vk.Ppl.Graphics.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HPList.Nil) nil (U3 gpls :** HPList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp pllyt

mkGraphicsPipelineCreateInfo' ::
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[] ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing,
			'[ '(Vertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
				'(2, RectPos), '(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing '(sl, '[AtomUbo sdsl nmt], '[]) sr '(sb, vs', ts', foo)
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

createFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	HPList.PL (Vk.ImgVw.I inm fmt) sis -> Vk.ImgVw.I dptnm dptfmt siv ->
	(forall sfs . RecreateFrmbffrs sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HPList.Nil _ f = f HPList.Nil
createFrmbffrs dv ex rp (v :** vs) dvw f =
	Vk.Frmbffr.create dv (frmbffrInfo ex rp v dvw) nil \fb ->
	createFrmbffrs dv ex rp vs dvw \fbs -> f (fb :** fbs)

createFrmbffrs' :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	HPList.PL (Vk.ImgVw.I inm fmt) sis ->
	(forall sfs . RecreateFrmbffrs sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs' _ _ _ HPList.Nil f = f HPList.Nil
createFrmbffrs' dv ex rp (v :** vs) f =
	Vk.Frmbffr.create dv (frmbffrInfo' ex rp v) nil \fb ->
	createFrmbffrs' dv ex rp vs \fbs -> f (fb :** fbs)

class RecreateFrmbffrs (sis :: [Type]) (sfs :: [Type]) where
	recreateFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
		HPList.PL (Vk.ImgVw.I inm fmt) sis ->
		HPList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFrmbffrs '[] '[] where
	recreateFrmbffrs _ _ _ HPList.Nil HPList.Nil = pure ()

instance RecreateFrmbffrs sis sfs =>
	RecreateFrmbffrs (si ': sis) (sf ': sfs) where
	recreateFrmbffrs dv ex rp (v :** vs) (fb :** fbs) =
		Vk.Frmbffr.unsafeRecreate dv (frmbffrInfo' ex rp v) nil fb >>
		recreateFrmbffrs dv ex rp vs fbs

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

frmbffrInfo' :: Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I inm fmt si ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(inm, fmt, si)]
frmbffrInfo' ex rp att = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = U3 att :** HPList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = ex

createCommandPool :: QueueFamilyIndices -> Vk.Dvc.D sd ->
	(forall sc . Vk.CmdPool.C sc -> IO a) -> IO a
createCommandPool qfis dvc f =
	Vk.CmdPool.create dvc poolInfo nil \cp -> f cp
	where poolInfo = Vk.CmdPool.CreateInfo {
		Vk.CmdPool.createInfoNext = TMaybe.N,
		Vk.CmdPool.createInfoFlags =
			Vk.CmdPool.CreateResetCommandBufferBit,
		Vk.CmdPool.createInfoQueueFamilyIndex = grFam qfis }

createVertexBuffer :: Vk.Phd.P ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPool.C sc -> (forall sm sb .
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
		Vk.Mem.write @"vertex-buffer" @(VObj.List 256 Vertex "") @0 dvc bm' zeroBits vertices
		copyBuffer dvc gq cp b' b
	f b

createIndexBuffer :: Vk.Phd.P ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPool.C sc -> (forall sm sb .
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
		Vk.Mem.write @"vertex-buffer" @(VObj.List 256 Word16 "") @0 dvc bm' zeroBits indices
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
		Vk.Mem.write @"rectangle-buffer" @(VObj.List 256 Rectangle "") @0 dvc bm' zeroBits rs
		copyBuffer dvc gq cp b' b
	pure b

createRectangleBuffer' :: Ord k =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPool.C sc ->
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
		Vk.Mem.write @"rectangle-buffer" @(VObj.List 256 Rectangle "") @0 dvc bm' zeroBits rs
		copyBuffer dvc gq cp b' b
	pure b

destroyRectangleBuffer :: Ord k => RectGroups sd sm sb nm k -> k -> IO ()
destroyRectangleBuffer (bgrp, mgrp) k = do
	r1 <- Vk.Mem.unsafeFree mgrp k
	r2 <- Vk.Bffr.unsafeDestroy bgrp k
	case (r1, r2) of
		(Left msg, _) -> error msg
		(_, Left msg) -> error msg
		_ -> pure ()

type RectGroups sd sm sb nm k = (
	Vk.Bffr.Group sd 'Nothing sb k nm '[VObj.List 256 Rectangle ""],
	Vk.Mem.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mem.BufferArg nm '[VObj.List 256 Rectangle ""])] )

createUniformBuffer :: Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
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
createDescriptorPool dvc = Vk.DscPool.create dvc poolInfo nil
	where
	poolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = TMaybe.N,
		Vk.DscPool.createInfoFlags =
			Vk.DscPool.CreateFreeDescriptorSetBit,
		Vk.DscPool.createInfoMaxSets = 1,
		Vk.DscPool.createInfoPoolSizes = [poolSize, poolSize1] }
	poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPool.sizeDescriptorCount = 1 }
	poolSize1 = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscPool.sizeDescriptorCount = 1 }

createDescriptorSet ::
	Vk.Dvc.D sd -> Vk.DscPool.P sp -> Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 ViewProjection 'Nothing] ->
	Vk.DscSetLyt.D sdsc '[
		'Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing],
		'Vk.DscSetLyt.Image '[ '(nmt, 'Vk.T.FormatR8g8b8a8Srgb)]] ->
	(forall sds .
		Vk.DscSet.D sds '(sdsc, '[
			'Vk.DscSetLyt.Buffer '[VObj.Atom 256 ViewProjection 'Nothing],
			'Vk.DscSetLyt.Image
				'[ '(nmt, 'Vk.T.FormatR8g8b8a8Srgb)] ]) ->
		IO a) -> IO a
createDescriptorSet dvc dscp ub dscslyt f =
	Vk.DscSet.allocateDs dvc allocInfo \(HPList.Singleton dscs) -> do
	Vk.DscSet.updateDs dvc
		(HPList.Singleton . U5 $ descriptorWrite ub dscs) HPList.Nil
	f dscs
	where
	allocInfo = Vk.DscSet.AllocateInfo {
		Vk.DscSet.allocateInfoNext = TMaybe.N,
		Vk.DscSet.allocateInfoDescriptorPool = dscp,
		Vk.DscSet.allocateInfoSetLayouts =
			HPList.Singleton $ U2 dscslyt }

descriptorWrite ::
	Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 ViewProjection 'Nothing] ->
	Vk.DscSet.D sds slbts ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[ '(
		sm, sb, nm, VObj.Atom 256 ViewProjection 'Nothing, 0)]) 0
descriptorWrite ub dscs = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = dscs,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		HPList.Singleton bufferInfo }
	where bufferInfo = U5 $ Vk.Dsc.BufferInfo ub

createBufferAtom :: forall sd nm a b . Storable a => Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.UsageFlags -> Vk.Mem.PropertyFlags -> (
		forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.Atom 256 a 'Nothing] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[VObj.Atom 256 a 'Nothing] )] ->
			IO b) -> IO b
createBufferAtom p dv usg props = createBuffer p dv VObj.LengthAtom usg props

createBufferList :: forall sd nm t a . Storable t =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mem.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 t ""] ->
		Vk.Mem.M sm '[ '(
			sb,
			'Vk.Mem.BufferArg nm '[VObj.List 256 t ""] ) ] ->
		IO a) -> IO a
createBufferList p dv ln usg props =
	createBuffer p dv (VObj.LengthList ln) usg props

createBufferList' :: forall sd nm t sm sb k . (Ord k, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
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

createBuffer' :: forall sd sb nm o sm k .
	(Ord k, VObj.SizeAlignment o) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
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
		Vk.Mem.allocateBind' mgrp k (HPList.Singleton . U2 $ Vk.Mem.Buffer b)
			(allcInfo mt) >>=
			\(AlwaysRight (HPList.Singleton (U2 (Vk.Mem.BufferBinded bnd)), mem)) -> pure (bnd, mem)
	where
	bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	bffrInfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HPList.Singleton ln,
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

findMemoryType :: Vk.Phd.P -> Vk.Mem.TypeBits -> Vk.Mem.PropertyFlags ->
	IO Vk.Mem.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.Phd.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> L.find ((&&)
		<$> (`Vk.Mem.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mem.mTypePropertyFlags . snd) tps
		where tps = Vk.Phd.memoryPropertiesMemoryTypes props1

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a . Storable' a =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPool.C sc ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 a ""] ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 a ""] -> IO ()
copyBuffer dvc gq cp src dst = do
	Vk.CmdBffr.allocate
		dvc allocInfo \(cb :*. HPList.Nil) -> do
		let	submitInfo = Vk.SubmitInfo {
				Vk.submitInfoNext = TMaybe.N,
				Vk.submitInfoWaitSemaphoreDstStageMasks =
					HPList.Nil,
				Vk.submitInfoCommandBuffers =
					HPList.Singleton cb,
				Vk.submitInfoSignalSemaphores = HPList.Nil }
		Vk.CmdBffr.begin @'Nothing @'Nothing cb beginInfo do
			Vk.Cmd.copyBuffer @'[ '( '[VObj.List 256 a ""], 0, 0)] cb src dst
		Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) Nothing
		Vk.Q.waitIdle gq
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
createCommandBuffer dvc cp f = Vk.CmdBffr.allocate dvc allocInfo $ f . \(cb :*. HPList.Nil) -> cb
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

recordCommandBuffer :: forall scb sr sf sl sg sm sb smr sbr nm sm' sb' nm' sdsl sds nmt .
	Vk.CmdBffr.C scb ->
	Vk.RndrPss.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl nmt], '[]) ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[VObj.List 256 Rectangle ""], Vk.Cmd.InstanceCount) ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	Vk.DscSet.D sds (AtomUbo sdsl nmt) ->
	IO ()
recordCommandBuffer cb rp fb sce pllyt gpl vb (rb, ic) ib ubds =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb (
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @Vertex @"" vb) :**
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @Rectangle @"" rb) :**
		HPList.Nil
		) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word16 @"" ib) >>
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics pllyt
		(HPList.Singleton $ U2 ubds)
		(HPList.Singleton (
			HPList.Nil :** HPList.Nil :**
			HPList.Nil )) >>
	Vk.Cmd.drawIndexed cbb (fromIntegral $ length indices) ic 0 0 0
	where
	rpInfo :: Vk.RndrPss.BeginInfo 'Nothing sr sf
		'[ 'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32]
	rpInfo = Vk.RndrPss.BeginInfo {
		Vk.RndrPss.beginInfoNext = TMaybe.N,
		Vk.RndrPss.beginInfoRenderPass = rp,
		Vk.RndrPss.beginInfoFramebuffer = fb,
		Vk.RndrPss.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = sce },
		Vk.RndrPss.beginInfoClearValues = HPList.Singleton
			. Vk.ClearValueColor . fromJust $ rgbaDouble 0 0 0 1 }

class Succable n where zero' :: n
instance Succable Bool where zero' = False
instance Succable Int where zero' = 0

checkTChan :: TChan () -> IO Bool
checkTChan t = atomically do
	ne <- not <$> isEmptyTChan t
	when ne $ readTChan t
	pure ne

mainLoop ::
	forall nmt scfmt sw ssfc sd sc scb sias srfs siff ssc nm sr sg sl
		sdsl sm sb sm' sb' nm' srm srb sds sm2 sb2 k r svs sfs . (
	Vk.T.FormatToValue scfmt, Ord k, Show k, Succable k,
	RecreateFrmbffrs svs sfs
	) =>
	TChan (Command k) -> TChan (Event k) -> Devices sd sc scb -> PipelineLayout sl sdsl nmt ->

	VertexBuffers sm sb nm sm' sb' nm' ->
	RectGroups sd srm srb nm k ->
	UniformBuffers sds sdsl nmt sm2 sb2 ->
	TVar (M.Map k (WinObjs sw ssfc sg sl sdsl nmt sias srfs siff scfmt ssc nm
--		(Replicate n siv) sr (Replicate n sf))) ->
		svs sr sfs)) ->
	TVar [IO ()] ->
	CairoT r RealWorld ->
	(View -> IO ()) -> IO () -> TChan () ->
	IO ()
mainLoop inp outp dvs@(_, _, dvc, _, _, _, _) pll vbs rgrps ubs vws ges cr
	wwww1 wwww2 wbw = do
	wm <- atomically newTChan
	fix \loop -> do
--		GlfwG.pollEvents
		M.lookup zero' <$> atomically (readTVar vws) >>= \case
			Just (WinObjs (_, fbrszd) _ _ _ _ _) -> checkResizedState fbrszd >>= bool (pure ()) (do
				putStrLn "recreateSwapchainEtcIfNeed: needed"
				atomically $ writeTChan outp EventNeedRedraw)
			_ -> pure ()
		atomically (readTChan inp) >>= \case
			Draw ds -> do
				b <- checkTChan wbw
				when b wwww2
--				putStrLn "DRAW BEGIN"
				Vk.Dvc.waitIdle dvc
				ws <- atomically $ readTVar vws
				runLoop' @_ @_ dvs pll ws vbs rgrps
--				runLoop' @n @siv @sf dvs pll ws vbs rgrps
					(rectsToDummy $ second (rectangle'ToRectangle <$>) <$> ds) ubs outp loop
			Draw2 view -> do
				_ <- forkIO do
					b <- atomically $ isEmptyTChan wm
					when b do
						atomically $ writeTChan wm ()
						wwww1 view
						atomically $ readTChan wm
						atomically (writeTChan wbw ())
				loop
			OpenWindow -> atomically (writeTChan outp $ EventOpenWindow zero') >> loop
			DestroyWindow k -> do
				atomically do
					b <- isEmptyTChan wm
					check b
				putStrLn $ "DESTROY WINDOW: " ++ show k
				ws <- atomically $ readTVar vws
				GlfwG.pollEvents
				cls <- and <$> GlfwG.Win.shouldClose `mapM` (winObjsToWin <$> ws)
				if cls then pure () else loop
			GetEvent -> do
				atomically (readTVar ges) >>= sequence_
				loop
			CalcTextLayoutExtent
				(CTE.CalcTextExtentsReq wid fn fs tx) -> do
				ex <- getPangoLayoutExtent
					cr fn (realToFrac fs) tx
				let	PixelExtents ie le = ex
					ex' =  mkte ie le
				atomically . writeTChan outp
					. EventTextLayoutExtentResult
					$ CTE.OccCalcTextExtents wid fn fs tx ex'
				loop
			EndWorld -> pure ()
	where
	mkte ie le = CTE.TextExtents (r2r ie) (r2r le)
	r2r r = rct
		(pangoRectanglePixelX r) (pangoRectanglePixelY r)
		(pangoRectanglePixelWidth r) (pangoRectanglePixelHeight r)
	rct	(fromIntegral -> l) (fromIntegral -> t)
		(fromIntegral -> w) (fromIntegral -> h) = CTE.Rectangle l t w h

type PipelineLayout sl sdsl nmt = Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[]

type UniformBuffers sds sdsl nmt sm2 sb2 =
	(Vk.DscSet.D sds (AtomUbo sdsl nmt), UniformBufferMemory sm2 sb2)

rectsToDummy :: M.Map k (b, [Rectangle]) -> M.Map k (b, [Rectangle])
rectsToDummy = M.map \(tm, rects) -> (tm, bool rects dummy $ null rects)

type Devices sd scp scb = (
	Vk.Phd.P, QueueFamilyIndices, Vk.Dvc.D sd,
	Vk.Q.Q, Vk.Q.Q, Vk.CmdPool.C scp, Vk.CmdBffr.C scb )

data WinObjs sw ssfc sg sl sdsl nmt sias srfs siff scfmt ssc nm ss sr sfs = WinObjs
	(WinEnvs sw) (Vk.Khr.Sfc.S ssfc) (TVar Vk.Extent2d)
	(Pipeline sg sl sdsl nmt) (SyncObjects '(sias, srfs, siff))
	(Swapchains scfmt ssc nm ss sr sfs)

data FramebufferResizedState = NoResized | HalfResized | Resized deriving Show

checkResizedState :: FramebufferResized -> IO Bool
checkResizedState fbrszd = atomically $
	readTVar fbrszd >>= \case
		Resized -> writeTVar fbrszd HalfResized >> pure True
		HalfResized -> writeTVar fbrszd NoResized >> pure True
		NoResized -> pure False

type WinEnvs sw = (GlfwG.Win.W sw , FramebufferResized)
type FramebufferResized = TVar FramebufferResizedState

type Swapchains scfmt ssc nm ss sr sfs = (
	Vk.Khr.Swpch.S scfmt ssc,
	HPList.PL (Vk.ImgVw.I nm scfmt) ss,
	Vk.RndrPss.R sr, HPList.PL Vk.Frmbffr.F sfs )

type Pipeline sg sl sdsl nmt = Vk.Ppl.Graphics.G sg
		'[ '(Vertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl nmt], '[])

type VertexBuffers sm sb nm sm' sb' nm' = (
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""],
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] )

data Recreates sw sl nm ssfc sr sg sdsl nmt fmt ssc sis sfs = Recreates
	(GlfwG.Win.W sw) (Vk.Khr.Sfc.S ssfc)
	(TVar Vk.Extent2d)
	(Vk.RndrPss.R sr)
	(Vk.Ppl.Graphics.G sg
		'[	'(Vertex, 'Vk.VtxInp.RateVertex),
			'(Rectangle, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl nmt], '[]))
	(Vk.Khr.Swpch.S fmt ssc)
	(HPList.PL (Vk.ImgVw.I nm fmt) sis)
	(HPList.PL Vk.Frmbffr.F sfs)

winObjsToRecreates ::
	WinObjs sw ssfc sg sl sdsl nmt sias srfs siff scfmt ssc nm sscivs sr sfs ->
	Recreates sw sl nm ssfc sr sg sdsl nmt scfmt ssc sscivs sfs
winObjsToRecreates (WinObjs (w, _) sfc vex gpl _iasrfsifs (sc, scivs, rp, fbs)) =
	Recreates w sfc vex rp gpl sc scivs fbs

data Draws sl sr sg sdsl nmt sias srfs siff fmt ssc sfs = Draws
	(TVar Vk.Extent2d) (Vk.RndrPss.R sr)
	(Vk.Ppl.Graphics.G sg
		'[	'(Vertex, 'Vk.VtxInp.RateVertex),
			'(Rectangle, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl nmt], '[]))
	(SyncObjects '(sias, srfs, siff))
	(Vk.Khr.Swpch.S fmt ssc)
	(HPList.PL Vk.Frmbffr.F sfs)

winObjsToDraws ::
	WinObjs sw ssfc sg sl sdsl nmt sias srfs siff scfmt ssc nm sscivs sr sfs ->
	Draws sl sr sg sdsl nmt sias srfs siff scfmt ssc sfs
winObjsToDraws (WinObjs _ _sfc vex gpl iasrfsifs (sc, _scivs, rp, fbs)) =
	Draws vex rp gpl iasrfsifs sc fbs

winObjsToWin ::
	WinObjs sw ssfc sg sl sdsl nmt sias srfs siff scfmt ssc nm sscivs sr sfs ->
	W sw
winObjsToWin (WinObjs (win, _) _ _ _ _ _) = win

runLoop' :: forall sfs svs -- (sf :: Type)
	sd sc scb sl
	sw ssfc sg sias srfs siff scfmt ssc sr
	smrct sbrct nmrct sds sdsl sm sb sm' sb' sm2 sb2 nm2 k nmt . (
	Vk.T.FormatToValue scfmt, Ord k,
	RecreateFrmbffrs svs sfs
	) =>
	Devices sd sc scb -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[] ->
	(M.Map k (WinObjs sw ssfc sg sl sdsl nmt sias srfs siff scfmt ssc nmrct
		svs sr sfs)) ->
--		(Replicate n siv) sr (Replicate n sf))) ->
	(	Vk.Bffr.Binded sm' sb' nmrct '[VObj.List 256 Vertex ""],
		Vk.Bffr.Binded sm2 sb2 nm2 '[VObj.List 256 Word16 ""] ) ->
	(	Vk.Bffr.Group sd 'Nothing sbrct k nmrct '[VObj.List 256 Rectangle ""],
		Vk.Mem.Group sd 'Nothing smrct k '[
			'(sbrct, 'Vk.Mem.BufferArg nmrct '[VObj.List 256 Rectangle ""])] ) ->
	M.Map k (ViewProjection, [Rectangle]) ->
	(Vk.DscSet.D sds (AtomUbo sdsl nmt), UniformBufferMemory sm sb) ->
	TChan (Event k) ->
	IO () -> IO ()
runLoop' dvs pll ws vbs rgrps rectss ubs outp loop = do
	let	(phdvc, qfis, dvc, gq, pq, _cp, cb) = dvs
		(vb, ib) = vbs
		(ubds, ubm) = ubs
	for_ (M.toList ws) \(k', wos) -> do
		let	(tm, rects') = lookupRects rectss k'
		destroyRectangleBuffer rgrps k'
		rb <- createRectangleBuffer dvs rgrps k' rects'
		let	rb' = (rb, fromIntegral $ length rects')
		catchAndDraw @_ @_ @_ phdvc qfis dvc gq pq pll vb rb' ib ubm ubds cb tm wos
	cls <- and <$> GlfwG.Win.shouldClose `mapM` (winObjsToWin <$> ws)
	if cls then (pure ()) else do
		for_ ws \wos ->
			recreateSwapchainEtcIfNeed @_ @_ @_ phdvc qfis dvc pll wos outp
		loop

lookupRects :: Ord k =>
	M.Map k (ViewProjection, [Rectangle]) -> k ->
	(ViewProjection, [Rectangle])
lookupRects rs = fromMaybe (viewProjectionIdentity, dummy) . (`M.lookup` rs)

catchAndDraw ::
	forall svs sfs
		sd sl sdsl sm sb smr sbr nm sm' sb' sm2 sb2 nm' sw ssfc sg sias srfs siff win ssc sr sds scb nmt . (
	Vk.T.FormatToValue win,
	RecreateFrmbffrs svs sfs
	) =>
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.Q.Q -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[] ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[VObj.List 256 Rectangle ""], Vk.Cmd.InstanceCount)  ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory sm2 sb2 -> Vk.DscSet.D sds (AtomUbo sdsl nmt) ->
	Vk.CmdBffr.C scb ->
	ViewProjection ->
	WinObjs sw ssfc sg sl sdsl nmt sias srfs siff win ssc nm
		svs sr sfs ->
--		(Replicate n siv) sr (Replicate n sf) ->
	IO ()
catchAndDraw phdvc qfis dvc gq pq pllyt vb rb ib ubm ubds cb ubo wos = do
	catchAndRecreate @_ @_ @_ @_ phdvc qfis dvc pllyt (winObjsToRecreates wos)
		$ drawFrame dvc gq pq pllyt (winObjsToDraws wos) vb rb ib ubm ubds cb ubo
	Vk.Dvc.waitIdle dvc

recreateSwapchainEtcIfNeed ::
	forall svs sfs
		sd sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm sr k nmt . (
	Vk.T.FormatToValue scfmt,
	RecreateFrmbffrs svs sfs
	) =>
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[] ->
	WinObjs sw ssfc sg sl sdsl nmt sias srfs siff scfmt ssc nm
		svs sr sfs ->
--		(Replicate n siv) sr (Replicate n sf) ->
		TChan (Event k) -> IO ()
recreateSwapchainEtcIfNeed phdvc qfis dvc pllyt wos@(WinObjs (_, fbrszd) _ _ _ _ _) outp =
--	checkFlag fbrszd >>= bool (pure ()) (do
	checkResizedState fbrszd >>= bool (pure ()) (do
		putStrLn "recreateSwapchainEtcIfNeed: needed"
		atomically $ writeTChan outp EventNeedRedraw
		recreateSwapchainEtc @_ @_ @_ phdvc qfis dvc pllyt $ winObjsToRecreates wos)
	

drawFrame :: forall sfs sd ssc sr sl sg sm sb smr sbr nm sm' sb' nm' sm2 sb2 scb sias srfs siff sdsl scfmt sds nmt .
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[] ->
	Draws sl sr sg sdsl nmt sias srfs siff scfmt ssc sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List 256 Vertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[VObj.List 256 Rectangle ""], Vk.Cmd.InstanceCount) ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List 256 Word16 ""] ->
	UniformBufferMemory sm2 sb2 ->
	Vk.DscSet.D sds (AtomUbo sdsl nmt) ->
	Vk.CmdBffr.C scb ->
	ViewProjection -> IO ()
drawFrame dvc gq pq
	pllyt
	(Draws vext rp gpl (SyncObjects ias rfs iff) sc fbs)
	vb rb ib ubm ubds cb
	ubo = do
	let	siff = HPList.Singleton iff
	ext <- atomically $ readTVar vext
	Vk.Fence.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc maxBound (Just ias) Nothing
	Vk.Fence.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HPList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext pllyt gpl vb rb ib ubds
	updateUniformBuffer' dvc ubm ubo
--	let	submitInfo :: Vk.SubmitInfo 'Nothing '[ssm, sias] '[scb] '[ssm, srfs]
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks =
--				(Vk.SemaphorePipelineStageFlags smp
--					Vk.Ppl.StageColorAttachmentOutputBit) :**
				HPList.Singleton
					(Vk.SemaphorePipelineStageFlags ias
						Vk.Ppl.StageColorAttachmentOutputBit),
			Vk.submitInfoCommandBuffers = HPList.Singleton cb,
--			Vk.submitInfoSignalSemaphores = smp :** HPList.Singleton rfs }
			Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
		presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphores = HPList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = HPList.Singleton
				$ Vk.Khr.SwapchainImageIndex sc imgIdx }
	Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) $ Just iff
--	putStrLn "BEFORE QUEUE PRESENT"
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfo
--	putStrLn "AFTER QUEUE PRESENT"
	Vk.Fence.waitForFs dvc siff True Nothing
--	putStrLn "AFTER WAIT FOR FENCES"
--	Vk.Q.waitIdle pq

updateUniformBuffer' :: Vk.Dvc.D sd ->
	UniformBufferMemory sm2 sb2 -> ViewProjection -> IO ()
updateUniformBuffer' dvc um obj = do
	Vk.Mem.write @"uniform-buffer" @(VObj.Atom 256 ViewProjection 'Nothing) @0
		dvc um zeroBits obj

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate ::
	forall scfmt sfs svs sw ssfc sd nm sr ssc sl sdsl sg nmt . (
	Vk.T.FormatToValue scfmt,
	RecreateFrmbffrs svs sfs
	) =>
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[] ->
	Recreates sw sl nm ssfc sr sg sdsl nmt scfmt
		ssc svs sfs ->
--		ssc (Replicate n siv) (Replicate n sf) ->
	IO () -> IO ()
catchAndRecreate phdvc qfis dvc pllyt rcs act = catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> do
		putStrLn "catchAndRecreate: catched"
		recreateSwapchainEtc @_ phdvc qfis dvc pllyt rcs

recreateSwapchainEtc :: forall
	svs sfs scfmt sw ssfc sd ssc nm sr sl sdsl sg nmt .
	(
	RecreateFrmbffrs svs sfs,
	Vk.T.FormatToValue scfmt) =>
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl nmt] '[] ->
	Recreates sw sl nm ssfc sr sg sdsl nmt scfmt ssc svs sfs ->
	IO ()
recreateSwapchainEtc
	phdvc qfis dvc pllyt
	(Recreates win sfc vex rp gpl sc scivs fbs) = do
	putStrLn "recreateSwapchainEtc begin"
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwapchain win sfc phdvc qfis dvc sc
	atomically $ writeTVar vex ext
	Vk.Khr.Swpch.getImages dvc sc >>= \imgs ->
		recreateImgVws dvc imgs scivs
	recreateGraphicsPipeline dvc ext rp pllyt gpl
	recreateFrmbffrs dvc ext rp scivs fbs
	putStrLn "recreateSwapchainEtc end"

waitFramebufferSize :: GlfwG.Win.W sw -> IO ()
waitFramebufferSize win = GlfwG.Win.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		GlfwG.waitEvents *> GlfwG.Win.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

createTextureSampler ::
	Vk.Phd.P -> Vk.Dvc.D sd -> (forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTextureSampler phdv dvc f = do
	prp <- Vk.Phd.getProperties phdv
	print . Vk.Phd.limitsMaxSamplerAnisotropy $ Vk.Phd.propertiesLimits prp
	let	samplerInfo = Vk.Smplr.CreateInfo {
			Vk.Smplr.createInfoNext = TMaybe.N,
			Vk.Smplr.createInfoFlags = zeroBits,
			Vk.Smplr.createInfoMagFilter = Vk.FilterLinear,
			Vk.Smplr.createInfoMinFilter = Vk.FilterLinear,
			Vk.Smplr.createInfoMipmapMode =
				Vk.Smplr.MipmapModeLinear,
			Vk.Smplr.createInfoAddressModeU =
				Vk.Smplr.AddressModeRepeat,
			Vk.Smplr.createInfoAddressModeV =
				Vk.Smplr.AddressModeRepeat,
			Vk.Smplr.createInfoAddressModeW =
				Vk.Smplr.AddressModeRepeat,
			Vk.Smplr.createInfoMipLodBias = 0,
			Vk.Smplr.createInfoAnisotropyEnable = True,
			Vk.Smplr.createInfoMaxAnisotropy =
				Vk.Phd.limitsMaxSamplerAnisotropy
					$ Vk.Phd.propertiesLimits prp,
			Vk.Smplr.createInfoCompareEnable = False,
			Vk.Smplr.createInfoCompareOp = Vk.CompareOpAlways,
			Vk.Smplr.createInfoMinLod = 0,
			Vk.Smplr.createInfoMaxLod = 0,
			Vk.Smplr.createInfoBorderColor =
				Vk.BorderColorIntOpaqueBlack,
			Vk.Smplr.createInfoUnnormalizedCoordinates = False }
	Vk.Smplr.create @'Nothing dvc samplerInfo nil f

data Vertex = Vertex {
	vertexPos :: Cglm.Vec2, vertexColor :: Cglm.Vec3,
	vertexTexCoord :: TexCoord }
	deriving (Show, Generic)

instance Storable Vertex where
	sizeOf = StrG.gSizeOf
	alignment = StrG.gAlignment
	peek = StrG.gPeek
	poke = StrG.gPoke

instance StrG.G Vertex where

newtype TexCoord = TexCoord Cglm.Vec2
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

data Rectangle = Rectangle {
	rectanglePos :: RectPos,
	rectangleSize :: RectSize,
	rectangleColor :: RectColor,
	rectangleModel0 :: RectModel0,
	rectangleModel1 :: RectModel1,
	rectangleModel2 :: RectModel2,
	rectangleModel3 :: RectModel3 }
	deriving (Show, Generic)

data Rectangle' = Rectangle' {
	rectanglePos' :: RectPos,
	rectangleSize' :: RectSize,
	rectangleColor' :: RectColor,
	rectangleModel' :: RectModel }
	deriving (Show, Generic)

rectangle'ToRectangle :: Rectangle' -> Rectangle
rectangle'ToRectangle Rectangle' {
	rectanglePos' = p,
	rectangleSize' = s,
	rectangleColor' = c,
	rectangleModel' = RectModel m } = Rectangle {
	rectanglePos = p,
	rectangleSize = s,
	rectangleColor = c,
	rectangleModel0 = RectModel0 m0,
	rectangleModel1 = RectModel1 m1,
	rectangleModel2 = RectModel2 m2,
	rectangleModel3 = RectModel3 m3 }
	where m0 :. m1 :. m2 :. m3 :. NilL = Cglm.mat4ToVec4s m

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

newtype RectModel = RectModel Cglm.Mat4 deriving (Show, Eq, Ord, Storable)

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
	Vertex (Cglm.Vec2 $ (- 0) :. (- 0) :. NilL)
		(Cglm.Vec3 $ 1.0 :. 0.0 :. 0.0 :. NilL)
		(TexCoord . Cglm.Vec2 $ 0 :. 0 :. NilL),
	Vertex (Cglm.Vec2 $ 1 :. (- 0) :. NilL)
		(Cglm.Vec3 $ 0.0 :. 1.0 :. 0.0 :. NilL)
		(TexCoord . Cglm.Vec2 $ 1 :. 0 :. NilL),
	Vertex (Cglm.Vec2 $ 1 :. 1 :. NilL)
		(Cglm.Vec3 $ 0.0 :. 0.0 :. 1.0 :. NilL)
		(TexCoord . Cglm.Vec2 $ 1 :. 1 :. NilL),
	Vertex (Cglm.Vec2 $ (- 0) :. 1 :. NilL)
		(Cglm.Vec3 $ 1.0 :. 1.0 :. 1.0 :. NilL)
		(TexCoord . Cglm.Vec2 $ 0 :. 1 :. NilL) ]

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

layout(location = 9) in vec2 inTexCoord;

layout(location = 0) out vec4 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void
main()
{
	gl_Position =
//		ubo.proj * ubo.view * rectModel *
		ubo.proj * ubo.view *
		rectModel * (
		vec4(inPosition * rectSize, 0.0, 1.0) +
		vec4(rectPosition, 0.0, 1.0) );
//	fragColor = inColor;
	fragColor = rectColor;
	fragTexCoord = inTexCoord;
}

|]

[glslFragmentShader|

#version 450

layout(location = 0) in vec4 fragColor;
layout(location = 1) in vec2 fragTexCoord;

layout(location = 0) out vec4 outColor;

layout(binding = 1) uniform sampler2D texSampler;

void
main()
{
//	outColor = fragColor;
	outColor = vec4(texture(texSampler, fragTexCoord).rgb, 1.0);
	if (outColor.w < 1) { discard; }
}

|]
