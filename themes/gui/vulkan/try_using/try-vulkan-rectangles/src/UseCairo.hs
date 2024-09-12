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

	useCairo,

	-- * COMMAND AND EVENT

	Command(..), Event(..),

	-- ** VIEW PROJECTION AND RECTANGLE

	ViewProj(..), RectModel(..),
	Rectangle(..), RectPos(..), RectSize(..), RectColor(..) ) where

import GHC.Generics
import GHC.TypeNats
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
import Data.Proxy
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.Tuple.Uncurry
import Data.Default
import Data.Bits
import Data.Maybe
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.List.Length
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Bool
import Data.Bool.ToolsYj
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
import Gpu.Vulkan.Instance.Internal qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt
import Gpu.Vulkan.Memory qualified as Vk.Mm
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

import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.Ex.DUtls.Msgr
import Gpu.Vulkan.Cglm qualified as Cglm

import Tools hiding (onlyIf)

import Graphics.UI.GLFW as Glfw
import Graphics.UI.GlfwG as GlfwG
import Graphics.UI.GlfwG.Window as GlfwG.Win
import Graphics.UI.GlfwG.Key as GlfwG.Ky

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Data.CairoImage.Internal

import Texture
import Gpu.Vulkan.CairoImage

import Gpu.Vulkan.Sampler qualified as Vk.Smplr

import Trial.Followbox.ViewType qualified as FV

import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC

import Data.Maybe.ToolsYj
import Data.List.ToolsYj

import Data.Ord.ToolsYj

import Debug

textureSize :: Integral n => (n, n)
textureWidth, textureHeight :: Integral n => n
textureSize@(textureWidth, textureHeight) =
	(1024 :: forall n . Num n => n, 1024 :: forall n . Num n => n)

-- USE CAIRO LOOP

useCairo :: TChan Command -> TChan Event -> TVar Vk.Extent2d -> IO ()
useCairo ip op vex = GlfwG.init error $ forkIO (controller op) >>
	createIst \ist -> bool id (dbgm ist) debug $
	createWin \w -> Vk.Khr.Sfc.Glfw.Win.create ist w nil \sfc ->
	pickPhd ist sfc >>= \(pd, qfis) -> createLgDvc pd qfis \dv gq pq ->
	run ip op vex w sfc pd qfis dv gq pq >>
	atomically (writeTChan op EventEnd)
	where dbgm i = Vk.Ex.DUtls.Msgr.create i dbgMsngrInfo nil

data Command
	= DrawRect (ViewProj, [Rectangle]) | SetViewAsTexture FV.View
	| GetEvent | EndWorld
	deriving Show

data Event
	= EventNeedRedraw | EventDeleteWindow | EventEnd
	| EventKeyDown GlfwG.Ky.Key | EventKeyUp GlfwG.Ky.Key
	| EventKeyRepeating GlfwG.Ky.Key
	| EventGamepadAxisLeftX Float | EventGamepadButtonAPressed
	deriving Show

controller :: TChan Event -> IO ()
controller op = fix \go -> (>> go) . (threadDelay 10000 >>)
	$ Glfw.getGamepadState Glfw.Joystick'1 >>= \case
		Nothing -> pure ()
		Just (Glfw.GamepadState gb ga) -> do
			when (btna == Glfw.GamepadButtonState'Pressed)
				. atomically
				$ writeTChan op EventGamepadButtonAPressed
			when (abs leftx > 0.1) . atomically
				. writeTChan op $ EventGamepadAxisLeftX leftx
			where
			btna = gb Glfw.GamepadButton'A
			leftx = ga Glfw.GamepadAxis'LeftX

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

dbgMsngrInfo :: Vk.Ex.DUtls.Msgr.CreateInfo 'Nothing '[] ()
dbgMsngrInfo = Vk.Ex.DUtls.Msgr.CreateInfo {
	Vk.Ex.DUtls.Msgr.createInfoNext = TMaybe.N,
	Vk.Ex.DUtls.Msgr.createInfoFlags = zeroBits,
	Vk.Ex.DUtls.Msgr.createInfoMessageSeverity =
		Vk.DbgUtls.MessageSeverityVerboseBit .|.
		Vk.DbgUtls.MessageSeverityWarningBit .|.
		Vk.DbgUtls.MessageSeverityErrorBit,
	Vk.Ex.DUtls.Msgr.createInfoMessageType =
		Vk.DbgUtls.MessageTypeGeneralBit .|.
		Vk.DbgUtls.MessageTypeValidationBit .|.
		Vk.DbgUtls.MessageTypePerformanceBit,
	Vk.Ex.DUtls.Msgr.createInfoFnUserCallback = dbgCallback,
	Vk.Ex.DUtls.Msgr.createInfoUserData = Nothing }
	where dbgCallback _svr _tp d _ud = False <$ Txt.putStrLn
		("validation layer: " <> Vk.Ex.DUtls.Msgr.callbackDataMessage d)

createWin :: (forall sw . GlfwG.Win.W sw -> IO a) -> IO a
createWin f = do
	GlfwG.Win.hint
		$ GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI
	GlfwG.Win.hint $ GlfwG.Win.WindowHint'Visible True
	GlfwG.Win.create 800 600 "USE CAIRO" Nothing Nothing f

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
	qinfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }
	info qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames = bool [] vldLayers debug,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures = Just def {
			Vk.Phd.featuresSamplerAnisotropy = True } }

run :: forall sw ssfc sd .
	TChan Command -> TChan Event -> TVar Vk.Extent2d ->
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QueueFamilyIndices -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> IO ()
run ip op vex w sfc pd qfis dv gq pq =
	createCmdPl qfis dv \cp -> createCmdBffr dv cp \cb ->
	unfrmBffrOstAlgn pd \(_ :: Proxy alu) ->
	createPplLyt @_ @alu dv \dsl pl ->
	createViewProjBffr pd dv \vp vpm ->
	createDscPl dv \dp -> createDscSt dv dp vp dsl \ds ->
	createVtxBffr pd dv gq cp vertices \vb ->
	createIdxBffr pd dv gq cp indices \ib ->

	Vk.Bffr.group dv nil \rbg -> Vk.Mm.group dv nil \rmg ->
	winObjs op w sfc pd dv gq cp qfis pl vex (rbg, rmg) \wos ->

	createTextureSampler pd dv \txsmplr ->
	createBindImg pd dv ds txsmplr textureSize
		\(tximg :: Vk.Img.Binded sm si nmt ifmt) ->
	createBffrImg pd dv textureWidth textureHeight \ibf ibfm ->
	cairoImageSurfaceCreate
		CairoFormatArgb32 textureWidth textureHeight >>= \crsfc ->
	cairoCreate crsfc >>= \cr ->
	let	viewToBffr = (writeBffr dv ibfm =<<) . drawViewIO crsfc cr
		bffrToImg = flashImg dv gq cp tximg ibf textureSize in
	viewToBffr (FV.View []) >> bffrToImg >>
	mainLoop @nmt ip op (pd, qfis, dv, gq, pq, cp, cb)
		pl (vb, ib) (rbg, rmg) (ds, vpm) wos viewToBffr bffrToImg

createCmdPl :: QueueFamilyIndices ->
	Vk.Dvc.D sd -> (forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfis dv = Vk.CmdPl.create dv info nil
	where info = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = grFam qfis }

createCmdBffr :: forall sd scp a .
	Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb . Vk.CBffr.C scb -> IO a) -> IO a
createCmdBffr dv cp f = Vk.CBffr.allocate dv info $ f . \(cb :*. HPList.Nil) -> cb
	where
	info :: Vk.CBffr.AllocateInfo 'Nothing scp '[ '()]
	info = Vk.CBffr.AllocateInfo {
		Vk.CBffr.allocateInfoNext = TMaybe.N,
		Vk.CBffr.allocateInfoCommandPool = cp,
		Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

unfrmBffrOstAlgn ::
	Vk.Phd.P -> (forall a . KnownNat a => Proxy a -> IO b) -> IO b
unfrmBffrOstAlgn pd f = (\(SomeNat p) -> f p) . someNatVal . fromIntegral
	. Vk.Phd.limitsMinUniformBufferOffsetAlignment . Vk.Phd.propertiesLimits
	=<< Vk.Phd.getProperties pd

createPplLyt :: forall sd alu nmt a . Vk.Dvc.D sd -> (forall sl sdsl .
	Vk.DscStLyt.D sdsl (DscStLytArg alu nmt) ->
	Vk.Ppl.Layout.P sl '[ '(sdsl, DscStLytArg alu nmt)] '[] -> IO a) -> IO a
createPplLyt dv f = createDscStLyt dv \dsl ->
	Vk.Ppl.Layout.create @_ @_ @_ @'[] dv (info dsl) nil $ f dsl
	where info dsl = Vk.Ppl.Layout.CreateInfo {
		Vk.Ppl.Layout.createInfoNext = TMaybe.N,
		Vk.Ppl.Layout.createInfoFlags = zeroBits,
		Vk.Ppl.Layout.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

createDscStLyt :: Vk.Dvc.D sd ->
	(forall (s :: Type) . Vk.DscStLyt.D s (DscStLytArg alu nmt) -> IO a) ->
	IO a
createDscStLyt dv = Vk.DscStLyt.create dv info nil
	where
	info = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = mbd :** tbd :** HPList.Nil }
	mbd = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	tbd = Vk.DscStLyt.BindingImage {
		Vk.DscStLyt.bindingImageDescriptorType =
			Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscStLyt.bindingImageStageFlags =
			Vk.ShaderStageFragmentBit }

type DscStLytArg alu nmt = '[
	'Vk.DscStLyt.Buffer '[AtomViewProj alu],
	'Vk.DscStLyt.Image '[ '(nmt, 'Vk.T.FormatR8g8b8a8Srgb)] ]

type AtomViewProj alu = VObj.Atom alu ViewProj 'Nothing

createViewProjBffr :: KnownNat alu => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb nmvp '[VObj.Atom alu ViewProj 'Nothing]  ->
	UniformBufferMemory sm sb nmvp alu -> IO a) -> IO a
createViewProjBffr pd dv = createBffrAtm pd dv
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

createDscPl :: Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDscPl dv = Vk.DscPl.create dv info nil
	where
	info = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = 1,
		Vk.DscPl.createInfoPoolSizes = [sz0, sz1] }
	sz0 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPl.sizeDescriptorCount = 1 }
	sz1 = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscPl.sizeDescriptorCount = 1 }

createDscSt :: forall sd sp sm sb nm alu sdsl nmt a . KnownNat alu =>
	Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.Bffr.Binded sm sb nm '[AtomViewProj alu] ->
	Vk.DscStLyt.D sdsl (DscStLytArg alu nmt) ->
	(forall sds . Vk.DscSt.D sds '(sdsl, DscStLytArg alu nmt) -> IO a) ->
	IO a
createDscSt dv dp vpb dsl f =
	Vk.DscSt.allocateDs dv info \(HPList.Singleton ds) ->
	Vk.DscSt.updateDs dv (HPList.Singleton . U5 $ wr ds) HPList.Nil >> f ds
	where
	info = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }
	wr :: Vk.DscSt.D sds slbts -> Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgBuffer
			'[ '(sm, sb, nm, AtomViewProj alu, 0)]) 0
	wr ds = Vk.DscSt.Write {
		Vk.DscSt.writeNext = TMaybe.N,
		Vk.DscSt.writeDstSet = ds,
		Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscSt.writeSources = Vk.DscSt.BufferInfos
			. HPList.Singleton . U5 $ Vk.Dsc.BufferInfo vpb }

createVtxBffr :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	[WVertex] -> (forall sm sb al . KnownNat al => Vk.Bffr.Binded sm sb bnm
		'[VObj.List al WVertex lnm] -> IO a) -> IO a
createVtxBffr = createBffrMem Vk.Bffr.UsageVertexBufferBit

createIdxBffr :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	[Word16] -> (forall sm sb al . KnownNat al => Vk.Bffr.Binded sm sb nm
		'[VObj.List al Word16 lnm] -> IO a) -> IO a
createIdxBffr = createBffrMem Vk.Bffr.UsageIndexBufferBit

createBffrMem :: forall sd sc nm t lnm a . Storable' t =>
	Vk.Bffr.UsageFlags -> Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> [t] ->
	(forall sm sb al . KnownNat al => Vk.Bffr.Binded sm sb nm
		'[VObj.List al t lnm] -> IO a) -> IO a
createBffrMem us pd dv gq cp xs@(fromIntegral . length -> ln) f =
	bffrAlgn @(VObj.List 256 t lnm) dv (VObj.LengthList ln)
		(Vk.Bffr.UsageTransferDstBit .|. us) \(_ :: Proxy al) ->
	createBffrLst pd dv ln (Vk.Bffr.UsageTransferDstBit .|. us)
		Vk.Mm.PropertyDeviceLocalBit \b _ -> do
		createBffrLst pd dv ln
			Vk.Bffr.UsageTransferSrcBit (
			Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit ) \
			(b' :: Vk.Bffr.Binded sm sb bnm' '[VObj.List al t lnm'])
			bm' -> do
			Vk.Mm.write
				@bnm' @(VObj.List al t lnm') @0 dv bm' zeroBits xs
			copyBffrLst dv gq cp b' b
		f b

copyBffrLst :: forall sd sc sm sb bnm al t lnm sm' sb' bnm' . (KnownNat al, Storable' t) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb bnm '[VObj.List al t lnm] ->
	Vk.Bffr.Binded sm' sb' bnm' '[VObj.List al t lnm] -> IO ()
copyBffrLst dv gq cp s d = singleTimeCmds dv gq cp \cb ->
	Vk.Cmd.copyBuffer @'[ '( '[VObj.List al t lnm], 0, 0)] cb s d

winObjs :: forall sw ssfc sd sc sl sdsl nm smrct sbrct nmrct nmt a alu .
	TChan Event ->
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C sc ->
	QueueFamilyIndices -> Vk.Ppl.Layout.P sl '[ '(sdsl, DscStLytArg alu nmt)] '[] ->
	TVar Vk.Extent2d ->
	(	Vk.Bffr.Group sd 'Nothing sbrct () nmrct '[VObj.List 256 RectangleRaw ""],
		Vk.Mm.Group sd 'Nothing smrct () '[ '(sbrct, Vk.Mm.BufferArg nmrct '[VObj.List 256 RectangleRaw ""])]
		) ->
	(forall sr sg scfmt ssc svs sfs sias srfs siff .
		(Vk.T.FormatToValue scfmt, RecreateFrmbffrs svs sfs) => WinObjs
		sw ssfc sg sl sdsl alu nmt sias srfs siff scfmt ssc nm
		svs sr sfs -> IO a) -> IO a
winObjs outp w sfc phd dv gq cp qfis pllyt vext_ rgrps f =
	atomically (newTVar NoResized) >>= \fbrszd ->
	GlfwG.Win.setKeyCallback w
		(Just \_w ky _sc act _mds -> do
			case act of
				GlfwG.Ky.KeyState'Pressed ->
					atomically $ writeTChan outp $ EventKeyDown ky
				GlfwG.Ky.KeyState'Released ->
					atomically $ writeTChan outp $ EventKeyUp ky
				GlfwG.Ky.KeyState'Repeating ->
					atomically $ writeTChan outp $ EventKeyRepeating ky
			) >>
	GlfwG.Win.setFramebufferSizeCallback w
		(Just \_ _ _ -> atomically $ writeTVar fbrszd Resized) >>

	createSwpch w sfc phd qfis dv \(sc :: Vk.Khr.Swpch.S scfmt ss) ext ->
	createRenderPass @_ @scfmt dv \rp ->

	createRectangleBuffer' phd dv gq cp rgrps () dummyRaw >>

	atomically (writeTVar vext_ ext) >>

	createGraphicsPipeline dv ext rp pllyt \gpl ->
	createSyncObjects dv \sos ->

	Vk.Khr.Swpch.getImages dv sc >>= \scis ->
	createImgVws dv scis \scivs ->
	createFrmbffrs dv ext rp scivs \fbs ->

	let	wos = WinObjs
			(w, fbrszd) sfc vext_ gpl sos (sc, scivs, rp, fbs) in
	f wos

data WinObjs
	sw ssfc sg sl sdsl alu nmt sias srfs siff scfmt ssc nm sivs sr sfs =
	WinObjs	(WinEnvs sw) (Vk.Khr.Sfc.S ssfc) (TVar Vk.Extent2d)
		(Pipeline sg sl sdsl alu nmt) (SyncObjects '(sias, srfs, siff))
		(Swapchains scfmt ssc nm sivs sr sfs)

type WinEnvs sw = (GlfwG.Win.W sw , FramebufferResized)
type FramebufferResized = TVar FramebufferResizedState

glfwEvents :: GlfwG.Win.W sw -> TChan Event -> IO ()
glfwEvents w outp = do
	GlfwG.pollEvents
	cls <- GlfwG.Win.shouldClose w
	when cls . atomically $ writeTChan outp EventDeleteWindow

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

querySwpchSupportFmt :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (SwpchSupportDetailsFmt fmt)
querySwpchSupportFmt dvc sfc = SwpchSupportDetailsFmt
	<$> Vk.Khr.Sfc.Phd.getCapabilities dvc sfc
	<*> Vk.Khr.Sfc.Phd.getFormatsFiltered dvc sfc
	<*> Vk.Khr.Sfc.Phd.getPresentModes dvc sfc

data SwpchSupportDetailsFmt fmt = SwpchSupportDetailsFmt {
	capabilitiesFmt :: Vk.Khr.Sfc.Capabilities,
	formatsFmt :: [Vk.Khr.Sfc.Format fmt],
	presentModesFmt :: [Vk.Khr.Sfc.PresentMode] } deriving Show

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
	forall sd (scfmt :: Vk.T.Format) a . (
	Vk.T.FormatToValue scfmt ) =>
	Vk.Dvc.D sd  ->
	(forall sr . Vk.RndrPss.R sr -> IO a) -> IO a
createRenderPass dv = Vk.RndrPss.create dv renderPassInfo nil
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

createGraphicsPipeline ::
	Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu nmt] '[] ->
	(forall sg . Pipeline sg sl sdsl alu nmt -> IO a) -> IO a
createGraphicsPipeline dv sce rp pllyt f =
	Vk.Ppl.Graphics.createGs dv Nothing (U14 pplInfo :** HPList.Nil) nil
			\(U3 gpl :** HPList.Nil) -> f gpl
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp pllyt

type Pipeline sg sl sdsl alu nmt = Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(RectangleRaw, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl alu nmt], '[])

type AtomUbo s alu nm = '(s, '[
	'Vk.DscStLyt.Buffer '[VObj.Atom alu ViewProj 'Nothing],
	'Vk.DscStLyt.Image '[ '(nm, 'Vk.T.FormatR8g8b8a8Srgb)] ])

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu nmt] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(RectangleRaw, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl alu nmt], '[]) -> IO ()
recreateGraphicsPipeline dvc sce rp pllyt gpls = Vk.Ppl.Graphics.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HPList.Nil) nil (U3 gpls :** HPList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp pllyt

mkGraphicsPipelineCreateInfo' ::
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu nmt] '[] ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing,
			'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(RectangleRaw, 'Vk.VtxInp.RateInstance)],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
				'(2, RectPos), '(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing '(sl, '[AtomUbo sdsl alu nmt], '[]) sr '(sb, vs', ts', foo)
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
	HPList.PL (Vk.ImgVw.I inm fmt) sis ->
	(forall sfs . RecreateFrmbffrs sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HPList.Nil f = f HPList.Nil
createFrmbffrs dv ex rp (v :** vs) f =
	Vk.Frmbffr.create dv (frmbffrInfo ex rp v) nil \fb ->
	createFrmbffrs dv ex rp vs \fbs -> f (fb :** fbs)

class RecreateFrmbffrs (sis :: [Type]) (sfs :: [Type]) where
	recreateFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
		HPList.PL (Vk.ImgVw.I inm fmt) sis ->
		HPList.PL Vk.Frmbffr.F sfs -> IO ()

instance RecreateFrmbffrs '[] '[] where
	recreateFrmbffrs _ _ _ HPList.Nil HPList.Nil = pure ()

instance RecreateFrmbffrs sis sfs =>
	RecreateFrmbffrs (si ': sis) (sf ': sfs) where
	recreateFrmbffrs dv ex rp (v :** vs) (fb :** fbs) =
		Vk.Frmbffr.unsafeRecreate dv (frmbffrInfo ex rp v) nil fb >>
		recreateFrmbffrs dv ex rp vs fbs

frmbffrInfo :: Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I inm fmt si ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(inm, fmt, si)]
frmbffrInfo ex rp att = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = U3 att :** HPList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = ex

bffrAlgn :: forall o sd a . VObj.SizeAlignment o =>
	Vk.Dvc.D sd -> VObj.Length o -> Vk.Bffr.UsageFlags ->
	(forall al . KnownNat al => Proxy al -> IO a) -> IO a
bffrAlgn dv ln us f = Vk.Bffr.create dv (bffrInfo ln us) nil \b ->
	(\(SomeNat p) -> f p) . someNatVal . fromIntegral =<<
	Vk.Mm.requirementsAlignment <$> Vk.Bffr.getMemoryRequirements dv b

createRectangleBuffer ::
	Devices sd sc scb -> RectGroups sd sm sb nm () -> () -> [RectangleRaw] ->
	IO (Vk.Bffr.Binded sm sb nm '[VObj.List 256 RectangleRaw ""])
createRectangleBuffer (phdvc, _qfis, dvc, gq, _pq, cp, _cb) (bgrp, mgrp) k rs =
	createBufferList' phdvc dvc bgrp mgrp k (fromIntegral $ length rs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit >>= \(b, _) -> do
	createBffrLst phdvc dvc (fromIntegral $ length rs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
			\(b' :: Vk.Bffr.Binded sm sb "rectangle-buffer" '[VObj.List 256 t ""]) bm' -> do
		Vk.Mm.write @"rectangle-buffer" @(VObj.List 256 RectangleRaw "") @0 dvc bm' zeroBits rs
		copyBffrLst dvc gq cp b' b
	pure b

createRectangleBuffer' :: Ord k =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	RectGroups sd sm sb nm k -> k -> [RectangleRaw] ->
	IO (Vk.Bffr.Binded sm sb nm '[VObj.List 256 RectangleRaw ""])
createRectangleBuffer' phdvc dvc gq cp (bgrp, mgrp) k rs =
	createBufferList' phdvc dvc bgrp mgrp k (fromIntegral $ length rs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit >>= \(b, _) -> do
	createBffrLst phdvc dvc (fromIntegral $ length rs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
			\(b' :: Vk.Bffr.Binded sm sb "rectangle-buffer" '[VObj.List 256 t ""]) bm' -> do
		Vk.Mm.write @"rectangle-buffer" @(VObj.List 256 RectangleRaw "") @0 dvc bm' zeroBits rs
		copyBffrLst dvc gq cp b' b
	pure b

destroyRectangleBuffer :: Ord k => RectGroups sd sm sb nm k -> k -> IO ()
destroyRectangleBuffer (bgrp, mgrp) k = do
	r1 <- Vk.Mm.unsafeFree mgrp k
	r2 <- Vk.Bffr.unsafeDestroy bgrp k
	case (r1, r2) of
		(Left msg, _) -> error msg
		(_, Left msg) -> error msg
		_ -> pure ()

type RectGroups sd sm sb nm k = (
	Vk.Bffr.Group sd 'Nothing sb k nm '[VObj.List 256 RectangleRaw ""],
	Vk.Mm.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mm.BufferArg nm '[VObj.List 256 RectangleRaw ""])] )

type UniformBufferMemory sm sb nmvp alu = Vk.Mm.M sm '[ '(
	sb,
	'Vk.Mm.BufferArg nmvp '[VObj.Atom alu ViewProj 'Nothing]
	)]

createBffrAtm :: forall sd nm al t a . (KnownNat al, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb nm '[VObj.Atom al t 'Nothing] ->
		Vk.Mm.M sm '[ '(
			sb, 'Vk.Mm.BufferArg nm '[VObj.Atom al t 'Nothing] )] ->
		IO a) -> IO a
createBffrAtm p dv = createBffr p dv VObj.LengthAtom

createBffrLst :: forall al sd bnm lnm t a . (KnownNat al, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[VObj.List al t lnm] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg bnm '[VObj.List al t lnm])] ->
		IO a) -> IO a
createBffrLst p dv ln = createBffr p dv $ VObj.LengthList ln

createBufferList' :: forall sd nm t sm sb k . (Ord k, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[VObj.List 256 t ""]  ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mm.BufferArg nm '[VObj.List 256 t ""])] ->
	k ->
	Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 t ""],
		Vk.Mm.M sm '[ '(
			sb, 'Vk.Mm.BufferArg nm '[VObj.List 256 t ""] ) ] )
createBufferList' p dv bgrp mgrp k ln usg props =
	createBuffer' p dv bgrp mgrp k (VObj.LengthList ln) usg props

createBuffer' :: forall sd sb nm o sm k .
	(Ord k, VObj.SizeAlignment o) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[o] ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] -> k ->
	VObj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[o],
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] )
createBuffer' p dv bgrp mgrp k ln usg props =
	Vk.Bffr.create' bgrp k binfo >>= \(AlwaysRight b) -> do
		reqs <- Vk.Bffr.getMemoryRequirements dv b
		mt <- findMemoryType p (Vk.Mm.requirementsMemoryTypeBits reqs) props
		Vk.Mm.allocateBind' mgrp k (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
			(allcInfo mt) >>=
			\(AlwaysRight (HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd)), mem)) -> pure (bnd, mem)
	where
	binfo :: Vk.Bffr.CreateInfo 'Nothing '[o]
	binfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths = HPList.Singleton ln,
		Vk.Bffr.createInfoUsage = usg,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	allcInfo :: Vk.Mm.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
	allcInfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

{-# COMPLETE AlwaysRight #-}

pattern AlwaysRight :: r -> Either l r
pattern AlwaysRight x <- Right x where
	AlwaysRight x = Right x

findMemoryType :: Vk.Phd.P -> Vk.Mm.TypeBits -> Vk.Mm.PropertyFlags ->
	IO Vk.Mm.TypeIndex
findMemoryType phdvc flt props =
	fromMaybe (error msg) . suitable <$> Vk.Phd.getMemoryProperties phdvc
	where
	msg = "failed to find suitable memory type!"
	suitable props1 = fst <$> L.find ((&&)
		<$> (`Vk.Mm.elemTypeIndex` flt) . fst
		<*> checkBits props . Vk.Mm.mTypePropertyFlags . snd) tps
		where tps = Vk.Phd.memoryPropertiesMemoryTypes props1

data SyncObjects (ssos :: (Type, Type, Type)) where
	SyncObjects :: {
		_imageAvailableSemaphores :: Vk.Semaphore.S sias,
		_renderFinishedSemaphores :: Vk.Semaphore.S srfs,
		_inFlightFences :: Vk.Fence.F sfs } ->
		SyncObjects '(sias, srfs, sfs)

createSyncObjects :: Vk.Dvc.D sd ->
	(forall sias srfs siff . SyncObjects '(sias, srfs, siff) -> IO a) -> IO a
createSyncObjects dv f =
	Vk.Semaphore.create @'Nothing dv def nil \ias ->
	Vk.Semaphore.create @'Nothing dv def nil \rfs ->
	Vk.Fence.create @'Nothing dv fncInfo nil \iff ->
	f $ SyncObjects ias rfs iff
	where
	fncInfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sl sg sm sb smr sbr nm sm' sb' nm' sdsl sds nmt alu ali alv .
	(KnownNat ali, KnownNat alv) =>
	Vk.CBffr.C scb ->
	Vk.RndrPss.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu nmt] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(RectangleRaw, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl alu nmt], '[]) ->
	Vk.Bffr.Binded sm sb nm '[VObj.List alv WVertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[VObj.List 256 RectangleRaw ""], Vk.Cmd.InstanceCount) ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List ali Word16 ""] ->
	Vk.DscSt.D sds (AtomUbo sdsl alu nmt) ->
	IO ()
recordCommandBuffer cb rp fb sce pllyt gpl vb (rb, ic) ib ubds =
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb (
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @"" vb) :**
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @RectangleRaw @"" rb) :**
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

checkTChan :: TChan () -> IO Bool
checkTChan t = atomically do
	ne <- not <$> isEmptyTChan t
	when ne $ readTChan t
	pure ne

mainLoop ::
	forall nmt scfmt sw ssfc sd sc scb sias srfs siff ssc nm sr sg sl
		sdsl sm sb sm' sb' nm' srm srb sds sm2 sb2 svs sfs alu ali alv . (
	Vk.T.FormatToValue scfmt,
	RecreateFrmbffrs svs sfs,
	KnownNat alu, KnownNat alv, KnownNat ali
	) =>
	TChan Command -> TChan Event -> Devices sd sc scb -> PipelineLayout sl sdsl alu nmt ->

	VertexBuffers sm sb nm alv sm' sb' nm' ali ->
	RectGroups sd srm srb nm () ->
	UniformBuffers sds sdsl alu nmt sm2 sb2 "uniform-buffer" ->
	WinObjs sw ssfc sg sl sdsl alu nmt sias srfs siff scfmt ssc nm svs sr sfs ->
	(FV.View -> IO ()) -> IO () ->
	IO ()
mainLoop inp outp dvs@(_, _, dvc, _, _, _, _) pll vbs rgrps ubs wos@(WinObjs (_, fbrszd) _ _ _ _ _)
	wwww1 wwww2 = do
	wbw <- atomically newTChan
	wm <- atomically newTChan
	fix \loop -> do
		checkResizedState fbrszd >>= bool (pure ()) do
			putStrLn "recreateSwapchainEtcIfNeed: needed"
			atomically $ writeTChan outp EventNeedRedraw
		atomically (readTChan inp) >>= \case
			DrawRect d -> do
				let	d' = rectsToDummyRaw $ second
						(rectangle'ToRectangleRaw <$>) d
				b <- checkTChan wbw
				when b wwww2
				Vk.Dvc.waitIdle dvc
				runLoop' dvs pll wos vbs rgrps d' ubs outp loop
			SetViewAsTexture view -> do
				_ <- forkIO do
					b <- atomically $ isEmptyTChan wm
					when b do
						atomically $ writeTChan wm ()
						wwww1 view
						atomically $ readTChan wm
						atomically (writeTChan wbw ())
				loop
			GetEvent -> do
				glfwEvents (winObjsToWin wos) outp
				loop
			EndWorld -> pure ()
	atomically $ check =<< isEmptyTChan wm

rectsToDummyRaw :: (b, [RectangleRaw]) -> (b, [RectangleRaw])
rectsToDummyRaw = \(tm, rects) -> (tm, bool rects dummyRaw $ null rects)

type PipelineLayout sl sdsl alu nmt = Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu nmt] '[]

type UniformBuffers sds sdsl alu nmt sm2 sb2 nmvp =
	(Vk.DscSt.D sds (AtomUbo sdsl alu nmt), UniformBufferMemory sm2 sb2 nmvp alu)

type Devices sd scp scb = (
	Vk.Phd.P, QueueFamilyIndices, Vk.Dvc.D sd,
	Vk.Q.Q, Vk.Q.Q, Vk.CmdPl.C scp, Vk.CBffr.C scb )

data FramebufferResizedState = NoResized | HalfResized | Resized deriving Show

checkResizedState :: FramebufferResized -> IO Bool
checkResizedState fbrszd = atomically $
	readTVar fbrszd >>= \case
		Resized -> writeTVar fbrszd HalfResized >> pure True
		HalfResized -> writeTVar fbrszd NoResized >> pure True
		NoResized -> pure False

type Swapchains scfmt ssc nm sivs sr sfs = (
	Vk.Khr.Swpch.S scfmt ssc,
	HPList.PL (Vk.ImgVw.I nm scfmt) sivs,
	Vk.RndrPss.R sr, HPList.PL Vk.Frmbffr.F sfs )

type VertexBuffers sm sb nm alv sm' sb' nm' ali = (
	Vk.Bffr.Binded sm sb nm '[VObj.List alv WVertex ""],
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List ali Word16 ""] )

data Recreates sw sl nm ssfc sr sg sdsl alu nmt fmt ssc sis sfs = Recreates
	(GlfwG.Win.W sw) (Vk.Khr.Sfc.S ssfc)
	(TVar Vk.Extent2d)
	(Vk.RndrPss.R sr)
	(Vk.Ppl.Graphics.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(RectangleRaw, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl alu nmt], '[]))
	(Vk.Khr.Swpch.S fmt ssc)
	(HPList.PL (Vk.ImgVw.I nm fmt) sis)
	(HPList.PL Vk.Frmbffr.F sfs)

winObjsToRecreates ::
	WinObjs sw ssfc sg sl sdsl alu nmt sias srfs siff scfmt ssc nm sscivs sr sfs ->
	Recreates sw sl nm ssfc sr sg sdsl alu nmt scfmt ssc sscivs sfs
winObjsToRecreates (WinObjs (w, _) sfc vex gpl _iasrfsifs (sc, scivs, rp, fbs)) =
	Recreates w sfc vex rp gpl sc scivs fbs

data Draws sl sr sg sdsl alu nmt sias srfs siff fmt ssc sfs = Draws
	(TVar Vk.Extent2d) (Vk.RndrPss.R sr)
	(Vk.Ppl.Graphics.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(RectangleRaw, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl alu nmt], '[]))
	(SyncObjects '(sias, srfs, siff))
	(Vk.Khr.Swpch.S fmt ssc)
	(HPList.PL Vk.Frmbffr.F sfs)

winObjsToDraws ::
	WinObjs sw ssfc sg sl sdsl alu nmt sias srfs siff scfmt ssc nm sscivs sr sfs ->
	Draws sl sr sg sdsl alu nmt sias srfs siff scfmt ssc sfs
winObjsToDraws (WinObjs _ _sfc vex gpl iasrfsifs (sc, _scivs, rp, fbs)) =
	Draws vex rp gpl iasrfsifs sc fbs

winObjsToWin ::
	WinObjs sw ssfc sg sl sdsl alu nmt sias srfs siff scfmt ssc nm sscivs sr sfs ->
	W sw
winObjsToWin (WinObjs (win, _) _ _ _ _ _) = win

runLoop' :: forall sfs svs -- (sf :: Type)
	sd sc scb sl
	sw ssfc sg sias srfs siff scfmt ssc sr
	smrct sbrct nmrct sds sdsl sm sb sm' sb' sm2 sb2 nm2 nmt alu ali alv . (
	Vk.T.FormatToValue scfmt,
	RecreateFrmbffrs svs sfs,
	KnownNat alu, KnownNat alv, KnownNat ali
	) =>
	Devices sd sc scb -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu nmt] '[] ->
	WinObjs sw ssfc sg sl sdsl alu nmt sias srfs siff scfmt ssc nmrct svs sr sfs ->
	(	Vk.Bffr.Binded sm' sb' nmrct '[VObj.List alv WVertex ""],
		Vk.Bffr.Binded sm2 sb2 nm2 '[VObj.List ali Word16 ""] ) ->
	(	Vk.Bffr.Group sd 'Nothing sbrct () nmrct '[VObj.List 256 RectangleRaw ""],
		Vk.Mm.Group sd 'Nothing smrct () '[
			'(sbrct, 'Vk.Mm.BufferArg nmrct '[VObj.List 256 RectangleRaw ""])] ) ->
	(ViewProj, [RectangleRaw]) ->
	(Vk.DscSt.D sds (AtomUbo sdsl alu nmt), UniformBufferMemory sm sb "uniform-buffer" alu) ->
	TChan Event ->
	IO () -> IO ()
runLoop' dvs pll wos vbs rgrps rectss ubs outp loop = do
	let	(phdvc, qfis, dvc, gq, pq, _cp, cb) = dvs
		(vb, ib) = vbs
		(ubds, ubm) = ubs
	do
		let	(tm, rects') = rectss
		destroyRectangleBuffer rgrps ()
		rb <- createRectangleBuffer dvs rgrps () rects'
		let	rb' = (rb, fromIntegral $ length rects')
		catchAndDraw @_ @_ @_ phdvc qfis dvc gq pq pll vb rb' ib ubm ubds cb tm wos
	cls <- GlfwG.Win.shouldClose $ winObjsToWin wos
	if cls then (pure ()) else do
		recreateSwapchainEtcIfNeed @_ @_ @_ phdvc qfis dvc pll wos outp
		loop

dummyRaw :: [RectangleRaw]
dummyRaw = let m0 :. m1 :. m2 :. m3 :. NilL = Cglm.mat4ToVec4s Cglm.mat4Identity in
	[RectangleRaw (RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
			(RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
			(RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 0.0 :. NilL)
			(RectModel0 m0) (RectModel1 m1)
			(RectModel2 m2) (RectModel3 m3)]

catchAndDraw ::
	forall svs sfs
		sd sl sdsl sm sb smr sbr nm sm' sb' sm2 sb2 nm' sw ssfc sg sias srfs siff win ssc sr sds scb nmt alu ali alv . (
	Vk.T.FormatToValue win,
	RecreateFrmbffrs svs sfs,
	KnownNat alu, KnownNat alv, KnownNat ali
	) =>
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.Q.Q -> Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu nmt] '[] ->
	Vk.Bffr.Binded sm sb nm '[VObj.List alv WVertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[VObj.List 256 RectangleRaw ""], Vk.Cmd.InstanceCount)  ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List ali Word16 ""] ->
	UniformBufferMemory sm2 sb2 "uniform-buffer" alu -> Vk.DscSt.D sds (AtomUbo sdsl alu nmt) ->
	Vk.CBffr.C scb ->
	ViewProj ->
	WinObjs sw ssfc sg sl sdsl alu nmt sias srfs siff win ssc nm
		svs sr sfs ->
	IO ()
catchAndDraw phdvc qfis dvc gq pq pllyt vb rb ib ubm ubds cb ubo wos = do
	catchAndRecreate @_ @_ @_ @_ phdvc qfis dvc pllyt (winObjsToRecreates wos)
		$ drawFrame dvc gq pq pllyt (winObjsToDraws wos) vb rb ib ubm ubds cb ubo
	Vk.Dvc.waitIdle dvc

recreateSwapchainEtcIfNeed ::
	forall svs sfs
		sd sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm sr nmt alu . (
	Vk.T.FormatToValue scfmt,
	RecreateFrmbffrs svs sfs
	) =>
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu nmt] '[] ->
	WinObjs sw ssfc sg sl sdsl alu nmt sias srfs siff scfmt ssc nm
		svs sr sfs ->
		TChan Event -> IO ()
recreateSwapchainEtcIfNeed phdvc qfis dvc pllyt wos@(WinObjs (_, fbrszd) _ _ _ _ _) outp =
	checkResizedState fbrszd >>= bool (pure ()) (do
		putStrLn "recreateSwapchainEtcIfNeed: needed"
		atomically $ writeTChan outp EventNeedRedraw
		recreateSwapchainEtc @_ @_ @_ phdvc qfis dvc pllyt $ winObjsToRecreates wos)
	

drawFrame :: forall sfs sd ssc sr sl sg sm sb smr sbr nm sm' sb' nm' sm2 sb2 scb sias srfs siff sdsl scfmt sds nmt alu alv ali .
	(KnownNat alu, KnownNat alv, KnownNat ali) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu nmt] '[] ->
	Draws sl sr sg sdsl alu nmt sias srfs siff scfmt ssc sfs ->
	Vk.Bffr.Binded sm sb nm '[VObj.List alv WVertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[VObj.List 256 RectangleRaw ""], Vk.Cmd.InstanceCount) ->
	Vk.Bffr.Binded sm' sb' nm' '[VObj.List ali Word16 ""] ->
	UniformBufferMemory sm2 sb2 "uniform-buffer" alu ->
	Vk.DscSt.D sds (AtomUbo sdsl alu nmt) ->
	Vk.CBffr.C scb ->
	ViewProj -> IO ()
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
	Vk.CBffr.reset cb def
	HPList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext pllyt gpl vb rb ib ubds
	updateUniformBuffer' dvc ubm ubo
	let	submitInfo :: Vk.SubmitInfo 'Nothing '[sias] '[scb] '[srfs]
		submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks =
				HPList.Singleton
					(Vk.SemaphorePipelineStageFlags ias
						Vk.Ppl.StageColorAttachmentOutputBit),
			Vk.submitInfoCommandBuffers = HPList.Singleton cb,
			Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
		presentInfo = Vk.Khr.PresentInfo {
			Vk.Khr.presentInfoNext = TMaybe.N,
			Vk.Khr.presentInfoWaitSemaphores = HPList.Singleton rfs,
			Vk.Khr.presentInfoSwapchainImageIndices = HPList.Singleton
				$ Vk.Khr.SwapchainImageIndex sc imgIdx }
	Vk.Q.submit gq (HPList.Singleton $ U4 submitInfo) $ Just iff
	catchAndSerialize $ Vk.Khr.queuePresent @'Nothing pq presentInfo
	Vk.Fence.waitForFs dvc siff True Nothing
--	Vk.Q.waitIdle pq

updateUniformBuffer' :: forall sd sm2 sb2 alu . KnownNat alu => Vk.Dvc.D sd ->
	UniformBufferMemory sm2 sb2 "uniform-buffer" alu -> ViewProj -> IO ()
updateUniformBuffer' dvc um obj = do
	Vk.Mm.write @"uniform-buffer" @(VObj.Atom alu ViewProj 'Nothing) @0
		dvc um zeroBits obj

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate ::
	forall scfmt sfs svs sw ssfc sd nm sr ssc sl sdsl sg nmt alu . (
	Vk.T.FormatToValue scfmt,
	RecreateFrmbffrs svs sfs
	) =>
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu nmt] '[] ->
	Recreates sw sl nm ssfc sr sg sdsl alu nmt scfmt
		ssc svs sfs ->
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
	svs sfs scfmt sw ssfc sd ssc nm sr sl sdsl sg nmt alu .
	(
	RecreateFrmbffrs svs sfs,
	Vk.T.FormatToValue scfmt) =>
	Vk.Phd.P -> QueueFamilyIndices -> Vk.Dvc.D sd ->
	Vk.Ppl.Layout.P sl '[AtomUbo sdsl alu nmt] '[] ->
	Recreates sw sl nm ssfc sr sg sdsl alu nmt scfmt ssc svs sfs ->
	IO ()
recreateSwapchainEtc
	phdvc qfis dvc pllyt
	(Recreates win sfc vex rp gpl sc scivs fbs) = do
	putStrLn "recreateSwapchainEtc begin"
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwpch win sfc phdvc qfis dvc sc
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

type WVertex = StrG.W Vertex

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

data RectangleRaw = RectangleRaw {
	rectanglePos :: RectPos,
	rectangleSize :: RectSize,
	rectangleColor :: RectColor,
	rectangleModel0 :: RectModel0,
	rectangleModel1 :: RectModel1,
	rectangleModel2 :: RectModel2,
	rectangleModel3 :: RectModel3 }
	deriving (Show, Generic)

data Rectangle = Rectangle {
	rectanglePos' :: RectPos,
	rectangleSize' :: RectSize,
	rectangleColor' :: RectColor,
	rectangleModel' :: RectModel }
	deriving (Show, Generic)

rectangle'ToRectangleRaw :: Rectangle -> RectangleRaw
rectangle'ToRectangleRaw Rectangle {
	rectanglePos' = p,
	rectangleSize' = s,
	rectangleColor' = c,
	rectangleModel' = RectModel m } = RectangleRaw {
	rectanglePos = p,
	rectangleSize = s,
	rectangleColor = c,
	rectangleModel0 = RectModel0 m0,
	rectangleModel1 = RectModel1 m1,
	rectangleModel2 = RectModel2 m2,
	rectangleModel3 = RectModel3 m3 }
	where m0 :. m1 :. m2 :. m3 :. NilL = Cglm.mat4ToVec4s m

instance StrG.G RectangleRaw where

instance Storable RectangleRaw where
	sizeOf = StrG.gSizeOf
	alignment = StrG.gAlignment
	peek = StrG.gPeek
	poke = StrG.gPoke

instance Default RectangleRaw where
	def = RectangleRaw
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

vertices :: [WVertex]
vertices = StrG.W <$> [
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

data ViewProj = ViewProj {
	viewProjectionView :: Cglm.Mat4,
	viewProjectionProj :: Cglm.Mat4 }
	deriving (Show, Generic)

instance Storable ViewProj where
	sizeOf = StrG.gSizeOf
	alignment = StrG.gAlignment
	peek = StrG.gPeek
	poke = StrG.gPoke

instance StrG.G ViewProj

instance Default ViewProj where
	def = ViewProj Cglm.mat4Identity Cglm.mat4Identity

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
