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
import GHC.TypeLits (Symbol)
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
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Exception qualified as Vk
import Gpu.Vulkan.Object qualified as Vk.Obj
import Gpu.Vulkan.Object.NoAlignment qualified as Vk.ObjNA
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
import Gpu.Vulkan.Pipeline.Graphics qualified as Vk.Ppl.Gr
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.VertexInputState qualified as Vk.Ppl.VertexInputSt
import Gpu.Vulkan.Pipeline.InputAssemblyState qualified as Vk.Ppl.InpAsmbSt
import Gpu.Vulkan.Pipeline.RasterizationState qualified as Vk.Ppl.RstSt
import Gpu.Vulkan.Pipeline.ColorBlendState qualified as Vk.Ppl.ClrBlndSt
import Gpu.Vulkan.Pipeline.ColorBlendAttachment qualified as Vk.Ppl.ClrBlndAtt
import Gpu.Vulkan.Pipeline.ViewportState qualified as Vk.Ppl.ViewportSt
import Gpu.Vulkan.Pipeline.MultisampleState qualified as Vk.Ppl.MltSmplSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.PplLyt
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

----------------------------------------------------------------------
--
-- * USE CAIRO LOOP
-- * CONTROLLER
-- * WINDOW, INSTANCE AND PHYSICAL AND LOGICAL DEVICES
-- * BODY
-- * WINDOW OBJECTS
-- * CREATE GRAPHICS PIPELINE
-- * CREATE BUFFER
-- * MAIN LOOP AND RUN
-- * RECREATE
-- * DRAW
-- * DATA TYPES
-- * SHADERS
--
----------------------------------------------------------------------

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
	body ip op vex w sfc pd qfis dv gq pq >>
	atomically (writeTChan op EventEnd)
	where dbgm i = Vk.Ex.DUtls.Msgr.create i dbgMsngrInfo nil

data Command
	= SetViewAsTexture FV.View | DrawRect ViewProj [Rectangle]
	| GetEvent | EndWorld
	deriving Show

data Event
	= EventDeleteWindow | EventEnd
	| EventKeyDown GlfwG.Ky.Key | EventKeyUp GlfwG.Ky.Key
	| EventKeyRepeating GlfwG.Ky.Key
	| EventGamepadAxisLeftX Float | EventGamepadButtonAPressed
	deriving Show

-- CONTROLLER

controller :: TChan Event -> IO ()
controller op = fix \go -> (>> go) . (threadDelay 10000 >>)
	$ Glfw.getGamepadState Glfw.Joystick'1 >>= atomically . \case
		Nothing -> pure ()
		Just (Glfw.GamepadState gb ga) -> do
			when (btna == Glfw.GamepadButtonState'Pressed)
				$ writeTChan op EventGamepadButtonAPressed
			when (abs leftx > 0.1)
				. writeTChan op $ EventGamepadAxisLeftX leftx
			where
			btna = gb Glfw.GamepadButton'A
			leftx = ga Glfw.GamepadAxis'LeftX

-- WINDOW, INSTANCE AND PHYSICAL AND LOGICAL DEVICES

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
	Vk.Ex.DUtls.Msgr.createInfoFnUserCallback = dbgcb,
	Vk.Ex.DUtls.Msgr.createInfoUserData = Nothing }
	where dbgcb _svr _tp d _ud = False <$ Txt.putStrLn
		("validation layer: " <> Vk.Ex.DUtls.Msgr.callbackDataMessage d)

createWin :: (forall sw . GlfwG.Win.W sw -> IO a) -> IO a
createWin f = do
	GlfwG.Win.hint
		$ GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI
	GlfwG.Win.hint $ GlfwG.Win.WindowHint'Visible True
	GlfwG.Win.create 800 600 "USE CAIRO" Nothing Nothing f

pickPhd :: Vk.Ist.I si -> Vk.Khr.Sfc.S ss -> IO (Vk.Phd.P, QFamIdcs)
pickPhd ist sfc = Vk.Phd.enumerate ist >>= \case
	[] -> error "failed to find GPUs with Gpu.Vulkan support!"
	pds -> findMaybeM suit pds >>= \case
		Nothing -> error "failed to find a suitable GPU!"
		Just pdqfi -> pure pdqfi
	where
	suit pd = ((&&) <$> espt pd <*> sa pd) >>= bool (pure Nothing) do
		qfis <- findQFams pd sfc
		querySwpchSupport pd sfc \ss -> pure . bool qfis Nothing
			$	HPListC.null (snd $ formats ss) ||
				null (presentModes ss)
	espt pd = elemAll dvcExtensions
		. (Vk.Phd.extensionPropertiesExtensionName <$>)
		<$> Vk.Phd.enumerateExtensionProperties pd Nothing
	sa pd = Vk.Phd.featuresSamplerAnisotropy <$> Vk.Phd.getFeatures pd

dvcExtensions :: [Vk.Phd.ExtensionName]
dvcExtensions = [Vk.Khr.Swpch.extensionName]

findQFams :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (Maybe QFamIdcs)
findQFams pd sfc = do
	prps@((fst <$>) -> is) <- Vk.Phd.getQueueFamilyProperties pd
	mp <- listToMaybe
		<$> filterM (flip (Vk.Khr.Sfc.Phd.getSupport pd) sfc) is
	pure $ QFamIdcs <$> (fst <$> L.find (grbit . snd) prps) <*> mp
	where grbit = checkBits Vk.Q.GraphicsBit . Vk.QFam.propertiesQueueFlags

data QFamIdcs = QFamIdcs { grFam :: Vk.QFam.Index, prFam :: Vk.QFam.Index }

createLgDvc :: Vk.Phd.P -> QFamIdcs ->
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
	qinfo qfi = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }
	info qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames = bool [] vldLayers debug,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures =
			Just def { Vk.Phd.featuresSamplerAnisotropy = True } }

-- BODY

body :: forall sw ssfc sd (nmt :: Symbol) .
	TChan Command -> TChan Event -> TVar Vk.Extent2d ->
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIdcs -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> IO ()
body ip op vex w sfc pd qfis dv gq pq =
	createCmdPl qfis dv \cp -> createCmdBffr dv cp \cb ->
	unfrmBffrOstAlgn pd \(_ :: Proxy alu) ->
	createPplLyt @_ @alu @nmt dv \dsl pl ->
	createViewProjBffr pd dv \vp vpm ->
	createDscPl dv \dp -> createDscSt dv dp vp dsl \ds ->
	createVtxBffr pd dv gq cp vertices \vb ->
	createIdxBffr pd dv gq cp indices \ib ->
	Vk.Bffr.group dv nil \rbg -> Vk.Mm.group dv nil \rmg ->
	winObjs op w sfc vex pd qfis dv gq cp pl (rbg, rmg) \wos ->
	createTxSmplr pd dv \txsmplr ->
	createBindImg @_ @_ @_ @_ @_ @nmt pd dv ds txsmplr textureSize \txi ->
	createBffrImg pd dv textureSize \ibf ibfm ->
	cairoImageSurfaceCreate
		CairoFormatArgb32 textureWidth textureHeight >>= \crsfc ->
	cairoCreate crsfc >>= \cr ->
	let	viewToBffr = (writeBffr dv ibfm =<<) . drawViewIO crsfc cr
		bffrToImg = flashImg dv gq cp txi ibf textureSize in
	viewToBffr (FV.View []) >> bffrToImg >>
	mainloop ip op (pd, qfis, dv, gq, pq, cp, cb)
		pl wos (vb, ib) ds vpm (rbg, rmg) viewToBffr bffrToImg

createCmdPl ::
	QFamIdcs -> Vk.Dvc.D sd -> (forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfis dv = Vk.CmdPl.create dv info nil
	where info = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = grFam qfis }

createCmdBffr :: forall sd scp a .
	Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb . Vk.CBffr.C scb -> IO a) -> IO a
createCmdBffr dv cp f =
	Vk.CBffr.allocate dv info $ f . \(cb :*. HPList.Nil) -> cb
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
	Vk.DscStLyt.D sdsl (DscStLytArg alu "" nmt) ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu "" nmt)] '[] -> IO a) -> IO a
createPplLyt dv f = createDscStLyt dv \dsl ->
	Vk.PplLyt.create @_ @_ @_ @'[] dv (info dsl) nil $ f dsl
	where info dsl = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

createDscStLyt :: Vk.Dvc.D sd ->
	(forall (s :: Type) . Vk.DscStLyt.D s (DscStLytArg alu "" nmt) -> IO a) ->
	IO a
createDscStLyt dv = Vk.DscStLyt.create dv info nil
	where
	info = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = vpbd :** tbd :** HPList.Nil }
	vpbd = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	tbd = Vk.DscStLyt.BindingImage {
		Vk.DscStLyt.bindingImageDescriptorType =
			Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscStLyt.bindingImageStageFlags = Vk.ShaderStageFragmentBit }

type DscStLytArg alu nmvp nmt = '[
	'Vk.DscStLyt.Buffer '[AtomViewProj alu nmvp],
	'Vk.DscStLyt.Image '[ '(nmt, 'Vk.T.FormatR8g8b8a8Srgb)] ]

type AtomViewProj alu nmvp = Vk.Obj.AtomNew alu WViewProj nmvp

createViewProjBffr :: KnownNat alu => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb bnmvp '[AtomViewProj alu nmvp]  ->
	ViewProjMemory sm sb bnmvp alu nmvp -> IO a) -> IO a
createViewProjBffr pd dv = createBffrAtm pd dv
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

type ViewProjMemory sm sb bnmvp alu nmvp =
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg bnmvp '[AtomViewProj alu nmvp])]

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
	Vk.Bffr.Binded sm sb nm '[AtomViewProj alu ""] ->
	Vk.DscStLyt.D sdsl (DscStLytArg alu "" nmt) ->
	(forall sds . Vk.DscSt.D sds '(sdsl, DscStLytArg alu "" nmt) -> IO a) ->
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
			'[ '(sm, sb, nm, AtomViewProj alu "", 0)]) 0
	wr ds = Vk.DscSt.Write {
		Vk.DscSt.writeNext = TMaybe.N,
		Vk.DscSt.writeDstSet = ds,
		Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscSt.writeSources = Vk.DscSt.BufferInfos
			. HPList.Singleton . U5 $ Vk.Dsc.BufferInfo vpb }

createVtxBffr :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	[WVertex] -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List WVertex nmv] ->
		IO a) -> IO a
createVtxBffr = createBffrMem Vk.Bffr.UsageVertexBufferBit

createIdxBffr :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	[Word16] -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Vk.ObjNA.List Word16 lnm] ->
		IO a) -> IO a
createIdxBffr = createBffrMem Vk.Bffr.UsageIndexBufferBit

createBffrMem :: forall sd sc t bnm lnm a . Storable' t =>
	Vk.Bffr.UsageFlags -> Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> [t] -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.Obj.List 1 t lnm] -> IO a) -> IO a
createBffrMem us pd dv gq cp xs@(fromIntegral . length -> ln) f =
	createBffrLst pd dv ln (Vk.Bffr.UsageTransferDstBit .|. us)
		Vk.Mm.PropertyDeviceLocalBit \b _ -> do
		createBffrLst pd dv ln
			Vk.Bffr.UsageTransferSrcBit
			(	Vk.Mm.PropertyHostVisibleBit .|.
				Vk.Mm.PropertyHostCoherentBit )
			\(b' :: Vk.Bffr.Binded sm sb bnm'
				'[Vk.Obj.List al t lnm']) bm' -> do
			Vk.Mm.write @bnm' @(Vk.Obj.List al t lnm') @0
				dv bm' zeroBits xs
			copyBffrLst dv gq cp b' b
		f b

createTxSmplr ::
	Vk.Phd.P -> Vk.Dvc.D sd -> (forall ss . Vk.Smplr.S ss -> IO a) -> IO a
createTxSmplr pd dv f = do
	lmts <- Vk.Phd.propertiesLimits <$> Vk.Phd.getProperties pd
	Vk.Smplr.create dv (info lmts) nil f
	where info lmts = Vk.Smplr.CreateInfo {
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
			Vk.Phd.limitsMaxSamplerAnisotropy lmts,
		Vk.Smplr.createInfoCompareEnable = False,
		Vk.Smplr.createInfoCompareOp = Vk.CompareOpAlways,
		Vk.Smplr.createInfoMinLod = 0, Vk.Smplr.createInfoMaxLod = 0,
		Vk.Smplr.createInfoBorderColor = Vk.BorderColorIntOpaqueBlack,
		Vk.Smplr.createInfoUnnormalizedCoordinates = False }

-- WINDOW OBJECTS

winObjs :: forall sw ssfc sd sc sl sdsl alu nmt nmscv smr sbr bnmr nmr a .
	TChan Event -> GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	TVar Vk.Extent2d -> Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu "" nmt)] '[] ->
	RectGroups sd smr sbr bnmr nmr () ->
	(forall scfmt ssc svs sr sfs sg sias srfs siff .
		(Vk.T.FormatToValue scfmt, RecreateFrmbffrs svs sfs) =>
		WinObjs sw ssfc scfmt ssc nmscv svs sr sfs
			sg sl sdsl alu "" nmt sias srfs siff -> IO a) -> IO a
winObjs op w sfc vex pd qfis dv gq cp pl rgs f =
	atomically (newTVar False) >>= \fr ->
	GlfwG.Win.setKeyCallback w (Just \_w ky _sc act _mds ->
		atomically $ writeTChan op $ ($ ky) case act of
			GlfwG.Ky.KeyState'Pressed -> EventKeyDown
			GlfwG.Ky.KeyState'Released -> EventKeyUp
			GlfwG.Ky.KeyState'Repeating -> EventKeyRepeating) >>
	GlfwG.Win.setFramebufferSizeCallback w
		(Just \_ _ _ -> atomically $ writeTVar fr True) >>
	createSwpch w sfc pd qfis dv \(sc :: Vk.Khr.Swpch.S scfmt ss) ex ->
	Vk.Khr.Swpch.getImages dv sc >>= \scis -> createImgVws dv scis \scvs ->
	createRndrPss @scfmt dv \rp -> createFrmbffrs dv ex rp scvs \fbs ->
	createGrPpl dv ex rp pl \gp ->
	createSyncObjs dv \sos ->
	atomically (writeTVar vex ex) >>
	createRectBffr pd dv gq cp rgs () dummyRect >>
	f (WinObjs (w, fr) sfc vex (sc, scvs, rp, fbs) gp sos)

type RectGroups sd smr sbr bnmr nmr k = (
	Vk.Bffr.Group sd 'Nothing sbr k bnmr '[Vk.ObjNA.List WRect nmr],
	Vk.Mm.Group sd 'Nothing smr k '[ '(
		sbr,
		'Vk.Mm.BufferArg bnmr '[Vk.ObjNA.List WRect nmr] )] )

data WinObjs
	sw ssfc scfmt ssc nmscv svs sr sfs sg sl sdsl alu nmvp nmt sias srfs siff =
	WinObjs	(WinEnvs sw) (Vk.Khr.Sfc.S ssfc) (TVar Vk.Extent2d)
		(Swapchains scfmt ssc nmscv svs sr sfs)
		(Pipeline sg sl sdsl alu nmvp nmt) (SyncObjs '(sias, srfs, siff))

type WinEnvs sw = (GlfwG.Win.W sw , FramebufferResized)
type FramebufferResized = TVar Bool

type Swapchains scfmt ssc nmscv svs sr sfs = (
	Vk.Khr.Swpch.S scfmt ssc, HPList.PL (Vk.ImgVw.I nmscv scfmt) svs,
	Vk.RndrPss.R sr, HPList.PL Vk.Frmbffr.F sfs )

createSwpch :: GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIdcs -> Vk.Dvc.D sd -> (forall ss scfmt .
		Vk.T.FormatToValue scfmt =>
		Vk.Khr.Swpch.S scfmt ss -> Vk.Extent2d -> IO a) -> IO a
createSwpch w sfc pd qfis dv f = querySwpchSupport pd sfc \scs -> do
	ex <- swapExtent w $ capabilities scs
	let	cps = capabilities scs
		pm = findDefault Vk.Khr.Sfc.PresentModeFifo
			(== Vk.Khr.Sfc.PresentModeMailbox) $ presentModes scs
	chooseSwpSfcFmt (formats scs)
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
	presentModes :: [Vk.Khr.Sfc.PresentMode] }

chooseSwpSfcFmt :: (
	[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
	HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ) ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Khr.Sfc.Format fmt -> a) -> a
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`L.find` fmts)
	$ (== Vk.Khr.Sfc.ColorSpaceSrgbNonlinear) . Vk.Khr.Sfc.formatColorSpace
chooseSwpSfcFmt (_, HPListC.Nil) _ = error "no available swap surface formats"

recreateSwpch :: forall sw ssfc sd fmt ssc . Vk.T.FormatToValue fmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	QFamIdcs -> Vk.Dvc.D sd -> Vk.Khr.Swpch.S fmt ssc -> IO Vk.Extent2d
recreateSwpch w sfc pd qfis0 dv sc = do
	scs <- querySwpchSupportFmt @fmt pd sfc
	ex <- swapExtent w $ capabilitiesFmt scs
	let	cps = capabilitiesFmt scs
		Vk.Khr.Sfc.Format cs = fromMaybe
			(error "no available swap surface formats")
			. listToMaybe $ formatsFmt scs
		pm = findDefault Vk.Khr.Sfc.PresentModeFifo
			(== Vk.Khr.Sfc.PresentModeMailbox) $ presentModesFmt scs
	ex <$ Vk.Khr.Swpch.unsafeRecreate dv
		(swpchInfo @fmt sfc qfis0 cps cs pm ex) nil sc

querySwpchSupportFmt :: Vk.T.FormatToValue fmt =>
	Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (SwpchSupportDetailsFmt fmt)
querySwpchSupportFmt dv sfc = SwpchSupportDetailsFmt
	<$> Vk.Khr.Sfc.Phd.getCapabilities dv sfc
	<*> Vk.Khr.Sfc.Phd.getFormatsFiltered dv sfc
	<*> Vk.Khr.Sfc.Phd.getPresentModes dv sfc

data SwpchSupportDetailsFmt fmt = SwpchSupportDetailsFmt {
	capabilitiesFmt :: Vk.Khr.Sfc.Capabilities,
	formatsFmt :: [Vk.Khr.Sfc.Format fmt],
	presentModesFmt :: [Vk.Khr.Sfc.PresentMode] } deriving Show

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
	Vk.Khr.Sfc.S ss -> QFamIdcs -> Vk.Khr.Sfc.Capabilities ->
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

createImgVws :: Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> [Vk.Img.Binded ss ss inm fmt] ->
	(forall svs . HPList.PL (Vk.ImgVw.I inm fmt) svs -> IO a) -> IO a
createImgVws _ [] f = f HPList.Nil
createImgVws dv (i : is) f =
	Vk.ImgVw.create dv (imgVwInfo i Vk.Img.AspectColorBit) nil \v ->
	createImgVws dv is $ f . (v :**)

recreateImgVws :: Vk.T.FormatToValue fmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss inm fmt] ->
	HPList.PL (Vk.ImgVw.I inm fmt) svs -> IO ()
recreateImgVws _ [] HPList.Nil = pure ()
recreateImgVws dv (i : is) (v :** vs) =
	Vk.ImgVw.unsafeRecreate dv (imgVwInfo i Vk.Img.AspectColorBit) nil v >>
	recreateImgVws dv is vs
recreateImgVws _ _ _ =
	error "number of Vk.Image.I and Vk.ImageView.I should be same"

createRndrPss :: forall scifmt sd a . Vk.T.FormatToValue scifmt =>
	Vk.Dvc.D sd -> (forall sr . Vk.RndrPss.R sr -> IO a) -> IO a
createRndrPss dv = Vk.RndrPss.create @'Nothing @'[scifmt] dv info nil
	where
	info = Vk.RndrPss.CreateInfo {
		Vk.RndrPss.createInfoNext = TMaybe.N,
		Vk.RndrPss.createInfoFlags = zeroBits,
		Vk.RndrPss.createInfoAttachments = HPList.Singleton ca,
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
	sbpss = Vk.Subpass.Description {
		Vk.Subpass.descriptionFlags = zeroBits,
		Vk.Subpass.descriptionPipelineBindPoint =
			Vk.Ppl.BindPointGraphics,
		Vk.Subpass.descriptionInputAttachments = [],
		Vk.Subpass.descriptionColorAndResolveAttachments = Left [car],
		Vk.Subpass.descriptionDepthStencilAttachment = Nothing,
		Vk.Subpass.descriptionPreserveAttachments = [] }
	car = Vk.Att.Reference {
		Vk.Att.referenceAttachment = 0,
		Vk.Att.referenceLayout = Vk.Img.LayoutColorAttachmentOptimal }
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

createFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	HPList.PL (Vk.ImgVw.I inm fmt) sis -> (forall sfs .
		RecreateFrmbffrs sis sfs =>
		HPList.PL Vk.Frmbffr.F sfs -> IO a) -> IO a
createFrmbffrs _ _ _ HPList.Nil f = f HPList.Nil
createFrmbffrs dv ex rp (v :** vs) f =
	Vk.Frmbffr.create dv (frmbffrInfo ex rp v) nil \fb ->
	createFrmbffrs dv ex rp vs $ f . (fb :**)

class RecreateFrmbffrs (svs :: [Type]) (sfs :: [Type]) where
	recreateFrmbffrs :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
		HPList.PL (Vk.ImgVw.I inm fmt) svs ->
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
	Vk.Frmbffr.createInfoAttachments = HPList.Singleton $ U3 att,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = ex

createSyncObjs :: Vk.Dvc.D sd ->
	(forall sias srfs siff . SyncObjs '(sias, srfs, siff) -> IO a) -> IO a
createSyncObjs dv f =
	Vk.Semaphore.create @'Nothing dv def nil \ias ->
	Vk.Semaphore.create @'Nothing dv def nil \rfs ->
	Vk.Fence.create @'Nothing dv finfo nil \iff -> f $ SyncObjs ias rfs iff
	where
	finfo = def { Vk.Fence.createInfoFlags = Vk.Fence.CreateSignaledBit }

data SyncObjs (ssos :: (Type, Type, Type)) where
	SyncObjs :: {
		_imageAvailableSemaphores :: Vk.Semaphore.S sias,
		_renderFinishedSemaphores :: Vk.Semaphore.S srfs,
		_inFlightFences :: Vk.Fence.F sfs } ->
		SyncObjs '(sias, srfs, sfs)

createRectBffr :: Ord k =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	RectGroups sd smr sbr bnmr nmr k -> k -> [WRect] ->
	IO (Vk.Bffr.Binded smr sbr bnmr '[Vk.ObjNA.List WRect nmr])
createRectBffr pd dv gq cp (bg, mg) k rs =
	createBffrLst' pd dv bg mg k ln
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit >>= \(b, _) -> do
	createBffrLst pd dv ln Vk.Bffr.UsageTransferSrcBit
		(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)
		\(b' :: Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List t nm]) bm' ->
		Vk.Mm.write @bnm @(Vk.ObjNA.List WRect nm) @0
			dv bm' zeroBits rs >>
		copyBffrLst dv gq cp b' b
	pure b
	where ln = fromIntegral $ length rs

destroyRectBffr :: Ord k => RectGroups sd smr sbr bnmr nmr k -> k -> IO ()
destroyRectBffr (bg, mg) k = do
	r1 <- Vk.Mm.unsafeFree mg k
	r2 <- Vk.Bffr.unsafeDestroy bg k
	case (r1, r2) of
		(Left msg, _) -> error msg; (_, Left msg) -> error msg
		_ -> pure ()

-- CREATE GRAPHICS PIPELINE

createGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu "" nmt)] '[] ->
	(forall sg . Pipeline sg sl sdsl alu "" nmt -> IO a) -> IO a
createGrPpl dv ex rp pl f = Vk.Ppl.Gr.createGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	\(HPList.Singleton (U3 gp)) -> f gp

recreateGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu nmvp nmt)] '[] ->
	Pipeline sg sl sdsl alu nmvp nmt -> IO ()
recreateGrPpl dv ex rp pl gp = Vk.Ppl.Gr.unsafeRecreateGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	(HPList.Singleton $ U3 gp)

type Pipeline sg sl sdsl alu nmvp nmt = Vk.Ppl.Gr.G sg
	'[	'(WVertex, 'Vk.VtxInp.RateVertex),
		'(WRect, 'Vk.VtxInp.RateInstance) ]
	'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
		'(2, RectPos), '(3, RectSize), '(4, RectColor),
		'(5, RectModel0), '(6, RectModel1),
		'(7, RectModel2), '(8, RectModel3), '(9, TexCoord) ]
	'(sl, '[ '(sdsl, DscStLytArg alu nmvp nmt)], '[])

grPplInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu nmvp nmt)] '[] ->
	Vk.Ppl.Gr.CreateInfo 'Nothing
		'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
		'(	'Nothing,
			'[	'(WVertex, 'Vk.VtxInp.RateVertex),
				'(WRect, 'Vk.VtxInp.RateInstance) ],
			'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
				'(2, RectPos), '(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1),
				'(7, RectModel2), '(8, RectModel3),
				'(9, TexCoord) ] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'Nothing '(sl, '[ '(sdsl, DscStLytArg alu nmvp nmt)], '[])
		sr '(sb, vs, ts, plas)
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
		Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeNone,
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

shaderStages :: HPList.PL (U5 Vk.Ppl.ShdrSt.CreateInfo)
	'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
shaderStages = U5 vinfo :** U5 finfo :** HPList.Nil
	where
	vinfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageVertexBit,
		Vk.Ppl.ShdrSt.createInfoModule =
			(minfo glslVertexShaderMain, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	finfo = Vk.Ppl.ShdrSt.CreateInfo {
		Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShdrSt.createInfoFlags = def,
		Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageFragmentBit,
		Vk.Ppl.ShdrSt.createInfoModule =
			(minfo glslFragmentShaderMain, nil),
		Vk.Ppl.ShdrSt.createInfoName = "main",
		Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Nothing }
	minfo cd = Vk.ShaderModule.CreateInfo {
		Vk.ShaderModule.createInfoNext = TMaybe.N,
		Vk.ShaderModule.createInfoFlags = def,
		Vk.ShaderModule.createInfoCode = cd }

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

-- CREATE BUFFER

createBffrAtm :: forall sd bnm al t mnm a . (KnownNat al, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.Obj.AtomMaybeName al t mnm] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg
				bnm '[Vk.Obj.AtomMaybeName al t mnm] )] ->
		IO a) -> IO a
createBffrAtm p dv = createBffr p dv Vk.Obj.LengthAtom

createBffrLst :: forall al sd bnm mnm t a . (KnownNat al, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.Obj.ListMaybeName al t mnm] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg
				bnm '[Vk.Obj.ListMaybeName al t mnm])] ->
		IO a) -> IO a
createBffrLst p dv ln = createBffr p dv $ Vk.Obj.LengthList ln

createBffrLst' :: forall sd sm sb k bnm al t mnm .
	(Ord k, Storable t, KnownNat al) => Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k bnm '[Vk.Obj.ListMaybeName al t mnm]  ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(
		sb, 'Vk.Mm.BufferArg bnm '[Vk.Obj.ListMaybeName al t mnm] )] ->
	k -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	IO (	Vk.Bffr.Binded sm sb bnm '[Vk.Obj.ListMaybeName al t mnm],
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg
				bnm '[Vk.Obj.ListMaybeName al t mnm] ) ] )
createBffrLst' p dv bg mg k ln = createBffr' p dv bg mg k $ Vk.Obj.LengthList ln

-- MAIN LOOP AND RUN

mainloop :: forall
	sd sc scb sl sdsl alu nmvp nmt sw ssfc scfmt ssc nmscv svs sr sfs sg
	sias srfs siff smv sbv bnmv nmv smi sbi bnmi nmi
	sds smvp sbvp bnmvp smr sbr bnmr nmr .
	(KnownNat alu, Vk.T.FormatToValue scfmt, RecreateFrmbffrs svs sfs) =>
	TChan Command -> TChan Event -> Devices sd sc scb ->
	PipelineLayout sl sdsl alu nmvp nmt ->
	WinObjs sw ssfc scfmt ssc nmscv svs sr sfs
		sg sl sdsl alu nmvp nmt sias srfs siff ->
	VertexBuffers smv sbv bnmv nmv smi sbi bnmi nmi ->
	Vk.DscSt.D sds '(sdsl, DscStLytArg alu nmvp nmt) ->
	ViewProjMemory smvp sbvp bnmvp alu nmvp ->
	RectGroups sd smr sbr bnmr nmr () ->
	(FV.View -> IO ()) -> IO () -> IO ()
mainloop ip op dvs@(_, _, dv, _, _, _, _) pl wos vbs ds vpm rgs vtb bti = do
	(bz, rd) <- atomically $ (,) <$> newTVar False <*> newTChan
	fix \go -> atomically (readTChan ip) >>= \case
		SetViewAsTexture vw -> (>> go) $ forkIO do
			b <- atomically $ readTVar bz <* writeTVar bz True
			when (not b) $ vtb vw >> atomically
				(writeTVar bz False >> writeTChan rd ())
		DrawRect vp (list dummyRect (:) . (rectToRectRaw <$>) -> rs) ->
			((`when` (bti >> Vk.Dvc.waitIdle dv)) =<< ready rd) >>
			run dvs pl wos vbs ds vpm rgs (StrG.W vp) rs go
		GetEvent -> dev (objsToWin wos) >> go
		EndWorld -> pure ()
	atomically $ check . not =<< readTVar bz
	where
	ready c = atomically
		$ not <$> isEmptyTChan c >>= \p -> p <$ when p (readTChan c)
	dev w = GlfwG.pollEvents >> GlfwG.Win.shouldClose w >>=
		(`when` atomically (writeTChan op EventDeleteWindow))

type Devices sd scp scb = (
	Vk.Phd.P, QFamIdcs, Vk.Dvc.D sd,
	Vk.Q.Q, Vk.Q.Q, Vk.CmdPl.C scp, Vk.CBffr.C scb )

type PipelineLayout sl sdsl alu nmvp nmt =
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu nmvp nmt)] '[]

type VertexBuffers smv sbv bnmv nmv smi sbi bnmi nmi = (
	Vk.Bffr.Binded smv sbv bnmv '[Vk.ObjNA.List WVertex nmv],
	Vk.Bffr.Binded smi sbi bnmi '[Vk.ObjNA.List Word16 nmi] )

objsToWin ::
	WinObjs sw ssfc scfmt ssc nmscv svs sr sfs
		sg sl sdsl alu nmvp nmt sias srfs siff -> W sw
objsToWin (WinObjs (win, _) _ _ _ _ _) = win

run :: forall sd scp scb sl sdsl alu nmvp nmt
	sw ssfc scfmt ssc nmscv svs sr sfs sg sias srfs siff
	smv sbv bnmv nmv smi sbi bnmi nmi sds
	smvp sbvp bnmvp smr sbr nmr bnmr .
	(KnownNat alu, Vk.T.FormatToValue scfmt, RecreateFrmbffrs svs sfs) =>
	Devices sd scp scb ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu nmvp nmt)] '[] ->
	WinObjs sw ssfc
		scfmt ssc nmscv svs sr sfs
		sg sl sdsl alu nmvp nmt sias srfs siff ->
	VertexBuffers smv sbv bnmv nmv smi sbi bnmi nmi ->
	Vk.DscSt.D sds '(sdsl, DscStLytArg alu nmvp nmt) ->
	ViewProjMemory smvp sbvp bnmvp alu nmvp ->
	RectGroups sd smr sbr bnmr nmr () -> WViewProj -> [WRect] ->
	IO () -> IO ()
run (pd, qfis, dv, gq, pq, cp, cb) pl
	wos@(WinObjs (_, fr) _ _ _ _ _) (vb, ib) ds mvp rgs vp rs go = do
	rb <- destroyRectBffr rgs () >> createRectBffr pd dv gq cp rgs () rs
	catchAndRecreate pd qfis dv pl (objsToRecreates wos)
		$ draw dv gq pq pl (objsToDraws wos) vb rb ib mvp ds cb vp
	Vk.Dvc.waitIdle dv
	(,) <$> GlfwG.Win.shouldClose (objsToWin wos) <*> checkFlag fr >>= \case
		(True, _) -> pure (); (_, False) -> go
		(_, _) -> recreateAll pd qfis dv pl (objsToRecreates wos) >> go

-- RECERATE

catchAndRecreate ::
	forall sd sl sdsl alu nmvp nmt sw ssfc scfmt ssc nmscv svs sr sfs sg .
	(Vk.T.FormatToValue scfmt, RecreateFrmbffrs svs sfs) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu nmvp nmt)] '[] ->
	Recreates sw ssfc scfmt ssc nmscv svs sr sfs sg sl sdsl alu nmvp nmt ->
	IO () -> IO ()
catchAndRecreate pd qfis dv pl rcs go = catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just (); Vk.SuboptimalKhr -> Just ()
		_ -> Nothing) go
	\_ -> recreateAll pd qfis dv pl rcs

data Recreates sw ssfc scfmt ssc nmscv sis sr sfs
	sg sl sdsl alu nmvp nmt = Recreates
	(GlfwG.Win.W sw) (Vk.Khr.Sfc.S ssfc) (TVar Vk.Extent2d)
	(Vk.Khr.Swpch.S scfmt ssc)
	(HPList.PL (Vk.ImgVw.I nmscv scfmt) sis) (Vk.RndrPss.R sr)
	(HPList.PL Vk.Frmbffr.F sfs)
	(Vk.Ppl.Gr.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(WRect, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3), '(9, TexCoord) ]
		'(sl, '[ '(sdsl, DscStLytArg alu nmvp nmt)], '[]))

objsToRecreates ::
	WinObjs sw ssfc scfmt ssc nm sscvs sr sfs
		sg sl sdsl alu nmvp nmt sias srfs siff ->
	Recreates sw ssfc scfmt ssc nm sscvs sr sfs sg sl sdsl alu nmvp nmt
objsToRecreates (WinObjs (w, _) sfc vex (sc, scvs, rp, fbs) gp _) =
	Recreates w sfc vex sc scvs rp fbs gp

recreateAll ::
	forall sd sl sdsl alu nmvp nmt sw ssfc scfmt ssc nmscv svs sr sfs sg .
	(Vk.T.FormatToValue scfmt, RecreateFrmbffrs svs sfs) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu nmvp nmt)] '[] ->
	Recreates sw ssfc scfmt ssc nmscv svs sr sfs sg sl sdsl alu nmvp nmt ->
	IO ()
recreateAll pd qfis dv pl (Recreates w sfc vex sc scvs rp fbs gp) = do
	waitFramebufferSize w >> Vk.Dvc.waitIdle dv
	ex <- recreateSwpch w sfc pd qfis dv sc
	atomically $ writeTVar vex ex
	Vk.Khr.Swpch.getImages dv sc >>= \is -> recreateImgVws dv is scvs
	recreateFrmbffrs dv ex rp scvs fbs
	recreateGrPpl dv ex rp pl gp

waitFramebufferSize :: GlfwG.Win.W sw -> IO ()
waitFramebufferSize w = size >>= \sz -> when (zero sz)
	$ fix \go -> (`when` go) . zero =<< GlfwG.waitEvents *> size
	where
	size = GlfwG.Win.getFramebufferSize w
	zero = uncurry (||) . ((== 0) *** (== 0))

-- DRAW

draw :: forall
	sd sl sdsl alu nmvp nmt scfmt ssc sr sfs sg sias srfs siff
	smv sbv bnmv nmv smr sbr bnmr nmr smi sbi bnmi nmi smvp sbvp bnmvp
	sds scb . KnownNat alu =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu nmvp nmt)] '[] ->
	Draws scfmt ssc sr sfs sg sl sdsl alu nmvp nmt sias srfs siff ->
	Vk.Bffr.Binded smv sbv bnmv '[Vk.ObjNA.List WVertex nmv] ->
	Vk.Bffr.Binded smr sbr bnmr '[Vk.ObjNA.List WRect nmr] ->
	Vk.Bffr.Binded smi sbi bnmi '[Vk.ObjNA.List Word16 nmi] ->
	ViewProjMemory smvp sbvp bnmvp alu nmvp ->
	Vk.DscSt.D sds '(sdsl, DscStLytArg alu nmvp nmt) -> Vk.CBffr.C scb ->
	WViewProj -> IO ()
draw dv gq pq pl (Draws vex sc rp fbs gp (SyncObjs ias rfs iff))
	vb rb ib mvp ds cb vp = atomically (readTVar vex) >>= \ex -> do
	Vk.Fence.waitForFs dv siff True Nothing >> Vk.Fence.resetFs dv siff
	ii <- Vk.Khr.acquireNextImageResult
		[Vk.Success, Vk.SuboptimalKhr] dv sc maxBound (Just ias) Nothing
	Vk.CBffr.reset cb def
	HPList.index fbs ii \fb -> recordCmdBffr cb rp fb ex pl gp vb rb ib ds
	updateViewProjBffr dv mvp vp
	Vk.Q.submit gq (HPList.Singleton $ U4 sinfo) $ Just iff
	catchSeq . Vk.Khr.queuePresent @'Nothing pq $ pinfo ii
	where
	siff = HPList.Singleton iff
	sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Singleton
			(Vk.SemaphorePipelineStageFlags ias
				Vk.Ppl.StageColorAttachmentOutputBit),
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
	catchSeq = (`catch`
		\(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)
	pinfo ii = Vk.Khr.PresentInfo {
		Vk.Khr.presentInfoNext = TMaybe.N,
		Vk.Khr.presentInfoWaitSemaphores = HPList.Singleton rfs,
		Vk.Khr.presentInfoSwapchainImageIndices =
			HPList.Singleton $ Vk.Khr.SwapchainImageIndex sc ii }

data Draws scfmt ssc sr sfs sg sl sdsl alu nmvp nmt sias srfs siff = Draws
	(TVar Vk.Extent2d)
	(Vk.Khr.Swpch.S scfmt ssc) (Vk.RndrPss.R sr) (HPList.PL Vk.Frmbffr.F sfs)
	(Vk.Ppl.Gr.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(WRect, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3), '(9, TexCoord) ]
		'(sl, '[ '(sdsl, DscStLytArg alu nmvp nmt)], '[]))
	(SyncObjs '(sias, srfs, siff))

objsToDraws ::
	WinObjs sw ssfc scfmt ssc nm sscivs sr sfs
		sg sl sdsl alu nmvp nmt sias srfs siff ->
	Draws scfmt ssc sr sfs sg sl sdsl alu nmvp nmt sias srfs siff
objsToDraws (WinObjs _ _sfc vex (sc, _scivs, rp, fbs) gp sos) =
	Draws vex sc rp fbs gp sos

recordCmdBffr :: forall
	scb sr sf sl sdsl alu nmvp nmt sg
	smv sbv bnmv nmv smr sbr bnmr nmr smi sbi bnmi nmi sds .
	Vk.CBffr.C scb -> Vk.RndrPss.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu nmvp nmt)] '[] ->
	Vk.Ppl.Gr.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(WRect, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3), '(9, TexCoord) ]
		'(sl, '[ '(sdsl, DscStLytArg alu nmvp nmt)], '[]) ->
	Vk.Bffr.Binded smv sbv bnmv '[Vk.ObjNA.List WVertex nmv] ->
	Vk.Bffr.Binded smr sbr bnmr '[Vk.ObjNA.List WRect nmr] ->
	Vk.Bffr.Binded smi sbi bnmi '[Vk.ObjNA.List Word16 nmi] ->
	Vk.DscSt.D sds '(sdsl, DscStLytArg alu nmvp nmt) -> IO ()
recordCmdBffr cb rp fb ex pl gp vb rb ib ds =
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpinfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gp \cbb ->
	Vk.Cmd.bindVertexBuffers cbb (
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @nmv vb) :**
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @WRect @nmr rb) :**
		HPList.Nil ) >>
	Vk.Cmd.bindIndexBuffer
		cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word16 @nmi ib) >>
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics pl
		(HPList.Singleton $ U2 ds)
		(HPList.Singleton $ HPList.Nil :** HPList.Nil :** HPList.Nil) >>
	Vk.Cmd.drawIndexed cbb (bffrLstLn ib) (bffrLstLn rb) 0 0 0
	where
	rpinfo :: Vk.RndrPss.BeginInfo 'Nothing sr sf
		'[ 'Vk.ClearTypeColor 'Vk.ClearColorTypeFloat32]
	rpinfo = Vk.RndrPss.BeginInfo {
		Vk.RndrPss.beginInfoNext = TMaybe.N,
		Vk.RndrPss.beginInfoRenderPass = rp,
		Vk.RndrPss.beginInfoFramebuffer = fb,
		Vk.RndrPss.beginInfoRenderArea = Vk.Rect2d {
			Vk.rect2dOffset = Vk.Offset2d 0 0,
			Vk.rect2dExtent = ex },
		Vk.RndrPss.beginInfoClearValues = HPList.Singleton
			. Vk.ClearValueColor . fromJust $ rgbaDouble 0 0 0 1 }

bffrLstLn :: Num n =>
	Vk.Bffr.Binded sm sb bnm '[Vk.Obj.ListMaybeName al v mnm] -> n
bffrLstLn b = fromIntegral sz
	where HPList.Singleton (Vk.Obj.LengthList' sz) = Vk.Bffr.lengthBinded b

updateViewProjBffr :: forall sd sm sb al bnm nm . KnownNat al =>
	Vk.Dvc.D sd -> ViewProjMemory sm sb bnm al nm -> WViewProj -> IO ()
updateViewProjBffr dvc um obj =
	Vk.Mm.write @bnm @(Vk.Obj.AtomNew al WViewProj nm) @0 dvc um zeroBits obj

-- DATA TYPES

data Rectangle = Rectangle {
	rectanglePos' :: RectPos, rectangleSize' :: RectSize,
	rectangleColor' :: RectColor, rectangleModel' :: RectModel }
	deriving (Show, Generic)

rectToRectRaw :: Rectangle -> WRect
rectToRectRaw Rectangle {
	rectanglePos' = p, rectangleSize' = s,
	rectangleColor' = c, rectangleModel' = RectModel m } =
	StrG.W RectangleRaw {
		rectanglePos = p, rectangleSize = s,
		rectangleColor = c,
		rectangleModel0 = RectModel0 m0,
		rectangleModel1 = RectModel1 m1,
		rectangleModel2 = RectModel2 m2,
		rectangleModel3 = RectModel3 m3 }
	where m0 :. m1 :. m2 :. m3 :. NilL = Cglm.mat4ToVec4s m

type WRect = StrG.W RectangleRaw

data RectangleRaw = RectangleRaw {
	rectanglePos :: RectPos, rectangleSize :: RectSize,
	rectangleColor :: RectColor,
	rectangleModel0 :: RectModel0, rectangleModel1 :: RectModel1,
	rectangleModel2 :: RectModel2, rectangleModel3 :: RectModel3 }
	deriving (Show, Generic)

dummyRect :: [WRect]
dummyRect =
	let m0 :. m1 :. m2 :. m3 :. NilL = Cglm.mat4ToVec4s Cglm.mat4Identity in
	[StrG.W $ RectangleRaw
		(RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
		(RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
		(RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 0.0 :. NilL)
		(RectModel0 m0) (RectModel1 m1)
		(RectModel2 m2) (RectModel3 m3)]

instance StrG.G RectangleRaw where

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

type WVertex = StrG.W Vertex

data Vertex = Vertex {
	vertexPos :: Cglm.Vec2, vertexColor :: Cglm.Vec3,
	vertexTexCoord :: TexCoord }
	deriving (Show, Generic)

instance StrG.G Vertex

newtype TexCoord = TexCoord Cglm.Vec2
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

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

type WViewProj = StrG.W ViewProj

data ViewProj = ViewProj {
	viewProjectionView :: Cglm.Mat4, viewProjectionProj :: Cglm.Mat4 }
	deriving (Show, Generic)

instance StrG.G ViewProj

-- SHADERS

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
