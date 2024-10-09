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
import GHC.TypeNats
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.Storable.Generic qualified as StrG
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
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
import Gpu.Vulkan.Object qualified as Vk.Obj
import Gpu.Vulkan.Object.NoAlignment qualified as Vk.ObjNA
import Gpu.Vulkan.AllocationCallbacks qualified as Vk.AllocationCallbacks
import Gpu.Vulkan.Instance.Internal qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Semaphore qualified as Vk.Smph
import Gpu.Vulkan.Fence qualified as Vk.Fnc
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
import Gpu.Vulkan.Component qualified as Vk.Component
import Gpu.Vulkan.ColorComponent qualified as Vk.ClrCmp

import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.DbgUtls.Msgr
import Gpu.Vulkan.Cglm qualified as Cglm

import Graphics.UI.GlfwG as GlfwG
import Graphics.UI.GlfwG.Window as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type as GlfwG.Win
import Graphics.UI.GlfwG.Key as GlfwG.Ky
import Graphics.UI.GlfwG.Mouse as GlfwG.Ms

import Graphics.UI.GLFW qualified as Glfw (setWindowShouldClose)

import Debug
import Data.Ord.ToolsYj
import Data.Bits.ToolsYj
import Data.Maybe.ToolsYj
import Data.Either.ToolsYj
import Data.List.ToolsYj
import Data.Bool.ToolsYj
import Control.Concurrent.STM.ToolsYj

import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC

import Gpu.Vulkan.PushConstant qualified as Vk.PushConstant

----------------------------------------------------------------------
--
-- * RECTANGLES
-- * BODY
-- * PROVIDE WINDOW OBJECTS
-- * CREATE AND COPY BUFFERS
-- * MAINLOOP
-- * RECREATE
-- * DRAW
-- * DATA TYPES
-- * SHADERS
--
----------------------------------------------------------------------

-- RECTANGLES

rectangles :: (Ord k, Succable k) =>
	TChan (Command k) -> TChan (Event k) ->
	TVar (M.Map k (TVar Vk.Extent2d)) -> IO ()
rectangles ip op vex = GlfwG.init error $
	createIst \ist -> Vk.Dvc.group nil \dvg -> bool id (dbgm ist) debug $
	GlfwG.Win.group \wg -> initWin False wg () >>= \dw ->
	crsfc dw ist \dsfc -> pickPhd ist dsfc >>= \(pd, qfis) ->
	querySwpchSupport pd dsfc \ssd ->
	chooseSwpSfcFmt (formats ssd) \(_ :: Vk.Khr.Sfc.Format fmt) ->
	createLgDvc pd dvg () qfis >>= \(dv, gq, pq) ->
	swapExtent dw (capabilities ssd) >>= \ex ->
	swpchImgNum @fmt dv dsfc ssd ex qfis >>= \n -> num n \(_ :: Proxy n) ->
	body @n @fmt ip op vex ist pd qfis dv gq pq >>
	atomically (writeTChan op EventEnd)
	where
	dbgm i = Vk.DbgUtls.Msgr.create i dbgMsngrInfo nil
	crsfc :: GlfwG.Win.W sw -> Vk.Ist.I si ->
		(forall ss . Vk.Khr.Sfc.S ss -> IO a) -> IO a
	crsfc w i f = Vk.Khr.Sfc.group i nil \sg ->
		Vk.Khr.Sfc.Glfw.Win.create' sg () w >>= f . forceRight'
	num :: [a] -> (forall (n :: [()]) .
		(HPList.HomoListN n, NumToVal n) => Proxy n -> b) -> b
	num [] f = f (Proxy :: Proxy '[])
	num (_ : xs) f =
		num xs \(Proxy :: Proxy n) -> f (Proxy :: Proxy ('() ': n))

data Command k
	= OpenWindow | DestroyWindow k | GetEvent
	| Draw (M.Map k (ViewProjection, [Rectangle]))
	deriving Show

data Event k
	= EventOpenWindow k | EventDeleteWindow k | EventEnd
	| EventKeyDown k GlfwG.Ky.Key | EventKeyUp k GlfwG.Ky.Key
	| EventMouseButtonDown k GlfwG.Ms.MouseButton
	| EventMouseButtonUp k GlfwG.Ms.MouseButton
	| EventCursorPosition k Double Double
	deriving Show

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
		Vk.applicationInfoApplicationName = "Rectangles",
		Vk.applicationInfoApplicationVersion =
			Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoEngineName = "No Engine",
		Vk.applicationInfoEngineVersion = Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoApiVersion = Vk.apiVersion_1_0 }

vldLayers :: [Vk.LayerName]
vldLayers = [Vk.layerKhronosValidation]

dbgMsngrInfo :: Vk.DbgUtls.Msgr.CreateInfo 'Nothing '[] ()
dbgMsngrInfo = Vk.DbgUtls.Msgr.CreateInfo {
	Vk.DbgUtls.Msgr.createInfoNext = TMaybe.N,
	Vk.DbgUtls.Msgr.createInfoFlags = zeroBits,
	Vk.DbgUtls.Msgr.createInfoMessageSeverity =
		Vk.DbgUtls.MessageSeverityVerboseBit .|.
		Vk.DbgUtls.MessageSeverityWarningBit .|.
		Vk.DbgUtls.MessageSeverityErrorBit,
	Vk.DbgUtls.Msgr.createInfoMessageType =
		Vk.DbgUtls.MessageTypeGeneralBit .|.
		Vk.DbgUtls.MessageTypeValidationBit .|.
		Vk.DbgUtls.MessageTypePerformanceBit,
	Vk.DbgUtls.Msgr.createInfoFnUserCallback = dbgcb,
	Vk.DbgUtls.Msgr.createInfoUserData = Nothing }
	where dbgcb _svr _tp d _ud = False <$ Txt.putStrLn
		("validation layer: " <> Vk.DbgUtls.Msgr.callbackDataMessage d)

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

data QFamIdcs = QFamIdcs { grFam :: Vk.QFam.Index, prFam :: Vk.QFam.Index }

findQFams :: Vk.Phd.P -> Vk.Khr.Sfc.S ss -> IO (Maybe QFamIdcs)
findQFams pd sfc = do
	prps@((fst <$>) -> is) <- Vk.Phd.getQueueFamilyProperties pd
	mp <- listToMaybe
		<$> filterM (flip (Vk.Khr.Sfc.Phd.getSupport pd) sfc) is
	pure $ QFamIdcs <$> (fst <$> L.find (grbit . snd) prps) <*> mp
	where grbit = checkBits Vk.Q.GraphicsBit . Vk.QFam.propertiesQueueFlags

dvcExtensions :: [Vk.Phd.ExtensionName]
dvcExtensions = [Vk.Khr.Swpch.extensionName]

createLgDvc :: (Ord k, Vk.AllocationCallbacks.ToMiddle ma) =>
	Vk.Phd.P -> Vk.Dvc.Group ma sd k -> k -> QFamIdcs ->
	IO (Vk.Dvc.D sd, Vk.Q.Q, Vk.Q.Q)
createLgDvc pd dvg k qfis =
	hetero qinfo unqqfs \qs ->
	Vk.Dvc.create' pd dvg k (cinfo qs) >>= \(forceRight' -> dv) -> (,,) dv
		<$> Vk.Dvc.getQueue dv (grFam qfis) 0
		<*> Vk.Dvc.getQueue dv (prFam qfis) 0
	where
	hetero :: WithPoked (TMaybe.M s) => (a -> t s) -> [a] -> (forall ss .
		HPList.ToListWithCM' WithPoked TMaybe.M ss =>
		HPList.PL t ss -> b) -> b
	hetero _ [] f = f HPList.Nil
	hetero g (x : xs) f = hetero g xs \xs' -> f (g x :** xs')
	qinfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }
	unqqfs = L.nub [grFam qfis, prFam qfis]
	cinfo qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames = bool [] vldLayers debug,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures = Just def }

swpchImgNum :: forall (fmt :: Vk.T.Format) sd ssfc fmts .
	Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Khr.Sfc.S ssfc -> SwpchSupportDetails fmts ->
	Vk.Extent2d -> QFamIdcs -> IO [()]
swpchImgNum dv sfc ssd ex qfis =
	Vk.Khr.Swpch.group @fmt dv nil \scg ->
	createSwpch' sfc qfis scg () ssd ex >>= \sc ->
	map (const ()) <$> Vk.Khr.Swpch.getImages dv sc

-- BODY

body :: forall
	(n :: [()]) (scfmt :: Vk.T.Format)
	k si sd . (
	HPList.HomoListN n, NumToVal n, Ord k, Succable k,
	Vk.T.FormatToValue scfmt ) =>
	TChan (Command k) -> TChan (Event k) ->
	TVar (M.Map k (TVar Vk.Extent2d)) -> Vk.Ist.I si -> Vk.Phd.P ->
	QFamIdcs -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> IO ()
body ip op vex ist pd qfis dv gq pq =
	createCmdPl qfis dv \cp -> createCmdBffr dv cp \cb ->
	let	dvs = (pd, qfis, dv, gq, pq, cp, cb) in
	unfrmBffrOstAlgn pd \(_ :: Proxy alu) ->
	createPplLyt @alu dv \dsl pl ->
	createVtxBffr pd dv gq cp vertices \vb ->
	createIdxBffr pd dv gq cp indices \ib -> let vbs = (vb, ib) in
	createViewProjBffr pd dv \vp vpm ->
	GlfwG.Win.group \wg -> Vk.Khr.Sfc.group ist nil \sfcg ->
	Vk.Khr.Swpch.group dv nil \scg ->
	Vk.ImgVw.group dv nil
		\(ivg :: Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm ivfmt) ->
	Vk.RndrPss.group dv nil \rpg ->
	Vk.Frmbffr.group dv nil
		\(fbg :: Vk.Frmbffr.Group sd 'Nothing sf (k, Int)) ->
	Vk.Ppl.Gr.group dv nil \gpg ->
	Vk.Smph.group dv nil \iasg -> Vk.Smph.group dv nil \rfsg ->
	Vk.Fnc.group dv nil \iffg ->
	Vk.Bffr.group dv nil \rbg -> Vk.Mm.group dv nil \rmg ->
	let	rgs = (rbg, rmg) in
	createDscPl dv \dp -> createDscSt dv dp vp dsl \ds ->
	let	dss = (ds, vpm) in
	atomically (newTVar M.empty) >>= \ges ->

	let	crwos = provideWinObjs @n @scfmt
			op vex pd dv gq cp qfis pl wg gs rgs ges
		drwos = destroyWinObjs @n wg gs rgs
		gs = (sfcg, scg, ivg, rpg, fbg, gpg, iasg, rfsg, iffg) in

	mainloop @n @siv @sf ip op dvs pl crwos drwos vbs rgs dss ges

createCmdPl ::
	QFamIdcs -> Vk.Dvc.D sd -> (forall sc . Vk.CmdPl.C sc -> IO a) -> IO a
createCmdPl qfis dv = Vk.CmdPl.create dv pinfo nil
	where pinfo = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = grFam qfis }

createCmdBffr :: forall sd scp a .
	Vk.Dvc.D sd -> Vk.CmdPl.C scp ->
	(forall scb . Vk.CmdBffr.C scb -> IO a) -> IO a
createCmdBffr dv cp f =
	Vk.CmdBffr.allocate dv ainfo $ f . \(cb :*. HPList.Nil) -> cb
	where
	ainfo :: Vk.CmdBffr.AllocateInfo 'Nothing scp '[ '()]
	ainfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }

unfrmBffrOstAlgn ::
	Vk.Phd.P -> (forall a . KnownNat a => Proxy a -> IO b) -> IO b
unfrmBffrOstAlgn pd f = (\(SomeNat p) -> f p) . someNatVal . fromIntegral
	. Vk.Phd.limitsMinUniformBufferOffsetAlignment . Vk.Phd.propertiesLimits
	=<< Vk.Phd.getProperties pd

createPplLyt :: forall alu sd mnmvp b . Vk.Dvc.D sd -> (forall sdsl sl .
	Vk.DscStLyt.D sdsl (DscStLytArg alu mnmvp) ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[] -> IO b) -> IO b
createPplLyt dv f = createDSLyt dv \l -> Vk.PplLyt.create dv (info l) nil $ f l
	where
	info :: Vk.DscStLyt.D sdsl (DscStLytArg alu mnmvp) ->
		Vk.PplLyt.CreateInfo 'Nothing
			'[ '(sdsl, DscStLytArg alu mnmvp)]
			('Vk.PushConstant.Layout '[] '[])
	info l = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 l }

createDSLyt :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscStLyt.D s (DscStLytArg alu mnmvp)  -> IO a) -> IO a
createDSLyt dv = Vk.DscStLyt.create dv info nil
	where
	info = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = HPList.Singleton vpb }
	vpb = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }

type DscStLytArg alu mnmvp = '[ 'Vk.DscStLyt.Buffer '[AtomViewProj alu mnmvp]]

createVtxBffr ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> [WVertex] ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List WVertex nm] ->
		IO a) -> IO a
createVtxBffr = createBffrMem Vk.Bffr.UsageVertexBufferBit

createIdxBffr ::
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> [Word16] ->
	(forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List Word16 nm] ->
		IO a) -> IO a
createIdxBffr = createBffrMem Vk.Bffr.UsageIndexBufferBit

createBffrMem :: forall sd sc t bnm nm a . Storable' t =>
	Vk.Bffr.UsageFlags -> Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> [t] -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.Obj.List 1 t nm] -> IO a) -> IO a
createBffrMem us pd dv gq cp xs@(fromIntegral . length -> ln) f =
	createBffrLst pd dv ln (Vk.Bffr.UsageTransferDstBit .|. us)
		Vk.Mm.PropertyDeviceLocalBit \b _ -> do
	createBffrLst pd dv ln
		Vk.Bffr.UsageTransferSrcBit
		(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)
		\(b' :: Vk.Bffr.Binded sm sb bnm' '[Vk.Obj.List al t nm']) m ->
		Vk.Mm.write @bnm' @(Vk.Obj.List al t nm') @0 dv m zeroBits xs >>
		copyBffrLst dv gq cp b' b
	f b

createViewProjBffr :: KnownNat alu => Vk.Phd.P -> Vk.Dvc.D sd -> (forall sm sb .
	Vk.Bffr.Binded sm sb bnmvp '[AtomViewProj alu mnmvp]  ->
	ViewProjMemory sm sb bnmvp alu mnmvp -> IO a) -> IO a
createViewProjBffr pd dv = createBffrAtm pd dv
	Vk.Bffr.UsageUniformBufferBit
	(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)

type ViewProjMemory sm sb bnm alu mnm =
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg bnm '[AtomViewProj alu mnm])]

type AtomViewProj alu mnm = Vk.Obj.AtomMaybeName alu WViewProj mnm

createDscPl :: Vk.Dvc.D sd -> (forall sp . Vk.DscPl.P sp -> IO a) -> IO a
createDscPl dv = Vk.DscPl.create dv info nil
	where
	info = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = 1,
		Vk.DscPl.createInfoPoolSizes = [szvp] }
	szvp = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPl.sizeDescriptorCount = 1 }

createDscSt :: KnownNat alu => Vk.Dvc.D sd -> Vk.DscPl.P sp ->
	Vk.Bffr.Binded sm sb nm '[Vk.Obj.AtomMaybeName alu WViewProj mnmvp] ->
	Vk.DscStLyt.D sdsl (DscStLytArg alu mnmvp) ->
	(forall sds .
		Vk.DscSt.D sds '(sdsl, DscStLytArg alu mnmvp) -> IO a) -> IO a
createDscSt dv dp vpb dsl f =
	Vk.DscSt.allocateDs dv ainfo \(HPList.Singleton ds) ->
	Vk.DscSt.updateDs dv
		(HPList.Singleton . U5 $ dscWrite vpb ds) HPList.Nil >> f ds
	where ainfo = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }

dscWrite :: KnownNat alu =>
	Vk.Bffr.Binded sm sb nm '[Vk.Obj.Atom alu WViewProj mnmvp] ->
	Vk.DscSt.D sds slbts ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer
		'[ '(sm, sb, nm, Vk.Obj.Atom alu WViewProj mnmvp, 0)]) 0
dscWrite vpb ds = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos
		. HPList.Singleton . U5 $ Vk.Dsc.BufferInfo vpb }

-- PROVIDE WINDOW OBJECTS

provideWinObjs :: forall (n :: [()]) (scfmt :: Vk.T.Format) k
	si sd sc sl sdsl alu mnmvp sw ssfc ssc siv nmi sr sf sg
	sias srfs siff smr sbr bnmr nmr .
	(HPList.HomoListN n, Vk.T.FormatToValue scfmt, Ord k) =>
	TChan (Event k) -> TVar (M.Map k (TVar Vk.Extent2d)) ->
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> QFamIdcs ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[] ->
	GlfwG.Win.Group sw k ->
	WinObjGroups k si ssfc sd scfmt
		ssc siv nmi sr sf sg sl sdsl alu mnmvp sias srfs siff ->
	RectGroups sd smr sbr bnmr nmr k -> TVar (M.Map k (IO ())) -> k ->
	IO (WinObjs sw ssfc sg sl sdsl alu mnmvp sias srfs siff scfmt ssc nmi
		(HPList.Replicate n siv) sr (HPList.Replicate n sf))
provideWinObjs outp vext_ phd dv gq cp qfis pllyt
	wg (sfcgrp, scgrp, ivgrp, rpgrp, fbgrp, gpgrp, iasgrp, rfsgrp, iffgrp) rgrps ges k =
	(,) <$> initWin True wg k <*> atomically (newTVar False) >>= \(w, fr) ->
	winObjs @n outp phd dv gq cp qfis pllyt vext_
		sfcgrp rpgrp gpgrp rgrps iasgrp rfsgrp iffgrp scgrp ivgrp fbgrp ges w fr k

initWin :: Ord k => Bool -> GlfwG.Win.Group sw k -> k -> IO (GlfwG.Win.W sw)
initWin v wg k = do
	GlfwG.Win.hint
		$ GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI
	GlfwG.Win.hint $ GlfwG.Win.WindowHint'Visible v
	(forceRight' -> w) <-
		uncurry (GlfwG.Win.create' wg k) wSize wName Nothing Nothing
	pure w
	where wName = "Rectangles"; wSize = (800, 600)

winObjs :: forall (n :: [()]) (scfmt :: Vk.T.Format) k
	si sd sc sw ssfc sg sl sdsl sias srfs siff ssc nm siv sr sf
	smrct sbrct nmrct alu mnmvp nmr . (
	HPList.HomoListN n, Vk.T.FormatToValue scfmt, Ord k ) =>
	TChan (Event k) -> Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C sc ->
	QFamIdcs -> Vk.PplLyt.P sl '[AtomUbo sdsl alu mnmvp] '[] ->
	TVar (M.Map k (TVar Vk.Extent2d)) ->
	Vk.Khr.Sfc.Group si 'Nothing ssfc k ->
	Vk.RndrPss.Group sd 'Nothing sr k ->
	Vk.Ppl.Gr.Group sd 'Nothing sg k '[ '(
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(WRect, 'Vk.VtxInp.RateInstance) ],
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3) ],
		'(sl, '[AtomUbo sdsl alu mnmvp], '[]) )] ->
	(	Vk.Bffr.Group sd 'Nothing sbrct k nmrct '[Vk.Obj.List 256 WRect nmr],
		Vk.Mm.Group sd 'Nothing smrct k '[ '(sbrct, Vk.Mm.BufferArg nmrct '[Vk.Obj.List 256 WRect nmr])]
		) ->
	Vk.Smph.Group sd 'Nothing sias k ->
	Vk.Smph.Group sd 'Nothing srfs k ->
	Vk.Fnc.Group sd 'Nothing siff k ->
	Vk.Khr.Swpch.Group sd 'Nothing scfmt ssc k ->
	Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm scfmt ->
	Vk.Frmbffr.Group sd 'Nothing sf (k, Int) ->
	TVar (M.Map k (IO ())) ->
	GlfwG.Win.W sw -> TVar Bool ->
	k ->
	IO (WinObjs
		sw ssfc sg sl sdsl alu mnmvp sias srfs siff scfmt ssc nm
		(HPList.Replicate n siv) sr (HPList.Replicate n sf))
winObjs outp phd dv gq cp qfis pllyt vext_
	sfcgrp rpgrp gpgrp rgrps iasgrp rfsgrp iffgrp scgrp ivgrp fbgrp ges w fbrszd k =
	GlfwG.Win.setKeyCallback w
		(Just \dw ky sc act mods -> do
			putStrLn $
				show dw ++ " " ++ show ky ++ " " ++
				show sc ++ " " ++ show act ++ " " ++ show mods
			case act of
				GlfwG.Ky.KeyState'Pressed ->
					atomically $ writeTChan outp $ EventKeyDown k ky
				GlfwG.Ky.KeyState'Released ->
					atomically $ writeTChan outp $ EventKeyUp k ky
				_ -> pure ()
			) >>
	GlfwG.Win.setFramebufferSizeCallback w
		(Just \_ _ _ -> atomically $ writeTVar fbrszd True) >>

	Vk.Khr.Sfc.Glfw.Win.create' sfcgrp k w >>= \(forceRight' -> sfc) ->
	createRenderPass @scfmt rpgrp k >>= \rp ->
	prepareSwpch w sfc phd \spp' ext ->

	createRectangleBuffer' phd dv gq cp rgrps k dummy >>

	atomically (
		newTVar (Vk.Extent2d 0 0) >>= \v ->
		writeTVar v ext >>
		v <$ modifyTVar vext_ (M.insert k v) ) >>= \vext ->

	createGraphicsPipeline gpgrp k ext rp pllyt >>= \gpl ->
	createSyncObjects iasgrp rfsgrp iffgrp k >>= \sos ->
	createSwpch' sfc qfis scgrp k spp' ext >>= \sc ->

	Vk.Khr.Swpch.getImages dv sc >>= \scis ->
	createImageViews' @n ivgrp k scis >>= \scivs ->
	createFramebuffers' @n @siv fbgrp k ext rp scivs >>= \fbs ->

	let	wos = WinObjs
			(w, fbrszd) sfc vext gpl sos (sc, scivs, rp, fbs) in

	setGlfwEvents outp w fbrszd ges k >>

	pure wos

destroyWinObjs :: forall (n :: [()]) (scfmt :: Vk.T.Format) k
	si sd sw ssfc sg sl sdsl sias srfs siff ssc nmi mnmvp siv sr sf
	smrct sbrct nmrct alu nmr . (NumToVal n,  Ord k) =>
	Group sw k ->
	WinObjGroups k si ssfc sd scfmt
		ssc siv nmi sr sf sg sl sdsl alu mnmvp sias srfs siff ->
	(	Vk.Bffr.Group sd 'Nothing sbrct k nmrct '[Vk.Obj.List 256 WRect nmr],
		Vk.Mm.Group sd 'Nothing smrct k '[ '(sbrct, Vk.Mm.BufferArg nmrct '[Vk.Obj.List 256 WRect nmr])]
		) ->
	k -> IO ()
destroyWinObjs
	wgrp (sfcgrp, scgrp, ivgrp, rpgrp, fbgrp, gpgrp, iasgrp, rfsgrp, iffgrp) (rbgrp, rmgrp) k = do
	mw <- GlfwG.Win.lookup wgrp k
	case mw of
		Nothing -> pure ()
		Just (GlfwG.Win.W w) -> do
			Glfw.setWindowShouldClose w True
			either error pure =<< unsafeDestroy wgrp k

			either error pure =<< Vk.Khr.Swpch.unsafeDestroy scgrp k
			either error pure =<< Vk.Khr.Sfc.unsafeDestroy sfcgrp k

			either error pure =<< Vk.RndrPss.unsafeDestroy rpgrp k
			either error pure =<< Vk.Ppl.Gr.unsafeDestroyGs gpgrp k
			either error pure =<< Vk.Bffr.unsafeDestroy rbgrp k
			either error pure =<< Vk.Mm.unsafeFree rmgrp k
			either error pure =<< Vk.Smph.unsafeDestroy iasgrp k
			either error pure =<< Vk.Smph.unsafeDestroy rfsgrp k
			either error pure =<< Vk.Fnc.unsafeDestroy iffgrp k
			for_ [0 .. numToValue @n - 1] \i -> do
				either error pure =<< Vk.ImgVw.unsafeDestroy ivgrp (k, i)
				either error pure =<< Vk.Frmbffr.unsafeDestroy fbgrp (k, i)

class NumToVal (n :: [()]) where numToValue :: Int
instance NumToVal '[] where numToValue = 0

instance NumToVal n => NumToVal ('() ': n) where
	numToValue = 1 + numToValue @n

data WinObjs sw ssfc sg sl sdsl alu mnmvp sias srfs siff scfmt ssc nm ss sr sfs = WinObjs
	(WinEnvs sw) (Vk.Khr.Sfc.S ssfc) (TVar Vk.Extent2d)
	(Pipeline sg sl sdsl alu mnmvp) (SyncObjects '(sias, srfs, siff))
	(Swapchains scfmt ssc nm ss sr sfs)

type WinObjGroups k si ssfc sd scfmt
	ssc siv nmi sr sf sg sl sdsl alu mnmvp sias srfs siff = (
	Vk.Khr.Sfc.Group si 'Nothing ssfc k,
	Vk.Khr.Swpch.Group sd 'Nothing scfmt ssc k,
	Vk.ImgVw.Group sd 'Nothing siv (k, Int) nmi scfmt,
	Vk.RndrPss.Group sd 'Nothing sr k,
	Vk.Frmbffr.Group sd 'Nothing sf (k, Int),
	Vk.Ppl.Gr.Group sd 'Nothing sg k '[ '(
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(WRect, 'Vk.VtxInp.RateInstance) ],
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3) ],
		'(sl, '[ '(sdsl, DscStLytArg alu mnmvp)], '[]) )],
	Vk.Smph.Group sd 'Nothing sias k, Vk.Smph.Group sd 'Nothing srfs k,
	Vk.Fnc.Group sd 'Nothing siff k )

-- sfcg scg ivg rpg fbg gpg siasg srfsg siffg

type WinEnvs sw = (GlfwG.Win.W sw , FramebufferResized)
type FramebufferResized = TVar Bool

-- CREATE AND COPY BUFFERS
-- MAINLOOP
-- RECREATE
-- DRAW
-- DATA TYPES
-- SHADERS

querySwpchSupport :: Vk.Phd.P -> Vk.Khr.Sfc.S ssfc -> (forall fmts .
	Show (HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts) =>
	SwpchSupportDetails fmts -> IO a) -> IO a
querySwpchSupport pd sfc f = Vk.Khr.Sfc.Phd.getFormats pd sfc \fmts -> f
	=<< SwpchSupportDetails
		<$> Vk.Khr.Sfc.Phd.getCapabilities pd sfc
		<*> ((, fmts) <$> Vk.Khr.Sfc.Phd.getFormatsFiltered pd sfc)
		<*> Vk.Khr.Sfc.Phd.getPresentModes pd sfc

data SwpchSupportDetails fmts = SwpchSupportDetails {
	capabilities :: Vk.Khr.Sfc.Capabilities,
	formats :: (
		[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
		HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ),
	presentModes :: [Vk.Khr.Sfc.PresentMode] }

prepareSwpch :: forall sw ssfc a .
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	(forall fmts . SwpchSupportDetails fmts -> Vk.Extent2d -> IO a) -> IO a
prepareSwpch w sfc pd f =
	querySwpchSupport pd sfc \sd -> swapExtent w (capabilities sd) >>= f sd

createSwpch' :: forall ssfc sd ma scfmt ssc k fmts .
	(Vk.AllocationCallbacks.ToMiddle ma, Vk.T.FormatToValue scfmt, Ord k) =>
	Vk.Khr.Sfc.S ssfc -> QFamIdcs -> Vk.Khr.Swpch.Group sd ma scfmt ssc k ->
	k -> SwpchSupportDetails fmts -> Vk.Extent2d ->
	IO (Vk.Khr.Swpch.S scfmt ssc)
createSwpch' sfc qfis scg k ssd ex = (\(forceRight' -> sc) -> sc)
	<$> Vk.Khr.Swpch.create' @scfmt scg k (swpchInfoSsd sfc qfis ssd ex)

recreateSwpch :: Vk.T.FormatToValue scfmt =>
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc ->
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd -> Vk.Khr.Swpch.S scfmt ssc ->
	IO Vk.Extent2d
recreateSwpch w sfc pd qfis dv sc = querySwpchSupport pd sfc \ssd ->
	swapExtent w (capabilities ssd) >>= \ex -> ex <$
	Vk.Khr.Swpch.unsafeRecreate dv (swpchInfoSsd sfc qfis ssd ex) nil sc

swpchInfoSsd ::
	Vk.Khr.Sfc.S ss -> QFamIdcs -> SwpchSupportDetails fmts ->
	Vk.Extent2d -> Vk.Khr.Swpch.CreateInfo 'Nothing ss fmt
swpchInfoSsd sfc qfis ssd ex = chooseSwpSfcFmt (formats ssd)
	\(Vk.Khr.Sfc.Format sc :: Vk.Khr.Sfc.Format fmt) ->
	swpchInfo sfc qfis (capabilities ssd) sc (pmd $ presentModes ssd) ex
	where pmd = fromMaybe Vk.Khr.Sfc.PresentModeFifo
		. L.find (== Vk.Khr.Sfc.PresentModeMailbox)

chooseSwpSfcFmt :: (
	[Vk.Khr.Sfc.Format Vk.T.FormatB8g8r8a8Srgb],
	HPListC.PL Vk.T.FormatToValue Vk.Khr.Sfc.Format fmts ) ->
	(forall fmt . Vk.T.FormatToValue fmt => Vk.Khr.Sfc.Format fmt -> a) -> a
chooseSwpSfcFmt (fmts, (fmt0 :^* _)) f = maybe (f fmt0) f $ (`L.find` fmts)
	$ (== Vk.Khr.Sfc.ColorSpaceSrgbNonlinear) . Vk.Khr.Sfc.formatColorSpace
chooseSwpSfcFmt (_, HPListC.Nil) _ = error "no available swap surface formats"

swpchInfo :: forall ssfc fmt .
	Vk.Khr.Sfc.S ssfc -> QFamIdcs -> Vk.Khr.Sfc.Capabilities ->
	Vk.Khr.Sfc.ColorSpace -> Vk.Khr.Sfc.PresentMode -> Vk.Extent2d ->
	Vk.Khr.Swpch.CreateInfo 'Nothing ssfc fmt
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

swapExtent :: GlfwG.Win.W sw -> Vk.Khr.Sfc.Capabilities -> IO Vk.Extent2d
swapExtent win cps
	| Vk.extent2dWidth c /= maxBound = pure c
	| otherwise = (<$> GlfwG.Win.getFramebufferSize win)
		\(fromIntegral -> w, fromIntegral -> h) ->
		Vk.Extent2d
			(clamp (Vk.extent2dWidth n) (Vk.extent2dWidth x) w)
			(clamp (Vk.extent2dHeight n) (Vk.extent2dHeight x) h)
	where
	c = Vk.Khr.Sfc.capabilitiesCurrentExtent cps
	n = Vk.Khr.Sfc.capabilitiesMinImageExtent cps
	x = Vk.Khr.Sfc.capabilitiesMaxImageExtent cps

createImageViews' :: forall n ivfmt sd si siv k sm nm ifmt . (
	HPList.HomoListN n, Ord k, Vk.T.FormatToValue ivfmt ) =>
	Vk.ImgVw.Group sd 'Nothing siv (k, Int) nm ivfmt ->
	k -> [Vk.Img.Binded sm si nm ifmt] ->
	IO (HPList.PL (Vk.ImgVw.I nm ivfmt) (HPList.Replicate n siv))
createImageViews' ivgrp k imgs = do
	ivs <- createImageViewList ivgrp k imgs
	pure $ HPList.homoListNFromList @_ @n ivs
--	pure $ homoListFromList @_ @n ivs

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

fromRight :: Either String a -> a
fromRight (Left emsg) = error emsg
fromRight (Right x) = x

recreateImageViews :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss nm scfmt] -> HPList.PL (Vk.ImgVw.I nm scfmt) sis -> IO ()
recreateImageViews _dvc [] HPList.Nil = pure ()
recreateImageViews dvc (sci : scis) (iv :** ivs) =
	Vk.ImgVw.unsafeRecreate dvc (mkImageViewCreateInfoNew sci) nil iv >>
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

type AtomUbo s alu mnmvp = '(s, '[ 'Vk.DscStLyt.Buffer '[Vk.Obj.AtomMaybeName alu WViewProj mnmvp]])

createGraphicsPipeline :: (Ord k, Vk.AllocationCallbacks.ToMiddle mac) =>
	Vk.Ppl.Gr.Group sd mac sg k '[ '(
		'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(WRect, 'Vk.VtxInp.RateInstance)],
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3) ],
			'(sl, '[AtomUbo sdsl alu mnmvp], '[]) )] -> k ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[AtomUbo sdsl alu mnmvp] '[] -> IO (
		Vk.Ppl.Gr.G sg
			'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(WRect, 'Vk.VtxInp.RateInstance)]
			'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
				'(2, RectPos), '(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3) ]
			'(sl, '[AtomUbo sdsl alu mnmvp], '[]))
createGraphicsPipeline gpgrp k sce rp pllyt =
	Vk.Ppl.Gr.createGs' gpgrp k Nothing (U14 pplInfo :** HPList.Nil)
			>>= \(fromRight -> (U3 gpl :** HPList.Nil)) -> pure gpl
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp pllyt

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[AtomUbo sdsl alu mnmvp] '[] ->
	Vk.Ppl.Gr.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(WRect, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[AtomUbo sdsl alu mnmvp], '[]) -> IO ()
recreateGraphicsPipeline dvc sce rp pllyt gpls = Vk.Ppl.Gr.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HPList.Nil) nil (U3 gpls :** HPList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp pllyt

mkGraphicsPipelineCreateInfo' ::
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[AtomUbo sdsl alu mnmvp] '[] ->
	Vk.Ppl.Gr.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing,
			'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(WRect, 'Vk.VtxInp.RateInstance)],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
				'(2, RectPos), '(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3) ] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing '(sl, '[AtomUbo sdsl alu mnmvp], '[]) sr '(sb, vs', ts', foo)
mkGraphicsPipelineCreateInfo' sce rp pllyt = Vk.Ppl.Gr.CreateInfo {
	Vk.Ppl.Gr.createInfoNext = TMaybe.N,
	Vk.Ppl.Gr.createInfoFlags = Vk.Ppl.CreateFlagsZero,
	Vk.Ppl.Gr.createInfoStages = shaderStages,
	Vk.Ppl.Gr.createInfoVertexInputState = Just $ U3 def,
	Vk.Ppl.Gr.createInfoInputAssemblyState = Just inputAssembly,
	Vk.Ppl.Gr.createInfoViewportState = Just $ mkViewportState sce,
	Vk.Ppl.Gr.createInfoRasterizationState = Just rasterizer,
	Vk.Ppl.Gr.createInfoMultisampleState = Just multisampling,
	Vk.Ppl.Gr.createInfoDepthStencilState = Nothing,
	Vk.Ppl.Gr.createInfoColorBlendState = Just colorBlending,
	Vk.Ppl.Gr.createInfoDynamicState = Nothing,
	Vk.Ppl.Gr.createInfoLayout = U3 pllyt,
	Vk.Ppl.Gr.createInfoRenderPass = rp,
	Vk.Ppl.Gr.createInfoSubpass = 0,
	Vk.Ppl.Gr.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Gr.createInfoBasePipelineIndex = - 1,
	Vk.Ppl.Gr.createInfoTessellationState = Nothing }

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

createFramebuffers' ::
	forall ts siv k sd sf sr nm fmt .
	(HPList.HomoListN ts, Ord k) =>
	Vk.Frmbffr.Group sd 'Nothing sf (k, Int) -> k ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	HPList.PL (Vk.ImgVw.I nm fmt) (HPList.Replicate ts siv) ->
	IO (HPList.PL Vk.Frmbffr.F (HPList.Replicate ts sf))
createFramebuffers' fbgrp k sce rp =
	HPList.mapHomoListNMWithI @_ @ts @_ @_ @siv 0 \i sciv ->
	fromRight <$> Vk.Frmbffr.create'
		fbgrp (k, i) (mkFramebufferCreateInfo sce rp sciv)

recreateFramebuffers' :: forall ts sd sr nm fmt siv sf .
	HPList.HomoListN ts =>
	Vk.Dvc.D sd -> Vk.Extent2d ->
	Vk.RndrPss.R sr -> HPList.PL (Vk.ImgVw.I nm fmt) (HPList.Replicate ts siv) ->
	HPList.PL Vk.Frmbffr.F (HPList.Replicate ts sf) -> IO ()
recreateFramebuffers' dvc sce rp =
	HPList.zipWithHomoListNM_ @_ @ts @_ @_ @siv @_ @sf \sciv fb ->
	Vk.Frmbffr.unsafeRecreate dvc (mkFramebufferCreateInfo sce rp sciv) nil fb

mkFramebufferCreateInfo ::
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I nm fmt si ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(nm, fmt, si)]
mkFramebufferCreateInfo sce rp attch = Vk.Frmbffr.CreateInfo {
	Vk.Frmbffr.createInfoNext = TMaybe.N,
	Vk.Frmbffr.createInfoFlags = zeroBits,
	Vk.Frmbffr.createInfoRenderPass = rp,
	Vk.Frmbffr.createInfoAttachments = U3 attch :** HPList.Nil,
	Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
	Vk.Frmbffr.createInfoLayers = 1 }
	where
	Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } = sce

copyBffrLst :: forall sd sc sm sb bnm t lnm sm' sb' bnm' . Storable' t =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List t lnm] ->
	Vk.Bffr.Binded sm' sb' bnm' '[Vk.ObjNA.List t lnm] -> IO ()
copyBffrLst dv gq cp s d = singleTimeCmds dv gq cp \cb ->
	Vk.Cmd.copyBuffer @'[ '( '[Vk.ObjNA.List t lnm], 0, 0)] cb s d

singleTimeCmds :: forall sd sc a . Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> (forall scb . Vk.CmdBffr.C scb -> IO a) -> IO a
singleTimeCmds dv gq cp cmds =
	Vk.CmdBffr.allocate dv ainfo \(cb :*. HPList.Nil) ->
	Vk.CmdBffr.begin @_ @'Nothing cb binfo (cmds cb) <* do
	Vk.Q.submit gq (HPList.Singleton . U4 $ sinfo cb) Nothing
	Vk.Q.waitIdle gq
	where
	ainfo :: Vk.CmdBffr.AllocateInfo 'Nothing sc '[ '()]
	ainfo = Vk.CmdBffr.AllocateInfo {
		Vk.CmdBffr.allocateInfoNext = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPool = cp,
		Vk.CmdBffr.allocateInfoLevel = Vk.CmdBffr.LevelPrimary }
	binfo = Vk.CmdBffr.BeginInfo {
		Vk.CmdBffr.beginInfoNext = TMaybe.N,
		Vk.CmdBffr.beginInfoFlags = Vk.CmdBffr.UsageOneTimeSubmitBit,
		Vk.CmdBffr.beginInfoInheritanceInfo = Nothing }
	sinfo cb = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

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

createBffrLst :: forall sd bnm al t mnm a . (KnownNat al, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb bnm '[Vk.Obj.ListMaybeName al t mnm] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg
				bnm '[Vk.Obj.ListMaybeName al t mnm])] ->
		IO a) -> IO a
createBffrLst p dv ln = createBffr p dv $ Vk.Obj.LengthList ln

createBffr :: forall sd o nm a . Vk.Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] -> IO a) -> IO a
createBffr p dv ln us prs f = Vk.Bffr.create dv (bffrInfo ln us) nil \b -> do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(mmAllcInfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bb))) -> bb

bffrInfo ::
	Vk.Obj.Length s -> Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo Nothing '[s]
bffrInfo ln us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N, Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton ln,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

findMmType ::
	Vk.Phd.P -> Vk.Mm.TypeBits -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmType pd flt prs =
	fromMaybe (error msg) . suit <$> Vk.Phd.getMemoryProperties pd
	where
	msg = "failed to find suitable memory type!"
	suit p = fst <$> L.find (uncurry (&&) .
			((`Vk.Mm.elemTypeIndex` flt) ***
			checkBits prs . Vk.Mm.mTypePropertyFlags))
		(Vk.Phd.memoryPropertiesMemoryTypes p)

mmAllcInfo :: Vk.Mm.TypeIndex -> Vk.Mm.AllocateInfo 'Nothing
mmAllcInfo mt = Vk.Mm.AllocateInfo {
	Vk.Mm.allocateInfoNext = TMaybe.N,
	Vk.Mm.allocateInfoMemoryTypeIndex = mt }

createRectangleBuffer :: Ord k =>
	Devices sd sc scb -> RectGroups sd sm sb nm nmr k -> k -> [Rectangle] ->
	IO (Vk.Bffr.Binded sm sb nm '[Vk.Obj.List 256 WRect nmr])
createRectangleBuffer (phdvc, _qfis, dvc, gq, _pq, cp, _cb) (bgrp, mgrp) k rs =
	createBufferList' phdvc dvc bgrp mgrp k (fromIntegral $ length rs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit >>= \(b, _) -> do
	createBufferList phdvc dvc (fromIntegral $ length rs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
			\(b' :: Vk.Bffr.Binded sm sb "rectangle-buffer" '[Vk.Obj.List 256 t nmr]) bm' -> do
		Vk.Mm.write @"rectangle-buffer" @(Vk.Obj.List 256 WRect nmr) @0 dvc bm' zeroBits $ StrG.W <$> rs
		copyBuffer dvc gq cp b' b
	pure b

createRectangleBuffer' :: Ord k =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	RectGroups sd sm sb nm nmr k -> k -> [Rectangle] ->
	IO (Vk.Bffr.Binded sm sb nm '[Vk.Obj.List 256 WRect nmr])
createRectangleBuffer' phdvc dvc gq cp (bgrp, mgrp) k rs =
	createBufferList' phdvc dvc bgrp mgrp k (fromIntegral $ length rs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit >>= \(b, _) -> do
	createBufferList phdvc dvc (fromIntegral $ length rs)
		Vk.Bffr.UsageTransferSrcBit
		(	Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
			\(b' :: Vk.Bffr.Binded sm sb "rectangle-buffer" '[Vk.Obj.List 256 t nmr]) bm' -> do
		Vk.Mm.write @"rectangle-buffer" @(Vk.Obj.List 256 WRect nmr) @0 dvc bm' zeroBits $ StrG.W <$> rs
		copyBuffer dvc gq cp b' b
	pure b

destroyRectangleBuffer :: Ord k => RectGroups sd sm sb nm nmr k -> k -> IO ()
destroyRectangleBuffer (bgrp, mgrp) k = do
	r1 <- Vk.Mm.unsafeFree mgrp k
	r2 <- Vk.Bffr.unsafeDestroy bgrp k
	case (r1, r2) of
		(Left msg, _) -> error msg
		(_, Left msg) -> error msg
		_ -> pure ()

type RectGroups sd sm sb bnm nm k = (
	Vk.Bffr.Group sd 'Nothing sb k bnm '[Vk.Obj.List 256 WRect nm],
	Vk.Mm.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mm.BufferArg bnm '[Vk.Obj.List 256 WRect nm])] )

createBufferList :: forall sd nm t a nmm . Storable t =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[Vk.Obj.List 256 t nmm] ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg nm '[Vk.Obj.List 256 t nmm] ) ] ->
		IO a) -> IO a
createBufferList p dv ln usg props =
	createBuffer p dv (Vk.Obj.LengthList ln) usg props

createBufferList' :: forall sd nm t sm sb k nmm . (Ord k, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[Vk.Obj.List 256 t nmm]  ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mm.BufferArg nm '[Vk.Obj.List 256 t nmm])] ->
	k ->
	Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[Vk.Obj.List 256 t nmm],
		Vk.Mm.M sm '[ '(
			sb, 'Vk.Mm.BufferArg nm '[Vk.Obj.List 256 t nmm] ) ] )
createBufferList' p dv bgrp mgrp k ln usg props =
	createBuffer' p dv bgrp mgrp k (Vk.Obj.LengthList ln) usg props

createBuffer :: forall sd nm o a . Vk.Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mm.M sm
			'[ '(sb, 'Vk.Mm.BufferArg nm '[o])] ->
		IO a) -> IO a
createBuffer p dv ln usg props f =
	Vk.Bffr.group dv nil \bgrp -> Vk.Mm.group dv nil \mgrp ->
	uncurry f =<< createBuffer' p dv bgrp mgrp () ln usg props

createBuffer' :: forall sd sb nm o sm k .
	(Ord k, Vk.Obj.SizeAlignment o) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[o] ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] -> k ->
	Vk.Obj.Length o ->
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

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a nmm . Storable' a =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[Vk.Obj.List 256 a nmm] ->
	Vk.Bffr.Binded sm' sb' nm' '[Vk.Obj.List 256 a nmm] -> IO ()
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
			Vk.Cmd.copyBuffer @'[ '( '[Vk.Obj.List 256 a nmm], 0, 0)] cb src dst
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

data SyncObjects (ssos :: (Type, Type, Type)) where
	SyncObjects :: {
		_imageAvailableSemaphores :: Vk.Smph.S sias,
		_renderFinishedSemaphores :: Vk.Smph.S srfs,
		_inFlightFences :: Vk.Fnc.F sfs } ->
		SyncObjects '(sias, srfs, sfs)

createSyncObjects :: (Ord k, Vk.AllocationCallbacks.ToMiddle ma) =>
	Vk.Smph.Group sd ma sias k -> Vk.Smph.Group sd ma srfs k ->
	Vk.Fnc.Group sd ma siff k -> k -> IO (SyncObjects '(sias, srfs, siff))
createSyncObjects iasgrp rfsgrp iffgrp k =
	Vk.Smph.create' @_ @'Nothing iasgrp k def >>= \(fromRight -> ias) ->
	Vk.Smph.create' @_ @'Nothing rfsgrp k def >>= \(fromRight -> rfs) ->
	Vk.Fnc.create' @_ @'Nothing iffgrp k fncInfo >>= \(fromRight -> iff) ->
	pure $ SyncObjects ias rfs iff
	where
	fncInfo = def { Vk.Fnc.createInfoFlags = Vk.Fnc.CreateSignaledBit }

recordCommandBuffer :: forall scb sr sf sl sg sm sb smr sbr nm sm' sb' nm' sdsl sds alu mnmvp nmr .
	Vk.CmdBffr.C scb ->
	Vk.RndrPss.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.PplLyt.P sl '[AtomUbo sdsl alu mnmvp] '[] ->
	Vk.Ppl.Gr.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(WRect, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[AtomUbo sdsl alu mnmvp], '[]) ->
	Vk.Bffr.Binded sm sb nm '[Vk.Obj.List 1 WVertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[Vk.Obj.List 256 WRect nmr], Vk.Cmd.InstanceCount) ->
	Vk.Bffr.Binded sm' sb' nm' '[Vk.Obj.List 1 Word16 ""] ->
	Vk.DscSt.D sds (AtomUbo sdsl alu mnmvp) ->
	IO ()
recordCommandBuffer cb rp fb sce pllyt gpl vb (rb, ic) ib ubds =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb (
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @"" vb) :**
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @WRect @nmr rb) :**
		HPList.Nil
		) >>
	Vk.Cmd.bindIndexBuffer cbb (Vk.Bffr.IndexedForList @_ @_ @_ @Word16 @"" ib) >>
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics pllyt
		(HPList.Singleton $ U2 ubds)
		(HPList.Singleton (
			HPList.Nil :**
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

class Succable n where
	zero' :: n
	succ' :: n -> n

instance Succable Bool where
	zero' = False
	succ' = \case False -> True; True -> error "no more"

instance Succable Int where
	zero' = 0
	succ' = succ

mainloop ::
	forall n siv sf scfmt sw ssfc sd sc scb sias srfs siff ssc nm sr sg sl
		sdsl sm sb sm' sb' nm' srm srb sds sm2 sb2 k alu mnmvp .
	(HPList.HomoListN n, Vk.T.FormatToValue scfmt, Ord k, Succable k, KnownNat alu) =>
	TChan (Command k) -> TChan (Event k) -> Devices sd sc scb -> PipelineLayout sl sdsl alu mnmvp ->

	(k -> IO (WinObjs sw ssfc sg sl sdsl alu mnmvp sias srfs siff scfmt ssc nm
		(HPList.Replicate n siv) sr (HPList.Replicate n sf))) ->

	(k -> IO ()) ->

	VertexBuffers sm sb nm sm' sb' nm' ->
	RectGroups sd srm srb nm "" k ->
	UniformBuffers sds sdsl alu mnmvp sm2 sb2 ->
	TVar (M.Map k (IO ())) ->
	IO ()
mainloop inp outp dvs pll crwos drwos vbs rgrps ubs ges =
	atomically (newTVar zero') >>= \vwid ->
	atomically (newTVar M.empty) >>= \vws -> do
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
			DestroyWindow k -> do
				atomically (modifyTVar ges (M.delete k))
				atomically (modifyTVar vws (M.delete k)) >> drwos k
				ws <- atomically $ readTVar vws
				cls <- and <$> GlfwG.Win.shouldClose `mapM` (winObjsToWin <$> ws)
				if cls then pure () else loop
			GetEvent -> atomically (readTVar ges) >>= sequence_ >> loop

rectsToDummy :: M.Map k (b, [Rectangle]) -> M.Map k (StrG.W b, [Rectangle])
rectsToDummy = M.map \(tm, rects) -> (StrG.W tm, bool rects dummy $ null rects)

type Devices sd scp scb = (
	Vk.Phd.P, QFamIdcs, Vk.Dvc.D sd,
	Vk.Q.Q, Vk.Q.Q, Vk.CmdPl.C scp, Vk.CmdBffr.C scb )

type Swapchains scfmt ssc nm ss sr sfs = (
	Vk.Khr.Swpch.S scfmt ssc,
	HPList.PL (Vk.ImgVw.I nm scfmt) ss,
	Vk.RndrPss.R sr, HPList.PL Vk.Frmbffr.F sfs )

type Pipeline sg sl sdsl alu mnmvp = Vk.Ppl.Gr.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(WRect, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[AtomUbo sdsl alu mnmvp], '[])

type PipelineLayout sl sdsl alu mnmvp = Vk.PplLyt.P sl '[AtomUbo sdsl alu mnmvp] '[]

type VertexBuffers sm sb nm sm' sb' nm' = (
	Vk.Bffr.Binded sm sb nm '[Vk.Obj.List 1 WVertex ""],
	Vk.Bffr.Binded sm' sb' nm' '[Vk.Obj.List 1 Word16 ""] )

type UniformBuffers sds sdsl alu mnmvp sm2 sb2 =
	(Vk.DscSt.D sds (AtomUbo sdsl alu mnmvp), UniformBufferMemory sm2 sb2 alu mnmvp)

type UniformBufferMemory sm sb alu mnmvp = Vk.Mm.M sm '[ '(
	sb,
	'Vk.Mm.BufferArg "uniform-buffer" '[Vk.Obj.Atom alu WViewProj mnmvp]
	)]

data Recreates sw sl nm ssfc sr sg sdsl alu mnmvp fmt ssc sis sfs = Recreates
	(GlfwG.Win.W sw) (Vk.Khr.Sfc.S ssfc)
	(TVar Vk.Extent2d)
	(Vk.RndrPss.R sr)
	(Vk.Ppl.Gr.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(WRect, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[AtomUbo sdsl alu mnmvp], '[]))
	(Vk.Khr.Swpch.S fmt ssc)
	(HPList.PL (Vk.ImgVw.I nm fmt) sis)
	(HPList.PL Vk.Frmbffr.F sfs)

winObjsToRecreates ::
	WinObjs sw ssfc sg sl sdsl alu mnmvp sias srfs siff scfmt ssc nm sscivs sr sfs ->
	Recreates sw sl nm ssfc sr sg sdsl alu mnmvp scfmt ssc sscivs sfs
winObjsToRecreates (WinObjs (w, _) sfc vex gpl _iasrfsifs (sc, scivs, rp, fbs)) =
	Recreates w sfc vex rp gpl sc scivs fbs

data Draws sl sr sg sdsl alu mnmvp sias srfs siff fmt ssc sfs = Draws
	(TVar Vk.Extent2d) (Vk.RndrPss.R sr)
	(Vk.Ppl.Gr.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(WRect, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[AtomUbo sdsl alu mnmvp], '[]))
	(SyncObjects '(sias, srfs, siff))
	(Vk.Khr.Swpch.S fmt ssc)
	(HPList.PL Vk.Frmbffr.F sfs)

winObjsToDraws ::
	WinObjs sw ssfc sg sl sdsl alu mnmvp sias srfs siff scfmt ssc nm sscivs sr sfs ->
	Draws sl sr sg sdsl alu mnmvp sias srfs siff scfmt ssc sfs
winObjsToDraws (WinObjs _ _sfc vex gpl iasrfsifs (sc, _scivs, rp, fbs)) =
	Draws vex rp gpl iasrfsifs sc fbs

winObjsToWin ::
	WinObjs sw ssfc sg sl sdsl alu mnmvp sias srfs siff scfmt ssc nm sscivs sr sfs ->
	W sw
winObjsToWin (WinObjs (win, _) _ _ _ _ _) = win

runLoop' :: forall n (siv :: Type) (sf :: Type)
	sd sc scb sl
	sw ssfc sg sias srfs siff scfmt ssc sr
	smrct sbrct nmrct sds sdsl sm sb sm' sb' sm2 sb2 nm2 k alu mnmvp nmr .
	(HPList.HomoListN n, Vk.T.FormatToValue scfmt, Ord k, KnownNat alu) =>
	Devices sd sc scb -> Vk.PplLyt.P sl '[AtomUbo sdsl alu mnmvp] '[] ->
	(M.Map k (WinObjs sw ssfc sg sl sdsl alu mnmvp sias srfs siff scfmt ssc nmrct
		(HPList.Replicate n siv) sr (HPList.Replicate n sf))) ->
	(	Vk.Bffr.Binded sm' sb' nmrct '[Vk.Obj.List 1 WVertex ""],
		Vk.Bffr.Binded sm2 sb2 nm2 '[Vk.Obj.List 1 Word16 ""] ) ->
	(	Vk.Bffr.Group sd 'Nothing sbrct k nmrct '[Vk.Obj.List 256 WRect nmr],
		Vk.Mm.Group sd 'Nothing smrct k '[
			'(sbrct, 'Vk.Mm.BufferArg nmrct '[Vk.Obj.List 256 WRect nmr])] ) ->
	M.Map k (WViewProj, [Rectangle]) ->
	(Vk.DscSt.D sds (AtomUbo sdsl alu mnmvp), UniformBufferMemory sm sb alu mnmvp) ->
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
	cls <- and <$> GlfwG.Win.shouldClose `mapM` (winObjsToWin <$> ws)
	if cls then (pure ()) else do
		for_ ws \wos ->
			recreateSwapchainEtcIfNeed @n @siv @sf phdvc qfis dvc pll wos
		loop

lookupRects :: Ord k =>
	M.Map k (WViewProj, [Rectangle]) -> k ->
	(WViewProj, [Rectangle])
lookupRects rs = fromMaybe (viewProjectionIdentity, dummy) . (`M.lookup` rs)

catchAndDraw ::
	forall n siv sf
		sd sl sdsl sm sb smr sbr nm sm' sb' sm2 sb2 nm' sw ssfc sg sias srfs siff win ssc sr sds scb alu mnmvp nmr .
	(HPList.HomoListN n, Vk.T.FormatToValue win, KnownNat alu) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.Q.Q -> Vk.PplLyt.P sl '[AtomUbo sdsl alu mnmvp] '[] ->
	Vk.Bffr.Binded sm sb nm '[Vk.Obj.List 1 WVertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[Vk.Obj.List 256 WRect nmr], Vk.Cmd.InstanceCount)  ->
	Vk.Bffr.Binded sm' sb' nm' '[Vk.Obj.List 1 Word16 ""] ->
	UniformBufferMemory sm2 sb2 alu mnmvp -> Vk.DscSt.D sds (AtomUbo sdsl alu mnmvp) ->
	Vk.CmdBffr.C scb ->
	WViewProj ->
	WinObjs sw ssfc sg sl sdsl alu mnmvp sias srfs siff win ssc nm
		(HPList.Replicate n siv) sr (HPList.Replicate n sf) ->
	IO ()
catchAndDraw phdvc qfis dvc gq pq pllyt vb rb ib ubm ubds cb ubo wos = do
	catchAndRecreate @n @_ @siv @sf phdvc qfis dvc pllyt (winObjsToRecreates wos)
		$ drawFrame dvc gq pq pllyt (winObjsToDraws wos) vb rb ib ubm ubds cb ubo
	Vk.Dvc.waitIdle dvc

recreateSwapchainEtcIfNeed ::
	forall n siv sf
		sd sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm sr alu mnmvp .
	(Vk.T.FormatToValue scfmt, HPList.HomoListN n) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.PplLyt.P sl '[AtomUbo sdsl alu mnmvp] '[] ->
	WinObjs sw ssfc sg sl sdsl alu mnmvp sias srfs siff scfmt ssc nm
		(HPList.Replicate n siv) sr (HPList.Replicate n sf) -> IO ()
recreateSwapchainEtcIfNeed phdvc qfis dvc pllyt wos@(WinObjs (_, fbrszd) _ _ _ _ _) =
	atomically (checkFlag fbrszd) >>= bool (pure ())
		(recreateSwapchainEtc @n @siv @sf phdvc qfis dvc pllyt $ winObjsToRecreates wos)
	

drawFrame :: forall sfs sd ssc sr sl sg sm sb smr sbr nm sm' sb' nm' sm2 sb2 scb sias srfs siff sdsl scfmt sds alu mnmvp nmr .
	KnownNat alu =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.PplLyt.P sl '[AtomUbo sdsl alu mnmvp] '[] ->
	Draws sl sr sg sdsl alu mnmvp sias srfs siff scfmt ssc sfs ->
	Vk.Bffr.Binded sm sb nm '[Vk.Obj.List 1 WVertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[Vk.Obj.List 256 WRect nmr], Vk.Cmd.InstanceCount) ->
	Vk.Bffr.Binded sm' sb' nm' '[Vk.Obj.List 1 Word16 ""] ->
	UniformBufferMemory sm2 sb2 alu mnmvp ->
	Vk.DscSt.D sds (AtomUbo sdsl alu mnmvp) ->
	Vk.CmdBffr.C scb ->
	WViewProj -> IO ()
drawFrame dvc gq pq
	pllyt
	(Draws vext rp gpl (SyncObjects ias rfs iff) sc fbs)
	vb rb ib ubm ubds cb
	ubo = do
	let	siff = HPList.Singleton iff
	ext <- atomically $ readTVar vext
	Vk.Fnc.waitForFs dvc siff True Nothing
	imgIdx <- Vk.Khr.acquireNextImageResult [Vk.Success, Vk.SuboptimalKhr]
		dvc sc maxBound (Just ias) Nothing
	Vk.Fnc.resetFs dvc siff
	Vk.CmdBffr.reset cb def
	HPList.index fbs imgIdx \fb ->
		recordCommandBuffer cb rp fb ext pllyt gpl vb rb ib ubds
	updateUniformBuffer' dvc ubm ubo
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

updateUniformBuffer' :: forall sd sm2 sb2 alu mnmvp . KnownNat alu => Vk.Dvc.D sd ->
	UniformBufferMemory sm2 sb2 alu mnmvp -> WViewProj -> IO ()
updateUniformBuffer' dvc um obj = do
	Vk.Mm.write @"uniform-buffer" @(Vk.Obj.Atom alu WViewProj mnmvp) @0
		dvc um zeroBits obj

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate ::
	forall n scfmt siv sf sw ssfc sd nm sr ssc sl sdsl sg alu mnmvp .
	(HPList.HomoListN n, Vk.T.FormatToValue scfmt) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.PplLyt.P sl '[AtomUbo sdsl alu mnmvp] '[] ->
	Recreates sw sl nm ssfc sr sg sdsl alu mnmvp scfmt
		ssc (HPList.Replicate n siv) (HPList.Replicate n sf) ->
	IO () -> IO ()
catchAndRecreate phdvc qfis dvc pllyt rcs act = catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> recreateSwapchainEtc @n @siv @sf phdvc qfis dvc pllyt rcs

recreateSwapchainEtc :: forall
	n siv sf scfmt sw ssfc sd ssc nm sr sl sdsl sg alu mnmvp .
	(
	Vk.T.FormatToValue scfmt, HPList.HomoListN n ) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.PplLyt.P sl '[AtomUbo sdsl alu mnmvp] '[] ->
	Recreates sw sl nm ssfc sr sg sdsl alu mnmvp scfmt ssc (HPList.Replicate n siv) (HPList.Replicate n sf) ->
	IO ()
recreateSwapchainEtc
	phdvc qfis dvc pllyt
	(Recreates win sfc vex rp gpl sc scivs fbs) = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwpch win sfc phdvc qfis dvc sc
	atomically $ writeTVar vex ext
	Vk.Khr.Swpch.getImages dvc sc >>= \imgs ->
		recreateImageViews dvc imgs scivs
	recreateGraphicsPipeline dvc ext rp pllyt gpl
	recreateFramebuffers' @n @_ @_ @_ @_ @siv @sf dvc ext rp scivs fbs

waitFramebufferSize :: GlfwG.Win.W sw -> IO ()
waitFramebufferSize win = GlfwG.Win.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		GlfwG.waitEvents *> GlfwG.Win.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

type WVertex = StrG.W Vertex

data Vertex = Vertex { vertexPos :: Cglm.Vec2, vertexColor :: Cglm.Vec3 }
	deriving (Show, Generic)

instance Storable Vertex where
	sizeOf = StrG.gSizeOf
	alignment = StrG.gAlignment
	peek = StrG.gPeek
	poke = StrG.gPoke

instance StrG.G Vertex

type WRect = StrG.W Rectangle

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

vertices :: [WVertex]
vertices = StrG.W <$> [
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

type WViewProj = StrG.W ViewProjection

data ViewProjection = ViewProjection {
	viewProjectionView :: Cglm.Mat4,
	viewProjectionProj :: Cglm.Mat4 }
	deriving (Show, Generic)

viewProjectionIdentity :: WViewProj
viewProjectionIdentity = StrG.W ViewProjection {
	viewProjectionView = Cglm.mat4Identity,
	viewProjectionProj = Cglm.mat4Identity }

instance StrG.G ViewProjection

instance Default ViewProjection where
	def = ViewProjection Cglm.mat4Identity Cglm.mat4Identity

shaderModuleCreateInfo :: SpirV.S sknd -> Vk.ShaderModule.CreateInfo 'Nothing sknd
shaderModuleCreateInfo code = Vk.ShaderModule.CreateInfo {
	Vk.ShaderModule.createInfoNext = TMaybe.N,
	Vk.ShaderModule.createInfoFlags = def,
	Vk.ShaderModule.createInfoCode = code }

-- GET GLFW EVENTS

setGlfwEvents :: Ord k =>
	TChan (Event k) -> GlfwG.Win.W sw -> FramebufferResized ->
	TVar (M.Map k (IO ())) -> k -> IO ()
setGlfwEvents op w fr ges k = do
	GlfwG.Win.setKeyCallback w $ Just \_ ky _ a _ -> atomically case a of
		GlfwG.Ky.KeyState'Pressed -> writeTChan op $ EventKeyDown k ky
		GlfwG.Ky.KeyState'Released -> writeTChan op $ EventKeyUp k ky
		_ -> pure ()
	GlfwG.Win.setFramebufferSizeCallback w $ Just \_ _ _ ->
		atomically $ writeTVar fr True
	atomically do
		(vcls, vmbs) <- (,) <$> newTVar False <*> newTVar mbst0
		modifyTVar ges . M.insert k $ glfwEvents k w op vcls vmbs
	where mbst0 = foldr (uncurry M.insert) M.empty
		$ (, GlfwG.Ms.MouseButtonState'Released)
			<$> [GlfwG.Ms.MouseButton'1 .. GlfwG.Ms.MouseButton'8]

glfwEvents :: k -> GlfwG.Win.W sw -> TChan (Event k) -> TVar Bool -> TVar MouseButtonStateDict -> IO ()
glfwEvents k w op vcls vmbst = do
	GlfwG.pollEvents
	(cls, scls) <- (,)
		<$> GlfwG.Win.shouldClose w <*> atomically (readTVar vcls)
	atomically $ writeTVar vcls cls
	when (not scls && cls)
		$ atomically . writeTChan op $ EventDeleteWindow k
	(mb, mbst) <- (,) <$> getMouseButtons w <*> atomically (readTVar vmbst)
	atomically $ writeTVar vmbst mb
	sendMouseButtonDown k mbst mb op `mapM_` mouseButtonAll
	if mAny (== GlfwG.Ms.MouseButtonState'Pressed) mb && not cls
	then atomically . writeTChan op . uncurry (EventCursorPosition k)
		=<< GlfwG.Ms.getCursorPos w
	else pure ()
	sendMouseButtonUp k mbst mb op `mapM_` mouseButtonAll
	where mAny p = M.foldr (\x b -> p x || b) False

getMouseButtons :: GlfwG.Win.W sw -> IO MouseButtonStateDict
getMouseButtons w = foldr (uncurry M.insert) M.empty
	. zip mouseButtonAll <$> GlfwG.Ms.getButton w `mapM` mouseButtonAll

type MouseButtonStateDict = M.Map GlfwG.Ms.MouseButton GlfwG.Ms.MouseButtonState

mouseButtonAll :: [GlfwG.Ms.MouseButton]
mouseButtonAll = [GlfwG.Ms.MouseButton'1 .. GlfwG.Ms.MouseButton'8]

sendMouseButtonDown, sendMouseButtonUp ::
	k -> MouseButtonStateDict -> MouseButtonStateDict ->
	TChan (Event k) -> GlfwG.Ms.MouseButton -> IO ()
sendMouseButtonDown k = sendMouseButton k EventMouseButtonDown
	GlfwG.Ms.MouseButtonState'Released GlfwG.Ms.MouseButtonState'Pressed

sendMouseButtonUp k = sendMouseButton k EventMouseButtonUp
	GlfwG.Ms.MouseButtonState'Pressed GlfwG.Ms.MouseButtonState'Released

sendMouseButton ::
	k -> (k -> GlfwG.Ms.MouseButton -> Event k) ->
	GlfwG.Ms.MouseButtonState -> GlfwG.Ms.MouseButtonState ->
	MouseButtonStateDict -> MouseButtonStateDict -> TChan (Event k) ->
	GlfwG.Ms.MouseButton -> IO ()
sendMouseButton k ev pst st pbss bss op b =
	if pbss M.! b == pst && bss M.! b == st
	then atomically . writeTChan op $ ev k b else pure ()

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
