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

	-- * RUN

	rectangles, Succable,

	-- * COMMAND AND EVENT

	Command(..), Event(..),

	-- ** VIEW PROJECTION AND RECTANGLE

	ViewProjection(..),
	Rectangle(..), RectPos(..), RectSize(..), RectColor(..), RectModel(..)

	) where

import GHC.Generics
import GHC.TypeNats
import Foreign.Storable
import Foreign.Storable.Generic qualified as StrG
import Foreign.Storable.PeekPoke
import Control.Arrow hiding (loop)
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent.STM
import Control.Concurrent.STM.ToolsYj
import Control.Exception
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Foldable
import Data.Proxy
import Data.Ord.ToolsYj
import Data.Bits
import Data.Bits.ToolsYj
import Data.Default
import Data.Maybe
import Data.Maybe.ToolsYj
import Data.Either.ToolsYj
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.List.Length
import Data.List.ToolsYj
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
import Data.Map qualified as M
import Data.Bool
import Data.Bool.ToolsYj
import Data.Word
import Data.Text.IO qualified as Txt
import Data.Color

import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc.TH
import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Key as GlfwG.Ky
import Graphics.UI.GlfwG.Mouse as GlfwG.Ms

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Exception qualified as Vk
import Gpu.Vulkan.Object qualified as Vk.Obj
import Gpu.Vulkan.Object.NoAlignment qualified as Vk.ObjNA
import Gpu.Vulkan.AllocationCallbacks qualified as Vk.AllocationCallbacks
import Gpu.Vulkan.Instance qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.Image qualified as Vk.Img
import Gpu.Vulkan.ImageView qualified as Vk.ImgVw
import Gpu.Vulkan.Framebuffer qualified as Vk.Frmbffr
import Gpu.Vulkan.Semaphore qualified as Vk.Smph
import Gpu.Vulkan.Fence qualified as Vk.Fnc

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Pipeline.Graphics qualified as Vk.Ppl.Gr
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.Pipeline.InputAssemblyState qualified as Vk.Ppl.InpAsmbSt
import Gpu.Vulkan.Pipeline.ViewportState qualified as Vk.Ppl.ViewportSt
import Gpu.Vulkan.Pipeline.VertexInputState qualified as Vk.Ppl.VertexInputSt
import Gpu.Vulkan.Pipeline.RasterizationState qualified as Vk.Ppl.RstSt
import Gpu.Vulkan.Pipeline.MultisampleState qualified as Vk.Ppl.MltSmplSt
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
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt

import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch
import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.DbgUtls.Msgr
import Gpu.Vulkan.Cglm qualified as Cglm

import Debug

----------------------------------------------------------------------
--
-- * RECTANGLES
-- * BODY
-- * PROVIDE WINDOW OBJECTS
-- * WINDOW OBJECTS
-- * RECTANGLE BUFFER
-- * GET GLFW EVENTS
-- * CREATE AND COPY BUFFERS
-- * GRAPHICS PIPELINE
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
	atomically (newTVar M.empty) >>= \ges ->
	let	crwos = provideWinObjs @n @scfmt
			op vex pd dv gq cp qfis pl wg gs rgs ges
		drwos = destroyWinObjs @n wg gs rgs ges
		gs = (sfcg, scg, ivg, rpg, fbg, gpg, iasg, rfsg, iffg) in
	mainloop @n @siv @sf ip op dvs pl crwos drwos vbs rgs ds vpm ges

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
createPplLyt dv f = createDSLyt dv \d -> Vk.PplLyt.create dv (info d) nil $ f d
	where
	info :: Vk.DscStLyt.D sdsl (DscStLytArg alu mnmvp) ->
		Vk.PplLyt.CreateInfo 'Nothing
			'[ '(sdsl, DscStLytArg alu mnmvp)]
			('Vk.PushConstant.Layout '[] '[])
	info d = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 d }

createDSLyt :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscStLyt.D s (DscStLytArg alu mnmvp) -> IO a) -> IO a
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
	Vk.Bffr.Binded sm sb bnmvp '[AtomViewProj alu mnmvp] ->
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
	IO (WinObjs sw ssfc scfmt ssc nmi
		(HPList.Replicate n siv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnmvp sias srfs siff)
provideWinObjs op vexs pd dv gq cp qfis pl wg gs rgs ges k =
	(,) <$> initWin True wg k <*> atomically (newTVar False) >>= \(w, fr) ->
	winObjs @n vexs pd dv qfis pl w fr gs k <* do
	_ <- createRctBffr pd dv gq cp rgs k dummy
	setGlfwEvents op w fr ges k

initWin :: Ord k => Bool -> GlfwG.Win.Group sw k -> k -> IO (GlfwG.Win.W sw)
initWin v wg k = do
	GlfwG.Win.hint
		$ GlfwG.Win.WindowHint'ClientAPI GlfwG.Win.ClientAPI'NoAPI
	GlfwG.Win.hint $ GlfwG.Win.WindowHint'Visible v
	(forceRight' -> w) <-
		uncurry (GlfwG.Win.create' wg k) wSize wName Nothing Nothing
	pure w
	where wName = "Rectangles"; wSize = (800, 600)

-- WINDOW OBJECTS

winObjs :: forall (n :: [()]) (scfmt :: Vk.T.Format) k
	si sd sl sdsl alu mnmvp sw ssfc ssc sv nmi sr sf sg sias srfs siff .
	(HPList.HomoListN n, Vk.T.FormatToValue scfmt, Ord k) =>
	TVar (M.Map k (TVar Vk.Extent2d)) ->
	Vk.Phd.P -> Vk.Dvc.D sd -> QFamIdcs ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[] ->
	GlfwG.Win.W sw -> FramebufferResized -> WinObjGroups k si ssfc sd scfmt
		ssc sv nmi sr sf sg sl sdsl alu mnmvp sias srfs siff -> k ->
	IO (WinObjs sw ssfc scfmt ssc nmi
		(HPList.Replicate n sv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnmvp sias srfs siff)
winObjs vexs pd dv qfis pl w fr
	(sfcg, scg, ivg, rpg, fbg, gpg, iasg, rfsg, iffg) k =
	Vk.Khr.Sfc.Glfw.Win.create' sfcg k w >>= \(forceRight' -> sfc) ->
	prepareSwpch w sfc pd \ssd ex ->
	atomically (
		newTVar (Vk.Extent2d 0 0) >>= \v -> writeTVar v ex >>
		v <$ modifyTVar vexs (M.insert k v) ) >>= \vex ->
	createSwpch' sfc qfis scg k ssd ex >>= \sc ->
	Vk.Khr.Swpch.getImages dv sc >>= \scis ->
	createImgVws @n ivg k scis >>= \scvs ->
	createRndrPss @scfmt rpg k >>= \rp ->
	createFrmbffrs @n @sv fbg k ex rp scvs >>= \fbs ->
	createGrPpl gpg k ex rp pl >>= \gp ->
	createSyncObjs iasg rfsg iffg k >>= \sos ->
	pure $ WinObjs (w, fr) sfc vex (sc, scvs, rp, fbs) gp sos

destroyWinObjs :: forall (n :: [()]) (scfmt :: Vk.T.Format) k
	si sd sl sdsl alu mnmvp sw ssfc ssc siv nmi sr sf sg sias srfs siff
	smr sbr bnmr nmr . (NumToVal n,  Ord k) =>
	GlfwG.Win.Group sw k -> WinObjGroups k si ssfc sd scfmt
		ssc siv nmi sr sf sg sl sdsl alu mnmvp sias srfs siff ->
	RectGroups sd smr sbr bnmr nmr k -> TVar (M.Map k (IO ())) -> k -> IO ()
destroyWinObjs wg (sfcg, scg, ivg, rpg, fbg, gpg, iasg, rfsg, iffg)
	(rbg, rmg) ges k = GlfwG.Win.lookup wg k >>= \case
	Nothing -> pure ()
	Just w -> do
		GlfwG.Win.setShouldClose w True
		tr =<< GlfwG.Win.unsafeDestroy wg k
		tr =<< Vk.Khr.Swpch.unsafeDestroy scg k
		tr =<< Vk.Khr.Sfc.unsafeDestroy sfcg k
		tr =<< Vk.RndrPss.unsafeDestroy rpg k
		for_ [0 .. numToVal @n - 1] \i -> do
			tr =<< Vk.ImgVw.unsafeDestroy ivg (k, i)
			tr =<< Vk.Frmbffr.unsafeDestroy fbg (k, i)
		tr =<< Vk.Ppl.Gr.unsafeDestroyGs gpg k
		tr =<< Vk.Smph.unsafeDestroy iasg k
		tr =<< Vk.Smph.unsafeDestroy rfsg k
		tr =<< Vk.Fnc.unsafeDestroy iffg k
		tr =<< Vk.Bffr.unsafeDestroy rbg k
		tr =<< Vk.Mm.unsafeFree rmg k
		atomically (modifyTVar ges (M.delete k))
	where tr = either error pure

class NumToVal (n :: [()]) where numToVal :: Int
instance NumToVal '[] where numToVal = 0
instance NumToVal n => NumToVal ('() ': n) where numToVal = 1 + numToVal @n

data WinObjs sw ssfc scfmt
	ssc nmscv svs sr sfs sg sl sdsl alu mnmvp sias srfs siff = WinObjs
	(WinEnvs sw) (Vk.Khr.Sfc.S ssfc) (TVar Vk.Extent2d)
	(Swapchains scfmt ssc nmscv svs sr sfs)
	(Pipeline sg sl sdsl alu mnmvp) (SyncObjs '(sias, srfs, siff))

type WinEnvs sw = (GlfwG.Win.W sw , FramebufferResized)
type FramebufferResized = TVar Bool

type Swapchains scfmt ssc nmi svs sr sfs = (
	Vk.Khr.Swpch.S scfmt ssc, HPList.PL (Vk.ImgVw.I nmi scfmt) svs,
	Vk.RndrPss.R sr, HPList.PL Vk.Frmbffr.F sfs )

type Pipeline sg sl sdsl alu mnmvp = Vk.Ppl.Gr.G sg
	'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(WRect, 'Vk.VtxInp.RateInstance)]
	'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
		'(2, RectPos), '(3, RectSize), '(4, RectColor),
		'(5, RectModel0), '(6, RectModel1),
		'(7, RectModel2), '(8, RectModel3) ]
	'(sl, '[ '(sdsl, DscStLytArg alu mnmvp)], '[])

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

prepareSwpch :: forall sw ssfc a .
	GlfwG.Win.W sw -> Vk.Khr.Sfc.S ssfc -> Vk.Phd.P ->
	(forall fmts . SwpchSupportDetails fmts -> Vk.Extent2d -> IO a) -> IO a
prepareSwpch w sfc pd f =
	querySwpchSupport pd sfc \sd -> swapExtent w (capabilities sd) >>= f sd

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

createImgVws :: forall n sd sv k nm vfmt sm si ifmt .
	(HPList.HomoListN n, Ord k, Vk.T.FormatToValue vfmt) =>
	Vk.ImgVw.Group sd 'Nothing sv (k, Int) nm vfmt -> k ->
	[Vk.Img.Binded sm si nm ifmt] ->
	IO (HPList.PL (Vk.ImgVw.I nm vfmt) (HPList.Replicate n sv))
createImgVws vg k is = HPList.homoListNFromList @_ @n
	<$> (\(i, img) -> createImgVw' vg (k, i) img) `mapM` zip [0 ..] is

recreateImgVws :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss nm scfmt] -> HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	IO ()
recreateImgVws _ [] HPList.Nil = pure ()
recreateImgVws dv (i : is) (v :** vs) =
	recreateImgVw dv i v >> recreateImgVws dv is vs
recreateImgVws _ _ _ = error "number of Vk.Img.I and Vk.ImgVw.I should be same"

createImgVw' :: forall sd sv k nm vfmt sm si ifmt .
	(Ord k, Vk.T.FormatToValue vfmt) =>
	Vk.ImgVw.Group sd 'Nothing sv k nm vfmt ->
	k -> Vk.Img.Binded sm si nm ifmt -> IO (Vk.ImgVw.I nm vfmt sv)
createImgVw' vg k i = forceRight' <$> Vk.ImgVw.create' vg k (imgVwInfo i)

recreateImgVw :: Vk.T.FormatToValue ivfmt => Vk.Dvc.D sd ->
	Vk.Img.Binded sm si nm ifmt -> Vk.ImgVw.I nm ivfmt sv -> IO ()
recreateImgVw dv i = Vk.ImgVw.unsafeRecreate dv (imgVwInfo i) nil

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

createRndrPss :: forall scfmt sd ma sr k .
	(Vk.T.FormatToValue scfmt, Vk.AllocationCallbacks.ToMiddle ma, Ord k) =>
	Vk.RndrPss.Group sd ma sr k -> k -> IO (Vk.RndrPss.R sr)
createRndrPss rpg k = forceRight' <$> Vk.RndrPss.create' rpg k info
	where
	info = Vk.RndrPss.CreateInfo {
		Vk.RndrPss.createInfoNext = TMaybe.N,
		Vk.RndrPss.createInfoFlags = zeroBits,
		Vk.RndrPss.createInfoAttachments = HPList.Singleton att,
		Vk.RndrPss.createInfoSubpasses = [sbpss],
		Vk.RndrPss.createInfoDependencies = [dpnd] }
	att :: Vk.Att.Description scfmt
	att = Vk.Att.Description {
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
		Vk.Subpass.descriptionColorAndResolveAttachments = Left [attrf],
		Vk.Subpass.descriptionDepthStencilAttachment = Nothing,
		Vk.Subpass.descriptionPreserveAttachments = [] }
	attrf = Vk.Att.Reference {
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

createFrmbffrs :: forall n sv sd sf k sr nm fmt . (HPList.HomoListN n, Ord k) =>
	Vk.Frmbffr.Group sd 'Nothing sf (k, Int) -> k -> Vk.Extent2d ->
	Vk.RndrPss.R sr ->
	HPList.PL (Vk.ImgVw.I nm fmt) (HPList.Replicate n sv) ->
	IO (HPList.PL Vk.Frmbffr.F (HPList.Replicate n sf))
createFrmbffrs fbg k ex rp =
	HPList.mapHomoListNMWithI @_ @n @_ @_ @sv 0 \i v ->
	forceRight' <$> Vk.Frmbffr.create' fbg (k, i) (frmbffrInfo ex rp v)

recreateFrmbffrs :: forall n sd sr nm fmt sv sf . HPList.HomoListN n =>
	Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	HPList.PL (Vk.ImgVw.I nm fmt) (HPList.Replicate n sv) ->
	HPList.PL Vk.Frmbffr.F (HPList.Replicate n sf) -> IO ()
recreateFrmbffrs dv ex rp =
	HPList.zipWithHomoListNM_ @_ @n @_ @_ @sv @_ @sf \v fb ->
	Vk.Frmbffr.unsafeRecreate dv (frmbffrInfo ex rp v) nil fb

frmbffrInfo :: Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.ImgVw.I nm fmt si ->
	Vk.Frmbffr.CreateInfo 'Nothing sr '[ '(nm, fmt, si)]
frmbffrInfo Vk.Extent2d { Vk.extent2dWidth = w, Vk.extent2dHeight = h } rp att =
	Vk.Frmbffr.CreateInfo {
		Vk.Frmbffr.createInfoNext = TMaybe.N,
		Vk.Frmbffr.createInfoFlags = zeroBits,
		Vk.Frmbffr.createInfoRenderPass = rp,
		Vk.Frmbffr.createInfoAttachments = HPList.Singleton $ U3 att,
		Vk.Frmbffr.createInfoWidth = w, Vk.Frmbffr.createInfoHeight = h,
		Vk.Frmbffr.createInfoLayers = 1 }

createSyncObjs :: (Ord k, Vk.AllocationCallbacks.ToMiddle ma) =>
	Vk.Smph.Group sd ma sias k -> Vk.Smph.Group sd ma srfs k ->
	Vk.Fnc.Group sd ma siff k -> k -> IO (SyncObjs '(sias, srfs, siff))
createSyncObjs iasg rfsg iffg k =
	Vk.Smph.create' @_ @'Nothing iasg k def >>= \(forceRight' -> ias) ->
	Vk.Smph.create' @_ @'Nothing rfsg k def >>= \(forceRight' -> rfs) ->
	Vk.Fnc.create' @_ @'Nothing iffg k finfo >>= \(forceRight' -> iff) ->
	pure $ SyncObjs ias rfs iff
	where finfo = def { Vk.Fnc.createInfoFlags = Vk.Fnc.CreateSignaledBit }

data SyncObjs (ssos :: (Type, Type, Type)) where
	SyncObjs :: {
		_imageAvailableSemaphores :: Vk.Smph.S sias,
		_renderFinishedSemaphores :: Vk.Smph.S srfs,
		_inFlightFences :: Vk.Fnc.F sfs } -> SyncObjs '(sias, srfs, sfs)

-- RECTANGLE BUFFER

createRctBffr :: Ord k =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	RectGroups sd sm sb bnm nm k -> k -> [WRect] ->
	IO (Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List WRect nm])
createRctBffr pd dv gq cp (bg, mg) k rs =
	createBffrLst' pd dv bg mg k (fromIntegral $ length rs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit >>= \(b, _) ->
	b <$ createBffrLst pd dv (fromIntegral $ length rs)
		Vk.Bffr.UsageTransferSrcBit
		(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)
		\(c :: Vk.Bffr.Binded m b bn '[Vk.ObjNA.List t n]) m ->
		Vk.Mm.write @bn
			@(Vk.ObjNA.List t n) @0 dv m zeroBits rs >>
		copyBffrLst dv gq cp c b

destroyRctBffr :: Ord k => RectGroups sd sm sb bnm nm k -> k -> IO ()
destroyRctBffr (bg, mg) k =
	(,) <$> Vk.Mm.unsafeFree mg k <*> Vk.Bffr.unsafeDestroy bg k >>= \case
		(Left m, _) -> error m; (_, Left m) -> error m; _ -> pure ()

type RectGroups sd sm sb bnm nm k = (
	Vk.Bffr.Group sd 'Nothing sb k bnm '[Vk.Obj.List 1 WRect nm],
	Vk.Mm.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mm.BufferArg bnm '[Vk.Obj.List 1 WRect nm])] )

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

-- GRAPHICS PIPELINE

createGrPpl :: (Ord k, Vk.AllocationCallbacks.ToMiddle ma) =>
	Vk.Ppl.Gr.Group sd ma sg k '[ '(
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(WRect, 'Vk.VtxInp.RateInstance) ],
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3) ],
			'(sl, '[ '(sdsl, DscStLytArg alu mnmvp)], '[]) )] -> k ->
	Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[] -> IO (
		Vk.Ppl.Gr.G sg
			'[	'(WVertex, 'Vk.VtxInp.RateVertex),
				'(WRect, 'Vk.VtxInp.RateInstance) ]
			'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
				'(2, RectPos), '(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1),
				'(7, RectModel2), '(8, RectModel3) ]
			'(sl, '[ '(sdsl, DscStLytArg alu mnmvp)], '[]))
createGrPpl gpg k ex rp pl = (\(forceRight' -> (HPList.Singleton (U3 p))) -> p)
	<$> Vk.Ppl.Gr.createGs' gpg k Nothing
		(HPList.Singleton . U14 $ grPplInfo ex rp pl)

recreateGrPpl :: Vk.Dvc.D sd -> Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[] ->
	Vk.Ppl.Gr.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(WRect, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[ '(sdsl, DscStLytArg alu mnmvp)], '[]) -> IO ()
recreateGrPpl dv ex rp pl gp = Vk.Ppl.Gr.unsafeRecreateGs dv Nothing
	(HPList.Singleton . U14 $ grPplInfo ex rp pl) nil
	(HPList.Singleton $ U3 gp)

grPplInfo :: Vk.Extent2d -> Vk.RndrPss.R sr ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[] ->
	Vk.Ppl.Gr.CreateInfo 'Nothing
		'[GlslVertexShaderArgs, GlslFragmentShaderArgs]
		'(	'Nothing,
			'[	'(WVertex, 'Vk.VtxInp.RateVertex),
				'(WRect, 'Vk.VtxInp.RateInstance) ],
			'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
				'(2, RectPos), '(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1),
				'(7, RectModel2), '(8, RectModel3) ] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing
		'Nothing '(sl, '[ '(sdsl, DscStLytArg alu mnmvp)], '[])
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
--		Vk.Ppl.RstSt.createInfoCullMode = Vk.CullModeBackBit,
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
	minfo cd = Vk.ShaderModule.CreateInfo {
		Vk.ShaderModule.createInfoNext = TMaybe.N,
		Vk.ShaderModule.createInfoFlags = zeroBits,
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

-- CREATE AND COPY BUFFERS

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

createBffrLst' :: forall sd sm sb k bnm a t nm .
	(Ord k, KnownNat a, Storable t) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k bnm '[Vk.Obj.ListMaybeName a t nm] ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(
		sb,
		'Vk.Mm.BufferArg bnm '[Vk.Obj.ListMaybeName a t nm] )] ->
	k -> Vk.Dvc.Size -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags ->
	IO (	Vk.Bffr.Binded sm sb bnm '[Vk.Obj.ListMaybeName a t nm],
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg bnm '[Vk.Obj.ListMaybeName a t nm] )] )
createBffrLst' p dv bg mg k ln = createBffr' p dv bg mg k (Vk.Obj.LengthList ln)

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

createBffr' :: forall sd sb nm o sm k . (Ord k, Vk.Obj.SizeAlignment o) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.Group sd 'Nothing sb k nm '[o] ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] -> k ->
	Vk.Obj.Length o -> Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[o],
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] )
createBffr' p dv bg mg k ln us prs =
	Vk.Bffr.create' bg k (bffrInfo ln us) >>= \(forceRight' -> b) -> do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	(<$> Vk.Mm.allocateBind' mg k
		(HPList.Singleton . U2 $ Vk.Mm.Buffer b) (mmAllcInfo mt))
		\(forceRight' ->
			(HPList.Singleton (U2 (Vk.Mm.BufferBinded bb)), m)) ->
		(bb, m)

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

-- MAINLOOP

mainloop :: forall
	n sv sf k sd scp scb sl sdsl alu mnmvp sw ssfc scfmt ssc nmscv sr sg
	sias srfs siff smv sbv bnmv nmv smi sbi bnmi nmi smr sbr bnmr nmr
	sds smvp sbvp bnmvp . (

	HPList.HomoListN n, Ord k, Succable k, KnownNat alu,
	Vk.T.FormatToValue scfmt ) =>
	TChan (Command k) -> TChan (Event k) -> Devices sd scp scb ->
	PipelineLayout sl sdsl alu mnmvp ->
	(k -> IO (WinObjs sw ssfc scfmt ssc nmscv
		(HPList.Replicate n sv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnmvp sias srfs siff)) -> (k -> IO ()) ->
	VertexBuffers smv sbv bnmv nmv smi sbi bnmi nmi ->
	RectGroups sd smr sbr bnmr nmr k ->
	Vk.DscSt.D sds '(sdsl, DscStLytArg alu mnmvp) ->
	ViewProjMemory smvp sbvp bnmvp alu mnmvp -> TVar (M.Map k (IO ())) ->
	IO ()
mainloop ip op dvs pl crwos drwos vbs rgs ds vpm ges = do
	(vwi, vws) <- atomically $ (,) <$> newTVar zero' <*> newTVar M.empty
	let	crwos' = do
			wi <- atomically $ readTVar vwi <* modifyTVar vwi succ'
			wi <$ (atomically
				. modifyTVar vws . M.insert wi =<< crwos wi)
	fix \go -> atomically (readTChan ip) >>= \case
		GetEvent -> atomically (readTVar ges) >>= sequence_ >> go
		OpenWindow -> crwos' >>=
			atomically . writeTChan op . EventOpenWindow >> go
		DestroyWindow k -> do
			atomically (modifyTVar vws (M.delete k)) >> drwos k
			ws <- atomically $ readTVar vws
			GlfwG.pollEvents
			bool go (pure ()) =<< and <$> GlfwG.Win.shouldClose
				`mapM` (wobjsToWin <$> ws)
		Draw drs -> do
			ws <- atomically $ readTVar vws
			run @n @sv @sf dvs pl ws vbs rgs (rects drs) ds vpm go
	where rects = M.map \(vp, rs) ->
		(StrG.W vp, bool (rectToRectRaw <$> rs) dummy (null rs))

class Succable n where zero' :: n; succ' :: n -> n

instance Succable Bool where
	zero' = False; succ' = \case False -> True; True -> error "no more"

instance Succable Int where zero' = 0; succ' = succ

type Devices sd scp scb = (
	Vk.Phd.P, QFamIdcs, Vk.Dvc.D sd,
	Vk.Q.Q, Vk.Q.Q, Vk.CmdPl.C scp, Vk.CmdBffr.C scb )

type PipelineLayout sl sdsl alu mnmvp =
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[]

type VertexBuffers smv sbv bnmv nmv smi sbi bnmi nmi = (
	Vk.Bffr.Binded smv sbv bnmv '[Vk.ObjNA.List WVertex nmv],
	Vk.Bffr.Binded smi sbi bnmi '[Vk.ObjNA.List Word16 nmi] )

wobjsToWin :: WinObjs sw ssfc scfmt ssc nmscv svs sr sfs
	sg sl sdsl alu mnmvp sias srfs siff -> GlfwG.Win.W sw
wobjsToWin (WinObjs (win, _) _ _ _ _ _) = win

run :: forall
	n (sv :: Type) (sf :: Type) k sd sc scb sl sdsl alu mnmvp
	sw ssfc scfmt ssc nmsv sr sg sias srfs siff
	smv sbv bnmv nmv smi sbi bnmi nmi smr sbr bnmr nmr smvp sbvp bnmvp
	sds .
	(HPList.HomoListN n, Ord k, KnownNat alu, Vk.T.FormatToValue scfmt) =>
	Devices sd sc scb -> Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[] ->
	(M.Map k (WinObjs sw ssfc scfmt ssc nmsv
		(HPList.Replicate n sv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnmvp sias srfs siff)) ->
	VertexBuffers smv sbv bnmv nmv smi sbi bnmi nmi ->
	RectGroups sd smr sbr bnmr nmr k -> M.Map k (WViewProj, [WRect]) ->
	Vk.DscSt.D sds '(sdsl, DscStLytArg alu mnmvp) ->
	ViewProjMemory smvp sbvp bnmvp alu mnmvp -> IO () -> IO ()
run (pd, qfis, dv, gq, pq, cp, cb) pl ws (vb, ib) rgs rss ds vpm go = do
	for_ (M.toList ws) \(k, os) -> do
		let	(vp, rs) = vprs k
		destroyRctBffr rgs k
		rb <- createRctBffr pd dv gq cp rgs k rs
		catchAndRecreate @n @sv @sf pd qfis dv pl (wobjsToRecrs os)
			$ draw dv gq pq pl (wobjsToDrs os) vb rb ib vpm ds cb vp
	Vk.Dvc.waitIdle dv
	cls <- and <$> GlfwG.Win.shouldClose `mapM` (wobjsToWin <$> ws)
	when (not cls)
		$ for_ ws (recreateAllIfNeed @n @sv @sf pd qfis dv pl) >> go
	where vprs = fromMaybe (viewProjIdentity, dummy) . (`M.lookup` rss)

-- RECREATE

catchAndRecreate ::
	forall n sv sf sd sl sdsl alu mnmvp sw ssfc sr ssc nmi scfmt sg .
	(HPList.HomoListN n, Vk.T.FormatToValue scfmt) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[] ->
	Recrs sw ssfc scfmt ssc nmi
		(HPList.Replicate n sv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnmvp -> IO () -> IO ()
catchAndRecreate pd qfis dv pl rcs act = catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just (); _ -> Nothing) act
	\_ -> recreateAll @n @sv @sf pd qfis dv pl rcs

recreateAllIfNeed :: forall
	n sv sf sd sl sdsl mnmvp sw ssfc scfmt ssc nmi sr sg alu
	sias srfs siff . (HPList.HomoListN n, Vk.T.FormatToValue scfmt) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[] ->
	WinObjs sw ssfc scfmt ssc nmi
		(HPList.Replicate n sv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnmvp sias srfs siff -> IO ()
recreateAllIfNeed pd qfis dv pl wos@(WinObjs (_, fr) _ _ _ _ _) =
	atomically (checkFlag fr) >>= bool (pure ())
		(recreateAll @n @sv @sf pd qfis dv pl $ wobjsToRecrs wos)

recreateAll :: forall
	n sv sf sd sl sdsl alu mnmvp sw ssfc sr ssc nmi scfmt sg .
	(HPList.HomoListN n, Vk.T.FormatToValue scfmt) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[] ->
	Recrs sw ssfc scfmt ssc nmi
		(HPList.Replicate n sv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnmvp -> IO ()
recreateAll pd qfis dv pl (Recrs w sfc vex sc scvs rp fbs gp) = do
	waitFrmbffrSize w >> Vk.Dvc.waitIdle dv
	ex <- recreateSwpch w sfc pd qfis dv sc
	atomically $ writeTVar vex ex
	Vk.Khr.Swpch.getImages dv sc >>= \is -> recreateImgVws dv is scvs
	recreateFrmbffrs @n @_ @_ @_ @_ @sv @sf dv ex rp scvs fbs
	recreateGrPpl dv ex rp pl gp

waitFrmbffrSize :: GlfwG.Win.W sw -> IO ()
waitFrmbffrSize w = GlfwG.Win.getFramebufferSize w >>= \sz ->
	when (zero sz) $ fix \go -> (`when` go) . zero =<<
		GlfwG.waitEvents *> GlfwG.Win.getFramebufferSize w
	where zero = uncurry (||) . ((== 0) *** (== 0))

data Recrs sw ssfc scfmt ssc nmv svs sr sfs sg sl sdsl alu mnmvp = Recrs
	(GlfwG.Win.W sw) (Vk.Khr.Sfc.S ssfc) (TVar Vk.Extent2d)
	(Vk.Khr.Swpch.S scfmt ssc)
	(HPList.PL (Vk.ImgVw.I nmv scfmt) svs) (Vk.RndrPss.R sr)
	(HPList.PL Vk.Frmbffr.F sfs)
	(Vk.Ppl.Gr.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(WRect, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[ '(sdsl, DscStLytArg alu mnmvp)], '[]))

wobjsToRecrs ::
	WinObjs sw ssfc scfmt ssc nmv sscvs sr sfs
		sg sl sdsl alu mnmvp sias srfs siff ->
	Recrs sw ssfc scfmt ssc nmv sscvs sr sfs sg sl sdsl alu mnmvp
wobjsToRecrs (WinObjs (w, _) sfc vex (sc, scvs, rp, fbs) gp _) =
	Recrs w sfc vex sc scvs rp fbs gp

-- DRAW

draw :: forall
	sd sl sdsl alu mnmvp scfmt ssc sr sfs sg sias srfs siff
	smv sbv bnmv nmv smr sbr bnmr nmr smi sbi bnmi nmi smvp sbvp bnmvp
	sds scb . KnownNat alu =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[] ->
	Draws scfmt ssc sr sfs sg sl sdsl alu mnmvp sias srfs siff ->
	Vk.Bffr.Binded smv sbv bnmv '[Vk.ObjNA.List WVertex nmv] ->
	Vk.Bffr.Binded smr sbr bnmr '[Vk.ObjNA.List WRect nmr] ->
	Vk.Bffr.Binded smi sbi bnmi '[Vk.ObjNA.List Word16 nmi] ->
	ViewProjMemory smvp sbvp bnmvp alu mnmvp ->
	Vk.DscSt.D sds '(sdsl, DscStLytArg alu mnmvp) ->
	Vk.CmdBffr.C scb -> WViewProj -> IO ()
draw dv gq pq pl (Draws vex sc rp fbs gp (SyncObjs ias rfs iff))
	vb rb ib mvp ds cb vp = do
	ex <- atomically $ readTVar vex
	Vk.Fnc.waitForFs dv siff True Nothing >> Vk.Fnc.resetFs dv siff
	Vk.CmdBffr.reset cb def
	ii <- Vk.Khr.Swpch.acquireNextImageResult
		[Vk.Success, Vk.SuboptimalKhr] dv sc maxBound (Just ias) Nothing
	HPList.index fbs ii \fb -> recordCmdBffr cb rp fb ex pl gp vb rb ib ds
	updateViewProjBffr dv mvp vp
	Vk.Q.submit gq (HPList.Singleton $ U4 sinfo) $ Just iff
	catchAndSerialize . Vk.Khr.Swpch.queuePresent @'Nothing pq $ pinfo ii
	where
	siff = HPList.Singleton iff
	sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks =
			HPList.Singleton $ Vk.SemaphorePipelineStageFlags ias
				Vk.Ppl.StageColorAttachmentOutputBit,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Singleton rfs }
	pinfo ii = Vk.Khr.Swpch.PresentInfo {
		Vk.Khr.Swpch.presentInfoNext = TMaybe.N,
		Vk.Khr.Swpch.presentInfoWaitSemaphores = HPList.Singleton rfs,
		Vk.Khr.Swpch.presentInfoSwapchainImageIndices = HPList.Singleton
			$ Vk.Khr.Swpch.SwapchainImageIndex sc ii }

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

updateViewProjBffr :: forall sd sm sb bnm alu mnm . KnownNat alu =>
	Vk.Dvc.D sd -> ViewProjMemory sm sb bnm alu mnm -> WViewProj -> IO ()
updateViewProjBffr dv mvp vp =
	Vk.Mm.write @bnm @(Vk.Obj.Atom alu WViewProj mnm) @0 dv mvp zeroBits vp

data Draws fmt ssc sr sfs sg sl sdsl alu mnmvp sias srfs siff = Draws
	(TVar Vk.Extent2d) (Vk.Khr.Swpch.S fmt ssc) (Vk.RndrPss.R sr)
	(HPList.PL Vk.Frmbffr.F sfs)
	(Vk.Ppl.Gr.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(WRect, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[ '(sdsl, DscStLytArg alu mnmvp)], '[]))
	(SyncObjs '(sias, srfs, siff))

wobjsToDrs ::
	WinObjs sw ssfc scfmt ssc nm sscivs sr sfs sg sl sdsl alu mnmvp
		sias srfs siff ->
	Draws scfmt ssc sr sfs sg sl sdsl alu mnmvp sias srfs siff
wobjsToDrs (WinObjs _ _ vex (sc, _, rp, fbs) gp soss) =
	Draws vex sc rp fbs gp soss

recordCmdBffr :: forall
	scb sr sf sl sdsl alu mnmvp sg
	smv sbv bnmv nmv smr sbr bnmr nmr smi sbi bnmi nmi sds .
	Vk.CmdBffr.C scb -> Vk.RndrPss.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp)] '[] ->
	Vk.Ppl.Gr.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(WRect, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3) ]
		'(sl, '[ '(sdsl, DscStLytArg alu mnmvp)], '[]) ->
	Vk.Bffr.Binded smv sbv bnmv '[Vk.ObjNA.List WVertex nmv] ->
	Vk.Bffr.Binded smr sbr bnmr '[Vk.ObjNA.List WRect nmr] ->
	Vk.Bffr.Binded smi sbi bnmi '[Vk.ObjNA.List Word16 nmi] ->
	Vk.DscSt.D sds '(sdsl, DscStLytArg alu mnmvp) -> IO ()
recordCmdBffr cb rp fb ex pl gp vb rb ib ds =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpinfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gp \cbb ->
	Vk.Cmd.bindVertexBuffers cbb (
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @nmv vb) :**
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @WRect @nmr rb) :**
		HPList.Nil ) >>
	Vk.Cmd.bindIndexBuffer cbb
		(Vk.Bffr.IndexedForList @_ @_ @_ @Word16 @nmi ib) >>
	Vk.Cmd.bindDescriptorSetsGraphics cbb Vk.Ppl.BindPointGraphics pl
		(HPList.Singleton $ U2 ds)
		(HPList.Singleton (HPList.Nil :** HPList.Nil )) >>
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

-- DATA TYPES

type WVertex = StrG.W Vertex

data Vertex = Vertex { vertexPos :: Cglm.Vec2, vertexColor :: Cglm.Vec3 }
	deriving (Show, Generic)

instance StrG.G Vertex

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

data Rectangle = Rectangle {
	rectanglePos' :: RectPos, rectangleSize' :: RectSize,
	rectangleColor' :: RectColor, rectangleModel' :: RectModel }
	deriving (Show, Generic)

type WRect = StrG.W RectangleRaw

data RectangleRaw = RectangleRaw {
	rectanglePos :: RectPos, rectangleSize :: RectSize,
	rectangleColor :: RectColor,
	rectangleModel0 :: RectModel0, rectangleModel1 :: RectModel1,
	rectangleModel2 :: RectModel2, rectangleModel3 :: RectModel3 }
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

dummy :: [WRect]
dummy = rectToRectRaw <$> [Rectangle
	(RectPos . Cglm.Vec2 $ (- 1) :. (- 1) :. NilL)
	(RectSize . Cglm.Vec2 $ 0.3 :. 0.3 :. NilL)
	(RectColor . Cglm.Vec4 $ 1.0 :. 0.0 :. 0.0 :. 0.0 :. NilL)
	(RectModel Cglm.mat4Identity)]

type WViewProj = StrG.W ViewProjection

data ViewProjection = ViewProjection {
	viewProjectionView :: Cglm.Mat4, viewProjectionProj :: Cglm.Mat4 }
	deriving (Show, Generic)

viewProjIdentity :: WViewProj
viewProjIdentity = StrG.W ViewProjection {
	viewProjectionView = Cglm.mat4Identity,
	viewProjectionProj = Cglm.mat4Identity }

instance StrG.G ViewProjection

instance Default ViewProjection where
	def = ViewProjection Cglm.mat4Identity Cglm.mat4Identity

-- SHADERS

[glslVertexShader|

#version 450

layout(binding = 0) uniform ViewProjection { mat4 view; mat4 proj; } vp;

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
	gl_Position = vp.proj * vp.view * rectModel *
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
