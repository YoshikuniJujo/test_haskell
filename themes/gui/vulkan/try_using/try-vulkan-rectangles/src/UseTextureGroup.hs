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

module UseTextureGroup (

	-- * RECTANGLES

	useTextureGroup,

	-- * COMMAND

	Command(..),

	-- ** VIEW PROJECTION

	ViewProjection(..),

	-- ** RECTANGLE

	Rectangle(..),
	RectPos(..), RectSize(..), RectColor(..),
	RectModel0(..), RectModel1(..), RectModel2(..), RectModel3(..),

	-- * EVENT

	Event(..),

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
import Data.HeteroParList.Constrained (pattern (:^*))
import Data.HeteroParList.Constrained qualified as HPListC
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
import Gpu.Vulkan.Pipeline.Graphics qualified as Vk.Ppl.Graphics
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

import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swpch
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.PhysicalDevice qualified as Vk.Khr.Sfc.Phd
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw.Win
import Gpu.Vulkan.Ext.DebugUtils qualified as Vk.DbgUtls
import Gpu.Vulkan.Ext.DebugUtils.Messenger qualified as Vk.DbgUtls.Msgr
import Gpu.Vulkan.Cglm qualified as Cglm

import Tools hiding (onlyIf)

import Graphics.UI.GlfwG qualified as GlfwG
import Graphics.UI.GlfwG.Window qualified as GlfwG.Win
import Graphics.UI.GlfwG.Window.Type as GlfwG.Win
import Graphics.UI.GlfwG.Key as GlfwG.Ky
import Graphics.UI.GlfwG.Mouse as GlfwG.Ms

import CreateTextureGroup

import Codec.Picture qualified as Pct

import Debug

import Data.Maybe.ToolsYj
import Data.List.ToolsYj

import Data.Ord.ToolsYj
import Data.Bool.ToolsYj
import Data.Either.ToolsYj

import Gpu.Vulkan.PushConstant qualified as Vk.PushConstant

----------------------------------------------------------------------
--
-- * USE TEXTURE GROUP
-- * BODY
-- * WINDOW OBJECTS
-- * CREATE AND COPY BUFFERS
-- * MAINLOOP
-- * DATA TYPES
-- * SHADERS
--
----------------------------------------------------------------------

-- USE TEXTURE GROUP

useTextureGroup :: forall k . (Show k, Ord k, Succable k) =>
	TChan (Command k) -> TChan (Event k) ->
	TVar (M.Map k (TVar Vk.Extent2d)) -> Pct.Image Pct.PixelRGBA8 -> IO ()
useTextureGroup ip op vex pct = GlfwG.init error $
	createIst \ist -> Vk.Dvc.group nil \dvg -> bool id (dbgm ist) debug $
	GlfwG.Win.group \wg -> initWin False wg () >>= \dw ->
	crsfc dw ist \dsfc -> pickPhd ist dsfc >>= \(pd, qfis) ->
	querySwpchSupport pd dsfc \ssd ->
	chooseSwpSfcFmt (formats ssd) \(_ :: Vk.Khr.Sfc.Format fmt) ->
	createLgDvc pd dvg () qfis >>= \(dv, gq, pq) ->
	swapExtent dw (capabilities ssd) >>= \ex ->
	swpchImgNum @fmt dv dsfc ssd ex qfis >>= \n -> num n \(_ :: Proxy n) ->
	body @n @fmt ip op vex ist pd qfis dv gq pq pct >>
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
	= OpenWindow | DestroyWindow k | GetEvent | EndWorld
	| Draw (M.Map k (ViewProjection, [Rectangle]))
	| SetPicture k (Pct.Image Pct.PixelRGBA8)

data Event k
	= EventOpenWindow k | EventDeleteWindow k | EventNeedRedraw | EventEnd
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
		Vk.applicationInfoApplicationName = "Use Texture Group",
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
	unqqfs = L.nub [grFam qfis, prFam qfis]
	qinfo qf = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dvc.queueCreateInfoQueuePriorities = [1] }
	cinfo qs = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = qs,
		Vk.Dvc.createInfoEnabledLayerNames = bool [] vldLayers debug,
		Vk.Dvc.createInfoEnabledExtensionNames = dvcExtensions,
		Vk.Dvc.createInfoEnabledFeatures =
			Just def { Vk.Phd.featuresSamplerAnisotropy = True } }

swpchImgNum :: forall (fmt :: Vk.T.Format) sd ssfc fmts .
	Vk.T.FormatToValue fmt =>
	Vk.Dvc.D sd -> Vk.Khr.Sfc.S ssfc -> SwpchSupportDetails fmts ->
	Vk.Extent2d -> QFamIdcs -> IO [()]
swpchImgNum dv sfc ssd ex qfis =
	Vk.Khr.Swpch.group @fmt dv nil \scg ->
	createSwpch' sfc qfis scg () ssd ex >>= \sc ->
	map (const ()) <$> Vk.Khr.Swpch.getImages dv sc

-- BODY

body :: forall (n :: [()]) (scfmt :: Vk.T.Format) k si sd . (
	HPList.HomoListN n, NumToVal n, Vk.T.FormatToValue scfmt,
	Show k, Ord k, Succable k ) =>
	TChan (Command k) -> TChan (Event k) ->
	TVar (M.Map k (TVar Vk.Extent2d)) -> Vk.Ist.I si -> Vk.Phd.P ->
	QFamIdcs -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q -> Image Pct.PixelRGBA8 ->
	IO ()
body ip op vex ist pd qfis dv gq pq pct =
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
	Vk.Ppl.Graphics.group dv nil \gpg ->
	Vk.Smph.group dv nil \iasg -> Vk.Smph.group dv nil \rfsg ->
	Vk.Fnc.group dv nil \iffg ->
	Vk.Bffr.group dv nil \rbg -> Vk.Mm.group dv nil \rmg ->
	let	rgs = (rbg, rmg) in
	createDscPl dv \dp -> Vk.DscSt.group dv \dsg ->
	atomically (newTVar M.empty) >>= \ges ->
	let	crwos = provideWinObjs @n @scfmt
			op vex pd dv gq cp qfis pl wg gs dsl dp dsg vp rgs ges
		drwos = destroyWinObjs @n wg gs dsg rgs ges
		gs = (sfcg, scg, ivg, rpg, fbg, gpg, iasg, rfsg, iffg) in
	txGroup dv \txg -> createTxSmplr pd dv \txs ->
	let	crtx k p = createTx pd dv gq cp dsg txg txs (ImageRgba8 p) k
		drtx = destroyTx txg in
	mainloop @n @siv @sf
		ip op dvs pl crwos drwos vbs rgs dsg vpm ges crtx pct drtx

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

createPplLyt :: forall alu sd mnmvp nmt b . Vk.Dvc.D sd -> (forall sdsl sl .
	Vk.DscStLyt.D sdsl (DscStLytArg alu mnmvp nmt) ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp nmt)] '[] ->
	IO b) -> IO b
createPplLyt dv f = createDSLyt dv \d -> Vk.PplLyt.create dv (info d) nil $ f d
	where
	info :: Vk.DscStLyt.D sdsl (DscStLytArg alu mnmvp nmt) ->
		Vk.PplLyt.CreateInfo 'Nothing
			'[ '(sdsl, DscStLytArg alu mnmvp nmt)]
			('Vk.PushConstant.Layout '[] '[])
	info d = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 d }

createDSLyt :: Vk.Dvc.D sd -> (forall (s :: Type) .
	Vk.DscStLyt.D s (DscStLytArg alu mnmvp nmt)  -> IO a) -> IO a
createDSLyt dv = Vk.DscStLyt.create dv info nil
	where
	info :: Vk.DscStLyt.CreateInfo 'Nothing (DscStLytArg alu mnmvp nmt)
	info = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = vpb :** txb :** HPList.Nil }
	vpb :: Vk.DscStLyt.Binding
		('Vk.DscStLyt.Buffer '[Vk.Obj.Atom alu WViewProj mnmvp])
	vpb = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeUniformBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageVertexBit }
	txb :: Vk.DscStLyt.Binding
		('Vk.DscStLyt.Image '[ '(nmt, Vk.T.FormatR8g8b8a8Srgb)])
	txb = Vk.DscStLyt.BindingImage {
		Vk.DscStLyt.bindingImageDescriptorType =
			Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscStLyt.bindingImageStageFlags = Vk.ShaderStageFragmentBit }

type DscStLytArg alu mnmvp nmt = '[
	'Vk.DscStLyt.Buffer '[AtomViewProj alu mnmvp],
	'Vk.DscStLyt.Image '[ '(nmt, 'Vk.T.FormatR8g8b8a8Srgb)] ]

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
		Vk.DscPl.createInfoMaxSets = 15,
		Vk.DscPl.createInfoPoolSizes = [szvp, sztx] }
	szvp = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscPl.sizeDescriptorCount = 15 }
	sztx = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeCombinedImageSampler,
		Vk.DscPl.sizeDescriptorCount = 15 }

-- PROVIDE WINDOW OBJECTS

provideWinObjs :: forall (n :: [()]) (scfmt :: Vk.T.Format) k
	si sd sc sl sdsl bnmvp alu mnmvp nmt sw ssfc ssc siv nmi sr sf sg
	sias srfs siff smvp sbvp sdp sds smr sbr bnmr nmr .
	(HPList.HomoListN n, Vk.T.FormatToValue scfmt, Show k, Ord k, KnownNat alu) =>
	TChan (Event k) -> TVar (M.Map k (TVar Vk.Extent2d)) ->
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc -> QFamIdcs ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp nmt)] '[] ->
	GlfwG.Win.Group sw k ->
	WinObjGroups k si ssfc sd scfmt
		ssc siv nmi sr sf sg sl sdsl alu mnmvp nmt sias srfs siff ->
	Vk.DscStLyt.D sdsl (DscStLytArg alu mnmvp nmt) -> Vk.DscPl.P sdp ->
	Vk.DscSt.Group sd sds k sdp '[ '(sdsl, DscStLytArg alu mnmvp nmt)] ->
	Vk.Bffr.Binded smvp sbvp bnmvp '[AtomViewProj alu mnmvp] ->
	RectGroups sd smr sbr bnmr nmr k -> TVar (M.Map k (IO ())) -> k ->
	IO (WinObjs sw ssfc scfmt ssc nmi (HPList.Replicate n siv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnmvp nmt sias srfs siff)
provideWinObjs op vexs pd dv gq cp qfis pl wg gs dsl dp dsg vp rgs ges k =
	(,) <$> initWin True wg k <*> atomically (newTVar False) >>= \(w, fr) ->
	winObjs @n vexs pd dv qfis pl w fr gs k <* do
	_ <- createDscSt' dv dsl dsg k dp vp
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
	where wName = "Use Texture Group"; wSize = (800, 600)

createDscSt' :: forall sd sdsl alu mnmvp nmt sds sp smvp sbvp bnmvp k .
	(KnownNat alu, Ord k) =>
	Vk.Dvc.D sd -> Vk.DscStLyt.D sdsl (DscStLytArg alu mnmvp nmt) ->
	Vk.DscSt.Group sd sds k sp '[ '(sdsl, DscStLytArg alu mnmvp nmt)] -> k ->
	Vk.DscPl.P sp -> Vk.Bffr.Binded smvp sbvp bnmvp '[AtomViewProj alu mnmvp] ->
	IO (Vk.DscSt.D sds '(sdsl, DscStLytArg alu mnmvp nmt))
createDscSt' dv dsl dsg k dp bvp =
	Vk.DscSt.allocateDs' dsg k info >>= \(forceRight' -> HPList.Singleton ds) ->
	Vk.DscSt.updateDs dv (HPList.Singleton . U5 $ wr ds) HPList.Nil >> pure ds
	where
	info = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dp,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }
	wr :: Vk.DscSt.D sds slbts -> Vk.DscSt.Write 'Nothing sds slbts
		('Vk.DscSt.WriteSourcesArgBuffer
			'[ '(smvp, sbvp, bnmvp, AtomViewProj alu mnmvp, 0)]) 0
	wr ds = Vk.DscSt.Write {
		Vk.DscSt.writeNext = TMaybe.N,
		Vk.DscSt.writeDstSet = ds,
		Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeUniformBuffer,
		Vk.DscSt.writeSources = Vk.DscSt.BufferInfos
			. HPList.Singleton . U5 $ Vk.Dsc.BufferInfo bvp }

-- WINDOW OBJECTS

winObjs :: forall (n :: [()]) (scfmt :: Vk.T.Format) k
	si sd sl sdsl alu mnmvp nmt sw ssfc ssc sv nmi sr sf sg sias srfs siff .
	(HPList.HomoListN n, Vk.T.FormatToValue scfmt, Ord k) =>
	TVar (M.Map k (TVar Vk.Extent2d)) ->
	Vk.Phd.P -> Vk.Dvc.D sd -> QFamIdcs ->
	Vk.PplLyt.P sl '[ '(sdsl, DscStLytArg alu mnmvp nmt)] '[] ->
	GlfwG.Win.W sw -> FramebufferResized -> WinObjGroups k si ssfc sd scfmt
		ssc sv nmi sr sf sg sl sdsl alu mnmvp nmt sias srfs siff -> k ->
	IO (WinObjs sw ssfc scfmt ssc nmi (HPList.Replicate n sv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnmvp nmt sias srfs siff)
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
	si sd sl sdsl alu mnmvp nmt sw ssfc ssc siv nm sr sf sg sias srfs siff
	smr sbr bnmr nmr sdp sds . (NumToVal n,  Ord k) =>
	GlfwG.Win.Group sw k -> WinObjGroups k si ssfc sd scfmt
		ssc siv nm sr sf sg sl sdsl alu mnmvp nmt sias srfs siff ->
	Vk.DscSt.Group sd sds k sdp '[ '(sdsl, DscStLytArg alu mnmvp nmt)] ->
	RectGroups sd smr sbr bnmr nmr k -> TVar (M.Map k (IO ())) -> k -> IO ()
destroyWinObjs wg (sfcg, scg, ivg, rpg, fbg, gpg, iasg, rfsg, iffg)
	dsg (rbg, rmg) ges k = GlfwG.Win.lookup wg k >>= \mw -> do
	case mw of
		Nothing -> pure ()
		Just w -> do
			GlfwG.Win.setShouldClose w True
			tr =<< GlfwG.Win.unsafeDestroy wg k
			tr =<< Vk.Khr.Sfc.unsafeDestroy sfcg k
			tr =<< Vk.Khr.Swpch.unsafeDestroy scg k
			tr =<< Vk.RndrPss.unsafeDestroy rpg k
			for_ [0 .. numToVal @n - 1] \i -> do
				tr =<< Vk.ImgVw.unsafeDestroy ivg (k, i)
				tr =<< Vk.Frmbffr.unsafeDestroy fbg (k, i)
			tr =<< Vk.Ppl.Graphics.unsafeDestroyGs gpg k
			tr =<< Vk.Smph.unsafeDestroy iasg k
			tr =<< Vk.Smph.unsafeDestroy rfsg k
			tr =<< Vk.Fnc.unsafeDestroy iffg k
			tr =<< Vk.DscSt.unsafeFreeDs dsg k
			tr =<< Vk.Bffr.unsafeDestroy rbg k
			tr =<< Vk.Mm.unsafeFree rmg k
	atomically (modifyTVar ges (M.delete k))
	where tr = either error pure

class NumToVal (n :: [()]) where numToVal :: Int
instance NumToVal '[] where numToVal = 0
instance NumToVal n => NumToVal ('() ': n) where numToVal = 1 + numToVal @n

data WinObjs sw ssfc scfmt
	ssc nmscv svs sr sfs sg sl sdsl alu mnmvp nmt sias srfs siff = WinObjs
	(WinEnvs sw) (Vk.Khr.Sfc.S ssfc) (TVar Vk.Extent2d)
	(Swapchains scfmt ssc nmscv svs sr sfs)
	(Pipeline sg sl sdsl alu mnmvp nmt) (SyncObjs '(sias, srfs, siff))

type WinEnvs sw = (GlfwG.Win.W sw , FramebufferResized)
type FramebufferResized = TVar Bool

type WinObjGroups k si ssfc sd scfmt
	ssc siv nmi sr sf sg sl sdsl alu mnmvp nmt sias srfs siff = (
	Vk.Khr.Sfc.Group si 'Nothing ssfc k,
	Vk.Khr.Swpch.Group sd 'Nothing scfmt ssc k,
	Vk.ImgVw.Group sd 'Nothing siv (k, Int) nmi scfmt,
	Vk.RndrPss.Group sd 'Nothing sr k,
	Vk.Frmbffr.Group sd 'Nothing sf (k, Int),
	Vk.Ppl.Graphics.Group sd 'Nothing sg k '[ '(
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(Rectangle, 'Vk.VtxInp.RateInstance) ],
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3), '(9, TexCoord) ],
		'(sl, '[ '(sdsl, DscStLytArg alu mnmvp nmt)], '[]) )],
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
	where pmd = fromMaybe
		Vk.Khr.PresentModeFifo . L.find (== Vk.Khr.PresentModeMailbox)

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
	<$> (\(i, img) -> createImgVw vg (k, i) img) `mapM` zip [0 ..] is

createImgVw :: forall sd sv k nm vfmt sm si ifmt .
	(Ord k, Vk.T.FormatToValue vfmt) =>
	Vk.ImgVw.Group sd 'Nothing sv (k, Int) nm vfmt ->
	(k, Int) -> Vk.Img.Binded sm si nm ifmt -> IO (Vk.ImgVw.I nm vfmt sv)
createImgVw vg k i = forceRight' <$> Vk.ImgVw.create' vg k (imgVwInfo i)

recreateImgVws :: Vk.T.FormatToValue scfmt => Vk.Dvc.D sd ->
	[Vk.Img.Binded ss ss nm scfmt] -> HPList.PL (Vk.ImgVw.I nm scfmt) sis ->
	IO ()
recreateImgVws _ [] HPList.Nil = pure ()
recreateImgVws dv (i : is) (v :** vs) = do
	Vk.ImgVw.unsafeRecreate dv (imgVwInfo i) nil v
	recreateImgVws dv is vs
recreateImgVws _ _ _ = error "number of Vk.Img.I and Vk.ImgVw.I should be same"

createRndrPss :: forall scfmt sd ma sr k .
	(Vk.T.FormatToValue scfmt, Vk.AllocationCallbacks.ToMiddle ma, Ord k) =>
	Vk.RndrPss.Group sd ma sr k -> k -> IO (Vk.RndrPss.R sr)
createRndrPss rpg k = forceRight' <$> Vk.RndrPss.create' @_ @_ @_ rpg k info
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
	Vk.RndrPss.R sr -> HPList.PL (Vk.ImgVw.I nm fmt) (HPList.Replicate n sv) ->
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
	RectGroups sd sm sb bnm nm k -> k -> [Rectangle] ->
	IO (Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List Rectangle nm])
createRctBffr pd dv gq cp (bg, mg) k rs =
	createBffrLst' pd dv bg mg k (fromIntegral $ length rs)
		(Vk.Bffr.UsageTransferDstBit .|. Vk.Bffr.UsageVertexBufferBit)
		Vk.Mm.PropertyDeviceLocalBit >>= \(b, _) ->
	b <$ createBffrLst pd dv (fromIntegral $ length rs)
		Vk.Bffr.UsageTransferSrcBit
		(Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit)
		\(c :: Vk.Bffr.Binded m b bn '[Vk.ObjNA.List t n]) m ->
		Vk.Mm.write @bn @(Vk.ObjNA.List t n) @0 dv m zeroBits rs >>
		copyBuffer dv gq cp c b

destroyRctBffr :: Ord k => RectGroups sd sm sb nm nmrct k -> k -> IO ()
destroyRctBffr (bg, mg) k =
	(,) <$> Vk.Mm.unsafeFree mg k <*> Vk.Bffr.unsafeDestroy bg k >>= \case
		(Left m, _) -> error m; (_, Left m) -> error m; _ -> pure ()

type RectGroups sd sm sb bnm nm k = (
	Vk.Bffr.Group sd 'Nothing sb k bnm '[Vk.Obj.List 1 Rectangle nm],
	Vk.Mm.Group sd 'Nothing sm k
		'[ '(sb, 'Vk.Mm.BufferArg bnm '[Vk.Obj.List 1 Rectangle nm])] )

-- GET GLFW EVENTS

setGlfwEvents :: (Ord k, Show k) =>
	TChan (Event k) -> GlfwG.Win.W sw -> FramebufferResized ->
	TVar (M.Map k (IO ())) -> k -> IO ()
setGlfwEvents op w fr ges k =
	GlfwG.Win.setKeyCallback w
		(Just \_w ky sc' act mods -> do
			putStrLn $
				show w ++ " " ++ show ky ++ " " ++
				show sc' ++ " " ++ show act ++ " " ++ show mods
			case act of
				GlfwG.Ky.KeyState'Pressed ->
					atomically $ writeTChan op $ EventKeyDown k ky
				GlfwG.Ky.KeyState'Released ->
					atomically $ writeTChan op $ EventKeyUp k ky
				_ -> pure ()
			) >>
	GlfwG.Win.setFramebufferSizeCallback w
		(Just \_ _ _ -> atomically $ writeTVar fr True) >>

	let	initMouseButtonStates = foldr (uncurry M.insert) M.empty
			$ (, GlfwG.Ms.MouseButtonState'Released) <$>
				[GlfwG.Ms.MouseButton'1 .. GlfwG.Ms.MouseButton'8] in
	atomically (newTVar False) >>= \vb ->
	atomically (newTVar initMouseButtonStates) >>= \vmbs ->
	atomically (modifyTVar ges (M.insert k (glfwEvents k w op vb vmbs)))

glfwEvents :: Show k => k -> GlfwG.Win.W sw -> TChan (Event k) -> TVar Bool -> TVar MouseButtonStateDict -> IO ()
glfwEvents k w outp vscls vmb1p = do
--	threadDelay 10000
	GlfwG.pollEvents
	cls <- GlfwG.Win.shouldClose w
	scls <- atomically $ readTVar vscls
	atomically $ writeTVar vscls cls
--	when cls . putStrLn $ "glfwEvents: scls = " ++ show scls
	when (not scls && cls) do
		putStrLn $ "send EventDeleteWindow " ++ show k
		atomically . writeTChan outp $ EventDeleteWindow k
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
	where
	mAny p = M.foldr (\x b -> p x || b) False

getMouseButtons :: GlfwG.Win.W sw -> IO MouseButtonStateDict
getMouseButtons w = foldr (uncurry M.insert) M.empty . zip bs
	<$> GlfwG.Ms.getButton w `mapM` bs
	where bs = [GlfwG.Ms.MouseButton'1 .. GlfwG.Ms.MouseButton'8]

type MouseButtonStateDict = M.Map GlfwG.Ms.MouseButton GlfwG.Ms.MouseButtonState

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

-- GRAPHICS PIPELINE

createGrPpl :: (Ord k, Vk.AllocationCallbacks.ToMiddle mac) =>
	Vk.Ppl.Graphics.Group sd mac sg k '[ '(
		'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)],
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ],
			'(sl, '[AtomUbo sdsl alu mnm nmt], '[]) )] -> k ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[AtomUbo sdsl alu mnm nmt] '[] -> IO (
		Vk.Ppl.Graphics.G sg
			'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)]
			'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3),
				'(2, RectPos), '(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
				'(9, TexCoord) ]
			'(sl, '[AtomUbo sdsl alu mnm nmt], '[]))
createGrPpl gpgrp k sce rp pllyt =
	Vk.Ppl.Graphics.createGs' gpgrp k Nothing (U14 pplInfo :** HPList.Nil)
			>>= \(forceRight' -> (U3 gpl :** HPList.Nil)) -> pure gpl
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp pllyt

type AtomUbo s alu mnmvp tnm = '(s, '[
	'Vk.DscStLyt.Buffer '[Vk.Obj.Atom alu WViewProj mnmvp],
	'Vk.DscStLyt.Image '[ '(tnm, 'Vk.T.FormatR8g8b8a8Srgb)] ])

recreateGraphicsPipeline :: Vk.Dvc.D sd ->
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[AtomUbo sdsl alu mnm "texture"] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl alu mnm "texture"], '[]) -> IO ()
recreateGraphicsPipeline dvc sce rp pllyt gpls = Vk.Ppl.Graphics.unsafeRecreateGs
	dvc Nothing (U14 pplInfo :** HPList.Nil) nil (U3 gpls :** HPList.Nil)
	where pplInfo = mkGraphicsPipelineCreateInfo' sce rp pllyt

mkGraphicsPipelineCreateInfo' ::
	Vk.Extent2d -> Vk.RndrPss.R sr -> Vk.PplLyt.P sl '[AtomUbo sdsl alu mnm nmt] '[] ->
	Vk.Ppl.Graphics.CreateInfo 'Nothing '[
			'( 'Nothing, 'Nothing, 'GlslVertexShader, 'Nothing, '[]),
			'( 'Nothing, 'Nothing, 'GlslFragmentShader, 'Nothing, '[]) ]
		'(	'Nothing,
			'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)],
			'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
				'(2, RectPos), '(3, RectSize), '(4, RectColor),
				'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ] )
		'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing 'Nothing '(sl, '[AtomUbo sdsl alu mnm nmt], '[]) sr '(sb, vs', ts', foo)
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

shaderModuleCreateInfo :: SpirV.S sknd -> Vk.ShaderModule.CreateInfo 'Nothing sknd
shaderModuleCreateInfo code = Vk.ShaderModule.CreateInfo {
	Vk.ShaderModule.createInfoNext = TMaybe.N,
	Vk.ShaderModule.createInfoFlags = def,
	Vk.ShaderModule.createInfoCode = code }

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

copyBffrLst :: forall sd sc sm sb bnm t lnm sm' sb' bnm' . Storable' t =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb bnm '[Vk.ObjNA.List t lnm] ->
	Vk.Bffr.Binded sm' sb' bnm' '[Vk.ObjNA.List t lnm] -> IO ()
copyBffrLst dv gq cp s d = singleTimeCmds dv gq cp \cb ->
	Vk.Cmd.copyBuffer @'[ '( '[Vk.ObjNA.List t lnm], 0, 0)] cb s d

singleTimeCmds :: forall sd sc a . Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.CmdPl.C sc -> (forall s . Vk.CmdBffr.C s -> IO a) -> IO a
singleTimeCmds dv gq cp cmd =
	Vk.CmdBffr.allocate dv ainfo \(cb :*. HPList.Nil) ->
	Vk.CmdBffr.begin @_ @'Nothing cb binfo (cmd cb) <* do
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

createBffr :: forall sd nm o a . Vk.Obj.SizeAlignment o =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Obj.Length o ->
	Vk.Bffr.UsageFlags -> Vk.Mm.PropertyFlags -> (forall sm sb .
		Vk.Bffr.Binded sm sb nm '[o] ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[o])] -> IO a) -> IO a
createBffr p dv ln us prs f = Vk.Bffr.create dv (bffrInfo ln us) nil \b -> do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mt <- findMmType p (Vk.Mm.requirementsMemoryTypeBits rqs) prs
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b)
		(ainfo mt) nil
		$ f . \(HPList.Singleton (U2 (Vk.Mm.BufferBinded bb))) -> bb
	where ainfo mt = Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mt }

findMmType ::
	Vk.Phd.P -> Vk.Mm.TypeBits -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmType pd flt prs =
	fromMaybe (error msg) . suit <$> Vk.Phd.getMemoryProperties pd
	where
	msg = "failed to find suitable memory type!"
	suit prs1 = fst <$> L.find ((&&)
		<$> (`Vk.Mm.elemTypeIndex` flt) . fst
		<*> checkBits prs . Vk.Mm.mTypePropertyFlags . snd)
			(Vk.Phd.memoryPropertiesMemoryTypes prs1)

bffrInfo ::
	Vk.Obj.Length s -> Vk.Bffr.UsageFlags -> Vk.Bffr.CreateInfo Nothing '[s]
bffrInfo ln us = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton ln,
	Vk.Bffr.createInfoUsage = us,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

createBffrLst' :: forall sd nm t sm sb k nm' al . (Ord k, Storable t, KnownNat al) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.Group sd 'Nothing sb k nm '[Vk.Obj.List al t nm']  ->
	Vk.Mm.Group sd 'Nothing sm k '[ '(sb, 'Vk.Mm.BufferArg nm '[Vk.Obj.List al t nm'])] ->
	k ->
	Vk.Dvc.Size -> Vk.Bffr.UsageFlags ->
	Vk.Mm.PropertyFlags -> IO (
		Vk.Bffr.Binded sm sb nm '[Vk.Obj.List al t nm'],
		Vk.Mm.M sm '[ '(
			sb, 'Vk.Mm.BufferArg nm '[Vk.Obj.List al t nm'] ) ] )
createBffrLst' p dv bgrp mgrp k ln usg props =
	createBuffer' p dv bgrp mgrp k (Vk.Obj.LengthList ln) usg props

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

copyBuffer :: forall sd sc sm sb nm sm' sb' nm' a nmm al . (Storable' a, KnownNat al) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C sc ->
	Vk.Bffr.Binded sm sb nm '[Vk.Obj.List al a nmm] ->
	Vk.Bffr.Binded sm' sb' nm' '[Vk.Obj.List al a nmm] -> IO ()
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
			Vk.Cmd.copyBuffer @'[ '( '[Vk.Obj.List al a nmm], 0, 0)] cb src dst
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

-- MAINLOOP

mainloop ::
	forall n siv sf scfmt sw ssfc sd sc scb sias srfs siff ssc nm sr sg sl
		sdsl sm sb sm' sb' nm' srm srb sm2 sb2 k mnm sdp sds' alu .
	(HPList.HomoListN n, Vk.T.FormatToValue scfmt, Ord k, Show k, Succable k, KnownNat alu) =>
	TChan (Command k) -> TChan (Event k) -> Devices sd sc scb -> PipelineLayout sl sdsl alu mnm ->

	(k -> IO (WinObjs sw ssfc
		scfmt ssc nm (HPList.Replicate n siv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnm "texture" sias srfs siff
		)) ->

	(k -> IO ()) ->

	VertexBuffers sm sb nm sm' sb' nm' ->
	RectGroups sd srm srb nm "" k ->
	Vk.DscSt.Group sd sds' k sdp '[(AtomUbo sdsl alu mnm "texture")] ->
	ViewProjMemory sm2 sb2 "uniform-buffer" alu mnm ->
	TVar (M.Map k (IO ())) ->
	(k -> Pct.Image Pct.PixelRGBA8 -> IO ()) -> Pct.Image Pct.PixelRGBA8 ->
	(k -> IO ()) -> IO ()
mainloop inp outp dvs@(_, _, dvc, _, _, _, _) pll crwos drwos vbs rgrps dsg vpm ges crtx pct0 drcr = do
	(vwid, vws) <- atomically ((,) <$> newTVar zero' <*> newTVar M.empty)
	let	crwos' = do
			wi <- atomically do
				i <- readTVar vwid
				i <$ modifyTVar vwid succ'
			atomically . modifyTVar vws . M.insert wi =<< crwos wi
			crtx wi pct0
			pure wi
	fix \loop -> do
		atomically (readTChan inp) >>= \case
			Draw ds' -> do
				Vk.Dvc.waitIdle dvc
				ws <- atomically $ readTVar vws
				runLoop' @n @siv @sf dvs pll ws vbs rgrps (rectsToDummy ds') dsg vpm outp loop
			SetPicture k pct -> do
				putStrLn "DRAW PICTURE BEGIN"
				drcr k >> crtx k pct
				Vk.Dvc.waitIdle dvc
				loop
			OpenWindow ->
				crwos' >>= atomically . writeTChan outp . EventOpenWindow >> loop
			DestroyWindow k -> do
				putStrLn $ "DESTROY WINDOW: " ++ show k
				atomically (modifyTVar vws (M.delete k)) >> drwos k
				ws <- atomically $ readTVar vws
				GlfwG.pollEvents
				cls <- and <$> GlfwG.Win.shouldClose `mapM` (winObjsToWin <$> ws)
				putStrLn "DestroyWindow: before if"
				if cls then pure () else loop
			GetEvent -> do
				atomically (readTVar ges) >>= sequence_
				loop
			EndWorld -> pure ()

class Succable n where zero' :: n; succ' :: n -> n

instance Succable Bool where
	zero' = False; succ' = \case False -> True; True -> error "no more"

instance Succable Int where zero' = 0; succ' = succ

rectsToDummy :: M.Map k (ViewProjection, [Rectangle]) -> M.Map k (WViewProj, [Rectangle])
rectsToDummy = M.map \(tm, rects) -> (StrG.W tm, bool rects dummy $ null rects)

type Devices sd scp scb = (
	Vk.Phd.P, QFamIdcs, Vk.Dvc.D sd,
	Vk.Q.Q, Vk.Q.Q, Vk.CmdPl.C scp, Vk.CmdBffr.C scb )

type Swapchains scfmt ssc nm ss sr sfs = (
	Vk.Khr.Swpch.S scfmt ssc,
	HPList.PL (Vk.ImgVw.I nm scfmt) ss,
	Vk.RndrPss.R sr, HPList.PL Vk.Frmbffr.F sfs )

type Pipeline sg sl sdsl alu mnm nmt = Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl alu mnm nmt], '[])

type PipelineLayout sl sdsl alu mnm = Vk.PplLyt.P sl '[AtomUbo sdsl alu mnm "texture"] '[]

type VertexBuffers sm sb nm sm' sb' nm' = (
	Vk.Bffr.Binded sm sb nm '[Vk.ObjNA.List WVertex ""],
	Vk.Bffr.Binded sm' sb' nm' '[Vk.ObjNA.List Word16 ""] )

data Recreates sw sl nm ssfc sr sg sdsl alu mnm fmt ssc sis sfs = Recreates
	(GlfwG.Win.W sw) (Vk.Khr.Sfc.S ssfc)
	(TVar Vk.Extent2d)
	(Vk.RndrPss.R sr)
	(Vk.Ppl.Graphics.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(Rectangle, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl alu mnm "texture"], '[]))
	(Vk.Khr.Swpch.S fmt ssc)
	(HPList.PL (Vk.ImgVw.I nm fmt) sis)
	(HPList.PL Vk.Frmbffr.F sfs)

winObjsToRecreates ::
	WinObjs sw ssfc
		scfmt ssc nm sscivs sr sfs
		sg sl sdsl alu mnm "texture" sias srfs siff
		->
	Recreates sw sl nm ssfc sr sg sdsl alu mnm scfmt ssc sscivs sfs
winObjsToRecreates (WinObjs (w, _) sfc vex (sc, scivs, rp, fbs) gpl _iasrfsifs) =
	Recreates w sfc vex rp gpl sc scivs fbs

data Draws sl sr sg sdsl alu mnm sias srfs siff fmt ssc sfs = Draws
	(TVar Vk.Extent2d) (Vk.RndrPss.R sr)
	(Vk.Ppl.Graphics.G sg
		'[	'(WVertex, 'Vk.VtxInp.RateVertex),
			'(Rectangle, 'Vk.VtxInp.RateInstance) ]
		'[	'(0, Cglm.Vec2), '(1, Cglm.Vec3), '(2, RectPos),
			'(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1),
			'(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl alu mnm "texture"], '[]))
	(SyncObjs '(sias, srfs, siff))
	(Vk.Khr.Swpch.S fmt ssc)
	(HPList.PL Vk.Frmbffr.F sfs)

winObjsToDraws ::
	WinObjs sw ssfc
		scfmt ssc nm sscivs sr sfs
		sg sl sdsl alu mnm "texture" sias srfs siff
		->
	Draws sl sr sg sdsl alu mnm sias srfs siff scfmt ssc sfs
winObjsToDraws (WinObjs _ _sfc vex (sc, _scivs, rp, fbs) gpl iasrfsifs) =
	Draws vex rp gpl iasrfsifs sc fbs

winObjsToWin ::
	WinObjs sw ssfc sg sl sdsl mnm sias srfs siff scfmt ssc alu nm "texture" sscivs sr sfs ->
	W sw
winObjsToWin (WinObjs (win, _) _ _ _ _ _) = win

runLoop' :: forall n (siv :: Type) (sf :: Type)
	sd sc scb sl
	sw ssfc sg sias srfs siff scfmt ssc sr
	smrct sbrct nmrct sds sdsl sm sb sm' sb' sm2 sb2 nm2 k mnm sdp alu .
	(HPList.HomoListN n, Vk.T.FormatToValue scfmt, Ord k, KnownNat alu) =>
	Devices sd sc scb -> Vk.PplLyt.P sl '[AtomUbo sdsl alu mnm "texture"] '[] ->
	(M.Map k (WinObjs sw ssfc
		scfmt ssc nmrct (HPList.Replicate n siv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnm "texture" sias srfs siff
		)) ->
	(	Vk.Bffr.Binded sm' sb' nmrct '[Vk.ObjNA.List WVertex ""],
		Vk.Bffr.Binded sm2 sb2 nm2 '[Vk.ObjNA.List Word16 ""] ) ->
	(	Vk.Bffr.Group sd 'Nothing sbrct k nmrct '[Vk.Obj.List 1 Rectangle ""],
		Vk.Mm.Group sd 'Nothing smrct k '[
			'(sbrct, 'Vk.Mm.BufferArg nmrct '[Vk.Obj.List 1 Rectangle ""])] ) ->
	M.Map k (WViewProj, [Rectangle]) ->
	Vk.DscSt.Group sd sds k sdp '[AtomUbo sdsl alu mnm "texture"] ->
	ViewProjMemory sm sb "uniform-buffer" alu mnm ->
	TChan (Event k) -> IO () -> IO ()
runLoop' (pd, qfis, dv, gq, pq, cp, cb)  pll ws (vb, ib) rgrps rectss dsg ubm outp loop = do
	for_ (M.toList ws) \(k', wos) -> do
		let	(tm, rects') = lookupRects rectss k'
		destroyRctBffr rgrps k'
		rb <- createRctBffr pd dv gq cp  rgrps k' rects'
		let	rb' = (rb, fromIntegral $ length rects')
		(fromJust -> HPList.Singleton dsnew) <- Vk.DscSt.lookup dsg k'
		catchAndDraw @n @siv @sf pd qfis dv gq pq pll vb rb' ib ubm dsnew cb tm wos
	cls <- and <$> GlfwG.Win.shouldClose `mapM` (winObjsToWin <$> ws)
	if cls then (pure ()) else do
		for_ ws \wos ->
			recreateAllIfNeed @n @siv @sf pd qfis dv pll wos outp
		loop

lookupRects :: Ord k =>
	M.Map k (WViewProj, [Rectangle]) -> k ->
	(WViewProj, [Rectangle])
lookupRects rs = fromMaybe (StrG.W viewProjectionIdentity, dummy) . (`M.lookup` rs)

catchAndDraw ::
	forall n siv sf
		sd sl sdsl sm sb smr sbr nm sm' sb' sm2 sb2 nm' sw ssfc sg sias srfs siff win ssc sr sds scb mnm alu .
	(HPList.HomoListN n, Vk.T.FormatToValue win, KnownNat alu) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.Q.Q -> Vk.PplLyt.P sl '[AtomUbo sdsl alu mnm "texture"] '[] ->
	Vk.Bffr.Binded sm sb nm '[Vk.ObjNA.List WVertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[Vk.Obj.List 1 Rectangle ""], Vk.Cmd.InstanceCount)  ->
	Vk.Bffr.Binded sm' sb' nm' '[Vk.ObjNA.List Word16 ""] ->
	ViewProjMemory sm2 sb2 "uniform-buffer" alu mnm -> Vk.DscSt.D sds (AtomUbo sdsl alu mnm "texture") ->
	Vk.CmdBffr.C scb ->
	WViewProj ->
	WinObjs sw ssfc
		win ssc nm (HPList.Replicate n siv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnm "texture" sias srfs siff
		->
	IO ()
catchAndDraw phdvc qfis dvc gq pq pllyt vb rb ib ubm ubds cb ubo wos = do
	catchAndRecreate @n @_ @siv @sf phdvc qfis dvc pllyt (winObjsToRecreates wos)
		$ drawFrame dvc gq pq pllyt (winObjsToDraws wos) vb rb ib ubm ubds cb ubo
	Vk.Dvc.waitIdle dvc

recreateAllIfNeed ::
	forall n siv sf
		sd sw ssfc sg sl sdsl sias srfs siff scfmt ssc nm sr k mnm alu .
	(Vk.T.FormatToValue scfmt, HPList.HomoListN n) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.PplLyt.P sl '[AtomUbo sdsl alu mnm "texture"] '[] ->
	WinObjs sw ssfc
		scfmt ssc nm (HPList.Replicate n siv) sr (HPList.Replicate n sf)
		sg sl sdsl alu mnm "texture" sias srfs siff
		-> TChan (Event k) -> IO ()
recreateAllIfNeed phdvc qfis dvc pllyt wos@(WinObjs (_, fbrszd) _ _ _ _ _) outp =
	checkFlag fbrszd >>= bool (pure ()) (do
		putStrLn "recreateAllIfNeed: needed"
		atomically $ writeTChan outp EventNeedRedraw
		recreateAll @n @siv @sf phdvc qfis dvc pllyt $ winObjsToRecreates wos)
	

drawFrame :: forall sfs sd ssc sr sl sg sm sb smr sbr nm sm' sb' nm' sm2 sb2 scb sias srfs siff sdsl scfmt sds mnm alu .
	KnownNat alu =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Q.Q ->
	Vk.PplLyt.P sl '[AtomUbo sdsl alu mnm "texture"] '[] ->
	Draws sl sr sg sdsl alu mnm sias srfs siff scfmt ssc sfs ->
	Vk.Bffr.Binded sm sb nm '[Vk.ObjNA.List WVertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[Vk.Obj.List 1 Rectangle ""], Vk.Cmd.InstanceCount) ->
	Vk.Bffr.Binded sm' sb' nm' '[Vk.ObjNA.List Word16 ""] ->
	ViewProjMemory sm2 sb2 "uniform-buffer" alu mnm ->
	Vk.DscSt.D sds (AtomUbo sdsl alu mnm "texture") ->
	Vk.CmdBffr.C scb ->
	WViewProj -> IO ()
drawFrame dvc gq pq
	pllyt
	(Draws vext rp gpl (SyncObjs ias rfs iff) sc fbs)
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

updateUniformBuffer' :: forall sd sm2 sb2 mnm alu . KnownNat alu => Vk.Dvc.D sd ->
	ViewProjMemory sm2 sb2 "uniform-buffer" alu mnm -> WViewProj -> IO ()
updateUniformBuffer' dvc um obj = do
	Vk.Mm.write @"uniform-buffer" @(Vk.Obj.Atom alu WViewProj mnm) @0
		dvc um zeroBits obj

catchAndSerialize :: IO () -> IO ()
catchAndSerialize =
	(`catch` \(Vk.MultiResult rs) -> sequence_ $ (throw . snd) `NE.map` rs)

catchAndRecreate ::
	forall n scfmt siv sf sw ssfc sd nm sr ssc sl sdsl sg mnm alu .
	(HPList.HomoListN n, Vk.T.FormatToValue scfmt) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.PplLyt.P sl '[AtomUbo sdsl alu mnm "texture"] '[] ->
	Recreates sw sl nm ssfc sr sg sdsl alu mnm scfmt
		ssc (HPList.Replicate n siv) (HPList.Replicate n sf) ->
	IO () -> IO ()
catchAndRecreate phdvc qfis dvc pllyt rcs act = catchJust
	(\case	Vk.ErrorOutOfDateKhr -> Just ()
		Vk.SuboptimalKhr -> Just ()
		_ -> Nothing)
	act
	\_ -> do
		putStrLn "catchAndRecreate: catched"
		recreateAll @n @siv @sf phdvc qfis dvc pllyt rcs

recreateAll :: forall
	n siv sf scfmt sw ssfc sd ssc nm sr sl sdsl sg mnm alu .
	(
	Vk.T.FormatToValue scfmt, HPList.HomoListN n ) =>
	Vk.Phd.P -> QFamIdcs -> Vk.Dvc.D sd ->
	Vk.PplLyt.P sl '[AtomUbo sdsl alu mnm "texture"] '[] ->
	Recreates sw sl nm ssfc sr sg sdsl alu mnm scfmt ssc (HPList.Replicate n siv) (HPList.Replicate n sf) ->
	IO ()
recreateAll
	phdvc qfis dvc pllyt
	(Recreates win sfc vex rp gpl sc scivs fbs) = do
	waitFramebufferSize win
	Vk.Dvc.waitIdle dvc

	ext <- recreateSwpch win sfc phdvc qfis dvc sc
	atomically $ writeTVar vex ext
	Vk.Khr.Swpch.getImages dvc sc >>= \imgs ->
		recreateImgVws dvc imgs scivs
	recreateGraphicsPipeline dvc ext rp pllyt gpl
	recreateFrmbffrs @n @_ @_ @_ @_ @siv @sf dvc ext rp scivs fbs

waitFramebufferSize :: GlfwG.Win.W sw -> IO ()
waitFramebufferSize win = GlfwG.Win.getFramebufferSize win >>= \sz ->
	when (zero sz) $ fix \loop -> (`when` loop) . zero =<<
		GlfwG.waitEvents *> GlfwG.Win.getFramebufferSize win
	where zero = uncurry (||) . ((== 0) *** (== 0))

recordCommandBuffer :: forall scb sr sf sl sg sm sb smr sbr nm sm' sb' nm' sdsl sds mnm alu .
	Vk.CmdBffr.C scb ->
	Vk.RndrPss.R sr -> Vk.Frmbffr.F sf -> Vk.Extent2d ->
	Vk.PplLyt.P sl '[AtomUbo sdsl alu mnm "texture"] '[] ->
	Vk.Ppl.Graphics.G sg
		'[ '(WVertex, 'Vk.VtxInp.RateVertex), '(Rectangle, 'Vk.VtxInp.RateInstance)]
		'[ '(0, Cglm.Vec2), '(1, Cglm.Vec3),
			'(2, RectPos), '(3, RectSize), '(4, RectColor),
			'(5, RectModel0), '(6, RectModel1), '(7, RectModel2), '(8, RectModel3),
			'(9, TexCoord) ]
		'(sl, '[AtomUbo sdsl alu mnm "texture"], '[]) ->
	Vk.Bffr.Binded sm sb nm '[Vk.ObjNA.List WVertex ""] ->
	(Vk.Bffr.Binded smr sbr nm '[Vk.Obj.List 1 Rectangle ""], Vk.Cmd.InstanceCount) ->
	Vk.Bffr.Binded sm' sb' nm' '[Vk.ObjNA.List Word16 ""] ->
	Vk.DscSt.D sds (AtomUbo sdsl alu mnm "texture") ->
	IO ()
recordCommandBuffer cb rp fb sce pllyt gpl vb (rb, ic) ib ubds =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.beginRenderPass cb rpInfo Vk.Subpass.ContentsInline $
	Vk.Cmd.bindPipelineGraphics cb Vk.Ppl.BindPointGraphics gpl \cbb ->
	Vk.Cmd.bindVertexBuffers cbb (
		U5 (Vk.Bffr.IndexedForList @_ @_ @_ @WVertex @"" vb) :**
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

-- DATA TYPES

type WVertex = StrG.W Vertex

data Vertex = Vertex {
	vertexPos :: Cglm.Vec2, vertexColor :: Cglm.Vec3,
	vertexTexCoord :: TexCoord }
	deriving (Show, Generic)

instance StrG.G Vertex where

newtype TexCoord = TexCoord Cglm.Vec2
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

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

type WViewProj = StrG.W ViewProjection

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
		ubo.proj * ubo.view * rectModel * (
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
