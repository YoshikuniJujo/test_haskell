{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Types
import Gpu.Vulkan.Object.Base qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM hiding (check)
import Control.Exception
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Maybe
import Data.List qualified as L
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*), pattern (:*.), pattern (:**))
import Data.Word
import Data.ByteString qualified as BS

import qualified Data.Vector.Storable as V

import Language.SpirV qualified as SpirV
import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Instance as Vk.Ist
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.QueueFamily as Vk.QF
import qualified Gpu.Vulkan.Device as Vk.Dv
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPl
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.PplLyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DS
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscStLyt

import qualified Gpu.Vulkan.BufferView as Vk.BffrVw
import qualified Gpu.Vulkan.PushConstant as Vk.PC

import Gpu.Vulkan.TypeEnum qualified as Vk.T

import Codec.Picture qualified as P
import System.Environment

import qualified Gpu.Vulkan.Ext.DebugUtils as Vk.DbgUtls
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger as Vk.DbgUtls.Msngr

import Data.Text.IO qualified as Txt

import Data.Map qualified as M

import System.IO

main :: IO ()
main = do
	ifp : _ <- getArgs
	ic@(inp, cnt) <- (,) <$> atomically newTChan <*> atomically newTChan
	_ <- forkIO do
		realMain ifp "autogen/nega_result.png" ic
		atomically $ writeTChan cnt False
	fix \rec -> do
		putStr "> " >> hFlush stdout
		atomically . writeTChan inp =<< getLine
		(`when` rec) =<< atomically (readTChan cnt)

realMain :: FilePath -> FilePath -> InputContinue -> IO ()
realMain ifp outfp ic = withDvc \pd dv q cb ->
	crDscStLyt dv \dsl -> crPpls dv dsl \pp pp2 ->
	crDscSt dv dsl \ds -> let dvs = (pd, dv, q, cb, ds) in
	withGroups dv \grps ->
	mkPxls ic dvs pp grps initialName ifp >>= \szm@(sz, _) ->
	atomically (newTVar $ M.singleton initialName sz) >>= \szs ->
	mainloop outfp ic dvs pp pp2 grps szs szm nega

type InputContinue = (TChan String, TChan Bool)

initialName :: String
initialName = "initial"

withDvc :: (forall sd scb .
	Vk.Phd.P -> Vk.Dv.D sd -> Vk.Q.Q -> Vk.CmdBuf.C scb -> IO a) -> IO a
withDvc a = Vk.Ist.create @_ @'Nothing iinfo nil \ist -> dbgMssngr ist do
	pd <- head' <$> Vk.Phd.enumerate ist
	qf <- findQFam pd Vk.Q.ComputeBit
	Vk.Dv.create pd (dinfo qf) nil \dv ->
		Vk.Dv.getQueue dv qf 0 >>= \q -> crCmdBffr dv qf $ a pd dv q
	where
	iinfo = Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.J debugMessengerCreateInfo,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo = Nothing,
		Vk.Ist.createInfoEnabledLayerNames =
			[Vk.layerKhronosValidation],
		Vk.Ist.createInfoEnabledExtensionNames =
			[Vk.DbgUtls.extensionName] }
	dinfo qf = Vk.Dv.CreateInfo {
		Vk.Dv.createInfoNext = TMaybe.N,
		Vk.Dv.createInfoFlags = zeroBits,
		Vk.Dv.createInfoQueueCreateInfos = HPList.Singleton $ qinfo qf,
		Vk.Dv.createInfoEnabledLayerNames = [Vk.layerKhronosValidation],
		Vk.Dv.createInfoEnabledExtensionNames = [],
		Vk.Dv.createInfoEnabledFeatures = Nothing }
	qinfo qf = Vk.Dv.QueueCreateInfo {
		Vk.Dv.queueCreateInfoNext = TMaybe.N,
		Vk.Dv.queueCreateInfoFlags = zeroBits,
		Vk.Dv.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dv.queueCreateInfoQueuePriorities = [0] }
	dbgMssngr i = Vk.DbgUtls.Msngr.create i debugMessengerCreateInfo nil

debugMessengerCreateInfo :: Vk.DbgUtls.Msngr.CreateInfo 'Nothing '[] ()
debugMessengerCreateInfo = Vk.DbgUtls.Msngr.CreateInfo {
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
	Vk.DbgUtls.Msngr.createInfoFnUserCallback = debugCallback,
	Vk.DbgUtls.Msngr.createInfoUserData = Nothing }

debugCallback :: Vk.DbgUtls.Msngr.FnCallback '[] ()
debugCallback _svr _tp cbdt _ud = False <$ Txt.putStrLn
	("validation layer: " <> Vk.DbgUtls.Msngr.callbackDataMessage cbdt)

findQFam :: Vk.Phd.P -> Vk.Q.FlagBits -> IO Vk.QF.Index
findQFam pd qf =
	fst . head' . filter (checkBits qf . Vk.QF.propertiesQueueFlags . snd)
		<$> Vk.Phd.getQueueFamilyProperties pd

crCmdBffr :: forall sd a . Vk.Dv.D sd ->
	Vk.QF.Index -> (forall scb . Vk.CmdBuf.C scb -> IO a) -> IO a
crCmdBffr dv qf a = Vk.CmdPl.create dv cpinfo nil \cp ->
	Vk.CmdBuf.allocate dv (cbInfo cp) \(cb :*. HPList.Nil) -> a cb where
	cpinfo = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = qf }
	cbInfo :: Vk.CmdPl.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
	cbInfo cp = Vk.CmdBuf.AllocateInfo {
		Vk.CmdBuf.allocateInfoNext = TMaybe.N,
		Vk.CmdBuf.allocateInfoCommandPool = cp,
		Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

crDscStLyt :: Vk.Dv.D sd -> (forall s .
	Vk.DscStLyt.D s '[
		'Vk.DscStLyt.BufferView '[ '("", Pixel)],
		'Vk.DscStLyt.BufferView '[ '("", PixelFloat)] ] -> IO a) -> IO a
crDscStLyt dv = Vk.DscStLyt.create dv dslinfo TPMaybe.N where
	dslinfo = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = bdng :** bdng :** HPList.Nil }
	bdng = Vk.DscStLyt.BindingBufferView {
		Vk.DscStLyt.bindingBufferViewDescriptorType =
			Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DscStLyt.bindingBufferViewStageFlags =
			Vk.ShaderStageComputeBit }

crPpls :: forall sd sl bts a .
	Vk.Dv.D sd -> Vk.DscStLyt.D sl bts -> (forall sp1 spl1 sp2 spl2 .
		PlytPpl sp1 spl1 '(sl, bts) '[Word32] ->
		PlytPpl sp2 spl2 '(sl, bts) PushConstants -> IO a) -> IO a
crPpls dv dsl a =
	Vk.PplLyt.create dv plinfo nil \pl ->
	Vk.Ppl.Cmpt.createCs dv Nothing
		(HPList.Singleton . U4 $ pinfo glslComputeShaderMain pl) nil
		\(HPList.Singleton p) ->
	Vk.PplLyt.create dv plinfo nil \pl2 ->
	Vk.Ppl.Cmpt.createCs dv Nothing
		(HPList.Singleton . U4 $ pinfo glslComputeShaderMain2 pl2) nil
		\(HPList.Singleton p2) -> a (pl, p) (pl2, p2)
	where
	plinfo :: Vk.PplLyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PC.Layout pcs '[ 'Vk.PC.Range
			'[ 'Vk.T.ShaderStageComputeBit] pcs])
	plinfo = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }
	pinfo sdr pl = Vk.Ppl.Cmpt.CreateInfo {
		Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
		Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
		Vk.Ppl.Cmpt.createInfoStage = U5 $ sinfo sdr,
		Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
		Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }
	sinfo :: SpirV.S 'GlslComputeShader -> Vk.Ppl.ShaderSt.CreateInfo
		'Nothing 'Nothing 'GlslComputeShader 'Nothing '[Word32, Word32]
	sinfo sdr = Vk.Ppl.ShaderSt.CreateInfo {
		Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
		Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
		Vk.Ppl.ShaderSt.createInfoModule = (mdinfo sdr, nil),
		Vk.Ppl.ShaderSt.createInfoName = "main",
		Vk.Ppl.ShaderSt.createInfoSpecializationInfo =
			Just $ 3 :* 10 :* HPList.Nil }
	mdinfo sdr = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = sdr }

type PlytPpl sp spl sdlbts pcs =
	(Vk.PplLyt.P spl '[sdlbts] pcs, Vk.Ppl.Cmpt.C sp '(spl, '[sdlbts], pcs))

type PushConstants = '[
	Word32, CFloat, CFloat, CFloat, CFloat, CFloat, CFloat, CFloat, CFloat ]

crDscSt :: forall bts sd sl a .
	Default (HPList.PL (HPList.PL KObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) =>
	Vk.Dv.D sd -> Vk.DscStLyt.D sl bts ->
	(forall sds .  Vk.DS.D sds '(sl, bts) -> IO a) -> IO a
crDscSt dv dsl a = Vk.DscPl.create dv pinfo nil \pl ->
	Vk.DS.allocateDs dv (sinfo pl) \(HPList.Singleton ds) -> a ds where
	pinfo = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = 1,
		Vk.DscPl.createInfoPoolSizes = [psize] }
	psize = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DscPl.sizeDescriptorCount = 2 }
	sinfo :: Vk.DscPl.P sp -> Vk.DS.AllocateInfo 'Nothing sp '[ '(sl, bts)]
	sinfo pl = Vk.DS.AllocateInfo {
		Vk.DS.allocateInfoNext = TMaybe.N,
		Vk.DS.allocateInfoDescriptorPool = pl,
		Vk.DS.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }

withGroups :: Vk.Dv.D sd ->
	(forall sm sb bnm sbp sbpf . Groups sd k sm sb bnm sbp sbpf -> IO a) ->
	IO a
withGroups dv a =
	Vk.Bffr.group dv nil \bgrp -> Vk.Mm.group dv nil \mgrp ->
	Vk.BffrVw.group dv nil \pgrp -> Vk.BffrVw.group dv nil \pfgrp ->
	a (mgrp, bgrp, pgrp, pfgrp)

type Groups sd k sm sb bnm sbp sbpf = (
	Vk.Mm.Group sd 'Nothing sm k '[
		'(sb, Vk.Mm.BufferArg bnm '[PixelList, PixelFloatList]) ],
	Vk.Bffr.Group sd 'Nothing sb k bnm '[PixelList, PixelFloatList],
	Vk.BffrVw.Group 'Nothing sbp k "" Pixel,
	Vk.BffrVw.Group 'Nothing sbpf k "" PixelFloat )

mkPxls :: (
	Ord k,
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0,
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", PixelFloat)] 0,
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ ['[], '[]] ) =>
	InputContinue -> Devices sd sc sds '(sl, bts) ->
	PlytPpl sp spl '(sl, bts) '[Word32] ->
	Groups sd k sm sb bnm sbp sbpf -> k -> FilePath ->
	IO (Size, Memory sm sb bnm)
mkPxls ic dvs pp grps nm fp = openPxls dvs pp grps nm =<< readPixels ic fp

type Devices sd sc sds sdlbts =
	(Vk.Phd.P, Vk.Dv.D sd, Vk.Q.Q, Vk.CmdBuf.C sc, Vk.DS.D sds sdlbts)

type Size = ((Int, Int), (Word32, Word32))

type Memory sm sb nm =
	Vk.Mm.M sm '[ '( sb, 'Vk.Mm.BufferArg nm '[PixelList, PixelFloatList])]

type PixelList = VObj.List 256 Pixel ""
type PixelFloatList = VObj.List 256 PixelFloat ""

openPxls :: (
	Ord k,
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[], '[]],
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0,
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", PixelFloat)] 0 ) =>
	Devices sd sc sds '(sl, bts) -> PlytPpl sp spl '(sl, bts) '[Word32] ->
	Groups sd k sm sb nm sbp sbpf -> k -> Pixels ->
	IO (Size, Memory sm sb nm)
openPxls (pd, dv, q, cb, ds) (pl, cpl) grps k
	(sz@(fromIntegral -> w, fromIntegral -> h), v) =
	crBffrMmUpdate pd dv ds grps k v >>= \m ->
	((sz, (w, h)), m) <$ loadPxls q cb pl cpl ds w h

type Pixels = ((Int, Int), V.Vector Pixel)

crBffrMmUpdate :: (
	Ord k,
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", PixelFloat)] 0,
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0 ) =>
	Vk.Phd.P -> Vk.Dv.D sd -> Vk.DS.D sds '(sl, bts) ->
	Groups sd k sm sb nm sbp sbpf -> k -> V.Vector Pixel ->
	IO (Memory sm sb nm)
crBffrMmUpdate pd dv ds (mgrp, bgrp, pgrp, pfgrp) k v =
	crBffrMm pd dv mgrp bgrp k v >>= \(bd, m) ->
	Vk.BffrVw.create' dv pgrp k (bvinfo bd) >>= \(Right bv) ->
	Vk.BffrVw.create' dv pfgrp k (bvinfo bd) >>= \(Right bv2) ->
	m <$ update dv ds bv bv2
	where bvinfo bd = Vk.BffrVw.CreateInfo {
		Vk.BffrVw.createInfoNext = TMaybe.N,
		Vk.BffrVw.createInfoFlags = zeroBits,
		Vk.BffrVw.createInfoBuffer = U4 bd }

crBffrMm :: forall sd sm sb bnm k . Ord k =>
	Vk.Phd.P -> Vk.Dv.D sd ->
	Vk.Mm.Group sd 'Nothing sm k '[
		'(sb, Vk.Mm.BufferArg bnm '[PixelList, PixelFloatList])] ->
	Vk.Bffr.Group sd 'Nothing sb k bnm '[PixelList, PixelFloatList] ->
	k -> V.Vector Pixel -> IO (
		Vk.Bffr.Binded sm sb bnm '[PixelList, PixelFloatList],
		Memory sm sb bnm )
crBffrMm pd dv mgrp bgrp k v =
	Vk.Bffr.create' bgrp k binfo >>= \(Right bf) -> minfo bf >>= \mi ->
	Vk.Mm.allocateBind'
		mgrp k (HPList.Singleton (U2 (Vk.Mm.Buffer bf))) mi >>=
		\(	AlwaysRight
				(HPList.Singleton (U2 (Vk.Mm.BufferBinded bd)),
			m) ) ->
	(bd, m) <$ Vk.Mm.write @bnm @PixelList @0 dv m zeroBits v
	where
	binfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths =
			VObj.LengthList (fromIntegral $ V.length v) :**
			VObj.LengthList (fromIntegral $ V.length v) :**
			HPList.Nil,
		Vk.Bffr.createInfoUsage =
			Vk.Bffr.UsageStorageBufferBit .|.
			Vk.Bffr.UsageStorageTexelBufferBit,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	minfo b = do
		rq <- Vk.Bffr.getMemoryRequirements dv b
		mt <- findMmTpIdx pd rq (
			Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
		pure Vk.Mm.AllocateInfo {
			Vk.Mm.allocateInfoNext = TMaybe.N,
			Vk.Mm.allocateInfoMemoryTypeIndex = mt }

findMmTpIdx :: Vk.Phd.P ->
	Vk.Mm.Requirements -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmTpIdx pd rq wt = Vk.Phd.getMemoryProperties pd >>= \prs -> do
	let	rqts = Vk.Mm.requirementsMemoryTypeBits rq
		wtts = (fst <$>)
			. filter (checkBits wt . Vk.Mm.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes prs
	case filter (`Vk.Mm.elemTypeIndex` rqts) wtts of
		[] -> error "No available memory types"; i : _ -> pure i

update :: forall sd sds sl bts sb sb2 . (
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", PixelFloat)] 0,
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0 ) =>
	Vk.Dv.D sd -> Vk.DS.D sds '(sl, bts) ->
	Vk.BffrVw.B sb "" Pixel -> Vk.BffrVw.B sb2 "" PixelFloat -> IO ()
update dv ds bv bv2 =
	Vk.DS.updateDs dv (U5 wr :** U5 wr2 :** HPList.Nil) HPList.Nil where
	wr :: Vk.DS.Write 'Nothing sds '(sl, bts)
		(Vk.DS.WriteSourcesArgBufferView '[ '(sb, "", Pixel)]) 0
	wr = Vk.DS.Write {
		Vk.DS.writeNext = TMaybe.N, Vk.DS.writeDstSet = ds,
		Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DS.writeSources =
			Vk.DS.TexelBufferViews . HPList.Singleton $ U3 bv }
	wr2 :: Vk.DS.Write 'Nothing sds '(sl, bts)
		(Vk.DS.WriteSourcesArgBufferView '[ '(sb2, "", PixelFloat)]) 0
	wr2 = Vk.DS.Write {
		Vk.DS.writeNext = TMaybe.N, Vk.DS.writeDstSet = ds,
		Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DS.writeSources =
			Vk.DS.TexelBufferViews . HPList.Singleton $ U3 bv2 }

loadPxls :: forall slbts sc sg sl sds . (
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts) ~
		'[ '[], '[]] ) =>
	Vk.Q.Q -> Vk.CmdBuf.C sc -> Vk.PplLyt.P sl '[slbts] '[Word32] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[Word32]) -> Vk.DS.D sds slbts ->
	Word32 -> Word32 -> IO ()
loadPxls q cb pl cpl ds w h = do
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def
		$ Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute cpl \ccb ->
			Vk.Cmd.bindDescriptorSetsCompute ccb pl
				(HPList.Singleton $ U2 ds)
				(HPList.Singleton $
					HPList.Nil :** HPList.Nil :**
					HPList.Nil) >>
			Vk.Cmd.pushConstantsCompute
				@'[ 'Vk.T.ShaderStageComputeBit]
				ccb pl (HPList.Singleton (HPList.Id w)) >>
			Vk.Cmd.dispatch ccb w h 1
	Vk.Q.submit q (HPList.Singleton $ U4 sinfo) Nothing
	Vk.Q.waitIdle q
	where sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

readPixels :: InputContinue -> FilePath -> IO Pixels
readPixels ic@(inp, cnt) fp = readPngRgba fp >>= \case
	Right img -> pure . ($ img) $
		(P.imageWidth &&& P.imageHeight) &&& V.unsafeCast . P.imageData
	Left e -> do
		putStrLn $ "readPixels: error " ++ show e
		putStr "Please input PNG (RGBA8) file path" >> hFlush stdout
		readPixels ic
			=<< atomically (readTChan inp <* writeTChan cnt True)

readPngRgba ::
	FilePath -> IO (Either (Either IOError String) (P.Image P.PixelRGBA8))
readPngRgba fp = try (BS.readFile fp) >>= \case
	Left e -> pure . Left $ Left e
	Right bs -> pure case P.decodePng bs of
		Left em -> Left $ Right em
		Right (P.ImageRGBA8 img) -> Right img
		Right _ -> Left $ Right "readPngRgba: The format is not RGBA8"

mainloop :: forall sd sc sds sp1 spl1 sp2 spl2 sl bts sm sb bnm sbp sbpf oss . (
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[], '[]],
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0,
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", PixelFloat)] 0,
	Vk.Mm.OffsetSize bnm (VObj.List 256 Pixel "") oss 0 ) =>
	FilePath -> InputContinue -> Devices sd sc sds '(sl, bts) ->
	PlytPpl sp1 spl1 '(sl, bts) '[Word32] ->
	PlytPpl sp2 spl2 '(sl, bts) PushConstants ->
	Groups sd String sm sb bnm sbp sbpf -> TVar (M.Map String Size) ->
	(Size, Vk.Mm.M sm oss) -> Constants -> IO ()
mainloop outf ic@(inp, cnt) dvs@(_, dv, q, cb, ds) pp pp2@(pl2, p2)
	gs@(mgrp, _, pgrp, pfgrp) szs szm@((sz, (w, h)), m) cs = do
	rslt <- (sz ,) <$> convertPxls @oss @bnm dv q cb pl2 p2 ds m w h cs
	writePixels outf rslt
	fix \go -> do
		cmd <- atomically $ readTChan inp
		case (words cmd, nameConstant cmd) of
			(["quit"], _) -> pure ()
			(["open", nm, fp], _) -> next >>
				mkPxls ic dvs pp gs nm fp >>= \szm'@(sz', _) ->
				putSize nm sz' >>
				mainloop outf ic dvs pp pp2 gs szs szm' cs
			(["select", nm], _) -> next >> do
				Just bp <- Vk.BffrVw.lookup pgrp nm
				Just bpf <- Vk.BffrVw.lookup pfgrp nm
				Just sz' <- lookupSize nm
				Just m' <- Vk.Mm.lookup mgrp nm
				update dv ds bp bpf
				mainloop outf ic dvs pp pp2 gs szs (sz', m') cs
			(_, Just c) ->
				next >> mainloop outf ic dvs pp pp2 gs szs szm c
			(_, Nothing) ->
				putStrLn "No such commands" >> next >> go
	where
	next = atomically $ writeTChan cnt True
	putSize n = atomically . modifyTVar szs . M.insert n
	lookupSize nm = M.lookup nm <$> atomically (readTVar szs)

convertPxls :: forall {sd} {sc} {sp} {spl} {sds} {sl} {bts} {sm} oss bnm pxl . (
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[], '[]],
	Storable pxl,
	Vk.Mm.OffsetSize bnm (VObj.List 256 pxl "") oss 0 ) =>
	Vk.Dv.D sd -> Vk.Q.Q -> Vk.CmdBuf.C sc ->
	Vk.PplLyt.P spl '[ '(sl, bts)] PushConstants ->
	Vk.Ppl.Cmpt.C sp '(spl, '[ '(sl,bts)], PushConstants) ->
	Vk.DS.D sds '(sl,bts) ->
	Vk.Mm.M sm oss -> Word32 -> Word32 -> Constants -> IO (V.Vector pxl)
convertPxls dv q cb plyt2 ppl2 ds m w h cs = do
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl2 \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb plyt2
				(U2 ds :** HPList.Nil)
				(HPList.Singleton $ HPList.Nil :** HPList.Nil :** HPList.Nil)
			Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit ]
				ccb plyt2 ((w :: Word32) :* cs)
			Vk.Cmd.dispatch ccb w h 1
	Vk.Q.submit q (U4 submitInfo2 :** HPList.Nil) Nothing
	Vk.Q.waitIdle q
	Vk.Mm.read @bnm @(VObj.List 256 pxl "") @0 @(V.Vector pxl) dv m def
	where
	submitInfo2 :: Vk.SubmitInfo 'Nothing '[] '[sc] '[]
	submitInfo2 = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
			Vk.submitInfoCommandBuffers = cb :** HPList.Nil,
			Vk.submitInfoSignalSemaphores = HPList.Nil }

writePixels :: FilePath -> Pixels -> IO ()
writePixels fp ((w, h), pxs) =
	P.writePng @P.PixelRGBA8 fp . P.Image w h $ V.unsafeCast pxs

nameConstant :: String -> Maybe Constants
nameConstant = \case
	"posi" -> Just posi; "nega" -> Just nega;
	"red" -> Just red; "green" -> Just green; "blue" -> Just blue
	_ -> Nothing

type Constants =
	HPList.L '[CFloat, CFloat, CFloat, CFloat, CFloat, CFloat, CFloat, CFloat]

nega, posi :: Constants
nega = mone :* one :* mone :* one :* mone :* one :* one :* zero :* HPList.Nil
posi = one :* zero :* one :* zero :* one :* zero :* one :* zero :* HPList.Nil

red :: Constants
red = one :* zero :* zero :* zero :* zero :* zero :* one :* zero :* HPList.Nil

green :: Constants
green = zero :* zero :* one :* zero :* zero :* zero :* one :* zero :* HPList.Nil

blue :: Constants
blue = zero :* zero :* zero :* zero :* one :* zero :* one :* zero :* HPList.Nil

{-# COMPLETE AlwaysRight #-}

pattern AlwaysRight :: b -> Either a b
pattern AlwaysRight x <- Right x where AlwaysRight x = Right x

zero, one, mone :: CFloat
zero = 0; one = 1; mone = - 1

data Pixel = Pixel Word8 Word8 Word8 Word8 deriving Show

type instance Vk.BffrVw.FormatOf Pixel = 'Vk.T.FormatR8g8b8a8Uint

instance Storable Pixel where
	sizeOf _ = 4 * sizeOf @Word8 undefined
	alignment _ = alignment @Word8 undefined
	peek p = peekArray 4 (castPtr p) >>= \case
		[r, g, b, a] -> pure (Pixel r g b a); _ -> error "never occur"
	poke p (Pixel r g b a) = pokeArray (castPtr p) [r, g, b, a]

data PixelFloat = PixelFloat CFloat CFloat CFloat CFloat deriving Show

type instance Vk.BffrVw.FormatOf PixelFloat = 'Vk.T.FormatR32g32b32a32Sfloat

instance Storable PixelFloat where
	sizeOf _ = 4 * sizeOf @CFloat undefined
	alignment _ = alignment @CFloat undefined
	peek p = peekArray 4 (castPtr p) >>= \case
		[r, g, b, a] -> pure (PixelFloat r g b a); _ -> error "never occur"
	poke p (PixelFloat r g b a) = pokeArray (castPtr p) [r, g, b, a]

head' :: [a] -> a
head' = fst . fromJust . L.uncons

glslComputeShaderMain :: SpirV.S 'GlslComputeShader
glslComputeShaderMain = [glslComputeShader|

#version 460
layout(local_size_x = 1, local_size_y = 1) in;

layout(binding = 0, rgba8ui) uniform uimageBuffer storageTexelBuffer;
layout(binding = 1, rgba32f) uniform imageBuffer storageTexelBuffer2;
layout(push_constant) uniform PushConstant { uint width; } pushConstant;

void
main()
{
	int index = int(gl_GlobalInvocationID.x +
		gl_GlobalInvocationID.y * pushConstant.width);

	uvec4 px = imageLoad(storageTexelBuffer, index);
	vec4 px2;
	px2.r = float(px.r) / 255;
	px2.g = float(px.g) / 255;
	px2.b = float(px.b) / 255;
	px2.a = float(px.a) / 255;
	imageStore(storageTexelBuffer2, index, px2);
}

|]

glslComputeShaderMain2 :: SpirV.S 'GlslComputeShader
glslComputeShaderMain2 = [glslComputeShader|

#version 460
layout(local_size_x = 1, local_size_y = 1) in;

layout(binding = 0, rgba8ui) uniform uimageBuffer storageTexelBuffer;
layout(binding = 1, rgba32f) uniform imageBuffer storageTexelBuffer2;
layout(push_constant) uniform PushConstant {
	uint width;
	float kr; float cr;
	float kg; float cg;
	float kb; float cb;
	float ka; float ca;
	} pushConstant;

void
main()
{
	int index = int(gl_GlobalInvocationID.x +
		gl_GlobalInvocationID.y * pushConstant.width);

	vec4 px2 = imageLoad(storageTexelBuffer2, index);
//	px2.r = 0; px2.g = px2.g; px2.b = 0;
//	px2.r = 1 - px2.r; px2.g = 1 - px2.g; px2.b = 1 - px2.b;
	px2.r = pushConstant.kr * px2.r + pushConstant.cr;
	px2.g = pushConstant.kg * px2.g + pushConstant.cg;
	px2.b = pushConstant.kb * px2.b + pushConstant.cb;
	px2.a = pushConstant.ka * px2.a + pushConstant.ca;

	uvec4 px;
	px.r = uint(px2.r * 255);
	px.g = uint(px2.g * 255);
	px.b = uint(px2.b * 255);
	px.a = uint(px2.a * 255);
	imageStore(storageTexelBuffer, index, px);
}

|]
