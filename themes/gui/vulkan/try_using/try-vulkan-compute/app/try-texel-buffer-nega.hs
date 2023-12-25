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
import Control.Monad.Fix
import Control.Concurrent
import Control.Concurrent.STM hiding (check)
import Control.Exception
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.List
import Data.HeteroParList qualified as HL
import Data.HeteroParList (pattern (:*), pattern (:*.), pattern (:**))
import Data.Word
import Data.ByteString qualified as BS

import qualified Data.Vector.Storable as V

import Language.SpirV qualified as SpirV
import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Ist
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QF
import qualified Gpu.Vulkan.Device as Vk.Dv
import qualified Gpu.Vulkan.CommandPool as Vk.CommandPool
import qualified Gpu.Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified Gpu.Vulkan.Buffer.Enum as Vk.Bff
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mm
import qualified Gpu.Vulkan.Descriptor.Enum as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DP
import qualified Gpu.Vulkan.DescriptorPool.Enum as Vk.DP
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DS
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified Gpu.Vulkan.CommandBuffer.Enum as Vk.CmdBuf
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bff
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DSLyt

import qualified Gpu.Vulkan.BufferView as Vk.BffVw
import qualified Gpu.Vulkan.PushConstant as Vk.PC

import Gpu.Vulkan.TypeEnum qualified as Vk.T

import Codec.Picture qualified as P
import System.Environment

import qualified Gpu.Vulkan.Ext.DebugUtils as Vk.Ext.DbgUtls
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger as Vk.Ext.DbgUtls.Msngr
import qualified Gpu.Vulkan.Ext.DebugUtils.Enum as Vk.Ext.DbgUtls

import Data.Text.IO qualified as Txt

import Data.Map qualified as M

main :: IO ()
main = do
	io@(inp, outp) <- (,) <$> atomically newTChan <*> atomically newTChan
	inf : _ <- getArgs
	_ <- forkIO do
		makeNega inf "autogen/nega_result.png" io
		atomically $ writeTChan outp False

	fix \rec -> do
		atomically . writeTChan inp =<< getLine
		b <- atomically $ readTChan outp
		if b then rec else pure ()

type InOut = (TChan String, TChan Bool)
type Size = ((Int, Int), (Word32, Word32))
type PplPlyt sp spl sdlbts pcs = (
	Vk.Ppl.Cmpt.C sp '(spl, '[sdlbts], pcs),
	Vk.Ppl.Lyt.P spl '[sdlbts] pcs )

makeNega :: FilePath -> FilePath -> InOut -> IO ()
makeNega inf outf io =
	device \phd qf dv -> dscSetLayout dv \dslyt ->
	descriptorSet dv dslyt \ds -> groups dv \grps ->
	pipeline dv qf dslyt \cb pplplyt pplplyt2 ->
	let dvs = (phd, dv, qf, cb, ds) in

	readPixels io inf >>= \pxs ->
	openPixels dvs pplplyt grps ("initial" :: String) pxs >>=
		\(szwhm :: (Size, Memory sm sb nm)) ->

	atomically (newTVar . M.singleton "initial" $ fst szwhm) >>= \szwhs ->

	mainLoop @nm dvs pplplyt pplplyt2 io grps szwhs outf szwhm nega

openPixels :: (
	'(sl, bts) ~ slbts,
	Vk.DSLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[], '[]],
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0,
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", PixelFloat)] 0,
	Ord k ) => Devices sd sc sds slbts ->
		PplPlyt sg sl1 slbts '[Word32] ->
	Groups k sm sd sb nm sbp sbpf -> k -> Pixels ->
	IO (Size, Memory sm sb nm)
openPixels (phd, dv, qf, cb, ds) (ppl, plyt) grps k pxs =
	pure pxs >>= \(sz@(fromIntegral -> w, fromIntegral -> h), v) ->
	buffer phd dv ds grps k v >>= \(m :: Memory sm sb nm) ->
	run1 dv qf cb ppl plyt ds w h >> pure ((sz, (w, h)), m)

type Devices sd sc sds sdlbts =
	(Vk.Phd.P, Vk.Dv.D sd, Vk.QF.Index, Vk.CmdBuf.C sc, Vk.DS.D sds sdlbts)

mainLoop :: forall nm4 objss4 slbts sl bts sbtss sd sc sg1 sl1 sg2 sl2 sm4 sds sb sbp sbpf . (
	Vk.DSLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts) ~ '[ '[], '[]],
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0,
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", PixelFloat)] 0,
	sbtss ~ '[slbts],
	slbts ~ '(sl, bts),
	Vk.Mm.OffsetSize nm4 (VObj.List 256 Pixel "") objss4 ) =>
	Devices sd sc sds slbts ->
	PplPlyt sg1 sl1 slbts '[Word32] ->
	PplPlyt sg2 sl2 slbts PushConstants ->
	InOut -> Groups String sm4 sd sb nm4 sbp sbpf ->
	TVar (M.Map String Size) ->
	FilePath -> (Size, Vk.Mm.M sm4 objss4) -> Constants -> IO ()
mainLoop dvs@(_, dv, qf, cb, ds) pplplyt pplplyt2@(ppl2, plyt2) io@(inp, outp)
	grps@(_, mgrp, pgrp, pfgrp) szwhs outf szwhm@((sz, (w, h)), m) cs = do
	rslt <- (sz ,) <$> run2 @nm4 @_ @objss4 dv qf cb ppl2 plyt2 ds m w h cs
	writePixels outf rslt
	fix \rec -> do
		cmd <- atomically $ readTChan inp
		case (words cmd, nameConstant cmd) of
			(["quit"], _) -> pure ()
			(["open", nm, fp], _) ->
				atomically (writeTChan outp True) >>
				readPixels io fp >>= \pxs ->
				openPixels dvs pplplyt grps nm pxs >>=
					\(szwhm' :: (Size, Memory sm sb nm)) ->
				atomically (modifyTVar szwhs . M.insert nm $ fst szwhm') >>
				mainLoop @nm dvs pplplyt pplplyt2 io grps szwhs outf szwhm' cs
			(["select", nm], _) -> do
				atomically $ writeTChan outp True
				Just m' <- Vk.Mm.lookup mgrp nm
				Just bp <- Vk.BffVw.lookup pgrp nm
				Just bpf <- Vk.BffVw.lookup pfgrp nm
				Just szwh <- M.lookup nm <$> atomically (readTVar szwhs)
				update dv ds bp bpf
				mainLoop @nm4 dvs pplplyt pplplyt2 io grps szwhs outf (szwh, m') cs
			(_, Just c) -> do
				atomically $ writeTChan outp True
				mainLoop @nm4 @objss4 dvs pplplyt pplplyt2 io grps szwhs outf szwhm c
			(_, Nothing) -> do
				atomically $ writeTChan outp True
				rec

nameConstant :: String -> Maybe Constants
nameConstant = \case
	"posi" -> Just posi; "nega" -> Just nega;
	"red" -> Just red; "green" -> Just green; "blue" -> Just blue
	_ -> Nothing

type Constants =
	HL.L '[CFloat, CFloat, CFloat, CFloat, CFloat, CFloat, CFloat, CFloat]

nega, posi :: Constants
nega = mone :* one :* mone :* one :* mone :* one :* one :* zero :* HL.Nil
posi = one :* zero :* one :* zero :* one :* zero :* one :* zero :* HL.Nil

red :: Constants
red = one :* zero :* zero :* zero :* zero :* zero :* one :* zero :* HL.Nil

green :: Constants
green = zero :* zero :* one :* zero :* zero :* zero :* one :* zero :* HL.Nil

blue :: Constants
blue = zero :* zero :* zero :* zero :* one :* zero :* one :* zero :* HL.Nil

type Memory sm sb nm = Vk.Mm.M sm
	'[ '( sb, 'Vk.Mm.BufferArg nm '[PixelList, PixelFloatList])]
type PixelList = VObj.List 256 Pixel ""

type PixelFloatList = VObj.List 256 PixelFloat ""

device :: (forall sd . Vk.Phd.P -> Vk.QF.Index -> Vk.Dv.D sd -> IO a) -> IO a
device f = Vk.Ist.create @_ @'Nothing instInfo nil \ist -> setupDebugMessenger ist do
	phd <- head <$> Vk.Phd.enumerate ist
	qf <- findQueueFamily phd Vk.Queue.ComputeBit
	Vk.Dv.create phd (dvcInfo qf) nil \dv -> f phd qf dv
	where
	instInfo :: Vk.Ist.CreateInfo
		('Just (Vk.Ext.DbgUtls.Msngr.CreateInfo 'Nothing '[] ()))
		'Nothing
	instInfo = Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.J debugMessengerCreateInfo,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo = Nothing,
		Vk.Ist.createInfoEnabledLayerNames =
			[Vk.layerKhronosValidation],
		Vk.Ist.createInfoEnabledExtensionNames =
			[Vk.Ext.DbgUtls.extensionName] }
	dvcInfo qf = Vk.Dv.CreateInfo {
		Vk.Dv.createInfoNext = TMaybe.N,
		Vk.Dv.createInfoFlags = zeroBits,
		Vk.Dv.createInfoQueueCreateInfos = HL.Singleton $ queueInfo qf,
		Vk.Dv.createInfoEnabledLayerNames = [Vk.layerKhronosValidation],
		Vk.Dv.createInfoEnabledExtensionNames = [],
		Vk.Dv.createInfoEnabledFeatures = Nothing }
	queueInfo qf = Vk.Dv.QueueCreateInfo {
		Vk.Dv.queueCreateInfoNext = TMaybe.N,
		Vk.Dv.queueCreateInfoFlags = zeroBits,
		Vk.Dv.queueCreateInfoQueueFamilyIndex = qf,
		Vk.Dv.queueCreateInfoQueuePriorities = [0] }

findQueueFamily :: Vk.Phd.P -> Vk.Queue.FlagBits -> IO Vk.QF.Index
findQueueFamily phd qf = fst . head
	. filter ((/= zeroBits) . (.&. qf) . Vk.QF.propertiesQueueFlags . snd)
	<$> Vk.Phd.getQueueFamilyProperties phd

dscSetLayout :: Vk.Dv.D sd -> (forall s .
	Vk.DSLyt.D s '[
		'Vk.DSLyt.BufferView '[ '("", Pixel)],
		'Vk.DSLyt.BufferView '[ '("", PixelFloat)] ] ->
	IO a) -> IO a
dscSetLayout dv = Vk.DSLyt.create dv dscSetLayoutInfo TPMaybe.N
	where
	dscSetLayoutInfo = Vk.DSLyt.CreateInfo {
		Vk.DSLyt.createInfoNext = TMaybe.N,
		Vk.DSLyt.createInfoFlags = zeroBits,
		Vk.DSLyt.createInfoBindings = bdng :** bdng :** HL.Nil }
	bdng = Vk.DSLyt.BindingBufferView {
		Vk.DSLyt.bindingBufferViewDescriptorType =
			Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DSLyt.bindingBufferViewStageFlags =
			Vk.ShaderStageComputeBit }

descriptorSet :: forall bts sd sl a .
	Default (HL.PL (HL.PL KObj.Length)
		(Vk.DSLyt.BindingTypeListBufferOnlyDynamics bts)) =>
	Vk.Dv.D sd -> Vk.DSLyt.D sl bts ->
	(forall sds .  Vk.DS.D sds '(sl, bts) -> IO a) -> IO a
descriptorSet dv lyt f =
	Vk.DP.create dv poolInfo nil \pl ->
	Vk.DS.allocateDs dv (setInfo pl)
		\((ds :: Vk.DS.D sds '(sl, bts)) :** HL.Nil) -> f ds
	where
	poolInfo = Vk.DP.CreateInfo {
		Vk.DP.createInfoNext = TMaybe.N,
		Vk.DP.createInfoFlags = Vk.DP.CreateFreeDescriptorSetBit,
		Vk.DP.createInfoMaxSets = 1,
		Vk.DP.createInfoPoolSizes = [poolSize] }
	poolSize = Vk.DP.Size {
		Vk.DP.sizeType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DP.sizeDescriptorCount = 2 }
	setInfo :: Vk.DP.P sp -> Vk.DS.AllocateInfo 'Nothing sp '[ '(sl, bts)]
	setInfo pl = Vk.DS.AllocateInfo {
		Vk.DS.allocateInfoNext = TMaybe.N,
		Vk.DS.allocateInfoDescriptorPool = pl,
		Vk.DS.allocateInfoSetLayouts = U2 lyt :** HL.Nil }

type Groups k smgrp sd sbgrp nm sbp sbpf = (
	Vk.Bff.Group sd 'Nothing sbgrp k nm '[PixelList, PixelFloatList],
	Vk.Mm.Group sd 'Nothing smgrp k '[
		'(sbgrp, Vk.Mm.BufferArg nm '[PixelList, PixelFloatList]) ],
	Vk.BffVw.Group 'Nothing sbp k "" Pixel,
	Vk.BffVw.Group 'Nothing sbpf k "" PixelFloat )

groups :: Vk.Dv.D sd -> (forall smgrp sbgrp nm sbp sbpf .
	Groups k smgrp sd sbgrp nm sbp sbpf -> IO a) -> IO a
groups dv f =
	Vk.Bff.group dv nil \bgrp -> Vk.Mm.group dv nil \mgrp ->
	Vk.BffVw.group dv nil \pgrp -> Vk.BffVw.group dv nil \pfgrp ->
	f (bgrp, mgrp, pgrp, pfgrp)

buffer :: forall bts sd sl nm sds smgrp sbgrp sbp sbpf k . (
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", PixelFloat)] 0,
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0, Ord k ) =>
	Vk.Phd.P -> Vk.Dv.D sd -> Vk.DS.D sds '(sl, bts) ->
	Groups k smgrp sd sbgrp nm sbp sbpf -> k -> V.Vector Pixel ->
	IO (Memory smgrp sbgrp nm)
buffer phd dv ds (bgrp, mgrp, pgrp, pfgrp) k v =
	bufferNew dv phd v bgrp mgrp k >>= \(bd, m) ->
	Vk.BffVw.create' dv pgrp k (bvInfo bd) >>= \(Right bv) ->
	Vk.BffVw.create' dv pfgrp k (bvInfo bd) >>= \(Right bv2) ->
	update dv ds bv bv2 >> pure m
	where
	bvInfo bd = Vk.BffVw.CreateInfo {
		Vk.BffVw.createInfoNext = TMaybe.N,
		Vk.BffVw.createInfoFlags = zeroBits,
		Vk.BffVw.createInfoBuffer = U4 bd }

update :: forall sd sds sl bts sb sb2 . (
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", PixelFloat)] 0,
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0 ) =>
	Vk.Dv.D sd -> Vk.DS.D sds '(sl, bts) ->
	Vk.BffVw.B sb "" Pixel -> Vk.BffVw.B sb2 "" PixelFloat -> IO ()
update dv ds bv bv2 =
	Vk.DS.updateDs dv (U5 write :** U5 write2 :** HL.Nil) HL.Nil
	where
	write :: Vk.DS.Write 'Nothing sds '(sl, bts)
		(Vk.DS.WriteSourcesArgBufferView '[ '(sb, "", Pixel)]) 0
	write = Vk.DS.Write {
		Vk.DS.writeNext = TMaybe.N,
		Vk.DS.writeDstSet = ds,
		Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DS.writeSources =
			Vk.DS.TexelBufferViews . HL.Singleton $ U3 bv }
	write2 :: Vk.DS.Write 'Nothing sds '(sl, bts)
		(Vk.DS.WriteSourcesArgBufferView '[ '(sb2, "", PixelFloat)]) 0
	write2 = Vk.DS.Write {
		Vk.DS.writeNext = TMaybe.N,
		Vk.DS.writeDstSet = ds,
		Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DS.writeSources =
			Vk.DS.TexelBufferViews . HL.Singleton $ U3 bv2 }

{-# COMPLETE AlwaysRight #-}

pattern AlwaysRight :: b -> Either a b
pattern AlwaysRight x <- Right x where AlwaysRight x = Right x

bufferNew :: forall sd sbgrp nm smgrp k . Ord k =>
	Vk.Dv.D sd -> Vk.Phd.P -> V.Vector Pixel ->
	Vk.Bff.Group sd 'Nothing sbgrp k nm '[PixelList, PixelFloatList] ->
	Vk.Mm.Group sd 'Nothing smgrp k '[
		'(sbgrp, Vk.Mm.BufferArg nm '[PixelList, PixelFloatList])] ->
	k -> IO (
		Vk.Bff.Binded smgrp sbgrp nm '[PixelList, PixelFloatList],
		Memory smgrp sbgrp nm )
bufferNew dv phd v bgrp mgrp k =
	Vk.Bff.create' bgrp k bufferInfo >>= \(Right bffr) ->
	memoryInfo bffr >>= \mi ->
	Vk.Mm.allocateBind' mgrp k (U2 (Vk.Mm.Buffer bffr) :** HL.Nil) mi >>=
		\(AlwaysRight (U2 (Vk.Mm.BufferBinded bnd) :** HL.Nil, m)) ->
	Vk.Mm.write @nm @PixelList dv m def v >> pure (bnd, m)
	where
	bufferInfo = Vk.Bff.CreateInfo {
		Vk.Bff.createInfoNext = TMaybe.N,
		Vk.Bff.createInfoFlags = def,
		Vk.Bff.createInfoLengths =
			VObj.LengthList (fromIntegral $ V.length v) :**
			VObj.LengthList (fromIntegral $ V.length v) :**
			HL.Nil,
		Vk.Bff.createInfoUsage =
			Vk.Bff.UsageStorageBufferBit .|.
			Vk.Bff.UsageStorageTexelBufferBit,
		Vk.Bff.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bff.createInfoQueueFamilyIndices = [] }
	memoryInfo :: Vk.Bff.B sb nm objs -> IO (Vk.Mm.AllocateInfo 'Nothing)
	memoryInfo b = do
		rq <- Vk.Bff.getMemoryRequirements dv b
		mt <- findMemoryTypeIndex phd rq (
			Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit )
		pure Vk.Mm.AllocateInfo {
			Vk.Mm.allocateInfoNext = TMaybe.N,
			Vk.Mm.allocateInfoMemoryTypeIndex = mt }

findMemoryTypeIndex ::
	Vk.Phd.P -> Vk.Mm.Requirements -> Vk.Mm.PropertyFlags ->
	IO Vk.Mm.TypeIndex
findMemoryTypeIndex phd rq prp0 = Vk.Phd.getMemoryProperties phd >>= \prps -> do
	let	rqts = Vk.Mm.requirementsMemoryTypeBits rq
		mtps = (fst <$>)
			. filter (check prp0 . Vk.Mm.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes prps
	case filter (`Vk.Mm.elemTypeIndex` rqts) mtps of
		[] -> error "No available memory types"; i : _ -> pure i
	where check = (.) <$> (==) <*> (.&.)

type PushConstants =
	'[Word32, CFloat, CFloat, CFloat, CFloat, CFloat, CFloat, CFloat, CFloat]

pipeline :: forall sd sl bts a .
	Vk.Dv.D sd -> Vk.QF.Index -> Vk.DSLyt.D sl bts -> (forall scb s1 s2 sl1 sl2 .
		Vk.CmdBuf.C scb ->
		PplPlyt s1 sl1 '(sl, bts) '[Word32] ->
		PplPlyt s2 sl2 '(sl, bts) PushConstants -> IO a) -> IO a
pipeline dv qf dslyt f =
	Vk.Ppl.Lyt.create dv plytInfo nil \plyt ->
	Vk.Ppl.Lyt.create dv plytInfo nil \plyt2 ->
	Vk.Ppl.Cmpt.createCs dv Nothing
		(U4 (pplInfo glslComputeShaderMain plyt) :** HL.Nil) nil
		\(ppl :** HL.Nil) ->
	Vk.Ppl.Cmpt.createCs dv Nothing
		(U4 (pplInfo glslComputeShaderMain2 plyt2) :** HL.Nil) nil
		\(ppl2 :** HL.Nil) ->
	Vk.CommandPool.create dv cpoolInfo nil \cp ->
	Vk.CmdBuf.allocate dv (cbInfo cp) \(cb :*. HL.Nil) -> f cb (ppl, plyt) (ppl2, plyt2)
	where
	plytInfo :: Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PC.Layout pcs '[ 'Vk.PC.Range
			'[ 'Vk.T.ShaderStageComputeBit] pcs])
	plytInfo = Vk.Ppl.Lyt.CreateInfo {
		Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
		Vk.Ppl.Lyt.createInfoFlags = zeroBits,
		Vk.Ppl.Lyt.createInfoSetLayouts = U2 dslyt :** HL.Nil }
	pplInfo sdr pl = Vk.Ppl.Cmpt.CreateInfo {
		Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
		Vk.Ppl.Cmpt.createInfoFlags = def,
		Vk.Ppl.Cmpt.createInfoStage = U5 $ shaderInfo sdr,
		Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
		Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }
	shaderInfo :: SpirV.S 'GlslComputeShader -> Vk.Ppl.ShaderSt.CreateInfo
		'Nothing 'Nothing 'GlslComputeShader 'Nothing '[Word32, Word32]
	shaderInfo sdr = Vk.Ppl.ShaderSt.CreateInfo {
		Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
		Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
		Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
		Vk.Ppl.ShaderSt.createInfoModule = (shaderModInfo sdr, nil),
		Vk.Ppl.ShaderSt.createInfoName = "main",
		Vk.Ppl.ShaderSt.createInfoSpecializationInfo =
			Just $ HL.Id 3 :** HL.Id 10 :** HL.Nil }
	shaderModInfo sdr = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = sdr } -- glslComputeShaderMain }
	cpoolInfo = Vk.CommandPool.CreateInfo {
		Vk.CommandPool.createInfoNext = TMaybe.N,
		Vk.CommandPool.createInfoFlags =
			Vk.CommandPool.CreateResetCommandBufferBit,
		Vk.CommandPool.createInfoQueueFamilyIndex = qf }
	cbInfo :: Vk.CommandPool.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
	cbInfo cp = Vk.CmdBuf.AllocateInfo {
		Vk.CmdBuf.allocateInfoNext = TMaybe.N,
		Vk.CmdBuf.allocateInfoCommandPool = cp,
		Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

run1 :: forall slbts sbtss sd sc sg sl sds . (
	Vk.DSLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts) ~ '[ '[], '[]],
	sbtss ~ '[slbts],
	InfixIndex '[slbts] sbtss ) =>
	Vk.Dv.D sd -> Vk.QF.Index -> Vk.CmdBuf.C sc ->
	Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[Word32]) ->
	Vk.Ppl.Lyt.P sl sbtss '[Word32] ->
	Vk.DS.D sds slbts -> Word32 -> Word32 -> IO ()
run1 dv qf cb ppl plyt ds w h = do
	q <- Vk.Dv.getQueue dv qf 0
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb plyt
				(U2 ds :** HL.Nil)
				(HL.Singleton $ HL.Nil :** HL.Nil :** HL.Nil)
			Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit ]
				ccb plyt (HL.Singleton (HL.Id (w :: Word32)))
			Vk.Cmd.dispatch ccb w h 1
	Vk.Queue.submit q (U4 submitInfo :** HL.Nil) Nothing
	Vk.Queue.waitIdle q
	where
	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[sc] '[]
	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Nil,
			Vk.submitInfoCommandBuffers = cb :** HL.Nil,
			Vk.submitInfoSignalSemaphores = HL.Nil }

run2 :: forall nm4 w4 objss4 slbts sbtss sd sc sg2 sl2 sm4 sds . (
	Vk.DSLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts) ~ '[ '[], '[]],
	sbtss ~ '[slbts],
	Storable w4,
	Vk.Mm.OffsetSize nm4 (VObj.List 256 w4 "") objss4,
	InfixIndex '[slbts] sbtss ) =>
	Vk.Dv.D sd -> Vk.QF.Index -> Vk.CmdBuf.C sc ->
	Vk.Ppl.Cmpt.C sg2 '(sl2, sbtss, PushConstants) ->
	Vk.Ppl.Lyt.P sl2 sbtss PushConstants ->
	Vk.DS.D sds slbts ->
	Vk.Mm.M sm4 objss4 -> Word32 -> Word32 -> Constants -> IO (V.Vector w4)
run2 dv qf cb ppl2 plyt2 ds m w h cs = do
	q <- Vk.Dv.getQueue dv qf 0
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl2 \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb plyt2
				(U2 ds :** HL.Nil)
				(HL.Singleton $ HL.Nil :** HL.Nil :** HL.Nil)
			Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit ]
				ccb plyt2 ((w :: Word32) :* cs)
			Vk.Cmd.dispatch ccb w h 1
	Vk.Queue.submit q (U4 submitInfo2 :** HL.Nil) Nothing
	Vk.Queue.waitIdle q
	Vk.Mm.read @nm4 @(VObj.List 256 w4 "") @(V.Vector w4) dv m def
	where
	submitInfo2 :: Vk.SubmitInfo 'Nothing '[] '[sc] '[]
	submitInfo2 = Vk.SubmitInfo {
			Vk.submitInfoNext = TMaybe.N,
			Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Nil,
			Vk.submitInfoCommandBuffers = cb :** HL.Nil,
			Vk.submitInfoSignalSemaphores = HL.Nil }

zero, one, mone :: CFloat
zero = 0; one = 1; mone = - 1

type Pixels = ((Int, Int), V.Vector Pixel)

readPixels :: InOut -> FilePath -> IO Pixels
readPixels io@(inp, outp) fp = readPngRgba fp >>= \case
	Right img -> pure
		$ ((P.imageWidth &&& P.imageHeight) &&&
			V.unsafeCast . P.imageData) img
	Left e -> do
		putStrLn $ "readPixels: error " ++ show e
		putStrLn "Input PNG (RGBA8) file path"
		fp' <- atomically $ readTChan inp
		atomically $ writeTChan outp True
		readPixels io fp'

readPngRgba ::
	FilePath -> IO (Either (Either IOError String) (P.Image P.PixelRGBA8))
readPngRgba fp = tryReadFile fp >>= \case
	Left e -> pure . Left $ Left e
	Right bs -> pure case P.decodePng bs of
		Left em -> Left $ Right em
		Right (P.ImageRGBA8 img) -> Right img
		Right _ -> Left $ Right "readPngRgba: The format is not RGBA8"

tryReadFile :: FilePath -> IO (Either IOError BS.ByteString)
tryReadFile = try . BS.readFile

writePixels :: FilePath -> Pixels -> IO ()
writePixels fp ((w, h), pxs) =
	P.writePng @P.PixelRGBA8 fp . P.Image w h $ V.unsafeCast pxs

data Pixel = Pixel Word8 Word8 Word8 Word8 deriving Show

type instance Vk.BffVw.FormatOf Pixel = 'Vk.T.FormatR8g8b8a8Uint

instance Storable Pixel where
	sizeOf _ = 4 * sizeOf @Word8 undefined
	alignment _ = alignment @Word8 undefined
	peek p = peekArray 4 (castPtr p) >>= \case
		[r, g, b, a] -> pure (Pixel r g b a); _ -> error "never occur"
	poke p (Pixel r g b a) = pokeArray (castPtr p) [r, g, b, a]

data PixelFloat = PixelFloat CFloat CFloat CFloat CFloat deriving Show

type instance Vk.BffVw.FormatOf PixelFloat = 'Vk.T.FormatR32g32b32a32Sfloat

instance Storable PixelFloat where
	sizeOf _ = 4 * sizeOf @CFloat undefined
	alignment _ = alignment @CFloat undefined
	peek p = peekArray 4 (castPtr p) >>= \case
		[r, g, b, a] -> pure (PixelFloat r g b a); _ -> error "never occur"
	poke p (PixelFloat r g b a) = pokeArray (castPtr p) [r, g, b, a]

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

setupDebugMessenger ::
	Vk.Ist.I si ->
	IO a -> IO a
setupDebugMessenger ist f = Vk.Ext.DbgUtls.Msngr.create ist
	debugMessengerCreateInfo nil f

debugMessengerCreateInfo :: Vk.Ext.DbgUtls.Msngr.CreateInfo 'Nothing '[] ()
debugMessengerCreateInfo = Vk.Ext.DbgUtls.Msngr.CreateInfo {
	Vk.Ext.DbgUtls.Msngr.createInfoNext = TMaybe.N,
	Vk.Ext.DbgUtls.Msngr.createInfoFlags = def,
	Vk.Ext.DbgUtls.Msngr.createInfoMessageSeverity =
		Vk.Ext.DbgUtls.MessageSeverityVerboseBit .|.
		Vk.Ext.DbgUtls.MessageSeverityWarningBit .|.
		Vk.Ext.DbgUtls.MessageSeverityErrorBit,
	Vk.Ext.DbgUtls.Msngr.createInfoMessageType =
		Vk.Ext.DbgUtls.MessageTypeGeneralBit .|.
		Vk.Ext.DbgUtls.MessageTypeValidationBit .|.
		Vk.Ext.DbgUtls.MessageTypePerformanceBit,
	Vk.Ext.DbgUtls.Msngr.createInfoFnUserCallback = debugCallback,
	Vk.Ext.DbgUtls.Msngr.createInfoUserData = Nothing }

debugCallback :: Vk.Ext.DbgUtls.Msngr.FnCallback '[] ()
debugCallback _msgSeverity _msgType cbdt _userData = False <$ Txt.putStrLn
	("validation layer: " <> Vk.Ext.DbgUtls.Msngr.callbackDataMessage cbdt)
