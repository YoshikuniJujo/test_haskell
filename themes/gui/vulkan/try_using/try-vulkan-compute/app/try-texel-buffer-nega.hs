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
import Control.Concurrent.STM
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.List
import Data.HeteroParList qualified as HL
import Data.HeteroParList (pattern (:*), pattern (:*.), pattern (:**))
import Data.Word

import qualified Data.Vector.Storable as V

import Language.SpirV qualified as SpirV
import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind
import Gpu.Vulkan.Misc

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

main :: IO ()
main = do
	inp <- atomically newTChan
	outp <- atomically newTChan
	inf : _ <- getArgs
	_ <- forkIO do
		makeNega "autogen/nega_result.png" inp outp =<< readPixels inf
		atomically $ writeTChan outp False

	fix \rec -> do
		atomically . writeTChan inp =<< getLine
		b <- atomically $ readTChan outp
		if b then rec else pure ()

makeNega :: FilePath -> TChan String -> TChan Bool -> Pixels -> IO ()
makeNega outf inp outp (sz@(fromIntegral -> w, fromIntegral -> h), v) =
	device \phd qf dv -> dscSetLayout dv \dslyt ->
	buffer phd dv dslyt v \ds (m :: Memory sm sb nm) ->
	pipeline dv qf dslyt \cb ppl ppl2 plyt plyt2 -> do
		run1 dv qf cb ppl plyt ds w h
		run2Loop @nm outf inp outp dv qf cb ppl2 plyt2 ds m w h nega sz
		{-
		rslt <- (sz ,) <$> run2 @nm dv qf cb ppl2 plyt2 ds m w h nega
		writePixels outf rslt
		atomically $ readTChan inp
		rslt <- (sz ,) <$> run2 @nm dv qf cb ppl2 plyt2 ds m w h red
		writePixels outf rslt
		atomically $ readTChan inp
		putStrLn "begin green"
		rslt <- (sz ,) <$> run2 @nm dv qf cb ppl2 plyt2 ds m w h green
		writePixels outf rslt
		putStrLn "end green"
		atomically $ readTChan inp
		putStrLn "begin blue"
		rslt <- (sz ,) <$> run2 @nm dv qf cb ppl2 plyt2 ds m w h blue
		writePixels outf rslt
		putStrLn "end blue"
		-}

run2Loop :: forall nm4 objss4 slbts sbtss sd sc sg2 sl2 sm4 sds . (
	Vk.DSLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts) ~ '[ '[], '[]],
	sbtss ~ '[slbts],
	Vk.Mm.OffsetSize nm4 (VObj.List 256 Pixel "") objss4,
	InfixIndex '[slbts] sbtss ) =>
	FilePath -> TChan String -> TChan Bool ->
	Vk.Dv.D sd -> Vk.QF.Index -> Vk.CmdBuf.C sc ->
	Vk.Ppl.Cmpt.C sg2 '(sl2, sbtss, PushConstants) ->
	Vk.Ppl.Lyt.P sl2 sbtss PushConstants ->
	Vk.DS.D sds slbts ->
	Vk.Mm.M sm4 objss4 -> Word32 -> Word32 -> Constants -> (Int, Int) ->
	IO ()
run2Loop outf inp outp dv qf cb ppl2 plyt2 ds m w h cs sz = do
	rslt <- (sz ,) <$> run2 @nm4 @_ @objss4 dv qf cb ppl2 plyt2 ds m w h cs
	writePixels outf rslt
	fix \rec -> do
		cmd <- atomically $ readTChan inp
		case (cmd, nameConstant cmd) of
			("quit", _) -> pure ()
			(_, Just c) -> do
				atomically $ writeTChan outp True
				run2Loop @nm4 @objss4 outf inp outp dv qf cb ppl2 plyt2 ds m w h c sz
			(_, Nothing) -> do
				atomically $ writeTChan outp True
				rec

nameConstant :: String -> Maybe Constants
nameConstant = \case
	"nega" -> Just nega; "red" -> Just red
	"green" -> Just green; "blue" -> Just blue
	_ -> Nothing

type Constants =
	HL.L '[CFloat, CFloat, CFloat, CFloat, CFloat, CFloat, CFloat, CFloat]

nega :: Constants
nega = mone :* one :* mone :* one :* mone :* one :* one :* zero :* HL.Nil

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
device f = Vk.Ist.create @_ @'Nothing instInfo nil' \ist -> do
	phd <- head <$> Vk.Phd.enumerate ist
	qf <- findQueueFamily phd Vk.Queue.ComputeBit
	Vk.Dv.create phd (dvcInfo qf) nil' \dv -> f phd qf dv
	where
	instInfo :: Vk.Ist.CreateInfo 'Nothing 'Nothing
	instInfo = def {
		Vk.Ist.createInfoEnabledLayerNames =
			[Vk.layerKhronosValidation] }
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

buffer :: forall bts sd sl nm a . (
	Default (HL.PL (HL.PL KObj.Length)
		(Vk.DSLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", PixelFloat)] 0,
	Vk.DS.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0 ) =>
	Vk.Phd.P -> Vk.Dv.D sd -> Vk.DSLyt.D sl bts -> V.Vector Pixel -> (
		forall sds sm sb .
		Vk.DS.D sds '(sl, bts) -> Memory sm sb nm -> IO a ) -> IO a
buffer phd dv lyt v f =
	Vk.DP.create dv poolInfo nil' \pl ->
	Vk.DS.allocateDs dv (setInfo pl)
		\((ds :: Vk.DS.D sds '(sl, bts)) :** HL.Nil) ->
	bufferNew dv phd v \(bd :: Vk.Bff.Binded sm sb nm '[PixelList, PixelFloatList]) m ->
	Vk.BffVw.create dv (bvInfo bd) nil' \(bv :: Vk.BffVw.B sbv "" Pixel) ->
	Vk.BffVw.create dv (bvInfo bd) nil' \(bv2 :: Vk.BffVw.B sbv2 "" PixelFloat) ->
	Vk.DS.updateDs dv
		(U5 (write ds bv) :** U5 (write2 ds bv2) :** HL.Nil)
		HL.Nil >> f ds m
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
	bvInfo bd = Vk.BffVw.CreateInfo {
		Vk.BffVw.createInfoNext = TMaybe.N,
		Vk.BffVw.createInfoFlags = zeroBits,
		Vk.BffVw.createInfoBuffer = U4 bd }
	write :: Vk.DS.D sds '(sl, bts) -> Vk.BffVw.B sb "" Pixel ->
		Vk.DS.Write 'Nothing sds '(sl, bts)
			(Vk.DS.WriteSourcesArgBufferView '[ '(sb, "", Pixel)]) 0
	write ds bv = Vk.DS.Write {
		Vk.DS.writeNext = TMaybe.N,
		Vk.DS.writeDstSet = ds,
		Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DS.writeSources =
			Vk.DS.TexelBufferViews . HL.Singleton $ U3 bv }
	write2 :: Vk.DS.D sds '(sl, bts) -> Vk.BffVw.B sb "" PixelFloat ->
		Vk.DS.Write 'Nothing sds '(sl, bts)
			(Vk.DS.WriteSourcesArgBufferView '[ '(sb, "", PixelFloat)]) 0
	write2 ds bv = Vk.DS.Write {
		Vk.DS.writeNext = TMaybe.N,
		Vk.DS.writeDstSet = ds,
		Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DS.writeSources =
			Vk.DS.TexelBufferViews . HL.Singleton $ U3 bv }

bufferNew :: forall sd nm a .
	Vk.Dv.D sd -> Vk.Phd.P -> V.Vector Pixel -> (forall sb sm .
		Vk.Bff.Binded sm sb nm '[PixelList, PixelFloatList] -> Memory sm sb nm ->
		IO a) -> IO a
bufferNew dv phd v f =
	Vk.Bff.create dv bufferInfo nil' \bffr ->
	memoryInfo bffr >>= \mi ->
	Vk.Mm.allocateBind dv (U2 (Vk.Mm.Buffer bffr) :** HL.Nil) mi nil'
		\(U2 (Vk.Mm.BufferBinded bnd) :** HL.Nil) m ->
	Vk.Mm.write @nm @PixelList dv m def v >> f bnd m
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
		Vk.Ppl.Cmpt.C s1 '(sl1, '[ '(sl, bts)], '[Word32]) ->
		Vk.Ppl.Cmpt.C s2 '(sl2, '[ '(sl, bts)], PushConstants) ->
		Vk.Ppl.Lyt.P sl1 '[ '(sl, bts)] '[Word32] ->
		Vk.Ppl.Lyt.P sl2 '[ '(sl, bts)] PushConstants ->
		IO a) -> IO a
pipeline dv qf dslyt f =
	Vk.Ppl.Lyt.create dv plytInfo nil' \plyt ->
	Vk.Ppl.Lyt.create dv plytInfo nil' \plyt2 ->
	Vk.Ppl.Cmpt.createCs dv Nothing
		(U4 (pplInfo glslComputeShaderMain plyt) :** HL.Nil) nil'
		\(ppl :** HL.Nil) ->
	Vk.Ppl.Cmpt.createCs dv Nothing
		(U4 (pplInfo glslComputeShaderMain2 plyt2) :** HL.Nil) nil'
		\(ppl2 :** HL.Nil) ->
	Vk.CommandPool.create dv cpoolInfo nil' \cp ->
	Vk.CmdBuf.allocate dv (cbInfo cp) \(cb :*. HL.Nil) -> f cb ppl ppl2 plyt plyt2
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
		Vk.Ppl.ShaderSt.createInfoModule = (shaderModInfo sdr, nil'),
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

readPixels :: FilePath -> IO Pixels
readPixels fp = (<$> P.readPng fp) \case
	Right (P.ImageRGBA8 img) -> ((P.imageWidth &&& P.imageHeight) &&&
			V.unsafeCast . P.imageData) img
	_ -> error "readPixels: can't get pixels"

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
