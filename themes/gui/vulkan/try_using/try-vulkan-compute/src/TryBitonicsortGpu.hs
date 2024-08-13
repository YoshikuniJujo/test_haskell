{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms, BangPatterns, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryBitonicsortGpu (bitonicsortGpu) where

import Foreign.Storable
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.List qualified as L
import Data.List.Length
import Data.HeteroParList (pattern (:*))
import Data.HeteroParList qualified as HPList
import Data.Word
import Data.Int

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.Instance qualified as Vk.Inst
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd
import Gpu.Vulkan.Semaphore qualified as Vk.Semaphore
import Gpu.Vulkan.Fence qualified as Vk.Fence

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Pipeline.Compute qualified as Vk.Ppl.Cmpt
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShdrSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.PplLyt
import Gpu.Vulkan.PushConstant qualified as Vk.PushConstant
import Gpu.Vulkan.ShaderModule qualified as Vk.ShdrMd
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt

import Tools

---------------------------------------------------------------------------

-- BITONICSORT GPU
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- BITONICSORT GPU

bitonicsortGpu :: (forall n . Integral n => n) -> [Word32] -> IO [Word32]
bitonicsortGpu sz dt = withDvc \pd dv q cp mgcx -> let szx = lg mgcx in
	Vk.DscStLyt.create dv dscStLytInfo nil \dsl ->
	prepareMm pd dv dsl dt \ds m -> calc sz dv q cp dsl ds szx (2 ^ szx) >>
	Vk.Mm.read @"" @(VObj.List 256 Word32 "") @0 dv m zeroBits
	where lg n | n < 2 = 0; lg n = 1 + lg (n `div` 2)

withDvc :: (forall sd scp .
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp -> Word32 ->
	IO a) -> IO a
withDvc a = Vk.Inst.create iinfo nil \inst -> do
	pd <- head' <$> Vk.Phd.enumerate inst
	qfi <- fst . head' . filter
		(checkBits Vk.Q.ComputeBit . Vk.QFam.propertiesQueueFlags . snd)
		<$> Vk.Phd.getQueueFamilyProperties pd
	mgcx :. _mgcy :. _mgcz :. _ <- Vk.Phd.limitsMaxComputeWorkGroupCount
		. Vk.Phd.propertiesLimits <$> Vk.Phd.getProperties pd
	Vk.Dvc.create pd (dinfo qfi) nil \dv ->
		Vk.Dvc.getQueue dv qfi 0 >>= \q ->
		Vk.CmdPl.create dv (cpinfo qfi) nil \cp -> a pd dv q cp mgcx
	where
	iinfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
	iinfo = def {
		Vk.Inst.createInfoEnabledLayerNames =
			[Vk.layerKhronosValidation] }
	dinfo qfi = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = TMaybe.N,
		Vk.Dvc.createInfoFlags = zeroBits,
		Vk.Dvc.createInfoQueueCreateInfos = HPList.Singleton $ qi qfi,
		Vk.Dvc.createInfoEnabledLayerNames =
			[Vk.layerKhronosValidation],
		Vk.Dvc.createInfoEnabledExtensionNames = [],
		Vk.Dvc.createInfoEnabledFeatures = Nothing }
	qi qfi = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }
	cpinfo qfi = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

dscStLytInfo :: Vk.DscStLyt.CreateInfo 'Nothing
	'[ 'Vk.DscStLyt.Buffer '[VObj.List 256 Word32 ""]]
dscStLytInfo = Vk.DscStLyt.CreateInfo {
	Vk.DscStLyt.createInfoNext = TMaybe.N,
	Vk.DscStLyt.createInfoFlags = zeroBits,
	Vk.DscStLyt.createInfoBindings = HPList.Singleton
		Vk.DscStLyt.BindingBuffer {
			Vk.DscStLyt.bindingBufferDescriptorType =
				Vk.Dsc.TypeStorageBuffer,
			Vk.DscStLyt.bindingBufferStageFlags =
				Vk.ShaderStageComputeBit } }

-- PREPARE MEMORIES

prepareMm :: forall sd sl bts a . (
	Vk.DscSt.BindingAndArrayElemBuffer bts '[VObj.List 256 Word32 ""] 0,
	Vk.DscSt.UpdateDynamicLength bts '[VObj.List 256 Word32 ""],
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	[Word32] -> (forall sds sm sb .
		Vk.DscSt.D sds '(sl, bts) ->
		Vk.Mm.M sm '[ '(
			sb,
			'Vk.Mm.BufferArg "" '[VObj.List 256 Word32 ""] )] ->
		IO a) -> IO a
prepareMm pd dv dsl ws a =
	Vk.DscPl.create dv dpinfo nil \dpl ->
	Vk.DscSt.allocateDs dv (dsinfo dpl) \(HPList.Singleton ds) ->
	bffrNew dv pd (fromIntegral $ L.length ws) \b m ->
	Vk.Mm.write @"" @(VObj.List 256 Word32 "") @0 dv m def ws >>
	Vk.DscSt.updateDs dv (HPList.Singleton . U5 $ wr ds b) HPList.Nil >>
	a ds m
	where
	dpinfo = Vk.DscPl.CreateInfo {
		Vk.DscPl.createInfoNext = TMaybe.N,
		Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
		Vk.DscPl.createInfoMaxSets = 1,
		Vk.DscPl.createInfoPoolSizes = [Vk.DscPl.Size {
			Vk.DscPl.sizeType = Vk.Dsc.TypeStorageBuffer,
			Vk.DscPl.sizeDescriptorCount = 1 }] }
	dsinfo :: Vk.DscPl.P sp ->
		Vk.DscSt.AllocateInfo 'Nothing sp '[ '(sl, bts)]
	dsinfo dpl = Vk.DscSt.AllocateInfo {
		Vk.DscSt.allocateInfoNext = TMaybe.N,
		Vk.DscSt.allocateInfoDescriptorPool = dpl,
		Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }
	wr :: Vk.DscSt.D sds slbts ->
		Vk.Bffr.Binded sm sb "" '[VObj.List 256 Word32 ""] ->
		Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer
			'[ '(sm, sb, "", VObj.List 256 Word32 "", 0)]) 0
	wr ds b = Vk.DscSt.Write {
		Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
		Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscSt.writeSources = Vk.DscSt.BufferInfos
			. HPList.Singleton . U5 $ Vk.Dsc.BufferInfo b }

bffrNew :: forall sd nm w a . Storable w =>
	Vk.Dvc.D sd -> Vk.Phd.P ->Vk.Dvc.Size -> (forall sb sm .
		Vk.Bffr.Binded sm sb nm '[VObj.List 256 w ""]  ->
		Vk.Mm.M sm '[
			'(sb, 'Vk.Mm.BufferArg nm '[VObj.List 256 w ""]) ] ->
		IO a) -> IO a
bffrNew dv pd ln a = Vk.Bffr.create dv binfo nil \bf -> mminfo bf >>= \mmi ->
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil
		\(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm -> a bnd mm
	where
	binfo = Vk.Bffr.CreateInfo {
		Vk.Bffr.createInfoNext = TMaybe.N,
		Vk.Bffr.createInfoFlags = zeroBits,
		Vk.Bffr.createInfoLengths =
			HPList.Singleton $ VObj.LengthList ln,
		Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
		Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
		Vk.Bffr.createInfoQueueFamilyIndices = [] }
	mminfo :: Vk.Bffr.B sb nm objs -> IO (Vk.Mm.AllocateInfo 'Nothing)
	mminfo bf = do
		rqs <- Vk.Bffr.getMemoryRequirements dv bf
		mti <- mmTpIdx rqs $
			Vk.Mm.PropertyHostVisibleBit .|.
			Vk.Mm.PropertyHostCoherentBit
		pure Vk.Mm.AllocateInfo {
			Vk.Mm.allocateInfoNext = TMaybe.N,
			Vk.Mm.allocateInfoMemoryTypeIndex = mti }
	mmTpIdx rqs pr0 = do
		prs <- Vk.Phd.getMemoryProperties pd
		let	rqts = Vk.Mm.requirementsMemoryTypeBits rqs
			prts = (fst <$>)
				. filter (checkBits pr0
					. Vk.Mm.mTypePropertyFlags . snd)
				$ Vk.Phd.memoryPropertiesMemoryTypes prs
		case filter (`Vk.Mm.elemTypeIndex` rqts) prts of
			[] -> error "No available memory types"; i : _ -> pure i

-- CALC

calc :: forall sd scp sds sl bts .
	(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]]) =>
	(forall n . Integral n => n) ->
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.D sds '(sl, bts) -> Word32 -> Word32 -> IO ()
calc sz dv qu cp dsl ds szx mgcx' =
	Vk.PplLyt.create dv plinfo nil \pl ->
	Vk.Ppl.Cmpt.createCs dv Nothing (HPList.Singleton . U4 $ cmpPplInfo szx pl)
		nil \(HPList.Singleton ppl) ->
	Vk.CmdBffr.allocateList dv cbinfo \cbs ->
	runAll sz dv qu pl ppl ds szx mgcx' (L.zip3 cbs ps qs) \fnc ->
	Vk.Fence.waitForFs dv (HPList.Singleton fnc) True Nothing
	where
	plinfo :: Vk.PplLyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout
			'[Int32, Int32]
			'[ 'Vk.PushConstant.Range
				'[ 'Vk.T.ShaderStageComputeBit]
				'[Int32, Int32]])
	plinfo = Vk.PplLyt.CreateInfo {
		Vk.PplLyt.createInfoNext = TMaybe.N,
		Vk.PplLyt.createInfoFlags = zeroBits,
		Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }
	cbinfo = Vk.CmdBffr.AllocateInfoList {
		Vk.CmdBffr.allocateInfoNextList = TMaybe.N,
		Vk.CmdBffr.allocateInfoCommandPoolList = cp,
		Vk.CmdBffr.allocateInfoLevelList = Vk.CmdBffr.LevelPrimary,
		Vk.CmdBffr.allocateInfoCommandBufferCountList =
			sz * (sz + 1) `div` 2 }
	(ps, qs) = unzip $ pqs sz 0 0
	pqs i p q
		| i <= p = []
		| p <= q = (p, q) : pqs i (p + 1) 0
		| otherwise = (p, q) : pqs i p (q + 1)

runAll :: (Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]] ) =>
	Word32 -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.PplLyt.P sl '[slbts] '[Int32, Int32] ->
	Vk.Ppl.Cmpt.C scppl '(sl, '[slbts], '[Int32, Int32]) ->
	Vk.DscSt.D sds slbts -> Word32 -> Word32 ->
	[(Vk.CmdBffr.C scb, Int32, Int32)] ->
	(forall sf . Vk.Fence.F sf -> IO a) -> IO a
runAll sz dv qu pl ppl ds szx mgcx' = repeatBeginEnd
	(run' sz dv qu pl ppl ds szx mgcx' Nothing)
	(\s -> run' sz dv qu pl ppl ds szx mgcx' $ Just s)
	(runEnd' sz dv qu pl ppl ds szx mgcx')

repeatBeginEnd :: Monad m =>
	(forall b . a -> (forall ss . s ss -> m b) -> m b) ->
	(forall ss b . s ss -> a -> (forall st . s st -> m b) -> m b) ->
	(forall ss b . s ss -> a -> (forall st . t st -> m b) -> m b) -> [a] ->
	(forall ss . t ss -> m c) -> m c
repeatBeginEnd _ _ _ [] _ = error "bad"
repeatBeginEnd b m e (x : xs) a = b x \s -> repeatEnd s xs m \t y -> e t y a

repeatEnd :: Monad m => s ss -> [a] ->
	(forall st b . s st -> a -> (forall su . s su -> m b) -> m b) ->
	(forall st . s st -> a -> m c) -> m c
repeatEnd _ [] _ _ = error "bad"
repeatEnd s [x] _ b = b s x
repeatEnd s (x : xs) a b = a s x \t -> repeatEnd t xs a b

run' :: (Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]]) =>
	Word32 -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.PplLyt.P sl '[slbts] '[Int32, Int32] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[Int32, Int32]) ->
	Vk.DscSt.D sds slbts -> Word32 -> Word32 ->
	Maybe (Vk.Semaphore.S ss) -> (Vk.CmdBffr.C sc, Int32, Int32) ->
	(forall st . Vk.Semaphore.S st -> IO b) -> IO b
run' sz dv qu pl ppl ds szx mgcx' Nothing (cb, p, q) =
	run sz dv qu cb pl ppl ds szx mgcx' HPList.Nil p q
run' sz dv qu pl ppl ds szx mgcx' (Just s) (cb, p, q) =
	run sz dv qu cb pl ppl ds szx mgcx' (HPList.Singleton s) p q

runEnd' :: (Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]]) =>
	Word32 -> Vk.Dvc.D sd -> Vk.Q.Q ->
	Vk.PplLyt.P sl '[slbts] '[Int32, Int32] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[Int32, Int32]) ->
	Vk.DscSt.D sds slbts -> Word32 -> Word32 ->
	Vk.Semaphore.S ss -> (Vk.CmdBffr.C sc, Int32, Int32) ->
	(forall sf . Vk.Fence.F sf -> IO b) -> IO b
runEnd' sz dv qu pl ppl ds szx mgcx' s (cb, n, q) a =
	runEnd sz dv qu cb pl ppl ds szx mgcx' (HPList.Singleton s) n q a

run :: forall sd sc sg sl sds slbts swss a .
	(Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]]) =>
	Word32 -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdBffr.C sc ->
	Vk.PplLyt.P sl '[slbts] '[Int32, Int32] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[Int32, Int32]) ->
	Vk.DscSt.D sds slbts -> Word32 -> Word32 ->
	HPList.PL Vk.Semaphore.S swss -> Int32 -> Int32 ->
	(forall ss . Vk.Semaphore.S ss -> IO a) -> IO a
run sz dv qu cb pl ppl ds szx mgcx' wss p q a =
	dispatch sz cb pl ppl ds szx mgcx' p q >>
	Vk.Semaphore.create dv Vk.Semaphore.CreateInfo {
		Vk.Semaphore.createInfoNext = TMaybe.N,
		Vk.Semaphore.createInfoFlags = zeroBits } nil \s ->
	Vk.Q.submit qu
		(HPList.Singleton . U4 . submitInfo cb wss $ HPList.Singleton s)
		Nothing >> a s

runEnd :: forall sd sc sg sl sds slbts swss a .
	(Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]]) =>
	Word32 -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdBffr.C sc ->
	Vk.PplLyt.P sl '[slbts] '[Int32, Int32] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[Int32, Int32]) ->
	Vk.DscSt.D sds slbts -> Word32 -> Word32 ->
	HPList.PL Vk.Semaphore.S swss -> Int32 -> Int32 ->
	(forall sf . Vk.Fence.F sf -> IO a) -> IO a
runEnd sz dv qu cb pl ppl ds szx mgcx' wss p q a =
	dispatch sz cb pl ppl ds szx mgcx' p q >>
	Vk.Fence.create dv Vk.Fence.CreateInfo {
		Vk.Fence.createInfoNext = TMaybe.N,
		Vk.Fence.createInfoFlags = zeroBits } nil \f ->
	Vk.Q.submit qu
		(HPList.Singleton . U4 $ submitInfo cb wss HPList.Nil)
		(Just f) >> a f

dispatch :: (Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]]) =>
	Word32 -> Vk.CmdBffr.C sc ->
	Vk.PplLyt.P sl '[slbts] '[Int32, Int32] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[Int32, Int32]) ->
	Vk.DscSt.D sds slbts -> Word32 -> Word32 ->
	Int32 -> Int32 -> IO ()
dispatch sz cb pl ppl ds szx mgcx' p q =
	Vk.CmdBffr.begin @'Nothing @'Nothing cb def $
	Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb ->
	Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit]
		ccb pl (p :* q :* HPList.Nil) >>
	Vk.Cmd.bindDescriptorSetsCompute ccb
		pl (HPList.Singleton $ U2 ds) (HPList.Singleton2 HPList.Nil) >>
	Vk.Cmd.dispatch ccb (mgcx' `div` 1024) ((2 ^ (sz - szx - 1)) `div` 1) 1

submitInfo :: Vk.CmdBffr.C sc ->
	HPList.PL Vk.Semaphore.S ss -> HPList.PL Vk.Semaphore.S sws ->
	Vk.SubmitInfo Nothing ss '[sc] sws
submitInfo cb wss ss = Vk.SubmitInfo {
	Vk.submitInfoNext = TMaybe.N,
	Vk.submitInfoWaitSemaphoreDstStageMasks =
		(`Vk.SemaphorePipelineStageFlags` Vk.Ppl.StageComputeShaderBit)
			`HPList.map` wss,
	Vk.submitInfoCommandBuffers = HPList.Singleton cb,
	Vk.submitInfoSignalSemaphores = ss }

-- COMPUTE PIPELINE INFO

cmpPplInfo :: Word32 ->Vk.PplLyt.P sl sbtss '[Int32, Int32] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[Word32])
		'(sl, sbtss, '[Int32, Int32]) sbph
cmpPplInfo szx pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U5 $ shaderStageInfo szx,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shaderStageInfo :: Word32 -> Vk.Ppl.ShdrSt.CreateInfo
	'Nothing 'Nothing 'GlslComputeShader 'Nothing '[Word32]
shaderStageInfo szx = Vk.Ppl.ShdrSt.CreateInfo {
	Vk.Ppl.ShdrSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShdrSt.createInfoFlags = zeroBits,
	Vk.Ppl.ShdrSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShdrSt.createInfoModule = (minfo, nil),
	Vk.Ppl.ShdrSt.createInfoName = "main",
	Vk.Ppl.ShdrSt.createInfoSpecializationInfo = Just $ szx :* HPList.Nil }
	where minfo = Vk.ShdrMd.CreateInfo {
		Vk.ShdrMd.createInfoNext = TMaybe.N,
		Vk.ShdrMd.createInfoFlags = zeroBits,
		Vk.ShdrMd.createInfoCode = glslComputeShaderMain }

[glslComputeShader|

#version 460

layout(local_size_x = 1024, local_size_y = 1) in;

layout(constant_id = 0) const uint szx = 0;
layout(binding = 0) buffer Data { uint v[]; } data[1];
layout(push_constant) uniform Pq { int p; int q; } pq;

void
main()
{
	int i = int(gl_GlobalInvocationID.x) +
		(int(gl_GlobalInvocationID.y) << szx);

	int r = pq.p - pq.q;
	int u = i >> r << r; int l = i ^ u;
	int x = u << 1 | l;
	int f = x | i >> pq.q & 1 << r; int t = x | ~i >> pq.q & 1 << r;

	if (data[0].v[f] > data[0].v[t])
		data[0].v[f] = atomicExchange(data[0].v[t], data[0].v[f]);
}

|]
