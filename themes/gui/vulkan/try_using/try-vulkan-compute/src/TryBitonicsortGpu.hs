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

import qualified Gpu.Vulkan.Memory as Vk.Mem

import Foreign.Storable
import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Object.Base qualified as KObj
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.List
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*), pattern (:**))
import Data.Word
import Data.Int

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Memory as Vk.Mm.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscStLyt

import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import Gpu.Vulkan.Semaphore qualified as Vk.Semaphore
import Gpu.Vulkan.Fence qualified as Vk.Fence

import Data.List qualified as L

import Gpu.Vulkan.TypeEnum qualified as Vk.T

import Tools

---------------------------------------------------------------------------

-- BITONICSORT GPU
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- BITONICSORT GPU

bitonicsortGpu :: (forall n . Integral n => n) -> [Word32] -> IO [Word32]
bitonicsortGpu sz dt = withDvc \pd dv q cp mgcx ->
	let mgcx' = 2 ^ (lg mgcx :: Int) in
	Vk.DscStLyt.create dv dscStLytInfo nil \dsl ->
	prepareMm pd dv dsl dt \ds m -> calc sz dv q cp dsl ds mgcx' >>
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

prepareMm :: (
	Default (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DscSet.BindingAndArrayElemBuffer bts '[VObj.List 256 W3 ""] 0,
	Vk.DscSet.UpdateDynamicLength bts '[VObj.List 256 W3 ""]
	) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	[W3] -> (forall sds sm3 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mm.M sm3 '[ '( sb3, 'Vk.Mm.BufferArg "" '[VObj.List 256 W3 ""])] -> IO a) -> IO a
prepareMm phdvc dvc dscSetLyt dc f =
	Vk.DscPool.create dvc dscPoolInfo nil \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(HPList.Singleton dscSet) ->
	storageBufferNew dvc phdvc (fromIntegral $ L.length dc) \bc mc ->
	Vk.Mm.write @"" @(VObj.List 256 W3 "") @0 dvc mc def dc >>
	Vk.DscSet.updateDs dvc
		(HPList.Singleton . U5 $ writeDscSet dscSet bc)
		HPList.Nil >>
	f dscSet mc

type W3 = Word32

dscPoolInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 1 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSet.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = TMaybe.N,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts =
		HPList.Singleton $ U2 lyt }

storageBufferNew :: forall sd nm w a . Storable w =>
	Vk.Dvc.D sd -> Vk.Phd.P ->Vk.Dvc.Size -> (forall sb sm .
		Vk.Buffer.Binded sm sb nm '[VObj.List 256 w ""]  ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[VObj.List 256 w ""])] ->
		IO a) -> IO a
storageBufferNew dvc phdvc ln f =
	Vk.Buffer.create dvc (bufferInfo ln) nil \bf ->
	getMemoryInfo phdvc dvc bf >>= \mmi ->
	Vk.Mm.allocateBind dvc
		(HPList.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil
		\(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	f bnd mm

bufferInfo :: Vk.Dvc.Size -> Vk.Buffer.CreateInfo 'Nothing '[VObj.List 256 w ""]
bufferInfo ln = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		VObj.LengthList ln :** HPList.Nil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Mem.AllocateInfo 'Nothing)
getMemoryInfo phdvc dvc buffer = do
	rqs <- Vk.Buffer.getMemoryRequirements dvc buffer
	mti <- findMemoryTypeIndex phdvc rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mti }

findMemoryTypeIndex ::
	Vk.Phd.P -> Vk.Mm.M.Requirements -> Vk.Mm.PropertyFlags ->
	IO Vk.Mm.M.TypeIndex
findMemoryTypeIndex phdvc rqs prp0 = do
	memoryProperties <- Vk.Phd.getMemoryProperties phdvc
	let	reqTypes = Vk.Mm.M.requirementsMemoryTypeBits rqs
		memPropTypes = (fst <$>)
			. filter (checkBits prp0
				. Vk.Mm.M.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes memoryProperties
	case filter (`Vk.Mm.M.elemTypeIndex` reqTypes) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> pure i

writeDscSet ::
	forall slbts sb3 sm3 objs3 sds . (
	Show (HPList.PL VObj.Length objs3),
	VObj.OffsetRange (VObj.List 256 W3 "") objs3 0 ) =>
	Vk.DscSet.D sds slbts ->
	Vk.Buffer.Binded sm3 sb3 "" objs3 ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm3, sb3, "", VObj.List 256 W3 "", 0) ]) 0
writeDscSet ds bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(VObj.List 256 W3 "") @0 bc) :**
		HPList.Nil }

-- CALC

calc :: forall scp slbts sl bts sd sds . (
	slbts ~ '(sl, bts),
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]] ) =>
	(forall n . Integral n => n) -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSet.D sds slbts ->
	Word32 -> IO ()
calc ls dvc q cmdPool dscSetLyt dscSet dsz =
	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dscSetLyt) nil \plyt ->
	Vk.Ppl.Cmpt.createCs dvc Nothing
		(HPList.Singleton . U4 $ computePipelineInfo plyt)
		nil \(ppl :** HPList.Nil) ->
	Vk.CmdBuf.allocateList dvc (commandBufferInfoList ls cmdPool) \cbs ->
		putStrLn "BEGIN CALC" >>
--		let (ps, qs) = pqs in
		let (ps, qs) = unzip $ makePqs' ls 0 0 in
		runAll' ls dvc q ppl plyt dscSet dsz (L.zip3 cbs ps qs) \fnc ->
		Vk.Fence.waitForFs dvc (HPList.Singleton fnc) True Nothing

makePqs' :: Int32 -> Int32 -> Int32 -> [(Int32, Int32)]
makePqs' i p q
	| i <= p = []
	| p <= q = (p, q) : makePqs' i (p + 1) 0
	| otherwise = (p, q) : makePqs' i p (q + 1)

runAll' :: (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]] ) =>
	Int -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[Int32, Int32]) ->
	Vk.Ppl.Lyt.P sl sbtss '[Int32, Int32] -> Vk.DscSet.D sds slbts -> Word32 ->
	[(Vk.CmdBuf.C sc, Int32, Int32)] ->
	(forall sf . Vk.Fence.F sf -> IO c) -> IO c
runAll' ls dvc qu ppl plyt dscSet dsz cbs f = do
	repeatBeginEnd
		(writeAndRunBegin ls dvc qu ppl plyt dscSet dsz)
		(writeAndRun ls dvc qu ppl plyt dscSet dsz)
		(writeAndRunEnd' ls dvc qu ppl plyt dscSet dsz)
		cbs f

writeAndRunBegin :: (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]] ) =>
	Int -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[Int32, Int32]) ->
	Vk.Ppl.Lyt.P sl sbtss '[Int32, Int32] -> Vk.DscSet.D sds slbts -> Word32 ->
	(	Vk.CmdBuf.C sc, Int32, Int32 ) ->
	(forall ss' . Vk.Semaphore.S ss' -> IO b) -> IO b
writeAndRunBegin ls dvc qu ppl plyt dscSet dsz (cb, n, q) f =
	run' ls dvc qu cb ppl plyt dscSet dsz HPList.Nil n q f

writeAndRun :: (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]] ) =>
	Int -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[Int32, Int32]) ->
	Vk.Ppl.Lyt.P sl sbtss '[Int32, Int32] -> Vk.DscSet.D sds slbts -> Word32 ->
	Vk.Semaphore.S ss -> (Vk.CmdBuf.C sc, Int32, Int32) ->
	(forall ss' . Vk.Semaphore.S ss' -> IO b) -> IO b
writeAndRun ls dvc qu ppl plyt dscSet dsz s (cb, n, q) f =
	run' ls dvc qu cb ppl plyt dscSet dsz (HPList.Singleton s) n q f

writeAndRunEnd' :: (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]] ) =>
	Int -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[Int32, Int32]) ->
	Vk.Ppl.Lyt.P sl sbtss '[Int32, Int32] -> Vk.DscSet.D sds slbts -> Word32 ->
	Vk.Semaphore.S ss -> (Vk.CmdBuf.C sc, Int32, Int32) ->
	(forall sf . Vk.Fence.F sf -> IO b) -> IO b
writeAndRunEnd' ls dvc qu ppl plyt dscSet dsz s (cb, n, q) f = do
	runEnd' ls dvc qu cb ppl plyt dscSet dsz (HPList.Singleton s) n q f

repeatBeginEnd ::
	(forall b . a -> (forall ss . s ss -> IO b) -> IO b) ->
	(forall ss b . s ss -> a -> (forall ss' . s ss' -> IO b) -> IO b) ->
	(forall ss b . s ss -> a -> (forall st . t st -> IO b) -> IO b) -> [a] ->
	(forall st . t st -> IO c) -> IO c
repeatBeginEnd _ _ _ [] _ = error "bad"
repeatBeginEnd b m e (x : xs) f = b x \s -> repeatActions s xs m \s' x' -> e s' x' f

repeatActions :: Monad m =>
	s ss -> [a] -> (forall ss' b . s ss' -> a -> (forall ss'' . s ss'' -> m b) -> m b) -> (forall ss' . s ss' -> a -> m c) -> m c
repeatActions _ [] _ _ = error "bad"
repeatActions s [x] _ f = f s x
repeatActions s (x : xs) g f = g s x \s' -> repeatActions s' xs g f

pplLayoutInfo :: Vk.DscStLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout
			'[Int32, Int32]
			'[ 'Vk.PushConstant.Range
				'[ 'Vk.T.ShaderStageComputeBit] '[Int32, Int32]])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

commandBufferInfoList :: Word32 -> Vk.CmdPl.C s -> Vk.CmdBuf.AllocateInfoList 'Nothing s
commandBufferInfoList n cmdPool = Vk.CmdBuf.AllocateInfoList {
	Vk.CmdBuf.allocateInfoNextList = TMaybe.N,
	Vk.CmdBuf.allocateInfoCommandPoolList = cmdPool,
	Vk.CmdBuf.allocateInfoLevelList = Vk.CmdBuf.LevelPrimary,
	Vk.CmdBuf.allocateInfoCommandBufferCountList = n * (n + 1) `div` 2 }

run' :: forall slbts sbtss sd sc sg sl sds swss a . (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]],
	InfixIndex '[slbts] sbtss ) =>
	Int -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdBuf.C sc -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[Int32, Int32]) ->
	Vk.Ppl.Lyt.P sl sbtss '[Int32, Int32] -> Vk.DscSet.D sds slbts -> Word32 ->
	HPList.PL Vk.Semaphore.S swss -> Int32 -> Int32 ->
	(forall ss . Vk.Semaphore.S ss -> IO a) -> IO a
run' ls dvc qu cb ppl pplLyt dscSet dsz ws n q f = do
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb ->
			Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit ]
				ccb pplLyt (n :* q :* HPList.Nil) >>
			Vk.Cmd.bindDescriptorSetsCompute ccb
				pplLyt (HPList.Singleton $ U2 dscSet)
				(HPList.Singleton $ HPList.Singleton HPList.Nil ::
					HPList.PL3 Vk.Cmd.DynamicIndex (Vk.Cmd.LayoutArgListOnlyDynamics sbtss)) >>
--			Vk.Cmd.dispatch ccb dsz (2 ^ (7 :: Int)) 1
			Vk.Cmd.dispatch ccb (dsz `div` 64) (2 ^ (ls - 16 :: Int)) 1
	Vk.Semaphore.create dvc Vk.Semaphore.CreateInfo {
		Vk.Semaphore.createInfoNext = TMaybe.N,
		Vk.Semaphore.createInfoFlags = zeroBits } nil \s ->
		Vk.Fence.create dvc Vk.Fence.CreateInfo {
			Vk.Fence.createInfoNext = TMaybe.N,
			Vk.Fence.createInfoFlags = zeroBits } nil \fnc ->
			Vk.Q.submit qu (HPList.Singleton . U4 $ submitInfo ws s) (Just fnc) >> f s
	where
	submitInfo :: forall swss' ss .
		HPList.PL Vk.Semaphore.S swss' ->
		Vk.Semaphore.S ss -> Vk.SubmitInfo 'Nothing swss' '[sc] '[ss]
	submitInfo wss s = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks =
			HPList.map (`Vk.SemaphorePipelineStageFlags` Vk.Ppl.StageComputeShaderBit) wss,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Singleton s }

runEnd' :: forall slbts sbtss sd sc sg sl sds swss a . (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]],
	InfixIndex '[slbts] sbtss ) =>
	Int -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdBuf.C sc -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[Int32, Int32]) ->
	Vk.Ppl.Lyt.P sl sbtss '[Int32, Int32] -> Vk.DscSet.D sds slbts -> Word32 ->
	HPList.PL Vk.Semaphore.S swss -> Int32 -> Int32 ->
	(forall sf . Vk.Fence.F sf -> IO a) -> IO a
runEnd' ls dvc qu cb ppl pplLyt dscSet dsz ws n q f = do
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb ->
			Vk.Cmd.pushConstantsCompute @'[ 'Vk.T.ShaderStageComputeBit ]
				ccb pplLyt (n :* q :* HPList.Nil) >>
			Vk.Cmd.bindDescriptorSetsCompute ccb
				pplLyt (HPList.Singleton $ U2 dscSet)
				(HPList.Singleton $ HPList.Singleton HPList.Nil ::
					HPList.PL3 Vk.Cmd.DynamicIndex (Vk.Cmd.LayoutArgListOnlyDynamics sbtss)) >>
--			Vk.Cmd.dispatch ccb dsz (2 ^ (7 :: Int)) 1
			Vk.Cmd.dispatch ccb (dsz `div` 64) (2 ^ (ls - 16 :: Int)) 1
	Vk.Fence.create dvc Vk.Fence.CreateInfo {
		Vk.Fence.createInfoNext = TMaybe.N,
		Vk.Fence.createInfoFlags = zeroBits } nil \fnc ->
		Vk.Q.submit qu (HPList.Singleton . U4 $ submitInfo ws) (Just fnc) >> f fnc
	where
	submitInfo :: forall swss' .
		HPList.PL Vk.Semaphore.S swss' ->
		Vk.SubmitInfo 'Nothing swss' '[sc] '[]
	submitInfo wss = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks =
			HPList.map (`Vk.SemaphorePipelineStageFlags` Vk.Ppl.StageComputeShaderBit) wss,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

-- COMPUTE PIPELINE INFO

computePipelineInfo :: Vk.Ppl.Lyt.P sl sbtss '[Int32, Int32] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[])
		'(sl, sbtss, '[Int32, Int32]) sbph
computePipelineInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U5 shaderStageInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shaderStageInfo ::
	Vk.Ppl.ShaderSt.CreateInfo 'Nothing 'Nothing 'GlslComputeShader 'Nothing '[]
shaderStageInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlags = def,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (shdrMdInfo, nil),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Nothing }
	where shdrMdInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

[glslComputeShader|

#version 460

layout(local_size_x = 64) in;

layout(binding = 0) buffer Data {
	uint val[];
} data[1];

layout(push_constant) uniform Foo { int p; int q; } foo;

void
main()
{
	int i = int(gl_GlobalInvocationID.x) + (int(gl_GlobalInvocationID.y) << 15);

	int r = foo.p - foo.q;
	int u = i >> r << r;
	int l = i ^ u;

	int x = u << 1 | l;

	int f = x | i >> foo.q & 1 << r;
	int t = x | ~i >> foo.q & 1 << r;

	if (data[0].val[f] > data[0].val[t]) {
//		data[0].val[f] = atomicExchange(data[0].val[t], data[0].val[f]); };
		uint tmp = data[0].val[f];
		data[0].val[f] = data[0].val[t];
		data[0].val[t] = tmp; }
}

|]
