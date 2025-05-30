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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.Maybe
import Data.List qualified as L
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.Word

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.Instance qualified as Vk.Inst
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Pipeline.Compute qualified as Vk.Ppl.Cmpt
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShaderSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.PplLyt
import Gpu.Vulkan.PushConstant qualified as Vk.PshCnst
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderMod
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt

import Gpu.Vulkan.Sparse qualified as Vk.Sp
import Gpu.Vulkan.Sparse.Buffer qualified as Vk.Sp.Bffr

---------------------------------------------------------------------------

-- MAIN
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- MAIN

main :: IO ()
-- main = withDvc \pd dv q cp -> putStrLn . map (chr . fromIntegral) =<<
main = withDvc \pd dv q cp ->
	Vk.DscStLyt.create dv dscStLytInfo nil \(dsl :: DscStLyt sdsl nmh) ->
	createBffr pd dv \b1 (m1 :: Mm sm1 sr1 sb1 bnmh1 nmh1) ->
	prepareMem @_ @_ @nmh pd dv q dsl \dss b ->
	calc dv q cp dsl dss b b1 bffrSize >>
	(print =<< Vk.Mm.read
		@bnmh1 @(Word32List nmh1) @0 @[Word32] dv m1 zeroBits)

type DscStLyt sdsl nmh =
	Vk.DscStLyt.D sdsl '[Vk.DscStLyt.Buffer '[Word32List nmh]]

type Bffr sm sb bnmh nmh = Vk.Bffr.Binded sm sb bnmh '[Word32List nmh]

type Mm sm sr sb bnmh nmh = Vk.Mm.M sm '[
	'(sb, 'Vk.Mm.BufferArg bnmh '[Word32List nmh]) ]

-- type Word32List nmh = Obj.List 256 Word32 nmh
type Word32List nmh = Obj.List 32 Word32 nmh
-- type Word32List nmh = Obj.List 1 Word32 nmh

bffrSize :: Integral n => n
bffrSize = 30

withDvc :: (forall sd scp .
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp -> IO a) -> IO a
withDvc a = Vk.Inst.create instInfo nil \inst -> do
	pd <- head' <$> Vk.Phd.enumerate inst
	qfi <- fst . head' . filter (
			checkBits Vk.Q.ComputeBit .
			Vk.QFam.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	f <- Vk.Phd.getFeatures pd
	Vk.Dvc.create pd (dvcInfo qfi f) nil \dv ->
		Vk.Dvc.getQueue dv qfi 0 >>= \q ->
		Vk.CmdPl.create dv (cpinfo qfi) nil $ a pd dv q
	where cpinfo qfi = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.layerKhronosValidation] }

dvcInfo :: Vk.QFam.Index -> Vk.Phd.Features -> Vk.Dvc.CreateInfo 'Nothing '[ 'Nothing]
dvcInfo qfi f = Vk.Dvc.CreateInfo {
	Vk.Dvc.createInfoNext = TMaybe.N, Vk.Dvc.createInfoFlags = zeroBits,
	Vk.Dvc.createInfoQueueCreateInfos = HPList.Singleton qinfo,
	Vk.Dvc.createInfoEnabledLayerNames = [Vk.layerKhronosValidation],
	Vk.Dvc.createInfoEnabledExtensionNames = [],
	Vk.Dvc.createInfoEnabledFeatures = Just f }
	where qinfo = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }

dscStLytInfo :: Vk.DscStLyt.CreateInfo
	'Nothing '[ 'Vk.DscStLyt.Buffer '[Word32List nmh]]
dscStLytInfo = Vk.DscStLyt.CreateInfo {
	Vk.DscStLyt.createInfoNext = TMaybe.N,
	Vk.DscStLyt.createInfoFlags = zeroBits,
	Vk.DscStLyt.createInfoBindings = HPList.Singleton bdg }
	where bdg = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

-- PREPARE MEMORIES

prepareMem :: forall bts bnmh nmh sd sl a . (
	Vk.DscSt.BindingAndArrayElemBuffer bts '[Word32List nmh] 0,
	Vk.DscSt.UpdateDynamicLength bts '[Word32List nmh],
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.DscStLyt.D sl bts -> (forall sds sm sb .
		Vk.DscSt.D sds '(sl, bts) ->
		Vk.Bffr.Binded sm sb bnmh '[Word32List nmh] ->
		IO a) -> IO a
prepareMem pd dv q dsl f =
	Vk.DscPl.create dv dscPlInfo nil \dp ->
	Vk.DscSt.allocateDs dv (dscStInfo dp dsl) \(HPList.Singleton dss) ->
	createBffr' pd dv q \b ->
	Vk.DscSt.updateDs dv
		(HPList.Singleton . U5 $ writeDscSt @_ @nmh dss b) HPList.Nil >>
	f dss b

dscPlInfo :: Vk.DscPl.CreateInfo 'Nothing
dscPlInfo = Vk.DscPl.CreateInfo {
	Vk.DscPl.createInfoNext = TMaybe.N,
	Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
	Vk.DscPl.createInfoMaxSets = 1,
	Vk.DscPl.createInfoPoolSizes = (: []) Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPl.sizeDescriptorCount = 1 } }

dscStInfo :: Vk.DscPl.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscStInfo dpl dsl = Vk.DscSt.AllocateInfo {
	Vk.DscSt.allocateInfoNext = TMaybe.N,
	Vk.DscSt.allocateInfoDescriptorPool = dpl,
	Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }

createBffr :: forall sd sr bnm nm a . Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sb sm . Bffr sm sb bnm nm -> Mm sm sr sb bnm nm -> IO a) -> IO a
createBffr pd dv f =
	Vk.Bffr.create dv bffrInfo nil \bf -> mmInfo pd dv bf >>= \mmi ->
	Vk.Mm.allocateBind dv
		(U2 (Vk.Mm.Buffer bf) :** HPList.Nil) mmi nil
		\(U2 (Vk.Mm.BufferBinded bnd) :** HPList.Nil) mm -> f bnd mm

createBffr' :: forall sd bnm nm a . Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q ->
	(forall sb sm . Bffr sm sb bnm nm -> IO a) -> IO a
createBffr' pd dv q f =
	Vk.Bffr.create dv bffrInfo' nil \bf -> mmInfo pd dv bf >>= \mmi ->
	Vk.Mm.allocateBind dv
		(U2 (Vk.Mm.Raw 16 65536) :** HPList.Nil) mmi nil
		\(U2 (Vk.Mm.RawBinded _ _) :** HPList.Nil) mm ->
	Vk.Q.bindSparse
		dv q (HPList.Singleton . U6 $ bindSparseInfo bf mm) Nothing >>
	f (Vk.Bffr.unsafeToBinded bf)

bindSparseInfo :: Vk.Bffr.B sb bnm objs -> Vk.Mm.M sm ibargs ->
	Vk.Q.BindSparseInfo 'Nothing
		'[] '[ '(sb, bnm, objs, '[ '(sm, ibargs, 0)])] '[] '[] '[]
bindSparseInfo b mm = Vk.Q.BindSparseInfo {
	Vk.Q.bindSparseInfoNext = TMaybe.N,
	Vk.Q.bindSparseInfoWaitSemaphores = HPList.Nil,
	Vk.Q.bindSparseInfoBufferBinds =
		HPList.Singleton . U4 $ memoryBindInfo b mm,
	Vk.Q.bindSparseInfoImageOpaqueBinds = HPList.Nil,
	Vk.Q.bindSparseInfoImageBinds = HPList.Nil,
	Vk.Q.bindSparseInfoSignalSemaphores = HPList.Nil }

memoryBindInfo :: Vk.Bffr.B sb bnm objs -> Vk.Mm.M sm ibargs ->
	Vk.Sp.Bffr.MemoryBindInfo sb bnm objs '[ '(sm, ibargs, 0)]
memoryBindInfo b mm = Vk.Sp.Bffr.MemoryBindInfo {
	Vk.Sp.Bffr.memoryBindInfoBuffer = b,
	Vk.Sp.Bffr.memoryBindInfoBinds = HPList.Singleton . U3 $ memoryBind mm }

memoryBind :: Vk.Mm.M sm ibargs -> Vk.Sp.MemoryBind sm ibargs 0
memoryBind mm = Vk.Sp.MemoryBind {
	Vk.Sp.memoryBindResourceOffset = 0,
	Vk.Sp.memoryBindSize = 65536,
	Vk.Sp.memoryBindMemory = mm,
	Vk.Sp.memoryBindMemoryOffset = 0,
	Vk.Sp.memoryBindFlags = zeroBits }

bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[Word32List nmh]
bffrInfo = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N, Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton $ Obj.LengthList bffrSize,
	Vk.Bffr.createInfoUsage = -- Vk.Bffr.UsageStorageBufferBit,
		Vk.Bffr.UsageTransferDstBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

bffrInfo' :: Vk.Bffr.CreateInfo 'Nothing '[Word32List nmh]
bffrInfo' = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = Vk.Bffr.CreateSparseBindingBit,
	Vk.Bffr.createInfoLengths = HPList.Singleton $ Obj.LengthList bffrSize,
	Vk.Bffr.createInfoUsage =
		Vk.Bffr.UsageStorageBufferBit .|. Vk.Bffr.UsageTransferSrcBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

mmInfo :: Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.B sb nm objs -> IO (Vk.Mm.AllocateInfo 'Nothing)
mmInfo pd dv bf = do
	rqs <- Vk.Bffr.getMemoryRequirements dv bf
	print rqs
	mti <- findMmTpIdx pd rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mti }

findMmTpIdx :: Vk.Phd.P ->
	Vk.Mm.Requirements -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmTpIdx pd rqs wt = Vk.Phd.getMemoryProperties pd >>= \prs ->
	let	rqts = Vk.Mm.requirementsMemoryTypeBits rqs
		wtts = (fst <$>)
			. filter (checkBits wt . Vk.Mm.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes prs in
	case filter (`Vk.Mm.elemTypeIndex` rqts) wtts of
		[] -> error "No available memory types"; i : _ -> pure i

writeDscSt :: forall bnmh nmh sds slbts sm sb os . (
	Show (HPList.PL Obj.Length os),
	Obj.OffsetRange (Word32List nmh) os 0 ) =>
	Vk.DscSt.D sds slbts -> Vk.Bffr.Binded sm sb bnmh os ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer
		'[ '(sm, sb, bnmh, Word32List nmh, 0)]) 0
writeDscSt ds bf = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources =
		Vk.DscSt.BufferInfos . HPList.Singleton . U5 $ Vk.Dsc.BufferInfo bf }

-- CALC

calc :: forall sd scpl sds slbts sdsl bts sm0 sb0 bnm0 sm sb bnm1 nm1 . (
	slbts ~ '(sdsl, bts),
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]] ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	Vk.DscStLyt.D sdsl bts -> Vk.DscSt.D sds slbts ->
	Vk.Bffr.Binded sm0 sb0 bnm0 '[Word32List nm1] ->
	Vk.Bffr.Binded sm sb bnm1 '[Word32List nm1] -> Word32 -> IO ()
calc dv q cpl dsl dss b b1 sz =
	Vk.Q.bindSparse dv q HPList.Nil Nothing >>
	Vk.PplLyt.create dv (pplLytInfo dsl) nil \pl ->
	Vk.Ppl.Cmpt.createCs dv Nothing (HPList.Singleton . U4 $ pplInfo pl)
		nil \(cppl :** HPList.Nil) ->
	Vk.CBffr.allocateCs dv (cmdBffrInfo cpl) \(cb :*. HPList.Nil) ->
	run q cb pl cppl dss b b1 sz

pplLytInfo :: Vk.DscStLyt.D sl bts -> Vk.PplLyt.CreateInfo
	'Nothing '[ '(sl, bts)] ('Vk.PshCnst.Layout '[] '[])
pplLytInfo dsl = Vk.PplLyt.CreateInfo {
	Vk.PplLyt.createInfoNext = TMaybe.N,
	Vk.PplLyt.createInfoFlags = zeroBits,
	Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

cmdBffrInfo :: Vk.CmdPl.C s -> Vk.CBffr.AllocateInfo 'Nothing s '[ '()]
cmdBffrInfo cpl = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cpl,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

run :: forall slbts sc spl sg sds sm0 sb0 bnm0 sm1 sb1 bnm1 nm1 .
	(Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]]) =>
	Vk.Q.Q -> Vk.CBffr.C sc -> Vk.PplLyt.P spl '[slbts] '[] ->
	Vk.Ppl.Cmpt.C sg '(spl, '[slbts], '[]) ->
	Vk.DscSt.D sds slbts ->
	Vk.Bffr.Binded sm0 sb0 bnm0 '[Word32List nm1] ->
	Vk.Bffr.Binded sm1 sb1 bnm1 '[Word32List nm1] ->
	Word32 -> IO ()
run q cb pl cppl dss b b1 sz = do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute cppl \ccb ->
		Vk.Cmd.bindDescriptorSetsCompute
			ccb pl (HPList.Singleton $ U2 dss) def >>
		Vk.Cmd.dispatch ccb sz 1 1 >>
		Vk.Cmd.copyBuffer @'[ '( '[Word32List nm1], 0, 0)] cb b b1
	Vk.Q.submit q (HPList.Singleton $ U4 sinfo) Nothing
	Vk.Q.waitIdle q
	where sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

-- COMPUTE PIPELINE INFO

pplInfo :: Vk.PplLyt.P sl sbtss '[] -> Vk.Ppl.Cmpt.CreateInfo 'Nothing
	'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[])
	'(sl, sbtss, '[]) sbph
pplInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U5 shdrStInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shdrStInfo :: Vk.Ppl.ShaderSt.CreateInfo
	'Nothing 'Nothing 'GlslComputeShader 'Nothing '[]
shdrStInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (mdinfo, nil),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Nothing }
	where mdinfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

head' :: [a] -> a
head' = fst . fromJust . L.uncons

[glslComputeShader|

#version 460

layout(binding = 0) buffer Data { uint val[]; };

uint hello[] = uint[](
	72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 );

void
main()
{
	int i = int(gl_GlobalInvocationID.x);
	val[i] = hello[i];
}

|]
