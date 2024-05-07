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
import Data.TypeLevel.List
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.Word
import Data.Char

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
import Gpu.Vulkan.PipelineLayout qualified as Vk.Ppl.Lyt
import Gpu.Vulkan.PushConstant qualified as Vk.PushConstant
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderMod
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPool
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt

---------------------------------------------------------------------------

-- MAIN
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- MAIN

main :: IO ()
main = withDvc \pd dv q cpl -> putStrLn . toString =<<
	Vk.DscStLyt.create dv dscStLytInfo nil \(dsl :: DscStLyt sdsl nmh) ->
	prepareMem @_ @nmh pd dv dsl \dss _ (m :: Mm sm sb bnmh nmh) ->
	calc dv q cpl dsl dss bffrSize >>
	Vk.Mm.read @bnmh @(Word32List nmh) @0 @[Word32] dv m zeroBits

toString :: [Word32] -> String
toString = map (chr . fromIntegral)

type DscStLyt sdsl nmh = Vk.DscStLyt.D sdsl '[Vk.DscStLyt.Buffer '[Word32List nmh]]

type Bffr sm sb bnmh nmh = Vk.Bffr.Binded sm sb bnmh (BffrContents nmh)

type Mm sm sb bnmh nmh =
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg bnmh (BffrContents nmh))]

type BffrContents nmh = '[Word32List nmh]

bffrSize :: Integral n => n
bffrSize = 30

type Word32List nmh = Obj.List 256 Word32 nmh

withDvc :: (forall sd scpl .
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl -> IO a) -> IO a
withDvc f = Vk.Inst.create instInfo nil \inst -> do
	pd <- head <$> Vk.Phd.enumerate inst
	qfi <- fst . head . filter (
			checkBits Vk.Q.ComputeBit .
			Vk.QFam.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	Vk.Dvc.create pd (dvcInfo qfi) nil \dv ->
		Vk.Dvc.getQueue dv qfi 0 >>= \q ->
		Vk.CmdPl.create dv (cmdPlInfo qfi) nil \cpl -> f pd dv q cpl

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.layerKhronosValidation] }
	
dvcInfo :: Vk.QFam.Index -> Vk.Dvc.CreateInfo 'Nothing '[ 'Nothing]
dvcInfo qfi = Vk.Dvc.CreateInfo {
	Vk.Dvc.createInfoNext = TMaybe.N, Vk.Dvc.createInfoFlags = zeroBits,
	Vk.Dvc.createInfoQueueCreateInfos = HPList.Singleton qinfo,
	Vk.Dvc.createInfoEnabledLayerNames = [Vk.layerKhronosValidation],
	Vk.Dvc.createInfoEnabledExtensionNames = [],
	Vk.Dvc.createInfoEnabledFeatures = Nothing }
	where qinfo = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }

dscStLytInfo :: Vk.DscStLyt.CreateInfo 'Nothing '[ 'Vk.DscStLyt.Buffer '[Word32List nmh]]
dscStLytInfo = Vk.DscStLyt.CreateInfo {
	Vk.DscStLyt.createInfoNext = TMaybe.N, Vk.DscStLyt.createInfoFlags = zeroBits,
	Vk.DscStLyt.createInfoBindings = HPList.Singleton bdg }
	where bdg = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

-- PREPARE MEMORIES

prepareMem :: forall bts bnmh nmh sd sl a . (
	Default (HPList.PL (HPList.PL BObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DscSt.BindingAndArrayElemBuffer bts '[Word32List nmh] 0,
	Vk.DscSt.UpdateDynamicLength bts '[Word32List nmh] ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts -> (forall sds sm sb .
		Vk.DscSt.D sds '(sl, bts) ->
		Bffr sm sb bnmh nmh -> Mm sm sb bnmh nmh -> IO a) -> IO a
prepareMem pd dv dsl f =
	Vk.DscPool.create dv dscPlInfo nil \dp ->
	Vk.DscSt.allocateDs dv (dscStInfo dp dsl) \(HPList.Singleton ds) ->
	createBffr pd dv \b m ->
	Vk.DscSt.updateDs dv (HPList.Singleton . U5 $ writeDscSt @_ @nmh ds b) HPList.Nil >>
	f ds b m

dscPlInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPlInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = (: []) Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 1 } }

dscStInfo :: Vk.DscPool.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscStInfo pl lyt = Vk.DscSt.AllocateInfo {
	Vk.DscSt.allocateInfoNext = TMaybe.N,
	Vk.DscSt.allocateInfoDescriptorPool = pl,
	Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 lyt }

createBffr :: forall sd nm nmh a . Vk.Phd.P -> Vk.Dvc.D sd ->
	(forall sb sm . Bffr sm sb nm nmh -> Mm sm sb nm nmh -> IO a) -> IO a
createBffr pd dv f =
	Vk.Bffr.create dv bffrInfo nil \bf -> mmInfo pd dv bf >>= \mmi ->
	Vk.Mm.allocateBind dv
		(HPList.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil
		\(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	f bnd mm

bffrInfo :: Vk.Bffr.CreateInfo 'Nothing (BffrContents nmh)
bffrInfo = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton $ Obj.LengthList bffrSize,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

mmInfo :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.B sb nm objs ->
	IO (Vk.Mm.AllocateInfo 'Nothing)
mmInfo pd dv bf = do
	rqs <- Vk.Bffr.getMemoryRequirements dv bf
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
		[] -> error "No available memory types"
		i : _ -> pure i

writeDscSt :: forall bnmh nmh sds slbts sm sb os . (
	Show (HPList.PL Obj.Length os),
	Obj.OffsetRange' (Word32List nmh) os 0 ) =>
	Vk.DscSt.D sds slbts -> Vk.Bffr.Binded sm sb bnmh os ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer
		'[ '(sm, sb, bnmh, Word32List nmh, 0)]) 0
writeDscSt ds bf = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources =
		Vk.DscSt.BufferInfos . HPList.Singleton . U5 $ Vk.Dsc.BufferInfo bf }

-- CALC

calc :: forall slbts sl bts sd scpl sds . (
	slbts ~ '(sl, bts),
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	InfixIndex '[slbts] '[slbts] ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	Vk.DscStLyt.D sl bts -> Vk.DscSt.D sds slbts -> Word32 -> IO ()
calc dv q cpl dsl ds sz =
	Vk.Ppl.Lyt.create dv (pplLytInfo dsl) nil \pl ->
	Vk.Ppl.Cmpt.createCs dv Nothing
		(HPList.Singleton . U4 $ pplInfo pl) nil \(cppl :** HPList.Nil) ->
	Vk.CBffr.allocate dv (cmdBffrInfo cpl) \(cb :*. HPList.Nil) ->
	run q ds cb pl cppl sz

pplLytInfo :: Vk.DscStLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLytInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

cmdPlInfo :: Vk.QFam.Index -> Vk.CmdPl.CreateInfo 'Nothing
cmdPlInfo qfi = Vk.CmdPl.CreateInfo {
	Vk.CmdPl.createInfoNext = TMaybe.N,
	Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
	Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

cmdBffrInfo :: Vk.CmdPl.C s -> Vk.CBffr.AllocateInfo 'Nothing s '[ '()]
cmdBffrInfo cpl = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cpl,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

run :: forall slbts sd sc sg sl s . (
	Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]],
	InfixIndex '[slbts] '[slbts] ) =>
	Vk.Q.Q -> Vk.DscSt.D s slbts -> Vk.CBffr.C sc ->
	Vk.Ppl.Lyt.P sl '[slbts] '[] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[]) -> Word32 -> IO ()
run q ds cb pl cppl sz = do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute cppl \ccb ->
		Vk.Cmd.bindDescriptorSetsCompute
			ccb pl (HPList.Singleton $ U2 ds) def >>
		Vk.Cmd.dispatch ccb sz 1 1
	Vk.Q.submit q (HPList.Singleton $ U4 sinfo) Nothing
	Vk.Q.waitIdle q
	where
	sinfo :: Vk.SubmitInfo 'Nothing '[] '[sc] '[]
	sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

-- COMPUTE PIPELINE INFO

pplInfo :: Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.Ppl.Cmpt.CreateInfo 'Nothing
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
