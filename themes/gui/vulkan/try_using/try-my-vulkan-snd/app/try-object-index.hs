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

import Control.Arrow
import Data.TypeLevel.ParMaybe (nil)
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList qualified as HL
import Data.Word
import Data.Char

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.Instance qualified as Vk.Inst
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Queue
import Gpu.Vulkan.QueueFamily qualified as Vk.QFm
import Gpu.Vulkan.Device qualified as Vk.Dv
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShaderSt
import Gpu.Vulkan.Pipeline.Compute qualified as Vk.Ppl.Cmpt
import Gpu.Vulkan.PipelineLayout qualified as Vk.Ppl.Lyt
import Gpu.Vulkan.PushConstant qualified as Vk.PushConstant
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderMod
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPool
import Gpu.Vulkan.DescriptorSet qualified as Vk.DS
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DSLyt

---------------------------------------------------------------------------

-- MAIN
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- MAIN

main :: IO ()
main = withDevice \pd qfi dv -> print . toString =<< -- print . (toString *** toString) =<<
	Vk.DSLyt.create dv dscStLytInfo nil \(dsl :: DscStLyt sdsl nmh) ->
	prepareMem @_ @nmh pd dv dsl \dss (m :: Mm sm sb bnmh nmh) ->
	calc qfi dv dsl dss bffrSize >> -- (,)
	Vk.Mm.read @bnmh @(Word32List nmh) @0 @[Word32] dv m zeroBits
--	<$> Vk.Mm.read @bnmh @(Word32List nmh) @0 @[Word32] dv m zeroBits
--	<*> Vk.Mm.read @bnmh @(Word32List nmh) @1 @[Word32] dv m zeroBits

toString :: [Word32] -> String
toString = map (chr . fromIntegral)

type DscStLyt sdsl nmh = Vk.DSLyt.D sdsl '[Vk.DSLyt.Buffer '[Word32List nmh, Word32List nmh]]

type Bffr sm sb nm nmh = Vk.Bffr.Binded sm sb nm '[Word32List nmh, Word32List nmh]

type Mm sm sb bnmh nmh =
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg bnmh '[Word32List nmh, Word32List nmh])]

bffrSize :: Integral n => n
bffrSize = 30

type Word32List nmh = Obj.List 256 Word32 nmh

withDevice :: (forall s . Vk.Phd.P -> Vk.QFm.Index -> Vk.Dv.D s -> IO a) -> IO a
withDevice f = Vk.Inst.create instInfo nil \inst -> do
	pd <- head <$> Vk.Phd.enumerate inst
	qfi <- fst . head . filter (
			checkBits Vk.Queue.ComputeBit .
			Vk.QFm.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	Vk.Dv.create pd (dvcInfo qfi) nil $ f pd qfi

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.layerKhronosValidation] }
	
dvcInfo :: Vk.QFm.Index -> Vk.Dv.CreateInfo 'Nothing '[ 'Nothing]
dvcInfo qfi = Vk.Dv.CreateInfo {
	Vk.Dv.createInfoNext = TMaybe.N, Vk.Dv.createInfoFlags = zeroBits,
	Vk.Dv.createInfoQueueCreateInfos = HL.Singleton qinfo,
	Vk.Dv.createInfoEnabledLayerNames = [Vk.layerKhronosValidation],
	Vk.Dv.createInfoEnabledExtensionNames = [],
	Vk.Dv.createInfoEnabledFeatures = Nothing }
	where qinfo = Vk.Dv.QueueCreateInfo {
		Vk.Dv.queueCreateInfoNext = TMaybe.N,
		Vk.Dv.queueCreateInfoFlags = zeroBits,
		Vk.Dv.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dv.queueCreateInfoQueuePriorities = [0] }

dscStLytInfo :: Vk.DSLyt.CreateInfo 'Nothing '[ 'Vk.DSLyt.Buffer '[Word32List nmh, Word32List nmh]]
dscStLytInfo = Vk.DSLyt.CreateInfo {
	Vk.DSLyt.createInfoNext = TMaybe.N, Vk.DSLyt.createInfoFlags = zeroBits,
	Vk.DSLyt.createInfoBindings = HL.Singleton bdg }
	where bdg = Vk.DSLyt.BindingBuffer {
		Vk.DSLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
		Vk.DSLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

-- PREPARE MEMORIES

prepareMem :: forall bts bnmh nmh sd sl a . (
	Default (HL.PL (HL.PL BObj.Length)
		(Vk.DSLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DS.BindingAndArrayElemBuffer bts '[Word32List nmh] 0,
	Vk.DS.UpdateDynamicLength bts '[Word32List nmh] ) =>
	Vk.Phd.P -> Vk.Dv.D sd -> Vk.DSLyt.D sl bts -> (forall sds sm sb .
		Vk.DS.D sds '(sl, bts) -> Mm sm sb bnmh nmh -> IO a) -> IO a
prepareMem pd dv dsl f =
	Vk.DscPool.create dv dscPlInfo nil \dp ->
	Vk.DS.allocateDs dv (dscStInfo dp dsl) \(HL.Singleton ds) ->
	createBffr pd dv \b m ->
	Vk.DS.updateDs dv (HL.Singleton . U5 $ writeDscSt @_ @nmh ds b) HL.Nil >>
	f ds m

dscPlInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPlInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = (: []) Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 2 } }

dscStInfo :: Vk.DscPool.P sp -> Vk.DSLyt.D sl bts ->
	Vk.DS.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscStInfo pl lyt = Vk.DS.AllocateInfo {
	Vk.DS.allocateInfoNext = TMaybe.N,
	Vk.DS.allocateInfoDescriptorPool = pl,
	Vk.DS.allocateInfoSetLayouts = HL.Singleton $ U2 lyt }

createBffr :: forall sd nm nmh a . Vk.Phd.P -> Vk.Dv.D sd ->
	(forall sb sm . Bffr sm sb nm nmh -> Mm sm sb nm nmh -> IO a) -> IO a
createBffr pd dv f =
	Vk.Bffr.create dv bffrInfo nil \bf -> mmInfo pd dv bf >>= \mmi ->
	Vk.Mm.allocateBind dv
		(HL.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil
		\(HL.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	f bnd mm

bffrInfo :: Vk.Bffr.CreateInfo 'Nothing '[Word32List nmh, Word32List nmh]
bffrInfo = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = Obj.LengthList bffrSize :** Obj.LengthList bffrSize :** HL.Nil,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

mmInfo :: Vk.Phd.P -> Vk.Dv.D sd -> Vk.Bffr.B sb nm objs ->
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

writeDscSt :: forall bnmh nmh sds slbts sm sb os .
	(Show (HL.PL Obj.Length os), Obj.OffsetRange (Word32List nmh) os) =>
	Vk.DS.D sds slbts -> Vk.Bffr.Binded sm sb bnmh os ->
	Vk.DS.Write 'Nothing sds slbts ('Vk.DS.WriteSourcesArgBuffer
		'[ '(sm, sb, bnmh, Word32List nmh)]) 0
writeDscSt ds bf = Vk.DS.Write {
	Vk.DS.writeNext = TMaybe.N, Vk.DS.writeDstSet = ds,
	Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DS.writeSources =
		Vk.DS.BufferInfos . HL.Singleton . U4 $ Vk.Dsc.BufferInfo bf }

-- CALC

calc :: forall slbts sl bts sd sds . (
	slbts ~ '(sl, bts),
	Vk.DSLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	InfixIndex '[slbts] '[slbts] ) =>
	Vk.QFm.Index -> Vk.Dv.D sd ->
	Vk.DSLyt.D sl bts -> Vk.DS.D sds slbts -> Word32 -> IO ()
calc qfi dv dsl ds sz =
	Vk.Ppl.Lyt.create dv (pplLytInfo dsl) nil \pl ->
	Vk.Ppl.Cmpt.createCs dv Nothing
		(HL.Singleton . U4 $ pplInfo pl) nil \(cppl :** HL.Nil) ->
	Vk.CmdPl.create dv (cmdPlInfo qfi) nil \cpl ->
	Vk.CBffr.allocate dv (cmdBffrInfo cpl) \(cb :*. HL.Nil) ->
	run qfi dv ds cb pl cppl sz

pplLytInfo :: Vk.DSLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLytInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HL.Singleton $ U2 dsl }

cmdPlInfo :: Vk.QFm.Index -> Vk.CmdPl.CreateInfo 'Nothing
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
	Vk.QFm.Index -> Vk.Dv.D sd -> Vk.DS.D s slbts -> Vk.CBffr.C sc ->
	Vk.Ppl.Lyt.P sl '[slbts] '[] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[]) -> Word32 -> IO ()
run qfi dv ds cb pl cppl sz = Vk.Dv.getQueue dv qfi 0 >>= \q -> do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute cppl \ccb ->
		Vk.Cmd.bindDescriptorSetsCompute
			ccb pl (HL.Singleton $ U2 ds) def >>
		Vk.Cmd.dispatch ccb sz 1 1
	Vk.Queue.submit q (HL.Singleton $ U4 sinfo) Nothing
	Vk.Queue.waitIdle q
	where
	sinfo :: Vk.SubmitInfo 'Nothing '[] '[sc] '[]
	sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HL.Nil,
		Vk.submitInfoCommandBuffers = HL.Singleton cb,
		Vk.submitInfoSignalSemaphores = HL.Nil }

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
