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

import qualified Gpu.Vulkan.Memory as Vk.Mem

import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as KObj
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import qualified Data.HeteroParList as HL
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word
import Data.Char

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFm
import qualified Gpu.Vulkan.Device as Vk.Dv
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPool
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Memory as Vk.Mm.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DS
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscStLyt

import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

---------------------------------------------------------------------------

-- MAIN
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- MAIN

main :: IO ()
main = withDvc \pd dv q cp -> putStrLn . map (chr . fromIntegral) =<<
	Vk.DscStLyt.create dv dscStLytInfo nil \(dsl :: DscStLyt sdsl nmh) ->
	prepareMems pd dv dsl \dss m ->
	calc dv q cp dsl dss bffrSize >>
	Vk.Mm.read @"" @(Word32List "") @0 @[Word32] dv m zeroBits

type DscStLyt sdsl nmh =
	Vk.DscStLyt.D sdsl '[Vk.DscStLyt.Buffer '[Word32List nmh]]

type Word32List nmh = Obj.List 256 Word32 nmh

bffrSize :: Integral n => n
bffrSize = 30

withDvc :: (forall sd sc .
	Vk.Phd.P -> Vk.Dv.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> IO a) -> IO a
withDvc f = withDevice \pd qfi dv ->
	Vk.Dv.getQueue dv qfi 0 >>= \q ->
	Vk.CmdPool.create dv (commandPoolInfo qfi) nil \cp ->
	f pd dv q cp

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

dscStLytInfo :: Vk.DscStLyt.CreateInfo 'Nothing '[ 'Vk.DscStLyt.Buffer '[Word32List ""]]
dscStLytInfo = Vk.DscStLyt.CreateInfo {
	Vk.DscStLyt.createInfoNext = TMaybe.N, Vk.DscStLyt.createInfoFlags = zeroBits,
	Vk.DscStLyt.createInfoBindings = HL.Singleton bdg }
	where bdg = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)

-- PREPARE MEMORIES

prepareMems :: (
	Default (HL.PL (HL.PL KObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DS.BindingAndArrayElemBuffer bts '[Word32List ""] 0,
	Vk.DS.UpdateDynamicLength bts '[Word32List ""] ) =>
	Vk.Phd.P -> Vk.Dv.D sd -> Vk.DscStLyt.D sl bts ->
	(forall sds sm sb .
		Vk.DS.D sds '(sl, bts) ->
		Vk.Mm.M sm '[ '( sb, 'Vk.Mm.BufferArg "" '[Word32List ""])] ->
		IO a) -> IO a
prepareMems pd dv dslyt f =
	Vk.DscPool.create dv dscPoolInfo nil \dp ->
	Vk.DS.allocateDs dv (dscSetInfo dp dslyt) \(HL.Singleton ds) ->
	storageBufferNew pd dv \b m ->
	Vk.DS.updateDs dv (HL.Singleton . U5 $ writeDscSet ds b) HL.Nil >>
	f ds m

dscPoolInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [sz] }
	where sz = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DS.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DS.AllocateInfo {
	Vk.DS.allocateInfoNext = TMaybe.N,
	Vk.DS.allocateInfoDescriptorPool = pl,
	Vk.DS.allocateInfoSetLayouts = HL.Singleton $ U2 lyt }

storageBufferNew :: forall sd nm a . Vk.Phd.P -> Vk.Dv.D sd -> (forall sb sm .
	Vk.Bffr.Binded sm sb nm '[Word32List ""]  ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[Word32List ""])] -> IO a) -> IO a
storageBufferNew pd dv f =
	Vk.Bffr.create dv bufferInfo nil \bf ->
	getMemoryInfo pd dv bf >>= \mmi ->
	Vk.Mm.allocateBind dv
		(HL.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil
		\(HL.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	f bnd mm

bufferInfo :: Vk.Bffr.CreateInfo 'Nothing '[Word32List ""]
bufferInfo = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HL.Singleton $ Obj.LengthList bffrSize,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.Phd.P -> Vk.Dv.D sd -> Vk.Bffr.B sb nm objs ->
	IO (Vk.Mem.AllocateInfo 'Nothing)
getMemoryInfo pd dv bff = do
	rqs <- Vk.Bffr.getMemoryRequirements dv bff
	mti <- findMemoryTypeIndex pd rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mti }

findMemoryTypeIndex :: Vk.Phd.P ->
	Vk.Mm.M.Requirements -> Vk.Mm.PropertyFlags -> IO Vk.Mm.M.TypeIndex
findMemoryTypeIndex pd rqs prp0 = Vk.Phd.getMemoryProperties pd >>= \prps ->
	let	rqts = Vk.Mm.M.requirementsMemoryTypeBits rqs
		prpts = (fst <$>)
			. filter (checkBits prp0
				. Vk.Mm.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes prps in
	case filter (`Vk.Mm.M.elemTypeIndex` rqts) prpts of
		[] -> error "No available memory types"
		i : _ -> pure i

writeDscSet :: forall s slbts sb sm os . (
	Show (HL.PL Obj.Length os),
	Obj.OffsetRange' (Obj.List 256 Word32 "") os 0
	) =>
	Vk.DS.D s slbts -> Vk.Bffr.Binded sm sb "" os ->
	Vk.DS.Write 'Nothing s slbts
		('Vk.DS.WriteSourcesArgBuffer '[ '(sm, sb, "", Word32List "", 0)]) 0
writeDscSet ds ba = Vk.DS.Write {
	Vk.DS.writeNext = TMaybe.N,
	Vk.DS.writeDstSet = ds,
	Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DS.writeSources =
		Vk.DS.BufferInfos . HL.Singleton . U5 $ Vk.Dsc.BufferInfo ba }

-- CALC

calc :: forall sc slbts sl bts sd s . (
	slbts ~ '(sl, bts),
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]] ) =>
	Vk.Dv.D sd -> Vk.Queue.Q -> Vk.CmdPool.C sc -> Vk.DscStLyt.D sl bts ->
	Vk.DS.D s slbts -> Word32 -> IO ()
calc dv q cp dslyt ds sz =
	Vk.Ppl.Lyt.create dv (pplLayoutInfo dslyt) nil \plyt ->
	Vk.Ppl.Cmpt.createCs dv Nothing
		(HL.Singleton . U4 $ pplInfo plyt) nil \(pl :** HL.Nil) ->
	Vk.CBffr.allocate dv (commandBufferInfo cp) \(cb :*. HL.Nil) ->
	run q ds cb plyt pl sz

pplLayoutInfo :: Vk.DscStLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HL.Singleton $ U2 dsl }

commandPoolInfo :: Vk.QFm.Index -> Vk.CmdPool.CreateInfo 'Nothing
commandPoolInfo qfi = Vk.CmdPool.CreateInfo {
	Vk.CmdPool.createInfoNext = TMaybe.N,
	Vk.CmdPool.createInfoFlags = Vk.CmdPool.CreateResetCommandBufferBit,
	Vk.CmdPool.createInfoQueueFamilyIndex = qfi }

commandBufferInfo :: Vk.CmdPool.C s -> Vk.CBffr.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cmdPool = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cmdPool,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

run :: forall slbts sc sg sl s . (
	Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]] ) =>
	Vk.Queue.Q -> Vk.DS.D s slbts -> Vk.CBffr.C sc ->
	Vk.Ppl.Lyt.P sl '[slbts] '[] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[]) -> Word32 -> IO ()
run q ds cb lyt pl sz = do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute pl \ccb ->
		Vk.Cmd.bindDescriptorSetsCompute
			ccb lyt (HL.Singleton $ U2 ds) def >>
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

pplInfo :: Vk.Ppl.Lyt.P sl sbtss '[] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing '( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[])
		'(sl, sbtss, '[]) sbph
pplInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U5 shaderStInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shaderStInfo :: Vk.Ppl.ShaderSt.CreateInfo 'Nothing 'Nothing 'GlslComputeShader 'Nothing '[]
shaderStInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
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

layout(binding = 0) buffer Data { uint val[]; };

uint hello[] = uint[](
	72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 );

void
main()
{
	int index = int(gl_GlobalInvocationID.x);
	val[index] = hello[index];
}

|]
