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

module Main where

import qualified Gpu.Vulkan.Memory as Vk.Mem

import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as BObj
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List
import qualified Data.HeteroParList as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word
import Data.Char

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind
import Data.TypeLevel.ParMaybe (nil)

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
main = withDvc \pd dv q cpl -> putStrLn . toString =<<
	Vk.DscStLyt.create dv dscStLytInfo nil \(dsl :: DscStLyt sdsl nmh) ->
	prepareMem @_ @_ @nmh pd dv dsl \_dss dss' (m :: Mm sm sb bnmh nmh) ->
	calc dv q cpl dsl dss' bffrSize >>
	Vk.Mm.read @bnmh @(Word32List nmh) @0 @[Word32] dv m zeroBits

toString :: [Word32] -> String
toString = map (chr . fromIntegral)

type DscStLyt sdsl nmh = Vk.DscStLyt.D sdsl '[Vk.DscStLyt.Buffer '[Word32List nmh]]

type Mm sm sb bnmh nmh =
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg bnmh '[Word32List nmh])]

type Word32List nmh = Obj.List 256 Word32 nmh

bffrSize :: Integral n => n
bffrSize = 30

withDvc :: (forall sd scpl .
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl -> IO a) -> IO a
withDvc a = Vk.Inst.create instInfo nil \inst -> do
	pd <- head' <$> Vk.Phd.enumerate inst
	qfi <- fst . head' . filter (
			checkBits Vk.Q.ComputeBit .
			Vk.QFam.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	Vk.Dvc.create pd (dvcInfo qfi) nil \dv ->
		Vk.Dvc.getQueue dv qfi 0 >>= \q ->
		Vk.CmdPl.create dv (cpinfo qfi) nil \cpl -> a pd dv q cpl
	where
	cpinfo qfi = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

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
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DS.BindingAndArrayElemBuffer bts '[Word32List nmh] 0,
	Vk.DS.UpdateDynamicLength bts '[Word32List nmh] ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	(forall sds sm sb .
		Vk.DS.D sds '(sl, bts) ->
		Vk.DS.D sds '(sl, bts) ->
		Vk.Mm.M sm '[ '( sb, 'Vk.Mm.BufferArg "" '[Word32List nmh])] ->
		IO a) -> IO a
prepareMem pd dv dslyt f =
	Vk.DscPool.create dv dscPoolInfo nil \dp ->
	Vk.DS.allocateDs dv (dscSetInfo dp dslyt) \(ds :** ds' :** HPList.Nil) ->
	storageBufferNew pd dv \b m ->
	Vk.DS.updateDs dv
		(HPList.Singleton . U5 $ writeDscSet @nmh ds b)
		(HPList.Singleton . U8 $ copyDscSet @nmh ds ds') >>
	f ds ds' m

dscPoolInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 2,
	Vk.DscPool.createInfoPoolSizes = [sz] }
	where sz = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DS.AllocateInfo 'Nothing sp '[ '(sl, bts), '(sl, bts)]
dscSetInfo pl lyt = Vk.DS.AllocateInfo {
	Vk.DS.allocateInfoNext = TMaybe.N,
	Vk.DS.allocateInfoDescriptorPool = pl,
	Vk.DS.allocateInfoSetLayouts = U2 lyt :** U2 lyt :** HPList.Nil }

storageBufferNew :: forall sd nm a nmh . Vk.Phd.P -> Vk.Dvc.D sd -> (forall sb sm .
	Vk.Bffr.Binded sm sb nm '[Word32List nmh]  ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[Word32List nmh])] -> IO a) -> IO a
storageBufferNew pd dv f =
	Vk.Bffr.create dv bufferInfo nil \bf ->
	getMemoryInfo pd dv bf >>= \mmi ->
	Vk.Mm.allocateBind dv
		(HPList.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil
		\(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	f bnd mm

bufferInfo :: Vk.Bffr.CreateInfo 'Nothing '[Word32List nmh]
bufferInfo = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HPList.Singleton $ Obj.LengthList bffrSize,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Bffr.B sb nm objs ->
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
				. Vk.Mm.M.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes prps in
	case filter (`Vk.Mm.M.elemTypeIndex` rqts) prpts of
		[] -> error "No available memory types"
		i : _ -> pure i

writeDscSet :: forall nmh slbts sb sm os sds . (
	Show (HPList.PL Obj.Length os),
	Obj.OffsetRange (Obj.List 256 Word32 nmh) os 0 ) =>
	Vk.DS.D sds slbts -> Vk.Bffr.Binded sm sb "" os ->
	Vk.DS.Write 'Nothing sds slbts
		('Vk.DS.WriteSourcesArgBuffer '[ '(sm, sb, "", Word32List nmh, 0)]) 0
writeDscSet ds ba = Vk.DS.Write {
	Vk.DS.writeNext = TMaybe.N,
	Vk.DS.writeDstSet = ds,
	Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DS.writeSources =
		Vk.DS.BufferInfos . HPList.Singleton . U5 $ Vk.Dsc.BufferInfo ba }

copyDscSet :: forall nmh sds slbts sds' . Vk.DS.D sds slbts -> Vk.DS.D sds' slbts -> Vk.DS.Copy
	'Nothing sds slbts 0 sds' slbts 0 (Vk.DscStLyt.Buffer '[Word32List nmh])
copyDscSet s d = Vk.DS.Copy {
	Vk.DS.copyNext = TMaybe.N, Vk.DS.copySrcSet = s, Vk.DS.copyDstSet = d }

-- CALC

calc :: forall slbts sl bts sd sds scpl . (
	slbts ~ '(sl, bts),
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	InfixIndex '[slbts] '[slbts]) =>
	Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C scpl ->
	Vk.DscStLyt.D sl bts ->
	Vk.DS.D sds slbts -> Word32 -> IO ()
calc dv q cp dslyt ds sz =
	Vk.Ppl.Lyt.create dv (pplLayoutInfo dslyt) nil \plyt ->
	Vk.Ppl.Cmpt.createCs dv Nothing
		(HPList.Singleton . U4 $ pplInfo plyt) nil \(pl :** HPList.Nil) ->
	Vk.CBffr.allocate dv (commandBufferInfo cp) \(cb :*. HPList.Nil) ->
	run q ds cb plyt pl sz

pplLayoutInfo :: Vk.DscStLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

commandPoolInfo :: Vk.QFam.Index -> Vk.CmdPl.CreateInfo 'Nothing
commandPoolInfo qfi = Vk.CmdPl.CreateInfo {
	Vk.CmdPl.createInfoNext = TMaybe.N,
	Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
	Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

commandBufferInfo :: Vk.CmdPl.C s -> Vk.CBffr.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cmdPool = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cmdPool,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

run :: forall slbts sc sg sl sds . (
	Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]],
	InfixIndex '[slbts] '[slbts] ) =>
	Vk.Q.Q -> Vk.DS.D sds slbts -> Vk.CBffr.C sc ->
	Vk.Ppl.Lyt.P sl '[slbts] '[] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[]) -> Word32 -> IO ()
run q ds cb lyt pl sz = do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute pl \ccb ->
		Vk.Cmd.bindDescriptorSetsCompute
			ccb lyt (HPList.Singleton $ U2 ds) def >>
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

head' :: [a] -> a
head' = \case [] -> error "empty list"; x : _ -> x

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
