{-# LANGUAGE ImportQualifiedPost #-}
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

import Gpu.Vulkan.Object qualified as Obj
import Data.Kind.Object qualified as KObj
import Data.Default
import Data.Bits
import Data.TypeLevel.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import qualified Data.HeteroParList as HL
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word
import Data.Char

import Shaderc.TH
import Shaderc.EnumAuto
import Gpu.Vulkan.Misc

import Gpu.Vulkan.AllocationCallbacks qualified as Vk.AllocCallbacks

import qualified Gpu.VulkanNew as Vk
import qualified Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.QueueNew as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFm
import qualified Gpu.Vulkan.QueueFamily.Middle as Vk.QFm
import qualified Gpu.Vulkan.Device as Vk.Dv
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPool
import qualified Gpu.Vulkan.Buffer.Enum as Vk.Bffr
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Memory.Kind as Vk.Mm.K
import qualified Gpu.Vulkan.Memory.Enum as Vk.Mm
import qualified Gpu.Vulkan.Memory.Middle as Vk.Mm.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.Pipeline.Layout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DS
import qualified Gpu.Vulkan.DescriptorSet.TypeLevel.Write as Vk.DS
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr
import qualified Gpu.Vulkan.CommandNew as Vk.Cmd
import qualified Gpu.Vulkan.Command.TypeLevel as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.Memory.AllocateInfo as Vk.Dv.Mem.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DSLyt
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.DSLyt

import qualified Gpu.Vulkan.Khr as Vk.Khr
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

---------------------------------------------------------------------------

-- MAIN
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- MAIN

bffSize :: Integral n => n
bffSize = 30

main :: IO ()
main = withDevice \pd qfi dv -> putStrLn . map (chr . fromIntegral) =<<
	Vk.DSLyt.create dv dscSetLayoutInfo nil nil \dslyt ->
	prepareMems pd dv dslyt \dscs m ->
	calc qfi dv dslyt dscs bffSize >>
	Vk.Mm.read @"" @Word32List @[Word32] dv m zeroBits

allocationCallbacks :: Vk.AllocCallbacks.A ()
allocationCallbacks = Vk.AllocCallbacks.A {
	Vk.AllocCallbacks.allocationCallbacksUserData = (),
	Vk.AllocCallbacks.allocationCallbacksFnAllocation = undefined,
	Vk.AllocCallbacks.allocationCallbacksFnReallocation = undefined,
	Vk.AllocCallbacks.allocationCallbacksFnFree = undefined,
	Vk.AllocCallbacks.allocationCallbacksFnInternalAllocationFree = Nothing }

type Word32List = Obj.List 256 Word32 ""

withDevice :: (forall s . Vk.Phd.P -> Vk.QFm.Index -> Vk.Dv.D s -> IO a) -> IO a
withDevice f = Vk.Inst.create instInfo nil nil \inst -> do
	pd <- head <$> Vk.Phd.enumerate inst
	qfi <- fst . head . filter (
			checkBits Vk.Queue.ComputeBit .
			Vk.QFm.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	Vk.Dv.create pd (dvcInfo qfi) nil nil $ f pd qfi

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.Khr.validationLayerName] }
	
dvcInfo :: Vk.QFm.Index -> Vk.Dv.CreateInfo 'Nothing '[ 'Nothing]
dvcInfo qfi = Vk.Dv.CreateInfo {
	Vk.Dv.createInfoNext = TMaybe.N, Vk.Dv.createInfoFlags = zeroBits,
	Vk.Dv.createInfoQueueCreateInfos = HL.Singleton qinfo,
	Vk.Dv.createInfoEnabledLayerNames = [Vk.Khr.validationLayerName],
	Vk.Dv.createInfoEnabledExtensionNames = [],
	Vk.Dv.createInfoEnabledFeatures = Nothing }
	where qinfo = Vk.Dv.QueueCreateInfo {
		Vk.Dv.queueCreateInfoNext = TMaybe.N,
		Vk.Dv.queueCreateInfoFlags = zeroBits,
		Vk.Dv.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dv.queueCreateInfoQueuePriorities = [0] }

dscSetLayoutInfo :: Vk.DSLyt.CreateInfo 'Nothing '[ 'Vk.DSLyt.Buffer '[Word32List]]
dscSetLayoutInfo = Vk.DSLyt.CreateInfo {
	Vk.DSLyt.createInfoNext = TMaybe.N, Vk.DSLyt.createInfoFlags = zeroBits,
	Vk.DSLyt.createInfoBindings = HL.Singleton bdg }
	where bdg = Vk.DSLyt.BindingBuffer {
		Vk.DSLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
		Vk.DSLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)

-- PREPARE MEMORIES

prepareMems :: (
	Default (HL.PL (HL.PL KObj.ObjectLength)
		(Vk.DSLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DS.BindingAndArrayElem bts '[Word32List] 0 ) =>
	Vk.Phd.P -> Vk.Dv.D sd -> Vk.DSLyt.L sl bts ->
	(forall s sm sb .
		Vk.DS.S sd s '(sl, bts) ->
		Vk.Mm.M sm '[ '( sb, 'Vk.Mm.K.Buffer "" '[Word32List])] ->
		IO a) -> IO a
prepareMems pd dv dslyt f =
	Vk.DscPool.create dv dscPoolInfo nil nil \dp ->
	Vk.DS.allocateSs dv (dscSetInfo dp dslyt) >>= \(HL.Singleton ds) ->
	storageBufferNew pd dv \b m ->
	Vk.DS.updateDs @_ @'Nothing dv (HL.Singleton . U4 $ writeDscSet ds b) [] >>
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

dscSetInfo :: Vk.DscPool.P sp -> Vk.DSLyt.L sl bts ->
	Vk.DS.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DS.AllocateInfo {
	Vk.DS.allocateInfoNext = TMaybe.N,
	Vk.DS.allocateInfoDescriptorPool = pl,
	Vk.DS.allocateInfoSetLayouts = HL.Singleton $ U2 lyt }

storageBufferNew :: forall sd nm a . Vk.Phd.P -> Vk.Dv.D sd -> (forall sb sm .
	Vk.Bffr.Binded sb sm nm '[Word32List]  ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.K.Buffer nm '[Word32List])] -> IO a) -> IO a
storageBufferNew pd dv f =
	Vk.Bffr.create dv bufferInfo nil nil \bf ->
	getMemoryInfo pd dv bf >>= \mmi ->
	Vk.Mm.allocateBind dv
		(HL.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil nil
		\(HL.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	f bnd mm

bufferInfo :: Vk.Bffr.CreateInfo 'Nothing '[Word32List]
bufferInfo = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HL.Singleton $ Obj.ObjectLengthList bffSize,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.Phd.P -> Vk.Dv.D sd -> Vk.Bffr.B sb nm objs ->
	IO (Vk.Dv.Mem.Buffer.AllocateInfo 'Nothing)
getMemoryInfo pd dv bff = do
	rqs <- Vk.Bffr.getMemoryRequirements dv bff
	mti <- findMemoryTypeIndex pd rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Dv.Mem.Buffer.AllocateInfo {
		Vk.Dv.Mem.Buffer.allocateInfoNext = TMaybe.N,
		Vk.Dv.Mem.Buffer.allocateInfoMemoryTypeIndex = mti }

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

writeDscSet :: forall sd sp slbts sb sm os .
	Vk.DS.S sd sp slbts -> Vk.Bffr.Binded sm sb "" os ->
	Vk.DS.Write 'Nothing sd sp slbts
		('Vk.DS.WriteSourcesArgBuffer '[ '(sb, sm, "", os, Word32List)])
writeDscSet ds ba = Vk.DS.Write {
	Vk.DS.writeNext = TMaybe.N,
	Vk.DS.writeDstSet = ds,
	Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DS.writeSources =
		Vk.DS.BufferInfos . HL.Singleton $ Vk.Dsc.BufferInfoList ba }

-- CALC

calc :: forall slbts sl bts sd sp . (
	slbts ~ '(sl, bts),
	Vk.DSLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	Vk.Cmd.SetPos '[slbts] '[slbts]) =>
	Vk.QFm.Index -> Vk.Dv.D sd -> Vk.DSLyt.L sl bts ->
	Vk.DS.S sd sp slbts -> Word32 -> IO ()
calc qfi dv dslyt ds sz =
	Vk.Ppl.Lyt.createNew dv (pplLayoutInfo dslyt) nil nil \plyt ->
	Vk.Ppl.Cmpt.createCsNew dv Nothing
		(HL.Singleton . U4 $ pplInfo plyt) nil nil \(pl :** HL.Nil) ->
	Vk.CmdPool.create dv (commandPoolInfo qfi) nil nil \cp ->
	Vk.CBffr.allocateNew dv (commandBufferInfo cp) \(cb :*. HL.Nil) ->
	run qfi dv ds cb plyt pl sz

pplLayoutInfo :: Vk.DSLyt.L sl bts ->
	Vk.Ppl.Lyt.CreateInfoNew 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.PushConstantLayout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfoNew {
	Vk.Ppl.Lyt.createInfoNextNew = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlagsNew = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayoutsNew = HL.Singleton $ U2 dsl }

commandPoolInfo :: Vk.QFm.Index -> Vk.CmdPool.CreateInfo 'Nothing
commandPoolInfo qfi = Vk.CmdPool.CreateInfo {
	Vk.CmdPool.createInfoNext = TMaybe.N,
	Vk.CmdPool.createInfoFlags = Vk.CmdPool.CreateResetCommandBufferBit,
	Vk.CmdPool.createInfoQueueFamilyIndex = qfi }

commandBufferInfo :: Vk.CmdPool.C s -> Vk.CBffr.AllocateInfoNew 'Nothing s 1
commandBufferInfo cmdPool = Vk.CBffr.AllocateInfoNew {
	Vk.CBffr.allocateInfoNextNew = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPoolNew = cmdPool,
	Vk.CBffr.allocateInfoLevelNew = Vk.CBffr.LevelPrimary }

run :: forall slbts sd sc sg sl sp . (
	Vk.DS.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]],
	Vk.Cmd.SetPos '[slbts] '[slbts] ) =>
	Vk.QFm.Index -> Vk.Dv.D sd -> Vk.DS.S sd sp slbts -> Vk.CBffr.C sc ->
	Vk.Ppl.Lyt.L sl '[slbts] '[] ->
	Vk.Ppl.Cmpt.CNew sg '(sl, '[slbts], '[]) -> Word32 -> IO ()
run qfi dv ds cb lyt pl sz = Vk.Dv.getQueue dv qfi 0 >>= \q -> do
	Vk.CBffr.beginNew @'Nothing @'Nothing cb def $
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

pplInfo :: Vk.Ppl.Lyt.L sl sbtss '[] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing '( 'Nothing, 'Nothing, 'GlslComputeShader, (), (), '[])
		'(sl, sbtss, '[]) sbph
pplInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U6 shaderStInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shaderStInfo :: Vk.Ppl.ShaderSt.CreateInfoNew 'Nothing 'Nothing 'GlslComputeShader () () '[]
shaderStInfo = Vk.Ppl.ShaderSt.CreateInfoNew {
	Vk.Ppl.ShaderSt.createInfoNextNew = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlagsNew = zeroBits,
	Vk.Ppl.ShaderSt.createInfoStageNew = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModuleNew = Vk.ShaderMod.M shdrMdInfo nil nil,
	Vk.Ppl.ShaderSt.createInfoNameNew = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfoNew = Nothing }
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