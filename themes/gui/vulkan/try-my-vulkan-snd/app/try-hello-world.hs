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

import Foreign.Storable
import Gpu.Vulkan.Object qualified as VObj
import Data.Kind.Object qualified as KObj
import Data.Default
import Data.Bits
import Data.TypeLevel.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word
import Data.Char

import qualified Data.Vector.Storable as V

import Shaderc.TH
import Shaderc.EnumAuto
import Gpu.Vulkan.Misc

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.QueueFamily.Middle as Vk.QFam
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPool
import qualified Gpu.Vulkan.Buffer.Enum as Vk.Buffer
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
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.DescriptorSet.TypeLevel as Vk.DscSet
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified Gpu.Vulkan.Command as Vk.Cmd
import qualified Gpu.Vulkan.Command.TypeLevel as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Buffer
import qualified Gpu.Vulkan.Memory.AllocateInfo as Vk.Dvc.Mem.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.DscSetLyt

import qualified Gpu.Vulkan.Khr as Vk.Khr
import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

---------------------------------------------------------------------------

-- MAIN
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- MAIN

bufferSize :: Integral n => n
bufferSize = 30

main :: IO ()
main = withDevice \pd qfi dv -> putStrLn . map (chr . fromIntegral)
	=<< Vk.DscSetLyt.create dv dscSetLayoutInfo nil nil \dslyt ->
		prepareMems pd dv dslyt \dscSet ma ->
		calc dv qfi dslyt dscSet bufferSize >>
		Vk.Mm.read @"" @Word32List @[Word32] dv ma def

type Word32List = VObj.List 256 Word32 ""

withDevice ::
	(forall sd . Vk.PhDvc.P -> Vk.QFam.Index -> Vk.Dvc.D sd -> IO a) -> IO a
withDevice f = Vk.Inst.create @() @() instInfo nil nil \inst -> do
	pd <- head <$> Vk.PhDvc.enumerate inst
	qfi <- fst . head . filter (
			(/= zeroBits) . (.&. Vk.Queue.ComputeBit)
				. Vk.QFam.propertiesQueueFlags . snd )
		<$> Vk.PhDvc.getQueueFamilyProperties pd
	Vk.Dvc.create @() @'[()] pd (dvcInfo qfi) nil nil \dv -> f pd qfi dv

instInfo :: Vk.Inst.CreateInfo () ()
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.Khr.validationLayerName] }
	
dvcInfo :: Vk.QFam.Index -> Vk.Dvc.CreateInfo n '[s]
dvcInfo qfi = Vk.Dvc.CreateInfo {
	Vk.Dvc.createInfoNext = Nothing,
	Vk.Dvc.createInfoFlags = def,
	Vk.Dvc.createInfoQueueCreateInfos = HeteroParList.Singleton queueInfo,
	Vk.Dvc.createInfoEnabledLayerNames = [Vk.Khr.validationLayerName],
	Vk.Dvc.createInfoEnabledExtensionNames = [],
	Vk.Dvc.createInfoEnabledFeatures = Nothing }
	where queueInfo = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = Nothing,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo () '[
	'Vk.DscSetLyt.Buffer '[Word32List] ]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = Nothing,
	Vk.DscSetLyt.createInfoFlags = zeroBits,
	Vk.DscSetLyt.createInfoBindings = HeteroParList.Singleton binding }

binding :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer objs)
binding = Vk.DscSetLyt.BindingBuffer {
	Vk.DscSetLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

-- PREPARE MEMORIES

prepareMems :: (
	Default (HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DscSet.BindingAndArrayElem bts '[Word32List]
	) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.L sl bts ->
	(forall s sm1 sb1 .
		Vk.DscSet.S sd s '(sl, bts) ->
		Vk.Mm.M sm1 '[ '( sb1, 'Vk.Mm.K.Buffer "" '[Word32List])] -> IO a) -> IO a
prepareMems phdvc dvc dscSetLyt f =
	Vk.DscPool.create dvc dscPoolInfo nil nil \dscPool ->
	Vk.DscSet.allocateSs dvc (dscSetInfo dscPool dscSetLyt)
		>>= \(HeteroParList.Singleton dscSet) ->
	storageBufferNew dvc phdvc \ba ma ->
	Vk.DscSet.updateDs @() @() dvc
		(HeteroParList.Singleton . U4 $ writeDscSet dscSet ba)
		[] >>
	f dscSet ma

dscPoolInfo :: Vk.DscPool.CreateInfo ()
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = Nothing,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscSetLyt.L sl bts ->
	Vk.DscSet.AllocateInfo () sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = Nothing,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts =
		HeteroParList.Singleton $ U2 lyt }

storageBufferNew :: forall sd nm a . Vk.Dvc.D sd -> Vk.PhDvc.P ->
	(forall sb sm .
		Vk.Buffer.Binded sb sm nm '[Word32List]  ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.K.Buffer nm '[Word32List])] ->
		IO a) -> IO a
storageBufferNew dvc phdvc f =
	Vk.Buffer.create dvc bufferInfo nil nil \bf ->
	getMemoryInfo phdvc dvc bf >>= \mmi ->
	Vk.Mm.allocateBind dvc
		(HeteroParList.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil nil
		\(HeteroParList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	f bnd mm

bufferInfo :: Vk.Buffer.CreateInfo () '[Word32List]
bufferInfo = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = Nothing,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		VObj.ObjectLengthList bufferSize :** HeteroParList.Nil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Dvc.Mem.Buffer.AllocateInfo ())
getMemoryInfo phdvc dvc buffer = do
	rqs <- Vk.Buffer.getMemoryRequirements dvc buffer
	mti <- findMemoryTypeIndex phdvc rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Dvc.Mem.Buffer.AllocateInfo {
		Vk.Dvc.Mem.Buffer.allocateInfoNext = Nothing,
		Vk.Dvc.Mem.Buffer.allocateInfoMemoryTypeIndex = mti }

findMemoryTypeIndex ::
	Vk.PhDvc.P -> Vk.Mm.M.Requirements -> Vk.Mm.PropertyFlags ->
	IO Vk.Mm.M.TypeIndex
findMemoryTypeIndex phdvc rqs prp0 = do
	memoryProperties <- Vk.PhDvc.getMemoryProperties phdvc
	let	reqTypes = Vk.Mm.M.requirementsMemoryTypeBits rqs
		memPropTypes = (fst <$>)
			. filter (checkBits prp0
				. Vk.Mm.M.mTypePropertyFlags . snd)
			$ Vk.PhDvc.memoryPropertiesMemoryTypes memoryProperties
	case filter (`Vk.Mm.M.elemTypeIndex` reqTypes) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> pure i

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)

writeDscSet :: forall sd sp slbts sb1 sm1 objs1 .
	Vk.DscSet.S sd sp slbts ->
	Vk.Buffer.Binded sm1 sb1 "" objs1 ->
	Vk.DscSet.Write () sd sp slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sb1, sm1, "", objs1, Word32List) ])
writeDscSet ds ba = Vk.DscSet.Write {
	Vk.DscSet.writeNext = Nothing,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		Vk.Dsc.BufferInfoList @_ @_ @_ @_ @_ @Word32 ba :**
		HeteroParList.Nil }

-- CALC

calc :: forall slbts sl bts sd sp . (
	slbts ~ '(sl, bts),
	Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	Show (HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(Vk.DscSet.LayoutArgOnlyDynamics slbts)),
	Vk.Cmd.SetPos '[slbts] '[ '(sl, bts)]) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.DscSetLyt.L sl bts ->
	Vk.DscSet.S sd sp slbts -> Word32 -> IO ()
calc dvc qfi dscSetLyt dscSet dsz =
	Vk.Ppl.Lyt.createNew dvc (pplLayoutInfo dscSetLyt) nil nil \plyt ->
	Vk.Ppl.Cmpt.createCs dvc Nothing
		(HeteroParList.Singleton . U4 $ computePipelineInfo plyt)
		nil nil \(ppl :*. HeteroParList.Nil) ->
	Vk.CmdPool.create dvc (commandPoolInfo qfi) nil nil \cmdPool ->
	Vk.CmdBuf.allocateOld dvc (commandBufferInfo cmdPool) \case
		[cmdBuf] -> run dvc qfi cmdBuf ppl plyt dscSet dsz
		_ -> error "never occur"

pplLayoutInfo :: Vk.DscSetLyt.L sl bts ->
	Vk.Ppl.Lyt.CreateInfoNew () '[ '(sl, bts)]
		('Vk.PushConstant.PushConstantLayout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfoNew {
	Vk.Ppl.Lyt.createInfoNextNew = Nothing,
	Vk.Ppl.Lyt.createInfoFlagsNew = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayoutsNew = HeteroParList.Singleton $ U2 dsl }

commandPoolInfo :: Vk.QFam.Index -> Vk.CmdPool.CreateInfo ()
commandPoolInfo qfi = Vk.CmdPool.CreateInfo {
	Vk.CmdPool.createInfoNext = Nothing,
	Vk.CmdPool.createInfoFlags = Vk.CmdPool.CreateResetCommandBufferBit,
	Vk.CmdPool.createInfoQueueFamilyIndex = qfi }

commandBufferInfo :: Vk.CmdPool.C s -> Vk.CmdBuf.AllocateInfoOld () s
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfoOld {
	Vk.CmdBuf.allocateInfoNextOld = Nothing,
	Vk.CmdBuf.allocateInfoCommandPoolOld = cmdPool,
	Vk.CmdBuf.allocateInfoLevelOld = Vk.CmdBuf.LevelPrimary,
	Vk.CmdBuf.allocateInfoCommandBufferCountOld = 1 }

run :: forall slbts sbtss sd sc vs sg sl sp . (
	sbtss ~ '[slbts],
	Vk.DscSet.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]],
	Show (HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(Vk.DscSet.LayoutArgOnlyDynamics slbts)),
	Vk.Cmd.SetPos '[slbts] sbtss ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.Binded sc vs -> Vk.Ppl.Cmpt.C sg ->
	Vk.Ppl.Lyt.L sl sbtss '[] -> Vk.DscSet.S sd sp slbts -> Word32 -> IO ()
run dvc qfi cb ppl pplLyt dscSet dsz = do
	q <- Vk.Dvc.getQueue dvc qfi 0
	Vk.CmdBuf.begin @() @() cb def do
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl
		Vk.Cmd.bindDescriptorSetsNew cb Vk.Ppl.BindPointCompute
			pplLyt (HeteroParList.Singleton $ U2 dscSet)
			(HeteroParList.Singleton $ HeteroParList.Singleton HeteroParList.Nil ::
				HeteroParList.PL3 Vk.Cmd.DynamicIndex (Vk.DscSet.LayoutArgListOnlyDynamics sbtss))
		Vk.Cmd.dispatch cb dsz 1 1
	Vk.Queue.submit q (HeteroParList.Singleton $ U4 submitInfo) Nothing
	Vk.Queue.waitIdle q
	where
	submitInfo :: Vk.SubmitInfo () '[] '[ '(sc, vs)] '[]
	submitInfo = Vk.SubmitInfo {
		Vk.submitInfoNext = Nothing,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
		Vk.submitInfoCommandBuffers = HeteroParList.Singleton $ U2 cb,
		Vk.submitInfoSignalSemaphores = HeteroParList.Nil }

-- COMPUTE PIPELINE INFO

computePipelineInfo :: Vk.Ppl.Lyt.L sl sbtss '[] ->
	Vk.Ppl.Cmpt.CreateInfo ()
		'((), (), 'GlslComputeShader, (), (), '[])
		'(sl, sbtss, '[]) sbph
computePipelineInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = Nothing,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U6 shaderStageInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandle = Nothing,
	Vk.Ppl.Cmpt.createInfoBasePipelineIndex = Nothing }

shaderStageInfo ::
	Vk.Ppl.ShaderSt.CreateInfoNew () () 'GlslComputeShader () () '[]
shaderStageInfo = Vk.Ppl.ShaderSt.CreateInfoNew {
	Vk.Ppl.ShaderSt.createInfoNextNew = Nothing,
	Vk.Ppl.ShaderSt.createInfoFlagsNew = def,
	Vk.Ppl.ShaderSt.createInfoStageNew = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModuleNew = Vk.ShaderMod.M shdrMdInfo nil nil,
	Vk.Ppl.ShaderSt.createInfoNameNew = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfoNew = Nothing }
	where shdrMdInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = Nothing,
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
