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

import Foreign.Storable
import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Object.Base qualified as KObj
import Data.Default
import Data.Bits
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.List
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word

import qualified Data.Vector.Storable as V

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.Device as Vk.Dvc
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
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt

import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

---------------------------------------------------------------------------

-- MAIN
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- MAIN

main :: IO ()
main = withDevice \phdvc qFam dvc mgcx -> do
	let	da = V.fromList $ W1 <$> [1 .. mgcx]
		db = V.fromList $ W2 <$> [100, 200 .. 100 * mgcx]
		dc = V.replicate mgcx $ W3 0
	(r1, r2, r3) <-
		Vk.DscSetLyt.create dvc dscSetLayoutInfo nil \dscSetLyt ->
		prepareMems phdvc dvc dscSetLyt da db dc \dscSet ma mb mc ->
		calc dvc qFam dscSetLyt dscSet mgcx >>
		(,,)	<$> Vk.Mm.read @"" @(VObj.List 256 W1 "") @0 @[W1] dvc ma def
			<*> Vk.Mm.read @"" @(VObj.List 256 W2 "") @0 @[W2] dvc mb def
			<*> Vk.Mm.read @"" @(VObj.List 256 W3 "") @0 @[W3] dvc mc def
	print . take 20 $ unW1 <$> r1
	print . take 20 $ unW2 <$> r2
	print . take 20 $ unW3 <$> r3

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

withDevice ::
	(forall sd . Vk.PhDvc.P -> Vk.QFam.Index -> Vk.Dvc.D sd ->
	(forall c . Integral c => c) -> IO a) -> IO a
withDevice f = Vk.Inst.create @_ @'Nothing instInfo nil \inst -> do
	phdvc <- head <$> Vk.PhDvc.enumerate inst
	qFam <- fst . head . filter (
			(/= zeroBits) . (.&. Vk.Queue.ComputeBit)
				. Vk.QFam.propertiesQueueFlags . snd )
		<$> Vk.PhDvc.getQueueFamilyProperties phdvc
	mgcx :. _ <- Vk.PhDvc.limitsMaxComputeWorkGroupCount
		. Vk.PhDvc.propertiesLimits <$> Vk.PhDvc.getProperties phdvc
	Vk.Dvc.create phdvc (dvcInfo qFam) nil $ \dvc ->
		f phdvc qFam dvc (fromIntegral mgcx)

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.layerKhronosValidation] }
	
dvcInfo :: Vk.QFam.Index -> Vk.Dvc.CreateInfo 'Nothing '[ 'Nothing]
dvcInfo qFam = Vk.Dvc.CreateInfo {
	Vk.Dvc.createInfoNext = TMaybe.N,
	Vk.Dvc.createInfoFlags = def,
	Vk.Dvc.createInfoQueueCreateInfos = HeteroParList.Singleton queueInfo,
	Vk.Dvc.createInfoEnabledLayerNames = [Vk.layerKhronosValidation],
	Vk.Dvc.createInfoEnabledExtensionNames = [],
	Vk.Dvc.createInfoEnabledFeatures = Nothing }
	where queueInfo = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qFam,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
	'Vk.DscSetLyt.Buffer '[VObj.List 256 W1 "",VObj.List 256 W2 "",VObj.List 256 W3 ""] ]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = TMaybe.N,
	Vk.DscSetLyt.createInfoFlags = zeroBits,
	Vk.DscSetLyt.createInfoBindings = HeteroParList.Singleton binding }

binding :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer objs)
binding = Vk.DscSetLyt.BindingBuffer {
	Vk.DscSetLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

-- PREPARE MEMORIES

prepareMems :: (
	Default (HeteroParList.PL
		(HeteroParList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DscSet.BindingAndArrayElemBuffer bts '[
		VObj.List 256 W1 "",VObj.List 256 W2 "",VObj.List 256 W3 "" ] 0,
	Vk.DscSet.UpdateDynamicLength bts '[
		VObj.List 256 W1 "",VObj.List 256 W2 "",VObj.List 256 W3 "" ]
	) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.D sl bts ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (forall sds sm1 sb1 sm2 sb2 sm3 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mm.M sm1 '[ '( sb1, 'Vk.Mm.BufferArg "" '[VObj.List 256 W1 ""])] ->
		Vk.Mm.M sm2 '[ '( sb2, 'Vk.Mm.BufferArg "" '[VObj.List 256 W2 ""])] ->
		Vk.Mm.M sm3 '[ '( sb3, 'Vk.Mm.BufferArg "" '[VObj.List 256 W3 ""])] -> IO a) -> IO a
prepareMems phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(HeteroParList.Singleton dscSet) ->
	storageBufferNew dvc phdvc da \ba ma ->
	storageBufferNew dvc phdvc db \bb mb ->
	storageBufferNew dvc phdvc dc \bc mc ->
	Vk.DscSet.updateDs dvc
		(HeteroParList.Singleton . U5 $ writeDscSet dscSet ba bb bc)
		HeteroParList.Nil >>
	f dscSet ma mb mc

dscPoolInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscSetLyt.D sl bts ->
	Vk.DscSet.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = TMaybe.N,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts =
		HeteroParList.Singleton $ U2 lyt }

storageBufferNew :: forall sd nm w a . Storable w =>
	Vk.Dvc.D sd -> Vk.PhDvc.P -> V.Vector w -> (forall sb sm .
		Vk.Buffer.Binded sm sb nm '[VObj.List 256 w ""]  ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[VObj.List 256 w ""])] ->
		IO a) -> IO a
storageBufferNew dvc phdvc xs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil \bf ->
	getMemoryInfo phdvc dvc bf >>= \mmi ->
	Vk.Mm.allocateBind dvc
		(HeteroParList.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil
		\(HeteroParList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	Vk.Mm.write @nm @(VObj.List 256 w "") @0 dvc mm def xs >> f bnd mm

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo 'Nothing '[VObj.List 256 w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		VObj.LengthList (fromIntegral $ V.length xs) :** HeteroParList.Nil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Mem.AllocateInfo 'Nothing)
getMemoryInfo phdvc dvc buffer = do
	rqs <- Vk.Buffer.getMemoryRequirements dvc buffer
	mti <- findMemoryTypeIndex phdvc rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mti }

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

writeDscSet ::
	forall slbts sb1 sb2 sb3 sm1 sm2 sm3 objs1 objs2 objs3 sds . (
	Show (HeteroParList.PL VObj.Length objs1),
	Show (HeteroParList.PL VObj.Length objs2),
	Show (HeteroParList.PL VObj.Length objs3),
	VObj.OffsetRange' (VObj.List 256 W1 "") objs1 0,
	VObj.OffsetRange' (VObj.List 256 W2 "") objs2 0,
	VObj.OffsetRange' (VObj.List 256 W3 "") objs3 0
	) =>
	Vk.DscSet.D sds slbts ->
	Vk.Buffer.Binded sm1 sb1 "" objs1 -> Vk.Buffer.Binded sm2 sb2 "" objs2 ->
	Vk.Buffer.Binded sm3 sb3 "" objs3 ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm1, sb1, "", VObj.List 256 W1 "", 0), '(sm2, sb2, "", VObj.List 256 W2 "", 0),
		'(sm3, sb3, "", VObj.List 256 W3 "", 0) ]) 0
writeDscSet ds ba bb bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(VObj.List 256 W1 "") ba) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(VObj.List 256 W2 "") bb) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(VObj.List 256 W3 "") bc) :**
		HeteroParList.Nil }

-- CALC

calc :: forall slbts sl bts sd sds . (
	slbts ~ '(sl, bts),
	Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	Show (HeteroParList.PL
		(HeteroParList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	InfixIndex '[slbts] '[ '(sl, bts)]) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.DscSetLyt.D sl bts ->
	Vk.DscSet.D sds slbts -> Word32 -> IO ()
calc dvc qFam dscSetLyt dscSet dsz =
	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dscSetLyt) nil \plyt ->
	Vk.Ppl.Cmpt.createCs dvc Nothing
		(HeteroParList.Singleton . U4 $ computePipelineInfo plyt)
		nil \(ppl :** HeteroParList.Nil) ->
	Vk.CmdPool.create dvc (commandPoolInfo qFam) nil \cmdPool ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cmdPool) \(cb :*. HeteroParList.Nil) ->
		run dvc qFam cb ppl plyt dscSet dsz

pplLayoutInfo :: Vk.DscSetLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HeteroParList.Singleton $ U2 dsl }

commandPoolInfo :: Vk.QFam.Index -> Vk.CmdPool.CreateInfo 'Nothing
commandPoolInfo qFam = Vk.CmdPool.CreateInfo {
	Vk.CmdPool.createInfoNext = TMaybe.N,
	Vk.CmdPool.createInfoFlags = Vk.CmdPool.CreateResetCommandBufferBit,
	Vk.CmdPool.createInfoQueueFamilyIndex = qFam }

commandBufferInfo :: Vk.CmdPool.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = TMaybe.N,
	Vk.CmdBuf.allocateInfoCommandPool = cmdPool,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

run :: forall slbts sbtss sd sc sg sl sds . (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]],
	Show (HeteroParList.PL
		(HeteroParList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	InfixIndex '[slbts] sbtss ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 -> IO ()
run dvc qFam cb ppl pplLyt dscSet dsz = do
	q <- Vk.Dvc.getQueue dvc qFam 0
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb
				pplLyt (HeteroParList.Singleton $ U2 dscSet)
				(HeteroParList.Singleton $ HeteroParList.Singleton HeteroParList.Nil ::
					HeteroParList.PL3 Vk.Cmd.DynamicIndex (Vk.Cmd.LayoutArgListOnlyDynamics sbtss))
			Vk.Cmd.dispatch ccb dsz 1 1
	Vk.Queue.submit q (HeteroParList.Singleton $ U4 submitInfo) Nothing
	Vk.Queue.waitIdle q
	where
	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[sc] '[]
	submitInfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HeteroParList.Nil,
		Vk.submitInfoCommandBuffers = HeteroParList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HeteroParList.Nil }

-- COMPUTE PIPELINE INFO

computePipelineInfo :: Vk.Ppl.Lyt.P sl sbtss '[] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[])
		'(sl, sbtss, '[]) sbph
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

layout(binding = 0) buffer Data {
	uint val[];
} data[3];

void
main()
{
	int index = int(gl_GlobalInvocationID.x);
	data[2].val[index] = (data[0].val[index] + data[1].val[index]);
}

|]
