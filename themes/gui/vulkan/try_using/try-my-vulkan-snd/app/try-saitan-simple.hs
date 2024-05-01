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
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Storable
import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as KObj
import Data.Default
import Data.Bits
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word

import qualified Data.Vector.Storable as V

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind
import Data.TypeLevel.ParMaybe (nil)

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
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscStLyt

import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import Data.Bits.ToolsYj

---------------------------------------------------------------------------

-- MAIN
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- MAIN

main :: IO ()
main = withDvc \pd qfi d mgcx -> do
	let	da = V.fromList $ W1 <$> [1 .. mgcx]
		db = V.fromList $ W2 <$> [100, 200 .. 100 * mgcx]
		dc = V.replicate mgcx $ W3 0
	(r1, r2, r3) <-
		Vk.DscStLyt.create d dscStLytInfo nil \dsl ->
		prepareMems pd d dsl da db dc \dss ma mb mc ->
		calc d qfi dsl dss mgcx >>
		(,,)	<$> Vk.Mm.read @"" @(Obj.List 256 W1 "") @[W1] d ma def
			<*> Vk.Mm.read @"" @(Obj.List 256 W2 "") @[W2] d mb def
			<*> Vk.Mm.read @"" @(Obj.List 256 W3 "") @[W3] d mc def
	print . take 20 $ unW1 <$> r1
	print . take 20 $ unW2 <$> r2
	print . take 20 $ unW3 <$> r3

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

withDvc :: (forall sd .
	Vk.PhDvc.P -> Vk.QFam.Index -> Vk.Dvc.D sd ->
	(forall c . Integral c => c) -> IO a) -> IO a
withDvc a = Vk.Inst.create @_ @'Nothing instInfo nil \ist -> do
	pd <- head <$> Vk.PhDvc.enumerate ist
	qfi <- fst . head . filter
			(checkBits Vk.Queue.ComputeBit
				. Vk.QFam.propertiesQueueFlags . snd)
		<$> Vk.PhDvc.getQueueFamilyProperties pd
	mgcx :. _ <- Vk.PhDvc.limitsMaxComputeWorkGroupCount
		. Vk.PhDvc.propertiesLimits <$> Vk.PhDvc.getProperties pd
	Vk.Dvc.create pd (dvcInfo qfi) nil $ \d ->
		a pd qfi d $ fromIntegral mgcx

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.layerKhronosValidation] }
	
dvcInfo :: Vk.QFam.Index -> Vk.Dvc.CreateInfo 'Nothing '[ 'Nothing]
dvcInfo qfi = Vk.Dvc.CreateInfo {
	Vk.Dvc.createInfoNext = TMaybe.N,
	Vk.Dvc.createInfoFlags = zeroBits,
	Vk.Dvc.createInfoQueueCreateInfos = HPList.Singleton qinfo,
	Vk.Dvc.createInfoEnabledLayerNames = [Vk.layerKhronosValidation],
	Vk.Dvc.createInfoEnabledExtensionNames = [],
	Vk.Dvc.createInfoEnabledFeatures = Nothing }
	where qinfo = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = def,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }

dscStLytInfo :: Vk.DscStLyt.CreateInfo 'Nothing '[
	'Vk.DscStLyt.Buffer
		'[Obj.List 256 W1 "", Obj.List 256 W2 "", Obj.List 256 W3 ""] ]
dscStLytInfo = Vk.DscStLyt.CreateInfo {
	Vk.DscStLyt.createInfoNext = TMaybe.N,
	Vk.DscStLyt.createInfoFlags = zeroBits,
	Vk.DscStLyt.createInfoBindings = HPList.Singleton bdng }
	where bdng = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

-- PREPARE MEMORIES

prepareMems :: (
	Default (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DscSet.BindingAndArrayElemBuffer bts '[
		Obj.List 256 W1 "",Obj.List 256 W2 "",Obj.List 256 W3 "" ] 0,
	Vk.DscSet.UpdateDynamicLength bts '[
		Obj.List 256 W1 "",Obj.List 256 W2 "",Obj.List 256 W3 "" ]
	) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (forall sds sm1 sb1 sm2 sb2 sm3 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mm.M sm1 '[ '( sb1, 'Vk.Mm.BufferArg "" '[Obj.List 256 W1 ""])] ->
		Vk.Mm.M sm2 '[ '( sb2, 'Vk.Mm.BufferArg "" '[Obj.List 256 W2 ""])] ->
		Vk.Mm.M sm3 '[ '( sb3, 'Vk.Mm.BufferArg "" '[Obj.List 256 W3 ""])] -> IO a) -> IO a
prepareMems phdvc dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPlInfo nil \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(HPList.Singleton dscSet) ->
	storageBufferNew dvc phdvc da \ba ma ->
	storageBufferNew dvc phdvc db \bb mb ->
	storageBufferNew dvc phdvc dc \bc mc ->
	Vk.DscSet.updateDs dvc
		(HPList.Singleton . U5 $ writeDscSet dscSet ba bb bc)
		HPList.Nil >>
	f dscSet ma mb mc

dscPlInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPlInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 3 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSet.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = TMaybe.N,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts =
		HPList.Singleton $ U2 lyt }

storageBufferNew :: forall sd nm w a . Storable w =>
	Vk.Dvc.D sd -> Vk.PhDvc.P -> V.Vector w -> (forall sb sm .
		Vk.Buffer.Binded sm sb nm '[Obj.List 256 w ""]  ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[Obj.List 256 w ""])] ->
		IO a) -> IO a
storageBufferNew dvc phdvc xs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil \bf ->
	getMemoryInfo phdvc dvc bf >>= \mmi ->
	Vk.Mm.allocateBind dvc
		(HPList.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil
		\(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	Vk.Mm.write @nm @(Obj.List 256 w "") dvc mm def xs >> f bnd mm

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo 'Nothing '[Obj.List 256 w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		Obj.LengthList (fromIntegral $ V.length xs) :** HPList.Nil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Mm.AllocateInfo 'Nothing)
getMemoryInfo phdvc dvc buffer = do
	rqs <- Vk.Buffer.getMemoryRequirements dvc buffer
	mti <- findMemoryTypeIndex phdvc rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mti }

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

writeDscSet ::
	forall slbts sb1 sb2 sb3 sm1 sm2 sm3 objs1 objs2 objs3 sds . (
	Show (HPList.PL Obj.Length objs1),
	Show (HPList.PL Obj.Length objs2),
	Show (HPList.PL Obj.Length objs3),
	Obj.OffsetRange (Obj.List 256 W1 "") objs1,
	Obj.OffsetRange (Obj.List 256 W2 "") objs2,
	Obj.OffsetRange (Obj.List 256 W3 "") objs3
	) =>
	Vk.DscSet.D sds slbts ->
	Vk.Buffer.Binded sm1 sb1 "" objs1 -> Vk.Buffer.Binded sm2 sb2 "" objs2 ->
	Vk.Buffer.Binded sm3 sb3 "" objs3 ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm1, sb1, "", Obj.List 256 W1 ""), '(sm2, sb2, "", Obj.List 256 W2 ""),
		'(sm3, sb3, "", Obj.List 256 W3 "") ]) 0
writeDscSet ds ba bb bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U4 (Vk.Dsc.BufferInfo @_ @_ @_ @(Obj.List 256 W1 "") ba) :**
		U4 (Vk.Dsc.BufferInfo @_ @_ @_ @(Obj.List 256 W2 "") bb) :**
		U4 (Vk.Dsc.BufferInfo @_ @_ @_ @(Obj.List 256 W3 "") bc) :**
		HPList.Nil }

-- CALC

calc :: forall slbts sl bts sd sds . (
	slbts ~ '(sl, bts),
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	Show (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	InfixIndex '[slbts] '[ '(sl, bts)]) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.DscStLyt.D sl bts ->
	Vk.DscSet.D sds slbts -> Word32 -> IO ()
calc dvc qFam dscSetLyt dscSet dsz =
	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dscSetLyt) nil \plyt ->
	Vk.Ppl.Cmpt.createCs dvc Nothing
		(HPList.Singleton . U4 $ computePipelineInfo plyt)
		nil \(ppl :** HPList.Nil) ->
	Vk.CmdPool.create dvc (commandPoolInfo qFam) nil \cmdPool ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cmdPool) \(cb :*. HPList.Nil) ->
		run dvc qFam cb ppl plyt dscSet dsz

pplLayoutInfo :: Vk.DscStLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

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
	Show (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	InfixIndex '[slbts] sbtss ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 -> IO ()
run dvc qFam cb ppl pplLyt dscSet dsz = do
	q <- Vk.Dvc.getQueue dvc qFam 0
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb
				pplLyt (HPList.Singleton $ U2 dscSet)
				(HPList.Singleton $ HPList.Singleton HPList.Nil ::
					HPList.PL3 Vk.Cmd.DynamicIndex (Vk.Cmd.LayoutArgListOnlyDynamics sbtss))
			Vk.Cmd.dispatch ccb dsz 1 1
	Vk.Queue.submit q (HPList.Singleton $ U4 submitInfo) Nothing
	Vk.Queue.waitIdle q
	where
	submitInfo :: Vk.SubmitInfo 'Nothing '[] '[sc] '[]
	submitInfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

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

layout(binding = 0) buffer Data { uint val[]; } data[3];

void
main()
{
	int i = int(gl_GlobalInvocationID.x);
	data[2].val[i] = (data[0].val[i] + data[1].val[i]);
}

|]
