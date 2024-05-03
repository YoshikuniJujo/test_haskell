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
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.List
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.List.Length
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Vector.Storable qualified as V
import Data.Word

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.Instance qualified as Vk.Inst
import Gpu.Vulkan.PhysicalDevice qualified as Vk.PhDvc
import Gpu.Vulkan.Queue qualified as Vk.Queue
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CmdBuf
import Gpu.Vulkan.Cmd qualified as Vk.Cmd

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Pipeline.Compute qualified as Vk.Ppl.Cmpt
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShaderSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.Ppl.Lyt
import Gpu.Vulkan.PushConstant qualified as Vk.PushConstant
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderMod
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPool
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSet
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt

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
		(,,)	<$> Vk.Mm.read @"" @(Obj.List 256 W1 "") @0 @[W1] d ma def
			<*> Vk.Mm.read @"" @(Obj.List 256 W2 "") @0 @[W2] d mb def
			<*> Vk.Mm.read @"" @(Obj.List 256 W3 "") @0 @[W3] d mc def
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
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DscSet.BindingAndArrayElemBuffer bts '[
		Obj.List 256 W1 "", Obj.List 256 W2 "", Obj.List 256 W3 "" ] 0,
	Vk.DscSet.UpdateDynamicLength bts '[
		Obj.List 256 W1 "", Obj.List 256 W2 "", Obj.List 256 W3 "" ]
	) =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 ->
	(forall sds sm1 sb1 sm2 sb2 sm3 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Mm sm1 sb1 "" W1 -> Mm sm2 sb2 "" W2 -> Mm sm3 sb3 "" W3 ->
		IO a) -> IO a
prepareMems pd dv dsl da db dc a =
	Vk.DscPool.create dv dscPlInfo nil \dp ->
	Vk.DscSet.allocateDs dv (dscStInfo dp dsl) \(HPList.Singleton dss) ->
	createBffr pd dv da \ba ma ->
	createBffr pd dv db \bb mb ->
	createBffr pd dv dc \bc mc ->
	Vk.DscSet.updateDs dv
		(HPList.Singleton . U5 $ writeDscSt dss ba bb bc) HPList.Nil >>
	a dss ma mb mc

type Bffr sm sb nm t = Vk.Bffr.Binded sm sb nm '[Obj.List 256 t ""]

type Mm sm sb nm t =
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[Obj.List 256 t ""])]

dscPlInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPlInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 3 }

dscStInfo :: Vk.DscPool.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSet.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscStInfo dpl dsl = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = TMaybe.N,
	Vk.DscSet.allocateInfoDescriptorPool = dpl,
	Vk.DscSet.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }

createBffr :: forall sd nm w a . Storable w =>
	Vk.PhDvc.P -> Vk.Dvc.D sd -> V.Vector w ->
	(forall sm sb . Bffr sm sb nm w -> Mm sm sb nm w -> IO a) -> IO a
createBffr pd dv xs a =
	Vk.Bffr.create dv (bffrInfo xs) nil \bf ->
	mmInfo pd dv bf >>= \mmi ->
	Vk.Mm.allocateBind dv
		(HPList.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil
		\(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	Vk.Mm.write @nm @(Obj.List 256 w "") @0 dv mm def xs >> a bnd mm

bffrInfo :: Storable w =>
	V.Vector w -> Vk.Bffr.CreateInfo 'Nothing '[Obj.List 256 w ""]
bffrInfo xs = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths =
		Obj.LengthList (fromIntegral $ V.length xs) :** HPList.Nil,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

mmInfo :: Vk.PhDvc.P -> Vk.Dvc.D sd ->
	Vk.Bffr.B sb nm objs -> IO (Vk.Mm.AllocateInfo 'Nothing)
mmInfo pd dv b = do
	rqs <- Vk.Bffr.getMemoryRequirements dv b
	mti <- findMmTpIdx pd rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mti }

findMmTpIdx :: Vk.PhDvc.P ->
	Vk.Mm.Requirements -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmTpIdx pd rqs wt = do
	prs <- Vk.PhDvc.getMemoryProperties pd
	let	rqtps = Vk.Mm.requirementsMemoryTypeBits rqs
		wttps = (fst <$>)
			. filter (checkBits wt . Vk.Mm.mTypePropertyFlags . snd)
			$ Vk.PhDvc.memoryPropertiesMemoryTypes prs
	case filter (`Vk.Mm.elemTypeIndex` rqtps) wttps of
		[] -> error "No available memory types"; i : _ -> pure i

writeDscSt ::
	forall sds slbts sm1 sm2 sm3 sb1 sb2 sb3 os1 os2 os3 . (
	Show (HPList.PL Obj.Length os1),
	Show (HPList.PL Obj.Length os2),
	Show (HPList.PL Obj.Length os3),
	Obj.OffsetRange' (Obj.List 256 W1 "") os1 0,
	Obj.OffsetRange' (Obj.List 256 W2 "") os2 0,
	Obj.OffsetRange' (Obj.List 256 W3 "") os3 0 ) =>
	Vk.DscSet.D sds slbts ->
	Vk.Bffr.Binded sm1 sb1 "" os1 -> Vk.Bffr.Binded sm2 sb2 "" os2 ->
	Vk.Bffr.Binded sm3 sb3 "" os3 ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm1, sb1, "", Obj.List 256 W1 "", 0),
		'(sm2, sb2, "", Obj.List 256 W2 "", 0),
		'(sm3, sb3, "", Obj.List 256 W3 "", 0) ]) 0
writeDscSt ds ba bb bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(Obj.List 256 W1 "") ba) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(Obj.List 256 W2 "") bb) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(Obj.List 256 W3 "") bc) :**
		HPList.Nil }

-- CALC

calc :: forall slbts sl bts sd sds . (
	slbts ~ '(sl, bts),
	Show (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	InfixIndex '[slbts] '[slbts]) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.DscStLyt.D sl bts ->
	Vk.DscSet.D sds slbts -> Word32 -> IO ()
calc dv qfi dsl dss sz =
	Vk.Ppl.Lyt.create dv (pplLayoutInfo dsl) nil \pl ->
	Vk.Ppl.Cmpt.createCs dv Nothing (HPList.Singleton . U4 $ cmpPplInfo pl)
		nil \(cppl :** HPList.Nil) ->
	Vk.CmdPl.create dv (cmdPlInfo qfi) nil \cpl ->
	Vk.CmdBuf.allocate dv (cmdBffrInfo cpl) \(cb :*. HPList.Nil) ->
	run dv qfi cb pl cppl dss sz

pplLayoutInfo :: Vk.DscStLyt.D sl bts -> Vk.Ppl.Lyt.CreateInfo
	'Nothing '[ '(sl, bts)] ('Vk.PushConstant.Layout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

cmdPlInfo :: Vk.QFam.Index -> Vk.CmdPl.CreateInfo 'Nothing
cmdPlInfo qfi = Vk.CmdPl.CreateInfo {
	Vk.CmdPl.createInfoNext = TMaybe.N,
	Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
	Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

cmdBffrInfo :: Vk.CmdPl.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
cmdBffrInfo cpl = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = TMaybe.N,
	Vk.CmdBuf.allocateInfoCommandPool = cpl,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

run :: forall slbtss slbts sl bts sd sc spl sg sds . (
	slbtss ~ '[slbts], slbts ~ '(sl, bts),
	Show (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.Cmd.LayoutArgListOnlyDynamics slbtss ~ '[ '[ '[]]],
	InfixIndex slbtss slbtss ) =>
	Vk.Dvc.D sd -> Vk.QFam.Index -> Vk.CmdBuf.C sc ->
	Vk.Ppl.Lyt.P spl slbtss '[] -> Vk.Ppl.Cmpt.C sg '(spl, slbtss, '[]) ->
	Vk.DscSet.D sds slbts -> Word32 -> IO ()
run dv qfi cb pl cppl dss sz = Vk.Dvc.getQueue dv qfi 0 >>= \q -> do
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute cppl \ccb ->
		Vk.Cmd.bindDescriptorSetsCompute ccb pl
			(HPList.Singleton $ U2 dss)
			(HPList.Singleton2 HPList.Nil) >>
		Vk.Cmd.dispatch ccb sz 1 1
	Vk.Queue.submit q (HPList.Singleton $ U4 sinfo) Nothing
	Vk.Queue.waitIdle q
	where sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

-- COMPUTE PIPELINE INFO

cmpPplInfo :: Vk.Ppl.Lyt.P sl sbtss '[] ->
	Vk.Ppl.Cmpt.CreateInfo 'Nothing
		'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[])
		'(sl, sbtss, '[]) sbph
cmpPplInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U5 shdrStInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shdrStInfo :: Vk.Ppl.ShaderSt.CreateInfo
	'Nothing 'Nothing 'GlslComputeShader 'Nothing '[]
shdrStInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlags = def,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (minfo, nil),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Nothing }
	where minfo = Vk.ShaderMod.CreateInfo {
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
