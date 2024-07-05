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

import Foreign.Storable
import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as BObj
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.TypeLevel.List
import Data.HeteroParList qualified as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word

import qualified Data.Vector.Storable as V

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Queue as Vk.Q
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPl
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPl
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSt
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscStLyt

import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import Data.Tuple.ToolsYj

---------------------------------------------------------------------------

-- MAIN
-- PREPARE BUFFERS AND MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- MAIN

main :: IO ()
main = withDvc \pd d q cpl mgcx -> do
	let	da = V.fromList $ W1 <$> [1 .. mgcx]
		db = V.fromList $ W2 <$> [100, 200 .. 100 * mgcx]
		dc = V.replicate mgcx $ W3 0
	rs <- Vk.DscStLyt.create d dscStLytInfo nil \dsl ->
		createBffr3Mm3 pd d dsl da db dc \dss ma mb mc ->
		calc d q cpl dsl dss mgcx >>
		(,,)	<$> Vk.Mm.read @"" @(OList W1) @0 d ma def
			<*> Vk.Mm.read @"" @(OList W2) @0 d mb def
			<*> Vk.Mm.read @"" @(OList W3) @0 d mc def
	mapTup3M_ (print . take 20)
		$ appTup3 (unW1 <$>) (unW2 <$>) (unW3 <$>) rs

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

withDvc :: (forall sd scpl .
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	(forall c . Integral c => c) -> IO a) -> IO a
withDvc a = Vk.Inst.create instInfo nil \inst -> do
	pd <- head' <$> Vk.Phd.enumerate inst
	mgcx :. _ <- Vk.Phd.limitsMaxComputeWorkGroupCount
		. Vk.Phd.propertiesLimits <$> Vk.Phd.getProperties pd
	qfi <- fst . head' . filter (
			checkBits Vk.Q.ComputeBit .
			Vk.QFam.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	Vk.Dvc.create pd (dvcInfo qfi) nil \dv ->
		Vk.Dvc.getQueue dv qfi 0 >>= \q ->
		Vk.CmdPl.create dv (cmdPlInfo qfi) nil \cpl ->
			a pd dv q cpl $ fromIntegral mgcx

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

cmdPlInfo :: Vk.QFam.Index -> Vk.CmdPl.CreateInfo 'Nothing
cmdPlInfo qfi = Vk.CmdPl.CreateInfo {
	Vk.CmdPl.createInfoNext = TMaybe.N,
	Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
	Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

dscStLytInfo :: Vk.DscStLyt.CreateInfo 'Nothing DscStLytArg
dscStLytInfo = Vk.DscStLyt.CreateInfo {
	Vk.DscStLyt.createInfoNext = TMaybe.N,
	Vk.DscStLyt.createInfoFlags = zeroBits,
	Vk.DscStLyt.createInfoBindings =
		HPList.Singleton Vk.DscStLyt.BindingBuffer {
			Vk.DscStLyt.bindingBufferDescriptorType =
				Vk.Dsc.TypeStorageBuffer,
			Vk.DscStLyt.bindingBufferStageFlags =
				Vk.ShaderStageComputeBit } }

type DscStLytArg = '[ 'Vk.DscStLyt.Buffer '[OList W1, OList W2, OList W3]]

-- PREPARE BUFFERS AND MEMORIES

createBffr3Mm3 :: (
	Vk.DscSt.BindingAndArrayElemBuffer
		bts '[OList W1, OList W2, OList W3] 0,
	Vk.DscSt.UpdateDynamicLength bts '[OList W1, OList W2, OList W3],
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sdsl bts ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (
		forall sds sm1 sb1 sm2 sb2 sm3 sb3 .
		Vk.DscSt.D sds '(sdsl, bts) ->
		Mm sm1 sb1 "" W1 -> Mm sm2 sb2 "" W2 -> Mm sm3 sb3 "" W3 ->
		IO a ) -> IO a
createBffr3Mm3 pd dv dsl da db dc a =
	Vk.DscPl.create dv dscPlInfo nil \dp ->
	Vk.DscSt.allocateDs dv (dscStInfo dp dsl) \(HPList.Singleton dss) ->
	bffr3Mm3 pd dv da db dc \(ba, ma) (bb, mb) (bc, mc) ->
	Vk.DscSt.updateDs dv (HPList.Singleton
		. U5 $ writeDscStBffr3 dss ba bb bc) HPList.Nil >>
	a dss ma mb mc

type BffrMm sm sb nm t = (Bffr sm sb nm t, Mm sm sb nm t)
type Mm sm sb nm t = Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[OList t])]
type Bffr sm sb nm t = Vk.Bffr.Binded sm sb nm '[OList t]
type OList t = Obj.List 256 t ""

dscPlInfo :: Vk.DscPl.CreateInfo 'Nothing
dscPlInfo = Vk.DscPl.CreateInfo {
	Vk.DscPl.createInfoNext = TMaybe.N,
	Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
	Vk.DscPl.createInfoMaxSets = 1,
	Vk.DscPl.createInfoPoolSizes = (: []) Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPl.sizeDescriptorCount = 3 } }

dscStInfo :: Vk.DscPl.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscStInfo dpl dsl = Vk.DscSt.AllocateInfo {
	Vk.DscSt.allocateInfoNext = TMaybe.N,
	Vk.DscSt.allocateInfoDescriptorPool = dpl,
	Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }

bffr3Mm3 :: Vk.Phd.P -> Vk.Dvc.D sd ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (
		forall sm1 sm2 sm3 sb1 sb2 sb3 .
		BffrMm sm1 sb1 nm1 W1 -> BffrMm sm2 sb2 nm2 W2 ->
		BffrMm sm3 sb3 nm3 W3 -> IO a ) -> IO a
bffr3Mm3 pd dv da db dc a =
	bffrMm pd dv da \ba ma -> bffrMm pd dv db \bb mb ->
	bffrMm pd dv dc \bc mc -> a (ba, ma) (bb, mb) (bc, mc)

bffrMm :: forall sd nm w a . Storable w =>
	Vk.Phd.P -> Vk.Dvc.D sd -> V.Vector w ->
	(forall sm sb . Bffr sm sb nm w -> Mm sm sb nm w -> IO a) -> IO a
bffrMm pd dv xs a = Vk.Bffr.create dv (bffrInfo xs) nil \b ->
	mmInfo pd dv b >>= \mmi ->
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b) mmi nil
		\(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	Vk.Mm.write @nm @(OList w) @0 dv mm def xs >> a bnd mm

bffrInfo :: Storable w => V.Vector w -> Vk.Bffr.CreateInfo 'Nothing '[OList w]
bffrInfo xs = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths =
		Obj.LengthList (fromIntegral $ V.length xs) :** HPList.Nil,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

mmInfo :: Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.B sb nm objs -> IO (Vk.Mm.AllocateInfo 'Nothing)
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
		[] -> error "No available memory types"; i : _ -> pure i

writeDscStBffr3 ::
	forall slbts sb1 sb2 sb3 sm1 sm2 sm3 os1 os2 objs3 sds . (
	Show (HPList.PL Obj.Length os1), Show (HPList.PL Obj.Length os2),
	Show (HPList.PL Obj.Length objs3),
	Obj.OffsetRange (OList W1) os1 0, Obj.OffsetRange (OList W2) os2 0,
	Obj.OffsetRange (Obj.List 256 W3 "") objs3 0
	) =>
	Vk.DscSt.D sds slbts ->
	Vk.Bffr.Binded sm1 sb1 "" os1 -> Vk.Bffr.Binded sm2 sb2 "" os2 ->
	Vk.Bffr.Binded sm3 sb3 "" objs3 ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer '[
		'(sm1, sb1, "", Obj.List 256 W1 "", 0), '(sm2, sb2, "", Obj.List 256 W2 "", 0),
		'(sm3, sb3, "", Obj.List 256 W3 "", 0) ]) 0
writeDscStBffr3 ds ba bb bc = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N,
	Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos $
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(Obj.List 256 W1 "") ba) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(Obj.List 256 W2 "") bb) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(Obj.List 256 W3 "") bc) :**
		HPList.Nil }

-- CALC

calc :: forall slbts sl bts sd sds scp . (
	slbts ~ '(sl, bts),
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	InfixIndex '[slbts] '[ '(sl, bts)]) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.D sds slbts -> Word32 -> IO ()
calc dvc q cmdPool dscSetLyt dscSet dsz =
	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dscSetLyt) nil \plyt ->
	Vk.Ppl.Cmpt.createCs dvc Nothing
		(HPList.Singleton . U4 $ computePipelineInfo plyt)
		nil \(ppl :** HPList.Nil) ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cmdPool) \(cb :*. HPList.Nil) ->
		run' q cb ppl plyt dscSet dsz

pplLayoutInfo :: Vk.DscStLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

commandBufferInfo :: Vk.CmdPl.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cmdPool = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = TMaybe.N,
	Vk.CmdBuf.allocateInfoCommandPool = cmdPool,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

run' :: forall slbts sbtss sc sg sl sds . (
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[]]],
	InfixIndex '[slbts] sbtss ) =>
	Vk.Q.Q -> Vk.CmdBuf.C sc -> Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSt.D sds slbts -> Word32 -> IO ()
run' q cb ppl plyt dscSet dsz = do
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb
				plyt (HPList.Singleton $ U2 dscSet)
				(HPList.Singleton $ HPList.Singleton HPList.Nil ::
					HPList.PL3 Vk.Cmd.DynamicIndex (Vk.Cmd.LayoutArgListOnlyDynamics sbtss))
			Vk.Cmd.dispatch ccb dsz 1 1
	Vk.Q.submit q (HPList.Singleton $ U4 submitInfo) Nothing
	Vk.Q.waitIdle q
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

head' :: [a] -> a
head' = \case [] -> error "empty list"; x : _ -> x

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
