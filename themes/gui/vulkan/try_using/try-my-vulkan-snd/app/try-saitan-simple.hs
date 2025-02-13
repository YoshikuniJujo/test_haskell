{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Storable
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.Tuple.ToolsYj
import Data.List.Length
import Data.Vector.Storable qualified as V
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.Word

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
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt

---------------------------------------------------------------------------

-- MAIN
-- PREPARE BUFFERS AND MEMORIES
-- CALC

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

writeDscStBffr3 :: forall
	sds slbts sm1 sm2 sm3 sb1 sb2 sb3 nm1 nm2 nm3 os1 os2 os3 . (
	Show (HPList.PL Obj.Length os1), Show (HPList.PL Obj.Length os2),
	Show (HPList.PL Obj.Length os3),
	Obj.OffsetRange (OList W1) os1 0, Obj.OffsetRange (OList W2) os2 0,
	Obj.OffsetRange (OList W3) os3 0 ) =>
	Vk.DscSt.D sds slbts ->
	Vk.Bffr.Binded sm1 sb1 nm1 os1 -> Vk.Bffr.Binded sm2 sb2 nm2 os2 ->
	Vk.Bffr.Binded sm3 sb3 nm3 os3 ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer '[
		'(sm1, sb1, nm1, OList W1, 0), '(sm2, sb2, nm2, OList W2, 0),
		'(sm3, sb3, nm3, OList W3, 0) ]) 0
writeDscStBffr3 ds ba bb bc = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos $
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(OList W1) ba) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(OList W2) bb) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(OList W3) bc) :**
		HPList.Nil }

-- CALC

calc :: forall sd scpl sds slbts sl bts . (
	slbts ~ '(sl, bts),
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]] ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	Vk.DscStLyt.D sl bts -> Vk.DscSt.D sds slbts -> Word32 -> IO ()
calc dv q cpl dsl dss sz =
	Vk.Ppl.Lyt.create dv (pplLytInfo dsl) nil \pl ->
	Vk.Ppl.Cmpt.createCs dv Nothing (HPList.Singleton . U4 $ cmpPplInfo pl)
		nil \(cppl :** HPList.Nil) ->
	Vk.CBffr.allocateCs dv (cmdBffrInfo cpl) \(cb :*. HPList.Nil) ->
	run q cb pl cppl dss sz

pplLytInfo :: Vk.DscStLyt.D sl bts -> Vk.Ppl.Lyt.CreateInfo
	'Nothing '[ '(sl, bts)] ('Vk.PushConstant.Layout '[] '[])
pplLytInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

cmpPplInfo :: Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.Ppl.Cmpt.CreateInfo 'Nothing
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
	Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (mdinfo, nil),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = HPList.Nil }
	where mdinfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

cmdBffrInfo :: Vk.CmdPl.C s -> Vk.CBffr.AllocateInfo 'Nothing s '[ '()]
cmdBffrInfo cpl = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cpl,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

run :: forall sc sg spl sds slbts . (
	Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]] ) =>
	Vk.Q.Q -> Vk.CBffr.C sc -> Vk.Ppl.Lyt.P spl '[slbts] '[] ->
	Vk.Ppl.Cmpt.C sg '(spl, '[slbts], '[]) ->
	Vk.DscSt.D sds slbts -> Word32 -> IO ()
run q cb pl cppl dss sz = do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute cppl \ccb ->
		Vk.Cmd.bindDescriptorSetsCompute
			ccb pl (HPList.Singleton $ U2 dss)
			(HPList.Singleton2 HPList.Nil) >>
		Vk.Cmd.dispatch ccb sz 1 1
	Vk.Q.submit q (HPList.Singleton $ U4 sinfo) Nothing
	Vk.Q.waitIdle q
	where sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

head' :: [a] -> a
head' = \case [] -> error "empty list"; x : _ -> x

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
