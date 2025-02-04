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
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.Tuple.ToolsYj
import Data.Maybe
import Data.List.Length
import Data.Vector.Storable qualified as V
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.Word
import System.Environment

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
-- PREPARE MEMORIES
-- CALC
-- COMPUTE PIPELINE INFO

---------------------------------------------------------------------------

-- MAIN

main :: IO ()
main = withDvc \pd d q cpl mgcx -> do
	di3 <- getDynIdx3
	let	da = V.fromList $ W1 <$> [1 .. mgcx]
		db = V.fromList $ W2 <$> [100, 200 .. 100 * mgcx]
		dc = V.replicate mgcx $ W3 0
	rs <- Vk.DscStLyt.create d dscStLytInfo nil \dsl ->
		createBffr3Mm3 pd d dsl da db dc \dss
			(ma :: Mm sm1 '[ '(sb1, MmBffrArg nm1 '[OList 2 W1])])
			(mb :: Mm sm2 '[ '(sb2, MmBffrArg nm2 '[OList 2 W2])])
			(mc :: Mm sm3 '[ '(sb3, MmBffrArg nm3 '[OList 2 W3])]) ->
		calc di3 d q cpl dsl dss mgcx >> (,,)
			<$> Vk.Mm.read @nm1
				@(OList 2 W1) @0 @[Maybe [W1]] d ma zeroBits
			<*> Vk.Mm.read @nm2
				@(OList 2 W2) @0 @[Maybe [W2]] d mb zeroBits
			<*> Vk.Mm.read @nm3
				@(OList 2 W3) @0 @[Maybe [W3]] d mc zeroBits
	mapTup3M_ (print . take 20) $ appTup3
		((unW1 <$>) . fromJust . head')
		((unW2 <$>) . fromJust . head')
		((unW3 <$>) . fromJust . head') rs
	putStrLn ""
	mapTup3M_ (print . take 20) $ appTup3
		((unW1 <$>) . fromJust . head' . tail')
		((unW2 <$>) . fromJust . head' . tail')
		((unW3 <$>) . fromJust . head' . tail') rs

type OList sz w = Obj.DynList sz 256 w ""
type Mm = Vk.Mm.M
type MmBffrArg = 'Vk.Mm.BufferArg

getDynIdx3 :: IO (DynIdx, DynIdx, DynIdx)
getDynIdx3 = (<$> getArgs) \case
	a : b : c : _ -> (rddi a, rddi b, rddi c); _ -> (Zero, Zero, Zero)
	where rddi = \case "1" -> One; _ -> Zero

data DynIdx = Zero | One deriving (Show, Enum)

dynIdx :: DynIdx -> Vk.Cmd.DynamicIndex o
dynIdx = Vk.Cmd.DynamicIndex . fromIntegral . fromEnum

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
	Vk.DscStLyt.createInfoBindings = HPList.Singleton bdg }
	where bdg = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBufferDynamic,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

type DscStLytArg =
	'[ 'Vk.DscStLyt.Buffer '[OList 2 W1, OList 2 W2, OList 2 W3]]

-- PREPARE MEMORIES

createBffr3Mm3 :: (
	Vk.DscSt.BindingAndArrayElemBuffer
		bts '[OList 2 W1, OList 2 W2, OList 2 W3] 0,
	Vk.DscSt.UpdateDynamicLength
		bts '[OList 2 W1, OList 2 W2, OList 2 W3],
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sl bts ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (
		forall sds sm1 sm2 sm3 sb1 sb2 sb3 .
		Vk.DscSt.D sds '(sl, bts) ->
		Mm sm1 '[ '( sb1, MmBffrArg nm1 '[OList 2 W1])] ->
		Mm sm2 '[ '( sb2, MmBffrArg nm2 '[OList 2 W2])] ->
		Mm sm3 '[ '( sb3, MmBffrArg nm3 '[OList 2 W3])] -> IO a) -> IO a
createBffr3Mm3 pd dv dsl da db dc a =
	Vk.DscPl.create dv dscPlInfo nil \dp ->
	Vk.DscSt.allocateDs dv (dscStInfo dp dsl) \(HPList.Singleton dss) ->
	bffrMm pd dv da \ba ma -> bffrMm pd dv db \bb mb ->
	bffrMm pd dv dc \bc mc ->
	Vk.DscSt.updateDs dv
		(HPList.Singleton . U5 $ writeDscStBffr3 dss ba bb bc)
		HPList.Nil >>
	a dss ma mb mc

dscPlInfo :: Vk.DscPl.CreateInfo 'Nothing
dscPlInfo = Vk.DscPl.CreateInfo {
	Vk.DscPl.createInfoNext = TMaybe.N,
	Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
	Vk.DscPl.createInfoMaxSets = 1,
	Vk.DscPl.createInfoPoolSizes = (: []) Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageBufferDynamic,
		Vk.DscPl.sizeDescriptorCount = 3 } }

dscStInfo :: Vk.DscPl.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscStInfo dpl dsl = Vk.DscSt.AllocateInfo {
	Vk.DscSt.allocateInfoNext = TMaybe.N,
	Vk.DscSt.allocateInfoDescriptorPool = dpl,
	Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }

bffrMm :: forall sd nm w a . Storable w =>
	Vk.Phd.P -> Vk.Dvc.D sd -> V.Vector w -> (forall sb sm .
		Vk.Bffr.Binded sm sb nm '[OList 2 w]  ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[OList 2 w])] ->
		IO a) -> IO a
bffrMm pd dv xs a = Vk.Bffr.create dv (bffrInfoBffr3 xs) nil \b ->
	mmInfo pd dv b >>= \mi ->
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b) mi nil
		\(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	Vk.Mm.write @nm @(OList 2 w) @0 dv mm zeroBits
		[Just xs, Just $ V.reverse xs] >> a bnd mm

bffrInfoBffr3 :: Storable w =>
	V.Vector w -> Vk.Bffr.CreateInfo 'Nothing '[OList 2 w]
bffrInfoBffr3 xs = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths =
		Obj.LengthDynList (fromIntegral $ V.length xs) :** HPList.Nil,
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
	Obj.OffsetRange (OList 2 W1) os1 0, Obj.OffsetRange (OList 2 W2) os2 0,
	Obj.OffsetRange (OList 2 W3) os3 0 ) =>
	Vk.DscSt.D sds slbts ->
	Vk.Bffr.Binded sm1 sb1 nm1 os1 -> Vk.Bffr.Binded sm2 sb2 nm2 os2 ->
	Vk.Bffr.Binded sm3 sb3 nm3 os3 ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer '[
		'(sm1, sb1, nm1, OList 2 W1, 0), '(sm2, sb2, nm2, OList 2 W2, 0),
		'(sm3, sb3, nm3, OList 2 W3, 0) ]) 0
writeDscStBffr3 ds ba bb bc = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBufferDynamic,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos $
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(OList 2 W1) ba) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(OList 2 W2) bb) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(OList 2 W3) bc) :**
		HPList.Nil }

-- CALC

calc :: forall sd scpl sds sdsl bts o0 o1 o2 . (
	BObj.SizeAlignment o0, BObj.SizeAlignment o1, BObj.SizeAlignment o2,
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[o0, o1, o2]],
	Show (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	(DynIdx, DynIdx, DynIdx) -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	Vk.DscStLyt.D sdsl bts -> Vk.DscSt.D sds '(sdsl, bts) -> Word32 -> IO ()
calc is dv q cpl dsl dss sz =
	Vk.Ppl.Lyt.create dv (pplLytInfo dsl) nil \pl ->
	Vk.Ppl.Cmpt.createCs dv Nothing (HPList.Singleton . U4 $ cmpPplInfo pl)
		nil \(cppl :** HPList.Nil) ->
	Vk.CBffr.allocateCs dv (cmdBffrInfo cpl) \(cb :*. HPList.Nil) ->
	run is q cb pl cppl dss sz

pplLytInfo :: Vk.DscStLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLytInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

cmdBffrInfo :: Vk.CmdPl.C s -> Vk.CBffr.AllocateInfo 'Nothing s '[ '()]
cmdBffrInfo cpl = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cpl,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

run :: forall sc sl sds slbts sg o0 o1 o2 . (
	BObj.SizeAlignment o0, BObj.SizeAlignment o1, BObj.SizeAlignment o2,
	Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[o0, o1, o2]]],
	Show (HPList.PL2 BObj.Length (
		Vk.DscStLyt.BindingTypeListBufferOnlyDynamics
			(TIndex.I1_2 slbts))) ) =>
	(DynIdx, DynIdx, DynIdx) ->
	Vk.Q.Q -> Vk.CBffr.C sc ->
	Vk.Ppl.Lyt.P sl '[slbts] '[] -> Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[]) ->
	Vk.DscSt.D sds slbts -> Word32 -> IO ()
run (ia, ib, ic) q cb pl cppl dss sz = do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute cppl \ccb ->
		Vk.Cmd.bindDescriptorSetsCompute
			ccb pl (HPList.Singleton $ U2 dss) idcs >>
		Vk.Cmd.dispatch ccb sz 1 1
	Vk.Q.submit q (HPList.Singleton $ U4 sinfo) Nothing
	Vk.Q.waitIdle q
	where
	sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }
	idcs = HPList.Singleton2
		$ dynIdx ia :** dynIdx ib :** dynIdx ic :** HPList.Nil

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
	Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (mdinfo, nil),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = HPList.Nil }
	where mdinfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

head' :: [a] -> a
head' = \case [] -> error "empty list"; x : _ -> x

tail' :: [a] -> [a]
tail' = \case [] -> error "empty list"; _ : xs -> xs

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
