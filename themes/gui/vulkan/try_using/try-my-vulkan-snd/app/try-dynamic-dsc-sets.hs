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

import Foreign.Storable
import Gpu.Vulkan.Object.Base qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Data.Default
import Data.Bits
import Data.List.Length
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List
import qualified Data.HeteroParList as HPList
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word

import qualified Data.Vector.Storable as V

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
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt

import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import Data.Maybe

import System.Environment

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
	(r1, r2, r3) <-
		Vk.DscSetLyt.create d dscSetLayoutInfo nil \dscSetLyt ->
		prepareMems pd d dscSetLyt da db dc \dscSet ma mb mc ->
		calc' di3 d q cpl dscSetLyt dscSet mgcx >>
		(,,)	<$> Vk.Mm.read @"" @(VObj.DynList 2 256 W1 "") @0 @[Maybe [W1]] d ma def
			<*> Vk.Mm.read @"" @(VObj.DynList 2 256 W2 "") @0 @[Maybe [W2]] d mb def
			<*> Vk.Mm.read @"" @(VObj.DynList 2 256 W3 "") @0 @[Maybe [W3]] d mc def
	print . take 20 $ unW1 <$> fromJust (head' r1)
	print . take 20 $ unW2 <$> fromJust (head' r2)
	print . take 20 $ unW3 <$> fromJust (head' r3)
	putStrLn ""
	print . take 20 $ unW1 <$> fromJust (head' $ tail' r1)
	print . take 20 $ unW2 <$> fromJust (head' $ tail' r2)
	print . take 20 $ unW3 <$> fromJust (head' $ tail' r3)

head' :: [a] -> a
head' = \case [] -> error "empty list"; x : _ -> x

tail' :: [a] -> [a]
tail' = \case [] -> error "empty list"; _ : xs -> xs

getDynIdx3 :: IO (DynIdx, DynIdx, DynIdx)
getDynIdx3 = (<$> getArgs) \case
	a : b : c : _ -> (readDynIdx a, readDynIdx b, readDynIdx c)
	_ -> (Zero, Zero, Zero)

readDynIdx :: String -> DynIdx
readDynIdx = \case "1" -> One; _ -> Zero

data DynIdx = Zero | One deriving (Show, Enum)

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

cmdPlInfo :: Vk.QFam.Index -> Vk.CmdPl.CreateInfo 'Nothing
cmdPlInfo qfi = Vk.CmdPl.CreateInfo {
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

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo 'Nothing '[
	'Vk.DscSetLyt.Buffer '[VObj.DynList 2 256 W1 "",VObj.DynList 2 256 W2 "",VObj.DynList 2 256 W3 ""] ]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = TMaybe.N,
	Vk.DscSetLyt.createInfoFlags = zeroBits,
	Vk.DscSetLyt.createInfoBindings = HPList.Singleton binding }

binding :: Vk.DscSetLyt.Binding ('Vk.DscSetLyt.Buffer objs)
binding = Vk.DscSetLyt.BindingBuffer {
	Vk.DscSetLyt.bindingBufferDescriptorType = Vk.Dsc.TypeStorageBufferDynamic,
	Vk.DscSetLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }

-- PREPARE MEMORIES

prepareMems :: (
	Default (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DscSet.BindingAndArrayElemBuffer bts '[
		VObj.DynList 2 256 W1 "",VObj.DynList 2 256 W2 "",VObj.DynList 2 256 W3 "" ] 0,
	Vk.DscSet.UpdateDynamicLength bts '[
		VObj.DynList 2 256 W1 "",VObj.DynList 2 256 W2 "",VObj.DynList 2 256 W3 "" ] ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscSetLyt.D sl bts ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> (forall sds sm1 sb1 sm2 sb2 sm3 sb3 .
		Vk.DscSet.D sds '(sl, bts) ->
		Vk.Mm.M sm1 '[ '( sb1, 'Vk.Mm.BufferArg "" '[VObj.DynList 2 256 W1 ""])] ->
		Vk.Mm.M sm2 '[ '( sb2, 'Vk.Mm.BufferArg "" '[VObj.DynList 2 256 W2 ""])] ->
		Vk.Mm.M sm3 '[ '( sb3, 'Vk.Mm.BufferArg "" '[VObj.DynList 2 256 W3 ""])] -> IO a) -> IO a
prepareMems pd dvc dscSetLyt da db dc f =
	Vk.DscPool.create dvc dscPoolInfo nil \dscPool ->
	Vk.DscSet.allocateDs dvc (dscSetInfo dscPool dscSetLyt)
		\(HPList.Singleton dscSet) ->
	storageBufferNew dvc pd da \ba ma ->
	storageBufferNew dvc pd db \bb mb ->
	storageBufferNew dvc pd dc \bc mc ->
	Vk.DscSet.updateDs dvc
		(HPList.Singleton . U5 $ writeDscSet dscSet ba bb bc)
		HPList.Nil >>
	f dscSet ma mb mc

dscPoolInfo :: Vk.DscPool.CreateInfo 'Nothing
dscPoolInfo = Vk.DscPool.CreateInfo {
	Vk.DscPool.createInfoNext = TMaybe.N,
	Vk.DscPool.createInfoFlags = Vk.DscPool.CreateFreeDescriptorSetBit,
	Vk.DscPool.createInfoMaxSets = 1,
	Vk.DscPool.createInfoPoolSizes = [poolSize] }
	where poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBufferDynamic,
		Vk.DscPool.sizeDescriptorCount = 10 }

dscSetInfo :: Vk.DscPool.P sp -> Vk.DscSetLyt.D sl bts ->
	Vk.DscSet.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DscSet.AllocateInfo {
	Vk.DscSet.allocateInfoNext = TMaybe.N,
	Vk.DscSet.allocateInfoDescriptorPool = pl,
	Vk.DscSet.allocateInfoSetLayouts =
		HPList.Singleton $ U2 lyt }

storageBufferNew :: forall sd nm w a . Storable w =>
	Vk.Dvc.D sd -> Vk.Phd.P -> V.Vector w -> (forall sb sm .
		Vk.Buffer.Binded sm sb nm '[VObj.DynList 2 256 w ""]  ->
		Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[VObj.DynList 2 256 w ""])] ->
		IO a) -> IO a
storageBufferNew dvc pd xs f =
	Vk.Buffer.create dvc (bufferInfo xs) nil \bf ->
	getMemoryInfo pd dvc bf >>= \mmi ->
	Vk.Mm.allocateBind dvc
		(HPList.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil
		\(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	Vk.Mm.write @nm @(VObj.DynList 2 256 w "") @0 dvc mm def [Just xs, Just $ V.reverse xs] >> f bnd mm

bufferInfo :: Storable w => V.Vector w -> Vk.Buffer.CreateInfo 'Nothing '[VObj.DynList 2 256 w ""]
bufferInfo xs = Vk.Buffer.CreateInfo {
	Vk.Buffer.createInfoNext = TMaybe.N,
	Vk.Buffer.createInfoFlags = def,
	Vk.Buffer.createInfoLengths =
		VObj.LengthDynList (fromIntegral $ V.length xs) :** HPList.Nil,
	Vk.Buffer.createInfoUsage = Vk.Buffer.UsageStorageBufferBit,
	Vk.Buffer.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Buffer.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Buffer.B sb nm objs ->
	IO (Vk.Mem.AllocateInfo 'Nothing)
getMemoryInfo pd dvc buffer = do
	rqs <- Vk.Buffer.getMemoryRequirements dvc buffer
	mti <- findMemoryTypeIndex pd rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Mem.AllocateInfo {
		Vk.Mem.allocateInfoNext = TMaybe.N,
		Vk.Mem.allocateInfoMemoryTypeIndex = mti }

findMemoryTypeIndex ::
	Vk.Phd.P -> Vk.Mm.M.Requirements -> Vk.Mm.PropertyFlags ->
	IO Vk.Mm.M.TypeIndex
findMemoryTypeIndex pd rqs prp0 = do
	memoryProperties <- Vk.Phd.getMemoryProperties pd
	let	reqTypes = Vk.Mm.M.requirementsMemoryTypeBits rqs
		memPropTypes = (fst <$>)
			. filter (checkBits prp0
				. Vk.Mm.M.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes memoryProperties
	case filter (`Vk.Mm.M.elemTypeIndex` reqTypes) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> pure i

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)

writeDscSet ::
	forall slbts sb1 sb2 sb3 sm1 sm2 sm3 objs1 objs2 objs3 sds . (
	Show (HPList.PL VObj.Length objs1),
	Show (HPList.PL VObj.Length objs2),
	Show (HPList.PL VObj.Length objs3),
	VObj.OffsetRange ('VObj.Dynamic 2 (KObj.List 256 W1 "")) objs1 0,
	VObj.OffsetRange ('VObj.Dynamic 2 (KObj.List 256 W2 "")) objs2 0,
	VObj.OffsetRange ('VObj.Dynamic 2 (KObj.List 256 W3 "")) objs3 0
	) =>
	Vk.DscSet.D sds slbts ->
	Vk.Buffer.Binded sm1 sb1 "" objs1 -> Vk.Buffer.Binded sm2 sb2 "" objs2 ->
	Vk.Buffer.Binded sm3 sb3 "" objs3 ->
	Vk.DscSet.Write 'Nothing sds slbts ('Vk.DscSet.WriteSourcesArgBuffer '[
		'(sm1, sb1, "", VObj.DynList 2 256 W1 "", 0), '(sm2, sb2, "", VObj.DynList 2 256 W2 "", 0),
		'(sm3, sb3, "", VObj.DynList 2 256 W3 "", 0) ]) 0
writeDscSet ds ba bb bc = Vk.DscSet.Write {
	Vk.DscSet.writeNext = TMaybe.N,
	Vk.DscSet.writeDstSet = ds,
	Vk.DscSet.writeDescriptorType = Vk.Dsc.TypeStorageBufferDynamic,
	Vk.DscSet.writeSources = Vk.DscSet.BufferInfos $
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(VObj.DynList 2 256 W1 "") ba) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(VObj.DynList 2 256 W2 "") bb) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(VObj.DynList 2 256 W3 "") bc) :**
		HPList.Nil }

-- CALC

calc' :: forall sd slbts sl bts o0 o1 o2 sds scpl . (
	KObj.SizeAlignment o0, KObj.SizeAlignment o1, KObj.SizeAlignment o2,
	slbts ~ '(sl, bts),
	Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[o0, o1, o2]],
	Show (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics bts)),
	InfixIndex '[slbts] '[ '(sl, bts)] ) =>
	(DynIdx, DynIdx, DynIdx) -> Vk.Dvc.D sd ->
	Vk.Q.Q -> Vk.CmdPl.C scpl ->
	Vk.DscSetLyt.D sl bts ->
	Vk.DscSet.D sds slbts -> Word32 -> IO ()
calc' is dvc q cpl dscSetLyt dscSet dsz =

	Vk.Ppl.Lyt.create dvc (pplLayoutInfo dscSetLyt) nil \plyt ->
	Vk.Ppl.Cmpt.createCs dvc Nothing
		(HPList.Singleton . U4 $ computePipelineInfo plyt)
		nil \(ppl :** HPList.Nil) ->
	Vk.CmdBuf.allocate dvc (commandBufferInfo cpl) \(cmdBuf :*. HPList.Nil) ->
	run is q cmdBuf ppl plyt dscSet dsz

pplLayoutInfo :: Vk.DscSetLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

commandBufferInfo :: Vk.CmdPl.C s -> Vk.CmdBuf.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cpl = Vk.CmdBuf.AllocateInfo {
	Vk.CmdBuf.allocateInfoNext = TMaybe.N,
	Vk.CmdBuf.allocateInfoCommandPool = cpl,
	Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary }

run :: forall slbts sbtss sc sg sl o0 o1 o2 sds . (
	KObj.SizeAlignment o0, KObj.SizeAlignment o1, KObj.SizeAlignment o2,
	sbtss ~ '[slbts],
	Vk.Cmd.LayoutArgListOnlyDynamics sbtss ~ '[ '[ '[o0, o1, o2]]],
	Show (HPList.PL
		(HPList.PL KObj.Length)
		(Vk.DscSetLyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))),
	InfixIndex '[slbts] sbtss ) =>
	(DynIdx, DynIdx, DynIdx) ->
	Vk.Q.Q -> Vk.CmdBuf.C sc ->
	Vk.Ppl.Cmpt.C sg '(sl, sbtss, '[]) ->
	Vk.Ppl.Lyt.P sl sbtss '[] -> Vk.DscSet.D sds slbts -> Word32 -> IO ()
run (ia, ib, ic) q cb ppl pplLyt dscSet dsz = do
	Vk.CmdBuf.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute cb Vk.Ppl.BindPointCompute ppl \ccb -> do
			Vk.Cmd.bindDescriptorSetsCompute ccb
				pplLyt (HPList.Singleton $ U2 dscSet)
				(HPList.Singleton (HPList.Singleton (
					Vk.Cmd.DynamicIndex (fromIntegral $ fromEnum ia) :**
					Vk.Cmd.DynamicIndex (fromIntegral $ fromEnum ib) :**
					Vk.Cmd.DynamicIndex (fromIntegral $ fromEnum ic) :**
					HPList.Nil )) ::
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
