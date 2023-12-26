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

import Control.Concurrent
import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as KObj
import Data.Default
import Data.Bits
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List
import qualified Data.HeteroParList as HL
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.Word
import Data.Char
import System.Directory

import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind
import Data.TypeLevel.ParMaybe (nil)

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.Phd
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFm
import qualified Gpu.Vulkan.Device as Vk.Dv
import qualified Gpu.Vulkan.CommandPool as Vk.CmdPool
import qualified Gpu.Vulkan.Memory as Vk.Mm
import qualified Gpu.Vulkan.Memory as Vk.Mm.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified "try-gpu-vulkan" Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.PipelineLayout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DS
import qualified Gpu.Vulkan.CommandBuffer as Vk.CBffr
import qualified Gpu.Vulkan.Cmd as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Bffr
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DSLyt

import qualified Gpu.Vulkan.PushConstant as Vk.PushConstant

import Gpu.Vulkan.PipelineCache qualified as Vk.PplCch
import Gpu.Vulkan.PipelineCache qualified as Vk.PplCch.M

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
main = withDevice \pd qfi dv ->
	readData "pipeline.cache" >>= \cch ->
	print cch >>
	Vk.PplCch.create dv (pplCchInfo cch) nil \pc -> do
	threadDelay 1000000
	print =<< Vk.PplCch.getData dv pc
	putStrLn . map (chr . fromIntegral) =<<
		Vk.DSLyt.create dv dscSetLayoutInfo nil \dslyt ->
		prepareMems pd dv dslyt \dscs m ->
		calc qfi dv pc dslyt dscs bffSize >>
		Vk.Mm.read @"" @Word32List @[Word32] dv m zeroBits
	cch' <- Vk.PplCch.getData dv pc
	print cch'
	Vk.PplCch.writeData "pipeline.cache" cch'
	cch'' <- readData "pipeline.cache"
	print cch''
	print $ cch' == cch''

--	Vk.PplCch.create dv (pplCchInfo cch') nil nil \pc' -> do

--		print =<< Vk.PplCch.getData dv pc'

--		bs3 <- BS.readFile "pipeline.cache"

--		print $ BS.length bs1
--		print $ BS.length bs2
--		let	foo = BS.zip bs1 bs2
--			bar = map (uncurry (==)) $ BS.zip bs1 bs2
--			bs3' = BS.drop 8 bs3
--	print $ zip bar foo
--	print bs1
--	print bs2
--		print $ BS.drop 8 bs3
--		print $ bs1 == bs3'
--		print $ bs2 == bs3'

readData :: FilePath -> IO Vk.PplCch.M.Data
readData fp = do
	b <- doesFileExist fp
	if b then Vk.PplCch.readData fp else pure def

pplCchInfo :: Vk.PplCch.M.Data -> Vk.PplCch.M.CreateInfo 'Nothing
pplCchInfo mid = Vk.PplCch.M.CreateInfo {
	Vk.PplCch.M.createInfoNext = TMaybe.N,
	Vk.PplCch.M.createInfoFlags = zeroBits,
	Vk.PplCch.M.createInfoInitialData = mid }

type Word32List = Obj.List 256 Word32 ""

withDevice :: (forall s . Vk.Phd.P -> Vk.QFm.Index -> Vk.Dv.D s -> IO a) -> IO a
withDevice f = Vk.Inst.create instInfo nil \inst -> do
	pd <- head <$> Vk.Phd.enumerate inst
	qfi <- fst . head . filter (
			checkBits Vk.Queue.ComputeBit .
			Vk.QFm.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	Vk.Dv.create pd (dvcInfo qfi) nil $ f pd qfi

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.layerKhronosValidation] }
	
dvcInfo :: Vk.QFm.Index -> Vk.Dv.CreateInfo 'Nothing '[ 'Nothing]
dvcInfo qfi = Vk.Dv.CreateInfo {
	Vk.Dv.createInfoNext = TMaybe.N, Vk.Dv.createInfoFlags = zeroBits,
	Vk.Dv.createInfoQueueCreateInfos = HL.Singleton qinfo,
	Vk.Dv.createInfoEnabledLayerNames = [Vk.layerKhronosValidation],
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
	Default (HL.PL (HL.PL KObj.Length)
		(Vk.DSLyt.BindingTypeListBufferOnlyDynamics bts)),
	Vk.DS.BindingAndArrayElemBuffer bts '[Word32List] 0,
	Vk.DS.UpdateDynamicLength bts '[Word32List] ) =>
	Vk.Phd.P -> Vk.Dv.D sd -> Vk.DSLyt.D sl bts ->
	(forall sds sm sb .
		Vk.DS.D sds '(sl, bts) ->
		Vk.Mm.M sm '[ '( sb, 'Vk.Mm.BufferArg "" '[Word32List])] ->
		IO a) -> IO a
prepareMems pd dv dslyt f =
	Vk.DscPool.create dv dscPoolInfo nil \dp ->
	Vk.DS.allocateDs dv (dscSetInfo dp dslyt) \(HL.Singleton ds) ->
	storageBufferNew pd dv \b m ->
	Vk.DS.updateDs dv (HL.Singleton . U5 $ writeDscSet ds b) HL.Nil >>
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

dscSetInfo :: Vk.DscPool.P sp -> Vk.DSLyt.D sl bts ->
	Vk.DS.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscSetInfo pl lyt = Vk.DS.AllocateInfo {
	Vk.DS.allocateInfoNext = TMaybe.N,
	Vk.DS.allocateInfoDescriptorPool = pl,
	Vk.DS.allocateInfoSetLayouts = HL.Singleton $ U2 lyt }

storageBufferNew :: forall sd nm a . Vk.Phd.P -> Vk.Dv.D sd -> (forall sb sm .
	Vk.Bffr.Binded sm sb nm '[Word32List]  ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[Word32List])] -> IO a) -> IO a
storageBufferNew pd dv f =
	Vk.Bffr.create dv bufferInfo nil \bf ->
	getMemoryInfo pd dv bf >>= \mmi ->
	Vk.Mm.allocateBind dv
		(HL.Singleton . U2 $ Vk.Mm.Buffer bf) mmi nil
		\(HL.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	f bnd mm

bufferInfo :: Vk.Bffr.CreateInfo 'Nothing '[Word32List]
bufferInfo = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths = HL.Singleton $ Obj.LengthList bffSize,
	Vk.Bffr.createInfoUsage = Vk.Bffr.UsageStorageBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

getMemoryInfo :: Vk.Phd.P -> Vk.Dv.D sd -> Vk.Bffr.B sb nm objs ->
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

writeDscSet :: forall slbts sb sm os sds . (
	Show (HL.PL Obj.Length os),
	Obj.OffsetRange (Obj.List 256 Word32 "") os ) =>
	Vk.DS.D sds slbts -> Vk.Bffr.Binded sm sb "" os ->
	Vk.DS.Write 'Nothing sds slbts
		('Vk.DS.WriteSourcesArgBuffer '[ '(sm, sb, "", Word32List)]) 0
writeDscSet ds ba = Vk.DS.Write {
	Vk.DS.writeNext = TMaybe.N,
	Vk.DS.writeDstSet = ds,
	Vk.DS.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DS.writeSources =
		Vk.DS.BufferInfos . HL.Singleton . U4 $ Vk.Dsc.BufferInfo ba }

-- CALC

calc :: forall slbts sl bts sd spc sds . (
	slbts ~ '(sl, bts),
	Vk.DSLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[]],
	InfixIndex '[slbts] '[slbts]) =>
	Vk.QFm.Index -> Vk.Dv.D sd -> Vk.PplCch.P spc ->
	Vk.DSLyt.D sl bts ->
	Vk.DS.D sds slbts -> Word32 -> IO ()
calc qfi dv pcch dslyt ds sz =
	Vk.Ppl.Lyt.create dv (pplLayoutInfo dslyt) nil \plyt ->
	Vk.Ppl.Cmpt.createCs dv (Just pcch)
		(HL.Singleton . U4 $ pplInfo plyt) nil \(pl :** HL.Nil) ->
	Vk.CmdPool.create dv (commandPoolInfo qfi) nil \cp ->
	Vk.CBffr.allocate dv (commandBufferInfo cp) \(cb :*. HL.Nil) ->
	run qfi dv ds cb plyt pl sz

pplLayoutInfo :: Vk.DSLyt.D sl bts ->
	Vk.Ppl.Lyt.CreateInfo 'Nothing '[ '(sl, bts)]
		('Vk.PushConstant.Layout '[] '[])
pplLayoutInfo dsl = Vk.Ppl.Lyt.CreateInfo {
	Vk.Ppl.Lyt.createInfoNext = TMaybe.N,
	Vk.Ppl.Lyt.createInfoFlags = zeroBits,
	Vk.Ppl.Lyt.createInfoSetLayouts = HL.Singleton $ U2 dsl }

commandPoolInfo :: Vk.QFm.Index -> Vk.CmdPool.CreateInfo 'Nothing
commandPoolInfo qfi = Vk.CmdPool.CreateInfo {
	Vk.CmdPool.createInfoNext = TMaybe.N,
	Vk.CmdPool.createInfoFlags = Vk.CmdPool.CreateResetCommandBufferBit,
	Vk.CmdPool.createInfoQueueFamilyIndex = qfi }

commandBufferInfo :: Vk.CmdPool.C s -> Vk.CBffr.AllocateInfo 'Nothing s '[ '()]
commandBufferInfo cmdPool = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cmdPool,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

run :: forall slbts sd sc sg sl sds . (
	Vk.Cmd.LayoutArgListOnlyDynamics '[slbts] ~ '[ '[ '[]]],
	InfixIndex '[slbts] '[slbts] ) =>
	Vk.QFm.Index -> Vk.Dv.D sd -> Vk.DS.D sds slbts -> Vk.CBffr.C sc ->
	Vk.Ppl.Lyt.P sl '[slbts] '[] ->
	Vk.Ppl.Cmpt.C sg '(sl, '[slbts], '[]) -> Word32 -> IO ()
run qfi dv ds cb lyt pl sz = Vk.Dv.getQueue dv qfi 0 >>= \q -> do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
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
