{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Storable
import Data.Kind.Object
import Data.Default
import Data.Bits
import Data.HeteroList
import Data.Word

import qualified Data.Vector.Storable as V

import Shaderc.TH
import Shaderc.EnumAuto
import Gpu.Vulkan.Base

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Inst
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhDvc
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QFam
import qualified Gpu.Vulkan.QueueFamily.EnumManual as Vk.QFam
import qualified Gpu.Vulkan.Device.Queue as Vk.Dvc.Queue
import qualified Gpu.Vulkan.Device.Queue.Enum as Vk.Dvc.Queue
import qualified Gpu.Vulkan.Device as Vk.Dvc
import qualified Gpu.Vulkan.CommandPool as Vk.CommandPool
import qualified Gpu.Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified Gpu.Vulkan.Buffer.Enum as Vk.Buffer
import qualified Gpu.Vulkan.Memory as Vk.Memory
import qualified Gpu.Vulkan.Memory.Enum as Vk.Memory
import qualified Gpu.Vulkan.Memory.Middle as Vk.Memory.M
import qualified Gpu.Vulkan.Descriptor as Vk.Dsc
import qualified Gpu.Vulkan.Descriptor.Enum as Vk.Dsc
import qualified Gpu.Vulkan.DescriptorPool as Vk.DscPool
import qualified Gpu.Vulkan.DescriptorPool.Enum as Vk.DscPool
import qualified Gpu.Vulkan.ShaderModule as Vk.ShaderMod
import qualified Gpu.Vulkan.DescriptorSetLayout.Enum as Vk.DscSetLyt
import qualified Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.Pipeline.Layout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.Layout.Type as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderSt
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.CommandBuffer as Vk.CmdBuf
import qualified Gpu.Vulkan.CommandBuffer.Type as Vk.CmdBuf
import qualified Gpu.Vulkan.CommandBuffer.Enum as Vk.CmdBuf
import qualified Gpu.Vulkan.Command as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Buffer
import qualified Gpu.Vulkan.Device.Memory.Buffer as Vk.Dvc.Memory.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.DscSetLyt


main :: IO ()
main = do
	(r1, r2, r3) <- calc dataSize datA datB datC
	print . take 20 $ unW1 <$> r1
	print . take 20 $ unW2 <$> r2
	print . take 20 $ unW3 <$> r3

calc :: Word32 ->
	V.Vector W1 -> V.Vector W2 -> V.Vector W3 -> IO ([W1], [W2], [W3])
calc dsz da db dc = withDevice \phdvc qFam dvc -> withDscPool dvc \dscPool ->
	Vk.DscSetLyt.create dvc dscSetLayoutInfo nil nil
		\(dscSetLayout :: Vk.DscSetLyt.L sl bts) ->
	let	pipelineLayoutInfo :: Vk.Ppl.Lyt.CreateInfo () '[ '(sl, bts)]
		pipelineLayoutInfo = Vk.Ppl.Lyt.CreateInfo {
			Vk.Ppl.Lyt.createInfoNext = Nothing,
			Vk.Ppl.Lyt.createInfoFlags = def,
			Vk.Ppl.Lyt.createInfoSetLayouts =
				Vk.Ppl.Lyt.Layout dscSetLayout :...: HVNil,
			Vk.Ppl.Lyt.createInfoPushConstantRanges = [] } in
	Vk.Ppl.Lyt.create dvc pipelineLayoutInfo nil nil \pipelineLayout ->
	let	computePipelineInfo = Vk.Ppl.Cmpt.CreateInfo {
			Vk.Ppl.Cmpt.createInfoNext = Nothing,
			Vk.Ppl.Cmpt.createInfoFlags = def,
			Vk.Ppl.Cmpt.createInfoStage = shaderStageInfo,
			Vk.Ppl.Cmpt.createInfoLayout = pipelineLayout,
			Vk.Ppl.Cmpt.createInfoBasePipelineHandle = Nothing,
			Vk.Ppl.Cmpt.createInfoBasePipelineIndex = Nothing } in
	Vk.Ppl.Cmpt.createCs @'[ '((), _, _)] @() @() @() dvc Nothing
		(Vk.Ppl.Cmpt.CreateInfo_ computePipelineInfo :...: HVNil)
		nil nil \pipelines ->
	let	dscSetInfo = Vk.DscSet.AllocateInfo' {
			Vk.DscSet.allocateInfoNext' = Nothing,
			Vk.DscSet.allocateInfoDescriptorPool' = dscPool,
			Vk.DscSet.allocateInfoSetLayouts' =
				Vk.DscSet.Layout dscSetLayout :...: HVNil } in
	Vk.DscSet.allocateSs' @() dvc dscSetInfo >>= \(dscSet :...: HVNil) ->
	storageBufferNew3 dvc phdvc da db dc
			\((bufA, memA), (bufB, memB), (bufC, memC)) ->
	let	descBufferInfos =
			(Vk.Dsc.BufferInfoList bufA ::
				Vk.Dsc.BufferInfo '(_, _, _, 'List W1)) :...:
			(Vk.Dsc.BufferInfoList bufB ::
				Vk.Dsc.BufferInfo '(_, _, _, 'List W2)) :...:
			(Vk.Dsc.BufferInfoList bufC ::
				Vk.Dsc.BufferInfo '(_, _, _, 'List W3)) :...:
			HVNil
		writeDescSet = Vk.DscSet.Write {
			Vk.DscSet.writeNext = Nothing,
			Vk.DscSet.writeDstSet = dscSet,
			Vk.DscSet.writeDescriptorType =
				Vk.Dsc.TypeStorageBuffer,
			Vk.DscSet.writeSources =
				Vk.DscSet.BufferInfos descBufferInfos } in
	Vk.DscSet.updateDs @() @() dvc
		(Vk.DscSet.Write_ writeDescSet :...: HVNil) [] >>
	Vk.Dvc.getQueue dvc qFam 0 >>= \queue ->
	Vk.CommandPool.create dvc (commandPoolInfo qFam) nil nil \cmdPool ->
	let	cmdBufInfo = Vk.CmdBuf.AllocateInfo {
			Vk.CmdBuf.allocateInfoNext = Nothing,
			Vk.CmdBuf.allocateInfoCommandPool = cmdPool,
			Vk.CmdBuf.allocateInfoLevel = Vk.CmdBuf.LevelPrimary,
			Vk.CmdBuf.allocateInfoCommandBufferCount = 1 } in
	Vk.CmdBuf.allocate @() dvc cmdBufInfo \cmdBufs -> case cmdBufs of
		[cmdBuf] -> run
			dvc queue cmdBuf (head pipelines)
			pipelineLayout dscSet dsz memA memB memC
		_ -> error "never occur"

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo ()
	'[ 'Vk.DscSetLyt.Buffer '[ 'List W1, 'List W2, 'List W3]]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = Nothing,
	Vk.DscSetLyt.createInfoFlags = def,
	Vk.DscSetLyt.createInfoBindings = binding0 :...: HVNil }
	where binding0 = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags =
			Vk.ShaderStageComputeBit }

commandPoolInfo :: Vk.QFam.Index -> Vk.CommandPool.CreateInfo ()
commandPoolInfo qFam = Vk.CommandPool.CreateInfo {
	Vk.CommandPool.createInfoNext = Nothing,
	Vk.CommandPool.createInfoFlags =
		Vk.CommandPool.CreateResetCommandBufferBit,
	Vk.CommandPool.createInfoQueueFamilyIndex = qFam }

run :: Vk.Dvc.D sd -> Vk.Queue.Q -> Vk.CmdBuf.C sc vs -> Vk.Ppl.Cmpt.C sg ->
	Vk.Ppl.Lyt.L sl -> Vk.DscSet.S' sd sp slbts -> Word32 ->
	Vk.Dvc.Memory.Buffer.M sm1 '[ '[ 'List W1]] ->
	Vk.Dvc.Memory.Buffer.M sm2 '[ '[ 'List W2]] ->
	Vk.Dvc.Memory.Buffer.M sm3 '[ '[ 'List W3]] -> IO ([W1], [W2], [W3])
run dvc queue cmdBuf ppl pipelineLayout dscSet dsz memA memB memC = do
	Vk.CmdBuf.begin @() @() cmdBuf def do
		Vk.Cmd.bindPipelineCompute cmdBuf Vk.Ppl.BindPointCompute ppl
		Vk.Cmd.bindDescriptorSets
			((\(Vk.CmdBuf.C c) -> c) cmdBuf)
			Vk.Ppl.BindPointCompute
			((\(Vk.Ppl.Lyt.L l) -> l) pipelineLayout) 0
			[(\(Vk.DscSet.S' s) -> s) dscSet] []
		Vk.Cmd.dispatch cmdBuf dsz 1 1
	Vk.Queue.submit @() queue [submitInfo] Nothing
	Vk.Queue.waitIdle queue
	(,,)	<$> Vk.Dvc.Memory.Buffer.read @[W1] @('List W1) dvc memA def
		<*> Vk.Dvc.Memory.Buffer.read @[W2] @('List W2) dvc memB def
		<*> Vk.Dvc.Memory.Buffer.read @[W3] @('List W3) dvc memC def
	where	submitInfo = Vk.SubmitInfo {
			Vk.submitInfoNext = Nothing,
			Vk.submitInfoWaitSemaphoreDstStageMasks = [],
			Vk.submitInfoCommandBuffers = [cmdBuf],
			Vk.submitInfoSignalSemaphores = [] }

withDevice ::
	(forall sd . Vk.PhDvc.P -> Vk.QFam.Index -> Vk.Dvc.D sd -> IO a) ->
	IO a
withDevice f = Vk.Inst.create @() @() def nil nil \inst -> do
	phdvc <- head <$> Vk.PhDvc.enumerate inst
	qFam <- findQueueFamily phdvc Vk.Queue.ComputeBit
	Vk.Dvc.create @() @() phdvc (dvcInfo qFam) nil nil $ f phdvc qFam
	where
	dvcInfo qFam = Vk.Dvc.CreateInfo {
		Vk.Dvc.createInfoNext = Nothing,
		Vk.Dvc.createInfoFlags = def,
		Vk.Dvc.createInfoQueueCreateInfos = [queueInfo qFam],
		Vk.Dvc.createInfoEnabledLayerNames = [],
		Vk.Dvc.createInfoEnabledExtensionNames = [],
		Vk.Dvc.createInfoEnabledFeatures = Nothing }
	queueInfo qFam = Vk.Dvc.Queue.CreateInfo {
		Vk.Dvc.Queue.createInfoNext = Nothing,
		Vk.Dvc.Queue.createInfoFlags = def,
		Vk.Dvc.Queue.createInfoQueueFamilyIndex = qFam,
		Vk.Dvc.Queue.createInfoQueuePriorities = [0] }

withDscPool :: Vk.Dvc.D sd -> (forall s . Vk.DscPool.P s -> IO a) -> IO a
withDscPool dvc = Vk.DscPool.create @() dvc descPoolInfo nil nil
	where
	descPoolInfo = Vk.DscPool.CreateInfo {
		Vk.DscPool.createInfoNext = Nothing,
		Vk.DscPool.createInfoFlags =
			Vk.DscPool.CreateFreeDescriptorSetBit,
		Vk.DscPool.createInfoMaxSets = 1,
		Vk.DscPool.createInfoPoolSizes = [poolSize] }
	poolSize = Vk.DscPool.Size {
		Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPool.sizeDescriptorCount = 10 }

storageBufferNew :: forall sd w a . (Show w, Storable w) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P -> V.Vector w -> (
		forall sb sm .
		Vk.Buffer.Binded sb sm '[ 'List w]  ->
		Vk.Dvc.Memory.Buffer.M sm '[ '[ 'List w]] -> IO a ) -> IO a
storageBufferNew dvc phdvc xs f = do
	let	bInfo :: Vk.Buffer.CreateInfo () '[ 'List w]
		bInfo = Vk.Buffer.CreateInfo {
			Vk.Buffer.createInfoNext = Nothing,
			Vk.Buffer.createInfoFlags = def,
			Vk.Buffer.createInfoLengths =
				ObjectLengthList (V.length xs) :...: HVNil,
			Vk.Buffer.createInfoUsage =
				Vk.Buffer.UsageStorageBufferBit,
			Vk.Buffer.createInfoSharingMode =
				Vk.SharingModeExclusive,
			Vk.Buffer.createInfoQueueFamilyIndices = [] }
	Vk.Buffer.create dvc bInfo nil nil \buffer -> do
		requirements <- Vk.Buffer.getMemoryRequirements dvc buffer
		memoryTypeIndex <- findMemoryTypeIndex phdvc requirements (
			Vk.Memory.PropertyHostVisibleBit .|.
			Vk.Memory.PropertyHostCoherentBit )
		let	memoryInfo = Vk.Dvc.Memory.Buffer.AllocateInfo {
				Vk.Dvc.Memory.Buffer.allocateInfoNext = Nothing,
				Vk.Dvc.Memory.Buffer.allocateInfoMemoryTypeIndex =
					memoryTypeIndex }
		Vk.Buffer.allocateBind @() dvc (buffer :...: HVNil) memoryInfo
			nil nil \(binded :...: HVNil) memory -> do
			Vk.Dvc.Memory.Buffer.write @_ @('List w) dvc memory
				Vk.Memory.M.MapFlagsZero xs
			f binded memory

storageBufferNew3 :: (
	Show w1, Show w2, Show w3,
	Storable w1, Storable w2, Storable w3 ) =>
	Vk.Dvc.D sd -> Vk.PhDvc.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 . (
			(	Vk.Buffer.Binded sb1 sm1 '[ 'List w1],
				Vk.Dvc.Memory.Buffer.M sm1 '[ '[ 'List w1]] ),
			(	Vk.Buffer.Binded sb2 sm2 '[ 'List w2],
				Vk.Dvc.Memory.Buffer.M sm2 '[ '[ 'List w2]] ),
			(	Vk.Buffer.Binded sb3 sm3 '[ 'List w3],
				Vk.Dvc.Memory.Buffer.M sm3 '[ '[ 'List w3]] ) ) ->
		IO a ) ->
	IO a
storageBufferNew3 dvc phdvc xs1 xs2 xs3 f =
	storageBufferNew dvc phdvc xs1 \b1 m1 ->
	storageBufferNew dvc phdvc xs2 \b2 m2 ->
	storageBufferNew dvc phdvc xs3 \b3 m3 ->
	f ((b1, m1), (b2, m2), (b3, m3))

findQueueFamily ::
	Vk.PhDvc.P -> Vk.Queue.FlagBits -> IO Vk.QFam.Index
findQueueFamily phdvc qb = do
	qFamProperties <- Vk.PhDvc.getQueueFamilyProperties phdvc
	pure . fst . head $ filter ((/= zeroBits)
			. (.&. qb) . Vk.QFam.propertiesQueueFlags . snd)
		qFamProperties

findMemoryTypeIndex :: Vk.PhDvc.P ->
	Vk.Memory.M.Requirements -> Vk.Memory.PropertyFlags ->
	IO Vk.Memory.TypeIndex
findMemoryTypeIndex physicalDevice requirements memoryProp = do
	memoryProperties <- Vk.PhDvc.getMemoryProperties physicalDevice
	let	reqTypes = Vk.Memory.M.requirementsMemoryTypeBits requirements
		memPropTypes = (fst <$>)
			. filter ((== memoryProp)
				. (.&. memoryProp)
				. Vk.Memory.M.mTypePropertyFlags . snd)
			$ Vk.PhDvc.memoryPropertiesMemoryTypes
				memoryProperties
	case filter (`Vk.Memory.M.elemTypeIndex` reqTypes) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> pure i


shaderStageInfo :: Vk.Ppl.ShaderSt.CreateInfo () () 'GlslComputeShader () () vs
shaderStageInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = Nothing,
	Vk.Ppl.ShaderSt.createInfoFlags = def,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = Vk.ShaderMod.M shaderModInfo nil nil,
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Nothing }
	where shaderModInfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = Nothing,
		Vk.ShaderMod.createInfoFlags = def,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

[glslComputeShader|

#version 460
layout(local_size_x = 1, local_size_y = 1) in;
layout(binding = 0) buffer Data {
	uint val[];
} data[3];

void
main()
{
	int index = int(gl_GlobalInvocationID.x);
	data[2].val[index] = data[0].val[index] + data[1].val[index];
}

|]

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

dataSize :: Integral n => n
dataSize = 1000000

datA :: V.Vector W1; datA = V.replicate dataSize $ W1 3
datB :: V.Vector W2; datB = V.fromList $ W2 <$> [1 .. dataSize]
datC :: V.Vector W3; datC = V.replicate dataSize $ W3 0
