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
import qualified Gpu.Vulkan.Instance as Vk.Instance
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QueueFamily
import qualified Gpu.Vulkan.QueueFamily.EnumManual as Vk.QueueFamily
import qualified Gpu.Vulkan.Device.Queue as Vk.Device.Queue
import qualified Gpu.Vulkan.Device.Queue.Enum as Vk.Device.Queue
import qualified Gpu.Vulkan.Device as Vk.Device
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
import qualified Gpu.Vulkan.ShaderModule as Vk.Shader.Module
import qualified Gpu.Vulkan.DescriptorSetLayout.Enum as Vk.DscSetLyt
import qualified Gpu.Vulkan.Pipeline.Enum as Vk.Ppl
import qualified Gpu.Vulkan.Pipeline.Layout as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.Layout.Type as Vk.Ppl.Lyt
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Ppl.ShaderStage
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Ppl.Cmpt
import qualified Gpu.Vulkan.DescriptorSet as Vk.DscSet
import qualified Gpu.Vulkan.CommandBuffer as Vk.CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer.Type as Vk.CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer.Enum as Vk.CommandBuffer
import qualified Gpu.Vulkan.Command as Vk.Cmd

import qualified Gpu.Vulkan.Buffer as Vk.Buffer
import qualified Gpu.Vulkan.Device.Memory.Buffer as Vk.Device.Memory.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DscSetLyt
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.DscSetLyt

main :: IO ()
main = withDevice \phdvc qFam device -> withDescriptorPool device \dscPool ->
	Vk.DscSetLyt.create
		device dscSetLayoutInfo nil nil
		\(dscSetLayout :: Vk.DscSetLyt.L sl bts) ->
	let	pipelineLayoutInfo :: Vk.Ppl.Lyt.CreateInfo () '[ '(sl, bts)]
		pipelineLayoutInfo = Vk.Ppl.Lyt.CreateInfo {
			Vk.Ppl.Lyt.createInfoNext = Nothing,
			Vk.Ppl.Lyt.createInfoFlags = def,
			Vk.Ppl.Lyt.createInfoSetLayouts =
				Vk.Ppl.Lyt.Layout dscSetLayout :...: HVNil,
			Vk.Ppl.Lyt.createInfoPushConstantRanges = [] } in
	Vk.Ppl.Lyt.create device pipelineLayoutInfo nil nil \pipelineLayout ->
	let	computePipelineInfo = Vk.Ppl.Cmpt.CreateInfo {
			Vk.Ppl.Cmpt.createInfoNext = Nothing,
			Vk.Ppl.Cmpt.createInfoFlags = def,
			Vk.Ppl.Cmpt.createInfoStage = shaderStageInfo,
			Vk.Ppl.Cmpt.createInfoLayout = pipelineLayout,
			Vk.Ppl.Cmpt.createInfoBasePipelineHandle = Nothing,
			Vk.Ppl.Cmpt.createInfoBasePipelineIndex = Nothing } in
	Vk.Ppl.Cmpt.createCs @'[ '((), _, _)] @() @() @() device Nothing
		(Vk.Ppl.Cmpt.CreateInfo_ computePipelineInfo :...: HVNil)
		nil nil \pipelines ->
	let	dscSetInfo = Vk.DscSet.AllocateInfo' {
			Vk.DscSet.allocateInfoNext' = Nothing,
			Vk.DscSet.allocateInfoDescriptorPool' = dscPool,
			Vk.DscSet.allocateInfoSetLayouts' =
				Vk.DscSet.Layout dscSetLayout :...: HVNil } in
	Vk.DscSet.allocateSs' @() device dscSetInfo >>= \(dscSet :...: HVNil) ->
	storageBufferNew3 device phdvc datA datB datC
			\((bufA, memA), (bufB, memB), (bufC, memC)) -> do
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
				Vk.DscSet.BufferInfos descBufferInfos }
	Vk.DscSet.updateDs @() @() device
		(Vk.DscSet.Write_ writeDescSet :...: HVNil) []
	queue <- Vk.Device.getQueue device qFam 0
	Vk.CommandPool.create device (mkCommandPoolInfo qFam) nil nil
		$ withCommandPool device queue
			dscSet (memA, memB, memC) pipelines pipelineLayout
	where shaderStageInfo = Vk.Ppl.ShaderStage.CreateInfo {
		Vk.Ppl.ShaderStage.createInfoNext = Nothing,
		Vk.Ppl.ShaderStage.createInfoFlags = def,
		Vk.Ppl.ShaderStage.createInfoStage = Vk.ShaderStageComputeBit,
		Vk.Ppl.ShaderStage.createInfoModule = shaderModule,
		Vk.Ppl.ShaderStage.createInfoName = "main",
		Vk.Ppl.ShaderStage.createInfoSpecializationInfo = Nothing }

dscSetLayoutInfo :: Vk.DscSetLyt.CreateInfo ()
	'[ 'Vk.DscSetLyt.Buffer '[ 'List W1, 'List W2, 'List W3]]
dscSetLayoutInfo = Vk.DscSetLyt.CreateInfo {
	Vk.DscSetLyt.createInfoNext = Nothing,
	Vk.DscSetLyt.createInfoFlags =
		Vk.DscSetLyt.CreateFlagsZero,
	Vk.DscSetLyt.createInfoBindings = binding0 :...: HVNil }
	where binding0 = Vk.DscSetLyt.BindingBuffer {
		Vk.DscSetLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscSetLyt.bindingBufferStageFlags =
			Vk.ShaderStageComputeBit }

type Mems sm1 sm2 sm3 = (
	Vk.Device.Memory.Buffer.M sm1 '[ '[ 'List W1]],
	Vk.Device.Memory.Buffer.M sm2 '[ '[ 'List W2]],
	Vk.Device.Memory.Buffer.M sm3 '[ '[ 'List W3]] )

mkCommandPoolInfo :: Vk.QueueFamily.Index -> Vk.CommandPool.CreateInfo ()
mkCommandPoolInfo qFam = Vk.CommandPool.CreateInfo {
	Vk.CommandPool.createInfoNext = Nothing,
	Vk.CommandPool.createInfoFlags =
		Vk.CommandPool.CreateResetCommandBufferBit,
	Vk.CommandPool.createInfoQueueFamilyIndex = qFam }

withCommandPool ::
	Vk.Device.D sd -> Vk.Queue.Q -> Vk.DscSet.S' sd sp slbts ->
	Mems sm1 sm2 sm3 -> [Vk.Ppl.Cmpt.C sg] -> Vk.Ppl.Lyt.L s ->
	Vk.CommandPool.C sc -> IO ()
withCommandPool device
	queue dscSet (memA, memB, memC) pipelines pipelineLayout commandPool = do
	let	commandBufferInfo = Vk.CommandBuffer.AllocateInfo {
			Vk.CommandBuffer.allocateInfoNext = Nothing,
			Vk.CommandBuffer.allocateInfoCommandPool = commandPool,
			Vk.CommandBuffer.allocateInfoLevel =
				Vk.CommandBuffer.LevelPrimary,
			Vk.CommandBuffer.allocateInfoCommandBufferCount = 1 }
	Vk.CommandBuffer.allocate @() device commandBufferInfo
		\commandBuffers -> case commandBuffers of
		[commandBuffer] -> do
			print commandBuffer
			Vk.CommandBuffer.begin @() @() commandBuffer def do
				Vk.Cmd.bindPipelineCompute commandBuffer
					Vk.Ppl.BindPointCompute $ head pipelines
				Vk.Cmd.bindDescriptorSets
					((\(Vk.CommandBuffer.C c) -> c) commandBuffer)
					Vk.Ppl.BindPointCompute
					((\(Vk.Ppl.Lyt.L l) -> l) pipelineLayout)
					0
					[(\(Vk.DscSet.S' s) -> s) dscSet]
					[]
				Vk.Cmd.dispatch commandBuffer dataSize 1 1
			let	submitInfo = Vk.SubmitInfo {
					Vk.submitInfoNext = Nothing,
					Vk.submitInfoWaitSemaphoreDstStageMasks = [],
					Vk.submitInfoCommandBuffers = [commandBuffer],
					Vk.submitInfoSignalSemaphores = [] }
			Vk.Queue.submit @() queue [submitInfo] Nothing
			Vk.Queue.waitIdle queue
			print =<< take 20 . (unW1 <$>)
				<$> Vk.Device.Memory.Buffer.read @[W1] @('List W1)
					device memA Vk.Memory.M.MapFlagsZero
			print =<< take 20 . (unW2 <$>)
				<$> Vk.Device.Memory.Buffer.read @[W2] @('List W2)
					device memB Vk.Memory.M.MapFlagsZero
			print =<< take 20 . (unW3 <$>)
				<$> Vk.Device.Memory.Buffer.read @[W3] @('List W3)
					device memC Vk.Memory.M.MapFlagsZero
		_ -> error "never occur"

withDevice :: (forall sd .
	Vk.PhysicalDevice.P -> Vk.QueueFamily.Index -> Vk.Device.D sd ->
	IO a) -> IO a
withDevice f = Vk.Instance.create @() @() def nil nil \inst -> do
	phdvc <- head <$> Vk.PhysicalDevice.enumerate inst
	qFam <- findQueueFamily phdvc Vk.Queue.ComputeBit
	Vk.Device.create phdvc (mkDeviceInfo qFam) nil nil $ f phdvc qFam
	where
	mkDeviceInfo :: Vk.QueueFamily.Index -> Vk.Device.CreateInfo () ()
	mkDeviceInfo qFam = Vk.Device.CreateInfo {
		Vk.Device.createInfoNext = Nothing,
		Vk.Device.createInfoFlags = Vk.Device.CreateFlagsZero,
		Vk.Device.createInfoQueueCreateInfos = [queueInfo qFam],
		Vk.Device.createInfoEnabledLayerNames = [],
		Vk.Device.createInfoEnabledExtensionNames = [],
		Vk.Device.createInfoEnabledFeatures = Nothing }
	queueInfo qFam = Vk.Device.Queue.CreateInfo {
		Vk.Device.Queue.createInfoNext = Nothing,
		Vk.Device.Queue.createInfoFlags =
			Vk.Device.Queue.CreateFlagsZero,
		Vk.Device.Queue.createInfoQueueFamilyIndex = qFam,
		Vk.Device.Queue.createInfoQueuePriorities = [0] }

withDescriptorPool ::
	Vk.Device.D sd -> (forall s . Vk.DscPool.P s -> IO a) -> IO a
withDescriptorPool dvc f = do
	let	poolSize = Vk.DscPool.Size {
			Vk.DscPool.sizeType = Vk.Dsc.TypeStorageBuffer,
			Vk.DscPool.sizeDescriptorCount = 10 }
		descPoolInfo = Vk.DscPool.CreateInfo {
			Vk.DscPool.createInfoNext = Nothing,
			Vk.DscPool.createInfoFlags =
				Vk.DscPool.CreateFreeDescriptorSetBit,
			Vk.DscPool.createInfoMaxSets = 1,
			Vk.DscPool.createInfoPoolSizes = [poolSize] }
	Vk.DscPool.create @() dvc descPoolInfo nil nil f

dataSize :: Integral n => n
dataSize = 1000000

datA :: V.Vector W1
datA = V.replicate dataSize $ W1 3

datB :: V.Vector W2
datB = V.fromList $ W2 <$> [1 .. dataSize]

datC :: V.Vector W3
datC = V.replicate dataSize $ W3 0

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable)

storageBufferNew :: forall sd w a . (Show w, Storable w) =>
	Vk.Device.D sd -> Vk.PhysicalDevice.P -> V.Vector w -> (
		forall sb sm .
		Vk.Buffer.Binded sb sm '[ 'List w]  ->
		Vk.Device.Memory.Buffer.M sm '[ '[ 'List w]] -> IO a ) ->
	IO a
storageBufferNew dvc phdvc xs f = do
	let	bInfo :: Vk.Buffer.CreateInfo () '[ 'List w]
		bInfo = Vk.Buffer.CreateInfo {
			Vk.Buffer.createInfoNext = Nothing,
			Vk.Buffer.createInfoFlags =
				Vk.Buffer.CreateFlagsZero,
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
		let	memoryInfo = Vk.Device.Memory.Buffer.AllocateInfo {
				Vk.Device.Memory.Buffer.allocateInfoNext = Nothing,
				Vk.Device.Memory.Buffer.allocateInfoMemoryTypeIndex =
					memoryTypeIndex }
		Vk.Buffer.allocateBind @() dvc (buffer :...: HVNil) memoryInfo
			nil nil \(binded :...: HVNil) memory -> do
			Vk.Device.Memory.Buffer.write @_ @('List w) dvc memory
				Vk.Memory.M.MapFlagsZero xs
			f binded memory

storageBufferNew3 :: (
	Show w1, Show w2, Show w3,
	Storable w1, Storable w2, Storable w3 ) =>
	Vk.Device.D sd -> Vk.PhysicalDevice.P ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 . (
			(	Vk.Buffer.Binded sb1 sm1 '[ 'List w1],
				Vk.Device.Memory.Buffer.M sm1 '[ '[ 'List w1]] ),
			(	Vk.Buffer.Binded sb2 sm2 '[ 'List w2],
				Vk.Device.Memory.Buffer.M sm2 '[ '[ 'List w2]] ),
			(	Vk.Buffer.Binded sb3 sm3 '[ 'List w3],
				Vk.Device.Memory.Buffer.M sm3 '[ '[ 'List w3]] ) ) ->
		IO a ) ->
	IO a
storageBufferNew3 dvc phdvc xs1 xs2 xs3 f =
	storageBufferNew dvc phdvc xs1 \b1 m1 ->
	storageBufferNew dvc phdvc xs2 \b2 m2 ->
	storageBufferNew dvc phdvc xs3 \b3 m3 ->
	f ((b1, m1), (b2, m2), (b3, m3))

findQueueFamily ::
	Vk.PhysicalDevice.P -> Vk.Queue.FlagBits -> IO Vk.QueueFamily.Index
findQueueFamily phdvc qb = do
	qFamProperties <- Vk.PhysicalDevice.getQueueFamilyProperties phdvc
	pure . fst . head $ filter ((/= zeroBits)
			. (.&. qb) . Vk.QueueFamily.propertiesQueueFlags . snd)
		qFamProperties

findMemoryTypeIndex :: Vk.PhysicalDevice.P ->
	Vk.Memory.M.Requirements -> Vk.Memory.PropertyFlags ->
	IO Vk.Memory.TypeIndex
findMemoryTypeIndex physicalDevice requirements memoryProp = do
	memoryProperties <- Vk.PhysicalDevice.getMemoryProperties physicalDevice
	let	reqTypes = Vk.Memory.M.requirementsMemoryTypeBits requirements
		memPropTypes = (fst <$>)
			. filter ((== memoryProp)
				. (.&. memoryProp)
				. Vk.Memory.M.mTypePropertyFlags . snd)
			$ Vk.PhysicalDevice.memoryPropertiesMemoryTypes
				memoryProperties
	case filter (`Vk.Memory.M.elemTypeIndex` reqTypes) memPropTypes of
		[] -> error "No available memory types"
		i : _ -> pure i

shaderModule :: Vk.Shader.Module.M () 'GlslComputeShader () ()
shaderModule = Vk.Shader.Module.M shaderModuleInfo nil nil
	where shaderModuleInfo = Vk.Shader.Module.CreateInfo {
		Vk.Shader.Module.createInfoNext = Nothing,
		Vk.Shader.Module.createInfoFlags =
			Vk.Shader.Module.CreateFlagsZero,
		Vk.Shader.Module.createInfoCode = glslComputeShaderMain }

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
