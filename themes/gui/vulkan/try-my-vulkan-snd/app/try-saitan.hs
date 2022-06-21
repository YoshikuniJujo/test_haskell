{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Default
import Data.Bits
import Data.HeteroList
import Data.Word

import qualified Data.Vector.Storable as V

import Shaderc.TH
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
import qualified Gpu.Vulkan.Buffer.List as Vk.Buffer.List
import qualified Gpu.Vulkan.Memory as Vk.Memory
import qualified Gpu.Vulkan.Memory.Enum as Vk.Memory
import qualified Gpu.Vulkan.Memory.Middle as Vk.Memory.M
import qualified Gpu.Vulkan.Memory.List as Vk.Memory.List
import qualified Gpu.Vulkan.Descriptor.Enum as Vk.Descriptor
import qualified Gpu.Vulkan.DescriptorPool as Vk.Descriptor.Pool
import qualified Gpu.Vulkan.DescriptorPool.Enum as Vk.Descriptor.Pool
import qualified Gpu.Vulkan.ShaderModule as Vk.Shader.Module
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.Descriptor.Set.Layout
import qualified Gpu.Vulkan.DescriptorSetLayout.Enum as Vk.Descriptor.Set.Layout
import qualified Gpu.Vulkan.Pipeline.Enum as Vk.Pipeline
import qualified Gpu.Vulkan.Pipeline.Layout as Vk.Pipeline.Layout
import qualified Gpu.Vulkan.Pipeline.Layout.Type as Vk.Pipeline.Layout
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Pipeline.ShaderStage
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Enum as Vk.Pipeline.ShaderStage
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Pipeline.Compute
import qualified Gpu.Vulkan.DescriptorSet as Vk.Descriptor.Set
import qualified Gpu.Vulkan.Descriptor.List as Vk.Descriptor.List
import qualified Gpu.Vulkan.DescriptorSet.List as Vk.Descriptor.Set.List
import qualified Gpu.Vulkan.CommandBuffer as Vk.CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer.Type as Vk.CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer.Enum as Vk.CommandBuffer
import qualified Gpu.Vulkan.Command as Vk.Cmd

main :: IO ()
main = do
	let	instanceInfo = def
	Vk.Instance.create @() @() instanceInfo nil nil \inst -> do
		print inst
		physicalDevice <- head <$> Vk.PhysicalDevice.enumerate inst
		print physicalDevice
		queueFamily <-
			findQueueFamily physicalDevice Vk.Queue.ComputeBit
		print queueFamily
		let	queueInfo = Vk.Device.Queue.CreateInfo {
				Vk.Device.Queue.createInfoNext = Nothing,
				Vk.Device.Queue.createInfoFlags =
					Vk.Device.Queue.CreateFlagsZero,
				Vk.Device.Queue.createInfoQueueFamilyIndex =
					queueFamily,
				Vk.Device.Queue.createInfoQueuePriorities =
					[0.0] }
			deviceInfo = Vk.Device.CreateInfo {
				Vk.Device.createInfoNext = Nothing,
				Vk.Device.createInfoFlags =
					Vk.Device.CreateFlagsZero,
				Vk.Device.createInfoQueueCreateInfos =
					[queueInfo],
				Vk.Device.createInfoEnabledLayerNames = [],
				Vk.Device.createInfoEnabledExtensionNames = [],
				Vk.Device.createInfoEnabledFeatures = Nothing }
		Vk.Device.create @() @() physicalDevice deviceInfo nil nil
			$ withDevice physicalDevice queueFamily

withDevice ::
	Vk.PhysicalDevice.P -> Vk.QueueFamily.Index -> Vk.Device.D sd -> IO ()
withDevice phdvc queueFamily device = do
	print device
	queue <- Vk.Device.getQueue device queueFamily 0
	print queue
	let	commandPoolInfo = Vk.CommandPool.CreateInfo {
			Vk.CommandPool.createInfoNext = Nothing,
			Vk.CommandPool.createInfoFlags =
				Vk.CommandPool.CreateResetCommandBufferBit,
			Vk.CommandPool.createInfoQueueFamilyIndex =
				queueFamily }
	Vk.CommandPool.create @() device commandPoolInfo nil nil
		$ withCommandPool phdvc device queue

withCommandPool ::
	Vk.PhysicalDevice.P -> Vk.Device.D sd -> Vk.Queue.Q -> Vk.CommandPool.C sc -> IO ()
withCommandPool phdvc device queue commandPool =
	print commandPool >>
	storageBufferNew3 device phdvc dataA dataB dataC
		\((bufA, memA), (bufB, memB), (bufC, memC)) ->
	print bufA >> print memA >>
	print bufB >> print memB >>
	print bufC >> print memC >>
	createDescriptorPool device \descPool -> do
	print descPool
	let	shaderModuleInfo = Vk.Shader.Module.CreateInfo {
			Vk.Shader.Module.createInfoNext = Nothing,
			Vk.Shader.Module.createInfoFlags =
				Vk.Shader.Module.CreateFlagsZero,
			Vk.Shader.Module.createInfoCode =
				glslComputeShaderMain }
		binding = Vk.Descriptor.Set.Layout.Binding {
			Vk.Descriptor.Set.Layout.bindingBinding = 0,
			Vk.Descriptor.Set.Layout.bindingDescriptorType =
				Vk.Descriptor.TypeStorageBuffer,
			Vk.Descriptor.Set.Layout.bindingDescriptorCountOrImmutableSamplers =
				Left 3,
			Vk.Descriptor.Set.Layout.bindingStageFlags =
				Vk.ShaderStageComputeBit }
		descSetLayoutInfo = Vk.Descriptor.Set.Layout.CreateInfo {
			Vk.Descriptor.Set.Layout.createInfoNext = Nothing,
			Vk.Descriptor.Set.Layout.createInfoFlags =
				Vk.Descriptor.Set.Layout.CreateFlagsZero,
			Vk.Descriptor.Set.Layout.createInfoBindings =
				[binding] }
	Vk.Descriptor.Set.Layout.create @() device descSetLayoutInfo nil nil \descSetLayout -> do
		let	pipelineLayoutInfo = Vk.Pipeline.Layout.CreateInfo {
				Vk.Pipeline.Layout.createInfoNext = Nothing,
				Vk.Pipeline.Layout.createInfoFlags =
					Vk.Pipeline.Layout.CreateFlagsZero,
				Vk.Pipeline.Layout.createInfoSetLayouts =
					descSetLayout :...: HVNil,
				Vk.Pipeline.Layout.createInfoPushConstantRanges
					= [] }
		print descSetLayout
		print @(Vk.Pipeline.Layout.CreateInfo () _)  pipelineLayoutInfo
		Vk.Pipeline.Layout.create @() device pipelineLayoutInfo nil nil \pipelineLayout -> do
			print pipelineLayout
			let	shaderStageInfo = Vk.Pipeline.ShaderStage.CreateInfo {
					Vk.Pipeline.ShaderStage.createInfoNext = Nothing,
					Vk.Pipeline.ShaderStage.createInfoFlags =
						Vk.Pipeline.ShaderStage.CreateFlagsZero,
					Vk.Pipeline.ShaderStage.createInfoStage =
						Vk.ShaderStageComputeBit,
					Vk.Pipeline.ShaderStage.createInfoModule =
						Vk.Shader.Module.M shaderModuleInfo nil nil,
					Vk.Pipeline.ShaderStage.createInfoName = "main",
					Vk.Pipeline.ShaderStage.createInfoSpecializationInfo =
						Nothing }
				computePipelineInfo = Vk.Pipeline.Compute.CreateInfo {
					Vk.Pipeline.Compute.createInfoNext = Nothing,
					Vk.Pipeline.Compute.createInfoFlags =
						Vk.Pipeline.CreateFlagsZero,
					Vk.Pipeline.Compute.createInfoStage =
						shaderStageInfo,
					Vk.Pipeline.Compute.createInfoLayout =
						pipelineLayout,
					Vk.Pipeline.Compute.createInfoBasePipelineHandle =
						Nothing,
					Vk.Pipeline.Compute.createInfoBasePipelineIndex =
						Nothing }
			Vk.Pipeline.Compute.createCs @'[ '((), _, _)] @() @() @() @_ @_ @_ @_ @_ device Nothing
				(Vk.Pipeline.Compute.CreateInfo_ computePipelineInfo :...: HVNil)
				nil nil \pipelines -> do
				print pipelines
				let	descSetInfo = Vk.Descriptor.Set.AllocateInfo {
						Vk.Descriptor.Set.allocateInfoNext = Nothing,
						Vk.Descriptor.Set.allocateInfoDescriptorPool =
							descPool,
						Vk.Descriptor.Set.allocateInfoSetLayouts =
							[descSetLayout] }
				print @(Vk.Descriptor.Set.AllocateInfo () _ _) descSetInfo
				descSets <- Vk.Descriptor.Set.allocateSs @() device descSetInfo
				print descSets
				let	descBufferInfos =
						Vk.Descriptor.List.BufferInfo bufA :...:
						Vk.Descriptor.List.BufferInfo bufB :...:
						Vk.Descriptor.List.BufferInfo bufC :...:
						HVNil
					writeDescSet = Vk.Descriptor.Set.List.Write {
						Vk.Descriptor.Set.List.writeNext = Nothing,
						Vk.Descriptor.Set.List.writeDstSet = descSets !! 0,
						Vk.Descriptor.Set.List.writeDstBinding = 0,
						Vk.Descriptor.Set.List.writeDstArrayElement = 0,
						Vk.Descriptor.Set.List.writeDescriptorType =
							Vk.Descriptor.TypeStorageBuffer,
						Vk.Descriptor.Set.List.writeImageBufferInfoTexelBufferViews =
							Right $ Vk.Descriptor.Set.List.BufferInfos descBufferInfos
						}
				print @(Vk.Descriptor.Set.List.Write () _ _ _ _) writeDescSet
				Vk.Descriptor.Set.List.updateSs @() @_ @() device
					(Vk.Descriptor.Set.List.Write_ writeDescSet :...: HVNil)
					[]
				let	commandBufferInfo = Vk.CommandBuffer.AllocateInfo {
						Vk.CommandBuffer.allocateInfoNext = Nothing,
						Vk.CommandBuffer.allocateInfoCommandPool = commandPool,
						Vk.CommandBuffer.allocateInfoLevel = Vk.CommandBuffer.LevelPrimary,
						Vk.CommandBuffer.allocateInfoCommandBufferCount = 1 }
				Vk.CommandBuffer.allocate @() device commandBufferInfo \commandBuffers -> case commandBuffers of
					[commandBuffer] -> do
						print commandBuffer
						Vk.CommandBuffer.begin @() @() commandBuffer def do
							Vk.Cmd.bindPipelineCompute commandBuffer
								Vk.Pipeline.BindPointCompute $ head pipelines
							Vk.Cmd.bindDescriptorSets
								((\(Vk.CommandBuffer.C c) -> c) commandBuffer)
								Vk.Pipeline.BindPointCompute
								((\(Vk.Pipeline.Layout.L l) -> l) pipelineLayout)
								0
								((\(Vk.Descriptor.Set.S s) -> s) <$> descSets)
								[]
							Vk.Cmd.dispatch commandBuffer dataSize 1 1
						let	submitInfo = Vk.SubmitInfo {
								Vk.submitInfoNext = Nothing,
								Vk.submitInfoWaitSemaphoreDstStageMasks = [],
								Vk.submitInfoCommandBuffers = [commandBuffer],
								Vk.submitInfoSignalSemaphores = [] }
						Vk.Queue.submit @() queue [submitInfo] Nothing
						Vk.Queue.waitIdle queue
						print =<< take 10 <$> Vk.Memory.List.readList device memA Vk.Memory.M.MapFlagsZero
						print =<< take 10 <$> Vk.Memory.List.readList device memB Vk.Memory.M.MapFlagsZero
						print =<< take 10 <$> Vk.Memory.List.readList device memC Vk.Memory.M.MapFlagsZero
					_ -> error "never occur"

createDescriptorPool :: Vk.Device.D sd ->
	(forall s . Vk.Descriptor.Pool.P s -> IO a) -> IO a
createDescriptorPool dvc f = do
	let	poolSize = Vk.Descriptor.Pool.Size {
			Vk.Descriptor.Pool.sizeType =
				Vk.Descriptor.TypeStorageBuffer,
			Vk.Descriptor.Pool.sizeDescriptorCount = 10 }
		descPoolInfo = Vk.Descriptor.Pool.CreateInfo {
			Vk.Descriptor.Pool.createInfoNext = Nothing,
			Vk.Descriptor.Pool.createInfoFlags =
				Vk.Descriptor.Pool.CreateFreeDescriptorSetBit,
			Vk.Descriptor.Pool.createInfoMaxSets = 1,
			Vk.Descriptor.Pool.createInfoPoolSizes = [poolSize] }
	Vk.Descriptor.Pool.create @() dvc descPoolInfo nil nil f

dataSize :: Integral n => n
dataSize = 1000000

dataA, dataB, dataC :: V.Vector Word32
dataA = V.replicate dataSize 3
dataB = V.replicate dataSize 5
dataC = V.replicate dataSize 0

storageBufferNew ::
	Vk.Device.D sd -> Vk.PhysicalDevice.P -> V.Vector Word32 -> (
		forall sb sm . 
			Vk.Buffer.List.Binded sb sm Word32 ->
			Vk.Device.MemoryList sm Word32 -> IO a ) -> IO a
storageBufferNew dvc phdvc xs f = do
	let	bInfo = Vk.Buffer.List.CreateInfo {
			Vk.Buffer.List.createInfoNext = Nothing,
			Vk.Buffer.List.createInfoFlags =
				Vk.Buffer.CreateFlagsZero,
			Vk.Buffer.List.createInfoLength = V.length xs,
			Vk.Buffer.List.createInfoUsage =
				Vk.Buffer.UsageStorageBufferBit,
			Vk.Buffer.List.createInfoSharingMode =
				Vk.SharingModeExclusive,
			Vk.Buffer.List.createInfoQueueFamilyIndices = [] }
	Vk.Buffer.List.create @_ @() @Word32 dvc bInfo nil nil \buffer -> do
		print buffer
		requirements <- Vk.Buffer.List.getMemoryRequirements dvc buffer
		print requirements
		memoryTypeIndex <- findMemoryTypeIndex phdvc requirements (
			Vk.Memory.PropertyHostVisibleBit .|.
			Vk.Memory.PropertyHostCoherentBit )
		print memoryTypeIndex
		let	memoryInfo = Vk.Memory.List.AllocateInfo {
				Vk.Memory.List.allocateInfoNext = Nothing,
				Vk.Memory.List.allocateInfoMemoryTypeIndex =
					memoryTypeIndex }
		Vk.Memory.List.allocate
			@() dvc buffer memoryInfo nil nil \memory -> do
			bnd <- Vk.Buffer.List.bindMemory dvc buffer memory
			Vk.Memory.List.writeMono
				dvc memory Vk.Memory.M.MapFlagsZero xs
			f bnd memory

type BufferMemory sb sm =
	(Vk.Buffer.List.Binded sb sm Word32, Vk.Device.MemoryList sm Word32)

storageBufferNew3 ::
	Vk.Device.D sd -> Vk.PhysicalDevice.P ->
	V.Vector Word32 -> V.Vector Word32 -> V.Vector Word32 -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 . (
			BufferMemory sb1 sm1,
			BufferMemory sb2 sm2,
			BufferMemory sb3 sm3 ) -> IO a ) -> IO a
storageBufferNew3 dvc phdvc xs1 xs2 xs3 f =
	storageBufferNew dvc phdvc xs1 \b1 m1 ->
	storageBufferNew dvc phdvc xs2 \b2 m2 ->
	storageBufferNew dvc phdvc xs3 \b3 m3 ->
	f ((b1, m1), (b2, m2), (b3, m3))

findQueueFamily ::
	Vk.PhysicalDevice.P -> Vk.Queue.FlagBits -> IO Vk.QueueFamily.Index
findQueueFamily phdvc qb = do
	queueFamilyProperties <-
		Vk.PhysicalDevice.getQueueFamilyProperties phdvc
	pure . fst . head $ filter ((/= zeroBits)
		. (.&. qb) . Vk.QueueFamily.propertiesQueueFlags
		. snd) queueFamilyProperties

findMemoryTypeIndex :: Vk.PhysicalDevice.P ->
	Vk.Memory.M.Requirements -> Vk.Memory.PropertyFlags ->
	IO Vk.Memory.TypeIndex
findMemoryTypeIndex physicalDevice requirements memoryProp = do
	memoryProperties <- Vk.PhysicalDevice.getMemoryProperties physicalDevice
	print memoryProperties
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
