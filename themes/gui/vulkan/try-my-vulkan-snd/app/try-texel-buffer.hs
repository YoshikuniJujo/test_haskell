{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Default
import Data.Bits
import Data.List.Length
import Data.HeteroList
import Data.Word

import qualified Foreign.Storable.Generic
import qualified Data.Vector.Storable as V

import Shaderc.TH
import Tools

import Gpu.Vulkan.Base

import qualified Gpu.Vulkan as Vk
import qualified Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Instance as Vk.Instance
import qualified Gpu.Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Gpu.Vulkan.PhysicalDevice.Struct as Vk.PhysicalDevice
import qualified Gpu.Vulkan.Queue as Vk.Queue
import qualified Gpu.Vulkan.Queue.Enum as Vk.Queue
import qualified Gpu.Vulkan.QueueFamily as Vk.QueueFamily
import qualified Gpu.Vulkan.QueueFamily.EnumManual as Vk.QueueFamily
import qualified Gpu.Vulkan.Device.Queue as Vk.Device.Queue
import qualified Gpu.Vulkan.Device.Queue.Enum as Vk.Device.Queue
import qualified Gpu.Vulkan.Device as Vk.Device
import qualified Gpu.Vulkan.Device.Type as Vk.Device
import qualified Gpu.Vulkan.CommandPool as Vk.CommandPool
import qualified Gpu.Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified Gpu.Vulkan.Buffer.Enum as Vk.Buffer
import qualified Gpu.Vulkan.Memory.Tmp as Vk.Memory
import qualified Gpu.Vulkan.Memory.Enum as Vk.Memory
import qualified Gpu.Vulkan.Memory.Middle as Vk.Memory.M
import qualified Gpu.Vulkan.Descriptor.Enum as Vk.Descriptor
import qualified Gpu.Vulkan.DescriptorPool as Vk.Descriptor.Pool
import qualified Gpu.Vulkan.DescriptorPool.Enum as Vk.Descriptor.Pool
import qualified Gpu.Vulkan.ShaderModule as Vk.Shader.Module
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.Descriptor.Set.Layout
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.Descriptor.Set.Layout
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as Vk.Descriptor.Set.Layout.M
import qualified Gpu.Vulkan.DescriptorSetLayout.Enum as Vk.Descriptor.Set.Layout
import qualified Gpu.Vulkan.Pipeline.Enum as Vk.Pipeline
import qualified Gpu.Vulkan.Pipeline.Layout as Vk.Pipeline.Layout
import qualified Gpu.Vulkan.Pipeline.Layout.Type as Vk.Pipeline.Layout
import qualified Gpu.Vulkan.Pipeline.ShaderStage as Vk.Pipeline.ShaderStage
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Enum as Vk.Pipeline.ShaderStage
import qualified Gpu.Vulkan.Pipeline.Compute as Vk.Pipeline.Compute
import qualified Gpu.Vulkan.DescriptorSet as Vk.Descriptor.Set
import qualified Gpu.Vulkan.CommandBuffer as Vk.CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer.Type as Vk.CommandBuffer
import qualified Gpu.Vulkan.CommandBuffer.Enum as Vk.CommandBuffer
import qualified Gpu.Vulkan.Command as Vk.Cmd
import qualified Gpu.Vulkan.Command.Middle as Vk.Cmd.M
import qualified Gpu.Vulkan.BufferView.Middle as Vk.BufferView.M
import qualified Gpu.Vulkan.BufferView.Core as Vk.BufferView.C

import qualified Gpu.Vulkan.Khr as Vk.Khr

import qualified Gpu.Vulkan.Device.Middle.Internal as Vk.Device.M
import qualified Gpu.Vulkan.Buffer.Middle.Internal as Vk.Buffer.Middle

import qualified Old.Gpu.Vulkan.Buffer.List as Vk.Buffer.List
import qualified Old.Gpu.Vulkan.Buffer.List.Type as Vk.Buffer.List
import qualified Old.Gpu.Vulkan.Memory.List as Vk.Memory.List
import qualified Old.Gpu.Vulkan.Descriptor.List as Vk.Descriptor.List
import qualified Old.Gpu.Vulkan.DescriptorSet.List as Vk.Descriptor.Set.List
import qualified Old.Gpu.Vulkan.Buffer.List.Middle as Vk.Buffer.List.Middle

main :: IO ()
main = do
	let	instanceInfo = def {
			Vk.Instance.createInfoEnabledLayerNames =
				[Vk.Khr.validationLayerName] }
	Vk.Instance.create @() @() instanceInfo nil nil \inst -> do
		print inst
		physicalDevice <- head <$> Vk.PhysicalDevice.enumerate inst
		checkFormatProperties physicalDevice Vk.FormatR8g8b8a8Sint
		checkFormatProperties physicalDevice Vk.FormatR32g32b32a32Sfloat
		print physicalDevice
		queueFamily <-
			findQueueFamily physicalDevice Vk.Queue.ComputeBit
		print queueFamily
		let	queueInfo = Vk.Device.Queue.QueueCreateInfo {
				Vk.Device.Queue.queueCreateInfoNext = Nothing,
				Vk.Device.Queue.queueCreateInfoFlags =
					Vk.Device.Queue.CreateFlagsZero,
				Vk.Device.Queue.queueCreateInfoQueueFamilyIndex =
					queueFamily,
				Vk.Device.Queue.queueCreateInfoQueuePriorities =
					[0.0] }
			deviceInfo = Vk.Device.CreateInfo {
				Vk.Device.createInfoNext = Nothing,
				Vk.Device.createInfoFlags = zeroBits,
				Vk.Device.createInfoQueueCreateInfos =
					Singleton queueInfo,
				Vk.Device.createInfoEnabledLayerNames =
					[Vk.Khr.validationLayerName],
				Vk.Device.createInfoEnabledExtensionNames = [],
				Vk.Device.createInfoEnabledFeatures = Nothing }
		Vk.Device.create @() @'[()] physicalDevice deviceInfo nil nil
			$ withDevice physicalDevice queueFamily

checkFormatProperties :: Vk.PhysicalDevice.P -> Vk.Format -> IO ()
checkFormatProperties p f = do
	print . toBits . Vk.formatPropertiesBufferFeatures
		=<< Vk.PhysicalDevice.getFormatProperties p f

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
withCommandPool phdvc device queue commandPool = do
	print commandPool
	limits <- Vk.PhysicalDevice.propertiesLimits 
		<$> Vk.PhysicalDevice.getProperties phdvc
	let	maxGroupCountX :. _ = Vk.PhysicalDevice.limitsMaxComputeWorkGroupCount limits
	print maxGroupCountX
	print $ Vk.PhysicalDevice.limitsMaxBoundDescriptorSets limits
	let	(dataA, dataB, dataC) = makeDatas $ maxGroupCountX * 4
		dataD = V.fromList . take (fromIntegral maxGroupCountX * 4) $ cycle [123, 321, 111, 333]
	storageBufferNew4 device phdvc dataA dataB dataC dataD
		\((bufA, memA), (bufB, memB), (bufC, memC), (bufD, memD)) ->
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
			binding = Vk.Descriptor.Set.Layout.M.Binding {
				Vk.Descriptor.Set.Layout.M.bindingBinding = 0,
				Vk.Descriptor.Set.Layout.M.bindingDescriptorType =
					Vk.Descriptor.TypeStorageBuffer,
				Vk.Descriptor.Set.Layout.M.bindingDescriptorCountOrImmutableSamplers =
					Left 3,
				Vk.Descriptor.Set.Layout.M.bindingStageFlags =
					Vk.ShaderStageComputeBit }
			binding2 = Vk.Descriptor.Set.Layout.M.Binding {
				Vk.Descriptor.Set.Layout.M.bindingBinding = 1,
				Vk.Descriptor.Set.Layout.M.bindingDescriptorType =
					Vk.Descriptor.TypeStorageBuffer,
				Vk.Descriptor.Set.Layout.M.bindingDescriptorCountOrImmutableSamplers =
					Left 1,
				Vk.Descriptor.Set.Layout.M.bindingStageFlags =
					Vk.ShaderStageComputeBit }
			binding2' = Vk.Descriptor.Set.Layout.M.Binding {
				Vk.Descriptor.Set.Layout.M.bindingBinding = 1,
				Vk.Descriptor.Set.Layout.M.bindingDescriptorType =
					Vk.Descriptor.TypeStorageTexelBuffer,
				Vk.Descriptor.Set.Layout.M.bindingDescriptorCountOrImmutableSamplers =
					Left 1,
				Vk.Descriptor.Set.Layout.M.bindingStageFlags =
					Vk.ShaderStageComputeBit }
			descSetLayoutInfo = Vk.Descriptor.Set.Layout.M.CreateInfo {
				Vk.Descriptor.Set.Layout.M.createInfoNext = Nothing,
				Vk.Descriptor.Set.Layout.M.createInfoFlags =
					Vk.Descriptor.Set.Layout.CreateFlagsZero,
				Vk.Descriptor.Set.Layout.M.createInfoBindings =
					[binding, binding2'] }
		Vk.Descriptor.Set.Layout.create'' @() device descSetLayoutInfo nil nil \(Vk.Descriptor.Set.Layout.L'' descSetLayout) -> do
			let	pipelineLayoutInfo = Vk.Pipeline.Layout.CreateInfo {
					Vk.Pipeline.Layout.createInfoNext = Nothing,
					Vk.Pipeline.Layout.createInfoFlags =
						Vk.Pipeline.Layout.CreateFlagsZero,
					Vk.Pipeline.Layout.createInfoSetLayouts =
						Vk.Pipeline.Layout.Layout (Vk.Descriptor.Set.Layout.L descSetLayout)
							:...: HVNil,
					Vk.Pipeline.Layout.createInfoPushConstantRanges
						= [] }
			print descSetLayout
--			print @(Vk.Pipeline.Layout.CreateInfo () _)  pipelineLayoutInfo
			Vk.Pipeline.Layout.create @() device pipelineLayoutInfo nil nil \(Vk.Pipeline.Layout.LL pipelineLayout) -> do
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
							(\(Vk.Pipeline.Layout.L l) -> Vk.Pipeline.Layout.LL l)
							$ Vk.Pipeline.Layout.L pipelineLayout,
						Vk.Pipeline.Compute.createInfoBasePipelineHandle =
							Nothing,
						Vk.Pipeline.Compute.createInfoBasePipelineIndex =
							Nothing }
				Vk.Pipeline.Compute.createCs @'[ '((), _, _, _)] @() @() @() @_ @_ @_ @_ @_ device Nothing
					(Vk.Pipeline.Compute.CreateInfo_ computePipelineInfo :...: HVNil)
					nil nil \pipelines -> do
					print pipelines
					let	descSetInfo = Vk.Descriptor.Set.AllocateInfo'' {
							Vk.Descriptor.Set.allocateInfoNext'' = Nothing,
							Vk.Descriptor.Set.allocateInfoDescriptorPool'' =
								descPool,
							Vk.Descriptor.Set.allocateInfoSetLayouts'' =
								[Vk.Descriptor.Set.Layout.L'' descSetLayout] }
					print @(Vk.Descriptor.Set.AllocateInfo'' () _ _) descSetInfo
					descSets <- Vk.Descriptor.Set.allocateSs'' @() device descSetInfo
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
						descBufferInfos2 =
							Vk.Descriptor.List.BufferInfo bufD :...:
							HVNil
						Vk.Buffer.List.Binded
							(Vk.Buffer.List.Middle.B _ cBufD) = bufD
						mBufD = Vk.Buffer.Middle.B cBufD
						bufferViewInfo = Vk.BufferView.M.CreateInfo {
							Vk.BufferView.M.createInfoNext = Nothing,
							Vk.BufferView.M.createInfoFlags =
								Vk.BufferView.C.CreateFlagsZero,
							Vk.BufferView.M.createInfoBuffer = mBufD,
							Vk.BufferView.M.createInfoFormat =
								Vk.FormatR32g32b32a32Sfloat,
							Vk.BufferView.M.createInfoOffset = 0,
							Vk.BufferView.M.createInfoRange =
								Vk.Device.M.Size $ 4 * 4 *
									fromIntegral maxGroupCountX
							}
						Vk.Device.D mDevice = device
					bffView <- Vk.BufferView.M.create @() mDevice bufferViewInfo nil
					let	writeDescSet2 = Vk.Descriptor.Set.List.Write {
							Vk.Descriptor.Set.List.writeNext = Nothing,
							Vk.Descriptor.Set.List.writeDstSet = descSets !! 0,
							Vk.Descriptor.Set.List.writeDstBinding = 1,
							Vk.Descriptor.Set.List.writeDstArrayElement = 0,
							Vk.Descriptor.Set.List.writeDescriptorType =
								Vk.Descriptor.TypeStorageBuffer,
							Vk.Descriptor.Set.List.writeImageBufferInfoTexelBufferViews =
								Right $ Vk.Descriptor.Set.List.BufferInfos descBufferInfos2
							}
						writeDescSet2' :: Vk.Descriptor.Set.List.Write () _ _ _ '[]
						writeDescSet2' = Vk.Descriptor.Set.List.Write {
							Vk.Descriptor.Set.List.writeNext = Nothing,
							Vk.Descriptor.Set.List.writeDstSet = descSets !! 0,
							Vk.Descriptor.Set.List.writeDstBinding = 1,
							Vk.Descriptor.Set.List.writeDstArrayElement = 0,
							Vk.Descriptor.Set.List.writeDescriptorType =
								Vk.Descriptor.TypeStorageTexelBuffer,
							Vk.Descriptor.Set.List.writeImageBufferInfoTexelBufferViews =
								Right $ Vk.Descriptor.Set.List.TexelBufferViews [bffView]
							}
					print @(Vk.Descriptor.Set.List.Write () _ _ _ _) writeDescSet
					Vk.Descriptor.Set.List.updateDs @() @_ @() device (
						Vk.Descriptor.Set.List.Write_ writeDescSet :...:
						Vk.Descriptor.Set.List.Write_ writeDescSet2' :...:
						HVNil )
						[]
					let	commandBufferInfo = Vk.CommandBuffer.AllocateInfo {
							Vk.CommandBuffer.allocateInfoNext = Nothing,
							Vk.CommandBuffer.allocateInfoCommandPool = commandPool,
							Vk.CommandBuffer.allocateInfoLevel = Vk.CommandBuffer.LevelPrimary,
							Vk.CommandBuffer.allocateInfoCommandBufferCount = 1 }
					Vk.CommandBuffer.allocate @() device commandBufferInfo \commandBuffers -> case commandBuffers of
						[commandBuffer] -> do
							-- print commandBuffer
							Vk.CommandBuffer.begin @() @() commandBuffer def do
								Vk.Cmd.bindPipelineCompute commandBuffer
									Vk.Pipeline.BindPointCompute
										. Vk.Pipeline.Compute.unPipeline
										$ oneOfOne pipelines
								Vk.Cmd.M.bindDescriptorSets
									((\(Vk.CommandBuffer.C c) -> c) commandBuffer)
									Vk.Pipeline.BindPointCompute
									((\(Vk.Pipeline.Layout.L l) -> l) $ Vk.Pipeline.Layout.L pipelineLayout)
									0
									((\(Vk.Descriptor.Set.S'' s) -> s) <$> descSets)
									[]
								Vk.Cmd.dispatch commandBuffer maxGroupCountX 1 1
							let	submitInfo = Vk.SubmitInfo {
									Vk.submitInfoNext = Nothing,
									Vk.submitInfoWaitSemaphoreDstStageMasks = HVNil,
									Vk.submitInfoCommandBuffers = [commandBuffer],
									Vk.submitInfoSignalSemaphores = [] }
							Vk.Queue.submit @() queue [submitInfo] Nothing
							Vk.Queue.waitIdle queue
							print =<< take 10 <$> Vk.Memory.List.readList device memA Vk.Memory.M.MapFlagsZero
							print =<< take 10 <$> Vk.Memory.List.readList device memB Vk.Memory.M.MapFlagsZero
							print =<< take 10 <$> Vk.Memory.List.readList device memC Vk.Memory.M.MapFlagsZero
							print =<< take 10 <$> Vk.Memory.List.readList device memD Vk.Memory.M.MapFlagsZero
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

makeDatas :: Word32 -> (V.Vector Float, V.Vector Float, V.Vector Float)
makeDatas (fromIntegral -> sz) =
	(V.replicate sz 3, V.replicate sz 5, V.replicate sz 0)

storageBufferNew :: forall sd a b . (
	V.Storable a, Foreign.Storable.Generic.G a) =>
	Vk.Device.D sd -> Vk.PhysicalDevice.P -> V.Vector a -> (
		forall sb sm . 
			Vk.Buffer.List.Binded sb sm a ->
			Vk.Device.MemoryList sm a -> IO b ) -> IO b
storageBufferNew dvc phdvc xs f = do
	let	bInfo = Vk.Buffer.List.CreateInfo {
			Vk.Buffer.List.createInfoNext = Nothing,
			Vk.Buffer.List.createInfoFlags =
				Vk.Buffer.CreateFlagsZero,
			Vk.Buffer.List.createInfoLength = V.length xs,
			Vk.Buffer.List.createInfoUsage =
				Vk.Buffer.UsageStorageBufferBit .|.
				Vk.Buffer.UsageStorageTexelBufferBit,
			Vk.Buffer.List.createInfoSharingMode =
				Vk.SharingModeExclusive,
			Vk.Buffer.List.createInfoQueueFamilyIndices = [] }
	Vk.Buffer.List.create @_ @() @a dvc bInfo nil nil \buffer -> do
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

type BufferMemory sb sm a =
	(Vk.Buffer.List.Binded sb sm a, Vk.Device.MemoryList sm a)

storageBufferNew3 :: (V.Storable a, Foreign.Storable.Generic.G a) =>
	Vk.Device.D sd -> Vk.PhysicalDevice.P ->
	V.Vector a -> V.Vector a -> V.Vector a -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 . (
			BufferMemory sb1 sm1 a,
			BufferMemory sb2 sm2 a,
			BufferMemory sb3 sm3 a ) -> IO b ) -> IO b
storageBufferNew3 dvc phdvc xs1 xs2 xs3 f =
	storageBufferNew dvc phdvc xs1 \b1 m1 ->
	storageBufferNew dvc phdvc xs2 \b2 m2 ->
	storageBufferNew dvc phdvc xs3 \b3 m3 ->
	f ((b1, m1), (b2, m2), (b3, m3))

storageBufferNew4 :: (V.Storable a, Foreign.Storable.Generic.G a) =>
	Vk.Device.D sd -> Vk.PhysicalDevice.P ->
	V.Vector a -> V.Vector a -> V.Vector a ->
	V.Vector a -> (
		forall sb1 sm1 sb2 sm2 sb3 sm3 sb4 sm4 . (
			BufferMemory sb1 sm1 a,
			BufferMemory sb2 sm2 a,
			BufferMemory sb3 sm3 a,
			BufferMemory sb4 sm4 a ) -> IO b ) -> IO b
storageBufferNew4 dvc phdvc xs1 xs2 xs3 xs4 f =
	storageBufferNew3 dvc phdvc xs1 xs2 xs3 \(bm1, bm2, bm3) ->
	storageBufferNew dvc phdvc xs4 \b4 m4 ->
	f ((bm1, bm2, bm3, (b4, m4)))

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
	float val[];
} data[3];

layout(binding = 1, rgba32f) uniform imageBuffer storageTexelBuffer;

void
main()
{
	int index = int(gl_GlobalInvocationID.x);
	data[2].val[index] = data[0].val[index] + data[1].val[index];
	vec4 some = imageLoad(storageTexelBuffer, index);
	data[0].val[index] = some.x;
	data[1].val[index] = some.y;
	data[2].val[index] = some.z;
}

|]
