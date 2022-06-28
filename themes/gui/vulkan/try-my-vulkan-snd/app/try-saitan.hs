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
import qualified Gpu.Vulkan.Descriptor as Vk.Descriptor
import qualified Gpu.Vulkan.Descriptor.Enum as Vk.Descriptor
import qualified Gpu.Vulkan.DescriptorPool as Vk.Descriptor.Pool
import qualified Gpu.Vulkan.DescriptorPool.Enum as Vk.Descriptor.Pool
import qualified Gpu.Vulkan.ShaderModule as Vk.Shader.Module
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

import qualified Gpu.Vulkan.Buffer as Vk.Buffer
import qualified Gpu.Vulkan.Device.Memory.Buffer as Vk.Device.Memory.Buffer
import qualified Gpu.Vulkan.DescriptorSetLayout as Vk.DescriptorSetLayout
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Vk.DescriptorSetLayout

main :: IO ()
main = Vk.Instance.create @() @() def nil nil \inst -> do
	physicalDevice <- head <$> Vk.PhysicalDevice.enumerate inst
	queueFamily <- findQueueFamily physicalDevice Vk.Queue.ComputeBit
	let	queueInfo = Vk.Device.Queue.CreateInfo {
			Vk.Device.Queue.createInfoNext = Nothing,
			Vk.Device.Queue.createInfoFlags =
				Vk.Device.Queue.CreateFlagsZero,
			Vk.Device.Queue.createInfoQueueFamilyIndex =
				queueFamily,
			Vk.Device.Queue.createInfoQueuePriorities = [0.0] }
		deviceInfo = Vk.Device.CreateInfo {
			Vk.Device.createInfoNext = Nothing,
			Vk.Device.createInfoFlags = Vk.Device.CreateFlagsZero,
			Vk.Device.createInfoQueueCreateInfos = [queueInfo],
			Vk.Device.createInfoEnabledLayerNames = [],
			Vk.Device.createInfoEnabledExtensionNames = [],
			Vk.Device.createInfoEnabledFeatures = Nothing }
	Vk.Device.create @() @() physicalDevice deviceInfo nil nil
		$ withDevice physicalDevice queueFamily

dscSetLayoutInfo :: Vk.DescriptorSetLayout.CreateInfo ()
	'[ 'Vk.DescriptorSetLayout.Buffer '[ 'List W1, 'List W2, 'List W3]]
dscSetLayoutInfo = Vk.DescriptorSetLayout.CreateInfo {
	Vk.DescriptorSetLayout.createInfoNext = Nothing,
	Vk.DescriptorSetLayout.createInfoFlags =
		Vk.Descriptor.Set.Layout.CreateFlagsZero,
	Vk.DescriptorSetLayout.createInfoBindings = binding0 :...: HVNil }
	where
	binding0 = Vk.DescriptorSetLayout.BindingBuffer {
		Vk.DescriptorSetLayout.bindingBufferDescriptorType =
			Vk.Descriptor.TypeStorageBuffer,
		Vk.DescriptorSetLayout.bindingBufferStageFlags =
			Vk.ShaderStageComputeBit }

withDevice ::
	Vk.PhysicalDevice.P -> Vk.QueueFamily.Index -> Vk.Device.D sd -> IO ()
withDevice phdvc queueFamily device = createDescriptorPool device \dscPool -> do
	Vk.DescriptorSetLayout.create' @()
		device dscSetLayoutInfo nil nil \dscSetLayout -> do
		let	pipelineLayoutInfo :: Vk.Pipeline.Layout.CreateInfo () '[] _
			pipelineLayoutInfo = Vk.Pipeline.Layout.CreateInfo {
				Vk.Pipeline.Layout.createInfoNext = Nothing,
				Vk.Pipeline.Layout.createInfoFlags =
					Vk.Pipeline.Layout.CreateFlagsZero,
				Vk.Pipeline.Layout.createInfoSetLayouts =
					Right $ Vk.Pipeline.Layout.Layout dscSetLayout :...: HVNil,
				Vk.Pipeline.Layout.createInfoPushConstantRanges
					= [] }
		Vk.Pipeline.Layout.create @() device pipelineLayoutInfo nil nil \pipelineLayout -> do
			let	shaderModuleInfo = Vk.Shader.Module.CreateInfo {
					Vk.Shader.Module.createInfoNext = Nothing,
					Vk.Shader.Module.createInfoFlags =
						Vk.Shader.Module.CreateFlagsZero,
					Vk.Shader.Module.createInfoCode =
						glslComputeShaderMain }
				shaderStageInfo = Vk.Pipeline.ShaderStage.CreateInfo {
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
			    let	dscSetInfo = Vk.Descriptor.Set.AllocateInfo' {
						Vk.Descriptor.Set.allocateInfoNext' = Nothing,
						Vk.Descriptor.Set.allocateInfoDescriptorPool' =
							dscPool,
						Vk.Descriptor.Set.allocateInfoSetLayouts' =
							Vk.Descriptor.Set.Layout dscSetLayout :...: HVNil }
			    dscSet :...: HVNil <- Vk.Descriptor.Set.allocateSs' @() device dscSetInfo
			    storageBufferNew3 device phdvc datA datB datC
					\((bufA, memA), (bufB, memB), (bufC, memC)) -> do
				let	descBufferInfos =
						(Vk.Descriptor.BufferInfoList bufA ::
							Vk.Descriptor.BufferInfo '(_, _, _, 'List W1)) :...:
						(Vk.Descriptor.BufferInfoList bufB ::
							Vk.Descriptor.BufferInfo '(_, _, _, 'List W2)) :...:
						(Vk.Descriptor.BufferInfoList bufC ::
							Vk.Descriptor.BufferInfo '(_, _, _, 'List W3)) :...:
						HVNil
					writeDescSet = Vk.Descriptor.Set.Write {
						Vk.Descriptor.Set.writeNext = Nothing,
						Vk.Descriptor.Set.writeDstSet = dscSet,
						Vk.Descriptor.Set.writeDescriptorType =
							Vk.Descriptor.TypeStorageBuffer,
						Vk.Descriptor.Set.writeSources =
							Vk.Descriptor.Set.BufferInfos descBufferInfos }
				Vk.Descriptor.Set.updateDs @() @() device
					(Vk.Descriptor.Set.Write_ writeDescSet :...: HVNil)
					[]
				queue <- Vk.Device.getQueue device queueFamily 0
				Vk.CommandPool.create device (mkCommandPoolInfo queueFamily) nil nil
					$ withCommandPoolNew device queue dscSet (memA, memB, memC) pipelines pipelineLayout

type Mems sm1 sm2 sm3 = (
	Vk.Device.Memory.Buffer.M sm1 '[ '[ 'List W1]],
	Vk.Device.Memory.Buffer.M sm2 '[ '[ 'List W2]],
	Vk.Device.Memory.Buffer.M sm3 '[ '[ 'List W3]] )

mkCommandPoolInfo :: Vk.QueueFamily.Index -> Vk.CommandPool.CreateInfo ()
mkCommandPoolInfo queueFamily = Vk.CommandPool.CreateInfo {
	Vk.CommandPool.createInfoNext = Nothing,
	Vk.CommandPool.createInfoFlags =
		Vk.CommandPool.CreateResetCommandBufferBit,
	Vk.CommandPool.createInfoQueueFamilyIndex = queueFamily }

withCommandPoolNew ::
	Vk.Device.D sd -> Vk.Queue.Q -> Vk.Descriptor.Set.S' sd sp slbts ->
	Mems sm1 sm2 sm3 ->
	[Vk.Pipeline.Compute.C sg] -> Vk.Pipeline.Layout.L s ->
	Vk.CommandPool.C sc -> IO ()
withCommandPoolNew device
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
					Vk.Pipeline.BindPointCompute $ head pipelines
				Vk.Cmd.bindDescriptorSets
					((\(Vk.CommandBuffer.C c) -> c) commandBuffer)
					Vk.Pipeline.BindPointCompute
					((\(Vk.Pipeline.Layout.L l) -> l) pipelineLayout)
					0
					[(\(Vk.Descriptor.Set.S' s) -> s) dscSet]
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
