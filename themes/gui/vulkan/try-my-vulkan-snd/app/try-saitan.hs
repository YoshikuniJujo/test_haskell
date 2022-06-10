{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Default
import Data.Bits
import Data.Word

import qualified Data.Vector.Storable as V

import Shaderc.TH
import Vulkan.Base

import qualified Vulkan.Enum as Vk
import qualified Vulkan.Instance as Vk.Instance
import qualified Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Vulkan.Queue.Enum as Vk.Queue
import qualified Vulkan.QueueFamily as Vk.QueueFamily
import qualified Vulkan.QueueFamily.EnumManual as Vk.QueueFamily
import qualified Vulkan.Device.Queue as Vk.Device.Queue
import qualified Vulkan.Device.Queue.Enum as Vk.Device.Queue
import qualified Vulkan.Device as Vk.Device
import qualified Vulkan.CommandPool as Vk.CommandPool
import qualified Vulkan.CommandPool.Enum as Vk.CommandPool
import qualified Vulkan.Buffer.Enum as Vk.Buffer
import qualified Vulkan.Buffer.List as Vk.Buffer.List
import qualified Vulkan.Memory as Vk.Memory
import qualified Vulkan.Memory.Enum as Vk.Memory
import qualified Vulkan.Memory.Middle as Vk.Memory.M
import qualified Vulkan.Memory.List as Vk.Memory.List

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
	Vk.CommandPool.create @() device commandPoolInfo nil nil \commandPool -> do
		print commandPool
		storageBufferNew device phdvc dataA

dataSize :: Integral n => n
dataSize = 1000000

dataA, dataB, dataC :: V.Vector Word32
dataA = V.replicate dataSize 3
dataB = V.replicate dataSize 5
dataC = V.replicate dataSize 0

storageBufferNew ::
	Vk.Device.D sd -> Vk.PhysicalDevice.P -> V.Vector Word32 -> IO ()
storageBufferNew dvc phdvc xs = do
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
			print memory
			bnd <- Vk.Buffer.List.bindMemory dvc buffer memory
			print bnd
			Vk.Memory.List.writeMono
				dvc memory Vk.Memory.M.MapFlagsZero xs

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
		[] -> error "NO available memory types"
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
