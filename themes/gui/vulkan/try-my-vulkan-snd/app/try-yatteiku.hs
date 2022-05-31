{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Default
import Data.Bits
import Data.List
import Data.Word

import Vulkan.Base

import qualified Vulkan.Enum as Vk
import qualified Vulkan.Instance as Vk.Instance
import qualified Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Vulkan.Device as Vk.Device
import qualified Vulkan.QueueFamily as Vk.QueueFamily
import qualified Vulkan.Device.Queue as Vk.Device.Queue
import qualified Vulkan.Device.Queue.Enum as Vk.Device.Queue

import qualified Vulkan.Khr as Vk.Khr

main :: IO ()
main = do
	let	createInfo :: Vk.Instance.CreateInfo () ()
		createInfo = def {
			Vk.Instance.createInfoEnabledLayerNames =
				[Vk.Khr.validationLayerName] }
	Vk.Instance.create createInfo nil nil \inst -> do
		physicalDevice <- head <$> Vk.PhysicalDevice.enumerate inst
		queueProps <- Vk.PhysicalDevice.getQueueFamilyProperties
			physicalDevice
		mapM_ printQueueProps queueProps
		let	Just graphicsQueueFamilyIndex =
				pickGraphicsQueueFamilyIndex queueProps
			queueCreateInfo = Vk.Device.Queue.CreateInfo {
				Vk.Device.Queue.createInfoNext = Nothing,
				Vk.Device.Queue.createInfoFlags =
					Vk.Device.Queue.CreateFlagsZero,
				Vk.Device.Queue.createInfoQueueFamilyIndex =
					graphicsQueueFamilyIndex,
				Vk.Device.Queue.createInfoQueuePriorities =
					[1.0] }
			devCreateInfo :: Vk.Device.CreateInfo () ()
			devCreateInfo = Vk.Device.CreateInfo {
				Vk.Device.createInfoNext = Nothing,
				Vk.Device.createInfoFlags =
					Vk.Device.CreateFlagsZero,
				Vk.Device.createInfoQueueCreateInfos =
					[queueCreateInfo],
				Vk.Device.createInfoEnabledLayerNames =
					[Vk.Khr.validationLayerName],
				Vk.Device.createInfoEnabledExtensionNames = [],
				Vk.Device.createInfoEnabledFeatures = Nothing }
		device <- Vk.Device.create physicalDevice devCreateInfo nil
		pure ()

pickGraphicsQueueFamilyIndex :: [Vk.QueueFamily.Properties] -> Maybe Word32
pickGraphicsQueueFamilyIndex ps = fromIntegral <$> findIndex
	((/= zeroBits) . (Vk.QueueGraphicsBit .&.)
		. Vk.QueueFamily.propertiesQueueFlags) ps

printQueueProps :: Vk.QueueFamily.Properties -> IO ()
printQueueProps qps = do
	print qps
	print . breakBits $ Vk.QueueFamily.propertiesQueueFlags qps

breakBits :: FiniteBits b => b -> [b]
breakBits = bb (bit 0 `rotateR` 1)
	where
	bb i n	| i == zeroBits = []
		| i .&. n == zeroBits = bs
		| otherwise = i : bs
		where
		bs = bb (i `shiftR` 1) n
