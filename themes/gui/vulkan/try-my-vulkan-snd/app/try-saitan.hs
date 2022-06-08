{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Default
import Data.Bits

import Shaderc.TH

import Vulkan.Base

import qualified Vulkan.Instance as Vk.Instance
import qualified Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Vulkan.Queue.Enum as Vk.Queue
import qualified Vulkan.QueueFamily as Vk.QueueFamily
import qualified Vulkan.QueueFamily.EnumManual as Vk.QueueFamily

findQueueFamily ::
	Vk.PhysicalDevice.P -> Vk.Queue.FlagBits -> IO Vk.QueueFamily.Index
findQueueFamily phdvc qb = do
	queueFamilyProperties <-
		Vk.PhysicalDevice.getQueueFamilyProperties phdvc
	pure . fst . head $ filter ((/= zeroBits)
		. (.&. qb) . Vk.QueueFamily.propertiesQueueFlags
		. snd) queueFamilyProperties

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
