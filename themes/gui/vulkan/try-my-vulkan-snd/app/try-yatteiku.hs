{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Default

import Vulkan.Base

import qualified Vulkan.Instance as Vk.Instance
import qualified Vulkan.PhysicalDevice as Vk.PhysicalDevice
import qualified Vulkan.Device as Vk.Device

import qualified Vulkan.Khr as Vk.Khr

main :: IO ()
main = do
	let	createInfo :: Vk.Instance.CreateInfo () ()
		createInfo = def {
			Vk.Instance.createInfoEnabledLayerNames =
				[Vk.Khr.validationLayerName] }
	Vk.Instance.create createInfo nil nil \inst -> do
		physicalDevice <- head <$> Vk.PhysicalDevice.enumerate inst
		pure ()
