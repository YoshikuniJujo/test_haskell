{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Vulkan.Base

import qualified Vulkan.Instance as Vk.Instance
import qualified Vulkan.PhysicalDevice as Vk.PhysicalDevice

main :: IO ()
main = Vk.Instance.create Vk.Instance.createInfoNil nil nil \inst -> do
	physicalDevice <- head <$> Vk.PhysicalDevice.enumerate inst
	pure ()
