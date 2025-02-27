{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Data.Bits
import Data.Default
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Text qualified as T

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Instance qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd

main :: IO ()
main = Vk.Ist.create @'Nothing @'Nothing (info []) nil \ist -> do
	pd0 : _ <- Vk.Phd.enumerate ist
	print . filter (("VK_KHR_dy" `T.isPrefixOf`) . fst)
		. (nameAndVersion <$>) =<< Vk.Phd.enumerateExtensionProperties pd0 Nothing
	putStr "VkPhysicalDeviceVulkan13Features.dynamicRendering: "
	print . Vk.Phd.vulkan13FeaturesDynamicRendering . (\(TMaybe.J x) -> x) . Vk.Phd.features2Next =<< Vk.Phd.getFeatures2
		@('Just (Vk.Phd.Vulkan13Features 'Nothing))  pd0
	where
	info exts = Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.N,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo = Just ainfo,
		Vk.Ist.createInfoEnabledLayerNames = [],
		Vk.Ist.createInfoEnabledExtensionNames = exts }
	ainfo = Vk.ApplicationInfo {
		Vk.applicationInfoNext = TMaybe.N,
		Vk.applicationInfoApplicationName = "Example Vulkan Application",
		Vk.applicationInfoApplicationVersion =
			Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoEngineName = "No Engine",
		Vk.applicationInfoEngineVersion = Vk.makeApiVersion 0 1 0 0,
		Vk.applicationInfoApiVersion = Vk.apiVersion_1_3 }

nameAndVersion :: Vk.Phd.ExtensionProperties -> (T.Text, Vk.ApiVersion)
nameAndVersion =
	Vk.Phd.unExtensionName . Vk.Phd.extensionPropertiesExtensionName &&&
	Vk.Phd.extensionPropertiesSpecVersion
