{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Data.Default
import Data.TypeLevel.ParMaybe (nil)
import Data.Text qualified as T

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Instance qualified as Vk.Ist
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd

main :: IO ()
main = Vk.Ist.create @'Nothing @'Nothing def nil \ist -> do
	pd0 : _ <- Vk.Phd.enumerate ist
	print . filter (("VK_KHR_dy" `T.isPrefixOf`) . fst)
		. (nameAndVersion <$>) =<< Vk.Phd.enumerateExtensionProperties pd0 Nothing
	print =<< Vk.Phd.getFeatures pd0
	print =<< Vk.Phd.getFeatures2 @'Nothing pd0

nameAndVersion :: Vk.Phd.ExtensionProperties -> (T.Text, Vk.ApiVersion)
nameAndVersion =
	Vk.Phd.unExtensionName . Vk.Phd.extensionPropertiesExtensionName &&&
	Vk.Phd.extensionPropertiesSpecVersion
