{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.SimplePolygon.PhysicalDevice (

	-- * ENUMERATE

	enumerate, P(..), QueueFamilyIndices(..),

	-- * PROPERTIES

	getProperties, Vk.PhDvc.Properties(..), Vk.PhDvc.Limits(..),

	-- * FEATURES

	getFeatures, Vk.PhDvc.Features(..)

	) where

import Control.Monad
import Data.Foldable
import Data.Bool
import Data.Maybe
import Data.List qualified as L

import Gpu.Vulkan.PhysicalDevice qualified as Vk.PhDvc
import Gpu.Vulkan.Queue qualified as Vk.Queue
import Gpu.Vulkan.QueueFamily qualified as Vk.QueueFamily
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Surface
import Gpu.Vulkan.Khr.Surface.PhysicalDevice
	qualified as Vk.Khr.Surface.PhysicalDevice
import Gpu.Vulkan.Khr.Swapchain qualified as Vk.Khr.Swapchain

import Tools

import Graphics.SimplePolygon.Instance qualified as Ist

enumerate :: Ist.I si -> Vk.Khr.Surface.S ss -> IO [P]
enumerate ist sfc = do
	ps <- Vk.PhDvc.enumerate ist
	catMaybes <$> (`suitable` sfc) `mapM` ps

suitable :: Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO (Maybe P)
suitable phd sfc = (P phd <$>) <$> isDeviceSuitable phd sfc

data P = P Vk.PhDvc.P QueueFamilyIndices deriving Show

getProperties :: P -> IO Vk.PhDvc.Properties
getProperties (P phd _) = Vk.PhDvc.getProperties phd

getFeatures :: P -> IO Vk.PhDvc.Features
getFeatures (P phd _) = Vk.PhDvc.getFeatures phd

isDeviceSuitable ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO (Maybe QueueFamilyIndices)
isDeviceSuitable phdvc sfc = do
	_deviceProperties <- Vk.PhDvc.getProperties phdvc
	deviceFeatures <- Vk.PhDvc.getFeatures phdvc
	is <- findQueueFamilies phdvc sfc
	extensionSupported <- checkDeviceExtensionSupport phdvc
	if extensionSupported && Vk.PhDvc.featuresSamplerAnisotropy deviceFeatures
	then (<$> querySwapChainSupport phdvc sfc) \spp ->
		bool (completeQueueFamilies is) Nothing
			$ null (formats spp) || null (presentModes spp)
	else pure Nothing

data QueueFamilyIndices = QueueFamilyIndices {
	graphicsFamily :: Vk.QueueFamily.Index,
	presentFamily :: Vk.QueueFamily.Index }
	deriving Show

data QueueFamilyIndicesMaybe = QueueFamilyIndicesMaybe {
	graphicsFamilyMaybe :: Maybe Vk.QueueFamily.Index,
	presentFamilyMaybe :: Maybe Vk.QueueFamily.Index }
	deriving Show

completeQueueFamilies :: QueueFamilyIndicesMaybe -> Maybe QueueFamilyIndices
completeQueueFamilies = \case
	QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = Just gf, presentFamilyMaybe = Just pf } ->
		Just QueueFamilyIndices {
			graphicsFamily = gf, presentFamily = pf }
	_ -> Nothing

findQueueFamilies ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO QueueFamilyIndicesMaybe
findQueueFamilies device sfc = do
	queueFamilies <- Vk.PhDvc.getQueueFamilyProperties device
	pfis <- filterM
		(\i -> Vk.Khr.Surface.PhysicalDevice.getSupport device i sfc)
		(fst <$> queueFamilies)
	pure QueueFamilyIndicesMaybe {
		graphicsFamilyMaybe = fst <$> find
			(checkBits Vk.Queue.GraphicsBit
				. Vk.QueueFamily.propertiesQueueFlags . snd)
			queueFamilies,
		presentFamilyMaybe = listToMaybe pfis }

checkDeviceExtensionSupport :: Vk.PhDvc.P -> IO Bool
checkDeviceExtensionSupport dvc =
	null . (deviceExtensions L.\\) . (Vk.PhDvc.extensionPropertiesExtensionName <$>)
		<$> Vk.PhDvc.enumerateExtensionProperties dvc Nothing

deviceExtensions :: [Vk.PhDvc.ExtensionName]
deviceExtensions = [Vk.Khr.Swapchain.extensionName]

querySwapChainSupport ::
	Vk.PhDvc.P -> Vk.Khr.Surface.S ss -> IO SwapChainSupportDetails
querySwapChainSupport dvc sfc = SwapChainSupportDetails
	<$> Vk.Khr.Surface.PhysicalDevice.getCapabilities dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getFormatsOld dvc sfc
	<*> Vk.Khr.Surface.PhysicalDevice.getPresentModes dvc sfc

data SwapChainSupportDetails = SwapChainSupportDetails {
	_capabilities :: Vk.Khr.Surface.Capabilities,
	formats :: [Vk.Khr.Surface.FormatOld],
	presentModes :: [Vk.Khr.PresentMode] }
