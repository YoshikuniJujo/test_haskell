{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.PhysicalDevice where

import Foreign.Marshal
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont
import Data.Word

import Gpu.Vulkan.Base
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Khr.Enum
import Gpu.Vulkan.Khr.Surface.Type

import qualified Gpu.Vulkan.PhysicalDevice as PhysicalDevice
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice.Core as C
import qualified Gpu.Vulkan.Khr.Surface.Middle as M

getSupport :: PhysicalDevice.P -> QueueFamily.Index -> S ss -> IO Bool
getSupport (PhysicalDevice.P phdvc) (QueueFamily.Index qfi) (S (M.S sfc)) =
	($ pure) . runContT $ bool32ToBool <$> do
		pSupported <- ContT alloca
		lift do	r <- C.getSupport phdvc qfi sfc pSupported
			throwUnlessSuccess $ Result r
			peek pSupported

getSupport' :: PhysicalDevice.P -> Word32 -> M.S -> IO Bool
getSupport' phdvc qfi sfc = getSupport phdvc (QueueFamily.Index qfi) (S sfc)

getCapabilities :: PhysicalDevice.P -> M.S -> IO M.Capabilities
getCapabilities (PhysicalDevice.P pdvc) (M.S sfc) =
	($ pure) . runContT $ M.capabilitiesFromCore <$> do
		pCapabilities <- ContT alloca
		lift do	r <- C.getCapabilities pdvc sfc pCapabilities
			throwUnlessSuccess $ Result r
			peek pCapabilities

getFormats :: PhysicalDevice.P -> M.S -> IO [M.Format]
getFormats (PhysicalDevice.P pdvc) (M.S sfc) =
	($ pure) . runContT $ (M.formatFromCore <$>) <$> do
		pFormatCount <- ContT alloca
		(fromIntegral -> formatCount) <- lift do
			r <- C.getFormats pdvc sfc pFormatCount NullPtr
			throwUnlessSuccess $ Result r
			peek pFormatCount
		pFormats <- ContT $ allocaArray formatCount
		lift do	r <- C.getFormats pdvc sfc pFormatCount pFormats
			throwUnlessSuccess $ Result r
			peekArray formatCount pFormats

getPresentModes :: PhysicalDevice.P -> M.S -> IO [PresentMode]
getPresentModes (PhysicalDevice.P pdvc) (M.S sfc) =
	($ pure) . runContT $ (PresentMode <$>) <$> do
		pPresentModeCount <- ContT alloca
		(fromIntegral -> presentModeCount) <- lift do
			r <- C.getPresentModes pdvc sfc pPresentModeCount NullPtr
			throwUnlessSuccess $ Result r
			peek pPresentModeCount
		pPresentModes <- ContT $ allocaArray presentModeCount
		lift do	r <- C.getPresentModes
				pdvc sfc pPresentModeCount pPresentModes
			throwUnlessSuccess $ Result r
			peekArray presentModeCount pPresentModes
