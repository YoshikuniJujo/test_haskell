{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.PhysicalDevice.Middle (
	getSupport, getCapabilities, getFormats, getPresentModes ) where

import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Monad.Cont

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Khr.Enum
import Gpu.Vulkan.Misc.Middle.Internal

import qualified Gpu.Vulkan.PhysicalDevice.Middle.Internal as PhysicalDevice
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice.Core as C
import qualified Gpu.Vulkan.Khr.Surface.Middle.Internal as M

getSupport :: PhysicalDevice.P -> QueueFamily.Index -> M.S -> IO Bool
getSupport (PhysicalDevice.P phdvc) (QueueFamily.Index qfi) (M.S sfc) =
	($ pure) . runContT $ bool32ToBool <$> do
		pSupported <- ContT alloca
		lift do	r <- C.getSupport phdvc qfi sfc pSupported
			throwUnlessSuccess $ Result r
			peek pSupported

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
