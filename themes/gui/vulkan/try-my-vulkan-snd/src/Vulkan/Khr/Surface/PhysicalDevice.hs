{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface.PhysicalDevice where

import Foreign.Marshal
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont

import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.Khr.Enum
import Vulkan.Khr.Surface

import qualified Vulkan.PhysicalDevice as PhysicalDevice
import qualified Vulkan.Khr.Surface.PhysicalDevice.Core as C

getCapabilities :: PhysicalDevice.P -> S -> IO Capabilities
getCapabilities (PhysicalDevice.P pdvc) (S sfc) =
	($ pure) . runContT $ capabilitiesFromCore <$> do
		pCapabilities <- ContT alloca
		lift do	r <- C.getCapabilities pdvc sfc pCapabilities
			throwUnlessSuccess $ Result r
			peek pCapabilities

getFormats :: PhysicalDevice.P -> S -> IO [Format]
getFormats (PhysicalDevice.P pdvc) (S sfc) =
	($ pure) . runContT $ (formatFromCore <$>) <$> do
		pFormatCount <- ContT alloca
		(fromIntegral -> formatCount) <- lift do
			r <- C.getFormats pdvc sfc pFormatCount NullPtr
			throwUnlessSuccess $ Result r
			peek pFormatCount
		pFormats <- ContT $ allocaArray formatCount
		lift do	r <- C.getFormats pdvc sfc pFormatCount pFormats
			throwUnlessSuccess $ Result r
			peekArray formatCount pFormats

getPresentModes :: PhysicalDevice.P -> S -> IO [PresentMode]
getPresentModes (PhysicalDevice.P pdvc) (S sfc) =
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
