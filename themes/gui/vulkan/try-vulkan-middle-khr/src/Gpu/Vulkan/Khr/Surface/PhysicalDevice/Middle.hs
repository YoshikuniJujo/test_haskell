{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.PhysicalDevice.Middle (

	-- * GET SUPPORT, CAPABILITIES, FORMATS AND PRESENT MODES

	getSupport, getCapabilities, getFormats, getPresentModes

	) where

import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke

import Gpu.Vulkan.Base.Middle.Internal
import Gpu.Vulkan.Exception.Middle
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Khr.Surface.Enum

import qualified Gpu.Vulkan.PhysicalDevice.Middle.Internal as PhysicalDevice
import qualified Gpu.Vulkan.QueueFamily.Middle as QueueFamily
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice.Core as C
import qualified Gpu.Vulkan.Khr.Surface.Middle.Internal as M

getSupport :: PhysicalDevice.P -> QueueFamily.Index -> M.S -> IO Bool
getSupport (PhysicalDevice.P phdvc) (QueueFamily.Index qfi) (M.S sfc) =
	bool32ToBool <$> alloca \pSupported -> do
		r <- C.getSupport phdvc qfi sfc pSupported
		throwUnlessSuccess $ Result r
		peek pSupported

getCapabilities :: PhysicalDevice.P -> M.S -> IO M.Capabilities
getCapabilities (PhysicalDevice.P pdvc) (M.S sfc) =
	M.capabilitiesFromCore <$> alloca \pCapabilities -> do
		r <- C.getCapabilities pdvc sfc pCapabilities
		throwUnlessSuccess $ Result r
		peek pCapabilities

getFormats :: PhysicalDevice.P -> M.S -> IO [M.Format]
getFormats (PhysicalDevice.P pdvc) (M.S sfc) =
	(M.formatFromCore <$>) <$> alloca \pFormatCount ->
	C.getFormats pdvc sfc pFormatCount NullPtr >>= \r ->
	throwUnlessSuccess (Result r) >>
	peek pFormatCount >>= \(fromIntegral -> formatCount) ->
	allocaArray formatCount \pFormats -> do
		r' <- C.getFormats pdvc sfc pFormatCount pFormats
		throwUnlessSuccess $ Result r'
		peekArray formatCount pFormats

getPresentModes :: PhysicalDevice.P -> M.S -> IO [PresentMode]
getPresentModes (PhysicalDevice.P pdvc) (M.S sfc) =
	 (PresentMode <$>) <$> alloca \pPresentModeCount ->
	C.getPresentModes pdvc sfc pPresentModeCount NullPtr >>= \r ->
	throwUnlessSuccess (Result r) >>
	peek pPresentModeCount >>= \(fromIntegral -> presentModeCount) ->
	allocaArray presentModeCount \pPresentModes -> do
		r' <- C.getPresentModes
			pdvc sfc pPresentModeCount pPresentModes
		throwUnlessSuccess $ Result r'
		peekArray presentModeCount pPresentModes
