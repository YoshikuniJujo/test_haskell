{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Data.Foldable
import Data.Word

import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks
import Vulkan.PhysicalDevice

import qualified Vulkan.Device.Internal as I
import qualified Vulkan.AllocationCallbacks.Internal as I

#include <vulkan/vulkan.h>

data DeviceQueueCreateInfo a = DeviceQueueCreateInfo {
	deviceQueueCreateInfoNext :: Maybe a,
	deviceQueueCreateInfoFlags :: I.DeviceQueueCreateFlags,
	deviceQueueCreateInfoQueueFamilyIndex :: #{type uint32_t},
	deviceQueueCreateInfoQueuePriorities :: [#{type float}] }
	deriving Show

deviceQueueCreateInfoToC :: Pointable a =>
	DeviceQueueCreateInfo a -> (I.DeviceQueueCreateInfo -> IO b) -> IO b
deviceQueueCreateInfoToC DeviceQueueCreateInfo {
	deviceQueueCreateInfoNext = mnxt,
	deviceQueueCreateInfoFlags = flgs,
	deviceQueueCreateInfoQueueFamilyIndex = fi,
	deviceQueueCreateInfoQueuePriorities = prs } f =
	withPointerMaybe mnxt \(castPtr -> pnxt) -> withArrayLen prs \cnt pprs -> f
		$ I.DeviceQueueCreateInfo () pnxt flgs fi (fromIntegral cnt) pprs

data DeviceCreateInfo n n' = DeviceCreateInfo {
	deviceCreateInfoNext :: Maybe n,
	deviceCreateInfoFlags :: I.DeviceCreateFlags,
	deviceCreateInfoQueueCreateInfos :: [DeviceQueueCreateInfo n'],
	deviceCreateInfoEnabledLayerNames :: [String],
	deviceCreateInfoEnabledExtensionNames :: [String],
	deviceCreateInfoEnabledFeatures :: PhysicalDeviceFeatures }
	deriving Show

withIDeviceQueueCreateInfoArray ::
	(Pointable n, Integral c) => [DeviceQueueCreateInfo n] ->
		(c -> Ptr I.DeviceQueueCreateInfo -> IO a) -> IO a
withIDeviceQueueCreateInfoArray dqcis f = allocaArray (length dqcis) \pidqcis ->
	withIDeviceQueueCreateInfos dqcis \idqis -> do
		for_ (zip [0 ..] idqis) \(i, idqi) ->
			poke (pidqcis `advancePtr` i) idqi
		f (fromIntegral $ length idqis) pidqcis

withIDeviceQueueCreateInfos :: Pointable n => [DeviceQueueCreateInfo n] ->
	([I.DeviceQueueCreateInfo] -> IO a) -> IO a
withIDeviceQueueCreateInfos [] f = f []
withIDeviceQueueCreateInfos (dqci : dqcis) f =
	deviceQueueCreateInfoToC dqci \idqci ->
		withIDeviceQueueCreateInfos dqcis \idqcis -> f $ idqci : idqcis

withCStringArray :: Integral c => [String] -> (c -> Ptr CString -> IO a) -> IO a
withCStringArray strs f =
	allocaArray (length strs) \pcstr -> withCStrings strs \cstrs -> do
		for_ (zip [0 ..] cstrs) \(i, cstr) ->
			poke (pcstr `advancePtr` i) cstr
		f (fromIntegral $ length strs) pcstr

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings [] f = f []
withCStrings (s : ss) f =
	withCString s \cs -> withCStrings ss \css -> f $ cs : css

deviceCreateInfoToC :: (Pointable n, Pointable n') =>
	DeviceCreateInfo n n' -> (I.DeviceCreateInfo -> IO b) -> IO b
deviceCreateInfoToC DeviceCreateInfo {
	deviceCreateInfoNext = mnxt,
	deviceCreateInfoFlags = flgs,
	deviceCreateInfoQueueCreateInfos = qcis,
	deviceCreateInfoEnabledLayerNames = elnms,
	deviceCreateInfoEnabledExtensionNames = eenms,
	deviceCreateInfoEnabledFeatures = efs
	} f = withPointerMaybe mnxt \(castPtr -> pnxt) ->
		withIDeviceQueueCreateInfoArray qcis \qcic pqcis ->
			withCStringArray elnms \elnmc pelnms ->
				withCStringArray eenms \eenmc peenms ->
					alloca \pefs -> do
						poke pefs efs
						f $ I.DeviceCreateInfo () pnxt
							flgs qcic pqcis
							elnmc pelnms
							eenmc peenms pefs

newtype Device = Device (Ptr Device) deriving (Show, Storable)

createDevice :: (Pointable n, Pointable n', Pointable n'') => PhysicalDevice ->
	DeviceCreateInfo n n' -> AllocationCallbacks n'' -> IO Device
createDevice phd dci ac = alloca \pd -> do
	deviceCreateInfoToC dci \(I.DeviceCreateInfo_ fidci) ->
		withForeignPtr fidci \pidci ->
			withAllocationCallbacksPtr ac \piac -> do
				r <- c_VkCreateDevice phd pidci piac pd
				throwUnlessSuccess r
				peek pd

foreign import ccall "VkCreateDevice" c_VkCreateDevice ::
	PhysicalDevice -> Ptr I.DeviceCreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr Device -> IO Result
