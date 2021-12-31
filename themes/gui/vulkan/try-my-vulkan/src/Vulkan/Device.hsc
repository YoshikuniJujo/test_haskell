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
import Control.Monad.Cont
import Data.Foldable
import Data.List
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
	deviceQueueCreateInfoQueuePriorities = prs } f = ($ pure) $ runContT do
	(castPtr -> pnxt) <- ContT $ withPointerMaybe mnxt
	(fromIntegral -> cnt, pprs) <- ContT $ withArrayLen prs . curry
	lift . f $ I.DeviceQueueCreateInfo () pnxt flgs fi cnt pprs

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
withIDeviceQueueCreateInfoArray dqcis f = ($ pure) $ runContT do
	pidqcis <- ContT . allocaArray $ length dqcis
	idqis <- ContT $ withIDeviceQueueCreateInfos dqcis
	lift do	for_ (zip [0 ..] idqis) \(i, idqi) ->
			poke (pidqcis `advancePtr` i) idqi
		f (genericLength idqis) pidqcis

withIDeviceQueueCreateInfos :: Pointable n => [DeviceQueueCreateInfo n] ->
	([I.DeviceQueueCreateInfo] -> IO a) -> IO a
withIDeviceQueueCreateInfos [] f = f []
withIDeviceQueueCreateInfos (dqci : dqcis) f =
	deviceQueueCreateInfoToC dqci \idqci ->
		withIDeviceQueueCreateInfos dqcis \idqcis -> f $ idqci : idqcis

withCStringArray :: Integral c => [String] -> (c -> Ptr CString -> IO a) -> IO a
withCStringArray strs f = ($ pure) $ runContT do
	pcstr <- ContT . allocaArray $ length strs
	cstrs <- ContT $ withCStrings strs
	lift do	for_ (zip [0 ..] cstrs) \(i, cstr) ->
			poke (pcstr `advancePtr` i) cstr
		f (genericLength cstrs) pcstr

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings = runContT . ((ContT . withCString) `mapM`)

deviceCreateInfoToC :: (Pointable n, Pointable n') =>
	DeviceCreateInfo n n' -> (I.DeviceCreateInfo -> IO b) -> IO b
deviceCreateInfoToC DeviceCreateInfo {
	deviceCreateInfoNext = mnxt,
	deviceCreateInfoFlags = flgs,
	deviceCreateInfoQueueCreateInfos = qcis,
	deviceCreateInfoEnabledLayerNames = elnms,
	deviceCreateInfoEnabledExtensionNames = eenms,
	deviceCreateInfoEnabledFeatures = efs
	} f = ($ pure) $ runContT do
	(castPtr -> pnxt) <- ContT $ withPointerMaybe mnxt
	(qcic, pqcis) <- ContT $ withIDeviceQueueCreateInfoArray qcis . curry
	(elnmc, pelnms) <- ContT $ withCStringArray elnms . curry
	(eenmc, peenms) <- ContT $ withCStringArray eenms . curry
	pefs <- ContT alloca
	lift do	poke pefs efs
		f $ I.DeviceCreateInfo
			() pnxt flgs qcic pqcis elnmc pelnms eenmc peenms pefs

newtype Device = Device (Ptr Device) deriving (Show, Storable)

createDevice :: (Pointable n, Pointable n', Pointable n'') => PhysicalDevice ->
	DeviceCreateInfo n n' -> Maybe (AllocationCallbacks n'') -> IO Device
createDevice phd dci mac = ($ pure) $ runContT do
	pd <- ContT alloca
	I.DeviceCreateInfo_ fidci <- ContT $ deviceCreateInfoToC dci
	pidci <- ContT $ withForeignPtr fidci
	piac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift do	r <- c_vkCreateDevice phd pidci piac pd
		throwUnlessSuccess r
		peek pd

foreign import ccall "vkCreateDevice" c_vkCreateDevice ::
	PhysicalDevice -> Ptr I.DeviceCreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr Device -> IO Result

destroyDevice :: Pointable n => Device -> Maybe (AllocationCallbacks n) -> IO ()
destroyDevice dv mac = ($ pure) $ runContT do
	piac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift $ c_vkDestroyDevice dv piac

foreign import ccall "vkDestroyDevice" c_vkDestroyDevice ::
	Device -> Ptr I.AllocationCallbacks -> IO ()

newtype Queue = Queue (Ptr Queue) deriving (Show, Storable)

getDeviceQueue :: Device -> #{type uint32_t} -> #{type uint32_t} -> IO Queue
getDeviceQueue dv fi qi = ($ pure) $ runContT do
	pq <- ContT alloca
	lift do	c_vkGetDeviceQueue dv fi qi pq
		peek pq

foreign import ccall "vkGetDeviceQueue" c_vkGetDeviceQueue ::
	Device -> #{type uint32_t} -> #{type uint32_t} -> Ptr Queue -> IO ()
