{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.Marshal

import qualified Vulkan as Vk

main :: IO ()
main = do
	let	appInfo = Vk.ApplicationInfo {
			Vk.applicationInfoNext = Nothing :: Maybe Bool,
			Vk.applicationInfoApplicationName = "Hello Allocation",
			Vk.applicationInfoApplicationVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoEngineName = "No Engine",
			Vk.applicationInfoEngineVersion =
				Vk.makeApiVersion 0 1 0 0,
			Vk.applicationInfoApiVersion = Vk.apiVersion1_0 }
		createInfo = Vk.InstanceCreateInfo {
			Vk.instanceCreateInfoNext = Nothing :: Maybe Bool,
			Vk.instanceCreateInfoApplicationInfo = appInfo,
			Vk.instanceCreateInfoFlags = Vk.InstanceCreateFlagsZero,
			Vk.instanceCreateInfoEnabledLayers = [],
			Vk.instanceCreateInfoExtensions = [] }
		allocationCallback = Vk.AllocationCallbacks {
			Vk.allocationCallbacksUserData = (),
			Vk.allocationCallbacksFnAllocation = fnAllocation,
			Vk.allocationCallbacksFnReallocation = fnReallocation,
			Vk.allocationCallbacksFnFree = fnFree,
			Vk.allocationCallbacksFnInternalAllocation = fnInternalAllocation,
			Vk.allocationCallbacksFnInternalFree = fnInternalFree
			}
	i <- Vk.createInstance createInfo (Just allocationCallback)
	print i

fnAllocation :: Vk.FnAllocationFunction ()
fnAllocation _pud sz _algn _scp = mallocBytes $ fromIntegral sz

fnReallocation :: Vk.FnReallocationFunction ()
fnReallocation _ _ _ _ _ = pure nullPtr

fnFree :: Vk.FnFreeFunction ()
fnFree _ _ = pure ()

fnInternalAllocation :: Vk.FnInternalAllocationNotification ()
fnInternalAllocation _ _ _ _ = pure ()

fnInternalFree :: Vk.FnInternalFreeNotification ()
fnInternalFree _ _ _ _ = pure ()
