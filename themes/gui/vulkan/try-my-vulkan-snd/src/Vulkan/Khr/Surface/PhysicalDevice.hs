{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface.PhysicalDevice where

import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import Vulkan
import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.Khr
import Vulkan.Khr.Surface

import qualified Vulkan.Khr.Surface.PhysicalDevice.Core as C

getCapabilities :: PhysicalDevice -> Surface -> IO Capabilities
getCapabilities (PhysicalDevice pdvc) (Surface sfc) =
	($ pure) . runContT $ capabilitiesFromCore <$> do
		pCapabilities <- ContT alloca
		lift do	r <- C.getCapabilities pdvc sfc pCapabilities
			throwUnlessSuccess $ Result r
			peek pCapabilities
