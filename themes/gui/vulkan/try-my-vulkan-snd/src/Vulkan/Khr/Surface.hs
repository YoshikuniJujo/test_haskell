{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface where

import Control.Monad.Cont
import Data.Word

import Vulkan
import Vulkan.Base
import Vulkan.AllocationCallbacks (AllocationCallbacks, maybeToCore)
import Vulkan.Khr
import Vulkan.Khr.Enum
import Vulkan.Khr.Surface.Enum

import qualified Vulkan.Core as C
import qualified Vulkan.Khr.Surface.Core as C
import qualified Vulkan.Image.Enum as Image

destroy :: Pointable n =>
	Instance -> Surface -> Maybe (AllocationCallbacks n) -> IO ()
destroy (Instance ist) (Surface sfc) mac = ($ pure) $ runContT do
	pac <- maybeToCore mac
	lift $ C.destroy ist sfc pac

data Capabilities = Capabilities {
	capabilitiesMinImageCount :: Word32,
	capabilitiesMaxImageCount :: Word32,
	capabilitiesCurrentExtent :: C.Extent2d,
	capabilitiesMinImageExtent :: C.Extent2d,
	capabilitiesMaxImageExtent :: C.Extent2d,
	capabilitiesMaxImageArrayLayers :: Word32,
	capabilitiesSupportedTransforms :: TransformFlags,
	capabilitiesCurrentTransform :: TransformFlagBits,
	capabilitiesSupportedCompositeAlpha :: CompositeAlphaFlags,
	capabilitiesSupportedUsageFlags :: Image.UsageFlags }
	deriving Show
