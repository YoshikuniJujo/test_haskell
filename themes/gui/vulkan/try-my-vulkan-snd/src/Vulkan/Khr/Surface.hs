{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface where

import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.Khr.Enum
import Vulkan.Khr.Surface.Enum

import qualified Vulkan.Core as C
import qualified Vulkan.Enum as E
import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Khr.Surface.Core as Sfc.C
import qualified Vulkan.Image.Enum as Image
import qualified Vulkan.Instance as Instance
import qualified Vulkan.PhysicalDevice as PhysicalDevice

newtype S = S Sfc.C.S deriving Show

destroy :: Pointable n =>
	Instance.I -> S -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Instance.I ist) (S sfc) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ Sfc.C.destroy ist sfc pac

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

capabilitiesFromCore :: Sfc.C.Capabilities -> Capabilities
capabilitiesFromCore Sfc.C.Capabilities {
	Sfc.C.capabilitiesMinImageCount = mnic,
	Sfc.C.capabilitiesMaxImageCount = mxic,
	Sfc.C.capabilitiesCurrentExtent = ce,
	Sfc.C.capabilitiesMinImageExtent = mnie,
	Sfc.C.capabilitiesMaxImageExtent = mxie,
	Sfc.C.capabilitiesMaxImageArrayLayers = mials,
	Sfc.C.capabilitiesSupportedTransforms = st,
	Sfc.C.capabilitiesCurrentTransform = ct,
	Sfc.C.capabilitiesSupportedCompositeAlpha = sca,
	Sfc.C.capabilitiesSupportedUsageFlags = suf
	} = Capabilities {
		capabilitiesMinImageCount = mnic,
		capabilitiesMaxImageCount = mxic,
		capabilitiesCurrentExtent = ce,
		capabilitiesMinImageExtent = mnie,
		capabilitiesMaxImageExtent = mxie,
		capabilitiesMaxImageArrayLayers = mials,
		capabilitiesSupportedTransforms = TransformFlagBits st,
		capabilitiesCurrentTransform = TransformFlagBits ct,
		capabilitiesSupportedCompositeAlpha =
			CompositeAlphaFlagBits sca,
		capabilitiesSupportedUsageFlags = Image.UsageFlagBits suf }

data Format = Format {
	formatFormat :: E.Format,
	formatColorSpace :: ColorSpace }
	deriving Show

formatFromCore :: Sfc.C.Format -> Format
formatFromCore Sfc.C.Format {
	Sfc.C.formatFormat = fmt,
	Sfc.C.formatColorSpace = cs
	} = Format {
		formatFormat = E.Format fmt,
		formatColorSpace = ColorSpace cs }

formatToCore :: Format -> Sfc.C.Format
formatToCore Format {
	formatFormat = E.Format fmt,
	formatColorSpace = ColorSpace cs } = Sfc.C.Format {
		Sfc.C.formatFormat = fmt,
		Sfc.C.formatColorSpace = cs }

getPhysicalDeviceSSupport :: PhysicalDevice.P -> Word32 -> S -> IO Bool
getPhysicalDeviceSSupport (PhysicalDevice.P phdvc) qfi (S sfc) =
	($ pure) . runContT $ bool32ToBool <$> do
		pSupported <- ContT alloca
		lift do	r <- Sfc.C.getPhysicalDeviceSSupport phdvc qfi sfc pSupported
			throwUnlessSuccess $ Result r
			peek pSupported
