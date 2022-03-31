{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface where

import Foreign.Pointable
import Control.Monad.Cont
import Data.Word

import Vulkan
import Vulkan.Khr
import Vulkan.Khr.Enum
import Vulkan.Khr.Surface.Enum

import qualified Vulkan.Core as C
import qualified Vulkan.Enum as E
import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Khr.Surface.Core as Sfc.C
import qualified Vulkan.Image.Enum as Image

destroy :: Pointable n =>
	Instance -> Surface -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Instance ist) (Surface sfc) mac = ($ pure) $ runContT do
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
