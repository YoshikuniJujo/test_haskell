{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.Middle.Internal (

	-- * DESTROY

	destroy, S(..),

	-- * CAPABILITIES AND FORMAT

	Capabilities(..), capabilitiesFromCore,
	Format(..), formatFromCore ) where

import Data.Word
import Data.TypeLevel.ParMaybe qualified as TPMaybe

import Gpu.Vulkan.Khr.Surface.Enum

import qualified Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Core as C
import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Khr.Surface.Core as Sfc.C
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.Instance.Middle.Internal as Instance

newtype S = S Sfc.C.S deriving Show

destroy :: Instance.I -> S -> TPMaybe.M AllocationCallbacks.A mn -> IO ()
destroy (Instance.I ist) (S sfc) mac =
	AllocationCallbacks.mToCore mac $ Sfc.C.destroy ist sfc

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
	formatFormat :: Vk.Format,
	formatColorSpace :: ColorSpace }
	deriving Show

formatFromCore :: Sfc.C.Format -> Format
formatFromCore Sfc.C.Format {
	Sfc.C.formatFormat = fmt,
	Sfc.C.formatColorSpace = cs
	} = Format {
		formatFormat = Vk.Format fmt,
		formatColorSpace = ColorSpace cs }

{-
formatToCore :: Format -> Sfc.C.Format
formatToCore Format {
	formatFormat = Vk.Format fmt,
	formatColorSpace = ColorSpace cs } = Sfc.C.Format {
		Sfc.C.formatFormat = fmt,
		Sfc.C.formatColorSpace = cs }
-}
