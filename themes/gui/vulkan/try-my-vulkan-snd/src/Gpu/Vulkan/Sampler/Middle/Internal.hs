{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sampler.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable	
import Foreign.Storable.PeekPoke
import Control.Monad.Cont

import Gpu.Vulkan.Misc.Middle.Internal
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Sampler.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Sampler.Core as C

newtype S = S C.S deriving Show

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoMagFilter :: Filter,
	createInfoMinFilter :: Filter,
	createInfoMipmapMode :: MipmapMode,
	createInfoAddressModeU :: AddressMode,
	createInfoAddressModeV :: AddressMode,
	createInfoAddressModeW :: AddressMode,
	createInfoMipLodBias :: Float,
	createInfoAnisotropyEnable :: Bool,
	createInfoMaxAnisotropy :: Float,
	createInfoCompareEnable :: Bool,
	createInfoCompareOp :: CompareOp,
	createInfoMinLod :: Float,
	createInfoMaxLod :: Float,
	createInfoBorderColor :: BorderColor,
	createInfoUnnormalizedCoordinates :: Bool }
	deriving Show

createInfoToCore :: WithPoked n =>
	CreateInfo n -> (Ptr C.CreateInfo -> IO r) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoMagFilter = Filter mgf,
	createInfoMinFilter = Filter mnf,
	createInfoMipmapMode = MipmapMode mmm,
	createInfoAddressModeU = AddressMode amu,
	createInfoAddressModeV = AddressMode amv,
	createInfoAddressModeW = AddressMode amw,
	createInfoMipLodBias = mlb,
	createInfoAnisotropyEnable = boolToBool32 -> aie,
	createInfoMaxAnisotropy = mai,
	createInfoCompareEnable = boolToBool32 -> ce,
	createInfoCompareOp = CompareOp cop,
	createInfoMinLod = mnl,
	createInfoMaxLod = mxl,
	createInfoBorderColor = BorderColor bc,
	createInfoUnnormalizedCoordinates = boolToBool32 -> unc } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	let	ci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoMagFilter = mgf,
			C.createInfoMinFilter = mnf,
			C.createInfoMipmapMode = mmm,
			C.createInfoAddressModeU = amu,
			C.createInfoAddressModeV = amv,
			C.createInfoAddressModeW = amw,
			C.createInfoMipLodBias = mlb,
			C.createInfoAnisotropyEnable = aie,
			C.createInfoMaxAnisotropy = mai,
			C.createInfoCompareEnable = ce,
			C.createInfoCompareOp = cop,
			C.createInfoMinLod = mnl,
			C.createInfoMaxLod = mxl,
			C.createInfoBorderColor = bc,
			C.createInfoUnnormalizedCoordinates = unc } in
	withPoked ci f

create :: (WithPoked n, WithPoked c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO S
create (Device.D dvc) ci mac = S <$> alloca \ps -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.maybeToCore mac \pac ->
			throwUnlessSuccess . Result
				=<< C.create dvc pci pac ps
	peek ps

destroy :: WithPoked d =>
	Device.D -> S -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (S s) mac =
	AllocationCallbacks.maybeToCore mac $ C.destroy dvc s
