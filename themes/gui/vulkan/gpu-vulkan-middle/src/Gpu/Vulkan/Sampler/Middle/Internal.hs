{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sampler.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable	
import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe

import Gpu.Vulkan.Base.Middle.Internal
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Sampler.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Sampler.Core as C

newtype S = S C.S deriving Show

pattern Null :: S
pattern Null <- S NullHandle where
	Null = S NullHandle

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
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

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO r) -> IO ()
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
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
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

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO S
create (Device.D dvc) ci mac = S <$> alloca \ps -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.mToCore mac \pac ->
			throwUnlessSuccess . Result
				=<< C.create dvc pci pac ps
	peek ps

destroy :: Device.D -> S -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) (S s) mac =
	AllocationCallbacks.mToCore mac $ C.destroy dvc s
