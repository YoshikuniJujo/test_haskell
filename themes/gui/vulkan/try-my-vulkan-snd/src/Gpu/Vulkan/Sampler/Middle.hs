{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sampler.Middle where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr
import Foreign.Storable	
import Foreign.Pointable
import Control.Monad.Cont

import Gpu.Vulkan.Base
import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Sampler.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle as Device
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

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
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
	createInfoUnnormalizedCoordinates = boolToBool32 -> unc } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	let	C.CreateInfo_ fci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
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
			C.createInfoUnnormalizedCoordinates = unc }
	ContT $ withForeignPtr fci

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A n') -> IO S
create (Device.D dvc) ci mac = (S <$>) . ($ pure) $ runContT do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	ps <- ContT alloca
	lift do	r <- C.create dvc pci pac ps
		throwUnlessSuccess $ Result r
		peek ps

destroy :: Pointable n =>
	Device.D -> S -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) (S s) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy dvc s pac
