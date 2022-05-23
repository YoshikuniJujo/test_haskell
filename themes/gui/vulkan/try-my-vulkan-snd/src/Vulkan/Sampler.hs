{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Sampler where

import Foreign.Ptr
import Foreign.Pointable
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.Enum
import Vulkan.Sampler.Enum

import qualified Vulkan.Sampler.Core as C

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

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO C.CreateInfo
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
	pure C.CreateInfo {
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
