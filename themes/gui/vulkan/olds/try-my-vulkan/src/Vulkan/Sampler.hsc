{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Sampler where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.Exception
import Vulkan.Device
import Vulkan.SamplerCreateFlagBits
import Vulkan.Filter
import Vulkan.SamplerMipmapMode
import Vulkan.SamplerAddressMode
import Vulkan.CompareOp
import Vulkan.BorderColor

import Vulkan.AllocationCallbacks

import qualified Vulkan.Sampler.Internal as I
import qualified Vulkan.AllocationCallbacks.Internal as I

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: SamplerCreateFlags,
	createInfoMagFilter :: Filter,
	createInfoMinFilter :: Filter,
	createInfoMipmapMode :: SamplerMipmapMode,
	createInfoAddressModeU, createInfoAddressModeV, createInfoAddressModeW
		:: SamplerAddressMode,
	createInfoMipLodBias :: Float,
	createInfoAnisotropyEnable :: Bool,
	createInfoMaxAnisotropy :: Float,
	createInfoCompareEnable :: Bool,
	createInfoCompareOp :: CompareOp,
	createInfoMinLod, createInfoMaxLod :: Float,
	createInfoBorderColor :: BorderColor,
	createInfoUnnormalizedCoordinates :: Bool }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoMagFilter = mgf,
	createInfoMinFilter = mnf,
	createInfoMipmapMode = mmm,
	createInfoAddressModeU = amu,
	createInfoAddressModeV = amv,
	createInfoAddressModeW = amw,
	createInfoMipLodBias = (floatToFloat -> mlb),
	createInfoAnisotropyEnable = (boolToBool32 -> aie),
	createInfoMaxAnisotropy = (floatToFloat -> mxai),
	createInfoCompareEnable = (boolToBool32 -> ce),
	createInfoCompareOp = co,
	createInfoMinLod = (floatToFloat -> mnl),
	createInfoMaxLod = (floatToFloat -> mxl),
	createInfoBorderColor = bc,
	createInfoUnnormalizedCoordinates = (boolToBool32 -> unc)
	} = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoMagFilter = mgf,
		I.createInfoMinFilter = mnf,
		I.createInfoMipmapMode = mmm,
		I.createInfoAddressModeU = amu,
		I.createInfoAddressModeV = amv,
		I.createInfoAddressModeW = amw,
		I.createInfoMipLodBias = mlb,
		I.createInfoAnisotropyEnable = aie,
		I.createInfoMaxAnisotropy = mxai,
		I.createInfoCompareEnable = ce,
		I.createInfoCompareOp = co,
		I.createInfoMinLod = mnl,
		I.createInfoMaxLod = mxl,
		I.createInfoBorderColor = bc,
		I.createInfoUnnormalizedCoordinates = unc }

data SamplerTag
newtype Sampler = Sampler (Ptr SamplerTag) deriving (Show, Storable)
type PtrSampler = Ptr Sampler

createSampler :: (Pointable n, Pointable n') =>
	Device -> CreateInfo n -> Maybe (AllocationCallbacks n') -> IO Sampler
createSampler dvc ci mac = ($ pure) $ runContT do
	I.CreateInfo_ fci <- ContT $ createInfoToC ci
	pci <- ContT $ withForeignPtr fci
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	ps <- ContT alloca
	lift do	r <- c_vkCreateSampler dvc pci pac ps
		throwUnlessSuccess r
		peek ps

foreign import ccall "vkCreateSampler" c_vkCreateSampler ::
	Device -> Ptr I.CreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr Sampler -> IO Result
