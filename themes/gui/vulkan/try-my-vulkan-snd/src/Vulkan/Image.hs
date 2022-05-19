{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Image where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Vulkan.Core
import Vulkan.Enum
import Vulkan.Image.Enum

import qualified Vulkan.Image.Core as C
import qualified Vulkan.Sample.Enum as Sample

data SubresourceRange = SubresourceRange {
	subresourceRangeAspectMask :: AspectFlags,
	subresourceRangeBaseMipLevel :: Word32,
	subresourceRangeLevelCount :: Word32,
	subresourceRangeBaseArrayLayer :: Word32,
	subresourceRangeLayerCount :: Word32 }
	deriving Show

subresourceRangeToCore :: SubresourceRange -> C.SubresourceRange
subresourceRangeToCore SubresourceRange {
	subresourceRangeAspectMask = AspectFlagBits am,
	subresourceRangeBaseMipLevel = bmlv,
	subresourceRangeLevelCount = lvc,
	subresourceRangeBaseArrayLayer = baly,
	subresourceRangeLayerCount = lyc } = C.SubresourceRange {
		C.subresourceRangeAspectMask = am,
		C.subresourceRangeBaseMipLevel = bmlv,
		C.subresourceRangeLevelCount = lvc,
		C.subresourceRangeBaseArrayLayer = baly,
		C.subresourceRangeLayerCount = lyc }

newtype I = I C.I deriving Show

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoImageType :: Type,
	createInfoFormat :: Format,
	createInfoExtent :: Extent3d,
	createInfoMipLevels :: Word32,
	createInfoArrayLayers :: Word32,
	createInfoSamples :: Sample.CountFlagBits,
	createInfoTiling :: Tiling,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [Word32],
	createInfoInitialLayout :: Layout }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoImageType = Type tp,
	createInfoFormat = Format fmt,
	createInfoExtent = ext,
	createInfoMipLevels = mls,
	createInfoArrayLayers = als,
	createInfoSamples = Sample.CountFlagBits smpls,
	createInfoTiling = Tiling tlng,
	createInfoUsage = UsageFlagBits usg,
	createInfoSharingMode = SharingMode sm,
	createInfoQueueFamilyIndices = length &&& id -> (qfic, qfis),
	createInfoInitialLayout = Layout lyt
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pqfis <- ContT $ allocaArray qfic
	lift $ pokeArray pqfis qfis
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoImageType = tp,
		C.createInfoFormat = fmt,
		C.createInfoExtent = ext,
		C.createInfoMipLevels = mls,
		C.createInfoArrayLayers = als,
		C.createInfoSamples = smpls,
		C.createInfoTiling = tlng,
		C.createInfoUsage = usg,
		C.createInfoSharingMode = sm,
		C.createInfoQueueFamilyIndexCount = fromIntegral qfic,
		C.createInfoPQueueFamilyIndices = pqfis,
		C.createInfoInitialLayout = lyt }
