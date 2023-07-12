{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain (

	-- * EXTENSION NAME

	M.extensionName,

	-- * CREATE

	create, recreate, S, CreateInfo(..),

	-- * GET IMAGES

	getImages ) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Khr.Enum
import Gpu.Vulkan.Khr.Swapchain.Type
import Gpu.Vulkan.Khr.Swapchain.Enum
import Gpu.Vulkan.Khr.Surface.Enum

import qualified Gpu.Vulkan.Middle as C
import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as M

import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.QueueFamily.Middle as QueueFamily
import qualified Gpu.Vulkan.Khr.Surface.Type as Surface

create :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfo mn ssfc fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall ssc . S fmt ssc -> IO a) -> IO a
create (Device.D dvc) ci (AllocationCallbacks.toMiddle -> macd) f = bracket
	(M.create dvc (createInfoToMiddle ci) macd)
	(\sc -> M.destroy dvc sc macd) (f . S)

recreate :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfo mn ssfc fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc -> S fmt ssc -> IO ()
recreate (Device.D dvc) ci (AllocationCallbacks.toMiddle -> macc) (S sc) =
	M.recreate dvc (createInfoToMiddle ci) macc sc

data CreateInfo mn ss (fmt :: T.Format) = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoSurface :: Surface.S ss,
	createInfoMinImageCount :: Word32,
	createInfoImageColorSpace :: ColorSpace,
	createInfoImageExtent :: C.Extent2d,
	createInfoImageArrayLayers :: Word32,
	createInfoImageUsage :: Image.UsageFlags,
	createInfoImageSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [QueueFamily.Index],
	createInfoPreTransform :: TransformFlagBits,
	createInfoCompositeAlpha :: CompositeAlphaFlagBits,
	createInfoPresentMode :: PresentMode,
	createInfoClipped :: Bool,
	createInfoOldSwapchain :: Maybe M.S }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn ss fmt)

createInfoToMiddle :: forall n ss fmt . T.FormatToValue fmt =>
	CreateInfo n ss fmt -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSurface = Surface.S sfc,
	createInfoMinImageCount = mic,
	createInfoImageColorSpace = cs,
	createInfoImageExtent = ext,
	createInfoImageArrayLayers = ials,
	createInfoImageUsage = usg,
	createInfoImageSharingMode = sm,
	createInfoQueueFamilyIndices = qfis,
	createInfoPreTransform = ptfm,
	createInfoCompositeAlpha = calph,
	createInfoPresentMode = pm,
	createInfoClipped = clpd,
	createInfoOldSwapchain = osc } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoSurface = sfc,
	M.createInfoMinImageCount = mic,
	M.createInfoImageFormat = T.formatToValue @fmt,
	M.createInfoImageColorSpace = cs,
	M.createInfoImageExtent = ext,
	M.createInfoImageArrayLayers = ials,
	M.createInfoImageUsage = usg,
	M.createInfoImageSharingMode = sm,
	M.createInfoQueueFamilyIndices = qfis,
	M.createInfoPreTransform = ptfm,
	M.createInfoCompositeAlpha = calph,
	M.createInfoPresentMode = pm,
	M.createInfoClipped = clpd,
	M.createInfoOldSwapchain = osc }

getImages :: Device.D sd -> S fmt ss -> IO [Image.Binded ss ss nm fmt]
getImages (Device.D dvc) (S sc) = (Image.Binded <$>) <$> M.getImages dvc sc
