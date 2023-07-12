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

	createNew, recreateNew, S, CreateInfoNew(..),

	-- * GET IMAGES

	getImagesNew ) where

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

createNew :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfoNew mn ssfc fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall ssc . S fmt ssc -> IO a) -> IO a
createNew (Device.D dvc) ci (AllocationCallbacks.toMiddle -> macd) f = bracket
	(M.create dvc (createInfoToMiddle ci) macd)
	(\sc -> M.destroy dvc sc macd) (f . S)

recreateNew :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfoNew mn ssfc fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc -> S fmt ssc -> IO ()
recreateNew (Device.D dvc) ci (AllocationCallbacks.toMiddle -> macc) (S sc) =
	M.recreate dvc (createInfoToMiddle ci) macc sc

data CreateInfoNew mn ss (fmt :: T.Format) = CreateInfoNew {
	createInfoNextNew :: TMaybe.M mn,
	createInfoFlagsNew :: CreateFlags,
	createInfoSurfaceNew :: Surface.S ss,
	createInfoMinImageCountNew :: Word32,
	createInfoImageColorSpaceNew :: ColorSpace,
	createInfoImageExtentNew :: C.Extent2d,
	createInfoImageArrayLayersNew :: Word32,
	createInfoImageUsageNew :: Image.UsageFlags,
	createInfoImageSharingModeNew :: SharingMode,
	createInfoQueueFamilyIndicesNew :: [QueueFamily.Index],
	createInfoPreTransformNew :: TransformFlagBits,
	createInfoCompositeAlphaNew :: CompositeAlphaFlagBits,
	createInfoPresentModeNew :: PresentMode,
	createInfoClippedNew :: Bool,
	createInfoOldSwapchainNew :: Maybe M.S }

deriving instance Show (TMaybe.M mn) => Show (CreateInfoNew mn ss fmt)

createInfoToMiddle :: forall n ss fmt . T.FormatToValue fmt =>
	CreateInfoNew n ss fmt -> M.CreateInfo n
createInfoToMiddle CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoSurfaceNew = Surface.S sfc,
	createInfoMinImageCountNew = mic,
	createInfoImageColorSpaceNew = cs,
	createInfoImageExtentNew = ext,
	createInfoImageArrayLayersNew = ials,
	createInfoImageUsageNew = usg,
	createInfoImageSharingModeNew = sm,
	createInfoQueueFamilyIndicesNew = qfis,
	createInfoPreTransformNew = ptfm,
	createInfoCompositeAlphaNew = calph,
	createInfoPresentModeNew = pm,
	createInfoClippedNew = clpd,
	createInfoOldSwapchainNew = osc } = M.CreateInfo {
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

getImagesNew :: Device.D sd -> S fmt ss -> IO [Image.Binded ss ss nm fmt]
getImagesNew (Device.D dvc) (S sc) = (Image.Binded <$>) <$> M.getImages dvc sc
