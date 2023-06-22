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
	createNew, recreateNew, CreateInfoNew(..), getImagesNew ) where

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
import qualified Gpu.Vulkan.Device.Middle as Device.M
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
	(forall ssc . SNew ssc fmt -> IO a) -> IO a
createNew (Device.D dvc) ci macc@(AllocationCallbacks.toMiddle -> macd) f =
	bracket (createNewM dvc ci macc) (\sc -> M.destroy dvc sc macd) (f . SNew)

recreateNew :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfoNew mn ssfc fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc -> SNew ssc fmt -> IO ()
recreateNew (Device.D dvc) ci macc (SNew sc) = recreateNewM dvc ci macc sc

getImagesNew :: Device.D sd -> SNew ss fmt -> IO [Image.Binded ss ss nm fmt]
getImagesNew (Device.D dvc) (SNew sc) = (Image.Binded <$>) <$> M.getImages dvc sc

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

createInfoFromNew :: forall n ss fmt . T.FormatToValue fmt =>
	CreateInfoNew n ss fmt -> CreateInfo n ss
createInfoFromNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoSurfaceNew = sfc,
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
	createInfoOldSwapchainNew = osc } = CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSurface = sfc,
	createInfoMinImageCount = mic,
	createInfoImageFormat = T.formatToValue @fmt,
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
	createInfoOldSwapchain = osc }

createNewM :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle msn'n' ) =>
	Device.M.D -> CreateInfoNew mn ss fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) msn'n' -> IO M.S
createNewM dvc ci mac = createM dvc (createInfoFromNew ci) mac

recreateNewM :: (
	WithPoked (TMaybe.M mn), T.FormatToValue fmt,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.M.D -> CreateInfoNew mn ss fmt ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	M.S -> IO ()
recreateNewM dvc ci mac = recreateM dvc (createInfoFromNew ci) mac

data CreateInfo mn ss = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoSurface :: Surface.S ss,
	createInfoMinImageCount :: Word32,
	createInfoImageFormat :: Format,
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

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn ss)

createM :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle msn'n') =>
	Device.M.D -> CreateInfo mn ss ->
	TPMaybe.M (U2 AllocationCallbacks.A) msn'n' -> IO M.S
createM dvc ci (AllocationCallbacks.toMiddle -> mac) =
	M.create dvc (createInfoToMiddle ci) mac

recreateM :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mscc) =>
	Device.M.D -> CreateInfo mn ss ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc -> M.S -> IO ()
recreateM dvc ci (AllocationCallbacks.toMiddle -> mac) s =
	M.recreate dvc (createInfoToMiddle ci) mac s

createInfoToMiddle :: CreateInfo n ss -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSurface = Surface.S sfc,
	createInfoMinImageCount = mic,
	createInfoImageFormat = ifmt,
	createInfoImageColorSpace = ics,
	createInfoImageExtent = iext,
	createInfoImageArrayLayers = ials,
	createInfoImageUsage = iusg,
	createInfoImageSharingMode = ism,
	createInfoQueueFamilyIndices = qfis,
	createInfoPreTransform = ptfm,
	createInfoCompositeAlpha = calp,
	createInfoPresentMode = pm,
	createInfoClipped = clpd,
	createInfoOldSwapchain = osc
	} = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoSurface = sfc,
	M.createInfoMinImageCount = mic,
	M.createInfoImageFormat = ifmt,
	M.createInfoImageColorSpace = ics,
	M.createInfoImageExtent = iext,
	M.createInfoImageArrayLayers = ials,
	M.createInfoImageUsage = iusg,
	M.createInfoImageSharingMode = ism,
	M.createInfoQueueFamilyIndices = qfis,
	M.createInfoPreTransform = ptfm,
	M.createInfoCompositeAlpha = calp,
	M.createInfoPresentMode = pm,
	M.createInfoClipped = clpd,
	M.createInfoOldSwapchain = osc }
