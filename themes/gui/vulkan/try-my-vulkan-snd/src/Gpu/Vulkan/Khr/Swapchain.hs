{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain (
	createNew, recreateNew, CreateInfoNew(..), getImagesNew,
	create, recreate, S, CreateInfo(..), getImages) where

import Foreign.Pointable
import Control.Exception
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Khr.Enum
import Gpu.Vulkan.Khr.Swapchain.Type
import Gpu.Vulkan.Khr.Swapchain.Enum
import Gpu.Vulkan.Khr.Surface.Enum

import qualified Gpu.Vulkan.Core as C
import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as M

import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.Khr.Surface.Type as Surface

createNew :: (Pointable n, Pointable c, Pointable d, T.FormatToValue fmt) =>
	Device.D sd -> CreateInfoNew n ssfc fmt ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall ssc . SNew ssc fmt -> IO a) -> IO a
createNew (Device.D dvc) ci macc macd f =
	bracket (createNewM dvc ci macc) (\sc -> M.destroy dvc sc macd) (f . SNew)

create :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> CreateInfo n ssfc ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall ssc . S ssc -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (createM dvc ci macc) (\sc -> M.destroy dvc sc macd) (f . S)

recreateNew :: (Pointable n, Pointable c, Pointable d, T.FormatToValue fmt) =>
	Device.D sd -> CreateInfoNew n ssfc fmt ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	SNew ssc fmt -> IO ()
recreateNew (Device.D dvc) ci macc macd (SNew sc) = recreateNewM dvc ci macc macd sc

recreate :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> CreateInfo n ssfc ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	S ssc -> IO ()
recreate (Device.D dvc) ci macc macd (S sc) = recreateM dvc ci macc macd sc

getImagesNew :: Device.D sd -> SNew ss fmt -> IO [Image.BindedNew ss ss nm fmt]
getImagesNew (Device.D dvc) (SNew sc) = (Image.BindedNew <$>) <$> M.getImages dvc sc

getImages :: Device.D sd -> S ss -> IO [Image.Binded ss ss]
getImages (Device.D dvc) (S sc) = (Image.Binded <$>) <$> M.getImages dvc sc

data CreateInfoNew n ss (fmt :: T.Format) = CreateInfoNew {
	createInfoNextNew :: Maybe n,
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
	deriving Show

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

createNewM :: (Pointable n, Pointable n', T.FormatToValue fmt) => Device.M.D ->
	CreateInfoNew n ss fmt -> Maybe (AllocationCallbacks.A n') -> IO M.S
createNewM dvc ci mac = createM dvc (createInfoFromNew ci) mac

recreateNewM :: (Pointable n, Pointable c, Pointable d, T.FormatToValue fmt) =>
	Device.M.D -> CreateInfoNew n ss fmt ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	M.S -> IO ()
recreateNewM dvc = recreateM dvc . createInfoFromNew

data CreateInfo n ss = CreateInfo {
	createInfoNext :: Maybe n,
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
	deriving Show

createM :: (Pointable n, Pointable n') =>
	Device.M.D -> CreateInfo n ss -> Maybe (AllocationCallbacks.A n') -> IO M.S
createM dvc ci mac = M.create dvc (createInfoToOld ci) mac

recreateM :: (Pointable n, Pointable c, Pointable d) =>
	Device.M.D -> CreateInfo n ss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	M.S -> IO ()
recreateM dvc ci macc macd s = M.recreate dvc (createInfoToOld ci) macc macd s

createInfoToOld :: CreateInfo n ss -> M.CreateInfo n
createInfoToOld CreateInfo {
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
