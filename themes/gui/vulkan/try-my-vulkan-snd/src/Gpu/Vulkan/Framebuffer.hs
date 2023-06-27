{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer (
	F, createNew, recreateNew, CreateInfoNew(..) ) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import qualified Data.HeteroParList as HeteroParList
import Data.Word

import Gpu.Vulkan.Framebuffer.Enum
import Gpu.Vulkan.Framebuffer.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.RenderPass.Type as RenderPass
import qualified Gpu.Vulkan.ImageView as ImageView
import qualified Gpu.Vulkan.Framebuffer.Middle as M

data CreateInfoNew mn sr fmtnmsis = CreateInfoNew {
	createInfoNextNew :: TMaybe.M mn,
	createInfoFlagsNew :: CreateFlags,
	createInfoRenderPassNew :: RenderPass.R sr,
	createInfoAttachmentsNew :: HeteroParList.PL (U3 ImageView.I) fmtnmsis,
	createInfoWidthNew :: Word32,
	createInfoHeightNew :: Word32,
	createInfoLayersNew :: Word32 }

type family MapThird (t :: [(j, k, l)]) where
	MapThird '[] = '[]
	MapThird ('(a, b, c) ': abcs) = c ': MapThird abcs

createInfoToMiddleNew :: CreateInfoNew n sr fmtmnsis -> M.CreateInfo n
createInfoToMiddleNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoRenderPassNew = RenderPass.R rp,
	createInfoAttachmentsNew = HeteroParList.toList (\(U3 (ImageView.I iv)) -> iv) -> ivs,
	createInfoWidthNew = w,
	createInfoHeightNew = h,
	createInfoLayersNew = lyrs } = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoRenderPass = rp,
		M.createInfoAttachments = ivs,
		M.createInfoWidth = w,
		M.createInfoHeight = h,
		M.createInfoLayers = lyrs }

createNew :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mscc) =>
	Device.D sd -> CreateInfoNew mn sr fmtnmsis ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s . F s -> IO a) -> IO a
createNew (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.create dvc (createInfoToMiddleNew ci) macc)
	(\fb -> M.destroy dvc fb macc) (f . F)

recreateNew :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mscc) =>
	Device.D sd -> CreateInfoNew mn sr fmtnmsis ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	F sf -> IO ()
recreateNew (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) (F fb) =
	M.recreate dvc (createInfoToMiddleNew ci) macc macc fb
