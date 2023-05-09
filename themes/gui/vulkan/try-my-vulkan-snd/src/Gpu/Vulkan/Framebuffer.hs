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
import Data.TypeLevel.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
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
	createInfoAttachmentsNew :: HeteroParList.PL (U3 ImageView.INew) fmtnmsis,
--	createInfoAttachmentsNew :: HeteroParList.PL (ImageView.INew fmt nm) sis,
	createInfoWidthNew :: Word32,
	createInfoHeightNew :: Word32,
	createInfoLayersNew :: Word32 }

type family MapThird (t :: [(j, k, l)]) where
	MapThird '[] = '[]
	MapThird ('(a, b, c) ': abcs) = c ': MapThird abcs

createInfoFromNew :: CreateInfoNew n sr fmtnmsis -> CreateInfo n sr (MapThird fmtnmsis)
createInfoFromNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoRenderPassNew = rndpss,
	createInfoAttachmentsNew = atts,
	createInfoWidthNew = wdt, createInfoHeightNew = hgt,
	createInfoLayersNew = lyrs } = CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoRenderPass = rndpss,
	createInfoAttachments = isToOld atts,
	createInfoWidth = wdt, createInfoHeight = hgt,
	createInfoLayers = lyrs }

isToOld :: HeteroParList.PL (U3 ImageView.INew) fmtnmsis ->
	HeteroParList.PL ImageView.I (MapThird fmtnmsis)
isToOld HeteroParList.Nil = HeteroParList.Nil
isToOld ((U3 i) :** is) = ImageView.iToOld i :** isToOld is

data CreateInfo mn sr sis = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoRenderPass :: RenderPass.R sr,
	createInfoAttachments :: HeteroParList.PL ImageView.I sis,
	createInfoWidth :: Word32,
	createInfoHeight :: Word32,
	createInfoLayers :: Word32 }

deriving instance (Show (TMaybe.M mn), Show (HeteroParList.PL ImageView.I sis)) =>
	Show (CreateInfo mn sr sis)

createInfoToMiddleNew :: CreateInfoNew n sr fmtnmsis -> M.CreateInfo n
createInfoToMiddleNew = createInfoToMiddle . createInfoFromNew

createInfoToMiddle :: CreateInfo n sr si -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoRenderPass = RenderPass.R rp,
	createInfoAttachments = HeteroParList.toList (\(ImageView.I iv) -> iv) -> ivs,
	createInfoWidth = w,
	createInfoHeight = h,
	createInfoLayers = lyrs } = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoRenderPass = rp,
		M.createInfoAttachments = ivs,
		M.createInfoWidth = w,
		M.createInfoHeight = h,
		M.createInfoLayers = lyrs }

createNew :: (WithPoked (TMaybe.M mn), Pokable c, Pokable d) =>
	Device.D sd -> CreateInfoNew mn sr fmtnmsis ->
	Maybe (AllocationCallbacks.A sc c) -> Maybe (AllocationCallbacks.A sd d) ->
	(forall s . F s -> IO a) -> IO a
createNew (Device.D dvc) ci
	((AllocationCallbacks.toMiddle <$>) -> macc)
	((AllocationCallbacks.toMiddle <$>) -> macd) f = bracket
	(M.create dvc (createInfoToMiddleNew ci) macc)
	(\fb -> M.destroy dvc fb macd) (f . F)

recreateNew :: (WithPoked (TMaybe.M mn), Pokable c, Pokable d) =>
	Device.D sd -> CreateInfoNew mn sr fmtnmsis ->
	Maybe (AllocationCallbacks.A sc c) -> Maybe (AllocationCallbacks.A sd d) ->
	F sf -> IO ()
recreateNew (Device.D dvc) ci
	((AllocationCallbacks.toMiddle <$>) -> macc)
	((AllocationCallbacks.toMiddle <$>) -> macd) (F fb) =
	M.recreate dvc (createInfoToMiddleNew ci) macc macd fb
