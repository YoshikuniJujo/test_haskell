{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer (
	FNew, createNew, recreateNew, CreateInfoNew(..),
	F, create, recreate, CreateInfo(..)
	, createInfoToMiddle -- <-- temporary
	) where

import Foreign.Storable.PeekPoke
import Foreign.Pointable
import Control.Exception
import Data.HeteroList
import Data.Word

import Gpu.Vulkan.Framebuffer.Enum
import Gpu.Vulkan.Framebuffer.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.RenderPass.Type as RenderPass
import qualified Gpu.Vulkan.ImageView as ImageView
import qualified Gpu.Vulkan.Framebuffer.Middle as M

data CreateInfoNew n sr fmtnmsis = CreateInfoNew {
	createInfoNextNew :: Maybe n,
	createInfoFlagsNew :: CreateFlags,
	createInfoRenderPassNew :: RenderPass.R sr,
	createInfoAttachmentsNew :: HeteroVarList (V3 ImageView.INew) fmtnmsis,
--	createInfoAttachmentsNew :: HeteroVarList (ImageView.INew fmt nm) sis,
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

isToOld :: HeteroVarList (V3 ImageView.INew) fmtnmsis ->
	HeteroVarList ImageView.I (MapThird fmtnmsis)
isToOld HVNil = HVNil
isToOld ((V3 i) :...: is) = ImageView.iToOld i :...: isToOld is

data CreateInfo n sr sis = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoRenderPass :: RenderPass.R sr,
	createInfoAttachments :: HeteroVarList ImageView.I sis,
	createInfoWidth :: Word32,
	createInfoHeight :: Word32,
	createInfoLayers :: Word32 }

deriving instance (Show n, Show (HeteroVarList ImageView.I sis)) =>
	Show (CreateInfo n sr sis)

createInfoToMiddleNew :: CreateInfoNew n sr fmtnmsis -> M.CreateInfo n
createInfoToMiddleNew = createInfoToMiddle . createInfoFromNew

createInfoToMiddle :: CreateInfo n sr si -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoRenderPass = RenderPass.R rp,
	createInfoAttachments = heteroVarListToList (\(ImageView.I iv) -> iv) -> ivs,
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

createNew :: (Pointable n, Pokable c, Pokable d) =>
	Device.D sd -> CreateInfoNew n sr fmtnmsis ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . F s -> IO a) -> IO a
createNew (Device.D dvc) ci macc macd f = bracket
	(M.create dvc (createInfoToMiddleNew ci) macc)
	(\fb -> M.destroy dvc fb macd) (f . F)

create :: (Pointable n, Pokable c, Pokable d) =>
	Device.D sd -> CreateInfo n sr si ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . F s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\fb -> M.destroy dvc fb macd) (f . F)

recreateNew :: (Pointable n, Pokable c, Pokable d) =>
	Device.D sd -> CreateInfoNew n sr fmtnmsis ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	F sf -> IO ()
recreateNew (Device.D dvc) ci macc macd (F fb) =
	M.recreate dvc (createInfoToMiddleNew ci) macc macd fb

recreate :: (Pointable n, Pokable c, Pokable d) =>
	Device.D sd -> CreateInfo n sr si ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	F sf -> IO ()
recreate (Device.D dvc) ci macc macd (F fb) =
	M.recreate dvc (createInfoToMiddle ci) macc macd fb
