{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer.Middle.Internal (
	F, CreateInfo(..), create, recreate, destroy,

	fToCore ) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke (
	withPoked, WithPoked, withPoked', withPtrS )
import Control.Arrow
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Word
import Data.IORef

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Framebuffer.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Types as Device
import {-# SOURCE #-} qualified Gpu.Vulkan.RenderPass.Middle.Internal as RenderPass
import qualified Gpu.Vulkan.ImageView.Middle.Internal as ImageView
import qualified Gpu.Vulkan.Framebuffer.Core as C

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoRenderPass :: RenderPass.R,
	createInfoAttachments :: [ImageView.I],
	createInfoWidth :: Word32,
	createInfoHeight :: Word32,
	createInfoLayers :: Word32 }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoRenderPass = RenderPass.R rp,
	createInfoAttachments = length &&& id -> (ac, as),
	createInfoWidth = w,
	createInfoHeight = h,
	createInfoLayers = l } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray ac \pas -> do
		pokeArray pas =<< ImageView.iToCore `mapM` as
		let	ci = C.CreateInfo {
				C.createInfoSType = (),
				C.createInfoPNext = pnxt',
				C.createInfoFlags = flgs,
				C.createInfoRenderPass = rp,
				C.createInfoAttachmentCount = fromIntegral ac,
				C.createInfoPAttachments = pas,
				C.createInfoWidth = w,
				C.createInfoHeight = h,
				C.createInfoLayers = l }
		withPoked ci f

newtype F = F (IORef C.F)

fToCore :: F -> IO C.F
fToCore (F f) = readIORef f

fFromCore :: C.F -> IO F
fFromCore f = F <$> newIORef f

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO F
create (Device.D dvc) ci mac = fFromCore =<< alloca \pf -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.mToCore mac \pac -> do
			throwUnlessSuccess . Result =<< C.create dvc pci pac pf
	peek pf

destroy :: Device.D -> F -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) f mac = AllocationCallbacks.mToCore mac \pac -> do
	f' <- fToCore f; C.destroy dvc f' pac

recreate :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc ->
	TPMaybe.M AllocationCallbacks.A md -> F -> IO ()
recreate (Device.D dvc) ci macc macd f@(F rf) =
	fToCore f >>= \o -> alloca \pf ->
	createInfoToCore ci \pci ->
	AllocationCallbacks.mToCore macc \pacc ->
	AllocationCallbacks.mToCore macd \pacd -> do
		r <- C.create dvc pci pacc pf
		throwUnlessSuccess $ Result r
		writeIORef rf =<< peek pf
		C.destroy dvc o pacd
