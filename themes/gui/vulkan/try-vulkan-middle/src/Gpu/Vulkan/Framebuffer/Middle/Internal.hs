{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer.Middle.Internal (
	F, CreateInfo(..), create, recreate, destroy,

	fToCore ) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke (
	withPoked, WithPoked, withPokedMaybe', withPtrS )
import Control.Arrow
import Data.IORef
import Data.Word

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Framebuffer.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as Device
import {-# SOURCE #-} qualified Gpu.Vulkan.RenderPass.Middle.Internal as RenderPass
import qualified Gpu.Vulkan.ImageView.Middle.Internal as ImageView
import qualified Gpu.Vulkan.Framebuffer.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoRenderPass :: RenderPass.R,
	createInfoAttachments :: [ImageView.I],
	createInfoWidth :: Word32,
	createInfoHeight :: Word32,
	createInfoLayers :: Word32 }
	deriving Show

createInfoToCore :: WithPoked n =>
	CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoRenderPass = RenderPass.R rp,
	createInfoAttachments = length &&& id -> (ac, as),
	createInfoWidth = w,
	createInfoHeight = h,
	createInfoLayers = l } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
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

create :: (WithPoked n, WithPoked c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO F
create (Device.D dvc) ci mac = fFromCore =<< alloca \pf -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.maybeToCore mac \pac -> do
			throwUnlessSuccess . Result =<< C.create dvc pci pac pf
	peek pf

destroy :: WithPoked d =>
	Device.D -> F -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) f mac = AllocationCallbacks.maybeToCore mac \pac -> do
	f' <- fToCore f; C.destroy dvc f' pac

recreate :: (WithPoked n, WithPoked c, WithPoked d) =>
	Device.D -> CreateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	F -> IO ()
recreate (Device.D dvc) ci macc macd f@(F rf) =
	fToCore f >>= \o -> alloca \pf ->
	createInfoToCore ci \pci ->
	AllocationCallbacks.maybeToCore macc \pacc ->
	AllocationCallbacks.maybeToCore macd \pacd -> do
		r <- C.create dvc pci pacc pf
		throwUnlessSuccess $ Result r
		writeIORef rf =<< peek pf
		C.destroy dvc o pacd
