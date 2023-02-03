{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer.Middle.Internal (
	F, CreateInfo(..), create, recreate, destroy,

	fToCore
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
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

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoRenderPass = RenderPass.R rp,
	createInfoAttachments = length &&& id -> (ac, as),
	createInfoWidth = w,
	createInfoHeight = h,
	createInfoLayers = l } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pas <- ContT $ allocaArray ac
	lift $ pokeArray pas =<< ImageView.iToCore `mapM` as
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoRenderPass = rp,
			C.createInfoAttachmentCount = fromIntegral ac,
			C.createInfoPAttachments = pas,
			C.createInfoWidth = w,
			C.createInfoHeight = h,
			C.createInfoLayers = l }
	ContT $ withForeignPtr fCreateInfo

newtype F = F (IORef C.F)

fToCore :: F -> IO C.F
fToCore (F f) = readIORef f

fFromCore :: C.F -> IO F
fFromCore f = F <$> newIORef f

create :: (Pointable n, WithPoked c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO F
create (Device.D dvc) ci mac = ($ pure) . runContT $ lift . fFromCore =<< do
	pci <- createInfoToCore ci
	ContT \f -> alloca \pf -> do
		AllocationCallbacks.maybeToCore' mac \pac -> do
			r <- C.create dvc pci pac pf
			throwUnlessSuccess $ Result r
		f =<< peek pf

destroy :: WithPoked d =>
	Device.D -> F -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) f mac = AllocationCallbacks.maybeToCore' mac \pac -> do
	f' <- fToCore f; C.destroy dvc f' pac

recreate :: (Pointable n, WithPoked c, WithPoked d) =>
	Device.D -> CreateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	F -> IO ()
recreate (Device.D dvc) ci macc macd f@(F rf) = ($ pure) $ runContT do
	o <- lift $ fToCore f
	pci <- createInfoToCore ci
	ContT \_f -> alloca \pf ->
		AllocationCallbacks.maybeToCore' macc \pacc ->
		AllocationCallbacks.maybeToCore' macd \pacd -> do
			r <- C.create dvc pci pacc pf
			throwUnlessSuccess $ Result r
			writeIORef rf =<< peek pf
			C.destroy dvc o pacd
