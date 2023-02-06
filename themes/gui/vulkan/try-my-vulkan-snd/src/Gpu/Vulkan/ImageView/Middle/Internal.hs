{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView.Middle.Internal (
	I, CreateInfo(..), create, recreate, destroy,

	iToCore
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.Pointable
import Control.Monad.Cont
import Data.IORef

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Component.Middle.Internal
import Gpu.Vulkan.ImageView.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Image.Middle.Internal as Image
import qualified Gpu.Vulkan.ImageView.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoImage :: Image.I,
	createInfoViewType :: Type,
	createInfoFormat :: Format,
	createInfoComponents :: Mapping,
	createInfoSubresourceRange :: Image.SubresourceRange }

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoImage = Image.I rimg,
	createInfoViewType = Type tp,
	createInfoFormat = Format fmt,
	createInfoComponents = cpns,
	createInfoSubresourceRange = srr
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	(_, img) <- lift $ readIORef rimg
	let C.CreateInfo_ fCreateInfo = C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoImage = img,
		C.createInfoViewType = tp,
		C.createInfoFormat = fmt,
		C.createInfoComponents = mappingToCore cpns,
		C.createInfoSubresourceRange =
			Image.subresourceRangeToCore srr }
	ContT $ withForeignPtr fCreateInfo

newtype I = I (IORef C.I)

instance Show I where show _ = "Vk.ImageView.I"

iToCore :: I -> IO C.I
iToCore (I i) = readIORef i

iFromCore :: C.I -> IO I
iFromCore i = I <$> newIORef i

create :: (Pointable n, WithPoked c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO I
create (Device.D dvc) ci mac = ($ pure) . runContT $ lift . iFromCore =<< do
	pci <- createInfoToCore ci
	ContT \f -> alloca \pView -> do
		AllocationCallbacks.maybeToCore' mac \pac -> do
			r <- C.create dvc pci pac pView
			throwUnlessSuccess $ Result r
		f =<< peek pView

recreate :: (Pointable n, WithPoked c, WithPoked d) =>
	Device.D -> CreateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	I -> IO ()
recreate (Device.D dvc) ci macc macd (I ri) = ($ pure) $ runContT do
	pView <- ContT alloca
	pci <- createInfoToCore ci
	ContT \_f -> AllocationCallbacks.maybeToCore' macc \pac -> do
		r <- C.create dvc pci pac pView
		throwUnlessSuccess $ Result r
		io <- readIORef ri
		AllocationCallbacks.maybeToCore' macd $ C.destroy dvc io
		writeIORef ri =<< peek pView

destroy :: WithPoked d =>
	Device.D -> I -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) iv mac = do
	iv' <- iToCore iv
	AllocationCallbacks.maybeToCore' mac $ C.destroy dvc iv'
