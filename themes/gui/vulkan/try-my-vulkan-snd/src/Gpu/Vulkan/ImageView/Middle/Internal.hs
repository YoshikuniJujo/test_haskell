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

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A n') -> IO I
create (Device.D dvc) ci mac = ($ pure) . runContT $ lift . iFromCore =<< do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pView <- ContT alloca
	lift do	r <- C.create dvc pci pac pView
		throwUnlessSuccess $ Result r
		peek pView

recreate :: (Pointable n, Pointable c, Pointable d) =>
	Device.D -> CreateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	I -> IO ()
recreate (Device.D dvc) ci macc macd (I ri) = ($ pure) $ runContT do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore macc
	pView <- ContT alloca
	lift do	r <- C.create dvc pci pac pView
		throwUnlessSuccess $ Result r
	io <- lift $ readIORef ri
	lift . C.destroy dvc io =<< AllocationCallbacks.maybeToCore macd
	lift $ writeIORef ri =<< peek pView

destroy :: Pointable n =>
	Device.D -> I -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) iv mac = do
	iv' <- iToCore iv
	($ pure) . runContT $ lift . C.destroy dvc iv' =<< AllocationCallbacks.maybeToCore mac
