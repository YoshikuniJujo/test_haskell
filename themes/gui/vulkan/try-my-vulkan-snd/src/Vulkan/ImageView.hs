{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.ImageView where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont

import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.Component
import Vulkan.ImageView.Enum

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.Image.Middle as Image
import qualified Vulkan.ImageView.Core as C
import qualified Vulkan.Format.Enum as Format

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoImage :: Image.I,
	createInfoViewType :: Type,
	createInfoFormat :: Format.F,
	createInfoComponents :: Mapping,
	createInfoSubresourceRange :: Image.SubresourceRange }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoImage = Image.I img,
	createInfoViewType = Type tp,
	createInfoFormat = Format.F fmt,
	createInfoComponents = cpns,
	createInfoSubresourceRange = srr
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
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

newtype I = I C.I deriving Show

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A n') -> IO I
create (Device.D dvc) ci mac = ($ pure) . runContT $ I <$> do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pView <- ContT alloca
	lift do	r <- C.create dvc pci pac pView
		throwUnlessSuccess $ Result r
		peek pView

destroy :: Pointable n =>
	Device.D -> I -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) (I iv) mac =
	($ pure) . runContT $ lift . C.destroy dvc iv =<< AllocationCallbacks.maybeToCore mac
