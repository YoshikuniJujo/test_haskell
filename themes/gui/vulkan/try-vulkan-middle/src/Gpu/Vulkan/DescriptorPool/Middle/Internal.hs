{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorPool.Middle.Internal (
	D(..), CreateInfo(..), Size(..), create, destroy ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke (WithPoked, withPoked', withPtrS)
import Control.Arrow
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Word

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Descriptor.Enum
import Gpu.Vulkan.DescriptorPool.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.DescriptorPool.Core as C

data Size = Size { sizeType :: Type, sizeDescriptorCount :: Word32 }
	deriving Show

sizeToCore :: Size -> C.Size
sizeToCore Size { sizeType = Type tp, sizeDescriptorCount = dc } =
	C.Size { C.sizeType = tp, C.sizeDescriptorCount = dc }

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoMaxSets :: Word32,
	createInfoPoolSizes :: [Size] }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoMaxSets = ms,
	createInfoPoolSizes = (length &&& (sizeToCore <$>) -> (psc, pss))
	} f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray psc \ppss ->
	pokeArray ppss pss >>
	let	C.CreateInfo_ fci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoMaxSets = ms,
			C.createInfoPoolSizeCount = fromIntegral psc,
			C.createInfoPPoolSizes = ppss } in
	withForeignPtr fci f

newtype D = D C.P deriving Show

create :: (WithPoked (TMaybe.M mn), WithPoked c) =>
	Device.D -> CreateInfo mn -> Maybe (AllocationCallbacks.A c) -> IO D
create (Device.D dvc) ci mac = D <$> alloca \pp -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.maybeToCore mac \pac ->
		throwUnlessSuccess . Result =<< C.create dvc pci pac pp
	peek pp

destroy :: WithPoked d =>
	Device.D -> D -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (D p) mac =
	AllocationCallbacks.maybeToCore mac $ C.destroy dvc p
