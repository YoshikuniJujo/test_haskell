{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.BindingFlags.Middle.Internal (
	CreateInfo(..) ) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Gpu.Vulkan.Descriptor.Enum qualified as Descriptor
import Gpu.Vulkan.DescriptorSetLayout.BindingFlags.Core qualified as C

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoBindingFlagsList :: [Descriptor.BindingFlags] }

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoBindingFlagsList = bfsl } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	let	bfsc = length bfsl in
	allocaArray bfsc \pbfs ->
	pokeArray pbfs (Descriptor.unBindingFlagBits <$> bfsl) >>
	f C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoBindingCount = fromIntegral bfsc,
		C.createInfoPBindingFlags = pbfs }

instance Sizable (CreateInfo mn) where
	sizeOf' = sizeOf @C.CreateInfo undefined
	alignment' = alignment @C.CreateInfo undefined

instance WithPoked (TMaybe.M mn) => WithPoked (CreateInfo mn) where
	withPoked' ci f = alloca \pcci -> do
		createInfoToCore ci \cci -> poke pcci cci
		f . ptrS $ castPtr pcci
