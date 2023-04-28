{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Semaphore.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Default
import Data.Bits
import Data.Word

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.AllocationCallbacks.Middle.Internal as AllocationCallbacks
import qualified Gpu.Vulkan.Semaphore.Core as C

import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as Device

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkSemaphoreCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn, createInfoFlags :: CreateFlags }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

instance Default (CreateInfo 'Nothing) where
	def = CreateInfo {
		createInfoNext = TMaybe.N, createInfoFlags = zeroBits }

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO r) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	let ci = C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoFlags = flgs } in withPoked ci f

newtype S = S { unS :: C.S } deriving Show

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> Maybe (AllocationCallbacks.A c) -> IO S
create (Device.D dvc) ci mac = S <$> alloca \ps -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.maybeToCoreNew mac \pac ->
			throwUnlessSuccess . Result
				=<< C.create dvc pci pac ps
	peek ps

destroy :: Device.D -> S -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (S s) mac =
	AllocationCallbacks.maybeToCoreNew mac $ C.destroy dvc s
