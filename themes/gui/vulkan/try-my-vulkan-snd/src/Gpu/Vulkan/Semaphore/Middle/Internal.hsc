{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Semaphore.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
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

data CreateInfo n = 
	CreateInfo { createInfoNext :: Maybe n, createInfoFlags :: CreateFlags }
	deriving Show

instance Default (CreateInfo n) where
	def = CreateInfo {
		createInfoNext = Nothing, createInfoFlags = zeroBits }

createInfoToCore :: WithPoked n =>
	CreateInfo n -> (Ptr C.CreateInfo -> IO r) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	let ci = C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoFlags = flgs } in withPoked ci f

newtype S = S { unS :: C.S } deriving Show

create :: (WithPoked n, WithPoked c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO S
create (Device.D dvc) ci mac = S <$> alloca \ps -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.maybeToCore' mac \pac ->
			throwUnlessSuccess . Result
				=<< C.create dvc pci pac ps
	peek ps

destroy :: WithPoked d =>
	Device.D -> S -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (S s) mac =
	AllocationCallbacks.maybeToCore' mac $ C.destroy dvc s
