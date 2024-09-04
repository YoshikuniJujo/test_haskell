{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Semaphore.Middle.Internal (

	-- * CREATE AND DESTROY

	create, destroy, S(..), CreateInfo(..), CreateFlags,

	-- * SIGNAL

	signal, SignalInfo(..)

	) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Default
import Data.Bits
import Data.Word

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.AllocationCallbacks.Middle.Internal as AllocationCallbacks
import qualified Gpu.Vulkan.Semaphore.Core as C

import qualified Gpu.Vulkan.Device.Middle.Types as Device

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
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO S
create (Device.D dvc) ci mac = S <$> alloca \ps -> do
	createInfoToCore ci \pci -> AllocationCallbacks.mToCore mac \pac ->
		throwUnlessSuccess . Result =<< C.create dvc pci pac ps
	peek ps

destroy :: Device.D -> S -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) (S s) mac =
	AllocationCallbacks.mToCore mac $ C.destroy dvc s

data SignalInfo mn = SignalInfo {
	signalInfoNext :: TMaybe.M mn, signalInfoSemaphore :: S,
	signalInfoValue :: Word64 }

signalInfoToCore :: WithPoked (TMaybe.M mn) =>
	SignalInfo mn -> (Ptr C.SignalInfo -> IO r) -> IO ()
signalInfoToCore SignalInfo {
	signalInfoNext = mnxt,
	signalInfoSemaphore = S s,
	signalInfoValue = v } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	let	si = C.SignalInfo {
			C.signalInfoSType = (),
			C.signalInfoPNext = pnxt',
			C.signalInfoSemaphore = s,
			C.signalInfoValue = fromIntegral v } in withPoked si f

signal :: WithPoked (TMaybe.M mn) => Device.D -> SignalInfo mn -> IO ()
signal (Device.D dvc) si = signalInfoToCore si \psi ->
	throwUnlessSuccess . Result =<< C.signal dvc psi
