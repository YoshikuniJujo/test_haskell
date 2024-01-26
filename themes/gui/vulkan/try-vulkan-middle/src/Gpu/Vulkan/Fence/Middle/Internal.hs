{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence.Middle.Internal (

	-- * CREATE AND DESTROY

	create, destroy, F(..), CreateInfo(..),

	-- * RESET AND WAIT

	resetFs, waitForFs,

	-- * INTERNAL USE

	maybeFToCore

	) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke (withPoked, withPoked', WithPoked, withPtrS)
import Control.Arrow
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Default
import Data.Bits
import Data.Word

import Gpu.Vulkan.Base.Middle.Internal
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Fence.Enum

import qualified Gpu.Vulkan.AllocationCallbacks.Middle.Internal as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Types as Device
import qualified Gpu.Vulkan.Fence.Core as C

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

instance Default (CreateInfo 'Nothing) where
	def = CreateInfo {
		createInfoNext = TMaybe.N, createInfoFlags = zeroBits }

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt, createInfoFlags = CreateFlagBits flgs } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
		withPoked C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs } f

newtype F = F C.F deriving Show

fToCore :: F -> C.F
fToCore (F f) = f

maybeFToCore :: Maybe F -> C.F
maybeFToCore Nothing = NullHandle
maybeFToCore (Just f) = fToCore f

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO F
create (Device.D dvc) ci mac = F <$> alloca \pf -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.mToCore mac \pac -> do
			r <- C.create dvc pci pac pf
			throwUnlessSuccess $ Result r
	peek pf

destroy :: Device.D -> F -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) (F f) mac =
	AllocationCallbacks.mToCore mac $ C.destroy dvc f

waitForFs :: Device.D -> [F] -> Bool -> Word64 -> IO ()
waitForFs (Device.D dvc) (length &&& ((\(F f) -> f) <$>) -> (fc, fs))
	(boolToBool32 -> wa) to = allocaArray fc \pfs -> do
		pokeArray pfs fs
		r <- C.waitForFs dvc (fromIntegral fc) pfs wa to
		throwUnlessSuccess $ Result r

resetFs :: Device.D -> [F] -> IO ()
resetFs (Device.D dvc)
	(length &&& ((\(F f) -> f) <$>) -> (fc, fs)) = allocaArray fc \pfs -> do
		pokeArray pfs fs
		r <- C.resetFs dvc (fromIntegral fc) pfs
		throwUnlessSuccess $ Result r
