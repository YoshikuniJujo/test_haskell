{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence.Middle.Internal (
	F(..), CreateInfo(..), create, destroy,

	waitForFs, resetFs,

	maybeFToCore ) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke (
	withPoked, WithPoked, withPokedMaybe', withPtrS )
import Control.Arrow
import Data.Default
import Data.Bits
import Data.Word

import Gpu.Vulkan.Misc.Middle.Internal
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Fence.Enum

import qualified Gpu.Vulkan.AllocationCallbacks.Middle.Internal as AllocationCallbacks
import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Fence.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags }
	deriving Show

instance Default (CreateInfo n) where
	def = CreateInfo {
		createInfoNext = Nothing, createInfoFlags = zeroBits }

createInfoToCore :: WithPoked n =>
	CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt, createInfoFlags = CreateFlagBits flgs } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
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

create :: (WithPoked n, WithPoked c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO F
create (Device.D dvc) ci mac = F <$> alloca \pf -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.maybeToCore mac \pac -> do
			r <- C.create dvc pci pac pf
			throwUnlessSuccess $ Result r
	peek pf

destroy :: WithPoked d =>
	Device.D -> F -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (F f) mac =
	AllocationCallbacks.maybeToCore mac $ C.destroy dvc f

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
