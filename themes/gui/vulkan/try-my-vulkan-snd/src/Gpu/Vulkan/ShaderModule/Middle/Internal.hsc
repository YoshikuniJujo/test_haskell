{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ShaderModule.Middle.Internal where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Data.Default
import Data.Bits
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS

import Shaderc
import Shaderc.EnumAuto

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.ShaderModule.Core as C

#include <vulkan/vulkan.h>

newtype M (sknd :: ShaderKind) = M C.Module deriving Show

enum "CreateFlagBits" ''#{type VkShaderModuleCreateFlags}
	[''Eq, ''Show, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

type CreateFlags = CreateFlagBits

instance Default CreateFlags where def = CreateFlagsZero

data CreateInfo n sknd = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoCode :: Spv sknd }
	deriving Show

createInfoToCore :: WithPoked n =>
	CreateInfo n sknd -> (Ptr C.CreateInfo -> IO r) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoCode = cd } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') -> do
		(p, n) <- readFromByteString $ (\(Spv spv) -> spv) cd
		let ci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoCodeSize = n,
			C.createInfoPCode = p }
		withPoked ci f

readFromByteString :: BS.ByteString -> IO (Ptr Word32, Word64)
readFromByteString (BS.PS f o l) = do
	p' <- mallocBytes l
	withForeignPtr f \p -> copyBytes p' (p `plusPtr` o) l
	pure (p', fromIntegral l)

create :: (WithPoked n, WithPoked c) =>
	Device.D ->
	CreateInfo n sknd -> Maybe (AllocationCallbacks.A c) -> IO (M sknd)
create (Device.D dvc) ci mac = M <$> alloca \pm -> do
	createInfoToCore ci \pcci ->
		AllocationCallbacks.maybeToCore mac \pac -> do
			r <- C.create dvc pcci pac pm
			throwUnlessSuccess $ Result r
	peek pm

destroy :: WithPoked d =>
	Device.D -> M sknd -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (M m) mac =
	AllocationCallbacks.maybeToCore mac $ C.destroy dvc m
