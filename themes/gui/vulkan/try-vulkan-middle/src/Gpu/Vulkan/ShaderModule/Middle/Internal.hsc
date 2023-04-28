{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ShaderModule.Middle.Internal where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Data.TypeLevel.Maybe qualified as TMaybe
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

data CreateInfo mn sknd = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoCode :: Spv sknd }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn sknd)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn sknd -> (Ptr C.CreateInfo -> IO r) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoCode = cd } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') -> do
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

create :: WithPoked (TMaybe.M mn) =>
	Device.D ->
	CreateInfo mn sknd -> Maybe (AllocationCallbacks.A c) -> IO (M sknd)
create (Device.D dvc) ci mac = M <$> alloca \pm -> do
	createInfoToCore ci \pcci ->
		AllocationCallbacks.maybeToCoreNew mac \pac -> do
			r <- C.create dvc pcci pac pm
			throwUnlessSuccess $ Result r
	peek pm

destroy :: Device.D -> M sknd -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (M m) mac =
	AllocationCallbacks.maybeToCoreNew mac $ C.destroy dvc m
