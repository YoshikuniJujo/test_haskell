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
import Foreign.C.Enum
import Control.Monad.Cont
import Data.Default
import Data.Bits
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS

import Foreign.Pointable
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

createInfoToCore :: Pointable n =>
	CreateInfo n sknd -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoCode = cd } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	(p, n) <- lift . readFromByteString $ (\(Spv spv) -> spv) cd
	let C.CreateInfo_ fci = C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoCodeSize = n,
		C.createInfoPCode = p }
	ContT $ withForeignPtr fci

readFromByteString :: BS.ByteString -> IO (Ptr Word32, Word64)
readFromByteString (BS.PS f o l) = do
	p' <- mallocBytes l
	withForeignPtr f \p -> copyBytes p' (p `plusPtr` o) l
	pure (p', fromIntegral l)

create :: (Pointable n, Pointable c) =>
	Device.D ->
	CreateInfo n sknd -> Maybe (AllocationCallbacks.A c) -> IO (M sknd)
create (Device.D dvc) ci mac = (M <$>) . ($ pure) $ runContT do
	pcci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pm <- ContT alloca
	lift do	r <- C.create dvc pcci pac pm
		throwUnlessSuccess $ Result r
		peek pm

destroy :: Pointable d =>
	Device.D -> M sknd -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (M m) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy dvc m pac
