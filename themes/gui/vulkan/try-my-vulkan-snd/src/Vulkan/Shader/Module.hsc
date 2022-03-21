{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Shader.Module where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr
import Control.Monad.Cont
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS

import Foreign.Pointable
import Shaderc

import qualified Vulkan.Shader.Module.Core as C

#include <vulkan/vulkan.h>

newtype CreateFlags =
	CreateFlags #{type VkShaderModuleCreateFlags} deriving Show

createFlagsZero :: CreateFlags
createFlagsZero = CreateFlags 0

data CreateInfo n sknd = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoCode :: Spv sknd }
	deriving Show

createInfoToCore :: Pointable n =>
	CreateInfo n sknd -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
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
