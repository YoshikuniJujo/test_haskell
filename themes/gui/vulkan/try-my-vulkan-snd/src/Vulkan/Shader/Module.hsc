{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Shader.Module where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS

import Foreign.Pointable
import Shaderc

import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.AllocationCallbacks (AllocationCallbacks, maybeToCore)
import Vulkan (Device(..))

import qualified Vulkan.Shader.Module.Core as C

#include <vulkan/vulkan.h>

newtype M = M C.Module deriving Show

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

create :: (Pointable n, Pointable n') =>
	Device -> CreateInfo n sknd -> Maybe (AllocationCallbacks n') -> IO M
create (Device dvc) ci mac = (M <$>) . ($ pure) $ runContT do
	pcci <- createInfoToCore ci
	pac <- maybeToCore mac
	pm <- ContT alloca
	lift do	r <- C.create dvc pcci pac pm
		throwUnlessSuccess $ Result r
		peek pm

destroy :: Pointable n => Device -> M -> Maybe (AllocationCallbacks n) -> IO ()
destroy (Device dvc) (M m) mac = ($ pure) $ runContT do
	pac <- maybeToCore mac
	lift $ C.destroy dvc m pac
