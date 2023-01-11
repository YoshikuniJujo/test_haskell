{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.Middle.Internal (
	L(..), CreateInfo(..), Binding(..), create, destroy ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.DescriptorSetLayout.Enum
import Gpu.Vulkan.Misc

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Sampler.Middle.Internal as Sampler
import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.DescriptorSetLayout.Core as C

data Binding = Binding {
	bindingBinding :: Word32,
	bindingDescriptorType :: Descriptor.Type,
	bindingDescriptorCountOrImmutableSamplers :: Either Word32 [Sampler.S],
	bindingStageFlags :: ShaderStageFlags }
	deriving Show

bindingToCore :: Binding -> ContT r IO C.Binding
bindingToCore Binding {
	bindingBinding = b,
	bindingDescriptorType = Descriptor.Type dt,
	bindingDescriptorCountOrImmutableSamplers =
		either ((, []) . Left) (Right . length &&& id) -> (dc, ss),
	bindingStageFlags = ShaderStageFlagBits sf } = do
		pss <- flip (either . const $ pure NullPtr) dc \c -> do
			p <- ContT $ allocaArray c
			p <$ lift (pokeArray p $ (\(Sampler.S s) -> s) <$> ss)
		pure C.Binding {
			C.bindingBinding = b,
			C.bindingDescriptorType = dt,
			C.bindingDescriptorCount = either id fromIntegral dc,
			C.bindingStageFlags = sf,
			C.bindingPImmutableSamplers = pss }

newtype L = L C.L deriving Show

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoBindings :: [Binding] }
	deriving Show

createInfoToCore :: Storable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoBindings = length &&& id -> (bc, bs) } = do
	(castPtr -> pnxt) <- maybeToStorable mnxt
	pbs <- ContT $ allocaArray bc
	cbs <- bindingToCore `mapM` bs
	lift $ pokeArray pbs cbs
	let	C.CreateInfo_ fci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoBindingCount = fromIntegral bc,
			C.createInfoPBindings = pbs }
	ContT $ withForeignPtr fci

create :: (Storable n, Storable c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO L
create (Device.D dvc) ci mac = (L <$>) . ($ pure) $ runContT do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pl <- ContT alloca
	lift do	r <- C.create dvc pci pac pl
		throwUnlessSuccess $ Result r
		peek pl

destroy :: Storable d =>
	Device.D -> L -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (L l) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy dvc l pac
