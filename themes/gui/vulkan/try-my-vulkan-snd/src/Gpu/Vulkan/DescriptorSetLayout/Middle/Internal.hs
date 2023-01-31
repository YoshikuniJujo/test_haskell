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
import Foreign.Storable.PeekPoke
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.DescriptorSetLayout.Enum
import Gpu.Vulkan.Misc.Middle.Internal

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

bindingToCore :: Binding -> (C.Binding -> IO a) -> IO a
bindingToCore Binding {
	bindingBinding = b,
	bindingDescriptorType = Descriptor.Type dt,
	bindingDescriptorCountOrImmutableSamplers =
		either ((, []) . Left) (Right . length &&& id) -> (dc, ss),
	bindingStageFlags = ShaderStageFlagBits sf } f = case dc of
		Left _ -> f $ mk NullPtr
		Right c -> allocaArray c \p -> do
			pokeArray p $ (\(Sampler.S s) -> s) <$> ss
			f $ mk p
	where mk p = C.Binding {
		C.bindingBinding = b,
		C.bindingDescriptorType = dt,
		C.bindingDescriptorCount = either id fromIntegral dc,
		C.bindingStageFlags = sf,
		C.bindingPImmutableSamplers = p }

newtype L = L C.L deriving Show

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoBindings :: [Binding] }
	deriving Show

createInfoToCore ::
	Pokable n => CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO a
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoBindings = length &&& id -> (bc, bs) } f =
	withPokedMaybe mnxt \(castPtr -> pnxt) ->
	allocaArray bc \pbs ->
	(bindingToCore `mapContM` bs) \cbs -> do
		pokeArray pbs cbs
		let	C.CreateInfo_ fci = C.CreateInfo {
				C.createInfoSType = (),
				C.createInfoPNext = pnxt,
				C.createInfoFlags = flgs,
				C.createInfoBindingCount = fromIntegral bc,
				C.createInfoPBindings = pbs }
		withForeignPtr fci f

create :: (Pokable n, WithPoked c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO L
create (Device.D dvc) ci mac = (L <$>) . ($ pure) $ runContT do
	pci <- ContT $ createInfoToCore ci
	pl <- ContT alloca
	lift $ AllocationCallbacks.maybeToCore' mac \pac -> do
		r <- C.create dvc pci pac pl
		throwUnlessSuccess $ Result r
	lift $ peek pl

destroy :: WithPoked d =>
	Device.D -> L -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (L l) mac =
	AllocationCallbacks.maybeToCore' mac \pac -> C.destroy dvc l pac
