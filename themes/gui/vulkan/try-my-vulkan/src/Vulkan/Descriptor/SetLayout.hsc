{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.SetLayout where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks
import Vulkan.Device
import Vulkan.DescriptorType
import Vulkan.ShaderStageFlagBits
import Vulkan.Sampler (Sampler)
import Vulkan.DescriptorSetLayoutCreateFlagBits

import qualified Vulkan.Descriptor.SetLayout.Internal as I
import qualified Vulkan.AllocationCallbacks.Internal as I

#include <vulkan/vulkan.h>

data Binding = Binding {
	bindingBinding :: Word32,
	bindingDescriptorType :: DescriptorType,
	bindingDescriptorCount :: Word32,
	bindingStageFlags :: ShaderStageFlags,
	bindingImmutableSamplers :: Maybe [Sampler] }
	deriving Show

checkBinding :: Binding -> IO ()
checkBinding Binding {
	bindingDescriptorCount = dc, bindingImmutableSamplers = miss } =
	case miss of
		Nothing -> pure ()
		Just iss
			| fromIntegral dc == length iss -> pure ()
			| otherwise -> error $
				"Bindings: bindingImmutableSamplers should have " ++
				"bindingDescriptorCount element if it is not Nothing"

word32ToUint32 :: Word32 -> #{type uint32_t}
word32ToUint32 = fromIntegral

bindingToC :: Binding -> (I.Binding -> IO a) -> IO a
bindingToC b@Binding {
	bindingBinding = (word32ToUint32 -> bd),
	bindingDescriptorType = dt,
	bindingDescriptorCount = (word32ToUint32 -> dc),
	bindingStageFlags = sfs,
	bindingImmutableSamplers = miss } = runContT do
	lift $ checkBinding b
	piss <- case miss of
		Nothing -> pure NullPtr
		Just iss -> do
			p <- ContT . allocaArray $ length miss
			p <$ lift (pokeArray p iss)
	pure I.Binding {
		I.bindingBinding = bd,
		I.bindingDescriptorType = dt,
		I.bindingDescriptorCount = dc,
		I.bindingStageFlags = sfs,
		I.bindingPImmutableSamplers = piss }

bindingsToC :: [Binding] -> ([I.Binding] -> IO a) -> IO a
bindingsToC = runContT . ((ContT . bindingToC) `mapM`)

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: DescriptorSetLayoutCreateFlags,
	createInfoBindings :: [Binding] }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt, createInfoFlags = flgs,
	createInfoBindings = bds } = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	ibds <- ContT $ bindingsToC bds
	let	ibdc = length ibds
	pbds <- ContT $ allocaArray ibdc
	lift $ pokeArray pbds ibds
	pure $ I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoBindingCount = fromIntegral ibdc,
		I.createInfoPBindings = pbds }

data DescriptorSetLayoutTag
newtype DescriptorSetLayout = DescriptorSetLayout (Ptr DescriptorSetLayoutTag)
	deriving (Show, Storable)
type PtrDescriptorSetLayout = Ptr DescriptorSetLayout

createDescriptorSetLayout :: (Pointable n, Pointable n') =>
	Device -> CreateInfo n -> Maybe (AllocationCallbacks n') ->
	IO DescriptorSetLayout
createDescriptorSetLayout dvc ci mac = ($ pure) $ runContT do
	I.CreateInfo_ fci <- ContT $ createInfoToC ci
	pci <- ContT $ withForeignPtr fci
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	pdsl <- ContT alloca
	lift do	r <- c_vkCreateDescriptorSetLayout dvc pci pac pdsl
		throwUnlessSuccess r
		peek pdsl

foreign import ccall "vkCreateDescriptorSetLayout"
	c_vkCreateDescriptorSetLayout ::
	Device -> Ptr I.CreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr DescriptorSetLayout -> IO Result
