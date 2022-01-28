{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.SetLayout where

import Foreign.Marshal.Array
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.DescriptorType
import Vulkan.ShaderStageFlagBits
import Vulkan.Sampler
import Vulkan.DescriptorSetLayoutCreateFlagBits

import qualified Vulkan.Descriptor.SetLayout.Internal as I

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

data CrateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: DescriptorSetLayoutCreateFlags,
	createInfoBindings :: [Binding] }
	deriving Show
