{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.DescriptorSet.Layout where

import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Vulkan.Enum

import qualified Vulkan.Shader.Stage.Enum as Shader.Stage
import qualified Vulkan.Sampler as Sampler
import qualified Vulkan.DescriptorSet.Layout.Core as C

data Binding = Binding {
	bindingBinding :: Word32,
	bindingDescriptorType :: DescriptorType,
	bindingDescriptorCountOrImmutableSamplers :: Either Word32 [Sampler.S],
	bindingStageFlags :: Shader.Stage.Flags }
	deriving Show

bindingToCore :: Binding -> ContT r IO C.Binding
bindingToCore Binding {
	bindingBinding = b,
	bindingDescriptorType = DescriptorType dt,
	bindingDescriptorCountOrImmutableSamplers =
		either ((, []) . Left) (Right . length &&& id) -> (dc, ss),
	bindingStageFlags = Shader.Stage.FlagBits sf } = do
		pss <- flip (either . const $ pure NullPtr) dc \c -> do
			p <- ContT $ allocaArray c
			p <$ lift (pokeArray p $ (\(Sampler.S s) -> s) <$> ss)
		pure C.Binding {
			C.bindingBinding = b,
			C.bindingDescriptorType = dt,
			C.bindingDescriptorCount = either id fromIntegral dc,
			C.bindingStageFlags = sf,
			C.bindingPImmutableSamplers = pss
			}

newtype L = L C.L deriving Show
