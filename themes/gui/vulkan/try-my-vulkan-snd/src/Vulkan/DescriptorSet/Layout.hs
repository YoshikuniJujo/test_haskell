{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.DescriptorSet.Layout where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Vulkan.Enum
import Vulkan.DescriptorSet.Layout.Enum

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
			C.bindingPImmutableSamplers = pss }

newtype L = L C.L deriving Show

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoBindings :: [Binding] }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO C.CreateInfo
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoBindings = length &&& id -> (bc, bs) } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pbs <- ContT $ allocaArray bc
	cbs <- bindingToCore `mapM` bs
	lift $ pokeArray pbs cbs
	pure C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt,
		C.createInfoFlags = flgs,
		C.createInfoBindingCount = fromIntegral bc,
		C.createInfoPBindings = pbs }
