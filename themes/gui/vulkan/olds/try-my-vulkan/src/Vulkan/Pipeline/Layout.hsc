{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Layout where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks
import Vulkan.Device
import Vulkan.Descriptor.SetLayout (DescriptorSetLayout)

import qualified Vulkan.Pipeline.Layout.Internal as I
import qualified Vulkan.AllocationCallbacks.Internal as I

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: I.CreateFlags,
	createInfoSetLayouts :: [DescriptorSetLayout],
	createInfoPushConstantRanges :: [I.PushConstantRange] }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSetLayouts = dsls,
	createInfoPushConstantRanges = pcrs
	} = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	let	dslc = length dsls
	pdsls <- ContT $ allocaArray dslc
	lift $ pokeArray pdsls dsls
	let	pcrc = length pcrs
	ppcrs <- ContT $ allocaArray pcrc
	lift $ pokeArray ppcrs pcrs
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoSetLayoutCount = fromIntegral dslc,
		I.createInfoPSetLayouts = pdsls,
		I.createInfoPushConstantRangeCount = fromIntegral pcrc,
		I.createInfoPPushConstantRanges = ppcrs }

data PipelineLayoutTag
newtype PipelineLayout = PipelineLayout (Ptr PipelineLayoutTag)
	deriving (Show, Storable)

createPipelineLayout :: (Pointable n, Pointable n') =>
	Device -> CreateInfo n -> Maybe (AllocationCallbacks n') ->
	IO PipelineLayout
createPipelineLayout dvc ci mac = ($ pure) $ runContT do
	I.CreateInfo_ fci <- ContT $ createInfoToC ci
	pci <- ContT $ withForeignPtr fci
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	ppl <- ContT alloca
	lift do	r <- c_vkCreatePipelineLayout dvc pci pac ppl
		throwUnlessSuccess r
		peek ppl

foreign import ccall "vkCreatePipelineLayout" c_vkCreatePipelineLayout ::
	Device -> Ptr I.CreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr PipelineLayout -> IO Result

destroyPipelineLayout :: Pointable n =>
	Device -> PipelineLayout -> Maybe (AllocationCallbacks n) -> IO ()
destroyPipelineLayout dvc ppl mac = ($ pure) $ runContT do
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift $ c_vkDestroyPipelineLayout dvc ppl pac

foreign import ccall "vkDestroyPipelineLayout" c_vkDestroyPipelineLayout ::
	Device -> PipelineLayout -> Ptr I.AllocationCallbacks -> IO ()
