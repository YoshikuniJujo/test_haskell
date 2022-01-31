{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Cache where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks
import Vulkan.Device
import Vulkan.PipelineCacheCreateFlagBits

import qualified Vulkan.AllocationCallbacks.Internal as I
import qualified Vulkan.Pipeline.Cache.Internal as I

data CreateInfo n d = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: PipelineCacheCreateFlags,
	createInfoInitialData :: d }
	deriving Show

createInfoToC :: (Pointable n, Storable d) =>
	CreateInfo n d -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt, createInfoFlags = flgs,
	createInfoInitialData = d } = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pd <- ContT alloca
	lift $ poke pd d
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoInitialDataSize = fromIntegral $ sizeOf d,
		I.createInfoPInitialData = castPtr pd }

data PipelineCacheTag
newtype PipelineCache = PipelineCache (Ptr PipelineCacheTag)
	deriving (Show, Storable)

pattern PipelineCacheNullHandle :: PipelineCache
pattern PipelineCacheNullHandle <- PipelineCache NullHandle where
	PipelineCacheNullHandle = PipelineCache NullHandle

create :: (Pointable n, Storable d, Pointable n') =>
	Device -> CreateInfo n d -> Maybe (AllocationCallbacks n') ->
	IO PipelineCache
create dvc ci mac = ($ pure) $ runContT do
	I.CreateInfo_ fci <- ContT $ createInfoToC ci
	pci <- ContT $ withForeignPtr fci
	pac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	ppc <- ContT alloca
	lift do	r <- c_vkCreatePipelineCache dvc pci pac ppc
		throwUnlessSuccess r
		peek ppc

foreign import ccall "vkCreatePipelineCache" c_vkCreatePipelineCache ::
	Device -> Ptr I.CreateInfo -> Ptr I.AllocationCallbacks ->
	Ptr PipelineCache -> IO Result
