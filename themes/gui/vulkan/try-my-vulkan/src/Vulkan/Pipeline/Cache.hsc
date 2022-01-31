{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Cache where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.PipelineCacheCreateFlagBits

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
