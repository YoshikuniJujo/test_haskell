{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.InputAssemblyState where

import Foreign.Ptr
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.PrimitiveTopology

import qualified Vulkan.Pipeline.InputAssemblyState.Internal as I

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: I.CreateFlags,
	createInfoTopology :: PrimitiveTopology,
	createInfoPrimitiveRestartEnable :: Bool }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt, createInfoFlags = flgs, createInfoTopology = tp,
	createInfoPrimitiveRestartEnable = re } = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pure I.CreateInfo {
		I.createInfoSType = (), I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs, I.createInfoTopology = tp,
		I.createInfoPrimitiveRestartEnable = boolToBool32 re }
