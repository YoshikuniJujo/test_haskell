{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.DynamicState where

import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.DynamicState

import qualified Vulkan.Pipeline.DynamicState.Internal as I

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: I.CreateFlags,
	createInfoDynamicStates :: [DynamicState] }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoDynamicStates = dss
	} = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	let	dsc = length dss
	pdss <- ContT $ allocaArray dsc
	lift $ pokeArray pdss dss
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoDynamicStateCount = fromIntegral dsc,
		I.createInfoPDynamicStates = pdss }
