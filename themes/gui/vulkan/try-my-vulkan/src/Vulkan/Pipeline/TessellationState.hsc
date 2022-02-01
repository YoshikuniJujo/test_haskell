{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.TessellationState where

import Foreign.Ptr
import Control.Monad.Cont
import Data.Word

import Vulkan.Base

import qualified Vulkan.Pipeline.TessellationState.Internal as I

#include <vulkan/vulkan.h>

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: I.CreateFlags,
	createInfoPatchControlPoints :: Word32 }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoPatchControlPoints = word32ToUint32T -> pcps
	} = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoPatchControlPoints = pcps }
