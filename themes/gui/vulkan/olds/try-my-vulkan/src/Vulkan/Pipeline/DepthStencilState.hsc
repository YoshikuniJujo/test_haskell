{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.DepthStencilState where

import Foreign.Ptr
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.CompareOp

import qualified Vulkan.Pipeline.DepthStencilState.Internal as I

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: I.CreateFlags,
	createInfoDepthTestEnable :: Bool,
	createInfoDepthWriteEnable :: Bool,
	createInfoDepthCompareOp :: CompareOp,
	createInfoDepthBoundsTestEnable :: Bool,
	createInfoStencilTestEnable :: Bool,
	createInfoFront :: I.StencilOpState,
	createInfoBack :: I.StencilOpState,
	createInfoMinDepthBounds :: Float,
	createInfoMaxDepthBounds :: Float }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt, createInfoFlags = flgs,
	createInfoDepthTestEnable = (boolToBool32 -> dte),
	createInfoDepthWriteEnable = (boolToBool32 -> dwe),
	createInfoDepthCompareOp = dco,
	createInfoDepthBoundsTestEnable = (boolToBool32 -> dbte),
	createInfoStencilTestEnable = (boolToBool32 -> ste),
	createInfoFront = fr, createInfoBack = bk,
	createInfoMinDepthBounds = (floatToFloat -> mndb),
	createInfoMaxDepthBounds = (floatToFloat -> mxdb)
	} = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt, I.createInfoFlags = flgs,
		I.createInfoDepthTestEnable = dte,
		I.createInfoDepthWriteEnable = dwe,
		I.createInfoDepthCompareOp = dco,
		I.createInfoDepthBoundsTestEnable = dbte,
		I.createInfoStencilTestEnable = ste,
		I.createInfoFront = fr, I.createInfoBack = bk,
		I.createInfoMinDepthBounds = mndb,
		I.createInfoMaxDepthBounds = mxdb
		}
