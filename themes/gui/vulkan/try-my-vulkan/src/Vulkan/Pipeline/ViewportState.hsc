{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ViewportState where

import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.Viewport

import qualified Vulkan.Pipeline.ViewportState.Internal as I

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n, createInfoFlags :: I.CreateFlags,
	createInfoViewports :: [Viewport], createInfoScissors :: [Rect2d] }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt, createInfoFlags = flgs,
	createInfoViewports = vps, createInfoScissors = sss } = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	let	vpc = length vps
	pvps <- ContT $ allocaArray vpc
	lift $ pokeArray pvps vps
	let	ssc = length sss
	psss <- ContT $ allocaArray ssc
	lift $ pokeArray psss sss
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt, I.createInfoFlags = flgs,
		I.createInfoViewportCount = fromIntegral vpc,
		I.createInfoPViewports = pvps,
		I.createInfoScissorCount = fromIntegral ssc,
		I.createInfoPScissors = psss }
