{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.MultisampleState where

import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.SampleCountFlagBits

import qualified Vulkan.Pipeline.MultisampleState.Internal as I

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: I.CreateFlags,
	createInfoRasterizationSamples :: SampleCountFlags,
	createInfoSampleShadingEnable :: Bool,
	createInfoMinSampleShading :: Float,
	createInfoSampleMasks :: [SampleMask],
	createInfoAlphaToCoverageEnable :: Bool,
	createInfoAlphaToOneEnable :: Bool }
	deriving Show

createInfoToC :: Pointable n => CreateInfo n -> (I.CreateInfo -> IO a) -> IO a
createInfoToC CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoRasterizationSamples = rss@(SampleCountFlagBits scfbs),
	createInfoSampleShadingEnable = (boolToBool32 -> sse),
	createInfoMinSampleShading = (floatToFloat -> mss),
	createInfoSampleMasks = (map (\(SampleMask sm) -> sm) -> sms),
	createInfoAlphaToCoverageEnable = (boolToBool32 -> atce),
	createInfoAlphaToOneEnable = (boolToBool32 -> atoe) } = runContT do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	let	smc = (scfbs - 1) `div` 32 + 1
		sms' = SampleMask <$> sms ++ replicate (fromIntegral smc - length sms) 0
	psms <- ContT $ allocaArray (length sms')
	lift $ pokeArray psms sms'
	pure I.CreateInfo {
		I.createInfoSType = (),
		I.createInfoPNext = pnxt,
		I.createInfoFlags = flgs,
		I.createInfoRasterizationSamples = rss,
		I.createInfoSampleShadingEnable = sse,
		I.createInfoMinSampleShading = mss,
		I.createInfoPSampleMask = psms,
		I.createInfoAlphaToCoverageEnable = atce,
		I.createInfoAlphaToOneEnable = atoe }
