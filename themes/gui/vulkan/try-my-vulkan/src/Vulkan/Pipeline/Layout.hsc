{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Layout where

import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad.Cont

import Vulkan.Base
import Vulkan.Descriptor.SetLayout (DescriptorSetLayout)

import qualified Vulkan.Pipeline.Layout.Internal as I

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
