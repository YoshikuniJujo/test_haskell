{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.Intermediate where

import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad.Cont

import Vulkan.Base

import qualified Vulkan.Pipeline.VertexInputState.Internal as I

data PipelineVertexInputStateCreateInfo n = PipelineVertexInputStateCreateInfo {
	pipelineVertexInputStateCreateInfoNext :: Maybe n,
	pipelineVertexInputStateCreateInfoFlags ::
		I.PipelineVertexInputStateCreateFlags,
	pipelineVertexInputStateCreateInfoVertexBindingDescriptions ::
		[I.VertexInputBindingDescription],
	pipelineVertexInputStateCreateInfoVertexAttributeDescriptions ::
		[I.VertexInputAttributeDescription] }
	deriving Show

pipelineVertexInputStateCreateInfoToC :: Pointable n =>
	PipelineVertexInputStateCreateInfo n -> (I.CreateInfo -> IO a) -> IO a
pipelineVertexInputStateCreateInfoToC PipelineVertexInputStateCreateInfo {
	pipelineVertexInputStateCreateInfoNext = mnxt,
	pipelineVertexInputStateCreateInfoFlags = flgs,
	pipelineVertexInputStateCreateInfoVertexBindingDescriptions = vbds,
	pipelineVertexInputStateCreateInfoVertexAttributeDescriptions = vads
	} = runContT do
		(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
		let	vbdc = length vbds
		pvbds <- ContT $ allocaArray vbdc
		lift $ pokeArray pvbds vbds
		let	vadc = length vads
		pvads <- ContT $ allocaArray vadc
		lift $ pokeArray pvads vads
		pure I.CreateInfo {
			I.createInfoSType = (),
			I.createInfoPNext = pnxt,
			I.createInfoFlags = flgs,
			I.createInfoVertexBindingDescriptionCount
				= fromIntegral vbdc,
			I.createInfoPVertexBindingDescriptions = pvbds,
			I.createInfoVertexAttributeDescriptionCount
				= fromIntegral vadc,
			I.createInfoPVertexAttributeDescriptions = pvads }
