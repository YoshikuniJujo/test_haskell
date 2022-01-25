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
	PipelineVertexInputStateCreateInfo n ->
	(I.PipelineVertexInputStateCreateInfo -> IO a) -> IO a
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
		pure I.PipelineVertexInputStateCreateInfo {
			I.pipelineVertexInputStateCreateInfoSType = (),
			I.pipelineVertexInputStateCreateInfoPNext = pnxt,
			I.pipelineVertexInputStateCreateInfoFlags = flgs,
			I.pipelineVertexInputStateCreateInfoVertexBindingDescriptionCount
				= fromIntegral vbdc,
			I.pipelineVertexInputStateCreateInfoPVertexBindingDescriptions
				= pvbds,
			I.pipelineVertexInputStateCreateInfoVertexAttributeDescriptionCount
				= fromIntegral vadc,
			I.pipelineVertexInputStateCreateInfoPVertexAttributeDescriptions
				= pvads
			}
