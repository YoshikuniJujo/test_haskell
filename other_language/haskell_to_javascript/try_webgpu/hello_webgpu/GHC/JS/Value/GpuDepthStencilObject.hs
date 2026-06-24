{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuDepthStencilObject where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.GpuTextureFormat qualified as JS.GpuTextureFormat
import GHC.JS.Value.GpuStencilComparisonOperationObject qualified as
	JS.GpuStencilComparisonOperationObject

import GHC.JS.Value.GpuDepthStencilCompare qualified as
	JS.GpuDepthStencilCompare

data G = G {
	depthBias :: Int,
	depthBiasClamp :: Int,
	depthBiasSlopeScale :: Int,
	depthCompare :: JS.GpuDepthStencilCompare.G,
	depthWriteEnabled :: Bool,
	format :: JS.GpuTextureFormat.G
	}
	deriving Show
