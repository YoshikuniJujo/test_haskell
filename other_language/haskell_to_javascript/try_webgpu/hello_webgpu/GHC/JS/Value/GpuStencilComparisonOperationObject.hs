{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuStencilComparisonOperationObject where

import GHC.JS.Value.GpuDepthStencilCompare qualified as
	JS.GpuDepthStencilCompare

data G = G {
	compare :: JS.GpuDepthStencilCompare.G,
	depthFailOp :: StencilOperation
	}

data StencilOperation
	= Zero | Keep | Replace | Invert
	| DecrementClamp | DecrementWrap | IncreamentClamp | IncrementWrap
	deriving Show
