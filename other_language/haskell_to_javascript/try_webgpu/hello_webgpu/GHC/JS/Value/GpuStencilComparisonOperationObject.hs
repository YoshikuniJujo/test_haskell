{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuStencilComparisonOperationObject where

import GHC.JS.Value.GpuDepthStencilCompare qualified as
	JS.GpuDepthStencilCompare

data G = G {
	compare :: JS.GpuDepthStencilCompare.G,
	depthFailOp :: StencilOperation,
	failOp :: StencilOperation,
	passOp :: StencilOperation }
	deriving Show

data StencilOperation
	= Zero | Keep | Replace | Invert
	| DecrementClamp | DecrementWrap | IncrementClamp | IncrementWrap
	deriving Show

stencilOperationToString :: StencilOperation -> String
stencilOperationToString = \case
	Zero -> "zero"; Keep -> "keep"; Replace -> "replace"; Invert -> "invert"
	DecrementClamp -> "decrement-clamp"; DecrementWrap -> "decrement-wrap"
	IncrementClamp -> "increment-clamp"; IncrementWrap -> "increment-wrap"
