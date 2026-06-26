{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuStencilComparisonOperationObject where

import Prelude hiding (compare)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

import GHC.JS.Value.GpuDepthStencilCompare qualified as
	JS.GpuDepthStencilCompare

data G = G {
	compare :: JS.GpuDepthStencilCompare.G,
	depthFailOp :: StencilOperation,
	failOp :: StencilOperation,
	passOp :: StencilOperation }
	deriving Show

toObject :: G -> IO JS.Object.O
toObject g = do
	o <- JS.Object.new
	JS.Object.set o "compare" $ compare g
	JS.Object.set o "depthFailOp" $ depthFailOp g
	JS.Object.set o "failOp" $ failOp g
	JS.Object.set o "passOp" $ passOp g
	pure o

data StencilOperation
	= Zero | Keep | Replace | Invert
	| DecrementClamp | DecrementWrap | IncrementClamp | IncrementWrap
	deriving Show

stencilOperationToString :: StencilOperation -> String
stencilOperationToString = \case
	Zero -> "zero"; Keep -> "keep"; Replace -> "replace"; Invert -> "invert"
	DecrementClamp -> "decrement-clamp"; DecrementWrap -> "decrement-wrap"
	IncrementClamp -> "increment-clamp"; IncrementWrap -> "increment-wrap"

instance JS.Value.IsJSVal StencilOperation where
	toJSVal = JS.Value.toJSVal . stencilOperationToString
instance JS.Value.V StencilOperation
