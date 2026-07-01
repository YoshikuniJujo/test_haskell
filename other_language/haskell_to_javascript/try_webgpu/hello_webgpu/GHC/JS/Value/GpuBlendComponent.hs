{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuBlendComponent where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

import Control.Monad.ST

data G = G {
	operation :: BlendOperation,
	srcFactor :: BlendFactor,
	dstFactor :: BlendFactor }
	deriving Show

def :: G
def = G {
	operation = Add,
	srcFactor = One,
	dstFactor = Zero }

data BlendOperation
	= Add | Subtract | ReverseSubtract | Min | Max
	deriving Show

blendOperationToString :: BlendOperation -> String
blendOperationToString = \case
	Add -> "add"
	Subtract -> "subtract"; ReverseSubtract -> "reverse-subtract"
	Min -> "min"; Max -> "max"

data BlendFactor
	= Zero | One | Constant | OneMinusConstant
	| Src | OneMinusSrc | SrcAlpha | OneMinusSrcAlpha
	| Dst | OneMinusDst | DstAlpha | OneMinusDstAlpha
	| Src1 | OneMinusSrc1 | Src1Alpha | OneMinusSrc1Alpha
	| SrcAlphaSaturated
	deriving Show

blendFactorToString :: BlendFactor -> String
blendFactorToString = \case
	Zero -> "zero"; One -> "one"
	Constant -> "constant"; OneMinusConstant -> "one-minus-constant"
	Src -> "src"; OneMinusSrc -> "one-minus-src"
	SrcAlpha -> "src-alpha"; OneMinusSrcAlpha -> "one-minus-src-alpha"
	Dst -> "dst"; OneMinusDst -> "one-minus-dst"
	DstAlpha -> "dst-alpha"; OneMinusDstAlpha -> "one-minus-dst-alpha"
	Src1 -> "src1"; OneMinusSrc1 -> "one-minus-src1"
	Src1Alpha -> "src1-alpha"; OneMinusSrc1Alpha -> "one-minus-src1-alpha"
	SrcAlphaSaturated -> "src-alpha-saturated"

toObject :: G -> ST s (JS.Object.ST s)
toObject g = JS.Object.new >>= \o -> o <$ do
	JS.Object.set o "operation" . blendOperationToString $ operation g
	JS.Object.set o "srcFactor" . blendFactorToString $ srcFactor g
	JS.Object.set o "dstFactor" . blendFactorToString $ dstFactor g

instance JS.Value.IsJSVal G where
	toJSVal g = JS.Value.toJSVal $ runST $ JS.Object.freeze =<< toObject g

instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue
