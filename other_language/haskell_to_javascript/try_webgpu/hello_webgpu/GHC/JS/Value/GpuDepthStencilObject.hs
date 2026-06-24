{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuDepthStencilObject where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.GpuTextureFormat qualified as JS.GpuTextureFormat

data G = G {
	depthBias :: Int,
	depthBiasClamp :: Int,
	depthBiasSlopeScale :: Int,
	depthCompare :: DepthCompare,
	depthWriteEnabled :: Bool,
	format :: JS.GpuTextureFormat.G
	}
	deriving Show

data DepthCompare
	= Never | Always | Equal | NotEqual
	| Less | LessEqual | Greater | GreaterEqual
	deriving Show

depthCompareToString :: DepthCompare -> String
depthCompareToString = \case
	Never -> "never"; Always -> "always"
	Equal -> "equal"; NotEqual -> "not-equal"
	Less -> "less"; LessEqual -> "less-equal"
	Greater -> "greater"; GreaterEqual -> "greater-equal"

instance JS.Value.IsJSVal DepthCompare where
	toJSVal = JS.Value.toJSVal . depthCompareToString

instance JS.Value.V DepthCompare
