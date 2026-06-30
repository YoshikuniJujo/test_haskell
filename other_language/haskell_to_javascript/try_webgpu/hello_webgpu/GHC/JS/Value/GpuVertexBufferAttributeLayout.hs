{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuVertexBufferAttributeLayout where

import System.IO.Unsafe

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuVertexFormat qualified as JS.GpuVertexFormat

data G = G {
	format :: JS.GpuVertexFormat.G,
	offset :: Int,
	shaderLocation :: Int }
	deriving Show

toObject :: G -> IO JS.Object.O
toObject g = do
	o <- JS.Object.new @JS.Object.IO
	JS.Object.set o "format" $ format g
	JS.Object.set o "offset" $ offset g
	JS.Object.set o "shaderLocation" $ shaderLocation g
	JS.Object.freeze o

instance JS.Value.IsJSVal G where
	toJSVal = unsafePerformIO . (JS.Value.toJSVal <$>) . toObject

instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue
