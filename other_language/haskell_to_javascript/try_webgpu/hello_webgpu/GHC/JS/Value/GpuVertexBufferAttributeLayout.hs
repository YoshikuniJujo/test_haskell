{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuVertexBufferAttributeLayout where

-- import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuVertexFormat qualified as JS.GpuVertexFormat

data G = G {
	format :: JS.GpuVertexFormat.G,
	offset :: Int,
	shaderLocation :: Int }
	deriving Show

toObject :: G -> IO JS.Object.O
toObject g = do
	o <- JS.Object.new
	JS.Object.set o "format" $ format g
	JS.Object.set o "offset" $ offset g
	JS.Object.set o "shaderLocation" $ shaderLocation g
	pure o
