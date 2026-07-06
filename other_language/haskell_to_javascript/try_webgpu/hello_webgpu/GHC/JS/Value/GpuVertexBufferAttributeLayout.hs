{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuVertexBufferAttributeLayout where

import Control.Monad.ST

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuVertexFormat qualified as JS.GpuVertexFormat

data G = G {
	format :: JS.GpuVertexFormat.G,
	offset :: Int,
	shaderLocation :: Int }
	deriving Show

toObject :: (Monad m, JS.Object.M o m) => G -> m o
toObject g = do
	o <- JS.Object.new
	JS.Object.set o "format" $ format g
	JS.Object.set o "offset" $ offset g
	JS.Object.set o "shaderLocation" $ shaderLocation g
	pure o

instance JS.Value.IsJSVal G where
	toJSVal g = JS.Value.toJSVal $ runST $ JS.Object.freeze =<< toO
		where toO :: forall s . ST s (JS.Object.ST s); toO = toObject g

instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue
