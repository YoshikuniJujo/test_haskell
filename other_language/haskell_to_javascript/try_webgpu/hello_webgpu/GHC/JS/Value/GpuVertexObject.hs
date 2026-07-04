{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuVertexObject where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuOverridableConstant qualified as JS.GpuOverridableConstant
import GHC.JS.Value.GpuShaderModule qualified as JS.GpuShaderModule
import GHC.JS.Value.GpuVertexBufferLayout qualified as JS.GpuVertexBufferLayout

import Control.Monad.ST

data G = G {
	constants :: Maybe JS.GpuOverridableConstant.G,
	entryPoint :: Maybe String,
	gModule :: JS.GpuShaderModule.G,
	buffers :: [JS.GpuVertexBufferLayout.G] }
	deriving Show

toObject :: G -> ST s (JS.Object.ST s)
toObject g = do
	o <- JS.Object.new
	maybe (pure ()) (JS.Object.set o "constants") $ constants g
	maybe (pure ()) (JS.Object.set o "entryPoint") $ entryPoint g
	JS.Object.set o "module" $ gModule g
	JS.Object.set o "buffers" $ buffers g
	pure o

instance JS.Value.IsJSVal G where
	toJSVal g = runST $ JS.Value.toJSVal <$> toObject g

instance JS.Value.V G where
	toV = JS.Object.toValue; fromV = JS.Object.fromValue
