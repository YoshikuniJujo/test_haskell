{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuBlendState where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuBlendComponent qualified as JS.GpuBlendComponent

import Control.Monad.ST

data G = G { color :: JS.GpuBlendComponent.G, alpha :: JS.GpuBlendComponent.G }
	deriving Show

toObject :: G -> ST s (JS.Object.ST s)
toObject g = JS.Object.new >>= \o -> o <$ do
	JS.Object.set o "color" $ color g
	JS.Object.set o "alpha" $ alpha g

instance JS.Value.IsJSVal G where
	toJSVal g = JS.Value.toJSVal $ runST $ JS.Object.freeze =<< toObject g

instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue
