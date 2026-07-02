{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuBlendState where

import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.GpuBlendComponent qualified as JS.GpuBlendComponent

import Control.Monad.ST

data G = G { color :: JS.GpuBlendComponent.G, alpha :: JS.GpuBlendComponent.G }
	deriving Show

toObject :: G -> ST s (JS.Object.ST s)
toObject g = JS.Object.new >>= \o -> o <$ do
	JS.Object.set o "color" $ color g
	JS.Object.set o "alpha" $ alpha g
