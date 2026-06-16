{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Navigator.Webgpu where

import GHC.JS.Prim (JSVal, isNull)
import GHC.JS.Value.Navigator qualified as JS.Navigator
import GHC.JS.Value.Gpu qualified as JS.Gpu

gpu :: JS.Navigator.N -> Maybe JS.Gpu.G
gpu (JS.Navigator.N n) =
	let g = js_gpu n in if isNull g then Nothing else Just $ JS.Gpu.G g

foreign import javascript "((n) => { return n.gpu; })"
	js_gpu :: JSVal -> JSVal
