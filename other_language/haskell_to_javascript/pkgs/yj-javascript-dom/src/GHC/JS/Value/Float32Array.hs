{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Float32Array where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

newtype F = F { unF :: JSVal }

instance Show F where
	show f = "(" ++ JS.Object.toString (JS.Object.toO f) ++ ")"

instance JS.Value.IsJSVal F where toJSVal (F v) = v
instance JS.Value.V F where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO F

new :: JS.Value.Some -> IO F
new (JS.Value.toJSVal -> o) = F <$> js_new o

foreign import javascript "((o) => { const r = new Float32Array(o); return r })"
	js_new :: JSVal -> IO JSVal
