{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Float32Array where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.Array qualified as JS.Array

newtype F = F { unF :: JSVal }

instance Show F where
	show f = "(" ++ JS.Object.toString (JS.Object.toO f) ++ ")"

instance JS.Value.IsJSVal F where toJSVal (F v) = v
instance JS.Value.V F where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO F

new :: JS.Value.Some -> IO F
new (JS.Value.toJSVal -> o) = F <$> js_new o

fromListIO :: JS.Value.V a => [a] -> IO F
fromListIO xs = F <$> do
	JS.Array.A a <- JS.Array.fromListIO xs
	js_new a

fromFloatList :: [Float] -> IO F
fromFloatList fs = F <$> do
	JS.Array.A a <- JS.Array.fromFloatList fs
	js_new a

foreign import javascript "((o) => { const r = new Float32Array(o); return r })"
	js_new :: JSVal -> IO JSVal

byteLength :: F -> Int
byteLength = js_byteLength . unF

foreign import javascript "((f) => { return f.byteLength })"
	js_byteLength :: JSVal -> Int

length :: F -> Int
length = js_length . unF

foreign import javascript "((f) => { return f.length })"
	js_length :: JSVal -> Int
