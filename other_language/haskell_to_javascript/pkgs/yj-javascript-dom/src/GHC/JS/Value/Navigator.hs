{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Navigator where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

newtype N = N { unN :: JSVal }

instance Show N where
	show n' = "(" ++ JS.Object.toString (JS.Object.toO n') ++ ")"

instance JS.Value.IsJSVal N where toJSVal (N v) = v
instance JS.Value.V N where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO N

n :: N; n = N js_n

foreign import javascript "(() => { return navigator; })" js_n :: JSVal
