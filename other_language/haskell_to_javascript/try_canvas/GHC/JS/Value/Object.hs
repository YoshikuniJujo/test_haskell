{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Object (
	O, toValue, fromValue, IsO, toO,
	isInstanceOf, Class(..), toString ) where

import GHC.JS.Prim (JSVal, fromJSString)
import GHC.JS.Value qualified as JS.Value
import Data.Typeable (cast)
import Data.Maybe (fromJust)

data O = forall o . JS.Value.V o => O o

instance JS.Value.IsJSVal O where toJSVal (O o) = JS.Value.toJSVal o
instance JS.Value.V O

toValue :: JS.Value.V o => o -> JS.Value.Some
toValue = JS.Value.toV . O

fromValue :: JS.Value.V o => JS.Value.Some -> Maybe o
fromValue v = JS.Value.fromV v >>= \(O o) -> cast o

toO :: IsO o => o -> O
toO = fromJust . JS.Value.cast

class JS.Value.V o => IsO o

isInstanceOf :: O -> Class -> Bool
o `isInstanceOf` Class c = JS.Value.toJSVal o `js_instanceof` c

foreign import javascript "((o, c) => { return (o instanceof c); })"
	js_instanceof :: JSVal -> JSVal -> Bool

newtype Class = Class JSVal

toString :: O -> String
toString obj = fromJSString . js_toString $ JS.Value.toJSVal obj

foreign import javascript "((o) => { return o.toString(); })"
	js_toString :: JSVal -> JSVal
