{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Object where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import Data.Typeable
import Data.Maybe

data O = forall o . JS.Value.V o => O { unO :: o }

instance JS.Value.IsJSVal O where toJSVal (O o) = JS.Value.toJSVal o

instance JS.Value.V O

toV :: JS.Value.V o => o -> JS.Value.Some
toV = JS.Value.toV . O

fromV :: JS.Value.V o => JS.Value.Some -> Maybe o
fromV v = do
	O o <- JS.Value.fromV v
	cast o

class JS.Value.V o => IsO o where toO :: o -> O; toO = fromJust . JS.Value.cast

newtype Class = Class JSVal

isInstanceOf :: O -> Class -> Bool
o `isInstanceOf` Class c = JS.Value.toJSVal o `js_instanceof` c

foreign import javascript "((o, c) => { return (o instanceof c); })"
	js_instanceof :: JSVal -> JSVal -> Bool

toString :: O -> String
toString obj = fromJSString . js_toString $ JS.Value.toJSVal obj

foreign import javascript "((o) => { return o.toString(); })"
	js_toString :: JSVal -> JSVal
