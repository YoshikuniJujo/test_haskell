{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Node where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import Data.Typeable
import Data.Maybe

data N = forall n . JS.Value.V n => N n

instance JS.Value.IsJSVal N where toJSVal (N n) = JS.Value.toJSVal n

instance JS.Value.V N where
	toV = JS.EventTarget.toV; fromV = JS.EventTarget.fromV

toV :: JS.Value.V n => n -> JS.Value.Some
toV = JS.Value.toV . N

fromV :: JS.Value.V n => JS.Value.Some -> Maybe n
fromV v = JS.Value.fromV v >>= \(N n) -> cast n

class JS.EventTarget.IsE n => IsN n where
	toN :: n -> N; toN = fromJust . JS.Value.cast

getNodeName :: N -> String
getNodeName nd = fromJSString . js_getNodeName $ JS.Value.toJSVal nd

foreign import javascript "((n) => { return n.nodeName; })"
	js_getNodeName :: JSVal -> JSVal
