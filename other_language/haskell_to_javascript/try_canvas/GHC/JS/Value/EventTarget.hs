{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.EventTarget (
	E, toValue, fromValue, toE, IsE, addEventListenerSimple ) where

import GHC.JS.Prim (JSVal, toJSString)
import GHC.JS.Foreign.Callback (Callback, syncCallback1, OnBlocked(..))
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.Event qualified as JS.Event
import Data.Typeable (cast)
import Data.Maybe (fromJust)

data E = forall et . JS.Value.V et => E et

instance JS.Value.IsJSVal E where toJSVal (E et) = JS.Value.toJSVal et
instance JS.Value.V E where toV = JS.Object.toValue; fromV = JS.Object.fromValue

toValue :: JS.Value.V et => et -> JS.Value.Some
toValue = JS.Value.toV . E

fromValue :: JS.Value.V et => JS.Value.Some -> Maybe et
fromValue v = JS.Value.fromV v >>= \(E et) -> cast et

toE :: IsE et => et -> E; toE = fromJust . JS.Value.cast
class JS.Object.IsO et => IsE et

addEventListenerSimple :: E -> String -> (JS.Event.E -> IO ()) -> IO ()
addEventListenerSimple etg etp ((. JS.Event.E . JS.Event.OtherE) -> lsn) =
	js_addEventListenerSimple (JS.Value.toJSVal etg) (toJSString etp)
		=<< syncCallback1 ThrowWouldBlock lsn

foreign import javascript
	"((etg, etp, lsn) => { etg.addEventListener(etp, lsn); })"
	js_addEventListenerSimple ::
		JSVal -> JSVal -> Callback (JSVal -> IO ()) -> IO ()
