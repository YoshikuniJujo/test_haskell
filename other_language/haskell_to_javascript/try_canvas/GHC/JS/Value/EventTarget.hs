{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.EventTarget where

import Data.Typeable

import GHC.JS.Prim
import GHC.JS.Foreign.Callback
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import Data.Maybe

data E = forall et . JS.Value.V et => E et

instance JS.Value.IsJSVal E where toJSVal (E et) = JS.Value.toJSVal et
instance JS.Value.V E where toV = JS.Object.toV; fromV = JS.Object.fromV

toV :: JS.Value.V et => et -> JS.Value.Some
toV = JS.Value.toV . E

fromV :: JS.Value.V et => JS.Value.Some -> Maybe et
fromV v = do
	E et <- JS.Value.fromV v
	cast et

class JS.Object.IsO et => IsE et where
	toE :: et -> E; toE = fromJust . JS.Value.cast

addEventListenerSimple :: E -> String -> (JSVal -> IO ()) -> IO ()
addEventListenerSimple etg etp lsn = do
	lsn' <- syncCallback1 ThrowWouldBlock lsn
	js_addEventListenerSimple (JS.Value.toJSVal etg) (toJSString etp) lsn'

foreign import javascript
	"((etg, etp, lsn) => { etg.addEventListener(etp, lsn); })"
	js_addEventListenerSimple ::
		JSVal -> JSVal -> Callback (JSVal -> IO ()) -> IO ()
