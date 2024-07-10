{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Window (
	W, w, document, setInterval, getInnerWidth, getInnerHeight ) where

import GHC.JS.Prim (JSVal)
import GHC.JS.Foreign.Callback (Callback, syncCallback, OnBlocked(..))
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Document qualified as JS.Document

newtype W = W JSVal

instance JS.Value.IsJSVal W where toJSVal (W v) = v

instance JS.Value.V W where
	toV = JS.EventTarget.toValue; fromV = JS.EventTarget.fromValue

instance JS.Object.IsO W
instance JS.EventTarget.IsE W

w :: W; w = W js_w

foreign import javascript "(() => { return window; })" js_w :: JSVal

document :: W -> JS.Document.D; document (W wn) = JS.Document.D $ js_document wn

foreign import javascript "((w) => { return w.document; })"
	js_document :: JSVal -> JSVal

setInterval :: W -> IO () -> Word -> IO ()
setInterval (W wn) f d =
	syncCallback ThrowWouldBlock f >>= \f' -> js_setInterval wn f' d

foreign import javascript "((w, f, d) => { w.setInterval(f, d); })"
	js_setInterval :: JSVal -> Callback (IO ()) -> Word -> IO ()

getInnerWidth, getInnerHeight :: W -> IO Double
getInnerWidth (W wn) = js_getInnerWidth wn
getInnerHeight (W wn) = js_getInnerHeight wn

foreign import javascript "((w) => { return w.innerWidth; })"
	js_getInnerWidth :: JSVal -> IO Double

foreign import javascript "((w) => { return w.innerHeight; })"
	js_getInnerHeight :: JSVal -> IO Double
