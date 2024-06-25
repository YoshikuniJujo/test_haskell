{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Window where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import Data.Maybe

newtype W = W JSVal

instance JS.Value.IsJSVal W where toJSVal (W v) = v

instance JS.Value.V W where
	toV = JS.EventTarget.toV
	fromV = JS.EventTarget.fromV

instance JS.EventTarget.IsE W where
	toE = fromJust . JS.Value.fromV . JS.Value.toV

w :: W
w = W js_w

foreign import javascript "(() => { return window; })" js_w :: JSVal

getInnerHeight, getInnerWidth :: W -> IO Double
getInnerHeight (W w) = js_getInnerHeight w
getInnerWidth (W w) = js_getInnerWidth w

foreign import javascript "((w) => { return w.innerHeight; })"
	js_getInnerHeight :: JSVal -> IO Double

foreign import javascript "((w) => { return w.innerWidth; })"
	js_getInnerWidth :: JSVal -> IO Double
