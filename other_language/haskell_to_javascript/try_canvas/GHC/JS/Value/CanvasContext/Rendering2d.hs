{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.CanvasContext.Rendering2d where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.CanvasContext qualified as JS.CanvasContext

newtype R = R JSVal

instance JS.Value.IsJSVal R where toJSVal (R v) = v

instance JS.Value.V R where
	toV = JS.CanvasContext.toV; fromV = JS.CanvasContext.fromV

instance JS.Object.IsO R

instance JS.CanvasContext.IsC R where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` rClass
	downMake = R

rClass :: JS.Object.Class
rClass = JS.Object.Class js_CanvasRenderingContext2D

foreign import javascript "(() => { return CanvasRenderingContext2D; })"
	js_CanvasRenderingContext2D :: JSVal

fillRect :: R -> Double -> Double -> Double -> Double -> IO ()
fillRect (R ctx) = js_fillRect ctx

foreign import javascript "((ctx, l, t, w, h) => { ctx.fillRect(l, t, w, h); })"
	js_fillRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
