{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.CanvasContext.Gpu where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.CanvasContext qualified as JS.CanvasContext

newtype G = G { unG :: JSVal }

instance Show G where
	show g = "(" ++ JS.Object.toString (JS.Object.toO g) ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G v) = v

instance JS.Value.V G where
	toV = JS.CanvasContext.toValue; fromV = JS.CanvasContext.fromValue

instance JS.Object.IsO G

instance JS.CanvasContext.IsC G where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` rClass
	downMake = G

rClass :: JS.Object.Class
rClass = JS.Object.Class js_GpuCanvasContext

foreign import javascript "(() => { return GPUCanvasContext; })"
	js_GpuCanvasContext :: JSVal

configure :: G -> JS.Object.O -> IO ()
configure (G g) (JS.Value.toJSVal -> as) = js_configure g as

foreign import javascript "((g, as) => { g.configure(as); })"
	js_configure :: JSVal -> JSVal -> IO ()
