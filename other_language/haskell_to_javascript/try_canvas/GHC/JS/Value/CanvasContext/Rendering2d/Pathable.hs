{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.CanvasContext.Rendering2d.Pathable where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value

newtype P = P JSVal

toP :: IsP p => p -> P
toP = P . JS.Value.toJSVal

class JS.Value.IsJSVal p => IsP p

rect :: P -> Double -> Double -> Double -> Double -> IO ()
rect (P p) = js_rect p

foreign import javascript "((p, x, y, w, h) => { p.rect(x, y, w, h); })"
	js_rect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

arc :: P -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
arc (P p) = js_arc p

foreign import javascript
	"((p, x, y, r, sa, ea, cc) => { p.arc(x, y, r, sa, ea, cc); })"
	js_arc :: JSVal ->
		Double -> Double -> Double -> Double -> Double -> Bool -> IO ()

arcTo :: P -> Double -> Double -> Double -> Double -> Double -> IO ()
arcTo (P p) = js_arcTo p

foreign import javascript
	"((p, x1, y1, x2, y2, r) => { p.arcTo(x1, y1, x2, y2, r); })"
	js_arcTo ::
		JSVal -> Double -> Double -> Double -> Double -> Double -> IO ()

moveTo, lineTo :: P -> Double -> Double -> IO ()
moveTo (P p) = js_moveTo p
lineTo (P p) = js_lineTo p

foreign import javascript "((p, x, y) => { p.moveTo(x, y); })"
	js_moveTo :: JSVal -> Double -> Double -> IO ()

foreign import javascript "((p, x, y) => { p.lineTo(x, y); })"
	js_lineTo :: JSVal -> Double -> Double -> IO ()

closePath :: P -> IO ()
closePath (P p) = js_closePath p

foreign import javascript "((p) => { p.closePath(); })"
	js_closePath :: JSVal -> IO ()

quadraticCurveTo :: P -> Double -> Double -> Double -> Double -> IO ()
quadraticCurveTo (P p) = js_quadraticCurveTo p

foreign import javascript
	"((p, cp1x, cp1y, x, y) => { p.quadraticCurveTo(cp1x, cp1y, x, y); })"
	js_quadraticCurveTo ::
		JSVal -> Double -> Double -> Double -> Double -> IO ()

bezierCurveTo :: P ->
	Double -> Double -> Double -> Double -> Double -> Double -> IO ()
bezierCurveTo (P p) = js_bezierCurveTo p

foreign import javascript
	"((p, cp1x, cp1y, cp2x, cp2y, x, y) => { p.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y); })"
	js_bezierCurveTo ::
		JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
