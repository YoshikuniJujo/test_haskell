{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.CanvasContext.Rendering2d.Pathable (

	P, toP, IsP,

	closePath,
	moveTo, lineTo, bezierCurveTo, quadraticCurveTo, arc, arcTo, rect

	) where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value

newtype P = P { unP :: JSVal }

toP :: IsP p => p -> P
toP = P . JS.Value.toJSVal

class JS.Value.IsJSVal p => IsP p

-- closePath()

closePath :: P -> IO ()
closePath = js_closePath . unP

foreign import javascript "((p) => { p.closePath(); })"
	js_closePath :: JSVal -> IO ()

-- moveTo() and lineTo()

moveTo, lineTo :: P -> Double -> Double -> IO ()
moveTo = js_moveTo . unP; lineTo = js_lineTo . unP

foreign import javascript "((p, x, y) => { p.moveTo(x, y); })"
	js_moveTo :: JSVal -> Double -> Double -> IO ()

foreign import javascript "((p, x, y) => { p.lineTo(x, y); })"
	js_lineTo :: JSVal -> Double -> Double -> IO ()

-- bezeirCurveTo() and quadraticCurveTo()

bezierCurveTo :: P ->
	Double -> Double -> Double -> Double -> Double -> Double -> IO ()
bezierCurveTo = js_bezierCurveTo . unP

foreign import javascript
	"((p, cp1x, cp1y, cp2x, cp2y, x, y) => \
		\ { p.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y); })"
	js_bezierCurveTo :: JSVal ->
		Double -> Double -> Double -> Double -> Double -> Double ->
		IO ()

quadraticCurveTo :: P -> Double -> Double -> Double -> Double -> IO ()
quadraticCurveTo = js_quadraticCurveTo . unP

foreign import javascript
	"((p, cp1x, cp1y, x, y) => { p.quadraticCurveTo(cp1x, cp1y, x, y); })"
	js_quadraticCurveTo ::
		JSVal -> Double -> Double -> Double -> Double -> IO ()

-- arc() and arcTo()

arc :: P -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
arc = js_arc . unP

foreign import javascript
	"((p, x, y, r, sa, ea, cc) => { p.arc(x, y, r, sa, ea, cc); })"
	js_arc :: JSVal ->
		Double -> Double -> Double -> Double -> Double -> Bool -> IO ()

arcTo :: P -> Double -> Double -> Double -> Double -> Double -> IO ()
arcTo = js_arcTo . unP

foreign import javascript
	"((p, x1, y1, x2, y2, r) => { p.arcTo(x1, y1, x2, y2, r); })"
	js_arcTo ::
		JSVal -> Double -> Double -> Double -> Double -> Double -> IO ()

-- rect()

rect :: P -> Double -> Double -> Double -> Double -> IO ()
rect = js_rect . unP

foreign import javascript "((p, x, y, w, h) => { p.rect(x, y, w, h); })"
	js_rect :: JSVal -> Double -> Double -> Double -> Double -> IO ()
