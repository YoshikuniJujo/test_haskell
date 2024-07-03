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
