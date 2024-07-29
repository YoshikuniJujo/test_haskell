{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.CanvasContext.Rendering2d (

	R,

	setFillStyle,

	clearRect, fillRect, strokeRect, fillText,
	beginPath, fill, nonzero, evenodd, stroke, translate, save, restore

	) where

import GHC.JS.Prim (JSVal, toJSString)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.CanvasContext qualified as JS.CanvasContext
import GHC.JS.Value.CanvasContext.Rendering2d.Path qualified as Path
import GHC.JS.Value.CanvasContext.Rendering2d.Pathable qualified as Pathable
import Data.Color (
	Rgb, rgbToString, Rgba, rgbaToString, ColorName, colorNameToString )

newtype R = R { unR :: JSVal }

instance JS.Value.IsJSVal R where toJSVal (R v) = v

instance JS.Value.V R where
	toV = JS.CanvasContext.toValue; fromV = JS.CanvasContext.fromValue

instance JS.Object.IsO R

instance JS.CanvasContext.IsC R where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` rClass
	downMake = R

instance Pathable.IsP R

rClass :: JS.Object.Class
rClass = JS.Object.Class js_CanvasRenderingContext2D

foreign import javascript "(() => { return CanvasRenderingContext2D; })"
	js_CanvasRenderingContext2D :: JSVal

---------------------------------------------------------------------------
--- INSTANCE PROPERTY                                                   ---
---------------------------------------------------------------------------

-- CanvasRenderingContext2D.fillStyle

setFillStyle :: IsFillStyle fs => R -> fs -> IO ()
setFillStyle (R ctx) = js_setFillStyle ctx . toJSString . fillStyleToString

foreign import javascript "((ctx, fs) => { ctx.fillStyle = fs; })"
	js_setFillStyle :: JSVal -> JSVal -> IO ()

class IsFillStyle fs where fillStyleToString :: fs -> String

instance IsFillStyle Rgb where fillStyleToString = rgbToString
instance IsFillStyle Rgba where fillStyleToString = rgbaToString
instance IsFillStyle ColorName where fillStyleToString = colorNameToString

---------------------------------------------------------------------------
--- INSTANCE METHOD                                                     ---
---------------------------------------------------------------------------

-- CanvasRenderingContext2D.clearRect()
-- CanvasRenderingContext2D.fillRect()
-- CanvasRenderingContext2D.strokeRect()

clearRect, fillRect, strokeRect ::
	R -> Double -> Double -> Double -> Double -> IO ()
clearRect = js_clearRect . unR; fillRect = js_fillRect . unR
strokeRect = js_strokeRect . unR

foreign import javascript
	"((ctx, l, t, w, h) => { ctx.clearRect(l, t, w, h); })"
	js_clearRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript "((ctx, l, t, w, h) => { ctx.fillRect(l, t, w, h); })"
	js_fillRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript
	"((ctx, l, t, w, h) => { ctx.strokeRect(l, t, w, h); })"
	js_strokeRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

-- CanvasRenderingContext2D.fillText()

fillText :: R -> String -> Double -> Double -> IO ()
fillText (R cxt) = js_fillText cxt . toJSString

foreign import javascript "((ctx, , str, x, y) => { ctx.fillText(str, x, y); })"
	js_fillText :: JSVal -> JSVal -> Double -> Double -> IO ()

-- CanvasRenderingContext2D.beginPath()

beginPath :: R -> IO ()
beginPath (R ctx) = js_beginPath ctx

foreign import javascript "((ctx) => { ctx.beginPath(); })"
	js_beginPath :: JSVal -> IO ()

-- CanvasRenderingContext2D.fill()

fill :: R -> Maybe Path.P -> FillRule -> IO ()
fill (R cxt) = maybe
	(js_fill cxt . unFillRule)
	((. unFillRule) . js_fill_path cxt . Path.unP)

foreign import javascript "((ctx, fr) => { ctx.fill(fr); })"
	js_fill :: JSVal -> JSVal -> IO ()

foreign import javascript "((ctx, p, fr) => { ctx.fill(p, fr); })"
	js_fill_path :: JSVal -> JSVal -> JSVal -> IO ()

newtype FillRule = FillRule { unFillRule :: JSVal }

nonzero, evenodd :: FillRule
nonzero = FillRule js_nonzero
evenodd = FillRule js_evenodd

foreign import javascript "(() => { return 'nonzero'; })" js_nonzero :: JSVal
foreign import javascript "(() => { return 'evenodd'; })" js_evenodd :: JSVal

-- CanvasRenderingContext2D.stroke()

stroke :: R -> Maybe Path.P -> IO ()
stroke (R ctx) = maybe (js_stroke ctx) (js_stroke_path ctx . Path.unP)

foreign import javascript "((ctx) => { ctx.stroke(); })"
	js_stroke :: JSVal -> IO ()

foreign import javascript "((ctx, p) => { ctx.stroke(p); })"
	js_stroke_path :: JSVal -> JSVal -> IO ()

-- CanvasRenderingContext2D.translate()

translate :: R -> Double -> Double -> IO ()
translate = js_translate . unR

foreign import javascript "((ctx, x, y) => { ctx.translate(x, y); })"
	js_translate :: JSVal -> Double -> Double -> IO ()

-- CanvasRenderingContext2D.save()
-- CanvasRenderingContext2D.restore()

save, restore :: R -> IO ()
save = js_save . unR; restore = js_restore . unR

foreign import javascript "((ctx) => { ctx.save(); })" js_save :: JSVal -> IO ()

foreign import javascript "((ctx) => { ctx.restore(); })"
	js_restore :: JSVal -> IO ()
