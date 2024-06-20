{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Foldable
import GHC.JS.Prim
import Data.Word

main :: IO ()
main = do
	foo <- js_getElementById (toJSString "foo")
	js_setTextContent foo (toJSString "bar")
	Just canvas <- getCanvasById "canvas"
	ctx <- getContext2D canvas
	setFillStyle ctx $ Rgb 200 0 0
	fillRect ctx
		$ Rectangle { left = 10, top = 10, width = 50, height = 50 }
	setFillStyle ctx $ Rgba 0 0 200 0.5
	fillRect ctx
		$ Rectangle { left = 20, top = 20, width = 50, height = 50 }
	setFillStyle ctx $ Rgb 0 0 0
	fillRect ctx
		$ Rectangle { left = 125, top = 25, width = 100, height = 100 }
	clearRect ctx
		$ Rectangle { left = 145, top = 45, width = 60, height = 60 }
	strokeRect ctx
		$ Rectangle { left = 150, top = 50, width = 50, height = 50 }
	beginPath ctx
	moveTo ctx 275 50
	lineTo ctx 300 75
	lineTo ctx 300 25
	fill ctx

	beginPath ctx
	arc ctx 425 75 50 0 (pi * 2) True
	moveTo ctx 460 75
	arc ctx 425 75 35 0 pi False
	moveTo ctx 415 65
	arc ctx 410 65 5 0 (pi * 2) True
	moveTo ctx 445 65
	arc ctx 440 65 5 0 (pi * 2) True
	stroke ctx

	beginPath ctx
	moveTo ctx 25 125
	lineTo ctx 105 125
	lineTo ctx 25 205
	fill ctx

	beginPath ctx
	moveTo ctx 125 225
	lineTo ctx 125 145
	lineTo ctx 45 225
	closePath ctx
	stroke ctx

	for_ [0 :: Int .. 3] \i@(fromIntegral -> i') ->
		for_ [0 :: Double .. 2] \j -> do
			beginPath ctx
			arc ctx (25 + j * 50) (275 + i' * 50) 20
				0 (pi + pi * j / 2) (i `mod` 2 /= 0)
			if (i > 1) then fill ctx else stroke ctx

	beginPath ctx
	moveTo ctx 225 175
	quadraticCurveTo ctx 175 175 175 212.5
	quadraticCurveTo ctx 175 250 200 250
	quadraticCurveTo ctx 200 270 180 275
	quadraticCurveTo ctx 210 270 215 250
	quadraticCurveTo ctx 275 250 275 212.5
	quadraticCurveTo ctx 275 175 225 175
	stroke ctx

	beginPath ctx
	moveTo ctx 375 190
	bezierCurveTo ctx 375 187 370 175 350 175
	bezierCurveTo ctx 320 175 320 212.5 320 212.5
	bezierCurveTo ctx 320 230 340 252 375 270
	bezierCurveTo ctx 410 252 430 230 430 212.5
	bezierCurveTo ctx 430 212.5 430 175 400 175
	bezierCurveTo ctx 385 175 375 187 375 190
	fill ctx

	translate ctx 200 300
	draw ctx

getCanvasById :: String -> IO (Maybe Canvas)
getCanvasById i = do
	e <- js_getElementById $ toJSString i
	pure if js_isCanvas e then Just $ Canvas e else Nothing

data Canvas = Canvas JSVal

foreign import javascript "((id) => { return document.getElementById(id); })"
	js_getElementById :: JSVal -> IO (JSVal)

foreign import javascript "((e) => { return e.getContext; })"
	js_isCanvas :: JSVal -> Bool

getContext2D :: Canvas -> IO Context2D
getContext2D (Canvas c) = Context2D <$> js_getContext2d c

data Context2D = Context2D JSVal

foreign import javascript "((c) => { return c.getContext('2d'); })"
	js_getContext2d :: JSVal -> IO (JSVal)

setFillStyle :: Context2D -> Color -> IO ()
setFillStyle (Context2D ctx) = js_setFillStyle ctx . colorToJSVal

foreign import javascript "((ctx, clr) => { ctx.fillStyle = clr; })"
	js_setFillStyle :: JSVal -> JSVal -> IO ()

fillRect, strokeRect, clearRect :: Context2D -> Rectangle -> IO ()
fillRect = rectFromJs js_fillRect
strokeRect = rectFromJs js_strokeRect
clearRect = rectFromJs js_clearRect

rectFromJs ::
	(JSVal -> Double -> Double -> Double -> Double -> IO ()) ->
	Context2D -> Rectangle -> IO ()
rectFromJs a (Context2D ctx)
	Rectangle { left = l, top = t, width = w, height = h } =
	a ctx l t w h

foreign import javascript "((ctx, l, t, w, h) => { ctx.fillRect(l, t, w, h); })"
	js_fillRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript "((ctx, l, t, w, h) => { ctx.strokeRect(l, t, w, h); })"
	js_strokeRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript "((ctx, l, t, w, h) => { ctx.clearRect(l, t, w, h); })"
	js_clearRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

beginPath, closePath, fill, stroke :: Context2D -> IO ()
beginPath (Context2D c) = js_beginPath c
closePath (Context2D c) = js_closePath c
fill (Context2D c) = js_fill c
stroke (Context2D c) = js_stroke c

foreign import javascript "((ctx) => { ctx.beginPath(); })"
	js_beginPath :: JSVal -> IO ()

foreign import javascript "((ctx) => { ctx.closePath(); })"
	js_closePath :: JSVal -> IO ()

foreign import javascript "((ctx) => { ctx.fill(); })"
	js_fill :: JSVal -> IO ()

foreign import javascript "((ctx) => { ctx.stroke(); })"
	js_stroke :: JSVal -> IO ()

moveTo, lineTo :: Context2D -> Double -> Double -> IO ()
moveTo (Context2D c) = js_moveTo c
lineTo (Context2D c) = js_lineTo c

foreign import javascript "((ctx, x, y) => { ctx.moveTo(x, y); })"
	js_moveTo :: JSVal -> Double -> Double -> IO ()

foreign import javascript "((ctx, x, y) => { ctx.lineTo(x, y); })"
	js_lineTo :: JSVal -> Double -> Double -> IO ()

arc :: Context2D -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
arc (Context2D ctx) = js_arc ctx

foreign import javascript
	"((ctx, x, y, r, sa, ea, cc) => { ctx.arc(x, y, r, sa, ea, cc); })"
	js_arc ::
		JSVal -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()

arcTo :: Context2D -> Double -> Double -> Double -> Double -> Double -> IO ()
arcTo (Context2D ctx) = js_arcTo ctx

foreign import javascript
	"((ctx, x1, y1, x2, y2, r) => { ctx.arcTo(x1, y1, x2, y2, r); })"
	js_arcTo :: JSVal -> Double -> Double -> Double -> Double -> Double -> IO ()

quadraticCurveTo :: Context2D -> Double -> Double -> Double -> Double -> IO ()
quadraticCurveTo (Context2D ctx) = js_quadraticCurveTo ctx

foreign import javascript
	"((ctx, cp1x, cp1y, x, y) => { ctx.quadraticCurveTo(cp1x, cp1y, x, y); })"
	js_quadraticCurveTo ::
		JSVal -> Double -> Double -> Double -> Double -> IO ()

bezierCurveTo :: Context2D ->
	Double -> Double -> Double -> Double -> Double -> Double -> IO ()
bezierCurveTo (Context2D ctx) = js_bezierCurveTo ctx

foreign import javascript
	"((ctx, cp1x, cp1y, cp2x, cp2y, x, y) => { ctx.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y); })"
	js_bezierCurveTo ::
		JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()

translate :: Context2D -> Double -> Double -> IO ()
translate (Context2D ctx) = js_translate ctx

foreign import javascript "((ctx, x, y) => { ctx.translate(x, y); })"
	js_translate :: JSVal -> Double -> Double -> IO ()

foreign import javascript "((e, t) => { e.textContent = t; })"
	js_setTextContent :: JSVal -> JSVal -> IO ()

data Color
	= Rgb Word8 Word8 Word8
	| Rgba Word8 Word8 Word8 Double
	| Black | White
	deriving Show

colorToJSVal :: Color -> JSVal
colorToJSVal = toJSString . colorToString

colorToString :: Color -> String
colorToString (Rgb r g b) =
	"rgb(" ++ show r ++ ", " ++ show g ++ ", "  ++ show b ++ ")"
colorToString (Rgba r g b a) =
	"rgba(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ", " ++
	show a ++ ")"
colorToString Black = "black"
colorToString White = "white"

data Rectangle =
	Rectangle { left :: Double, top :: Double, width :: Double, height :: Double }
	deriving Show

draw :: Context2D -> IO ()
draw ctx = do

	roundedRect ctx 12 12 150 150 15
	roundedRect ctx 19 19 150 150 9
	roundedRect ctx 53 53 49 33 10
	roundedRect ctx 53 119 49 16 6
	roundedRect ctx 135 53 49 33 10
	roundedRect ctx 135 119 25 49 10

	beginPath ctx
	arc ctx 37 37 13 (pi / 7) (- pi / 7) False
	lineTo ctx 31 37
	fill ctx

	for_ [0 .. 7] \i -> fillRect ctx Rectangle {
		left = 51 + i * 16, top = 35, width = 4, height = 4 }

	for_ [0 .. 5] \i -> fillRect ctx Rectangle {
		left = 115, top = 51 + i * 16, width = 4, height = 4 }

	for_ [0 .. 7] \i -> fillRect ctx Rectangle {
		left = 51 + i * 16, top = 99, width = 4, height = 4 }

	beginPath ctx
	moveTo ctx 83 116
	lineTo ctx 83 102
	bezierCurveTo ctx 83 94 89 88 97 88
	bezierCurveTo ctx 105 88 111 94 111 102
	lineTo ctx 111 116
	lineTo ctx 106.333 111.333
	lineTo ctx 101.666 116
	lineTo ctx 97 111.333
	lineTo ctx 92.333 116
	lineTo ctx 87.666 111.333
	lineTo ctx 83 116
	fill ctx

	setFillStyle ctx White
	beginPath ctx
	moveTo ctx 91 96
	bezierCurveTo ctx 88 96 87 99 87 101
	bezierCurveTo ctx 87 103 88 106 91 106
	bezierCurveTo ctx 94 106 95 103 95 101
	bezierCurveTo ctx 95 99 94 96 91 96
	moveTo ctx 103 96
	bezierCurveTo ctx 100 96 99 99 99 101
	bezierCurveTo ctx 99 103 100 106 103 106
	bezierCurveTo ctx 106 106 107 103 107 101
	bezierCurveTo ctx 107 99 106 96 103 96
	fill ctx

	setFillStyle ctx Black
	beginPath ctx
	arc ctx 101 102 2 0 (pi * 2) True
	fill ctx
	beginPath ctx
	arc ctx 89 102 2 0 (pi * 2) True
	fill ctx

roundedRect ::
	Context2D -> Double -> Double -> Double -> Double -> Double -> IO ()
roundedRect ctx x y w h rd = do
	beginPath ctx
	moveTo ctx x (y + rd)
	arcTo ctx x (y + h) (x + rd) (y + h) rd
	arcTo ctx (x + w) (y + h) (x + w) (y + h - rd) rd
	arcTo ctx (x + w) y (x + w - rd) y rd
	arcTo ctx x y x (y + rd) rd
	stroke ctx
