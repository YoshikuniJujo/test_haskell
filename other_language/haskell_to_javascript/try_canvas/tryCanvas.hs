{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Foldable
import GHC.JS.Prim
import GHC.JS.Foreign.Callback
import Data.Word

main :: IO ()
main = do
	foo <- js_getElementById (toJSString "foo")
	clocktime <- js_getElementById (toJSString "clocktime")
	js_setTextContent foo (toJSString "bar")
	setInterval (do
		nows <- show <$> newDate
		js_setTextContent clocktime (toJSString nows)) 1000
	Just canvas <- getCanvasById "canvas"

	onPointerdown canvas \e -> do
		let	e' = pointerEventToClickEvent e
		js_setTextContent foo . toJSString
			$ show (offsetX e', offsetY e')

	ctx <- getContext2D canvas
	let	pth0 = context2DToPath2D ctx
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
	moveTo pth0 275 50
	lineTo pth0 300 75
	lineTo pth0 300 25
	fill ctx Nothing

	beginPath ctx
	arc pth0 425 75 50 0 (pi * 2) True
	moveTo pth0 460 75
	arc pth0 425 75 35 0 pi False
	moveTo pth0 415 65
	arc pth0 410 65 5 0 (pi * 2) True
	moveTo pth0 445 65
	arc pth0 440 65 5 0 (pi * 2) True
	stroke ctx Nothing

	beginPath ctx
	moveTo pth0 25 125
	lineTo pth0 105 125
	lineTo pth0 25 205
	fill ctx Nothing

	beginPath ctx
	moveTo pth0 125 225
	lineTo pth0 125 145
	lineTo pth0 45 225
	closePath ctx
	stroke ctx Nothing

	for_ [0 :: Int .. 3] \i@(fromIntegral -> i') ->
		for_ [0 :: Double .. 2] \j -> do
			beginPath ctx
			arc pth0 (25 + j * 50) (275 + i' * 50) 20
				0 (pi + pi * j / 2) (i `mod` 2 /= 0)
			if (i > 1) then fill ctx Nothing else stroke ctx Nothing

	beginPath ctx
	moveTo pth0 225 175
	quadraticCurveTo pth0 175 175 175 212.5
	quadraticCurveTo pth0 175 250 200 250
	quadraticCurveTo pth0 200 270 180 275
	quadraticCurveTo pth0 210 270 215 250
	quadraticCurveTo pth0 275 250 275 212.5
	quadraticCurveTo pth0 275 175 225 175
	stroke ctx Nothing

	beginPath ctx
	moveTo pth0 375 190
	bezierCurveTo pth0 375 187 370 175 350 175
	bezierCurveTo pth0 320 175 320 212.5 320 212.5
	bezierCurveTo pth0 320 230 340 252 375 270
	bezierCurveTo pth0 410 252 430 230 430 212.5
	bezierCurveTo pth0 430 212.5 430 175 400 175
	bezierCurveTo pth0 385 175 375 187 375 190
	fill ctx Nothing

	save ctx
	translate ctx 175 300
	draw ctx

	translate ctx 200 25
	triangle ctx

	rectangle <- newPath2D Path2DFromScratch
	let	rct = addablePath2DToPath2D rectangle
	rect rct 10 10 50 50
	circle <- newPath2D Path2DFromScratch
	let	ccl = addablePath2DToPath2D circle
	arc ccl 100 35 25 0 (2 * pi) False

	restore ctx
	translate ctx 0 480
	stroke ctx $ Just rct
	fill ctx $ Just ccl

	addPath rectangle circle
	translate ctx 150 0
	stroke ctx $ Just rct

	svgp <- newPath2D $ Path2DFromSvgPath "M10 10 h 80 v 80 h -80 Z"
	translate ctx 150 0
	stroke ctx . Just $ addablePath2DToPath2D svgp

-- END OF MAIN

getCanvasById :: String -> IO (Maybe Canvas)
getCanvasById i = do
	e <- js_getElementById $ toJSString i
	pure if js_isCanvas e then Just $ Canvas e else Nothing

data Canvas = Canvas { unCanvas :: JSVal }

foreign import javascript "((id) => { return document.getElementById(id); })"
	js_getElementById :: JSVal -> IO (JSVal)

foreign import javascript "((e) => { return e.getContext; })"
	js_isCanvas :: JSVal -> Bool

data Context2D = Context2D JSVal

getContext2D :: Canvas -> IO Context2D
getContext2D (Canvas c) = Context2D <$> js_getContext2d c

foreign import javascript "((c) => { return c.getContext('2d'); })"
	js_getContext2d :: JSVal -> IO JSVal

data AddablePath2D = AddablePath2D JSVal

data Path2D = Path2D { unPath2D :: JSVal }

context2DToPath2D :: Context2D -> Path2D
context2DToPath2D (Context2D ctx) = Path2D ctx

addablePath2DToPath2D :: AddablePath2D -> Path2D
addablePath2DToPath2D (AddablePath2D pth) = Path2D pth

newPath2D :: Path2DFrom -> IO AddablePath2D
newPath2D = \case
	Path2DFromScratch -> AddablePath2D <$> js_newPath2D
	Path2DFromPath2D (Path2D pth) -> AddablePath2D <$> js_newPath2DFrom pth
	Path2DFromSvgPath sp -> AddablePath2D <$> js_newPath2DFrom (toJSString sp)

data Path2DFrom
	= Path2DFromScratch
	| Path2DFromPath2D Path2D
	| Path2DFromSvgPath String

foreign import javascript "(() => { return new Path2D(); })"
	js_newPath2D :: IO JSVal

foreign import javascript "((sp) => { return new Path2D(sp); })"
	js_newPath2DFrom :: JSVal -> IO JSVal

addPath :: AddablePath2D -> AddablePath2D -> IO ()
addPath (AddablePath2D ps) (AddablePath2D pd) = js_addPath ps pd

foreign import javascript "((ps, pd) => { ps.addPath(pd); })"
	js_addPath :: JSVal -> JSVal -> IO ()

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

beginPath, closePath :: Context2D -> IO ()
beginPath (Context2D c) = js_beginPath c
closePath (Context2D c) = js_closePath c

fill, stroke :: Context2D -> Maybe Path2D -> IO ()
fill (Context2D c) = maybe (js_fill c) (js_fillWithPath2D c . unPath2D)
stroke (Context2D c) = maybe (js_stroke c) (js_strokeWithPath2D c . unPath2D)

foreign import javascript "((ctx) => { ctx.beginPath(); })"
	js_beginPath :: JSVal -> IO ()

foreign import javascript "((ctx) => { ctx.closePath(); })"
	js_closePath :: JSVal -> IO ()

foreign import javascript "((ctx) => { ctx.fill(); })"
	js_fill :: JSVal -> IO ()

foreign import javascript "((ctx, pth) => { ctx.stroke(pth); })"
	js_strokeWithPath2D :: JSVal -> JSVal -> IO ()

foreign import javascript "((ctx, pth) => { ctx.fill(pth); })"
	js_fillWithPath2D :: JSVal -> JSVal -> IO ()

foreign import javascript "((ctx) => { ctx.stroke(); })"
	js_stroke :: JSVal -> IO ()

moveTo, lineTo :: Path2D -> Double -> Double -> IO ()
moveTo (Path2D c) = js_moveTo c
lineTo (Path2D c) = js_lineTo c

foreign import javascript "((ctx, x, y) => { ctx.moveTo(x, y); })"
	js_moveTo :: JSVal -> Double -> Double -> IO ()

foreign import javascript "((ctx, x, y) => { ctx.lineTo(x, y); })"
	js_lineTo :: JSVal -> Double -> Double -> IO ()

rect :: Path2D -> Double -> Double -> Double -> Double -> IO ()
rect (Path2D pth) = js_rect pth

foreign import javascript "((ctx, x, y, w, h) => { ctx.rect(x, y, w, h); })"
	js_rect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

arc :: Path2D -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()
arc (Path2D ctx) = js_arc ctx

foreign import javascript
	"((ctx, x, y, r, sa, ea, cc) => { ctx.arc(x, y, r, sa, ea, cc); })"
	js_arc ::
		JSVal -> Double -> Double -> Double -> Double -> Double -> Bool -> IO ()

arcTo :: Path2D -> Double -> Double -> Double -> Double -> Double -> IO ()
arcTo (Path2D ctx) = js_arcTo ctx

foreign import javascript
	"((ctx, x1, y1, x2, y2, r) => { ctx.arcTo(x1, y1, x2, y2, r); })"
	js_arcTo :: JSVal -> Double -> Double -> Double -> Double -> Double -> IO ()

quadraticCurveTo :: Path2D -> Double -> Double -> Double -> Double -> IO ()
quadraticCurveTo (Path2D ctx) = js_quadraticCurveTo ctx

foreign import javascript
	"((ctx, cp1x, cp1y, x, y) => { ctx.quadraticCurveTo(cp1x, cp1y, x, y); })"
	js_quadraticCurveTo ::
		JSVal -> Double -> Double -> Double -> Double -> IO ()

bezierCurveTo :: Path2D ->
	Double -> Double -> Double -> Double -> Double -> Double -> IO ()
bezierCurveTo (Path2D ctx) = js_bezierCurveTo ctx

foreign import javascript
	"((ctx, cp1x, cp1y, cp2x, cp2y, x, y) => { ctx.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y); })"
	js_bezierCurveTo ::
		JSVal -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()

save :: Context2D -> IO ()
save (Context2D ctx) = js_save ctx

foreign import javascript "((ctx) => { ctx.save(); })"
	js_save :: JSVal -> IO ()

restore :: Context2D -> IO ()
restore (Context2D ctx) = js_restore ctx

foreign import javascript "((ctx) => { ctx.restore(); })"
	js_restore :: JSVal -> IO ()

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

	let	pth0 = context2DToPath2D ctx

	roundedRect ctx 12 12 150 150 15
	roundedRect ctx 19 19 150 150 9
	roundedRect ctx 53 53 49 33 10
	roundedRect ctx 53 119 49 16 6
	roundedRect ctx 135 53 49 33 10
	roundedRect ctx 135 119 25 49 10

	beginPath ctx
	arc pth0 37 37 13 (pi / 7) (- pi / 7) False
	lineTo pth0 31 37
	fill ctx Nothing

	for_ [0 .. 7] \i -> fillRect ctx Rectangle {
		left = 51 + i * 16, top = 35, width = 4, height = 4 }

	for_ [0 .. 5] \i -> fillRect ctx Rectangle {
		left = 115, top = 51 + i * 16, width = 4, height = 4 }

	for_ [0 .. 7] \i -> fillRect ctx Rectangle {
		left = 51 + i * 16, top = 99, width = 4, height = 4 }

	beginPath ctx
	moveTo pth0 83 116
	lineTo pth0 83 102
	bezierCurveTo pth0 83 94 89 88 97 88
	bezierCurveTo pth0 105 88 111 94 111 102
	lineTo pth0 111 116
	lineTo pth0 106.333 111.333
	lineTo pth0 101.666 116
	lineTo pth0 97 111.333
	lineTo pth0 92.333 116
	lineTo pth0 87.666 111.333
	lineTo pth0 83 116
	fill ctx Nothing

	setFillStyle ctx White
	beginPath ctx
	moveTo pth0 91 96
	bezierCurveTo pth0 88 96 87 99 87 101
	bezierCurveTo pth0 87 103 88 106 91 106
	bezierCurveTo pth0 94 106 95 103 95 101
	bezierCurveTo pth0 95 99 94 96 91 96
	moveTo pth0 103 96
	bezierCurveTo pth0 100 96 99 99 99 101
	bezierCurveTo pth0 99 103 100 106 103 106
	bezierCurveTo pth0 106 106 107 103 107 101
	bezierCurveTo pth0 107 99 106 96 103 96
	fill ctx Nothing

	setFillStyle ctx Black
	beginPath ctx
	arc pth0 101 102 2 0 (pi * 2) True
	fill ctx Nothing
	beginPath ctx
	arc pth0 89 102 2 0 (pi * 2) True
	fill ctx Nothing

roundedRect ::
	Context2D -> Double -> Double -> Double -> Double -> Double -> IO ()
roundedRect ctx x y w h rd = do
	let	pth0 = context2DToPath2D ctx
	beginPath ctx
	moveTo pth0 x (y + rd)
	arcTo pth0 x (y + h) (x + rd) (y + h) rd
	arcTo pth0 (x + w) (y + h) (x + w) (y + h - rd) rd
	arcTo pth0 (x + w) y (x + w - rd) y rd
	arcTo pth0 x y x (y + rd) rd
	stroke ctx Nothing

triangle :: Context2D -> IO ()
triangle ctx = do
	let	pth0 = context2DToPath2D ctx
	beginPath ctx

	moveTo pth0 0 0
	lineTo pth0 150 0
	lineTo pth0 75 129.9

	moveTo pth0 75 20
	lineTo pth0 50 60
	lineTo pth0 100 60

	fill ctx Nothing

newtype EventType = EventType String deriving Show

onClick :: Canvas -> (ClickEvent -> IO ()) -> IO ()
onClick c = addEventListener c (EventType "click")

onTouchstart :: Canvas -> (TouchEvent -> IO ()) -> IO ()
onTouchstart c = addEventListener c (EventType "touchstart")

onPointerdown :: Canvas -> (PointerEvent -> IO ()) -> IO ()
onPointerdown c = addEventListener c (EventType "pointerdown")

addEventListener :: IsEvent e => Canvas -> EventType -> (e -> IO ()) -> IO ()
addEventListener (Canvas c) (EventType etp) f = do
	f' <- syncCallback1 ThrowWouldBlock $ f . fromJSVal
	js_addEventListener c (toJSString etp) f'

foreign import javascript
	"((etg, etp, f) => { etg.addEventListener(etp, (e) => { f(e) }); })"
	js_addEventListener :: JSVal -> JSVal -> Callback (JSVal -> IO ()) -> IO ()

class IsEvent e where
	fromJSVal :: JSVal -> e
	toJSVal :: e -> JSVal

data ClickEvent = ClickEvent JSVal

instance IsEvent ClickEvent where
	fromJSVal = ClickEvent
	toJSVal (ClickEvent e) = e

offsetX :: ClickEvent -> Double
offsetX (ClickEvent e) = js_offsetX e

foreign import javascript "((e) => { return e.offsetX; })"
	js_offsetX :: JSVal -> Double

offsetY :: ClickEvent -> Double
offsetY (ClickEvent e) = js_offsetY e

foreign import javascript "((e) => { return e.offsetY; })"
	js_offsetY :: JSVal -> Double

preventDefault :: IsEvent e => e -> IO ()
preventDefault = js_preventDefault . toJSVal

foreign import javascript "((ev) => { ev.preventDefault(); })"
	js_preventDefault :: JSVal -> IO ()

data TouchEvent = TouchEvent JSVal

instance IsEvent TouchEvent where
	fromJSVal = TouchEvent
	toJSVal (TouchEvent e) = e

data PointerEvent = PointerEvent JSVal

instance IsEvent PointerEvent where
	fromJSVal = PointerEvent
	toJSVal (PointerEvent e) = e

-- pointerEventToMouseEvent :: PointerEvent -> MouseEvent
-- pointerEventToMouseEvent (PointerEvent e) = MouseEvent e

pointerEventToClickEvent :: PointerEvent -> ClickEvent
pointerEventToClickEvent (PointerEvent e) = ClickEvent e

data Date = Date JSVal

newDate :: IO Date
newDate = Date <$> js_newDate

foreign import javascript "(() => { let d = new Date(); return d; })"
	js_newDate :: IO JSVal

instance Show Date where
	show (Date d) = fromJSString $ js_toString d

foreign import javascript "((o) => { return o.toString(); })"
	js_toString :: JSVal -> JSVal

setInterval :: IO () -> Double -> IO ()
setInterval f d = do
	f' <- syncCallback ThrowWouldBlock f
	js_setInterval f' d

foreign import javascript "((f, d) => { setInterval(f, d); })"
	js_setInterval :: Callback (IO ()) -> Double -> IO ()
