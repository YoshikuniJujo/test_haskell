{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Foldable
import GHC.JS.Prim
import GHC.JS.Foreign.Callback
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.String qualified as JS.Str
import Data.Word

import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Window qualified as JS.Window

import GHC.JS.Value.Node qualified as JS.Node
import GHC.JS.Value.Document qualified as JS.Document
import GHC.JS.Value.Element qualified as JS.Element
import GHC.JS.Value.HtmlElement qualified as JS.HtmlElement
import GHC.JS.Value.HtmlElement.Paragraph qualified as JS.HtmlParagraphElement
import GHC.JS.Value.HtmlElement.Canvas qualified as JS.HtmlCanvasElement

import GHC.JS.Value.CharacterData.Text qualified as JS.Text

import GHC.JS.Value.CanvasContext qualified as JS.CanvasContext
import GHC.JS.Value.CanvasContext.Rendering2d
	qualified as JS.CanvasRenderingContext2d
import GHC.JS.Value.CanvasContext.Rendering2d.Path qualified as JS.Path2d
import GHC.JS.Value.CanvasContext.Rendering2d.Pathable
	qualified as JS.Pathable2d

import GHC.JS.Value.Event qualified as JS.Event
import GHC.JS.Value.Event.Mouse qualified as JS.MouseEvent
import GHC.JS.Value.Event.Pointer qualified as JS.PointerEvent

import Data.Maybe

import Hello

main :: IO ()
main = do
	print . fromJSString $ js_toString js_this
	print $ js_same js_this JS.Window.js_w
	print $ js_same js_this JS.Document.js_d
	Just foo <- JS.Document.getElementById JS.Document.d "foo"
	JS.EventTarget.addEventListenerSimple
		(JS.EventTarget.toE JS.Window.w) "resize" \_ -> do
			wdt <- JS.Window.getInnerWidth JS.Window.w
			hgt <- JS.Window.getInnerHeight JS.Window.w
			szt <- JS.Text.new $ " Win Size: " ++ show (wdt, hgt)
			while_ (JS.Node.hasChildNodes $ JS.Node.toN foo) do
				Just fc <- JS.Node.firstChild (JS.Node.toN foo)
				() <$ JS.Node.removeChild (JS.Node.toN foo) fc
			JS.Node.toN foo `JS.Node.appendChild` JS.Node.toN szt
	print $ JS.Document.getDocumentURI JS.Document.d
	print . JS.Node.getNodeName $ JS.Node.toN JS.Document.d
	print . JS.Node.getNodeType $ JS.Node.toN JS.Document.d
	print @(Maybe JS.Document.D) . (JS.Node.fromN =<<)
		=<< parentOfChild (JS.Node.toN JS.Document.d)
	print $ JS.Element.getTagName foo
	print . (JS.Node.getNodeType <$>)
		=<< JS.Node.firstChild (JS.Node.toN foo)
	Just clocktime <- JS.Document.getElementById JS.Document.d "clocktime"
	baz <- JS.Text.new $ hello ++ " YJ"
	JS.Node.toN foo `JS.Node.appendChild` JS.Node.toN baz
	print . (JS.Node.getNodeType <$>)
		=<< JS.Node.firstChild (JS.Node.toN foo)
	print $ JS.Object.toO foo `JS.Object.isInstanceOf` JS.Element.eClass
	print $ JS.Object.toO JS.Window.w
		`JS.Object.isInstanceOf` JS.Element.eClass
	print =<< maybe (pure Nothing)
		((Just <$>) . JS.HtmlElement.getOffsetWidth)
		(JS.Element.fromE foo)
	print . isJust @JS.HtmlParagraphElement.P $ JS.Element.fromE foo
	Just canvas' <- JS.Document.getElementById JS.Document.d "canvas"
	let	Just cvs = JS.Element.fromE canvas'
	print . isJust @JS.HtmlParagraphElement.P $ JS.Element.fromE canvas'
	print =<< maybe (pure 0) JS.HtmlCanvasElement.getHeight (JS.Element.fromE canvas')
	print =<< JS.HtmlCanvasElement.getHeight cvs
	JS.Window.setInterval JS.Window.w (do
		nows <- show <$> newDate
		while_ (JS.Node.hasChildNodes $ JS.Node.toN clocktime) do
			Just fc <- JS.Node.firstChild (JS.Node.toN clocktime)
			() <$ JS.Node.removeChild (JS.Node.toN clocktime) fc
		tmt <- JS.Text.new $ "NOW: " ++ nows
		JS.Node.toN clocktime `JS.Node.appendChild` JS.Node.toN tmt) 1000

	onPointerdown cvs \e -> do
		szt <- JS.Text.new $ "ptr down: " ++ show (
			JS.MouseEvent.offsetX e,
			JS.MouseEvent.offsetY e )
		while_ (JS.Node.hasChildNodes $ JS.Node.toN foo) do
			Just fc <- JS.Node.firstChild (JS.Node.toN foo)
			() <$ JS.Node.removeChild (JS.Node.toN foo) fc
		JS.Node.toN foo `JS.Node.appendChild` JS.Node.toN szt

	Just ctx_ <- JS.HtmlCanvasElement.getContext cvs JS.HtmlCanvasElement.ContextType2d
	let	ctx = Context2D $ JS.Value.toJSVal ctx_
		pth0 = context2DToPath2D ctx
		Just ctx' = JS.CanvasContext.fromC ctx_
	setFillStyle ctx $ Rgb 200 0 0
	JS.CanvasRenderingContext2d.fillRect ctx' 10 10 50 50
	setFillStyle ctx $ Rgba 0 0 200 0.5
	JS.CanvasRenderingContext2d.fillRect ctx' 20 20 50 50

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


	rectangle <- JS.Path2d.new JS.Path2d.FromScratch
	JS.Pathable2d.rect (JS.Pathable2d.toP rectangle) 10 10 50 50

	circle <- JS.Path2d.new JS.Path2d.FromScratch
	JS.Pathable2d.arc
		(JS.Pathable2d.toP circle) 100 35 25 0 (2 * pi) False

	restore ctx

	translate ctx 0 480
	JS.CanvasRenderingContext2d.stroke ctx' (Just rectangle)
	JS.CanvasRenderingContext2d.fill ctx' (Just circle)
		JS.CanvasRenderingContext2d.nonzero

	JS.Path2d.addPathNoTransform rectangle circle
	translate ctx 150 0
	JS.CanvasRenderingContext2d.stroke ctx' $ Just rectangle

	svgp <- newPath2D $ Path2DFromSvgPath "M10 10 h 80 v 80 h -80 Z"
	translate ctx 150 0
	stroke ctx . Just $ addablePath2DToPath2D svgp

-- END OF MAIN

data Context2D = Context2D JSVal

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

onPointerdown :: JS.HtmlCanvasElement.C -> (JS.MouseEvent.M -> IO ()) -> IO ()
onPointerdown c a = JS.EventTarget.addEventListenerSimple
	(JS.EventTarget.toE c) "pointerdown" (a . fromJust . JS.Event.fromE)

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

foreign import javascript "((e) => { return e.tagName; })"
	js_getTagName :: JSVal -> JSVal

foreign import javascript "((w) => { return w.name; })"
	js_getWindowName :: JSVal -> JSVal

foreign import javascript "((w) => { return w.navigator; })"
	js_getWindowNavigator :: JSVal -> JSVal

foreign import javascript "((n) => { return n.userAgent; })"
	js_getNavigatorUserAgent :: JSVal -> JSVal

parentOfChild :: JS.Node.N -> IO (Maybe JS.Node.N)
parentOfChild nd = (JS.Node.parentNode =<<) <$> JS.Node.firstChild nd

while_ :: IO Bool -> IO a -> IO ()
while_ p act = do
	b <- p
	when b $ act >> while_ p act

foreign import javascript "(() => { return this; })" js_this :: JSVal

foreign import javascript "((a, b) => { return (a === b); })"
	js_same :: JSVal -> JSVal -> Bool
