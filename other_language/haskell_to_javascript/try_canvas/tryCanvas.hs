{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Foldable
import GHC.JS.Value.String qualified as JS.Str

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

import GHC.JS.Value.Date qualified as JS.Date
import Data.Color qualified as Color

import Hello

main :: IO ()
main = do
	let	document = JS.Window.document JS.Window.w
	Just foo <- JS.Document.getElementById document "foo"
	JS.EventTarget.addEventListenerSimple
		(JS.EventTarget.toE JS.Window.w) "resize" \_ -> do
			wdt <- JS.Window.getInnerWidth JS.Window.w
			hgt <- JS.Window.getInnerHeight JS.Window.w
			szt <- JS.Text.new $ " Win Size: " ++ show (wdt, hgt)
			while_ (JS.Node.hasChildNodes $ JS.Node.toN foo) do
				Just fc <- JS.Node.firstChild (JS.Node.toN foo)
				() <$ JS.Node.removeChild (JS.Node.toN foo) fc
			JS.Node.toN foo `JS.Node.appendChild` JS.Node.toN szt
	print $ JS.Document.getDocumentURI document
	print . JS.Node.getNodeName $ JS.Node.toN document
	print . JS.Node.getNodeType $ JS.Node.toN document
	print @(Maybe JS.Document.D) . (JS.Node.fromN =<<)
		=<< parentOfChild (JS.Node.toN document)
	print $ JS.Element.getTagName foo
	print . (JS.Node.getNodeType <$>)
		=<< JS.Node.firstChild (JS.Node.toN foo)
	Just clocktime <- JS.Document.getElementById document "clocktime"
	baz <- JS.Text.new $ hello ++ " Yoshikuni Jujo"
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
	Just canvas' <- JS.Document.getElementById document "canvas"
	let	Just cvs = JS.Element.fromE canvas'
	print . isJust @JS.HtmlParagraphElement.P $ JS.Element.fromE canvas'
	print =<< maybe (pure 0) JS.HtmlCanvasElement.getHeight (JS.Element.fromE canvas')
	print =<< JS.HtmlCanvasElement.getHeight cvs
	JS.Window.setInterval JS.Window.w (do
		nows <- JS.Object.toString . JS.Object.toO <$> JS.Date.new
		while_ (JS.Node.hasChildNodes $ JS.Node.toN clocktime) do
			Just fc <- JS.Node.firstChild $ JS.Node.toN clocktime
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
	let	Just ctx' = JS.CanvasContext.fromC ctx_
	JS.CanvasRenderingContext2d.setFillStyleRgb ctx' $ Color.Rgb 200 0 0
	JS.CanvasRenderingContext2d.fillRect ctx' 10 10 50 50
	JS.CanvasRenderingContext2d.setFillStyleRgba ctx' $ Color.Rgba 0 0 200 0x80
	JS.CanvasRenderingContext2d.fillRect ctx' 20 20 50 50

	JS.CanvasRenderingContext2d.setFillStyleRgb ctx' $ Color.Rgb 0 0 0
	JS.CanvasRenderingContext2d.fillRect ctx' 125 25 100 100
	JS.CanvasRenderingContext2d.clearRect ctx' 145 45 60 60
	JS.CanvasRenderingContext2d.strokeRect ctx' 150 50 50 50
	JS.CanvasRenderingContext2d.beginPath ctx'
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 275 50
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 300 75
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 300 25
	JS.CanvasRenderingContext2d.fill
		ctx' Nothing JS.CanvasRenderingContext2d.nonzero

	JS.CanvasRenderingContext2d.beginPath ctx'
	JS.Pathable2d.arc (JS.Pathable2d.toP ctx') 425 75 50 0 (pi * 2) True
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 460 75
	JS.Pathable2d.arc (JS.Pathable2d.toP ctx') 425 75 35 0 pi False
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 415 65
	JS.Pathable2d.arc (JS.Pathable2d.toP ctx') 410 65 5 0 (pi * 2) True
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 445 65
	JS.Pathable2d.arc (JS.Pathable2d.toP ctx') 440 65 5 0 (pi * 2) True
	JS.CanvasRenderingContext2d.stroke ctx' Nothing

	JS.CanvasRenderingContext2d.beginPath ctx'
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 25 125
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 105 125
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 25 205
	JS.CanvasRenderingContext2d.fill
		ctx' Nothing JS.CanvasRenderingContext2d.nonzero

	JS.CanvasRenderingContext2d.beginPath ctx'
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 125 225
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 125 145
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 45 225
	JS.Pathable2d.closePath (JS.Pathable2d.toP ctx')
	JS.CanvasRenderingContext2d.stroke ctx' Nothing

	for_ [0 :: Int .. 3] \i@(fromIntegral -> i') ->
		for_ [0 :: Double .. 2] \j -> do
			JS.CanvasRenderingContext2d.beginPath ctx'
			JS.Pathable2d.arc (JS.Pathable2d.toP ctx') (25 + j * 50) (275 + i' * 50) 20
				0 (pi + pi * j / 2) (i `mod` 2 /= 0)
			if (i > 1)
			then JS.CanvasRenderingContext2d.fill
				ctx' Nothing JS.CanvasRenderingContext2d.nonzero
			else JS.CanvasRenderingContext2d.stroke ctx' Nothing

	JS.CanvasRenderingContext2d.beginPath ctx'
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 225 175
	JS.Pathable2d.quadraticCurveTo (JS.Pathable2d.toP ctx') 175 175 175 212.5
	JS.Pathable2d.quadraticCurveTo (JS.Pathable2d.toP ctx') 175 250 200 250
	JS.Pathable2d.quadraticCurveTo (JS.Pathable2d.toP ctx') 200 270 180 275
	JS.Pathable2d.quadraticCurveTo (JS.Pathable2d.toP ctx') 210 270 215 250
	JS.Pathable2d.quadraticCurveTo (JS.Pathable2d.toP ctx') 275 250 275 212.5
	JS.Pathable2d.quadraticCurveTo (JS.Pathable2d.toP ctx') 275 175 225 175
	JS.CanvasRenderingContext2d.stroke ctx' Nothing

	JS.CanvasRenderingContext2d.beginPath ctx'
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 375 190
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 375 187 370 175 350 175
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 320 175 320 212.5 320 212.5
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 320 230 340 252 375 270
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 410 252 430 230 430 212.5
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 430 212.5 430 175 400 175
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 385 175 375 187 375 190
	JS.CanvasRenderingContext2d.fill
		ctx' Nothing JS.CanvasRenderingContext2d.nonzero

	JS.CanvasRenderingContext2d.save ctx'
	JS.CanvasRenderingContext2d.translate ctx' 175 300
	draw ctx'

	JS.CanvasRenderingContext2d.translate ctx' 200 25
	triangle ctx'

	rectangle <- JS.Path2d.new JS.Path2d.FromScratch
	JS.Pathable2d.rect (JS.Pathable2d.toP rectangle) 10 10 50 50

	circle <- JS.Path2d.new JS.Path2d.FromScratch
	JS.Pathable2d.arc
		(JS.Pathable2d.toP circle) 100 35 25 0 (2 * pi) False

	JS.CanvasRenderingContext2d.restore ctx'

	JS.CanvasRenderingContext2d.translate ctx' 0 480
	JS.CanvasRenderingContext2d.stroke ctx' (Just rectangle)
	JS.CanvasRenderingContext2d.fill ctx' (Just circle)
		JS.CanvasRenderingContext2d.nonzero

	JS.Path2d.addPathNoTransform rectangle circle
	JS.CanvasRenderingContext2d.translate ctx' 150 0
	JS.CanvasRenderingContext2d.stroke ctx' $ Just rectangle

	svgp <- JS.Path2d.new $ JS.Path2d.FromSvgPath "M10 10 h 80 v 80 h -80 Z"
	JS.CanvasRenderingContext2d.translate ctx' 150 0
	JS.CanvasRenderingContext2d.stroke ctx' $ Just svgp

-- END OF MAIN

draw :: JS.CanvasRenderingContext2d.R -> IO ()
draw ctx' = do

	roundedRect ctx' 12 12 150 150 15
	roundedRect ctx' 19 19 150 150 9
	roundedRect ctx' 53 53 49 33 10
	roundedRect ctx' 53 119 49 16 6
	roundedRect ctx' 135 53 49 33 10
	roundedRect ctx' 135 119 25 49 10

	JS.CanvasRenderingContext2d.beginPath ctx'
	JS.Pathable2d.arc (JS.Pathable2d.toP ctx') 37 37 13 (pi / 7) (- pi / 7) False
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 31 37
	JS.CanvasRenderingContext2d.fill
		ctx' Nothing JS.CanvasRenderingContext2d.nonzero

	for_ [0 .. 7] \i ->
		JS.CanvasRenderingContext2d.fillRect ctx' (51 + i * 16) 35 4 4

	for_ [0 .. 5] \i ->
		JS.CanvasRenderingContext2d.fillRect ctx' 115 (51 + i * 16) 4 4

	for_ [0 .. 7] \i ->
		JS.CanvasRenderingContext2d.fillRect ctx' (51 + i * 16) 99 4 4

	JS.CanvasRenderingContext2d.beginPath ctx'
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 83 116
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 83 102
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 83 94 89 88 97 88
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 105 88 111 94 111 102
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 111 116
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 106.333 111.333
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 101.666 116
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 97 111.333
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 92.333 116
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 87.666 111.333
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 83 116
	JS.CanvasRenderingContext2d.fill
		ctx' Nothing JS.CanvasRenderingContext2d.nonzero

	JS.CanvasRenderingContext2d.setFillStyleColorName ctx' Color.White
	JS.CanvasRenderingContext2d.beginPath ctx'
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 91 96
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 88 96 87 99 87 101
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 87 103 88 106 91 106
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 94 106 95 103 95 101
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 95 99 94 96 91 96
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 103 96
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 100 96 99 99 99 101
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 99 103 100 106 103 106
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 106 106 107 103 107 101
	JS.Pathable2d.bezierCurveTo (JS.Pathable2d.toP ctx') 107 99 106 96 103 96
	JS.CanvasRenderingContext2d.fill
		ctx' Nothing JS.CanvasRenderingContext2d.nonzero

	JS.CanvasRenderingContext2d.setFillStyleColorName ctx' Color.Black
	JS.CanvasRenderingContext2d.beginPath ctx'
	JS.Pathable2d.arc (JS.Pathable2d.toP ctx') 101 102 2 0 (pi * 2) True
	JS.CanvasRenderingContext2d.fill
		ctx' Nothing JS.CanvasRenderingContext2d.nonzero
	JS.CanvasRenderingContext2d.beginPath ctx'
	JS.Pathable2d.arc (JS.Pathable2d.toP ctx') 89 102 2 0 (pi * 2) True
	JS.CanvasRenderingContext2d.fill
		ctx' Nothing JS.CanvasRenderingContext2d.nonzero

roundedRect ::
	JS.CanvasRenderingContext2d.R -> Double -> Double -> Double -> Double -> Double -> IO ()
roundedRect ctx' x y w h rd = do
	JS.CanvasRenderingContext2d.beginPath ctx'
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') x (y + rd)
	JS.Pathable2d.arcTo (JS.Pathable2d.toP ctx') x (y + h) (x + rd) (y + h) rd
	JS.Pathable2d.arcTo (JS.Pathable2d.toP ctx') (x + w) (y + h) (x + w) (y + h - rd) rd
	JS.Pathable2d.arcTo (JS.Pathable2d.toP ctx') (x + w) y (x + w - rd) y rd
	JS.Pathable2d.arcTo (JS.Pathable2d.toP ctx') x y x (y + rd) rd
	JS.CanvasRenderingContext2d.stroke ctx' Nothing

triangle :: JS.CanvasRenderingContext2d.R -> IO ()
triangle ctx' = do
	JS.CanvasRenderingContext2d.beginPath ctx'

	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 0 0
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 150 0
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 75 129.9

	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx') 75 20
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 50 60
	JS.Pathable2d.lineTo (JS.Pathable2d.toP ctx') 100 60

	JS.CanvasRenderingContext2d.fill
		ctx' Nothing JS.CanvasRenderingContext2d.nonzero

onPointerdown :: JS.HtmlCanvasElement.C -> (JS.MouseEvent.M -> IO ()) -> IO ()
onPointerdown c a = JS.EventTarget.addEventListenerSimple
	(JS.EventTarget.toE c) "pointerdown" (a . fromJust . JS.Event.fromE)

while_ :: IO Bool -> IO a -> IO ()
while_ p act = do
	b <- p
	when b $ act >> while_ p act

parentOfChild :: JS.Node.N -> IO (Maybe JS.Node.N)
parentOfChild nd = (JS.Node.parentNode =<<) <$> JS.Node.firstChild nd
