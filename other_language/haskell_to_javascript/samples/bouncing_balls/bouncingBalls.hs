{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Document qualified as JS.Document
import GHC.JS.Value.Window qualified as JS.Window
import GHC.JS.Value.Element qualified as JS.Element
import GHC.JS.Value.HtmlElement.Canvas qualified as JS.HtmlCanvasElement
import GHC.JS.Value.CanvasContext qualified as JS.CanvasContext
import GHC.JS.Value.CanvasContext.Rendering2d
	qualified as JS.CanvasRenderingContext2d
import GHC.JS.Value.CanvasContext.Rendering2d.Pathable
	qualified as JS.Pathable2d
import GHC.JS.Value.Event qualified as JS.Event
import GHC.JS.Value.Event.Mouse qualified as JS.MouseEvent
import GHC.JS.Value.Date qualified as JS.Date

import Control.Arrow
import Control.Concurrent.STM
import Data.Maybe
import Data.Color qualified as Color
import CalcBall

main :: IO ()
main = atomically (newTVar []) >>= \balls -> do
	cvs <- raise "no such canvas" $ (JS.Element.fromE =<<)
		<$> JS.Document.getElementById document "canvas"
	(w, (h, h')) <- (second (id &&& subtract 10) .) . (,)
		<$> JS.HtmlCanvasElement.getWidth cvs
		<*> JS.HtmlCanvasElement.getHeight cvs
	ctx <- raise "no RenderingContext2d" $ (JS.CanvasContext.fromC =<<)
		<$> JS.HtmlCanvasElement.getContext
			cvs JS.HtmlCanvasElement.ContextType2d
	JS.CanvasRenderingContext2d.setFillStyle ctx $ Color.Rgb 200 0 0

	flip (JS.Window.setInterval JS.Window.w) 30 do
		t <- JS.Date.getTime <$> JS.Date.new
		JS.CanvasRenderingContext2d.beginPath ctx
		(uncurry (drawBall h' ctx t) `mapM_`)
			=<< atomically (readTVar balls)
		JS.CanvasRenderingContext2d.clearRect ctx 0 0 w h
		JS.CanvasRenderingContext2d.fill
			ctx Nothing JS.CanvasRenderingContext2d.nonzero

	flip (JS.Window.setInterval JS.Window.w) 2000
		$ atomically . modifyTVar balls . filter
			. uncurry . isAlive h' . JS.Date.getTime =<< JS.Date.new

	onPointerdown cvs \e -> atomically . modifyTVar balls . (:) =<< (,)
		<$> (JS.Date.getTime <$> JS.Date.new)
		<*> ((,)
			<$> JS.MouseEvent.offsetX e <*> JS.MouseEvent.offsetY e)

document :: JS.Document.D
document = JS.Window.document JS.Window.w

drawBall :: JS.Pathable2d.IsP p =>
	Double -> p -> Double -> Double -> (Double, Double) -> IO ()
drawBall h ctx t t0 (x0, y0) = do
	JS.Pathable2d.moveTo (JS.Pathable2d.toP ctx) x y
	JS.Pathable2d.arc (JS.Pathable2d.toP ctx) x (h - y) 10 0 (pi * 2) False
	where (x, fromMaybe 0 -> y) = ballPos t t0 (x0, h - y0)

isAlive :: Double -> Double -> Double -> (Double, Double) -> Bool
isAlive h t t0 (x, y) = isJust . snd $ ballPos t t0 (x, h - y)

onPointerdown :: JS.HtmlCanvasElement.C -> (JS.MouseEvent.M -> IO ()) -> IO ()
onPointerdown c a = JS.EventTarget.addEventListenerSimple
	(JS.EventTarget.toE c) "pointerdown" (a . fromJust . JS.Event.fromE)

raise :: String -> IO (Maybe a) -> IO a
raise emsg = (maybe (error emsg) pure =<<)
