module DrawArea (
	runArea,
	module Area
) where

import Area
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)
import Control.Monad

runArea :: Double -> Double -> AreaM a -> IO String
runArea w h = drawAll w h . runAreaM

drawAll :: Double -> Double -> [Draw] -> IO String
drawAll w h ds = do
	t <- initialize
	mapM_ (draw t) ds
	svgs <- getSVG t
	return $ showSVG w h svgs

initialize :: IO Turtle
initialize = do
	f <- openField
	topleft f
	newTurtle f

draw :: Turtle -> Draw -> IO ()
draw t (Rectangle x y w h) = do
	penup t
	goto t x y
	setheading t 0
	pensize t 2
	pendown t
	replicateM_ 2 $ do
		forward t w
		right t 90
		forward t h
		right t 90
draw t (HLine dot x y w) = do
	penup t
	goto t x y
	setheading t 0
	pensize t 1
	pendown t
	if dot then dotForward t 4 w else forward t w
draw t (VLine dot x y w) = do
	penup t
	goto t x y
	setheading t (- 90)
	pensize t 1
	pendown t
	if dot then dotForward t 4 w else forward t w
draw t (Str b x y s str) = do
	penup t
	goto t x y
	let font = if b then "KochiGothic" else "KochiMincho"
	write t font s str

dotForward :: Turtle -> Double -> Double -> IO ()
dotForward t d l = replicateM_ (floor $ l / (2 * d)) $ do
	pendown t
	forward t d
	penup t
	forward t d
