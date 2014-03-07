module AutomatonLib where

import Graphics.X11.Turtle
import Control.Monad

initialQ :: Turtle -> Double -> Double -> Double -> String -> IO ()
initialQ t s x y n = do
	goto t x y
	setheading t (- 90)
	forward t (15 * s)
	pendown t
	setheading t 0
	forward t (15 * s)
	right t 30
	backward t (5 * s)
	forward t (5 * s)
	left t 60
	backward t (5 * s)
	forward t (5 * s)
	right t 30
	penup t
	forward t (15 * s)
	setheading t 90
	forward t (15 * s)
	addQ t s n

addQ :: Turtle -> Double -> String -> IO ()
addQ t s n = do
	pendown t
	setheading t 180
	circle t (15 * s)
	penup t
	setheading t (- 90)
	forward t (20 * s)
	setheading t 0
	backward t (8 * s)
	write t "Monospace" (10 * s) n
	forward t (8 * s)
	setheading t (- 90)
	backward t (20 * s)

selfQ :: Turtle -> Double -> String -> IO ()
selfQ t s i = do
	penup t
	setheading t 0
	backward t (10 * s)
	setheading t (- 90)
	forward t (3 * s)
	setheading t 120
	pendown t
	replicateM_ 100 $ right t 1 >> forward t (0.2 * s)
	penup t
	h <- heading t
	setheading t 90
	forward t (5 * s)
	write t "Monospace" (10 * s) i
	backward t (5 * s)
	setheading t h
	pendown t
	replicateM_ 140 $ right t 1 >> forward t (0.2 * s)
	right t 30
	backward t (5 * s)
	forward t (5 * s)
	left t 60
	backward t (5 * s)
	forward t (5 * s)
	penup t
	setheading t 0
	backward t (10 * s)
	setheading t 90
	forward t (3 * s)

nextQ :: Turtle -> Double -> String -> String -> IO ()
nextQ t s i n = do
	setheading t 0
	forward t (13 * s)
	setheading t (- 90)
	forward t (8 * s)
	pendown t
	setheading t 30
	replicateM_ 20 $ right t 1 >> forward t (1 * s)
	penup t
	h <- heading t
	setheading t 90
	forward t (5 * s)
	write t "Monospace" (10 * s) i
	backward t (5 * s)
	setheading t h
	pendown t
	replicateM_ 40 $ right t 1 >> forward t (1 * s)
	right t 30
	backward t (5 * s)
	forward t (5 * s)
	left t 60
	backward t (5 * s)
	forward t (5 * s)
	penup t
	setheading t 0
	forward t (13 * s)
	setheading t 90
	forward t (8 * s)
	addQ t s n

acceptQ :: Turtle -> Double -> IO ()
acceptQ t s = do
	setheading t (- 90)
	forward t (3 * s)
	setheading t 180
	pendown t
	circle t (12 * s)
	penup t
	setheading t 90
	forward t (3 * s)

backQ :: Turtle -> Double -> String -> IO ()
backQ t s i = do
	setheading t 0
	backward t (13 * s)
	setheading t (- 90)
	forward t (22 * s)
	pendown t
	setheading t (- 150)
	replicateM_ 40 $ right t 1 >> forward t (1 * s)
	penup t
	h <- heading t
	setheading t (- 90)
	forward t (12 * s)
	write t "Monospace" (10 * s) i
	backward t (12 * s)
	setheading t h
	pendown t
	replicateM_ 20 $ right t 1 >> forward t (1 * s)
	right t 30
	backward t (5 * s)
	forward t (5 * s)
	left t 60
	backward t (5 * s)
	forward t (5 * s)
	right t 30
	penup t
	replicateM_ 60 $ left t 1 >> backward t (1 * s)
	setheading t 0
	forward t (13 * s)
	setheading t 90
	forward t (22 * s)
