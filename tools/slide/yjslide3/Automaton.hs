module Automaton where

import Control.Monad
import Lecture

initialQ :: Turtle -> Double -> Double -> String -> IO ()
initialQ t x y n = do
	pensizeRt t 1
	rtGoto t x y
	setheading t (- 90)
	forwardRt t 15
	pendown t
	setheading t 0
	forwardRt t 15
	right t 30
	backwardRt t 5
	forwardRt t 5
	left t 60
	backwardRt t 5
	forwardRt t 5
	right t 30
	penup t
	forwardRt t 15
	setheading t 90
	forwardRt t 15
	addQ t n

addQ :: Turtle -> String -> IO ()
addQ t n = do
	circleRt t 15
	setheading t (- 90)
	forwardRt t 20
	setheading t 0
	backwardRt t 8
	writeRt t n
	forwardRt t 8
	setheading t (- 90)
	backwardRt t 20

selfQ :: Turtle -> String -> IO ()
selfQ t i = do
	setheading t 0
	backwardRt t 10
	setheading t (- 90)
	forwardRt t 3
	setheading t 120
	pendown t
	replicateM_ 100 $ right t 1 >> forwardRt t 0.2
	penup t
	h <- heading t
	setheading t 90
	forwardRt t 5
	writeSmallRt t i
	backwardRt t 5
	setheading t h
	pendown t
	replicateM_ 140 $ right t 1 >> forwardRt t 0.2
	right t 30
	backwardRt t 5
	forwardRt t 5
	left t 60
	backwardRt t 5
	forwardRt t 5
	penup t
	setheading t 0
	backwardRt t 10
	setheading t 90
	forwardRt t 3

nextQ :: Turtle -> String -> String -> IO ()
nextQ t i n = do
	setheading t 0
	forwardRt t 13
	setheading t (- 90)
	forwardRt t 8
	pendown t
	setheading t 30
	replicateM_ 20 $ right t 1 >> forwardRt t 1
	penup t
	h <- heading t
	setheading t 90
	forwardRt t 5
	writeSmallRt t i
	backwardRt t 5
	setheading t h
	pendown t
	replicateM_ 40 $ right t 1 >> forwardRt t 1
	right t 30
	backwardRt t 5
	forwardRt t 5
	left t 60
	backwardRt t 5
	forwardRt t 5
	penup t
	setheading t 0
	forwardRt t 13
	setheading t 90
	forwardRt t 8
	addQ t n

acceptQ :: Turtle -> IO ()
acceptQ t = do
	setheading t (- 90)
	forwardRt t 3.5
	setheading t 180
	circleRt t 12
	setheading t 90
	forwardRt t 3.5

backQ :: Turtle -> String -> IO ()
backQ t i = do
	setheading t 0
	backwardRt t 13
	setheading t (- 90)
	forwardRt t 22
	pendown t
	setheading t (- 150)
	replicateM_ 40 $ right t 1 >> forwardRt t 1
	penup t
	h <- heading t
	setheading t (- 90)
	forwardRt t 12
	writeSmallRt t i
	backwardRt t 12
	setheading t h
	pendown t
	replicateM_ 20 $ right t 1 >> forwardRt t 1
	right t 30
	backwardRt t 5
	forwardRt t 5
	left t 60
	backwardRt t 5
	forwardRt t 5
	right t 30
	penup t
	replicateM_ 60 $ left t 1 >> backwardRt t 1
	setheading t 0
	forwardRt t 13
	setheading t 90
	forwardRt t 22
