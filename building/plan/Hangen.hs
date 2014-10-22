module Hangen () where

import Control.Monad
import Graphics.X11.Turtle

shaku :: Double
shaku = 10 / 33

hangen :: Double
hangen = 3 * shaku

data Hanjou = Hanjou [Structure] deriving Show

data Direction = East | West | South | North deriving Show

data Structure
	= Wall Direction
	| Through Direction
	| ThroughBE Direction Double Double
	deriving Show

left0, top0 :: Double
left0 = 50; top0 = 50

unitSize, wallWidth :: Double
unitSize = 35; wallWidth = 3

hangenToPos :: (Int, Int) -> (Double, Double)
hangenToPos (x, y) =
	(left0 + fromIntegral x * unitSize, top0 + fromIntegral y * unitSize)

drawPlan :: Turtle -> [[Hanjou]] -> IO ()
drawPlan t p = (\ys hss dp -> zipWithM_ dp ys hss) [0 ..] p $ \y hs ->
	(\xs hs d -> zipWithM_ d xs hs) [0 ..] hs $ \x h ->
		drawHanjou t (x, y) h

drawHanjou :: Turtle -> (Int, Int) -> Hanjou -> IO ()
drawHanjou t hp (Hanjou ss) = forM_ ss $ \s -> do
	let (x0, y0) = hangenToPos hp
	goto t x0 y0
	pendown t
	drawStructure t s
	penup t

makeWall :: Turtle -> Double -> IO ()
makeWall t ln = do
	forward t (ln * unitSize)
	left t 90
	forward t wallWidth
	left t 90
	forward t (ln * unitSize)
	left t 90
	forward t wallWidth
	left t 90

drawStructure :: Turtle -> Structure -> IO ()
drawStructure t (Wall East) = do
	setheading t 0
	forward t unitSize
	setheading t (- 90)
	forward t unitSize
	setheading t 90
	beginfill t
	makeWall t 1
	endfill t
drawStructure t (Wall West) = do
	beginfill t
	setheading t (- 90)
	makeWall t 1
	endfill t
drawStructure t (Wall North) = do
	setheading t 0
	forward t unitSize
	beginfill t
	setheading t 180
	makeWall t 1
	endfill t
drawStructure t (Wall South) = do
	penup t
	setheading t (- 90)
	forward t unitSize
	pendown t
	beginfill t
	setheading t 0
	makeWall t 1
	endfill t
drawStructure t (Through North) = do
	setheading t 0
	forward t unitSize
	setheading t 180
	makeWall t 1
drawStructure t (ThroughBE East b e) = do
	setheading t 0
	forward t unitSize
	setheading t (- 90)
	forward t unitSize
	setheading t 90
	beginfill t
	makeWall t (1 - e)
	endfill t
	forward t $ (1 - e) * unitSize
	makeWall t (e - b)
	forward t $ (e - b) * unitSize
	beginfill t
	makeWall t b
	endfill t
drawStructure t (ThroughBE West b e) = do
	setheading t (- 90)
	beginfill t
	makeWall t (1 - e)
	endfill t
	forward t $ (1 - e) * unitSize
	makeWall t (e - b)
	forward t $ (e - b) * unitSize
	beginfill t
	makeWall t b
	endfill t
drawStructure t (ThroughBE North b e) = do
	setheading t 0
	forward t unitSize
	setheading t 180
	beginfill t
	makeWall t (1 - e)
	endfill t
	forward t $ (1 - e) * unitSize
	makeWall t (e - b)
	forward t $ (e - b) * unitSize
	beginfill t
	makeWall t b
	endfill t
drawStructure _ _ = error "yet"

moduleTest :: IO ()
moduleTest = do
	f <- openField
	topleft f
	t <- newTurtle f
	hideturtle t
	penup t
	drawPlan t [
		[	Hanjou [Wall West, ThroughBE North 0.3 1],
			Hanjou [ThroughBE North 0 0.7],
			Hanjou [Wall West, ThroughBE North 0.2 1],
			Hanjou [Wall North],
			Hanjou [Wall West, Wall North],
			Hanjou [Wall West, Wall North],
			Hanjou [Wall North],
			Hanjou [Wall North, ThroughBE East 0.7 1]
			],
		[
			Hanjou [Wall West, Wall South],
			Hanjou [Wall South],
			Hanjou [ThroughBE West 0.2 0.8]
			]
		]
--	drawHanjou t (0, 0) (Hanjou [Wall West, ThroughBE North 0.3 0.7])
--	drawHanjou t (0, 0) (Hanjou [Wall West, Through North])
--	drawHanjou t (0, 0) (Hanjou [Wall West, Wall North])
	onkeypress f (return . (/= 'q'))
	waitField f
