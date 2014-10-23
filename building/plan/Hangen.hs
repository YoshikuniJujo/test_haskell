module Hangen () where

import Control.Monad
import Graphics.X11.Turtle

shaku :: Double
shaku = 10 / 33

hangen :: Double
hangen = 3 * shaku

data Hanjou = Hanjou [(Structure, Direction)] deriving Show

data Direction = East | West | South | North deriving (Show, Eq)

data Structure
	= Wall
	| Through
	| ThroughBE Double Double
	deriving (Show, Eq)

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
	drawStructureWithDir t s
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
drawStructure t Wall = beginfill t >> makeWall t 1 >> endfill t
drawStructure t Through = makeWall t 1
drawStructure t (ThroughBE b e) = do
	beginfill t
	makeWall t (1 - e)
	endfill t
	forward t $ (1 - e) * unitSize
	makeWall t (e - b)
	forward t $ (e - b) * unitSize
	beginfill t
	makeWall t b
	endfill t

drawStructureWithDir :: Turtle -> (Structure, Direction) -> IO ()
drawStructureWithDir t (s, East) = do
		penup t
		setheading t 0
		forward t unitSize
		setheading t (- 90)
		forward t unitSize
		setheading t 90
		pendown t
		drawStructure t s
drawStructureWithDir t (s, West) = do
		setheading t (- 90)
		drawStructure t s
drawStructureWithDir t (s, North) = do
		setheading t 0
		forward t unitSize
		setheading t 180
		drawStructure t s
drawStructureWithDir t (s, South) = do
		penup t
		setheading t (- 90)
		forward t unitSize
		pendown t
		setheading t 0
		drawStructure t s

moduleTest :: IO ()
moduleTest = do
	f <- openField
	topleft f
	t <- newTurtle f
	hideturtle t
	penup t
	drawPlan t [
		[	Hanjou [(Wall, West), (ThroughBE 0.3 1, North)],
			Hanjou [(ThroughBE 0 0.7, North)],
			Hanjou [(Wall, West), (ThroughBE 0.2 1, North)],
			Hanjou [(Wall, North)],
			Hanjou [(Wall, West), (Wall, North),
				(ThroughBE 0.1 0.8, South)],
			Hanjou [(Wall, West), (Wall, North)],
			Hanjou [(Wall, North)],
			Hanjou [(Wall, North), (ThroughBE 0.7 1, East)]
			],
		[
			Hanjou [(Wall, West), (Wall, South)],
			Hanjou [(Wall, South)],
			Hanjou [(ThroughBE 0.2 0.8, West),
				(ThroughBE 0 0.8, South)],
			Hanjou [(Wall, South)],
			Hanjou [(Wall, West)],
			Hanjou [],
			Hanjou [],
			Hanjou [(ThroughBE 0.2 0.5, East)]
			],
		[
			Hanjou [(ThroughBE 0.1 0.9, West)],
			Hanjou [],
			Hanjou [],
			Hanjou [],
			Hanjou [],
			Hanjou [],
			Hanjou [],
			Hanjou [(Wall, East)]
			],
		[
			Hanjou [(ThroughBE 0.2 0.8, West),
				(ThroughBE 0.2 0.8, North)],
			Hanjou [(Wall, West), (Wall, North)],
			Hanjou [],
			Hanjou [],
			Hanjou [],
			Hanjou [],
			Hanjou [],
			Hanjou [(Wall, East)]
			],
		[
			Hanjou [(Wall, West)],
			Hanjou [(Wall, West)],
			Hanjou [],
			Hanjou [],
			Hanjou [],
			Hanjou [],
			Hanjou [],
			Hanjou [(Through, East)]
			],
		[
			Hanjou [(Wall, West), (Wall, North),
				(ThroughBE 0.1 0.8, South)],
			Hanjou [(Wall, West), (Wall, South)],
			Hanjou [(ThroughBE 0.1 0.8, South)],
			Hanjou [(Wall, South), (Wall, East)],
			Hanjou [],
			Hanjou [],
			Hanjou [],
			Hanjou [(Through, East)]
			],
		[
			Hanjou [(ThroughBE 0.1 0.3, West)],
			Hanjou [],
			Hanjou [],
			Hanjou [(ThroughBE 0.1 0.7, East)],
			Hanjou [],
			Hanjou [],
			Hanjou [],
			Hanjou [(Through, East)]
			],
		[
			Hanjou [(Wall, West), (ThroughBE 0 0.5, South)],
			Hanjou [(ThroughBE 0.5 1, South)],
			Hanjou [(Wall, South)],
			Hanjou [(Wall, South), (Wall, East)],
			Hanjou [(Wall, South)],
			Hanjou [(Wall, South)],
			Hanjou [(Wall, South)],
			Hanjou [(Wall, South), (Wall, East)]
			]
		]
	onkeypress f (return . (/= 'q'))
	waitField f
