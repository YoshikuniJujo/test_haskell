module AI (
) where

import Control.Arrow (first, second)
import Data.List (partition)

import Game (
	Game, Turn(..), X(..), Y(..), Disk(..),
	turn, disks, placeable, nextGame)
import Tools (flipEnum, forMaybe, maximumBySnd)

type Table = [((X, Y), Int)]

flipXY, flipX, flipY :: (X, Y) -> (X, Y)
flipXY (x, y) = (toEnum $ fromEnum y, toEnum $ fromEnum x)
flipX = first flipEnum
flipY = second flipEnum

score :: Table -> (X, Y) -> Int
score t pos@(x, y)
	| x > D = score t $ flipX pos
	| y > Y4 = score t $ flipY pos
	| fromEnum x < fromEnum y = score t $ flipXY pos
score t pos = case lookup pos t of
	Just p -> p
	_ -> error "bad table"

table2 :: Table
table2 = [
	((A, Y1), 120),
	((B, Y1), -20),
	((B, Y2), -40),
	((C, Y1), 20),
	((C, Y2), -5),
	((C, Y3), 15),
	((D, Y1), 5),
	((D, Y2), -5),
	((D, Y3), 3),
	((D, Y4), 3) ]

table1 :: Table
table1 = [
	((A, Y1),  30),
	((B, Y1), -12),
	((B, Y2), -15),
	((C, Y1),   0),
	((C, Y2),  -3),
	((C, Y3),   0),
	((D, Y1),  -1),
	((D, Y2),  -3),
	((D, Y3),  -1),
	((D, Y4),  -1) ]

evaluateWith :: (Int -> (X, Y) -> Int) -> Disk -> Game -> Int
evaluateWith scr d g = ss me - ss you
	where
	ss = sum . map (scr (length ds) . fst)
	(me, you) = partition ((== d) . snd) ds
	ds = disks g
