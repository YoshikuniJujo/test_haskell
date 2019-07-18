{-# LANGUAGE QuasiQuotes, TypeFamilies, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AStar.Samples where

import Control.Arrow
import Data.Maybe
import Data.List
import Data.Bool
import Data.Int
import Text.Nowdoc

import AStar

data Maze = Maze String

sampleMaze :: Maze
sampleMaze = Maze [nowdoc|
...................................G.....
.........................................
........*******************************..
.........................................
.........................................
*****************************............
.........................................
*********...*****************************
.......................*.................
.......................*.................
.......................*........S........
.......................*.................
.........................................
.........................................
|]

simpleMaze :: Maze
simpleMaze = Maze [nowdoc|
.SG.
|]

checkPos :: Maze -> Int8 -> Int8 -> Char
checkPos (Maze m) x_ y_ = (!! x) . (!! y) $ lines m
	where [x, y] = fromIntegral <$> [x_, y_]

searchPos :: Maze -> Char -> Maybe (Int8, Int8)
searchPos (Maze m_) c = do
	y <- findIndex (c `elem`) m
	x <- findIndex (c ==) $ m !! y
	return (fromIntegral x, fromIntegral y)
	where m = lines m_

type MChar = Maybe Char

checkUpBmLtRt :: Maze -> Int8 -> Int8 -> (MChar, MChar, MChar, MChar)
checkUpBmLtRt mz@(Maze m_) x y = (u, b, l, r)
	where
	[u, b, l, r] = (uncurry (checkPos mz) <$>)
		. (\(z, v) -> (,) <$> z <*> v)
		. (insideOr 0 (w - 1) *** insideOr 0 (h - 1))
		<$> [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
	m = lines m_
	h = fromIntegral $ length m
	w = fromIntegral . length $ head m

walkable :: Maze -> Int8 -> Int8 -> [(Int8, Int8)]
walkable mz x y = catMaybes [
	bool Nothing (Just (x, y - 1)) $ u == Just '.' || u == Just 'G',
	bool Nothing (Just (x, y + 1)) $ b == Just '.' || b == Just 'G',
	bool Nothing (Just (x - 1, y)) $ l == Just '.' || l == Just 'G',
	bool Nothing (Just (x + 1, y)) $ r == Just '.' || r == Just 'G' ]
	where
	(u, b, l, r) = checkUpBmLtRt mz x y

insideOr :: Int8 -> Int8 -> Int8 -> Maybe Int8
insideOr mn mx x
	| mn <= x && x <= mx = Just x
	| otherwise = Nothing

instance AStar Maze where
	type AStarNode Maze = (Int8, Int8)
	startNode mz = fromJust $ searchPos mz 'S'
	isEndNode mz ps = ps == fromJust (searchPos mz 'G')
	nextNodes mz (x, y) = (, 1) <$> walkable mz x y
	distToEnd mz (x, y) = fromIntegral $ abs (gx - x) + abs (gy - y)
		where (gx, gy) = fromJust $ searchPos mz 'G'

pointChar :: [String] -> Int8 -> Int8 -> Char -> [String]
pointChar ss x_ y_ c = take y ss ++ [take x l ++ [c] ++ drop (x + 1) l] ++ drop (y + 1) ss
	where
	l =  ss !! y
	[x, y] = fromIntegral <$> [x_, y_]

pointWalks :: [String] -> [(Int8, Int8)] -> [String]
pointWalks = foldr (\(x, y) ss -> pointChar ss x y 'o')

showRoute :: Maze -> String
showRoute mz@(Maze m) = maybe m (unlines . pointWalks (lines m) . init . tail) $ astar mz
