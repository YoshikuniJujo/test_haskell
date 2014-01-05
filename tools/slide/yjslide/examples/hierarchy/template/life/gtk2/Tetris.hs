{-# LANGUAGE TupleSections #-}

module Tetris (
	LR(..),
	Tick(..),
	State,
	initialState,
	nextState,
	blocks,
	processKey,
) where

import Control.Arrow
import System.Random

import Data.List
import Gtk

fBlocks :: [[(Int, Int)]]
fBlocks = [
	[(3, 3), (4, 3), (5, 3), (6, 3)],
	[(3, 3), (4, 3), (3, 4), (4, 4)],
	[(4, 3), (5, 3), (3, 4), (4, 4)],
	[(3, 3), (4, 3), (4, 4), (5, 4)],
	[(3, 3), (4, 3), (5, 3), (3, 2)],
	[(3, 3), (4, 3), (5, 3), (3, 4)],
	[(4, 3), (3, 4), (4, 4), (5, 4)]
 ]

type LandBlocks = [(Int, [Int])]

getLandBlocks :: LandBlocks -> [(Int, Int)]
getLandBlocks = concatMap (\(y, xs) -> map (, y) xs)

land :: [(Int, Int)] -> LandBlocks -> LandBlocks
land = flip $ foldr land1

deleteLines :: LandBlocks -> LandBlocks
deleteLines = head . dropWhile (or . map isLineFull) . iterate deleteLine1

deleteLine1 :: LandBlocks -> LandBlocks
deleteLine1 lbs = map (first (+ 1)) upper ++ lower
	where (upper, _ : lower) = span (not . isLineFull) lbs

isLineFull :: (Int, [Int]) -> Bool
isLineFull (_, xs) = sort xs == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

land1 :: (Int, Int) -> LandBlocks -> LandBlocks
land1 (x, y) [] = [(y, [x])]
land1 (x, y) l@((ly, lxs) : rest)
	| ly == y = (ly, x : lxs) : rest
	| ly > y = (y, [x]) : l
	| otherwise = (ly, lxs) : land1 (x, y) rest

data LR = L | R | RotateL | RotateR | Landing deriving Show
data Tick = Tick deriving Show

data State = State {
	fallingBlocks :: [(Int, Int)],
	landBlocks :: LandBlocks,
	randGen :: StdGen
 } deriving Show

initialState :: IO State
initialState = do
	sg <- getStdGen
	return State {
		fallingBlocks = [(3, 3), (4, 3), (5, 3), (3, 4)],
		landBlocks = [],
		randGen = sg
	 }

nextState :: Either LR Tick -> State -> State
nextState (Left L) = move toLeft
nextState (Left R) = move toRight
nextState (Left RotateL) = move rotateLeft
nextState (Left RotateR) = move rotateRight
nextState (Left Landing) = downToLand
nextState (Right Tick) = moveDown

move :: ([(Int, Int)] -> [(Int, Int)]) -> State -> State
move f st@State{
	fallingBlocks = fbs,
	landBlocks = lbs
 } = let nfbs = f fbs in
 	if isSink nfbs lbs then st else st{ fallingBlocks = nfbs }

toLeft, toRight :: [(Int, Int)] -> [(Int, Int)]
toLeft = map (first (subtract 1))
toRight = map (first (+ 1))

rotateLeft, rotateRight :: [(Int, Int)] -> [(Int, Int)]
rotateLeft bs = map (rl $ bs !! 1) bs
	where rl (cx, cy) (x, y) = (cx + y - cy, cy - x + cx)
rotateRight bs = map (rr $ bs !! 1) bs
	where rr (cx, cy) (x, y) = (cx - y + cy, cy + x - cx)

downToLand :: State -> State
downToLand st@State{
	fallingBlocks = fbs,
	landBlocks = lbs
 } = st{
	fallingBlocks = last $ takeWhile (not . flip isSink lbs) $ iterate (map move1) fbs
 }	where
	move1 (x, y) = (x, y + 1)

moveDown :: State -> State
moveDown st@State{
	fallingBlocks = fbs,
	landBlocks = lbs,
	randGen = rg
 } = let nfbs = map move1 fbs in
 	if isSink nfbs lbs
		then st{
			fallingBlocks = nfb,
			landBlocks = deleteLines $ land fbs lbs,
			randGen = nrg
		 }
		else st{
			fallingBlocks = nfbs,
			landBlocks = lbs
		 }
 	where
	(nfb, nrg) = first (fBlocks !!) $ randomR (0, length fBlocks - 1) rg
	move1 (x, y) = (x, y + 1)

isSink :: [(Int, Int)] -> LandBlocks -> Bool
isSink fbs lbs =
	maximum (map snd fbs) > 21 ||
	minimum (map fst fbs) < 0 ||
	maximum (map fst fbs) > 9 ||
	not (null $ fbs `intersect` getLandBlocks lbs)

waku :: [(Int, Int)]
waku =	zip [-1 .. 10] [22, 22 .. ] ++ zip [-1 .. 10] [0, 0 .. ] ++
	zip [-1, -1 .. ] [1 .. 21] ++ zip [10, 10 .. ] [1 .. 21]

blocks :: State -> [(Int, Int)]
blocks s = fallingBlocks s ++ getLandBlocks (landBlocks s) ++ waku

processKey :: Keyval -> Maybe (Either LR Tick)
processKey kv
	| kv == char2keyval 'h' = Just $ Left L
	| kv == char2keyval 'l' = Just $ Left R
	| kv == char2keyval 'j' = Just $ Left RotateL
	| kv == char2keyval 'k' = Just $ Left RotateR
	| kv == char2keyval ' ' = Just $ Left Landing
	| otherwise = Nothing
