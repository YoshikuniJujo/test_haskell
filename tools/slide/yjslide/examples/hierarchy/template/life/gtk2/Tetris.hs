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

import Data.List
import Gtk

data LR = L | R | RotateL | RotateR | Landing deriving Show
data Tick = Tick deriving Show

data State = State {
	fallingBlocks :: [(Int, Int)],
	landed :: Bool,
	landBlocks :: [(Int, Int)]
 } deriving Show

initialState :: IO State
initialState = do

	return State {
		fallingBlocks = [(3, 3), (4, 3), (5, 3), (3, 4)],
		landed = False,
		landBlocks = []
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
	where rl (cx, cy) (x, y) = (cx - y + cy, cy + x - cx)
rotateRight bs = map (rr $ bs !! 1) bs
	where rr (cx, cy) (x, y) = (cx + y - cy, cy - x + cx)

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
	landBlocks = lbs
 } = let nfbs = map move1 fbs in
 	if isSink nfbs lbs
		then st{
			fallingBlocks = [(3, 3), (4, 3), (5, 3), (3, 4)],
			landBlocks = fbs ++ lbs
		 }
		else st{
			fallingBlocks = nfbs,
			landed = False,
			landBlocks = lbs
		 }
 	where
	move1 (x, y) = (x, y + 1)

isSink :: [(Int, Int)] -> [(Int, Int)] -> Bool
isSink fbs lbs =
	maximum (map snd fbs) > 21 ||
	minimum (map fst fbs) < 0 ||
	maximum (map fst fbs) > 9 ||
	not (null $ fbs `intersect` lbs)

waku :: [(Int, Int)]
waku =	zip [-1 .. 10] [22, 22 .. ] ++ zip [-1 .. 10] [0, 0 .. ] ++
	zip [-1, -1 .. ] [1 .. 21] ++ zip [10, 10 .. ] [1 .. 21]

blocks :: State -> [(Int, Int)]
blocks s = fallingBlocks s ++ landBlocks s ++ waku

processKey :: Keyval -> Maybe (Either LR Tick)
processKey kv
	| kv == char2keyval 'h' = Just $ Left L
	| kv == char2keyval 'l' = Just $ Left R
	| kv == char2keyval 'j' = Just $ Left RotateL
	| kv == char2keyval 'k' = Just $ Left RotateR
	| kv == char2keyval ' ' = Just $ Left Landing
	| otherwise = Nothing
