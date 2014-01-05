module Tetris (
	LR(..),
	Tick(..),
	State,
	initialState,
	nextState,
	blocks,
	processKey,
) where

import Data.List
import Gtk

data LR = L | R deriving Show
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
nextState (Left lr) = moveLR lr
nextState (Right Tick) = moveDown

moveLR :: LR -> State -> State
moveLR lr st@State{
	fallingBlocks = fbs,
	landBlocks = lbs
 } = let nfbs = map (move1 lr) fbs in
 	if isSink nfbs lbs then st
		else st{ fallingBlocks = nfbs }
 	where
	move1 L (x, y) = (x - 1, y)
	move1 R (x, y) = (x + 1, y)

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
	| otherwise = Nothing
