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

data LR = L | R deriving Show
data Tick = Tick deriving Show

data State = State {
	fallingBlocks :: [(Int, Int)],
	landBlocks :: [(Int, Int)]
 } deriving Show

initialState :: IO State
initialState = do

	return State {
		fallingBlocks = [(3, 3), (4, 3), (5, 3), (3, 4)],
		landBlocks = []
	 }

nextState :: Either LR Tick -> State -> State
nextState input st@State{
	fallingBlocks = fbs,
	landBlocks = lbs
 } = st {
	fallingBlocks = if isLand fbs lbs then [(3, 3), (4, 3), (5, 3), (3, 4)] else
		map (move input) fbs,
	landBlocks = if isLand fbs lbs then fbs ++ lbs else lbs
 }
	where
	move (Left L) (x, y) = (x - 1, y)
	move (Left R) (x, y) = (x + 1, y)
	move (Right Tick) (x, y) = (x, y + 1)

isLand :: [(Int, Int)] -> [(Int, Int)] -> Bool
isLand fbs lbs = maximum (map snd fbs) > 20 ||
	not (null $ downBlocks fbs `intersect` lbs)

downBlocks :: [(Int, Int)] -> [(Int, Int)]
downBlocks = map (second (+ 1))

blocks :: State -> [(Int, Int)]
blocks s = fallingBlocks s ++ landBlocks s

processKey :: Keyval -> Maybe (Either LR Tick)
processKey kv
	| kv == char2keyval 'h' = Just $ Left L
	| kv == char2keyval 'l' = Just $ Left R
	| otherwise = Nothing
