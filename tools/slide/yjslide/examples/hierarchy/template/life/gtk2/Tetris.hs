{-# LANGUAGE TupleSections #-}

module Tetris (
	LR(..),
	Tick(..),
	State,
	initialState,
	nextState,
	blocks,
	processKey,
	point,
	gameOver,
) where

import Control.Arrow
import System.Random

import Data.List
import Gtk

fBlocks :: [([(Int, Int)], Color)]
fBlocks = [
	([(3, 3), (4, 3), (5, 3), (6, 3)], (0.25, 0.25, 1)),
	([(3, 3), (4, 3), (3, 4), (4, 4)], (0.75, 0.75, 0)),
	([(4, 3), (5, 3), (3, 4), (4, 4)], (0, 0.75, 0)),
	([(3, 3), (4, 3), (4, 4), (5, 4)], (0.75, 0, 0)),
	([(3, 3), (4, 3), (5, 3), (3, 2)], (0, 0, 0.45)),
	([(3, 3), (4, 3), (5, 3), (3, 4)], (0.50, 0.25, 0)),
	([(3, 4), (4, 4), (4, 3), (5, 4)], (0.75, 0, 0.75))
 ]

type Color = (Double, Double, Double)
black, grey :: Color
black = (0, 0, 0)
grey = (0.9, 0.9, 0.9)

type LandBlocks = [(Int, [(Int, Color)])]

getLandBlocks :: LandBlocks -> [((Int, Int), Color)]
getLandBlocks = concatMap (\(y, xs) -> map (\(x, c) -> ((x, y), c)) xs)

land :: ([(Int, Int)], Color) -> LandBlocks -> LandBlocks
land (fbs, c) lbs = foldr (land1 c) lbs fbs

deleteLines :: LandBlocks -> (Int, LandBlocks)
deleteLines lbs = let
	(yet, deleted) = span (or . map isLineFull) $ iterate deleteLine1 lbs in
	(length yet, head deleted)

deleteLine1 :: LandBlocks -> LandBlocks
deleteLine1 lbs = map (first (+ 1)) upper ++ lower
	where (upper, _ : lower) = span (not . isLineFull) lbs

isLineFull :: (Int, [(Int, Color)]) -> Bool
isLineFull (_, xs) = map fst (sort xs) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

land1 :: Color -> (Int, Int) -> LandBlocks -> LandBlocks
land1 c (x, y) [] = [(y, [(x, c)])]
land1 c (x, y) l@((ly, lxs) : rest)
	| ly == y = (ly, (x, c) : lxs) : rest
	| ly > y = (y, [(x, c)]) : l
	| otherwise = (ly, lxs) : land1 c (x, y) rest

data LR = L | R | RotateL | RotateR | Landing deriving Show
data Tick = Tick deriving Show

data State = State {
	nextBlocks :: [([(Int, Int)], Color)],
	fallingBlocks :: ([(Int, Int)], Color),
	landBlocks :: LandBlocks,
	randGen :: StdGen,
	point :: Int,
	gameOver :: Bool
 } deriving Show

initialState :: IO State
initialState = do
	sg <- getStdGen
	let	(fbs1, nsg) = newBlock sg
		(fbs2, nsg') = newBlock nsg
		(fbs3, nsg'') = newBlock nsg'
		(fbs4, nsg''') = newBlock nsg''
	return State {
		nextBlocks = [fbs2, fbs3, fbs4],
		fallingBlocks = fbs1,
		landBlocks = [],
		randGen = nsg''',
		point = 0,
		gameOver = False
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
 } = let nfbs = first f fbs in
 	if isSink (fst nfbs) lbs then st else st{ fallingBlocks = nfbs }

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
	fallingBlocks = if null landingList then fbs else last landingList
 }	where
	move1 (x, y) = (x, y + 1)
	landingList = takeWhile (not . flip isSink lbs . fst) $
		iterate (first $ map move1) fbs

moveDown :: State -> State
moveDown st@State{
	nextBlocks = nb : nbs,
	fallingBlocks = fbs,
	landBlocks = lbs,
	randGen = rg,
	point = p
 } = let nfbs = first (map move1) fbs in
 	if isSink (fst nfbs) lbs
		then st{
			nextBlocks = nbs ++ [nfb],
			fallingBlocks = nb,
			landBlocks = nlbs,
			randGen = nrg,
			point = p + if dn == 0 then 10 else 100 * dn ^ (2 :: Int),
			gameOver = isSink (fst nfb) nlbs
		 }
		else st{
			fallingBlocks = nfbs,
			landBlocks = lbs
		 }
 	where
	(nfb, nrg) = newBlock rg
	move1 (x, y) = (x, y + 1)
	(dn, nlbs) = deleteLines $ land fbs lbs

newBlock :: StdGen -> (([(Int, Int)], Color), StdGen)
newBlock = first (fBlocks !!) . randomR (0, length fBlocks - 1)

isSink :: [(Int, Int)] -> LandBlocks -> Bool
isSink fbs lbs =
	maximum (map snd fbs) > 21 ||
	minimum (map fst fbs) < 0 ||
	maximum (map fst fbs) > 9 ||
	not (null $ fbs `intersect` map fst (getLandBlocks lbs))

waku :: [(Int, Int)]
waku =	zip [-1 .. 10] [22, 22 .. ] ++ zip [-1 .. 10] [0, 0 .. ] ++
	zip [-1, -1 .. ] [1 .. 21] ++ zip [10, 10 .. ] [1 .. 21]

blocks :: State -> [((Int, Int), Color)]
blocks s =
	map (, grey) (fst $ fallingBlocks (downToLand s)) ++
	uncurry separateColors (fallingBlocks s) ++ getLandBlocks (landBlocks s) ++
	map ((, black)) waku ++ concat (zipWith (\y b ->
		map (first $ movePoint 9 y) (uncurry separateColors b))
			[1, 4 .. ] (nextBlocks s))

movePoint :: Int -> Int -> (Int, Int) -> (Int, Int)
movePoint dx dy (x, y) = (x + dx, y + dy)

separateColors :: [(Int, Int)] -> Color -> [((Int, Int), Color)]
separateColors bs c = map (, c) bs

processKey :: Keyval -> Maybe (Either LR Tick)
processKey kv
	| kv == char2keyval 'h' = Just $ Left L
	| kv == char2keyval 'l' = Just $ Left R
	| kv == char2keyval 'j' = Just $ Left RotateL
	| kv == char2keyval 'k' = Just $ Left RotateR
	| kv == char2keyval ' ' = Just $ Left Landing
	| otherwise = Nothing
