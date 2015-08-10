{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Arrow
import Data.Maybe
import System.Environment

main :: IO ()
main = getArgs >>= readFile . head >>= putStrLn . show
	. (\((s, g), p) -> (s, g, p))
	. ((fromJust . lookup Start &&& fromJust . lookup Goal) . addPosition
		&&& catMaybes . map (\(ns, (y, x)) -> checkPath1 ns y x)
			. addPosition . flip (neighbors Nothing []) Nothing)
	. map (map toPart)
	. lines

data Part = Start | Goal | Space | Block deriving (Show, Eq)

toPart :: Char -> Part
toPart 'S' = Start
toPart 'G' = Goal
toPart ' ' = Space
toPart '*' = Block
toPart _ = error "toPart: bad part"

isSpace :: Part -> Bool
isSpace Block = False
isSpace _ = True

type Position = (Int, Int)
type Path = [(Position, [Position])]

type Neighbors a = (Maybe a, (Maybe a, a, Maybe a), Maybe a )

neighbors :: Maybe [a] -> [a] -> [[a]] -> Maybe [a] -> [[Neighbors a]]
neighbors _ _ [] _ = []
neighbors _ ls ([] : bss) _ =
	[] : neighbors (Just $ reverse ls) [] bss (listToMaybe bss)
neighbors mts ls ((h : rs) : bss) mbs =
	((mt, (ml, h, mr), mb) :) `heading` neighbors mts' (h : ls) (rs : bss) mbs'
	where
	(mt, mts') = ((head <$>) &&& (tail <$>)) mts
	ml = listToMaybe ls
	mr = listToMaybe rs
	(mb, mbs') = ((head <$>) &&& (tail <$>)) mbs

addPosition :: [[a]] -> [(a, Position)]
addPosition = concat . zipWith (\y -> zipWith (flip (,) . (, y)) [0 ..]) [0 ..]

heading :: (a -> a) -> [a] -> [a]
heading f (x : xs) = f x : xs
heading _ [] = []

checkPath :: [[Neighbors Part]] -> Path
checkPath ns = catMaybes . concat $
	zipWith (\y -> zipWith (\x n -> checkPath1 n y x) [0 ..]) [0 ..] ns

checkPath1 :: Neighbors Part -> Int -> Int -> Maybe (Position, [Position])
checkPath1 (t, (l, h, r), b) y x = case (h, ns) of
	(Block, _) -> Nothing
	(_, []) -> Nothing
	_ -> Just ((y, x), ns)
	where
	ns = catMaybes [
		if maybe False isSpace t then Just (y - 1, x) else Nothing,
		if maybe False isSpace l then Just (y, x - 1) else Nothing,
		if maybe False isSpace r then Just (y, x + 1) else Nothing,
		if maybe False isSpace b then Just (y + 1, x) else Nothing ]

dist :: Path -> Position -> Position -> Maybe Int
dist pt p1 p2
	| Just True <- (p2 `elem`) <$> lookup p1 pt = Just 1
	| True = Nothing

near :: Position -> Position -> Position -> Bool
near (y0, x0) (y1, x1) (y2, x2) =
	abs (y0 - y1) + abs (x0 - x1) < abs (y0 - y2) + abs (x0 - x2)

astar :: (Eq n, Ord d, Num d) =>
	(n -> n -> Maybe d) -> (n -> n -> Bool) -> [n] -> n -> n -> Maybe d
astar p h ns s g = Nothing

type Q n d = [(n, Maybe d)]

calculate :: (Eq n, Ord d, Num d) =>
	(n -> n -> Maybe d) -> (n -> n -> Bool) -> Q n d -> Q n d
calculate p h q = q
