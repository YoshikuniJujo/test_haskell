{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe
import System.Environment

main :: IO ()
main = getArgs >>= readFile . head >>= putStrLn . show
	. (\((s, g), p) ->
--		calc (map (, 1) . fromMaybe [] . (`lookup` p)) dist s g)
		astar (map (, 1) . fromMaybe [] . (`lookup` p)) dist s g)
	. ((fromJust . lookup Start &&& fromJust . lookup Goal) . addPosition
		&&& catMaybes . map (\(ns, (y, x)) -> checkPath1 ns y x)
			. addPosition . flip (neighbors Nothing []) Nothing)
	. map (map toPart)
	. lines

dist :: Position -> Position -> Int
dist (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2)

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
addPosition = concat . zipWith (\y -> zipWith (flip (,) . (y ,)) [0 ..]) [0 ..]

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

near :: Position -> Position -> Position -> Bool
near (y0, x0) (y1, x1) (y2, x2) =
	abs (y0 - y1) + abs (x0 - x1) < abs (y0 - y2) + abs (x0 - x2)

astar :: (Eq n, Ord d, Num d) =>
	(n -> [(n, d)]) -> (n -> n -> d) -> n -> n -> Maybe d
astar p h s g = lookup g . map (second fst) $
	calculate p h g [(s, (h g s, Nothing))] []

calc :: (Eq n, Ord d, Num d) =>
	(n -> [(n, d)]) -> (n -> n -> d) -> n -> n -> Q n d
calc p h s g = calculate p h g [(s, (h g s, Nothing))] []

type Q n d = [(n, (d, Maybe n))]

calculate :: (Eq n, Ord d, Num d) =>
	(n -> [(n, d)]) -> (n -> n -> d) -> n -> Q n d -> Q n d -> Q n d
calculate _ _ _ [] cs = cs
calculate p h g os cs
	| fst s == g = s : cs
	| True = calculate p h g os' (s : cs')
	where
	(s, ns) = popMinBy (fst . snd) os
	nds = nextDists p (h g) s
	(os', cs') = update ns cs (fst s) nds
--	(s, ns) = popMinBy (huristic $ h g) o

huristic :: Num d => (n -> d) -> (n, (d, Maybe n)) -> d
huristic h (n, (d, _)) = h n + d

nextDists :: Num d => (n -> [(n, d)]) -> (n -> d) -> (n, (d, Maybe n)) -> [(n, d)]
nextDists p h (n, (f, _)) =
	map (second (g +) . (fst &&& uncurry ((+) . h))) $ p n
	where g = f - h n

update :: (Eq n, Ord d) => Q n d -> Q n d -> n -> [(n, d)] -> (Q n d, Q n d)
update os cs n0 = foldl (\(ps, ds) -> update1 ps ds n0) (os, cs)

update1 :: (Eq n, Ord d) => Q n d -> Q n d -> n -> (n, d) -> (Q n d, Q n d)
update1 os cs n0 (n, nd)
	| Just ((_, (d, _)), os') <- pop ((== n) . fst) os =
		if nd < d then ((n, (nd, Just n0)) : os', cs) else (os, cs)
	| Just ((_, (d, _)), cs') <- pop ((== n) . fst) cs =
		if nd < d then ((n, (nd, Just n0)) : os, cs') else (os, cs)
	| True = ((n, (nd, Just n0)) : os, cs)

popMinBy :: Ord b => (a -> b) -> [a] -> (a, [a])
popMinBy f (x : xs) = pmb x xs
	where
	pmb m [] = (m, [])
	pmb m (y : ys)
		| f y >= f m = (y :) `second` pmb m ys
		| True = (m :) `second` pmb y ys
popMinBy _ _ = error "popMinBy: empty list"

pop :: (a -> Bool) -> [a] -> Maybe (a, [a])
pop _ [] = Nothing
pop p (x : xs)
	| p x = Just (x, xs)
	| True = second (x :) <$> pop p xs
