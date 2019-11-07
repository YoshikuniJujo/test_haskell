{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ScheduledBottomUpMergeSort where

newtype Schedule a = Schedule [[a]] deriving Show
data Sortable a = Sortable Int [([a], Schedule a)] deriving Show

mrg :: Ord a => [a] -> [a] -> [a]
mrg [] ys = ys
mrg xs [] = xs
mrg xs@(x : xs') ys@(y : ys')
	| x <= y = x : mrg xs' ys
	| otherwise = y : mrg xs ys'

exec1 :: Schedule a -> Schedule a
exec1 (Schedule []) = Schedule []
exec1 (Schedule ([] : sched)) = exec1 $ Schedule sched
exec1 (Schedule ((_ : xs) : sched)) = Schedule $ xs : sched

exec2 :: ([a], Schedule a) -> ([a], Schedule a)
exec2 (xs, sched) = (xs, exec1 $ exec1 sched)

empty :: Sortable a
empty = Sortable 0 []

add :: Ord a => a -> Sortable a -> Sortable a
add x (Sortable size segs) = Sortable (size + 1) (exec2 <$> segs')
	where segs' = addSeg [x] segs size []

addSeg :: Ord a => [a] -> [([a], Schedule a)] -> Int -> [[a]] -> [([a], Schedule a)]
addSeg xs segs size rsched
	| 0 <- size `mod` 2 = (xs, Schedule $ reverse rsched) : segs
	| otherwise = let
		(xs', Schedule []) : segs' = segs
		xs'' = mrg xs xs' in
		addSeg xs'' segs' (size `div` 2) (xs'' : rsched)


sort :: Ord a => Sortable a -> [a]
sort (Sortable _ segs) = mrgAll [] segs

mrgAll :: Ord a => [a] -> [([a], Schedule a)] -> [a]
mrgAll xs [] = xs
mrgAll xs ((xs', _) : segs) = mrgAll (mrg xs xs') segs

-- data Sortable a = Sortable Int [([a], Schedule a)] deriving Show
