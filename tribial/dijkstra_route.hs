{-# LANGUAGE TupleSections #-}

import Data.List
import Control.Applicative
import System.Environment

import ReadTable

type Queue = [(Int, Maybe ([Int], Int))]

ips :: Queue
ips = [
	(1, Just ([], 0)), (2, Nothing), (3, Nothing),
	(4, Nothing), (5, Nothing), (6, Nothing) ]

getMin :: Queue -> Maybe ((Int, ([Int], Int)), Queue)
getMin [] = Nothing
getMin (pnt@(_, Nothing) : p) = do
	(mn, rest) <- getMin p
	return (mn, pnt : rest)
getMin (pd@(pnt, Just rd@(_, d)) : p) = case getMin p of
	Just (pd0@(pnt0, rd0@(_, d0)), rest) -> return $ if d <= d0
		then ((pnt, rd), (pnt0, Just rd0) : rest)
		else (pd0, pd : rest)
	_ -> return ((pnt, rd), p)

next :: [[Maybe Int]] -> Queue -> Maybe ((Int, ([Int], Int)), Queue)
next p ps = do
	(pd0@(pnt0, d0), ps') <- getMin ps
	return $ (pd0, map (calc p pd0) ps')

calc :: [[Maybe Int]] -> (Int, ([Int], Int)) ->
	(Int, Maybe ([Int], Int)) -> (Int, Maybe ([Int], Int))
calc p (s, (r, d0)) (pnt, d) = (,) pnt $
	sel d $ (s : r ,) . (d0 +) <$> dist p s pnt
	where
	Just x@(_, dx) `sel` Just y@(_, dy)
		| dx <= dy = Just x
		| otherwise = Just y
	Just x `sel` _ = Just x
	_ `sel` Just y = Just y
	_ `sel` _ = Nothing

main :: IO ()
main = do
	fp : _ <- getArgs
	src <- readFile fp
	let	t = readTable' src
		Just (r_, d) = lookup 100 . unfoldr (next t) $
			(1, Just ([], 0)) : map (, Nothing) [2 .. 100]
		r = reverse $ 100 : r_
	print $ pairs r
	print d
	print $ map (get100 t) (pairs r)

get100 t dd = lookup 100 . unfoldr (next' t dd) $
		(1, Just ([], 0)) : map (, Nothing) [2 .. 100]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x : xs@(y : _)) = (x, y) : pairs xs

dist' :: [[Maybe Int]] -> (Int, Int) -> Int -> Int -> Maybe Int
dist' t (s0, g0) s g
	| s == s0 && g == g0 = (100 +) <$> dist t s g
	| otherwise = dist t s g

next' :: [[Maybe Int]] -> (Int, Int) -> Queue -> Maybe ((Int, ([Int], Int)), Queue)
next' p dd ps = do
	(pd0@(pnt0, d0), ps') <- getMin ps
	return $ (pd0, map (calc' p dd pd0) ps')

calc' :: [[Maybe Int]] -> (Int, Int) -> (Int, ([Int], Int)) ->
	(Int, Maybe ([Int], Int)) -> (Int, Maybe ([Int], Int))
calc' p dd (s, (r, d0)) (pnt, d) = (,) pnt $
	sel d $ (s : r ,) . (d0 +) <$> dist' p dd s pnt
	where
	Just x@(_, dx) `sel` Just y@(_, dy)
		| dx <= dy = Just x
		| otherwise = Just y
	Just x `sel` _ = Just x
	_ `sel` Just y = Just y
	_ `sel` _ = Nothing
