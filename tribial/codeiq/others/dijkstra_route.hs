{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.List
import Data.Char
import System.Environment

type Station = Int
type Path = [Station]
type Dist = Int

type Table = Station -> Station -> Maybe Dist
type Queue = [QueueElem]
type QueueElem = (Station, Maybe (Path, Dist))
type Result = (Station, (Path, Dist))

dijkstra :: Table -> Queue -> [Result]
dijkstra t q = unfoldr (next t) q

nearest :: Queue -> Maybe (Result, Queue)
nearest [] = Nothing
nearest (e@(_, Nothing) : q) = second (e :) <$> nearest q
nearest (e@(s, Just pd@(_, d)) : q) = case nearest q of
	Just (e0@(s0, pd0@(_, d0)), r) -> return $
		if d <= d0 then ((s, pd), (s0, Just pd0) : r) else (e0, e : r)
	_ -> return ((s, pd), q)

next :: Table -> Queue -> Maybe (Result, Queue)
next t q = nearest q >>= \(n, q') -> return (n, map (update t n) q')

update :: Table -> Result -> QueueElem -> QueueElem
update t (s0, (p, d0)) (s, d) = (s ,) . mn d $ (s0 : p ,) . (d0 +) <$> t s0 s
	where
	Just x@(_, dx) `mn` Just (_, dy) | dx <= dy = Just x
	_ `mn` Just y = Just y
	Just x `mn` _ = Just x
	_ `mn` _ = Nothing

main :: IO ()
main = do
	d <- process <$> (readFile . head =<< getArgs)
	let	Just r0 = route100 $ dijkstra (dist d) (initQueue d)
		rs = catMaybes
			. map (route100 . flip dijkstra (initQueue d) . dist' d)
			. zip r0 $ tail r0
		rs' = nub . sort $ r0 : rs
	putStr . unlines $ map (intercalate " -> " . map show) rs'
	where
	route100 = (reverse . (100 :) . fst <$>) . lookup 100

type Data = [[Maybe Int]]

process :: String -> Data
process ('{' : str) = case span (/= '}') str of
		(h, '}' : ',' : t) -> comma h : process t
		(h, '}' : _) -> [comma h]
		_ -> error "parse error"
	where
	comma s = case span (/= ',') s of
		(h, ',' : t) -> dst h : comma t
		(h, "") -> [dst h]
		_ -> error "parse error"
	dst s = case read s of 0 -> Nothing; n -> Just n
process (c : str) | isSpace c = process str
process _ = error "parse error"

dist :: Data -> Table
dist p x y = p !! (x - 1) !! (y - 1)

dist' :: Data -> (Station, Station) -> Table
dist' t (s0, g0) s g
	| s == s0 && g == g0 = (100 +) <$> dist t s g
	| otherwise = dist t s g

initQueue :: Data -> Queue
initQueue d = (1, Just ([], 0)) : map (, Nothing) [2 .. length d]
